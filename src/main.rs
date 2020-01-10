extern crate crossbeam_channel;
extern crate jack;
use crossbeam_channel::{bounded, unbounded};
use notify::{RecommendedWatcher, RecursiveMode, Watcher};
use std::collections::HashMap;
use std::error::Error;
use std::f64::consts::PI;
use std::fs;
use std::io;
use std::path::Path;
use std::time::Duration;

#[derive(Clone, Copy)]
enum ModuleHandle {
    Constant(f64),
    Envelope(usize),
    MidiBinding(usize),
    SineOscillator(usize),
    SawtoothOscillator(usize),
    SquareOscillator(usize),
}

struct EnvelopeModule {
    attack: ModuleHandle,
    decay: ModuleHandle,
    sustain: ModuleHandle,
    release: ModuleHandle,

    out: f64,
}

struct MidiBindingModule {
    binding: u8,
    min: f64,
    max: f64,

    out: f64,
}

struct OscillatorModule {
    amplitude: ModuleHandle,
    frequency: ModuleHandle,
    phase: f64,

    out: f64,
}

struct Config {
    note: u8,
    velocity: u8,
    frequency: f64,
    on: bool,
    time_since_toggle: f64,

    envelopes: Vec<EnvelopeModule>,
    midi_bindings: Vec<MidiBindingModule>,
    sine_oscillators: Vec<OscillatorModule>,
    sawtooth_oscillators: Vec<OscillatorModule>,
    square_oscillators: Vec<OscillatorModule>,

    out: ModuleHandle,
}

impl Config {
    fn new(value: &serde_json::Value) -> Result<Config, Box<dyn Error>> {
        let mut config = Config {
            note: 0,
            velocity: 0,
            frequency: 0.0,
            on: false,
            time_since_toggle: 0.0,
            envelopes: Vec::new(),
            midi_bindings: Vec::new(),
            sine_oscillators: Vec::new(),
            sawtooth_oscillators: Vec::new(),
            square_oscillators: Vec::new(),
            out: ModuleHandle::Constant(0.0),
        };

        let variables = match value
            .get("variables")
            .ok_or("top-level key 'variables' not found")?
        {
            serde_json::Value::Object(variables) => Ok(variables),
            _ => Err("top-level key 'variables' must be an object"),
        }?;
        let empty_map = HashMap::new();
        let variables_map = variables
            .iter()
            .map(|(key, value)| match config.add_json(&empty_map, &value) {
                Ok(handle) => Ok((key.clone(), handle)),
                Err(err) => Err(err),
            })
            .collect::<Result<HashMap<String, ModuleHandle>, Box<dyn Error>>>()?;

        let out = value.get("out").ok_or("top-level 'key' out not found")?;
        config.out = config.add_json(&variables_map, &out)?;

        Ok(config)
    }

    fn add_json(
        &mut self,
        variables: &HashMap<String, ModuleHandle>,
        value: &serde_json::Value,
    ) -> Result<ModuleHandle, Box<dyn Error>> {
        match value {
            serde_json::Value::Number(number) => {
                let number = number
                    .as_f64()
                    .ok_or(format!("failed to parse '{}' as f64", number))?;
                Ok(ModuleHandle::Constant(number))
            }
            serde_json::Value::String(key) => {
                let variable = variables
                    .get(key)
                    .ok_or(format!("unrecognized variable key '{}'", key))?;
                Ok(*variable)
            }
            serde_json::Value::Object(object) => {
                if let (Some(attack), Some(decay), Some(sustain), Some(release)) = (
                    object.get("attack"),
                    object.get("decay"),
                    object.get("sustain"),
                    object.get("release"),
                ) {
                    let (attack, decay, sustain, release) = (
                        self.add_json(variables, attack)?,
                        self.add_json(variables, decay)?,
                        self.add_json(variables, sustain)?,
                        self.add_json(variables, release)?,
                    );
                    self.envelopes.push(EnvelopeModule {
                        attack: attack,
                        decay: decay,
                        sustain: sustain,
                        release: release,
                        out: 0.0,
                    });
                    Ok(ModuleHandle::Envelope(self.envelopes.len() - 1))
                } else if let (Some(binding), Some(min), Some(max)) = (
                    object.get("midi_binding"),
                    object.get("min"),
                    object.get("max"),
                ) {
                    let binding = binding
                        .as_i64()
                        .ok_or(format!("failed to parse '{}' as i64", binding))?;
                    if binding < u8::min_value() as i64 || binding > u8::max_value() as i64 {
                        Err("midi_binding is outside range of u8")?
                    }
                    let min = min
                        .as_f64()
                        .ok_or(format!("failed to parse '{}' as f64", min))?;
                    let max = max
                        .as_f64()
                        .ok_or(format!("failed to parse '{}' as f64", max))?;
                    self.midi_bindings.push(MidiBindingModule {
                        binding: binding as u8,
                        min: min,
                        max: max,
                        out: 0.0,
                    });
                    Ok(ModuleHandle::MidiBinding(self.midi_bindings.len() - 1))
                } else if let (Some(shape), Some(amplitude), Some(frequency)) = (
                    object.get("shape"),
                    object.get("amplitude"),
                    object.get("frequency"),
                ) {
                    let shape = match shape {
                        serde_json::Value::String(shape) => Ok(shape),
                        _ => Err("key 'shape' in oscillator must be of type string"),
                    }?;
                    let (amplitude, frequency) = (
                        self.add_json(variables, amplitude)?,
                        self.add_json(variables, frequency)?,
                    );
                    if shape == "sine" {
                        self.sine_oscillators.push(OscillatorModule {
                            amplitude: amplitude,
                            frequency: frequency,
                            phase: 0.0,
                            out: 0.0,
                        });
                        Ok(ModuleHandle::SineOscillator(
                            self.sine_oscillators.len() - 1,
                        ))
                    } else if shape == "sawtooth" {
                        self.sawtooth_oscillators.push(OscillatorModule {
                            amplitude: amplitude,
                            frequency: frequency,
                            phase: 0.0,
                            out: 0.0,
                        });
                        Ok(ModuleHandle::SawtoothOscillator(
                            self.sawtooth_oscillators.len() - 1,
                        ))
                    } else if shape == "square" {
                        self.square_oscillators.push(OscillatorModule {
                            amplitude: amplitude,
                            frequency: frequency,
                            phase: 0.0,
                            out: 0.0,
                        });
                        Ok(ModuleHandle::SquareOscillator(
                            self.square_oscillators.len() - 1,
                        ))
                    } else {
                        Err("key 'shape' in oscillator must be 'sine', 'sawtooth', or 'square'")?
                    }
                } else {
                    Err(format!("failed to parse JSON object {:?}", object))?
                }
            }
            _ => Err(format!("failed to parse JSON value '{}'", value))?,
        }
    }

    fn write_midi_event(&mut self, bytes: &[u8]) {
        if bytes.len() != 3 {
            return;
        }
        let kind = bytes[0] & 0xf0;
        match kind {
            0x80 => {
                // Note off.
                self.on = false;
                self.time_since_toggle = 0.0;
            }
            0x90 => {
                // Note on.
                let note = bytes[1];
                let velocity = bytes[2];
                if velocity == 0 {
                    return;
                }
                let (a4_note, a4_frequency) = (69, 440.0);
                let frequency =
                    a4_frequency * (2.0_f64).powf(((note as isize) - a4_note) as f64 / 12.0);

                self.note = note;
                self.velocity = velocity;
                self.frequency = frequency;
                self.on = true;
                self.time_since_toggle = 0.0;
            }
            0xb0 => {
                // It's from a MIDI controller.
                let key = bytes[1];
                let value = bytes[2] as f64 / 127.0;
                self.midi_bindings
                    .iter_mut()
                    .find(|binding| binding.binding == key)
                    .map(|mut binding| {
                        binding.out = binding.min + (binding.max - binding.min) * value
                    });
            }
            _ => (),
        }
    }

    fn get_module_output(&self, handle: ModuleHandle) -> f64 {
        match handle {
            ModuleHandle::Constant(value) => value,
            ModuleHandle::Envelope(index) => self.envelopes[index].out,
            ModuleHandle::MidiBinding(index) => self.midi_bindings[index].out,
            ModuleHandle::SineOscillator(index) => self.sine_oscillators[index].out,
            ModuleHandle::SawtoothOscillator(index) => self.sawtooth_oscillators[index].out,
            ModuleHandle::SquareOscillator(index) => self.square_oscillators[index].out,
        }
    }

    fn step(&mut self, dt: f64) -> f64 {
        self.time_since_toggle += dt;

        // I'm really fighting the borrow-checker here..
        // Can I make this better?

        // Update modules.
        for index in 0..self.sine_oscillators.len() {
            let osc = &self.sine_oscillators[index];
            let a = self.get_module_output(osc.amplitude);
            let f = self.get_module_output(osc.frequency);
            let mut_osc = &mut self.sine_oscillators[index];
            mut_osc.phase = (mut_osc.phase + f * dt) % 1.0;
            mut_osc.out = a * (mut_osc.phase * 2.0 * PI).sin();
        }
        for index in 0..self.sawtooth_oscillators.len() {
            let osc = &self.sawtooth_oscillators[index];
            let a = self.get_module_output(osc.amplitude);
            let f = self.get_module_output(osc.frequency);
            let mut_osc = &mut self.sawtooth_oscillators[index];
            mut_osc.phase = (mut_osc.phase + f * dt) % 1.0;
            mut_osc.out = a * (-1.0 + 2.0 * mut_osc.phase).sin();
        }
        for index in 0..self.square_oscillators.len() {
            let osc = &self.square_oscillators[index];
            let a = self.get_module_output(osc.amplitude);
            let f = self.get_module_output(osc.frequency);
            let mut_osc = &mut self.square_oscillators[index];
            mut_osc.phase = (mut_osc.phase + f * dt) % 1.0;
            mut_osc.out = a * (if mut_osc.phase < 0.5 { -1.0 } else { 1.0 });
        }
        for index in 0..self.envelopes.len() {
            let env = &self.envelopes[index];
            let t = self.time_since_toggle;
            let (a, d, s, r) = (
                self.get_module_output(env.attack),
                self.get_module_output(env.decay),
                self.get_module_output(env.sustain),
                self.get_module_output(env.release),
            );
            let mut_env = &mut self.envelopes[index];
            mut_env.out = if self.on {
                if t < a {
                    if a == 0.0 {
                        1.0
                    } else {
                        t / a
                    }
                } else if t < a + d {
                    if d == 0.0 {
                        s
                    } else {
                        s + (1.0 - s) * (1.0 - (t - a) / d)
                    }
                } else {
                    s
                }
            } else {
                if t < r {
                    0.0
                } else {
                    if r == 0.0 {
                        0.0
                    } else {
                        s * t / r
                    }
                }
            };
        }

        self.get_module_output(self.out)
    }
}

fn read_config_from_file<P: AsRef<Path>>(path: P) -> Result<Config, Box<dyn Error>> {
    let file = fs::File::open(path)?;
    let reader = io::BufReader::new(file);
    let value = serde_json::from_reader(reader)?;
    Ok(Config::new(&value)?)
}

fn do_paths_refer_to_same_file<P: AsRef<Path>>(a: P, b: P) -> io::Result<bool> {
    Ok(fs::canonicalize(a)? == fs::canonicalize(b)?)
}

fn main() -> Result<(), Box<dyn Error>> {
    // Read config.
    let path = Path::new("test.json");
    let mut config = read_config_from_file(path)?;

    // Open a client and register a port.
    let (client, _status) = jack::Client::new(
        "synth",
        jack::ClientOptions::NO_START_SERVER | jack::ClientOptions::USE_EXACT_NAME,
    )?;
    let input_port = client.register_port("midi_in", jack::MidiIn::default())?;
    let mut output_port = client.register_port("signal_out", jack::AudioOut::default())?;

    // Queue to talk with the JACK thread.
    let (jack_tx, jack_rx) = bounded(1_000_000);

    // 3. define process callback handler
    let fs = client.sample_rate();
    let dt = 1.0 / fs as f64;
    let process_callback = move |_: &jack::Client, ps: &jack::ProcessScope| -> jack::Control {
        while let Ok(new_config) = jack_rx.try_recv() {
            config = new_config;
        }

        let mut current_sample: usize = 0;
        let out = output_port.as_mut_slice(ps);
        let len = out.len();

        // Process MIDI while writing the output.
        for event in input_port.iter(ps) {
            for v in &mut out[current_sample..event.time as usize] {
                *v = config.step(dt) as f32;
            }
            current_sample = event.time as usize;
            config.write_midi_event(event.bytes);
        }

        // Write the remainder of the output.
        for v in &mut out[current_sample..len] {
            *v = config.step(dt) as f32;
        }

        jack::Control::Continue
    };

    let _active_client =
        client.activate_async((), jack::ClosureProcessHandler::new(process_callback))?;

    // Watch the config file for changes.
    let (watcher_tx, watcher_rx) = unbounded();
    let mut watcher: RecommendedWatcher = Watcher::new(watcher_tx, Duration::from_secs(2))?;
    watcher.watch(".", RecursiveMode::NonRecursive)?;

    // In this thread, watch the config file for changes and the changes to the audio thread.
    loop {
        let event = match watcher_rx.recv() {
            Ok(event) => match event {
                Ok(event) => event,
                Err(err) => panic!("watch error: {:?}", err),
            },
            Err(err) => panic!("watch error: {:?}", err),
        };
        if !event.kind.is_modify() {
            continue;
        }
        let config_file_did_change = event
            .paths
            .into_iter()
            .any(|other_path| do_paths_refer_to_same_file(path, &other_path).unwrap());
        let path_buf = std::path::PathBuf::from(path);
        if config_file_did_change {
            let new_config = match read_config_from_file(&path_buf) {
                Ok(new_config) => new_config,
                Err(err) => {
                    eprintln!("{}", err);
                    continue;
                }
            };
            println!("new config!");
            jack_tx.send(new_config)?;
        }
    }

    // active_client.deactivate()?;
}
