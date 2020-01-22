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

type NodeHandle = usize;

struct EnvelopeModule {
    attack: NodeHandle,
    decay: NodeHandle,
    sustain: NodeHandle,
    release: NodeHandle,
    out: NodeHandle,
}

struct MidiBindingModule {
    binding: u8,
    min: f64,
    max: f64,
    out: NodeHandle,
}

struct OscillatorModule {
    amplitude: NodeHandle,
    frequency: NodeHandle,
    out: NodeHandle,

    phase: f64,
}

struct BiquadFilterModule {
    f0: NodeHandle,
    q: NodeHandle,
    input: NodeHandle,
    out: NodeHandle,

    a1: f64,
    a2: f64,
    b0: f64,
    b1: f64,
    b2: f64,
    s1: f64,
    s2: f64,
}

struct Mixer {
    children: Vec<NodeHandle>,
    out: NodeHandle,
}

struct Config {
    note: u8,
    velocity: u8,
    frequency: f64,
    on: bool,
    time_since_toggle: f64,

    nodes: Vec<f64>,
    envelopes: Vec<EnvelopeModule>,
    midi_bindings: Vec<MidiBindingModule>,
    sine_oscillators: Vec<OscillatorModule>,
    sawtooth_oscillators: Vec<OscillatorModule>,
    square_oscillators: Vec<OscillatorModule>,
    low_pass_filters: Vec<BiquadFilterModule>,
    high_pass_filters: Vec<BiquadFilterModule>,
    all_pass_filters: Vec<BiquadFilterModule>,
    mixers: Vec<Mixer>,
}

impl Config {
    fn new(value: &serde_json::Value) -> Result<Config, Box<dyn Error>> {
        let mut config = Config {
            note: 0,
            velocity: 0,
            frequency: 0.0,
            on: false,
            time_since_toggle: 0.0,
            // nodes: Vec::new(),
            nodes: vec![0.0], // TODO 0 is the synth node (for now).
            envelopes: Vec::new(),
            midi_bindings: Vec::new(),
            sine_oscillators: Vec::new(),
            sawtooth_oscillators: Vec::new(),
            square_oscillators: Vec::new(),
            low_pass_filters: Vec::new(),
            high_pass_filters: Vec::new(),
            all_pass_filters: Vec::new(),
            mixers: Vec::new(),
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
            .collect::<Result<HashMap<_, _>, Box<dyn Error>>>()?;

        let out = value.get("out").ok_or("top-level 'key' out not found")?;
        let out_index = config.add_json(&variables_map, &out)?; // TODO Output node ends up on top.

        Ok(config)
    }

    fn add_json(
        &mut self,
        variables: &HashMap<String, NodeHandle>,
        value: &serde_json::Value,
    ) -> Result<NodeHandle, Box<dyn Error>> {
        match value {
            serde_json::Value::Number(number) => {
                let number = number
                    .as_f64()
                    .ok_or(format!("failed to parse '{}' as f64", number))?;
                self.nodes.push(number);
                Ok(self.nodes.len() - 1)
            }
            serde_json::Value::String(key) => {
                if key == "synth" {
                    return Ok(0); // TODO 0 is the synth node for now.
                }
                let variable = variables
                    .get(key)
                    .ok_or(format!("unrecognized variable key '{}'", key))?;
                Ok(*variable)
            }
            serde_json::Value::Array(array) => {
                // TODO Rewrite this functionally.
                let mut mixer = Mixer {
                    children: Vec::new(),
                    out: 0,
                };
                for element in array {
                    mixer.children.push(self.add_json(variables, element)?);
                }
                self.nodes.push(0.0);
                let out = self.nodes.len() - 1;
                mixer.out = out;
                self.mixers.push(mixer);
                Ok(out)
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
                    self.nodes.push(0.0);
                    let out = self.nodes.len() - 1;
                    self.envelopes.push(EnvelopeModule {
                        attack: attack,
                        decay: decay,
                        sustain: sustain,
                        release: release,
                        out: out,
                    });
                    Ok(out)
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
                    let default = object
                        .get("default")
                        .map(|x| x.as_f64().unwrap_or((min + max) / 2.0))
                        .unwrap_or((min + max) / 2.0);
                    self.nodes.push(default);
                    let out = self.nodes.len() - 1;
                    self.midi_bindings.push(MidiBindingModule {
                        binding: binding as u8,
                        min: min,
                        max: max,
                        out: out,
                    });
                    Ok(out)
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
                    self.nodes.push(0.0);
                    let out = self.nodes.len() - 1;
                    if shape == "sine" {
                        self.sine_oscillators.push(OscillatorModule {
                            amplitude: amplitude,
                            frequency: frequency,
                            phase: 0.0,
                            out: out,
                        });
                    } else if shape == "sawtooth" {
                        self.sawtooth_oscillators.push(OscillatorModule {
                            amplitude: amplitude,
                            frequency: frequency,
                            phase: 0.0,
                            out: out,
                        });
                    } else if shape == "square" {
                        self.square_oscillators.push(OscillatorModule {
                            amplitude: amplitude,
                            frequency: frequency,
                            phase: 0.0,
                            out: out,
                        });
                    } else {
                        Err("key 'shape' in oscillator must be 'sine', 'sawtooth', or 'square'")?
                    }
                    Ok(out)
                } else if let (Some(kind), Some(f0), Some(q), Some(input)) = (
                    object.get("filter"),
                    object.get("f0"),
                    object.get("q"),
                    object.get("input"),
                ) {
                    // Biquad filter.

                    let kind = match kind {
                        serde_json::Value::String(kind) => Ok(kind),
                        _ => Err(
                            "key 'filter' in filter must be 'low_pass', 'high_pass', or 'all_pass'",
                        ),
                    }?;
                    let (f0, q, input) = (
                        self.add_json(variables, f0)?,
                        self.add_json(variables, q)?,
                        self.add_json(variables, input)?,
                    );
                    self.nodes.push(0.0);
                    let out = self.nodes.len() - 1;
                    let filter = BiquadFilterModule {
                        f0: f0,
                        q: q,
                        input: input,
                        out: out,
                        a1: 0.0,
                        a2: 0.0,
                        b0: 0.0,
                        b1: 0.0,
                        b2: 0.0,
                        s1: 0.0,
                        s2: 0.0,
                    };
                    if kind == "low_pass" {
                        self.low_pass_filters.push(filter);
                        Ok(out)
                    } else if kind == "high_pass" {
                        self.high_pass_filters.push(filter);
                        Ok(out)
                    } else if kind == "all_pass" {
                        self.all_pass_filters.push(filter);
                        Ok(out)
                    } else {
                        Err(
                            "key 'filter' in filter must be 'low_pass', 'high_pass', or 'all_pass'",
                        )?
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

                self.nodes[0] = frequency;
            }
            0xb0 => {
                // It's from a MIDI controller.
                let key = bytes[1];
                let value = bytes[2] as f64 / 127.0;
                if let Some(binding) = self
                    .midi_bindings
                    .iter()
                    .find(|binding| binding.binding == key)
                {
                    self.nodes[binding.out] = binding.min + (binding.max - binding.min) * value
                }
            }
            _ => (),
        }
    }

    fn step(&mut self, dt: f64) -> f64 {
        self.time_since_toggle += dt;

        // I'm really fighting the borrow-checker here..
        // Can I make this better?

        // Update modules.
        for index in 0..self.sine_oscillators.len() {
            let osc = &self.sine_oscillators[index];
            let a = self.nodes[osc.amplitude];
            let f = self.nodes[osc.frequency];
            let osc = &mut self.sine_oscillators[index];
            osc.phase = (osc.phase + f * dt) % 1.0;
            self.nodes[osc.out] = a * (osc.phase * 2.0 * PI).sin();
        }
        for index in 0..self.sawtooth_oscillators.len() {
            let osc = &self.sawtooth_oscillators[index];
            let a = self.nodes[osc.amplitude];
            let f = self.nodes[osc.frequency];
            let osc = &mut self.sawtooth_oscillators[index];
            osc.phase = (osc.phase + f * dt) % 1.0;
            self.nodes[osc.out] = a * (-1.0 + 2.0 * osc.phase).sin();
        }
        for index in 0..self.square_oscillators.len() {
            let osc = &self.square_oscillators[index];
            let a = self.nodes[osc.amplitude];
            let f = self.nodes[osc.frequency];
            let osc = &mut self.square_oscillators[index];
            osc.phase = (osc.phase + f * dt) % 1.0;
            self.nodes[osc.out] = a * (if osc.phase < 0.5 { -1.0 } else { 1.0 });
        }
        for index in 0..self.envelopes.len() {
            let env = &self.envelopes[index];
            let t = self.time_since_toggle;
            let (a, d, s, r) = (
                self.nodes[env.attack],
                self.nodes[env.decay],
                self.nodes[env.sustain],
                self.nodes[env.release],
            );
            let env = &mut self.envelopes[index];
            self.nodes[env.out] = if self.on {
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
                if t >= r {
                    0.0
                } else {
                    if r == 0.0 {
                        0.0
                    } else {
                        s * (1.0 - t / r)
                    }
                }
            };
        }
        for index in 0..self.low_pass_filters.len() {
            let filter = &self.low_pass_filters[index];
            let f0 = self.nodes[filter.f0];
            let q = self.nodes[filter.q];
            let input = self.nodes[filter.input];
            let filter = &mut self.low_pass_filters[index];

            let w0 = 2.0 * PI * f0 * dt as f64;
            let c = w0.cos();
            let s = w0.sin();
            let a = s / (2.0 * q);
            let a0 = 1.0 + a;
            filter.a1 = -2.0 * c / a0;
            filter.a2 = (1.0 - a) / a0;
            filter.b0 = (1.0 - c) / 2.0 / a0;
            filter.b1 = (1.0 - c) / a0;
            filter.b2 = (1.0 - c) / 2.0 / a0;

            let out = input * filter.b0 + filter.s1;
            self.nodes[filter.out] = out;
            filter.s1 = input * filter.b1 + out * filter.a1 + filter.s2;
            filter.s2 = input * filter.b2 - out * filter.a2;
        }
        for index in 0..self.high_pass_filters.len() {
            let filter = &self.high_pass_filters[index];
            let f0 = self.nodes[filter.f0];
            let q = self.nodes[filter.q];
            let input = self.nodes[filter.input];
            let filter = &mut self.high_pass_filters[index];

            let w0 = 2.0 * PI * f0 * dt as f64;
            let c = w0.cos();
            let s = w0.sin();
            let a = s / (2.0 * q);
            let a0 = 1.0 + a;
            filter.a1 = -2.0 * c / a0;
            filter.a2 = (1.0 - a) / a0;
            filter.b0 = (1.0 - c) / 2.0 / a0;
            filter.b1 = (-1.0 - c) / a0;
            filter.b2 = (1.0 + c) / 2.0 / a0;

            let out = input * filter.b0 + filter.s1;
            self.nodes[filter.out] = out;
            filter.s1 = input * filter.b1 + out * filter.a1 + filter.s2;
            filter.s2 = input * filter.b2 - out * filter.a2;
        }
        for index in 0..self.all_pass_filters.len() {
            let filter = &self.all_pass_filters[index];
            let f0 = self.nodes[filter.f0];
            let q = self.nodes[filter.q];
            let input = self.nodes[filter.input];
            let filter = &mut self.all_pass_filters[index];

            let w0 = 2.0 * PI * f0 * dt as f64;
            let c = w0.cos();
            let s = w0.sin();
            let a = s / (2.0 * q);
            let a0 = 1.0 + a;
            filter.a1 = -2.0 * c / a0;
            filter.a2 = (1.0 - a) / a0;
            filter.b0 = (1.0 - a) / a0;
            filter.b1 = -2.0 * c / a0;
            filter.b2 = (1.0 - a) / a0;

            let out = input * filter.b0 + filter.s1;
            self.nodes[filter.out] = out;
            filter.s1 = input * filter.b1 + out * filter.a1 + filter.s2;
            filter.s2 = input * filter.b2 - out * filter.a2;
        }
        for index in 0..self.mixers.len() {
            let mut sum = 0.0;
            for child in &self.mixers[index].children {
                sum += self.nodes[*child];
            }
            self.nodes[self.mixers[index].out] = sum;
        }

        self.nodes[self.nodes.len() - 1] // TODO
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
    let mut watcher: RecommendedWatcher = Watcher::new(watcher_tx, Duration::from_secs(1))?;
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
