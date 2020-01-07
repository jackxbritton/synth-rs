extern crate crossbeam_channel;
extern crate jack;
use crossbeam_channel::{bounded, unbounded};
use notify::{RecommendedWatcher, RecursiveMode, Watcher};
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::f64::consts::PI;
use std::fs;
use std::io;
use std::path::Path;
use std::time::Duration;

enum Module {
    Constant(f64),
    Envelope(EnvelopeModule),
    SineOscillator(OscillatorModule),
    SawtoothOscillator(OscillatorModule),
    SquareOscillator(OscillatorModule),

    Variable(String),
}

enum ModuleHandle {
    Constant(f64),
    Envelope(usize),
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

    envelopes: RefCell<Vec<EnvelopeModule>>,
    sine_oscillators: RefCell<Vec<OscillatorModule>>,
    sawtooth_oscillators: RefCell<Vec<OscillatorModule>>,
    square_oscillators: RefCell<Vec<OscillatorModule>>,

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
            envelopes: RefCell::new(Vec::new()),
            sine_oscillators: RefCell::new(Vec::new()),
            sawtooth_oscillators: RefCell::new(Vec::new()),
            square_oscillators: RefCell::new(Vec::new()),
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
            serde_json::Value::String(key) => {
                let variable = variables
                    .get(key)
                    .ok_or(format!("unrecognized variable key '{}'", key))?;
                Ok(*variable)
            }
            serde_json::Value::Object(object) => {
                if let (Some(shape), Some(amplitude), Some(frequency)) = (
                    object.get("shape"),
                    object.get("amplitude"),
                    object.get("frequency"),
                ) {
                    let shape = match shape {
                        serde_json::Value::String(shape) => Ok(shape),
                        _ => Err("key 'shape' in oscillator must be of type string"),
                    }?;
                    if shape == "sine" {
                        self.sine_oscillators.borrow_mut().push(OscillatorModule {
                            amplitude: self.add_json(variables, amplitude)?,
                            frequency: self.add_json(variables, frequency)?,
                            phase: 0.0,
                            out: 0.0,
                        });
                        Ok(ModuleHandle::SineOscillator(
                            self.sine_oscillators.borrow().len() - 1,
                        ))
                    } else {
                        Err("key 'shape' in oscillator must be 'sine', 'sawtooth', or 'square'")?
                    }
                } else {
                    Err(format!("failed to parse JSON object {:?}", object))?
                }
                /*
                let variables = match value
                    .get("shape")
                    .ok_or("top-level key 'variables' not found")?
                {
                    serde_json::Value::Object(variables) => Ok(variables),
                    _ => Err("top-level key 'variables' must be an object"),
                }?;
                */
            }
            _ => Err(format!("failed to parse JSON value {}", value))?,
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
            _ => (),
        }
    }

    fn get_module_output(&self, handle: ModuleHandle) -> f64 {
        match handle {
            ModuleHandle::Constant(value) => value,
            ModuleHandle::Envelope(index) => self.envelopes.borrow()[index].out,
            ModuleHandle::SineOscillator(index) => self.sine_oscillators.borrow()[index].out,
            ModuleHandle::SawtoothOscillator(index) => {
                self.sawtooth_oscillators.borrow()[index].out
            }
            ModuleHandle::SquareOscillator(index) => self.square_oscillators.borrow()[index].out,
        }
    }

    fn step(&mut self, dt: f64) -> f64 {
        self.time_since_toggle += dt;

        // I'm really fighting the borrow-checker here..
        // Can I make this better?

        // Update modules.

        for osc in self.sine_oscillators.borrow_mut().iter_mut() {
            let a = self.get_module_output(osc.amplitude);
            let f = self.get_module_output(osc.frequency);
            osc.phase = (osc.phase + f * dt) % 1.0;
            osc.out = a * (osc.phase * 2.0 * PI).sin();
        }
        /*
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
        */

        self.get_module_output(self.out)
    }
}

// impl Operator for EnvelopeOperator {
//     fn update(&mut self, config: &Config) {
//         let t = config.time_since_toggle;
//         let (a, d, s, r) = (
//             config.get(&self.attack),
//             config.get(&self.decay),
//             config.get(&self.sustain),
//             config.get(&self.release),
//         );
//         self.out = if config.on {
//             if t < a {
//                 if a == 0.0 {
//                     1.0
//                 } else {
//                     t / a
//                 }
//             } else if t < a + d {
//                 if d == 0.0 {
//                     s
//                 } else {
//                     s + (1.0 - s) * (1.0 - (t - a) / d)
//                 }
//             } else {
//                 s
//             }
//         } else {
//             if t < r {
//                 0.0
//             } else {
//                 if r == 0.0 {
//                     0.0
//                 } else {
//                     s * t / r
//                 }
//             }
//         };
//     }
// }

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
    let config = read_config_from_file(path)?;

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
