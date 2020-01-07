#include <stdio.h>
#include <fstream>
#include <map>
#include <math.h>
#include <string>
#include <unistd.h>
#include <vector>

#include <jack/jack.h>
#include <jack/midiport.h>

#include "json.hpp"
using json = nlohmann::json;

// TODO Some kind of handle type instead of double *?
// template <typename T>
// struct Handle {
//     std::vector<T> *array;
//     size_t index;
//     T &operator*() {
//         return *array[index];
//     }
// };

struct Synth {

	unsigned char note, velocity;
	bool on;
	double time_since_toggle;

	double frequency, phase;

};

struct Envelope {
	double *attack, *decay, *sustain, *release;
	double out;
};

struct Oscillator {
	double *amplitude, *frequency, *sync, phase, out;
};

struct Transform {
	double *scale, *offset, *input;
	double out;
};

struct Mixer {
	std::vector<double *> inputs;
	double out;
};

const size_t RESERVE_SIZE = 64;

struct Matrix {

	Synth synth;

	std::vector<double> constants;
	std::vector<Oscillator> sine_oscillators, sawtooth_oscillators, square_oscillators;
	std::vector<Envelope> envelopes;
	std::vector<Transform> transforms;
	std::vector<Mixer> mixers;

	double *out;

	Matrix() {
		constants.reserve(RESERVE_SIZE);
		sine_oscillators.reserve(RESERVE_SIZE);
		sawtooth_oscillators.reserve(RESERVE_SIZE);
		square_oscillators.reserve(RESERVE_SIZE);
		envelopes.reserve(RESERVE_SIZE);
		transforms.reserve(RESERVE_SIZE);
		mixers.reserve(RESERVE_SIZE);
	}

	double *deserialize_node(std::map<std::string, double *> *variables, json j) {
		
		// Identify the type of node, push a struct onto a vector, and return a
		// pointer to its output value.

		// Check if it's a constant.
		if (j.is_number()) {
			constants.push_back(j);
			return &constants[constants.size()-1];
		}

		// Check if it's a variable.
		if (j.is_string()) {

			// TODO 'synth' is hardcoded for now.
			if (j == "synth") return &synth.frequency;

			if (variables->find(j) == variables->end()) {
				fprintf(stderr, "deserialize() failed, '%s' is not a variable\n", std::string(j).c_str());
				return NULL;
			}
			return (*variables)[j];
		}

		// Check if it's a mixer.
		if (j.is_array()) {
			Mixer mixer;
			for (auto &element : j) {
				double *in = deserialize_node(variables, element);
				if (!in) return NULL;
				mixer.inputs.push_back(in);
			}
			mixers.push_back(mixer);
			return &mixers[mixers.size()-1].out;
		}

		// Everything else is represented with an object.
		if (!j.is_object()) return NULL;
		
		// Check if it's an oscillator.
		if (
			j.find("shape")     != j.end() &&
			j.find("amplitude") != j.end() &&
			j.find("frequency") != j.end()
		) {

			double *amplitude = deserialize_node(variables, j["amplitude"]);
			double *frequency = deserialize_node(variables, j["frequency"]);
			if (!amplitude || !frequency) return NULL;

			// If sync isn't found, set it to frequency so we don't have to check for NULL later.
			double *sync = frequency;
			if (j.find("sync") != j.end()) {
				sync = deserialize_node(variables, j["sync"]);
				if (!sync) return NULL;
			}

			Oscillator osc {
				amplitude,
				frequency,
				sync,
				0.0, 0.0
			};

			if (j["shape"] == "sine") {
				sine_oscillators.push_back(osc);
				return &sine_oscillators[sine_oscillators.size()-1].out;
			}
			if (j["shape"] == "sawtooth") {
				sawtooth_oscillators.push_back(osc);
				return &sawtooth_oscillators[sawtooth_oscillators.size()-1].out;
			}
			if (j["shape"] == "square") {
				square_oscillators.push_back(osc);
				return &square_oscillators[square_oscillators.size()-1].out;
			}

			fprintf(stderr, "deserialize() failed, unrecognized oscillator shape '%s'", std::string(j["shape"]).c_str());
			return NULL;
		}

		// Check if it's an envelope.
		if (
			j.find("attack")  != j.end() &&
			j.find("decay")   != j.end() &&
			j.find("sustain") != j.end() &&
			j.find("release") != j.end()
		) {
			Envelope env {
				deserialize_node(variables, j["attack"]),
				deserialize_node(variables, j["decay"]),
				deserialize_node(variables, j["sustain"]),
				deserialize_node(variables, j["release"])
			};
			if (!env.attack || !env.decay || !env.sustain || !env.release) return NULL;
			envelopes.push_back(env);
			return &envelopes[envelopes.size()-1].out;
		}

		// Check if it's a transform.
		if (
			j.find("input") != j.end() && (
				j.find("scale")  != j.end() ||
				j.find("offset") != j.end()
			)
		) {
			if      (j.find("scale")  == j.end()) j["scale"]  = 1.0;
			else if (j.find("offset") == j.end()) j["offset"] = 0.0;
			Transform transform {
				deserialize_node(variables, j["scale"]),
				deserialize_node(variables, j["offset"]),
				deserialize_node(variables, j["input"]),
			};
			if (!transform.scale || !transform.offset || !transform.input) return NULL;
			transforms.push_back(transform);
			return &transforms[transforms.size()-1].out;
		}

		fprintf(stderr, "deserialize() failed, unrecognized node '%s'", std::string(j).c_str());
		return NULL;
	}

	bool deserialize(json j) {
		if (!j.is_object()) return false;

		// The 'variables' key holds modules which can be referred to by their name in 'out'.
		// TODO Self-referential variables will be a problem.
		if (j.find("variables") == j.end() || !j["variables"].is_object()) {
			fprintf(stderr, "deserialize() failed, missing top-level key 'variables'\n");
			return false;
		}
		std::map<std::string, double *> variables;
		for (auto &element : j["variables"].items()) {
			double *value = deserialize_node(&variables, element.value());
			if (!value) return false;
			variables.insert(std::pair<std::string, double *>(element.key(), value));
		}

		if (j.find("out") == j.end()) {
			fprintf(stderr, "deserialize() failed, missing top-level key 'out'\n");
			return false;
		}
		out = deserialize_node(&variables, j["out"]);
		if (!out) return false;

		return true;
	}

	void write_midi_event(unsigned char *buffer, int size) {

		if (size != 3) return;
		unsigned char type = buffer[0] & 0xF0;
		unsigned char note = buffer[1];
		unsigned char velocity = buffer[2];

		if (type == 0x80 || (type == 0x90 && velocity == 0)) {
			// Note off.
			if (note != synth.note) return;
			synth.on = false;
			synth.time_since_toggle = 0.0;
		} else if (type == 0x90) {
			// Note on.
			unsigned char a4_note = 69;
			double a4_frequency = 440.0;
			double frequency = a4_frequency * pow(2.0, (note - a4_note) / 12.0);
			synth.note = note;
			synth.velocity = velocity;
			synth.on = true;
			synth.time_since_toggle = 0.0;
			synth.frequency = frequency;
		}

		// TODO Certain operators will accept MIDI.

	}

	double step(double dt) {

		for (auto &env : envelopes) {

			double &t = synth.time_since_toggle;
			double a = *env.attack;
			double d = *env.decay;
			double s = *env.sustain;
			double r = *env.release;
			if (synth.on) {
				if (t < a) {
					if (a == 0.0) env.out = 1.0;
					else          env.out = t / a;
				} else if (t < a + d) {
					if (d == 0.0) env.out = s;
					else          env.out = s + (1.0 - s) * (t - a) / d;
				} else env.out = s;
			} else {
				if (t < r) {
					if (r == 0.0) env.out = 0.0;
					else          env.out = s * (1.0 - t / r);
				} else env.out = 0.0;
			}

			t += dt;

		}

		// TODO There are notes online on how to make sync good. Apply them.
		for (auto &osc : sine_oscillators) {
			if (*osc.sync == 0.0) continue;
			osc.out = *osc.amplitude * sin(2.0 * M_PI * osc.phase);
			double rollover = *osc.frequency / *osc.sync;
			osc.phase = fmod(osc.phase + *osc.frequency * dt, rollover);
		}
		for (auto &osc : sawtooth_oscillators) {
			if (*osc.sync == 0.0) continue;
			osc.out = *osc.amplitude * (-1.0 + 2.0 * osc.phase);
			double rollover = *osc.frequency / *osc.sync;
			osc.phase = fmod(osc.phase + *osc.frequency * dt, rollover);
		}
		for (auto &osc : square_oscillators) {
			if (*osc.sync == 0.0) continue;
			osc.out = *osc.amplitude * (osc.phase < 0.5 ? -1.0 : 1.0);
			double rollover = *osc.frequency / *osc.sync;
			osc.phase = fmod(osc.phase + *osc.frequency * dt, rollover);
		}
		for (auto &transform : transforms) {
			transform.out = *transform.input * *transform.scale + *transform.offset;
		}
		for (auto &mixer : mixers) {
			mixer.out = 0.0;
			for (double *input : mixer.inputs) {
				mixer.out += *input;
			}
		}

		return *out;
	}

};

struct JackContext {
	double dt;
	jack_port_t *input_port, *output_port;
	Matrix matrix;
};

int jack_process(jack_nframes_t nframes, void *void_context) {

	JackContext *context = (JackContext *) void_context;
	void *in = jack_port_get_buffer(context->input_port, nframes);
	float *out = (float *) jack_port_get_buffer(context->output_port, nframes);

	jack_nframes_t event_count = jack_midi_get_event_count(in);
	jack_nframes_t current_frame = 0;

	for (int i = 0; i < event_count; i++) {

		// Read an event and process frames up until its time.
		jack_midi_event_t event;
		jack_midi_event_get(&event, in, i);
		for (; current_frame < event.time; current_frame++) {
			out[current_frame] = context->matrix.step(context->dt);
		}

		// Now process.
		context->matrix.write_midi_event(event.buffer, event.size);

	}

	// Finish processing frames.
	for (; current_frame < nframes; current_frame++) {
		out[current_frame] = context->matrix.step(context->dt);
	}
	return 0;      
}

void jack_shutdown(void *void_context) {
	JackContext context = *((JackContext *) void_context);
	exit(1);
}

int main(int argc, char *argv[]) {

	// Deserialize matrix.
	std::ifstream i("test.json");
	json j;
	i >> j;

	// Matrix uses pointers internally! You can't just copy it around.
	JackContext context;
	if (!context.matrix.deserialize(j)) return 1;

	// Very important so that 'sync' doesn't divide by zero.
	context.matrix.synth.frequency = 1.0;

	// Initialize the JACK client.

	jack_status_t status;
	jack_client_t *client = jack_client_open(
		"matrix",
		(jack_options_t) (JackNoStartServer | JackUseExactName),
		&status,
		NULL
	);
	if (!client) {
		fprintf(stderr, "jack_client_open() failed, did you start the server?\n");
		return 1;
	}

	context.dt = 1.0 / jack_get_sample_rate(client);

	context.input_port = jack_port_register(
		client,
		"midi_in",
		JACK_DEFAULT_MIDI_TYPE,
		JackPortIsInput,
		0
	);
	if (!context.input_port) {
		fprintf(stderr, "jack_port_register() failed, weird.\n");
		return 1;
	}

	context.output_port = jack_port_register(
		client,
		"signal_out",
		JACK_DEFAULT_AUDIO_TYPE,
		JackPortIsOutput,
		0
	);
	if (!context.output_port) {
		fprintf(stderr, "jack_port_register() failed, weird.\n");
		return 1;
	}

	jack_set_process_callback(client, jack_process, &context);
	jack_on_shutdown(client, jack_shutdown, &context);

	if (jack_activate(client) != 0) {
		fprintf(stderr, "jack_activate() failed, weird.\n");
		return 1;
	}

	while (true) {
		sleep(1);
		printf("tick\n");
		sleep(1);
		printf("tock\n");
	}

	jack_client_close(client);

	return 0;
}
