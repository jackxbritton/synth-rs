#!/bin/bash

set -e
trap 'kill $(jobs -p)' EXIT

cargo build
RUST_BACKTRACE=1 ./target/debug/synth &
sleep 0.5
jack_connect jack-keyboard:midi_out synth:midi_in
jack_connect 'a2j:nanoKONTROL2 [24] (capture): nanoKONTROL2 MIDI 1' synth:midi_in
jack_connect synth:signal_out system:playback_1
jack_connect synth:signal_out system:playback_2
read -n 1

