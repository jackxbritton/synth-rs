#!/bin/bash

set -e
trap 'kill $(jobs -p)' EXIT

make
./matrix &
sleep 0.5
jack_connect jack-keyboard:midi_out matrix:midi_in
jack_connect matrix:signal_out system:playback_1
jack_connect matrix:signal_out system:playback_2
read -n 1

