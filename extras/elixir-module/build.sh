#!/bin/bash
#
# IHS <3
#

rm -rf Elixir.Math.beam

# Build module.
elixirc math.ex

# Execute app (main).
elixir app.ex


