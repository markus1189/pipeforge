# pipeforge

<p align="center">
    <img src="./docs/pipeforge.png" alt="pipeforge logo" width="200"/>
</p>

A tool to interactively build pipelines for the command line

Note: this is currently a rough personal prototype, enjoy responsibly


## Run it

```
# Arbitrary shell pipelines with e.g. bash
seq 1 100 | nix run github:markus1189/pipeforge -- bash -c

# Build jq queries interactively
echo '{"foo": 1}\n{"bar": 2}' | nix run github:markus1189/pipeforge -- jq --slurp

# You need to toggle multi arg mode
echo '{"foo":{"bar":{"qux":{"quz":42}}}}' | nix run github:markus1189/pipeforge -- fx

# Also works without any input
nix run github:markus1189/pipeforge -- bash -c
```
