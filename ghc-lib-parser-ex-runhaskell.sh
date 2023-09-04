#!/usr/bin/env bash

set -euo pipefail

# Needs bash >=4. Use '/usr/local/bin/bash'.
declare -A builds=( \
   [stack-808-865.yaml]=8.8.0.0
   [stack-810-808.yaml]=8.10.0.0
   [stack-902-8107.yaml]=9.0.0.0
   [stack-926-902.yaml]=9.2.0.0
   [stack-944-902.yaml]=9.4.0.0
   [stack-exact.yaml]=9.6.0.0
   [stack.yaml]=9.6.0.0
)

for stack_yaml in "${!builds[@]}"; do
  version="${builds[$stack_yaml]}"
  set -x
  stack runhaskell --stack-yaml "$stack_yaml" --package extra --package optparse-applicative CI.hs -- --stack-yaml "$stack_yaml" --version-tag "$version"
  set +x
done
