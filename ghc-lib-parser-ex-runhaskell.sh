#!/usr/bin/env bash


# Using 'stack-exact.yaml'.
stack runhaskell --stack-yaml stack-exact.yaml --package extra --package optparse-applicative CI.hs -- \
      --version-tag 9.6.0.2

# Using 'stack.yaml'.
stack runhaskell --stack-yaml stack.yaml --package extra --package optparse-applicative CI.hs -- \
      --version-tag 9.6.0.2

stack_yamls=( \
  stack-961-927.yaml  \
  stack-944-902.yaml  \
  stack-926-902.yaml  \
  stack-902-8107.yaml \
  stack-810-808.yaml  \
 )

for stack_yaml in "${stack_yamls[@]}"; do
  flavor="$(echo "$stack_yaml" | sed -r 's/^stack-([0-9]+)-(.*)\.yaml$/\1/')"
  stack runhaskell --stack-yaml "$stack_yaml" --package extra --package optparse-applicative CI.hs -- --version-tag "$flavor"
done
