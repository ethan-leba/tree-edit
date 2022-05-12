#!/usr/bin/env sh

set -x # show executed commands
set -e # exit on error

export TESTING=1

cask emacs \
    -q \
    -batch \
    -f package-initialize \
    -l buttercup \
    --directory "tests/" \
    -l tree-edit \
    -l "tests/setup.el" \
    -f buttercup-run-discover \
    "tests/"
