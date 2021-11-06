#!/usr/bin/env sh

set -x # show executed commands
set -e # exit on error

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
