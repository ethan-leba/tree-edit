#!/usr/bin/env sh

set -x # show executed commands
set -e # exit on error

# This directory gets created and stored on the developer's machine and it can't
# be deleted by anyone else than root
#rm -rf .cask/
#cask

TERM=dumb SHELL=sh cask exec emacs \
    -Q \
    -batch \
    -f package-initialize \
    -l buttercup \
    --directory "tests/" \
    -l tree-edit \
    -l "tests/setup.el" \
    -f buttercup-run-discover \
    "tests/"
