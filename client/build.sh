#!/bin/bash

nix-build
closure-compiler result/bin/home.jsexe/all.js  > static/home.min.js