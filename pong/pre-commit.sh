#!/usr/bin/env bash

fourmolu --mode inplace $(git ls-files '*.hs')
