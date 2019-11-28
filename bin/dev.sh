#!/bin/sh

BASE=$(dirname "$0")/..

elm-live $BASE/src/Main.elm --open --dir $BASE/public/ -- --debug --output=$BASE/public/index.js