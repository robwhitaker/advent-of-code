#!/usr/bin/env bash

if [ $# -ne 1 ]; then
    echo "Missing destination folder."
    exit 1
fi

script_dir=$(dirname "$0")

if [ ! -d "$1" ]; then
    mkdir -p "$1"
    ${EDITOR:-vi} "$1/input.txt"
    cp "$script_dir/_template.hs" "$1/solution.hs"
    echo "Created folder: $1/."
    echo "|---- input.txt"
    echo "|---- solution.hs"
else
    echo "Folder already exists."
fi
