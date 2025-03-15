#!/bin/env sh

# script path
script_path=$(
    cd -- "$(
        dirname -- "${BASH_SOURCE[0]}"
    )" &>/dev/null && pwd
)

# generate csvs
cd $script_path/..
Rscript database/setup.r
cd $script_path

# create database
sqlite3 atlas.db <setup.sqlite

# # output database
# sqlite3 atlas.db <output.sqlite
# Rscript output.r
