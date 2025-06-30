#!/bin/sh

# script path
SCRIPT_PATH=$(
    cd -- "$(
        dirname -- "${BASH_SOURCE[0]}"
    )" &>/dev/null && pwd
)

cd $SCRIPT_PATH

# setup/update conda environment
bash atlas.sh

# # install miniconda if not installed
# if ! command -v conda &>/dev/null; then
#     bash ./miniconda.sh
# fi

# # environment specifications
# ENV_NAME="atlas"
# ENV_FILE="atlas.yaml"

# # create or update conda environment
# if conda info --envs | grep -q "^$ENV_NAME\s"; then
#     echo "$ENV_NAME already exists. Updating environment with "$ENV_FILE"."
#     conda env update --name $ENV_NAME --file $ENV_FILE
# else
#     echo "Creating $ENV_NAME environment with "$ENV_FILE"."
#     conda env create -f atlas.yaml
# fi

# set environment variables
# from .env file

# # activate the environment
# source activate base
# conda activate $ENV_NAME

# # install github packages
# Rscript ./github.r

# setup project root
cd ..
Rscript env/setup.r

# create database
if ! test -f database/atlas.db; then
    bash database/setup.sh
fi
