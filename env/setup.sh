#!/bin/sh

# script path
SCRIPT_PATH=$(
    cd -- "$(
        dirname -- "${BASH_SOURCE[0]}"
    )" &>/dev/null && pwd
)

cd $SCRIPT_PATH

# while loop to create each environment
ENV_NAME="atlas"
ENV_FILE="atlas.yaml"

# install miniconda if not installed
if ! command -v conda &>/dev/null; then
    bash ./miniconda.sh
fi

# create or update conda environment
if conda info --envs | grep -q "^$ENV_NAME\s"; then
    echo "$ENV_NAME already exists. Updating environment with "$ENV_FILE"."
    conda env update --name $ENV_NAME --file $ENV_FILE
else
    echo "Creating $ENV_NAME environment with "$ENV_FILE"."
    conda env create -f atlas.yaml
fi

# activate the environment
source activate base
conda activate $ENV_NAME

# # install github packages
# Rscript ./github.r

# setup project root
Rscript ./setup.r
