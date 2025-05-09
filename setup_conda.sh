#!/usr/bin/env zsh

# This script will install conda/mamba to your root directory. Skip this step if you already have conda/mamba
# This script installs the conda/mamba environments required for assembling bacterial genome from nanopore long-read sequences
# You only need to run this script once

cd

# 1. Install miniforge
mkdir -p ~/miniforge
curl -L https://github.com/conda-forge/miniforge/releases/download/23.3.1-1/Miniforge3-23.3.1-1-MacOSX-arm64.sh -o ~/miniforge/miniforge.sh
zsh ~/miniforge/miniforge.sh
rm -rf ~/miniforge

# 2. Install miniconda for macOS with Apple M1 chip
mkdir -p ~/miniconda3
curl https://repo.anaconda.com/miniconda/Miniconda3-py312_25.1.1-2-MacOSX-arm64.sh -o ~/miniconda3/miniconda.sh
zsh ~/miniconda3/miniconda.sh -b -u -p ~/miniconda
rm -rf ~/miniconda3/miniconda.sh
~/miniconda/bin/conda init zsh # Initiate the conda
conda --version # this should return conda 23.7.4

# 3. Create a conda env for osx-64 called intel_env
# Create a intel osx64 environment
conda create -n intel_env
conda activate intel_env

## configure architecture
conda config --env --set subdir osx-64

## configure channels for Bioconda
conda config --env --add channels defaults
conda config --env --add channels bioconda
conda config --env --add channels conda-forge

# The .condarc should look like this
cat ~/miniconda/envs/intel_env/.condarc
# subdir: osx-64
# channels:
#   - conda-forge
#   - bioconda
#   - defaults

# 4. Install mamba under the created conda env intel_env
conda install -c conda-forge mamba
mamba shell init
mamba config --env --set subdir osx-64
# Note that this command only modifies .bash_profile if you work in bash. For zsh environment, copy-paste the generated text from .bash_profile to .zshrc. Close and re-open a shell session. This step is essential for mamba activate to work
mamba env list

# 5. Install docker
# brew install docker # use homebrew to install docker
# cd ~/bioinformatics
# wget https://desktop.docker.com/mac/main/arm64/Docker.dmg?utm_source=docker&utm_medium=webreferral&utm_campaign=docs-driven-download-mac-arm64 -O Docker.dmg
# sudo hdiutil attach Docker.dmg
# sudo /Volumes/Docker/Docker.app/Contents/MacOS/install
# sudo hdiutil detach /Volumes/Docker
