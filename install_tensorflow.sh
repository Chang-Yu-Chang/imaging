#!/usr/bin/env zsh

mamba create -n segmentation python=3.9
mamba activate segmentation
mamba install tensorflow opencv matplotlib
mamba install numpy

# Verify TensorFlow installation
python -c "import tensorflow as tf; print(tf.__version__)"
