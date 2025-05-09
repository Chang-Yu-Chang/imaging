#!/usr/bin/env zsh



# Install mamba env
mamba create -n stitch
mamba activate stitch
mamba install python=3.10
mamba install -c conda-forge opencv

#
folder_data="/Users/cychang2/Dropbox/lab/imaging/data/"
python stitch_images.py $folder_data/stitch/115 $folder_data/stitch/output/115.tiff --max_dim 3000
python stitch_images.py $folder_data/stitch/197 $folder_data/stitch/output/197.tiff --max_dim 3000
