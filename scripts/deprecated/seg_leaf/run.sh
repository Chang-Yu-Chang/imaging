#!/bin/zsh

folder_data=/Users/cychang/Dropbox/lab/imaging/data
folder_raw=$folder_data/raw
folder_cropped=$folder_data/trichome/coinpages/cropped


train.py --train --images $folder_raw/*.tif --masks path/to/masks

train.py --predict --model "$MODEL_PATH" --input "$INPUT_PATH"
