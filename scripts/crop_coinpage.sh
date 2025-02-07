#!/usr/bin/env zsh

folder_data=/Users/cychang/Dropbox/lab/imaging/data
folder_full=$folder_data/trichome/coinpages/full
folder_cropped=$folder_data/trichome/coinpages/cropped

mkdir -p $folder_cropped

# Crop each image into 6x7 tiles
for file in $folder_full/*.png; do
    #file=$folder_full/test.png
    filename=$(basename "$file" .png)
    echo $filename
    mkdir -p $folder_cropped/$filename

    # Divide it into roughly equally sized divisions width by 6 and height by 7
    magick $folder_full/$filename.png -crop x7@ +repage $folder_cropped/$filename/%d.jpg
done
