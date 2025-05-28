#!/usr/bin/env zsh

folder_data=/Users/cychang/Dropbox/lab/imaging/data
folder_full=$folder_data/wild_samples/full
folder_cropped=$folder_data/wild_samples/cropped

mkdir -p $folder_cropped

# For sheet 1-5, crop each into 6x7 tiles
for file in $folder_full/Sheet1.tiff $folder_full/Sheet2.tiff $folder_full/Sheet3.tiff $folder_full/Sheet4.tiff $folder_full/Sheet5.tiff
do
    filename=$(basename "$file" .tiff)
    echo $filename
    # Divide it into roughly equally sized divisions width by 6 and height by 7
    magick $folder_full/$filename.tiff -crop 6x7@ +repage "$folder_cropped/$filename"_%d.jpg
done


# For sheet 6-8, crop each into 6x3 tiles
for file in $folder_full/Sheet6.tiff $folder_full/Sheet7.tiff $folder_full/Sheet8.tiff
do
    filename=$(basename "$file" .tiff)
    echo $filename
    # Divide it into roughly equally sized divisions width by 6 and height by 3
    magick $folder_full/$filename.tiff -crop 6x3@ +repage "$folder_cropped/$filename"_%d.jpg
done
