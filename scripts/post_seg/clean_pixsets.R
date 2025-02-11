#' This script cleans the pixel sets of segmented leaf scan images

library(tidyverse)
library(imager)
library(magick)

file_pattern <- "Tile_\\d"

folder_seg <- here::here("data/trichome/segmented/") # input
folder_temp <- here::here("data/trichome/temp/") # output

# Extract file names
file_names <- list.files(path = folder_seg, pattern = file_pattern)
file_names_extracted <- str_extract(file_names, "Tile_\\d+")
stopifnot(length(file_names) == length(file_names_extracted))

# i = 1
# file_name <- file_names[i]
# file_name_extracted <- file_names_extracted[i]


clean_pixsets <- function (file_name, file_name_extracted, save_imt = T) {
    #' A wrapper function for processing:
    #' 1. Manually crop the image
    #' 2. Thresholding the segmented image into a binary image
    #' 3. Remove small pixsets
    #' 4. Fill the pixsets
    #' Arguments:
    #' - file_name: the input image file name
    #' - file_extracted: cleaned file name
    #' - save_imt: whether to save the intermediate images, including binary, large pixsets, eroded, and dilated

    cat("\nProcessing ", file_name_extracted)
    # Create a folder per raw image
    if (!dir.exists(paste0(folder_temp, file_name_extracted))) dir.create(paste0(folder_temp, file_name_extracted))

    # Move the segmented image to the temp folder
    img_seg <- load.image(paste0(folder_seg, file_name)) # This is the default input
    save.image(img_seg, paste0(folder_temp, file_name_extracted, "/01-seg.png"))
    cat("\tSegmentation")

    # 1. Manual crop
    img_cropped <- imsub(img_seg, x>400 & x<=3000, y>400 & y<=3400)
    save.image(img_cropped, paste0(folder_temp, file_name_extracted, "/02-cropped.png"))

    # 2. Threshold
    ps_bin <- img_cropped < threshold(img_cropped)
    img_bin <- as.cimg(ps_bin)
    if (save_imt) save.image(img_bin, paste0(folder_temp, file_name_extracted, "/03-threshold.png"))
    cat("\tBinary")

    # 3. Create a new binary image containing only large objects
    img_labeled <- label(ps_bin)
    tab <- table(img_labeled)
    min_size <- 10000

    large_objects <- names(which(tab >= min_size))
    large_objects <- large_objects[-1] # remove the background
    img_large <- is.element(img_labeled, large_objects) %>% array(dim = dim(img_bin)) %>% as.cimg()
    if (save_imt) save.image(img_large, paste0(folder_temp, file_name_extracted, "/04-large.png"))
    cat("\tKept large pixsets")


    # 4. Fill the pixsets
    img_eroded <- erode_square(img_large, 2)
    if (save_imt) save.image(img_eroded, paste0(folder_temp, file_name_extracted, "/05-eroded.png"))
    cat("\t Eroded")
    img_dilated <- dilate_square(img_eroded, 6)
    if (save_imt) save.image(img_dilated, paste0(folder_temp, file_name_extracted, "/06-dilated.png"))
    cat("\tDilated")
    img_filled <- bucketfill(img_dilated, 1, 1, color = 2) %>% {!( . == 2) } %>% as.cimg()
    save.image(img_filled, paste0(folder_temp, file_name_extracted, "/07-filled.png"))
    cat("\tFilled")

    # 5. Extract the trichome pixel sets
    ## Prepare a mask that only cover the leaf
    img_mask <- dilate_square(img_filled, 50)
    save.image(img_mask, paste0(folder_temp, file_name_extracted, "/08-mask.png"))
    ## Subset the pixels
    ps_tri <- (img_cropped * img_mask) > 0.8
    img_tri <- as.cimg(ps_tri)
    save.image(img_tri, paste0(folder_temp, file_name_extracted, "/09-trichome.png"))
    ## Remove small pixsets
    img_tricle <- clean(img_tri, 2) %>% as.cimg()
    save.image(img_tricle, paste0(folder_temp, file_name_extracted, "/10-trichome_cleaned.png"))

}

for (i in 1:length(file_names)) clean_pixsets(file_names[i], file_names_extracted[i], save_imt = T)
