#' This script cleans the pixel sets of segmented leaf scan images

library(tidyverse)
library(imager)
library(magick)

folder_data <- "~/Dropbox/lab/imaging/data/"
folder_seg <- paste0(folder_data, "trichome/segmented/run2_corrected/") # input
folder_temp <- paste0(folder_data, "trichome/temp/") # output

# Extract file names
file_pattern <- "\\d+_\\d+"
file_names <- list.files(path = folder_seg, pattern = file_pattern)
file_names_extracted <- str_extract(file_names, "\\d+_\\d+")
stopifnot(length(file_names) == length(file_names_extracted))

# i = 4
i=1
file_name <- file_names[i]
file_name_extracted <- file_names_extracted[i]

remove_small <- function (img, object_size=1000) {
    #img <- img_bin
    #img <- dilate_square(img, 3)
    img_labeled <- label(img)
    tab <- table(img_labeled)
    to_keep <- names(which(tab >= object_size))
    img_large1 <- is.element(img_labeled, to_keep) %>% array(dim = dim(img)) %>% as.cimg()
    img_large2 <- img_large1 * img # remove the background
    return(img_large2)
}

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
    img_seg <- load.image(paste0(folder_seg, file_name))
    save.image(img_seg, paste0(folder_temp, file_name_extracted, "/01-seg.png"))
    cat("\tSegmentation")

    # 1. Crop borders
    img_cropped <- img_seg %>% crop.borders(nx = 300, ny = 300)
    save.image(img_cropped, paste0(folder_temp, file_name_extracted, "/02-cropped.png"))

    # 2. Threshold
    img_bin <- threshold(img_cropped, .1) %>% as.cimg()
    save.image(img_bin, paste0(folder_temp, file_name_extracted, "/03-threshold.png"))
    cat("\tBinary")

    # 3. Create a new binary image containing only large objects
    img_large <- remove_small(img_bin, 1e3)
    save.image(img_large, paste0(folder_temp, file_name_extracted, "/04-large.png"))
    cat("\tKept large pixsets")

    # 4. Fill the pixsets
    img_eroded <- erode_square(img_large, 5)
    img_eroded2 <- remove_small(img_eroded, 1e4)
    save.image(img_eroded2, paste0(folder_temp, file_name_extracted, "/05-eroded.png"))
    cat("\t Eroded")
    img_dilated <- dilate_square(img_eroded2, 7)
    save.image(img_dilated, paste0(folder_temp, file_name_extracted, "/06-dilated.png"))
    cat("\tDilated")
    img_filled <- bucketfill(img_dilated, 2, 2, color = 2)
    tab <- table(img_filled)
    img_filled2 <- img_filled %>% {!(. == names(which.max(tab)))} %>% as.cimg()
    img_filled3 <- remove_small(img_filled2, 1e5)
    img_filled4 <- px.remove_outer(as.pixset(img_filled3)) %>% as.cimg()
    save.image(img_filled4, paste0(folder_temp, file_name_extracted, "/07-filled.png"))
    cat("\tFilled")

    # 5. Extract the trichome pixel sets
    ## Prepare a mask that only cover the leaf
    img_mask <- dilate_square(img_filled4, 50)
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
