#' This script cleans the pixel sets of segmented leaf scan images

library(tidyverse)
library(imager)
library(magick)

folder_data <- "~/Dropbox/lab/imaging/data/"
folder_seg <- paste0(folder_data, "wild_samples/seg/") # input
folder_postseg <- paste0(folder_data, "wild_samples/postseg/") # output

# Extract file names
file_pattern <- "Sheet\\d_\\d+"
filenames <- list.files(path = folder_seg, pattern = file_pattern)
filenames_extracted <- str_extract(filenames, file_pattern)
stopifnot(length(filenames) == length(filenames_extracted))

# i = 4
i=3
filename <- filenames[i]
filename_extracted <- filenames_extracted[i]

remove_small <- function (img, object_size=1000) {
    img_labeled <- label(img)
    tab <- table(img_labeled)
    to_keep <- names(which(tab >= object_size))
    img_large1 <- is.element(img_labeled, to_keep) %>% array(dim = dim(img)) %>% as.cimg()
    img_large2 <- img_large1 * img # remove the background
    return(img_large2)
}

clean_pixsets <- function (filename, filename_extracted, save_imt = T) {
    #' A wrapper function for processing:
    #' 2. Thresholding the segmented image into a binary image
    #' 3. Remove small pixsets
    #' 4. Fill the pixsets
    #' Arguments:
    #' - filename: the input image file name
    #' - file_extracted: cleaned file name
    #' - save_imt: whether to save the intermediate images, including binary, large pixsets, eroded, and dilated

    cat("\nProcessing ", filename_extracted)
    # Create a folder per raw image
    if (!dir.exists(paste0(folder_postseg, filename_extracted))) dir.create(paste0(folder_postseg, filename_extracted))

    # Move the segmented image to the temp folder
    img_seg <- image_read(paste0(folder_seg, filename)) %>% magick2cimg()
    save.image(img_seg, paste0(folder_postseg, filename_extracted, "/01-seg.png"))
    cat("\tSegmentation")

    # 2. Threshold
    img_bin <- threshold(img_seg, .5) %>% as.cimg()
    save.image(img_bin, paste0(folder_postseg, filename_extracted, "/02-threshold.png"))
    cat("\tBinary")

    # 3. Create a new binary image containing only large objects
    img_large <- remove_small(img_bin, 1e4)
    save.image(img_large, paste0(folder_postseg, filename_extracted, "/03-large.png"))
    cat("\tKept large pixsets")

    # 4. Fill the pixsets
    img_filled <- clean(img_large, 5) %>% as.cimg()
    save.image(img_filled, paste0(folder_postseg, filename_extracted, "/04-filled.png"))
    cat("\tFilled")

    # 5. Extract the trichome pixel sets
    ## Prepare a mask that only cover the leaf
    img_mask <- dilate_square(img_filled, 50)
    save.image(img_mask, paste0(folder_postseg, filename_extracted, "/05-mask.png"))
    ## Subset the pixels
    ps_tri <- (img_seg * img_mask) > 0.8
    img_tri <- as.cimg(ps_tri)
    save.image(img_tri, paste0(folder_postseg, filename_extracted, "/06-trichome.png"))
    ## Remove small pixsets
    img_tricle <- clean(img_tri, 2) %>% as.cimg()
    save.image(img_tricle, paste0(folder_postseg, filename_extracted, "/07-trichome_cleaned.png"))
    cat("\tTrichome pixel sets")
}

for (i in 1:length(filenames))  {
    if (dir.exists(paste0(folder_postseg, filenames_extracted[i]))) {
        cat("\n", paste0(folder_postseg, filenames_extracted[i]), "exists")
        next
    } else {
        clean_pixsets(filenames[i], filenames_extracted[i], save_imt = T)

    }
}

