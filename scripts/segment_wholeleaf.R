#' This script segments the whole leaf

library(tidyverse)
library(imager)
library(magick)
library(EBImage)

folder_data <- "~/Dropbox/lab/imaging/data/" # Enter the directory of data
folder_raw <- paste0(folder_data, "trichome/raw2/") # input
folder_temp <- paste0(folder_data, "trichome/temp/") # output

# Extract file names
file_pattern <- ""
file_names <- list.files(path = folder_raw, pattern = file_pattern)
file_names_extracted <- str_remove(file_names, ".jpg")

i = 2
file_name <- file_names[i]
file_name_extracted <- file_names_extracted[i]


seg_leaf <- function (file_name, file_name_extracted) {
    cat("\nProcessing ", file_name_extracted)
    if (!dir.exists(paste0(folder_temp, file_name_extracted))) dir.create(paste0(folder_temp, file_name_extracted))

    # Read raw
    img_raw <- image_read(paste0(folder_raw, file_name)) %>% magick2cimg
    save.image(img_raw, paste0(folder_temp, file_name_extracted, "/01-raw.jpg"))
    cat("\tSaved raw")

    # Manual crop
    #img_cropped <- imsub(img_raw, x>200 & x<=4000, y>600 & y<=4400)
    #save.image(img_cropped, paste0(folder_temp, file_name_extracted, "/02-cropped.jpg"))

    # Single channel
    img_gray <- channels(img_raw)[[2]]
    save.image(img_gray, paste0(folder_temp, file_name_extracted, "/03-gray.jpg"))
    cat("\tSingle channeled")

    # Threshold
    ps_bin <- img_gray < threshold(img_gray)
    img_bin <- as.cimg(ps_bin)
    save.image(img_bin, paste0(folder_temp, file_name_extracted, "/04-threshold.jpg"))
    cat("\tThresholded")

    # Remove small objects
    img_labeled <- bwlabel(img_bin)
    m <- as.cimg(img_labeled) %>% as.Image()
    tab <- table(m)
    m2 <- rmObjects(m, names(which(tab < 2000)), reenumerate = F)
    writeImage(m2, paste0(folder_temp, file_name_extracted, "/05-large.jpg"))
    #save.image(img_large, paste0(folder_temp, file_name_extracted, "/05-large.jpg"))
    cat("\tRemoved small pixsets")

    # Remove non leaf object
    tb_shape <- computeFeatures.shape(m2[,,1,1]) # check the object shape
    tb_shape2 <- as_tibble(tb_shape) %>%
        mutate(id = rownames(tb_shape)) %>%
        #
        filter(!(s.radius.max/s.radius.min < 10))
    img_leaf <- rmObjects(m2, tb_shape2$id) %>% as.cimg()
    save.image(img_leaf, paste0(folder_temp, file_name_extracted, "/06-leaf.jpg"))
    cat("\tRemoved pixsets with weird shapes")


    # Fill the pixsets
    img_eroded <- erode_square(img_leaf, 2)
    save.image(img_eroded, paste0(folder_temp, file_name_extracted, "/07-eroded.jpg"))
    cat("\t Eroded")
    img_dilated <- dilate_square(img_eroded, 6)
    save.image(img_dilated, paste0(folder_temp, file_name_extracted, "/08-dilated.jpg"))
    cat("\tDilated")
    img_filled <- bucketfill(img_dilated, 1, 1, color = 2) %>% {!( . == 2) } %>% as.cimg()
    save.image(img_filled, paste0(folder_temp, file_name_extracted, "/09-filled.jpg"))
    cat("\tFilled")
}


for (i in 1:length(file_names)) seg_leaf(file_names[i], file_names_extracted[i])
