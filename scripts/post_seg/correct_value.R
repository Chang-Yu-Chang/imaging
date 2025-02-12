#' This script corrects the ilastic value

library(tidyverse)
library(imager)
library(magick)

folder_data <- "~/Dropbox/lab/imaging/data/"
folder_seg <- paste0(folder_data, "trichome/segmented/run2/") # input
folder_seg2 <- paste0(folder_data, "trichome/segmented/run2_corrected/") # output

if (!dir.exists(folder_seg2)) dir.create(folder_seg2)

# Extract file names
file_pattern <- "\\d+_\\d+_"
file_names <- list.files(path = folder_seg, pattern = file_pattern)
file_names_extracted <- str_extract(file_names, "\\d+_\\d+")
stopifnot(length(file_names) == length(file_names_extracted))

# i = 1
# file_name <- file_names[i]
# file_name_extracted <- file_names_extracted[i]

correct_value <- function (file_name, file_name_extracted) {
    cat("\nProcessing ", file_name_extracted)

    img_seg <- load.image(paste0(folder_seg, file_name))

    # Check if the image is reverted
    tab <- table(img_seg)
    xx <- names(tab)[which.max(tab)]
    img_seg[img_seg == xx] <- 0

    save.image(img_seg, paste0(folder_seg2, file_name_extracted, ".png"))
}

for (i in 1:length(file_names)) correct_value(file_names[i], file_names_extracted[i])
