#' This script extract the leaf area from filled images

library(tidyverse)
library(imager)

folder_data <- "~/Dropbox/lab/imaging/data/"
folder_postseg <- paste0(folder_data, "wild_samples/postseg/") # output

filenames <- list.files(path = paste0(folder_data, "/wild_samples/postseg/"), full.names = T)
filenames_extracted <- str_extract(filenames, "\\d_\\d+")

count_fore_pixels <- function(filename) {
    #' The input must be a binary image
    img_leaf <- load.image(paste0(filename, "/07-filled.png"))
    img_trichome <- load.image(paste0(filename, "/10-trichome_cleaned.png"))

    return(tibble(leaf = sum(img_leaf), trichome = sum(img_trichome)))
}

counts <- list()
for (i in 1:length(filenames)) {
    filename <- filenames[i]
    counts[[filenames_extracted[i]]] <- count_fore_pixels(filename)
    cat("\t", i)
}

areas <- bind_rows(counts, .id = "sample") %>%
    mutate(trichome_to_leaf = trichome / leaf)

write_csv(areas, paste0(folder_data, "trichome/areas.csv"))
