#' This script extract the leaf area from filled images

library(tidyverse)
library(imager)

folder_data <- "~/Dropbox/lab/imaging/data/"
folder_temp <- paste0(folder_data, "trichome/temp/") # output

file_names <- list.files(path = paste0(folder_data, "trichome/temp"), full.names = T)
file_names_extracted <- str_extract(file_names, "\\d_\\d+")

count_fore_pixels <- function(file_name) {
    #' The input must be a binary image
    img_leaf <- load.image(paste0(file_name, "/07-filled.png"))
    img_trichome <- load.image(paste0(file_name, "/10-trichome_cleaned.png"))

    return(tibble(leaf = sum(img_leaf), trichome = sum(img_trichome)))
}

counts <- list()
for (i in 1:length(file_names)) {
    file_name <- file_names[i]
    counts[[file_names_extracted[i]]] <- count_fore_pixels(file_name)
    cat("\t", i)
}

areas <- bind_rows(counts, .id = "sample") %>%
    mutate(trichome_to_leaf = trichome / leaf)

write_csv(areas, paste0(folder_data, "trichome/areas.csv"))
