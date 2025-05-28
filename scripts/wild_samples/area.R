#' This script extract the leaf area from filled images

library(tidyverse)
library(imager)

folder_data <- "~/Dropbox/lab/imaging/data/"
folder_postseg <- paste0(folder_data, "wild_samples/postseg/") # output

filenames <- list.files(path = paste0(folder_data, "/wild_samples/postseg/"), full.names = T)
filenames_extracted <- str_extract(filenames, "Sheet\\d_\\d+")

count_fore_pixels <- function(filename) {
    #' The input must be a binary image
    img_leaf <- load.image(paste0(filename, "/04-filled.png"))
    img_trichome <- load.image(paste0(filename, "/07-trichome_cleaned.png"))

    return(tibble(leaf = sum(img_leaf), trichome = sum(img_trichome)))
}

#counts <- list()
for (i in 1:length(filenames)) {
    if (filenames_extracted[i] %in% names(counts)) {
        next
    } else {
        filename <- filenames[i]
        counts[[filenames_extracted[i]]] <- count_fore_pixels(filename)
        cat("\t", i)
    }
}

areas <- bind_rows(counts, .id = "sample") %>%
    mutate(leaf_area_mm2 = leaf / 137.79^2, trichome_area_mm2 = trichome / 137.79^2) %>%
    mutate(trichome_to_leaf = trichome / leaf) %>%
    arrange(sample)


write_csv(areas, paste0(folder_data, "wild_samples/areas.csv"))

# 137.79^2 pixels = 1mm^2

areas %>%
    ggplot() +
    geom_point(aes(x = leaf_area_mm2, y = trichome_to_leaf)) +
    theme_bw() +
    theme() +
    guides() +
    labs()
