#' This script extract the leaf area from filled images

library(tidyverse)
library(imager)

file_names <- list.files(path = here::here("data/trichome/temp"), full.names = T)
file_names_extracted <- str_extract(file_names, "Tile_\\d+")

count_fore_pixels <- function(img) {
    #' The input must be a binary image, ideally a filled image
    img_labeled <- label(img)
    tab <- table(img) %>% as_tibble() %>% rename(pixeltype = img) %>%
        mutate(pixeltype = case_when(
            pixeltype == 0 ~ "background",
            pixeltype == 1 ~ "foreground"
        ))
    return(tab)
}

counts <- list()
for (i in 1:length(file_names)) {
    file_name <- file_names[i]
    img <- load.image(paste0(file_name, "/07-filled.png"))
    counts[[file_names_extracted[i]]] <- count_fore_pixels(img)
    cat("\t", i)
}

leaf_area <- bind_rows(counts, .id = "sample") %>%
    pivot_wider(names_from = pixeltype, values_from = n)

write_csv(leaf_area, here::here("data/trichome/leaf_area.csv"))
