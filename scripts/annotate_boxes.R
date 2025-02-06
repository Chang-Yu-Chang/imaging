#' This script annotates the raw image with sampling boxes

library(tidyverse)
library(imager)
library(magick) # for loading tiff
library(Cairo) # for computing and annotating the longest axis

file_pattern <- "Tile_\\d"

folder_raw <- "~/Dropbox/lab/imaging/data/raw/" # input
folder_temp <- here::here("data/trichome/temp/") #
folder_annotated <- here::here("data/trichome/annotated/") # output

# Extract file names
file_names <- list.files(path = folder_raw, pattern = file_pattern)
file_names_extracted <- str_extract(file_names, "Tile_\\d+")
stopifnot(length(file_names) == length(file_names_extracted))

find_farthest_points <- function (img) {
    #' Input is a binary img with only one pixel set
    img_labeled <- label(img)
    # Boundary pixles
    coords <- boundary(img_labeled) %>% as_tibble() %>% select(x, y)
    centroid <- round(colMeans(coords))

    coords_r <- coords %>%
        mutate(radius = sqrt((x - centroid[1])^2 + (y - centroid[2])^2)) %>%
        arrange(desc(radius))
    lrp1 <- coords_r[1, ]

    # Calculate direction vector from centroid to the longest radius pixel
    direction_vector <- c(lrp1$x - centroid[1], lrp1$y - centroid[2])
    length_direction <- sqrt(sum(direction_vector^2))
    normalized_vector <- direction_vector / length_direction
    distance_multiplier <- 2 # Change as needed for longer extrapolation

    line_start <- centroid
    line_end <- round(centroid + normalized_vector * (lrp1$radius * distance_multiplier))

    lrp2 <- coords %>%
        filter(
            abs((line_end[2] - line_start[2]) * (x - line_start[1]) - (line_end[1] - line_start[1]) * (y - line_start[2])) /
                sqrt((line_end[2] - line_start[2])^2 + (line_end[1] - line_start[1])^2) < 1 # Use a small threshold
        ) %>%
        arrange(desc(x)) %>%
        slice(1)

    # Longest radius pixels
    return(tibble(
        pixeltype = c("centroid", "lrp1", "lrp2"),
        x = c(centroid[1], lrp1$x[1], lrp2$x[1]),
        y = c(centroid[2], lrp1$y[1], lrp2$y[1])
    ))
}
draw_sq <- function (img, xc, yc, sl, color = "blue", lwd = 2) {
    # Coordinate of square center
    x0 = xc - sl/2
    x1 = xc + sl/2
    y0 = yc - sl/2
    y1 = yc + sl/2

    img_out <- img %>%
        implot(lines(c(x0, x1), c(y0, y0), col = color, lwd = lwd)) %>%
        implot(lines(c(x0, x1), c(y1, y1), col = color, lwd = lwd)) %>%
        implot(lines(c(x0, x0), c(y0, y1), col = color, lwd = lwd)) %>%
        implot(lines(c(x1, x1), c(y0, y1), col = color, lwd = lwd))

    return(img_out)
}
annotate_boxes <- function (file_name, file_name_extracted) {
    #'
    cat("\n\nProcessing ", file_name_extracted)

    # Load raw images
    img_raw <- image_read(paste0(folder_raw, file_name))
    img_raw <- magick2cimg(img_raw)
    img_raw_cropped <- as.cimg(img_raw[400:2900, 600:3400,1:3])
    #plot(img_raw_cropped)

    # Load mask
    img_mask <- load.image(paste0(folder_temp, file_name_extracted, "/08-mask.png"))
    img_mask3 <- array(rep(img_mask, 3), dim = c(dim(img_mask)[1:2], 1, 3))
    # Mask
    img_masked <- img_raw_cropped * img_mask3
    # Load segmented image
    img_filled <- load.image(paste0(folder_temp, file_name_extracted, "/07-filled.png"))
    cat("\nLoaded raw and mask")

    # Annotate the longest axis
    lrps <- find_farthest_points(img_filled)
    img_axis <- implot(img_filled, lines(c(lrps$x[2], lrps$x[3]), c(lrps$y[2], lrps$y[3]), col = "red", lwd = 4)) # draw line
    img_axis <- implot(img_axis, points(lrps$x[1], lrps$y[1], col = "red", cex = 5)) # draw point
    save.image(img_axis, paste0(folder_temp, file_name_extracted, "/11-axis.png"))
    cat("\nAnnotated the longest axis")

    # Rotate a image
    ## Calculate the angle to rotate (in degrees)
    delta_x <- lrps$x[2] - lrps$x[3]
    delta_y <- lrps$y[2] - lrps$y[3]
    angle <- 90-atan2(delta_y, delta_x) * (180 / pi)  # Convert radians to degrees
    img_rotated <- imrotate(img_filled, angle)
    lrps <- find_farthest_points(img_rotated)
    img_rotated_axis <- implot(img_rotated, lines(c(lrps$x[2], lrps$x[3]), c(lrps$y[2], lrps$y[3]), col = "red", lwd = 4))
    img_rotated_axis <- implot(img_rotated_axis, points(lrps$x[1], lrps$y[1], col = "red", cex = 5)) # draw point
    save.image(img_rotated_axis, paste0(folder_temp, file_name_extracted, "/12-rotated.png"))
    cat("\nRotated")

    # Annotate boxes
    sl <- 50 # side length
    img_box <- draw_sq(img_rotated_axis, lrps$x[1]+sl/2, lrps$y[1]+sl/2, sl) # box 1
    img_box <- draw_sq(img_box, lrps$x[1]+sl/2, lrps$y[1] - abs(lrps$y[1]-lrps$y[2])/2, sl) # box 2
    save.image(img_box, paste0(folder_temp, file_name_extracted, "/13-box.png"))
    cat("\nAnnotated boxes on the mask")

    # Annotate boxes on raw images
    lrps <- find_farthest_points(img_rotated)
    img_raw2 <- img_rotated <- imrotate(img_masked, angle) # rotate
    img_raw3 <- implot(img_raw2, lines(c(lrps$x[2], lrps$x[3]), c(lrps$y[2], lrps$y[3]), col = "red", lwd = 4)) # add axis
    img_raw4 <- implot(img_raw3, lines(c(lrps$x[2], lrps$x[3]), c(lrps$y[2], lrps$y[3]), col = "red", lwd = 4)) # add centroid
    img_raw5 <- draw_sq(img_raw4, lrps$x[1]+sl/2, lrps$y[1]+sl/2, sl) # box 1
    img_raw6 <- draw_sq(img_raw5, lrps$x[1]+sl/2, lrps$y[1] - abs(lrps$y[1]-lrps$y[2])/2, sl) # box 2
    cat("\nAnnotated boxes on the raw")

    save.image(img_raw6, paste0(folder_temp, file_name_extracted, "/14-box_raw.png"))
}


# i=2
# file_name <- file_names[i]
# file_name_extracted <- file_names_extracted[i]
# annotate_boxes(file_names[i], file_names_extracted[i])

for (i in 2:length(file_names)) annotate_boxes(file_names[i], file_names_extracted[i])
#for (i in 1:length(file_names)) annotate_boxes(file_names[i], file_names_extracted[i])

