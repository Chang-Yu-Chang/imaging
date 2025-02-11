#' This script annotates the raw image with sampling boxes

library(tidyverse)
library(imager)
library(magick) # for loading tiff
library(Cairo) # for computing and annotating the longest axis

file_pattern <- "Tile_\\d"

folder_raw <- "~/Dropbox/lab/imaging/data/trichome/raw/" # input
folder_temp <- here::here("data/trichome/temp/") #
folder_annotated <- here::here("data/trichome/annotated/") # output

# Extract file names
file_names <- list.files(path = folder_raw, pattern = file_pattern)
file_names_extracted <- str_extract(file_names, "Tile_\\d+")
stopifnot(length(file_names) == length(file_names_extracted))

# Functions
mask_img <- function (img, mask) {
    if (dim(mask)[4] == 1) mask <- array(rep(mask, 3), dim = c(dim(mask)[1:2], 1, 3))
    img_masked <- img * mask
    return(img_masked)
}
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
draw_axis <- function (img, lrps) {
    img_axis <- implot(img, lines(c(lrps$x[2], lrps$x[3]), c(lrps$y[2], lrps$y[3]), col = "red", lwd = 4)) # draw line
    img_axis <- implot(img_axis, points(lrps$x[1], lrps$y[1], col = "red", cex = 5)) # draw point
    return(img_axis)
}
rotate_by_axis <- function (img, lrps) {
    ## Calculate the angle to rotate (in degrees)
    delta_x <- lrps$x[2] - lrps$x[3]
    delta_y <- lrps$y[2] - lrps$y[3]
    angle <- 90-atan2(delta_y, delta_x) * (180 / pi)  # Convert radians to degrees
    img_rotated <- imrotate(img, angle)
    return(img_rotated)
}
draw_sqr <- function (img, xc, yc, sl, color = "blue", lwd = 2) {
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
draw_sqrs <- function (img, lrps, sl = 50) {
    #sl <- 50 # side length
    img_box <- draw_sqr(img, lrps$x[1]+sl/2, lrps$y[1]+sl/2, sl) # box 1
    img_box <- draw_sqr(img_box, lrps$x[1]+sl/2, lrps$y[1] - abs(lrps$y[1]-lrps$y[2])/2, sl) # box 2
    return(img_box)
}
crop_sqr <- function (img, xc, yc, sl = 50) {
    if ("cimg" %in% class(img)) {
        img <- cimg2magick(img)
    } else if ("magick-image" %in% class(img)) {
        NULL
    }
    x0 = xc - sl/2
    y0 = yc - sl/2

    img_box1 <- image_crop(img, paste0(sl, "x", sl, "+", x0, "+", y0)) %>%
        magick2cimg()

    return(img_box1)
}

i=1
file_name <- file_names[i]
file_name_extracted <- file_names_extracted[i]
#annotate_boxes(file_names[i], file_names_extracted[i])


annotate_boxes <- function (file_name, file_name_extracted) {
    #'
    cat("\n\nProcessing ", file_name_extracted)

    # Load raw images
    img_raw <- image_read(paste0(folder_raw, file_name)) %>% magick2cimg()
    img_raw_cropped <- imsub(img_raw, x>400 & x<=3000, y>400 & y<=3400)

    # Mask
    img_mask <- load.image(paste0(folder_temp, file_name_extracted, "/08-mask.png"))
    img_masked <- mask_img(img_raw_cropped, img_mask)
    cat("\nLoaded raw and mask")

    # Annotate the longest diameter
    img_filled <- load.image(paste0(folder_temp, file_name_extracted, "/07-filled.png")) # Load segmented image
    lrps <- find_farthest_points(img_filled)
    img_axis <- draw_axis(img_filled, lrps)
    save.image(img_axis, paste0(folder_temp, file_name_extracted, "/11-axis.png"))
    cat("\nDrew the longest axis")

    # Rotate a image
    img_rotated <- rotate_by_axis(img_axis, lrps)
    lrps2 <- find_farthest_points(img_rotated)
    img_rotated_axis <- draw_axis(img_rotated, lrps2)
    save.image(img_rotated_axis, paste0(folder_temp, file_name_extracted, "/12-rotated.png"))
    cat("\nRotated")

    # Annotate boxes
    img_box <- draw_sqrs(img_rotated_axis, lrps2, sl = 50)
    save.image(img_box, paste0(folder_temp, file_name_extracted, "/13-box.png"))
    cat("\nDrew boxes on the mask")

    # Annotate boxes on raw images
    #lrps2 <- find_farthest_points(img_masked)
    img_box_raw <- img_masked %>%
        rotate_by_axis(lrps) %>%
        draw_axis(lrps2) %>%
        draw_sqrs(lrps2)
    save.image(img_box_raw, paste0(folder_temp, file_name_extracted, "/14-box_raw.png"))
    cat("\nDrew boxes on the raw")

    # Output two boxes only

    # img <- img_masked %>%
    #     rotate_by_axis(lrps)
    # plot(img)
    # sl = 2000
    # img_raw %>%
    #     imsub(x>=400, x<2900) %>%
    #     plot
    #
    # img %>%
    #     cimg2magick() %>%
    #
    #     #image_crop(paste0(sl, "x", sl, "+", 2000-sl/2, "+", 800-sl/2)) %>%
    #     magick2cimg() %>%
    #     plot
    #
    # img %>%
    #     crop_sqr(2000, 800, sl = 2000) %>% plot
    #     #crop_sqr(lrps2$x[1]+sl/2, lrps2$y[1]+sl/2, sl) %>% plot
#
#
#     boats %>%
#         pad(20, pos = -1, "xy") %>%
#         pad(20, pos = 1, "xy") %>%
#         plot
}

for (i in 1:length(file_names)) annotate_boxes(file_names[i], file_names_extracted[i])
