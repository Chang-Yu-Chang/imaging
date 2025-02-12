#' This script annotates the raw image with sampling boxes

library(tidyverse)
library(imager)
library(magick) # for loading tiff
library(Cairo) # for computing and annotating the longest axis


folder_data <- "~/Dropbox/lab/imaging/data/"
folder_raw <- "~/Dropbox/lab/imaging/data/trichome/raw/" # input
folder_seg <- paste0(folder_data, "trichome/segmented/run2_corrected/") # input
folder_temp <- paste0(folder_data, "trichome/temp/") # output

# Extract file names
file_pattern <- "\\d_\\d+"
file_names <- list.files(path = folder_raw, pattern = file_pattern)
file_names_extracted <- str_extract(file_names, "\\d_\\d+")

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
    coords <- slice_sample(coords, prop = .05) %>% arrange(x, y)
    centroid <- round(colMeans(coords))
    # Initialize maximum distance and points
    max_distance <- 0
    #farthest_points <- tibble(pixeltype = NA, x = NA, y = NA)

    # Loop over all pairs of boundary pixels to find the farthest apart
    for (i in 1:(nrow(coords)-1)) {
        for (j in (i + 1):nrow(coords)) {
            distance <- sqrt((coords$x[i] - coords$x[j])^2 + (coords$y[i] - coords$y[j])^2)
            if (distance > max_distance) {
                max_distance <- distance
                farthest_points <- tibble(
                    x = c(coords$x[i], coords$x[j]),
                    y = c(coords$y[i], coords$y[j])
                )
            }
        }
    }

    return(tibble(
        pixeltype = c("centroid", "lrp1", "lrp2"),
        x = c(centroid[1], farthest_points$x[1], farthest_points$x[2]),
        y = c(centroid[2], farthest_points$y[1], farthest_points$y[2])
    ))
}
draw_axis <- function (img, lrps) {
    img %>%
        implot(lines(c(lrps$x[2], lrps$x[3]), c(lrps$y[2], lrps$y[3]), col = "red", lwd = 4)) %>%  # draw line
        implot(points(lrps$x[1], lrps$y[1], col = "red", cex = 5)) # draw point
}
rotate_by_axis <- function (img, lrps) {
    ## Calculate the angle to rotate (in degrees)
    delta_x <- lrps$x[2] - lrps$x[3]
    delta_y <- lrps$y[2] - lrps$y[3]
    angle <- 90-atan2(delta_y, delta_x) * (180 / pi)  # Convert radians to degrees
    img_rotated <- imrotate(img, angle)
    return(img_rotated)
}
draw_sqr <- function (img, xc, yc, sl, color = "blue", lwd = 1) {
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
    x0 = xc - sl/2
    x1 = xc + sl/2
    y0 = yc - sl/2
    y1 = yc + sl/2

    img_box1 <- imsub(img, x>x0 & x<=x1, y>y0 & y<=y1)
    #img_box1 <- image_crop(img, paste0(sl, "x", sl, "+", x0, "+", y0)) %>%

    return(img_box1)
}
make_sqr_pixset <- function (img, xc, yc, sl = 50) {
    x0 = xc - sl/2
    x1 = xc + sl/2
    y0 = yc - sl/2
    y1 = yc + sl/2

    bps <- bind_rows(
        tibble(x = seq(x0, x1), y = y0),     # Bottom edge
        tibble(x = seq(x0, x1), y = y1),     # Top edge
        tibble(x = x0, y = seq(y0, y1)),     # Left edge
        tibble(x = x1, y = seq(y0, y1))      # Right edge
    )
    bps <- distinct(bps) %>%
        slice_sample(prop = 1/3)
    #print(bps)

    img_m <- cimg(array(1, c(nrow(img), ncol(img), 1, 1)))

    for (i in 1:nrow(bps)) {
        if (i %% 10 == 0) cat("\t", i)
        img_m[bps$x[i], bps$y[i], 1, 1] <- 0
    }

    return(img_m)
}

# i=1
# file_name <- file_names[i]
# file_name_extracted <- file_names_extracted[i]


annotate_boxes <- function (file_name, file_name_extracted) {
    #'
    cat("\n\nProcessing ", file_name_extracted)

    # Load raw images
    img_raw <- image_read(paste0(folder_raw, file_name)) %>% magick2cimg()
    img_raw_cropped <- img_raw %>% crop.borders(nx = 300, ny = 300)

    # Mask
    img_mask <- load.image(paste0(folder_temp, file_name_extracted, "/08-mask.png"))
    img_masked <- mask_img(img_raw_cropped, img_mask)
    cat("\nLoaded raw and mask")

    # Resize
    img_filled <- load.image(paste0(folder_temp, file_name_extracted, "/07-filled.png")) # Load segmented image
    # down_size = 1/4
    # img_filled_re <- imresize(img_filled, down_size)
    # img_masked_re <- imresize(img_masked, down_size)

    # Annotate the longest diameter
    lrps <- find_farthest_points(img_filled)
    #img_axis <- draw_axis(img_filled_re, lrps)
    img_axis <- img_filled
    #save.image(img_axis, paste0(folder_temp, file_name_extracted, "/11-axis.png"))
    #cat("\nDrew the longest axis")

    # Rotate a image
    img_rotated <- rotate_by_axis(img_axis, lrps)
    lrps2 <- find_farthest_points(img_rotated)
    #save.image(img_rotated, paste0(folder_temp, file_name_extracted, "/12-rotated.png"))
    cat("\nRotated")

    # Annotate boxes
    # img_box <- draw_sqrs(img_rotated, lrps2, sl = 50)
    # save.image(img_box, paste0(folder_temp, file_name_extracted, "/13-box.png"))
    # cat("\nDrew boxes on the mask")

    # Annotate boxes on raw images
    # img_box_raw <- img_masked %>%
    #     rotate_by_axis(lrps) %>%
    #     #draw_axis(lrps2) %>%
    #     draw_sqrs(lrps2, sl = 138)
    # save.image(img_box_raw, paste0(folder_temp, file_name_extracted, "/14-box_raw.png"))
    # cat("\nDrew boxes on the raw")

    # Crop the square boxes
    sl=138
    img_r2 <- img_masked %>% rotate_by_axis(lrps)
    img_box1 <- img_r2 %>% crop_sqr(lrps2$x[1]+sl/2, lrps2$y[1]+sl/2, sl)
    img_box2 <- img_r2 %>% crop_sqr(lrps2$x[1]+sl/2, lrps2$y[1] - abs(lrps2$y[1]-lrps2$y[2])/2, sl)
    img_sqr <- imappend(list(img_box1, cimg(array(0, c(10, sl, 1, 1))), img_box2), "x")
    save.image(img_sqr, paste0(folder_temp, file_name_extracted, "/15-squares.png"))
    cat("\nCropped out the squares")

    # Make pixel sets
    img_m <- make_sqr_pixset(img_r2, lrps2$x[1]+sl/2, lrps2$y[1]+sl/2, sl)
    cat("\nDrew square 1")
    img_m2 <- make_sqr_pixset(img_r2, lrps2$x[1]+sl/2, lrps2$y[1] - abs(lrps2$y[1]-lrps2$y[2])/2, sl)
    cat("\nDrew square 2")
    img_mt <- as.cimg(array(rep(img_m, 3), dim = c(dim(img_m)[1], dim(img_m)[2], 3)))
    img_mt2 <- as.cimg(array(rep(img_m2, 3), dim = c(dim(img_m2)[1], dim(img_m2)[2], 3)))
    img_masked2 <- img_r2 * img_mt * img_mt2
    save.image(img_masked2, paste0(folder_temp, file_name_extracted, "/16-masked_squares.png"))

}

# file_names <- file_names[!file_names %in% c("1_0", "1_1", "1_2")]
# file_names_extracted <- file_names_extracted[!file_names_extracted %in% paste0(c("1_0", "1_1", "1_2"), ".tiff")]
#annotate_boxes(file_names[i], file_names_extracted[i])
for (i in 1:length(file_names)) annotate_boxes(file_names[i], file_names_extracted[i])
