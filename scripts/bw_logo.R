#'

library(tidyverse)
library(imager)
source(here::here("metadata.R"))
set.seed(42)

img <- load.image(here::here("data/logo/penn.png")) %>% 
    pad(nPix = 20, axes = "xy", pos = 1) %>% 
    pad(nPix = 20, axes = "xy", pos = -1) 
plot(img)
img[img==0] <- 1
img_bw <- as.cimg(img[,,,1] + img[,,,2] + img[,,,3]) / max(img[,,,1] + img[,,,2] + img[,,,3])

img_bw <- img_bw > .9
img_bw %>% 
    as.cimg(img_bw) %>% 
    save.image(here::here("data/logo/penn_white.png"))