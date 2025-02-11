#!/usr/bin/env zsh

# See this github repo for installing requirements for imager https://github.com/asgr/imager
# OS X users need XQuartz. On its own imager supports JPEG, PNG, TIFF and BMP formats. If you need support for other file types install ImageMagick. To load and save videos you'll need ffmpeg, no file formats are supported natively.

brew install --cask xquartz
brew install fftw
brew install imagemagick # for cropping
brew install ghostscript # for raster files
