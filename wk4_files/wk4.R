library(tidyverse)
library(spData)
library(spDataLarge)
library(sf)
library(stars)
library(terra)

dem <- terra::rast(system.file("raster/dem.tif", package = "spDataLarge"))
landsat <- terra::rast(system.file("raster/landsat.tif", package = "spDataLarge"))
srtm <- terra::rast(system.file("raster/srtm.tif", package = "spDataLarge"))

hist(dem,
     main = "Digital Elevation Model Distribution",
     xlab = "Value")

boxplot(dem,
        main = "Digital Elevation Model Distribution",
        ylab = "Value")

rcl <- matrix(c(-Inf, 300, 0, # values -Inf to 300 = 0
                300, 500, 1,  # values 300 to 500 = 1
                500, Inf, 2), # values 500 to Inf = 2
              ncol = 3, byrow = TRUE)

dem_rcl <- classify(dem, rcl = rcl)

levels(dem_rcl) <- tibble::tibble(id = 1:3, 
                                  cats = c("low", "medium", "high"))

elevation_mean <- zonal(dem, dem_rcl, fun = "mean")
elevation_mean

ndwi_fun <- function(green, nir) {
  
  (green - nir)/(green + nir)
  
}

ndvi_fun <- function(nir, red) {
  
  (nir - red)/(nir + red)
  
}

# Applying the nir layer (4) and the red layer (3) to our ndvi_fun function
ndvi_zion <- lapp(landsat[[c(4, 3)]], fun = 'ndvi_fun')

# Applying the nir layer (4) and the green layer (2) to our ndwi_function
ndwi_zion <- lapp(landsat[[c(4, 2)]], fun = 'ndwi_fun')

scale_factor <- 0.0001
offset <- 0

scale_function <- function(x) {
  x * scale_factor + offset
}

landsat_scaled <- terra::app(landsat, fun = scale_function)

terra::plot(ndwi_zion,
     main = "Zion National Park NDWI")

terra::plot(ndvi_zion,
     main = "Zion National Park NDVI")

combine <- c(ndvi_zion, ndwi_zion)
terra::plot(combine, main = c("NDVI", "NDWI"))

