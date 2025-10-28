#################################################
##' Upload, visualize, and prepare movement data
##' for integrated Step Selection Analysis (iSSA)
##' Davies lab `amt` workshop
##' 31 Oct. 2025
#################################################

# Clear the R environment
rm(list = ls())

## Load required packages
library(amt) # For processing movement data and preparing it for iSSA
library(lubridate) # For working with date and time data
library(terra) # For working with rasters
library(tidyverse) # For reshaping, and generally "wrangling" datasets

## *Download example data from GitHub and store it in your working directory

# Load hornbill GPS data
hornbills <- read.csv("~/white-thighed_hornbill_data.csv")

# Make the tag identifier a character vector
hornbills$tag.local.identifier <- as.character(hornbills$tag.local.identifier)

# Format the timestamps
OlsonNames() # Get a comprehensive list of recognized timezones
hornbills$timestamps <- as.POSIXct(strptime(hornbills$study.local.timestamp, "%Y-%m-%d %H:%M:%S", tz="Africa/Douala")) # Standardize timestamps

# Create a new column for animal ID
hornbills$id <- hornbills$tag.local.identifier

# Load environmental data (predictors of hornbill movement)
canopyHeight <- rast("~/Environmental Layers/ch.tif") # Canopy height
dist2gap50 <- rast("~/d50.tif") # Distance to canopy gap >= 50 m2
dist2gap500 <- rast("~/Environmental Layers/d500.tif") # Distance to canopy gap >= 500 m2
VCI <- rast("~/Environmental Layers/vc.tif") # Vertical Complexity Index
swamp <- rast("~/Environmental Layers/swamp.tif")

# Create a raster stack of all our covariates
veg.stack <- c(canopyHeight, dist2gap50, dist2gap500, VCI, swamp)

## Any anomalies in the data (duplicates, etc.)?

# # Function to remove duplicate timestamps
# dupIt <- function(df) {
#   # Identify duplicated timestamps
#   dups <- which(duplicated(as.POSIXct(strptime(df$timestamp, "%Y-%m-%d %H:%M:%S", tz="GMT"))))
#   # Remove the duplicated timestamps
#   df <- df[-dups,]
# }

# df94 <- dupIt(hornbills[which(hornbills$tag.local.identifier=="9894"),])

# Make a "track" object with hornbill movement data, using the amt package
hornbill.trk <- make_track(hornbills, .x=utm.easting, .y=utm.northing, .t=timestamps, id= id,
                           crs="EPSG:32633",
                           all_cols = TRUE)

# Nest data by individual
trk1 <- hornbill.trk |> nest(data = -"id")

# Prepare the track object for iSSA
hornbill.extracted <- trk1 |> 
  mutate(steps = map(data, function(x) # Function to generate movement "steps"--straight lines between each consecutive GPS point
    x |> track_resample(rate = minutes(30), tolerance = minutes(10)) |> # Resample track to standardized fix rate (30 mins)
      filter_min_n_burst(min_n = 5) |> # Only return "bursts" of at least 5 consecutive locations
      steps_by_burst() |> # Generate movement steps for each burst
      random_steps(n_control = 10) |> # Generate 10 random steps per observed step
      extract_covariates(veg.stack) # Extract the value of each environmental covariate at the end of each movement step
  ))

# Have a look at the data
View(hornbill.extracted)

# Save the iSSA-ready data
save(hornbill.extracted, file = "~/hornbill.extracted.RData")


