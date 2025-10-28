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
library(dplyr)

## *Download example data from GitHub and store it in your working directory

# ## Load in movement data
# hornbills <- read.csv("~/Dropbox/UCLA/Research/Data/Movement Data 2022-23/All e-obs data/Hornbill e-obs Cameroon GPS_19Jun2025.csv")
# wth <- c("8970", "9894", "9919", "11850", "11852")
# 
# white.thighed <- hornbills[which(hornbills$tag.local.identifier %in% wth),]
# white.thighed.clean <- white.thighed %>% dplyr::select(tag.local.identifier, study.local.timestamp, utm.easting, utm.northing,
#                                                        utm.zone, study.timezone)
# 
# write.csv(white.thighed.clean, "~/Dropbox/Harvard/amt Workshop/Davies Lab Amt Workshop/white-thighed_hornbill_data.csv")
hornbills <- read.csv("~/Dropbox/Harvard/amt Workshop/Davies Lab Amt Workshop/white-thighed_hornbill_data.csv")
# hornbills$Sex <- ifelse(hornbills$tag.local.identifier %in% c("8970", "9919", "11850"), "Female", "Male")
# hornbills <- hornbills[!is.na(hornbills$utm.easting),] # get rid of NA coordinates

hornbills$tag.local.identifier <- as.character(hornbills$tag.local.identifier) # Make the tag identifier a character 
hornbills$timestamps <- as.POSIXct(strptime(hornbills$study.local.timestamp, "%Y-%m-%d %H:%M:%S", tz="WAT")) # Standardize timestamps

hornbills$id <- hornbills$tag.local.identifier

## Load in environmental data
canopyHeight <- rast("~/Dropbox/Harvard/amt Workshop/Davies Lab Amt Workshop/Environmental Layers/ch.tif")
dist2gap50 <- rast("~/Dropbox/Harvard/amt Workshop/Davies Lab Amt Workshop/Environmental Layers/d50.tif")
dist2gap500 <- rast("~/Dropbox/Harvard/amt Workshop/Davies Lab Amt Workshop/Environmental Layers/d500.tif")
VCI <- rast("~/Dropbox/Harvard/amt Workshop/Davies Lab Amt Workshop/Environmental Layers/vc.tif")
swamp <- rast("~/Dropbox/Harvard/amt Workshop/Davies Lab Amt Workshop/Environmental Layers/swamp.tif")

veg.stack <- c(canopyHeight, dist2gap50, dist2gap500, VCI, swamp)
## Any anomalies in the data (duplicates, etc.)?

# # Remove duplicate timestamps
# dupIt <- function(df) {
#   # Identify duplicated timestamps
#   dups <- which(duplicated(as.POSIXct(strptime(df$timestamp, "%Y-%m-%d %H:%M:%S", tz="GMT"))))
#   # Remove the duplicated timestamps
#   df <- df[-dups,]
# }
# 
# 
# df94 <- dupIt(hornbills[which(hornbills$tag.local.identifier=="9894"),])
# df19 <- dupIt(hornbills[which(hornbills$tag.local.identifier=="9919"),])
# df50 <- dupIt(hornbills[which(hornbills$tag.local.identifier=="11850"),])
# df52 <- dupIt(hornbills[which(hornbills$tag.local.identifier=="11852"),])
# df70 <- hornbills[which(hornbills$tag.local.identifier=="8970"),]
# 
# hornbills.clean <- rbind(df70, df94, df19, df50, df52)
# hornbills.clean <- hornbills.clean[, -c(1, 9)]

# Write the clean csv
# write.csv(hornbills.clean, "~/Dropbox/Harvard/amt Workshop/Davies Lab Amt Workshop/white-thighed_hornbill_data.csv")

## Make a "track" object
# ## Create tracks
# trackIt <- function(bird) {
#   # Make the track
#   bird.trk <- make_track(bird, .x=utm.easting, .y=utm.northing, .t=timestamps, id= individual.local.identifier,
#                          crs="+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs",
#                          all_cols = TRUE)
#   # Return the track
#   return(bird.trk)
# }

# Hornbill tracks
hornbill.trk <- make_track(hornbills, .x=utm.easting, .y=utm.northing, .t=timestamps, id= id,
                           crs="+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs",
                           all_cols = TRUE)

# # WTH
# hb70.trk <- trackIt(df70)
# hb94.trk <- trackIt(df94)
# hb19.trk <- trackIt(df19)
# hb50.trk <- trackIt(df50)
# hb52.trk <- trackIt(df52)
library(tidyverse)
# Nest by individual
trk1 <- hornbill.trk |> nest(data = -"id")

trk2 <- trk1 |> 
  mutate(steps = map(data, function(x) 
    x |> track_resample(rate = minutes(30), tolerance = minutes(10)) |> 
      filter_min_n_burst(min_n = 5) |>
      steps_by_burst() |>
      random_steps(n_control = 10) |>
      extract_covariates(veg.stack)
    ))

hornbill.extracted <- trk2

save(hornbill.extracted, file = "~/Dropbox/Harvard/amt Workshop/Davies Lab Amt Workshop/hornbill.extracted.RData")
# ## Sample covariates
# ## Function for 30 min resampling
# extractIt30 <- function(trk, covar1, covar2, covar3, covar4, covar5) {
#   m1 <- trk %>%
#     amt::track_resample(rate = minutes(30), tolerance = minutes(10)) %>%
#     amt::filter_min_n_burst(min_n = 5) %>%
#     amt::steps_by_burst() %>% amt::random_steps() %>% 
#     amt::extract_covariates(covar1) %>% # canopyHeight
#     amt::extract_covariates(covar2) %>% # dist2gap50
#     amt::extract_covariates(covar3) %>% # dist2gap500
#     amt::extract_covariates(covar4) %>% # VCI
#     amt::extract_covariates(covar5) %>% # Swamp
#     
#   # Add time of day
#   m1$hour <- hour(m1$t1_)
#   # Month
#   m1$month <- month(m1$t1_)
#   # Day
#   m1$day <- as.POSIXlt(m1$t1_)$yday
#   # Return the data frame
#   return(m1)
# }
