# title: "1_Housekeeping.R"
# date started: "18/6/2019"
# last updated: "07/04/2022"


# This is Script 1 of 7
# The purpose of this script is to set up the working directory for this project,
# do general house keeping, insure all functions and packages are uploaded.

## Housekeeping ---------------

# set working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/PhD/Fishies/fishies/2022_moriarty_kelly_mckeon_updates")

# Remove files from R Global Environment 
rm(list = ls())

### library packages ---------------
list<-c("ggplot2", "data.table", "reshape2", "arm","car", "DMwR", "lme4", "plyr",
        "marmap", "plotrix", "colorspace", "plot3D", "plot3D", "rgl","MuMIn",
        "mapplots", "class", "gridExtra", "ggmap", "tidyverse", "beepr", "rgdal", 
        "marmap")

lapply(list, require, character.only=T)
lapply(list, citation)


### load functions ----------------

## create "not in" operator
'%nin%' = Negate('%in%')

# Calculate distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2) {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6371 # Mean radius (km) of the earth
  d <- R * c
  return(d)
  }

earth.dist(-15.5,56.25,-15.5,55.72)
# na.false function 
na.false <- function(x) {return(replace(x, which(is.na(x)), FALSE))}

# Function to compare if two dataset contain the same information
compare_function <- function(x.1,x.2,...){
  x.1p <- do.call("paste", x.1)
  x.2p <- do.call("paste", x.2)
  x.1[! x.1p %in% x.2p, ]
  }

# Funtion to replace multiple -9 across lots of data tables
replace_function=function(DT){
  cnames <- colnames(DT)
  for(cname in cnames) {
    set(DT, j = cname, value = gsub("[[:space:]]", "", DT[[cname]]))
  }
  for(cname in cnames){
    set(DT, i = which(DT[[cname]] == -9), j = cname, value = NA)
  }
}

# Function to create a random vector of numbers
rand_vect <- function(N, M, sd = 1, pos.only = TRUE) {
  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}

## Ruth's function for removing rows where all values are NA. CM
rem_na_df <- function(dat_fr = mydata) {
  
  # Create function which counts the number of NA's in each row.
  countNAs <- function(x) {length(which(is.na(x)))}
  numNAs <- apply(dat_fr,1,countNAs)
  # apply this function to each row (2nd argument = 1) in the dataset using apply,
  # and store this in a new vector called numNAs
  
  nrow(dat_fr[numNAs == ncol(dat_fr),])
  
  #  Delete all blank rows by Deleting rows where all cells are NA's
  #  These will be rows where numNAs is equal to the number col of the dataframe
  dat_fr <- dat_fr[numNAs != ncol(dat_fr),]
  
}

# If FALSE, mostly suppress CI computation
need.CI <- FALSE
# Number of bootstrap replicates
if(need.CI){
  nb <- 1000
}else{
  nb <- 3 ## just to make code run through
}
#
# Confidence interval range
CV <- .95
# Set seed for reproducible results
setSeed <- set.seed(627)

save(list=ls(all=T), file = "./script1_output.rda")
#load("./script1_output.rda")
