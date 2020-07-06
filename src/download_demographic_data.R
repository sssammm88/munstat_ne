# title: download_demographic_data ----

# Description ----

# Author: Samuel Colin
# Date: 06.07.2020
# Goal: the goal of this script is to download, reformat and save
# the demographic data for the munstat_ne project

# Seed
# set.seed(12345)

# Libraries ----

library("tidyverse")
library("BFS")
library("utils")
library("readxl")
library("magrittr")

# Functions and other sources ----

# None

# Download demographic data ----

#' Download, format and save demographicdata from FSO website.
#' 
#' No parameter is needed
#' 
#' @param url_demographics the px_url to the dataset
#' @param url_mun the url to the Excel file containing municipality id
#' @param save_path the path to which the .csv file should be saved
#' @param verbose do we want the program to be verbose?
#' @return nothing (a .csv file is written on the disc)
download_demographic_data <- function(
  url_demographics = "https://www.bfs.admin.ch/bfsstatic/dam/assets/9566432/master",
  url_mun = "https://www.bfs.admin.ch/bfsstatic/dam/assets/11467406/master",
  save_path = "../data/demographics.csv",
  verbose = FALSE
){
  # Download data and create new row with mun_id
  if (verbose) {
    print("Download demographic data")
  }
  data_demographics <- bfs_get_dataset(
    url_px = url_demographics, 
    language = "fr"
  )
  data_demographics$mun_id <- as.integer(
    data_demographics$canton_district_commune %>% 
      str_sub(start = 7L, end = 10L)
  )
  
  # Download and filter the mun_id data
  if (verbose) {
    print("Download municipal id data")
  }
  download.file(url = url_mun, destfile = "../data/mun_id.xlsx")
  mun_id_data <- read_excel("../data/mun_id.xlsx", sheet = "GDE")
  mun_id_ne <- mun_id_data %>% filter(GDEKT %in% "NE")
  
  # Keep only demographics from the canton of Neuchâtel
  if (verbose) {
    print("Join ")
  }
  data_demographics_ne <- inner_join(
    x = data_demographics,
    y = mun_id_ne,
    by = c("mun_id" = "GDENR")
  )
  
  # Reorganize, renameand save data
  data_demographics_ne %<>% select(
    GDENAME, mun_id, annee, sexe, nationalite_categorie,
    composante_demographique, value
  )
  names(data_demographics_ne)[7] <- "nbr_people"
  if (verbose){
    print("Save data")
  }
  write_csv(data_demographics_ne, path = save_path)
}
