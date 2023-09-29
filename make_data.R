# Code for creating the datasets in the right format for pendling visualisation
# Code is in three parts:
#   1. Setup - change year to create new dataset for the new year
#   2. Create kommune shape and midpoint files
#   3. Create data for kommune and pendling


#### 1. Setup ####
# Load library for SSBs API
library(PxWebApiData)
library(rgdal) # Being retired!!
library(sf)
library(sp)
library(lubridate)

# Load functions (need Make_kommune function from here)
source("Dotmap_Functions.R")

# Specify which years to create data for
years_all = "2022"



#### 2. Create kommune shape files and midpoints ####

# Function to adjusts names for duplicate kommune to include fylke
change_duplicates <- function(komm_punkt, year){
  if (year < 2020){
    komm_punkt$NAVN[komm_punkt$NR == "0236"] <- "Nes (Akershus)"
    komm_punkt$NAVN[komm_punkt$NR == "0616"] <- "Nes (Buskerud)"
    komm_punkt$NAVN[komm_punkt$NR == "0713"] <- "Sande (Vestfold)"
    komm_punkt$NAVN[komm_punkt$NR == "1514"] <- "Sande (Møre og Romsdal)"
    komm_punkt$NAVN[komm_punkt$NR == "0137"] <- "Våler (Østfold)"
    komm_punkt$NAVN[komm_punkt$NR == "0426"] <- "Våler (Hedmark)"    
    komm_punkt$NAVN[komm_punkt$NR == "0441"] <- "Os (Hedmark)"
    komm_punkt$NAVN[komm_punkt$NR == "1243"] <- "Os (Hordaland)"
    komm_punkt$NAVN[komm_punkt$NR == "0821"] <- "Bø (Telemark)"
    komm_punkt$NAVN[komm_punkt$NR == "1867"] <- "Bø (Nordland)"
    komm_punkt$NAVN[komm_punkt$NR == "1515"] <- "Herøy (Møre og Romsdal)"
    komm_punkt$NAVN[komm_punkt$NR == "1818"] <- "Herøy (Nordland)"
  } else {
    komm_punkt$NAVN[komm_punkt$NR == "3018"] <- "Våler (Viken)"
    komm_punkt$NAVN[komm_punkt$NR == "3419"] <- "Våler (Innlandet)"
    komm_punkt$NAVN[komm_punkt$NR == "1515"] <- "Herøy (Møre og Romsdal)"
    komm_punkt$NAVN[komm_punkt$NR == "1818"] <- "Herøy (Nordland)"
  }
  return(komm_punkt)
}

# funksjon for å lagre kommunegrenser spatial polygon datasett i R
shapeConverter <- function(year){
  yearfil <- year
  filnavn <- paste0("S:/Faglig/Kommunikasjon/Publisering/Kart/GISkartgrunnlag/_OLD/Illustrasjonskart", yearfil, ".gdb")
  while (!file.exists(filnavn) & yearfil >= 2017){
    yearfil <- as.numeric(yearfil) - 1
    filnavn <- paste0("S:/Faglig/Kommunikasjon/Publisering/Kart/GISkartgrunnlag/_OLD/Illustrasjonskart", yearfil, ".gdb")
  }
  if (!file.exists(filnavn)){
    stop("No kommune map file found")
    }
  
  laynavn <- paste0("N5000_kommune_flate_", yearfil)
  komm_shape <- st_read(filnavn, layer = laynavn)
  komm_shape <- sf:::as_Spatial(komm_shape)
  komm_shape <- sp::spTransform(komm_shape, sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0"))
  
  
  # format names
  komm_shape$KOMM <- as.numeric(komm_shape$KOMMUNENR)
  komm_shape$NR <- komm_shape$KOMMUNENR
  komm_shape$NAVN <- as.character(komm_shape$NAVN)
  Encoding(komm_shape$NAVN) <- "UTF-8"
  
  # change duplicate names to include fylke
  komm_shape <- change_duplicates(komm_shape, year)
  
  # save to data folder
  name <- paste0("komm_shape", year)
  assign(name, komm_shape)
  save(list = name, file=paste0("data/komm_shape", year, ".RData")) 
  
  # find midpoint for kommune_punkt dataset
  komm_punkt <- coordinates(komm_shape)
  komm_punkt <- sp::SpatialPointsDataFrame(coords=komm_punkt, data=komm_shape@data, 
                                           proj4string=sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0"))

  komm_punkt<- as.data.frame(komm_punkt, stringsAsFactors = FALSE)
  komm_punkt <- komm_punkt[,c("NAVN", "coords.x1", "coords.x2", "NR", "KOMM")]
  names(komm_punkt)[c(2,3)] <- c("lng", "lat")
  name <- paste0("komm_punkt", year) # create name for object including year
  assign(name, komm_punkt) # assign new name
  save(list = name, file=paste0("data/komm_punkt", year, ".RData")) # save to data folder

}

for (y in years_all){
  shapeConverter(y)
}

#### 3. Create data for pendling numbers and population numbers ####
for (y in years_all){
  
  # Make and save kommune (population) dataset
  load(paste0("data/komm_punkt", y, ".RData"))
  komm_punkt <- get(paste0("komm_punkt", y))
  data_kommune <- ApiData(url = "https://data.ssb.no/api/v0/no/table/11618/"
                          , Region = TRUE, Kjonn = "0", Tid = y, Alder = "15-74",
                          ContentsCode =TRUE
  )
  data_kommune <- Make_kommune(data_kommune$dataset, komm_var = "Region", komm_punkt = komm_punkt)
  saveRDS(data_kommune, file = paste0("data/data_kommune", y, ".rds"))
  
  # Make and save pendler dataset
  data_pendling <- ApiData(url = "https://data.ssb.no/api/v0/no/table/03321/"
                           , Tid = y, ArbstedKomm = TRUE, Bokommuen=TRUE)
  
  data_pendling <- Make_kommune(data_pendling$dataset, "Bokommuen", komm_punkt = komm_punkt) #ta ut data vi ikke har punkter for
  data_pendling <- Make_kommune(data_pendling, "ArbstedKomm", komm_punkt = komm_punkt) # ta ut linje vi ikke har arbsted punkt for
  
  saveRDS(data_pendling, file = paste0("data/data_pendling", y, ".rds"))
}
