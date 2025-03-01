library(tidyverse)
library(patchwork)
library(ggsci)
library(scico)
library(skimr)

########## 1. Import data ########## 
#---- Set working directory ----
getwd()
setwd("/Users/jackward/Documents/01_Projects/11_Brenainn/01_Data/01_Import") ## You need to set this to the appropriate directory
getwd()

#---- Import data ----
GEOROC_Clinopyroxene <- read_csv("2023-06-SGFTFN_CLINOPYROXENES_Brenainn.csv") 

#---- Check data ----
skim(GEOROC_Clinopyroxene)

########## 2. Format data ########## 
#---- Get column names ----
colnames(GEOROC_Clinopyroxene)

#---- Select relevant columns ----
GEOROC_Clinopyroxene_Working <- GEOROC_Clinopyroxene %>% 
  select(CITATION, `SAMPLE NAME`, `TECTONIC SETTING`, LOCATION, `LATITUDE (MIN.)`, `LONGITUDE (MIN.)`, `ROCK NAME`, 
         ALTERATION, MINERAL, SPOT, CRYSTAL, `PRIMARY/SECONDARY`, `MGO(WT%)`, `NA2O(WT%)`, `K2O(WT%)`, 
         `MNO(WT%)`, `P2O5(WT%)`, `AL2O3(WT%)`, `SIO2(WT%)`, `CAO(WT%)`, `FEO(WT%)`, `FEOT(WT%)`, `FE2O3(WT%)`, 
         `FE2O3T(WT%)`, `TIO2(WT%)`, `NIO(WT%)`, `CR2O3(WT%)`, `BAO(WT%)`, `SRO(WT%)`, `ZRO2(WT%)`, `ZNO(WT%)`)

#---- Rename columns ----
colnames(GEOROC_Clinopyroxene_Working)

colnames(GEOROC_Clinopyroxene_Working) <- c("Citation", "SampleName", "TectonicSetting", "Location", "Latitude", 
                                            "Longitude", "RockName", "Alteration", "Mineral", "Spot", "Crystal", 
                                            "PrimarySecondary", "MgO", "Na2O", "K2O", "MnO", "P2O5", "Al2O3", "SiO2", 
                                            "CaO", "FeO", "FeOT", "Fe2O3", "Fe2O3T", "TiO2", "NiO", "Cr2O3", "BaO", 
                                            "SrO", "ZrO2", "ZnO")

colnames(GEOROC_Clinopyroxene_Working)

#---- Correct column classes ----
GEOROC_Clinopyroxene_Working[, c(1:4, 7:12)] <- sapply(GEOROC_Clinopyroxene_Working[, c(1:4, 7:12)], as.character)
GEOROC_Clinopyroxene_Working[, c(5:6, 13:31)] <- sapply(GEOROC_Clinopyroxene_Working[, c(5:6, 13:31)], as.numeric)

#---- Format data frame as tibble ----
GEOROC_Clinopyroxene_Working <- GEOROC_Clinopyroxene_Working %>% 
  as_tibble()

#---- View data ----
View(GEOROC_Clinopyroxene_Working)

#---- Format references ----
## Create master citation column
GEOROC_Clinopyroxene_Working$CitationMaster <- GEOROC_Clinopyroxene_Working$Citation

## Format reference column
GEOROC_Clinopyroxene_Working <- GEOROC_Clinopyroxene_Working %>% 
  separate(Citation, c("ReferenceNumber", "Author", "Year"), sep = " ")

#---- Check data ----
View(GEOROC_Clinopyroxene_Working)

#---- Remove "Author" and "Year" columns ----
GEOROC_Clinopyroxene_Working$Author <-  NULL
GEOROC_Clinopyroxene_Working$Year <-  NULL

#---- Filter based on geochemistry ----
GEOROC_Clinopyroxene_Filtered <- GEOROC_Clinopyroxene_Working %>% 
  filter(PrimarySecondary == "primary", FeOT > 0, !is.na(TectonicSetting))

#---- Check filtered data set ----
skim(GEOROC_Clinopyroxene_Filtered)

#---- Order filtered data set to same as stoichiometry spreadsheet ----
GEOROC_Clinopyroxene_Filtered <- GEOROC_Clinopyroxene_Filtered %>% 
  select(ReferenceNumber, SampleName, TectonicSetting, Location, Latitude,  Longitude,  RockName, Alteration, Mineral,
         Spot, Crystal, PrimarySecondary, SiO2, TiO2, Al2O3, Cr2O3, MgO, FeOT, NiO, MnO, CaO, Na2O, K2O, BaO, P2O5, SrO, 
         ZrO2, ZnO)

#---- Check column names ----
colnames(GEOROC_Clinopyroxene_Filtered)

#---- Export data ----
write.csv(GEOROC_Clinopyroxene_Filtered, file = "/Users/jackward/Documents/01_Projects/11_Brenainn/01_Data/02_Export/GEOROC_Clinopyroxene_Working_Brenainn_Formatted.csv", na = "", row.names = FALSE)

