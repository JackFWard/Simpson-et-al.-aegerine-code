library(tidyverse)
library(patchwork)
library(ggsci)
library(scico)
library(skimr)
library(ggtern)

########## 1. Import data ########## 
#---- Set working directory ----
getwd()
setwd("/Users/jackward/Documents/01_Projects/11_Brenainn/01_Data/03_Working") ## You need to set this to the appropriate directory
getwd()

#---- Import data ----
GEOROC_CPX_Stoich <- read_csv("Brenainn_Modified_CalcStructuralFormulae_20231019.csv") ## You need to set this to the appropriate file name

#---- Check data ----
colnames(GEOROC_CPX_Stoich)

########## 2. Format data ##########
#---- Select relevant columns ----
GEOROC_CPX_Stoich <- GEOROC_CPX_Stoich %>% 
  select(ReferenceNumber, SampleName, TectonicSetting, Location, Latitude,  Longitude,  RockName, Alteration, Mineral,
         Spot, Crystal, PrimarySecondary, SiO2...13, TiO2...14, Al2O3...15, Cr2O3...16, MgO...17, FeOt...18, NiO...19, 
         MnO...20, CaO...21, Na2O...22, K2O...23, BaO...24, P2O5, SrO, ZrO2...27, ZnO, Total...31, `Mg#`, evaluation, 
         Mg...44, `Fe2+...45`, `Fe2+...50`, Mn...52, Na...54, En...63, Fs...64, Wo...65, grupo, `Q+J`, Total...57)

#---- Rename columns ----
colnames(GEOROC_CPX_Stoich)

colnames(GEOROC_CPX_Stoich) <- c("ReferenceNumber", "SampleName", "TectonicSetting", "Location", "Latitude",  "Longitude",  
                                 "RockName", "Alteration", "Mineral", "Spot", "Crystal", "PrimarySecondary", "SiO2", "TiO2", 
                                 "Al2O3", "Cr2O3", "MgO", "FeOt", "NiO", "MnO", "CaO", "Na2O", "K2O", "BaO", "P2O5", "SrO", 
                                 "ZrO2", "ZnO", "Total", "MgNum", "Evaluation", "Mg", "Fe2_a", "Fe2_b", "Mn", "Na", "En", 
                                 "Fs", "Wo", "Group", "QJ", "TotalCations")

colnames(GEOROC_CPX_Stoich)

#---- Perform geochemistry calculations ----
GEOROC_CPX_Stoich$Fe2Sum <- GEOROC_CPX_Stoich$Fe2_a + GEOROC_CPX_Stoich$Fe2_b
GEOROC_CPX_Stoich$Fe2MnSum <- GEOROC_CPX_Stoich$Fe2Sum + GEOROC_CPX_Stoich$Mn

#---- Generate "RockNameWorking" column ----
## Create RockNameMaster column
GEOROC_CPX_Stoich$RockNameMaster <- GEOROC_CPX_Stoich$RockName

## Correct column type
GEOROC_CPX_Stoich$RockName <- as.character(GEOROC_CPX_Stoich$RockName)

## Separate columns
RockNamesSplit <- str_split_fixed(GEOROC_CPX_Stoich$RockName, pattern = ", ", n = 3) %>% 
  as_tibble() 

## Rename columns 
colnames(RockNamesSplit) <- c("RockNameWorking", "RockNameDescriptor1", "RockNameDescriptor2")

#---- Merge "RockNamesSplit" with main data frame ----
GEOROC_CPX_Stoich <- bind_cols(GEOROC_CPX_Stoich, RockNamesSplit)

#---- Correct rock names ----
GEOROC_CPX_Stoich <- GEOROC_CPX_Stoich %>% 
  mutate(RockNameFinal = case_when(RockNameWorking == "NOT GIVEN" ~ "NotGiven",
                                   RockNameWorking == "BASALT" ~ "Basalt",
                                   RockNameWorking == "ANDESITE" ~ "Andesite",
                                   RockNameWorking == "LHERZOLITE" ~ "Peridotite",
                                   RockNameWorking == "KIMBERLITE" ~ "Kimberlite",
                                   RockNameWorking == "RHYOLITE" ~ "Rhyolite",
                                   RockNameWorking == "GABBRO" ~ "Gabbro",
                                   RockNameWorking == "CLINOPYROXENITE" ~ "Peridotite",
                                   RockNameWorking == "TRACHYTE" ~ "Trachyte",
                                   RockNameWorking == "PERIDOTITE" ~ "Peridotite",
                                   RockNameWorking == "LAMPROPHYRE" ~ "Lamprophyre",
                                   RockNameWorking == "BASANITE" ~ "Basanite",
                                   RockNameWorking == "HARZBURGITE" ~ "Peridotite",
                                   RockNameWorking == "DACITE" ~ "Dacite",
                                   RockNameWorking == "TEPHRITE" ~ "Tephrite",
                                   RockNameWorking == "WEHRLITE" ~ "Peridotite",
                                   RockNameWorking == "PYROXENITE" ~ "Peridotite",
                                   RockNameWorking == "TRACHYANDESITE" ~ "Trachyandesite",
                                   RockNameWorking == "PICRITE" ~ "Picrite",
                                   RockNameWorking == "DOLERITE" ~ "Dolerite",
                                   RockNameWorking == "WEBSTERITE" ~ "Peridotite",
                                   RockNameWorking == "TRACHYBASALT" ~ "Trachybasalt",
                                   RockNameWorking == "PHONOLITE" ~ "Phonolite",
                                   RockNameWorking == "LAMPROITE" ~ "Lamproite",
                                   RockNameWorking == "ECLOGITE" ~ "Eclogite",
                                   RockNameWorking == "LATITE" ~ "Trachyandesite",
                                   RockNameWorking == "SYENITE" ~ "Syenite",
                                   RockNameWorking == "NEPHELINITE" ~ "Foidite",
                                   RockNameWorking == "DIORITE" ~ "Diorite",
                                   RockNameWorking == "SHOSHONITE" ~ "Basaltic-trachyandesite",
                                   RockNameWorking == "GRANULITE" ~ "Granulite",
                                   RockNameWorking == "THOLEIITE" ~ "Tholeiitic basalt",
                                   RockNameWorking == "HAWAIITE" ~ "Trachybasalt",
                                   RockNameWorking == "DUNITE" ~ "Peridotite",
                                   RockNameWorking == "GABBRONORITE" ~ "Gabbro",
                                   RockNameWorking == "PHONOTEPHRITE" ~ "Phonotephrite",
                                   RockNameWorking == "CARBONATITE" ~ "Carbonatite",
                                   RockNameWorking == "DIABASE" ~ "Dolerite",
                                   RockNameWorking == "SKARN" ~ "Skarn",
                                   RockNameWorking == "MONZODIORITE" ~ "Monzodiorite",
                                   RockNameWorking == "MUGEARITE" ~ "Basaltic-trachyandesite",
                                   RockNameWorking == "NORITE" ~ "Gabbro",
                                   RockNameWorking == "MAFURITE" ~ "Kamafugite",
                                   RockNameWorking == "MONZONITE" ~ "Monzonite",
                                   RockNameWorking == "MEGACRYST" ~ "Megacryst",
                                   RockNameWorking == "ARENITE" ~ "Arenite",
                                   RockNameWorking == "HORNBLENDITE" ~ "Hornblendite",
                                   RockNameWorking == "KERSANTITE" ~ "Lamprophyre",
                                   RockNameWorking == "ANKARAMITE" ~ "Basanite",
                                   RockNameWorking == "IJOLITE" ~ "Foid-bearing syenite",
                                   RockNameWorking == "BENMOREITE" ~ "Trachyandesite",
                                   RockNameWorking == "MELILITITE" ~ "Picrite",
                                   RockNameWorking == "LEUCITITE" ~ "Leucitite",
                                   RockNameWorking == "KOMATIITE" ~ "Picrite",
                                   RockNameWorking == "TEPHRIPHONOLITE" ~ "Tephriphonolite",
                                   RockNameWorking == "HORNFELS" ~ "Hornfelds",
                                   RockNameWorking == "MONZOGABBRO" ~ "Monzogabbro",
                                   RockNameWorking == "GRANITE" ~ "Granite",
                                   RockNameWorking == "BONINITE" ~ "Andesite",
                                   RockNameWorking == "TROCTOLITE" ~ "Gabbro",
                                   RockNameWorking == "PANTELLERITE" ~ "Rhyolite",
                                   RockNameWorking == "RHYODACITE" ~ "Rhyolite",
                                   RockNameWorking == "ORTHOPYROXENITE" ~ "Peridotite",
                                   RockNameWorking == "ALBITITE" ~ "Albitite",
                                   RockNameWorking == "MELANEPHELINITE" ~ "Foidite",
                                   RockNameWorking == "PEGMATITE" ~ "Pegmatite",
                                   RockNameWorking == "ANORTHOSITE" ~ "Gabbro",
                                   RockNameWorking == "MEIMECHITE" ~ "Picrite",
                                   RockNameWorking == "MELAGABBRO" ~ "Gabbro",
                                   RockNameWorking == "SERPENTINITE" ~ "Serpentinite",
                                   RockNameWorking == "METAGABBRO" ~ "Metagabbro",
                                   RockNameWorking == "AMPHIBOLITE" ~ "Amphibolite",
                                   RockNameWorking == "MONCHIQUITE" ~ "Lamprophyre",
                                   RockNameWorking == "HAUYNITE" ~ "Hauynite",
                                   RockNameWorking == "MINETTE" ~ "Minette",
                                   RockNameWorking == "LEUCOGABBRO" ~ "Gabbro",
                                   RockNameWorking == "FENITE" ~ "Fenite",
                                   RockNameWorking == "COMENDITE" ~ "Rhyolite",
                                   RockNameWorking == "TRACHYPHONOLITE" ~ "Trachyphonolite",
                                   RockNameWorking == "NOSEANITE" ~ "Foidite",
                                   RockNameWorking == "SPESSARTINE" ~ "Lamprophyre",
                                   RockNameWorking == "JACUPRINAGITE" ~ "Foid-bearing syenite",
                                   RockNameWorking == "LIMBURGITE" ~ "Basanite",
                                   RockNameWorking == "UGANDITE" ~ "Kamafugite",
                                   RockNameWorking == "FELSITE" ~ "Felsite",
                                   RockNameWorking == "GRANODIORITE" ~ "Granodiorite",
                                   RockNameWorking == "PERIDODITE" ~ "Peridotite",
                                   RockNameWorking == "CAMPTONITE" ~ "Lamprophyre",
                                   RockNameWorking == "METAGRANITE" ~ "Metagranite",
                                   RockNameWorking == "FOIDITE" ~ "Foidite",
                                   RockNameWorking == "GABBRODIORITE" ~ "Gabbrodiorite",
                                   RockNameWorking == "TONALITE" ~ "Tonalite",
                                   RockNameWorking == "GABBRODOLERITE" ~ "Gabbro",
                                   RockNameWorking == "SANNAITE" ~ "Lamprophyre",
                                   RockNameWorking == "SANDSTONE" ~ "Sandstone",
                                   RockNameWorking == "MONZOSYENITE" ~ "Monzosyenite",
                                   RockNameWorking == "TRACHYDACITE" ~ "Trachydacite",
                                   RockNameWorking == "CHROMITITE" ~ "Chromitite",
                                   RockNameWorking == "NOTGIVEN" ~ "NotGiven",
                                   RockNameWorking == "WEHLERITE" ~ "Peridotite",
                                   RockNameWorking == "MELILITOLITE" ~ "Melilitolite",
                                   RockNameWorking == "MYLONITE" ~ "Mylonite",
                                   RockNameWorking == "ANKARATRITE" ~ "Foidite",
                                   RockNameWorking == "ESSEXITE" ~ "Trachybasalt",
                                   RockNameWorking == "METAGABBRONORITE" ~ "Metagabbronorite",
                                   RockNameWorking == "ALN÷ITE" ~ "Alnite",
                                   RockNameWorking == "KAMAFUGITE" ~ "Kamafugite",
                                   RockNameWorking == "CHARNOCKITE" ~ "Charnockite",
                                   RockNameWorking == "GREENSTONE" ~ "Greenstone",
                                   RockNameWorking == "ORE" ~ "Ore",
                                   RockNameWorking == "FOYAITE" ~ "Foid-bearing syenite",
                                   RockNameWorking == "MELTEIGITE" ~ "Foid-bearing syenite",
                                   RockNameWorking == "BASANITOID" ~ "Basanitoid",
                                   RockNameWorking == "GNEISS" ~ "Gneiss",
                                   RockNameWorking == "SYENODIORITE" ~ "Syenodiorite",
                                   RockNameWorking == "WYOMINGITE" ~ "Lamproite",
                                   RockNameWorking == "ABSAROKITE" ~ "Basaltic-trachyandesite",
                                   RockNameWorking == "MELADIORITE" ~ "Diorite",
                                   RockNameWorking == "MONZOGRANITE" ~ "Monzogranite",
                                   RockNameWorking == "METABASALT" ~ "Metabasalt",
                                   RockNameWorking == "METADOLERITE" ~ "Metadolerite",
                                   RockNameWorking == "MICROGRANITE" ~ "Granite",
                                   RockNameWorking == "SCHIST" ~ "Schist",
                                   RockNameWorking == "THERALITE" ~ "Foid-bearing gabbro",
                                   RockNameWorking == "ADAKITE" ~ "Adakite",
                                   RockNameWorking == "APPINITE" ~ "Appinite",
                                   RockNameWorking == "MONZOGABBRODIORITE" ~ "Monzogabbrodiorite",
                                   RockNameWorking == "PHOSCORITE" ~ "Phoscorite",
                                   RockNameWorking == "GRANOPHYRE" ~ "Granophyre",
                                   RockNameWorking == "LAMPYRITE" ~ "Lamproite",
                                   RockNameWorking == "MALIGNITE" ~ "Foid-bearing syenite",
                                   RockNameWorking == "MELAMONZOGABBRO" ~ "Monzogabbro",
                                   RockNameWorking == "TINGUAITE" ~ "Phonolite",
                                   RockNameWorking == "BENMORITE" ~ "Trachyandesite",
                                   RockNameWorking == "BRECCIA" ~ "Breccia",
                                   RockNameWorking == "OKAITE" ~ "Okaite",
                                   RockNameWorking == "VAUGNERITE" ~ "Vaugnerite",
                                   RockNameWorking == "DIOPSIDITE" ~ "Diopsidite",
                                   RockNameWorking == "PORPHYRY" ~ "Porphyry",
                                   RockNameWorking == "FOIDOLITE" ~ "Foidolite",
                                   RockNameWorking == "GLIMMERITE" ~ "Glimmerite",
                                   RockNameWorking == "TRACHYDOLERITE" ~ "Trachydolerite",
                                   RockNameWorking == "TRISTANITE" ~ "Tristanite",
                                   RockNameWorking == "ALVIKITE" ~ "Alvikite",
                                   RockNameWorking == "BUCHITE" ~ "Buchite",
                                   RockNameWorking == "HUANOPHYRE" ~ "Huanophyre",
                                   RockNameWorking == "MARIANITE" ~ "Marianite",
                                   RockNameWorking == "SOVITE" ~ "Sovite",
                                   RockNameWorking == "APATITITE" ~ "Apatitite",
                                   RockNameWorking == "HARZBURGITE†" ~ "Periodotite",
                                   RockNameWorking == "JOTUNITE" ~ "Jotunite",
                                   RockNameWorking == "METAPYROXENITE" ~ "Metapyroxenite",
                                   RockNameWorking == "PICRODOLERITE" ~ "Picrodolerite",
                                   RockNameWorking == "POLZENITE" ~ "Polzenite",
                                   RockNameWorking == "SHONKINITE" ~ "Shonkinite",
                                   RockNameWorking == "URTITE" ~ "Urtite",
                                   RockNameWorking == "LEUCODOLERITE" ~ "Lecuodolerite",
                                   RockNameWorking == "MARBLE" ~ "Marble",
                                   RockNameWorking == "TURJAITE" ~ "Turjaite",
                                   RockNameWorking == "AGPAITE" ~ "Agpaite",
                                   RockNameWorking == "ALKREMITE" ~ "Alkremite",
                                   RockNameWorking == "BERGALITE" ~ "Bergalite",
                                   RockNameWorking == "ICELANDITE" ~ "Icelandite",
                                   RockNameWorking == "KALSILITITE" ~ "Kalsilitite",
                                   RockNameWorking == "KATUNGITE" ~ "Katungite",
                                   RockNameWorking == "LEUCODIABASE" ~ "Leucodiabase",
                                   RockNameWorking == "LEUCOGRANITE" ~ "Leucogranite",
                                   RockNameWorking == "MELAYSENITE" ~ "Melasyenite",
                                   RockNameWorking == "METADIORITE" ~ "Metadiorite",
                                   RockNameWorking == "METALIMESTONE" ~ "Metalimestone",
                                   RockNameWorking == "METAQUARTZITE" ~ "Metaquartzite",
                                   RockNameWorking == "OCEANITE" ~ "Oceanite",
                                   RockNameWorking == "OLIVINITE" ~ "Olivinite",
                                   RockNameWorking == "ORENDITE" ~ "Orendite",
                                   RockNameWorking == "ORTHOPYROXENITE; PLAGIOCLASE" ~ "Plagioclase orthopyroxenite",
                                   RockNameWorking == "PHLOGOPITE" ~ "Phlogopite",
                                   RockNameWorking == "SPILITE" ~ "Spilite",
                                   RockNameWorking == "SYENOGRANITE" ~ "Syenogranite",
                                   RockNameWorking == "VIPETOITE" ~ "Vipetoite",
                                   RockNameWorking == "ALLIVALITE" ~ "Allivalite",
                                   RockNameWorking == "BLUESCHIST" ~ "Blueschist",
                                   RockNameWorking == "DURBACHITE" ~ "Durbachite",
                                   RockNameWorking == "FERGUSITE" ~ "Fergusite",
                                   RockNameWorking == "MADUPITE" ~ "Madupite",
                                   RockNameWorking == "MELAMONZONITE" ~ "Melamonzonite",
                                   RockNameWorking == "MONZONORITE" ~ "Monzorite",
                                   RockNameWorking == "SAND" ~ "Sand",
                                   RockNameWorking == "VOGESITE" ~ "Vogesite",
                                   RockNameWorking == "WEHRLITE. SPINEL" ~ "Spinel Wehrlite",
                                   TRUE ~ RockNameWorking))

#---- Classify rock type ----
GEOROC_CPX_Stoich$RockTypeFinal <- ifelse(GEOROC_CPX_Stoich$RockNameFinal %in% c("Adakite", "Andesite", "Basalt", "Basaltic-trachyandesite", "Basanite",
                                                                                     "Dacite", "Foidite", "Kamafugite", "Phonolite", "Phonotephrite", 
                                                                                     "Picrite", "Rhyolite", "Tephriphonolite", "Tephrite",
                                                                                     "Tholeiite", "Trachyandesite", "Trachybasalt", "Trachydacite", 
                                                                                     "Trachyphonolite", "Trachyte"), "Volcanic",
                                            ifelse(GEOROC_CPX_Stoich$RockNameFinal %in% c("Dolerite", "Granophyre", "Kimberlite", "Lamproite", 
                                                                                            "Lamprophyre"), "Subvolcanic",
                                                   ifelse(GEOROC_CPX_Stoich$RockNameFinal %in% c("Diorite", "Foid-bearing Gabbro", "Foid-bearing syenite", "Gabbro",
                                                                                                   "Granite", "Granodiorite", "Melilitolite", "Monzodiorite",
                                                                                                   "Monzogabbro", "Monzogranite", "Monzonite", "Monzosyenite",
                                                                                                   "Pegmatite", "Peridotite", "Phoscorite", "Syenite", "Tonalite"), "Plutonic", "Other")))

#---- Final rock name corrections ----
## Basaltic andesite
GEOROC_CPX_Stoich$RockNameFinal <- ifelse(GEOROC_CPX_Stoich$RockNameWorking == "ANDESITE" & GEOROC_CPX_Stoich$RockNameMaster %in% c("ANDESITE, BASALTIC", "ANDESITE, BASALTIC, 2-PYROXENE",
                                                                                                                                    "ANDESITE, BASALTIC, AMPHIBOLE-CLINOPYROXENE", 
                                                                                                                                    "ANDESITE, BASALTIC, BRONZITE-AUGITE",
                                                                                                                                    "ANDESITE, BASALTIC, CALC-ALKALINE",
                                                                                                                                    "ANDESITE, BASALTIC, CLINOPYROXENE",
                                                                                                                                    "ANDESITE, BASALTIC, OLIVINE",
                                                                                                                                    "ANDESITE, BASALTIC, OLIVINE-BRONZITE-AUGITE",
                                                                                                                                    "ANDESITE, BASALTIC, SHOSHONITIC",
                                                                                                                                    "ANDESITE, BASALTIC, SUBALKALINE",
                                                                                                                                    "ANDESITE, BASALTIC, THOLEIITIC"), "Basaltic andesite",
                                                         GEOROC_CPX_Stoich$RockNameFinal)

## Alkali basalt
GEOROC_CPX_Stoich$RockNameFinal <- ifelse(GEOROC_CPX_Stoich$RockNameWorking == "BASALT" & GEOROC_CPX_Stoich$RockNameMaster %in% c("BASALT, ALKALINE", "BASALT, ALKALINE, OLIVINE",
                                                                                                                                  "BASALT, ALKALINE, TRANSITIONAL"), "Basalt",
                                                         GEOROC_CPX_Stoich$RockNameFinal)

#---- Classify TAS series for Volcanic and Plutonic rocks ----
GEOROC_CPX_Stoich$TAS_Series <- ifelse(GEOROC_CPX_Stoich$RockNameFinal %in% c("Basalt", "Basaltic andesite", "Rhyolite", "Andesite", "Picrite", 
                                                                              "Dacite", "Gabbro", "Diorite", "Granite", "Granodiorite", "Tonalite"), "1. Series_A",
                                       ifelse(GEOROC_CPX_Stoich$RockNameFinal %in% c("Trachyte", "Trachyandesite", "Trachybasalt", "Basaltic-trachyandesite",
                                                                                     "Trachyphonolite", "Trachydacite", "Syenite", "Monzogranite", "Monzodiorite",
                                                                                     "Monzonite", "Monzogabbro", "Monzosyenite"), "2. Series_B",
                                              ifelse(GEOROC_CPX_Stoich$RockNameFinal %in% c("Basanite", "Tephrite", "Phonolite", "Phonotephrite",
                                                                                            "Tephriphonolite", "Foid-bearing syenite"), "3. Series_C",
                                                     ifelse(GEOROC_CPX_Stoich$RockNameFinal %in% c("Foidite"), "4. Series_D", "5. Other"))))

#---- Classify maficity ----
GEOROC_CPX_Stoich$MaficityGroup <- ifelse(GEOROC_CPX_Stoich$RockNameFinal %in% c("Kamafugite", "Kimberlite", "Lamproite", "Lamprophyre", "Melilitolite", 
                                                                            "Peridotite", "Phoscorite", "Picrite"), "1. Ultramafic",
                                     ifelse(GEOROC_CPX_Stoich$RockNameFinal %in% c("Basanite", "Foidite", "Tephrite"), "2. Ultramafic–mafic",
                                            ifelse(GEOROC_CPX_Stoich$RockNameFinal %in% c("Basalt", "Dolerite", "Gabbro", "Monzogabbro", 
                                                                                          "Trachybasalt"), "3. Mafic",
                                                   ifelse(GEOROC_CPX_Stoich$RockNameFinal %in% c("Basaltic-trachyandesite", "Monzodiorite", 
                                                                                                 "Phonotephrite", "Tephriphonolite"), "4. Mafic–intermediate",
                                                          ifelse(GEOROC_CPX_Stoich$RockNameFinal %in% c("Andesite", "Basaltic andesite", "Diorite", 
                                                                                                        "Foid-bearing syenite", "Monzonite", "Phonolite", 
                                                                                                        "Trachyandesite"), "5. Intermediate",
                                                                 ifelse(GEOROC_CPX_Stoich$RockNameFinal %in% c("Dacite", "Granite", "Granodiorite",
                                                                                                               "Granophyre", "Monzogranite", "Monzosyenite",
                                                                                                               "Pegmatite", "Rhyolite", "Syenite", "Tonalite",
                                                                                                               "Trachydacite", "Trachyphonolite", "Trachyte"), "6. Felsic", "Other"))))))

#---- Remove unwanted basalt varieties ----
GEOROC_CPX_Stoich <- GEOROC_CPX_Stoich %>% 
  filter(!RockNameMaster %in% c("BASALT, THOLEIITIC", "BASALT, OLIVINE", "BASALT, SUBALKALINE", "BASALT, PICRITIC", "BASALT, AMPHIBOLE", 
                                "BASALT, TRANSITIONAL", "BASALT, CALC-ALKALINE", "BASALT, CLINOPYROXENE", "BASALT, ANDESITIC", "BASALT, CLINOPYROXENE-OLIVINE",
                                "BASALT, KOMATIITIC", "BASALT, THOLEIITIC, TRANSITIONAL", "BASALT, OLIVINE-CLINOPYROXENE", "BASALT, PICRITIC, FERRO",
                                "BASALT, BRONZITE-AUGITE-OLIVINE", "BASALT, BRONZITE-AUGITE-PIGEONITE", "BASALT, HORNBLENDE", "BASALT, DOLERITIC", 
                                "BASALT, PYROXENE", "BASALT, ANKARAMITIC", "BASALT, OLIVINE-CLINOPYROXENE-PLAGIOCLASE", "BASALT, PLAGIOCLASE-OLIVINE-CLINOPYROXENE",
                                "BASALT, AUGITE", "BASALT, TRACHYTIC", "BASALT, AUGITE-OLIVINE", "BASALT, LEUCITE", "BASALT, OLIVINE-AUGITE", 
                                "BASALT, OLIVINE-BRONZITE-PIGEONITE", "BASALT, PLAGIOCLASE", "BASALT, PYROXENE-AMPHIBOLE", "BASALT, PYROXENE-OLIVINE",
                                "BASALT, SHOSHONITIC", "BASALT, OLIVINE-PLAGIOCLASE", "BASALT, ANORTHITE", "BASALT, KAERSUTITE", "BASALT, OLIGOCLASE",
                                "BASALT, QUARTZ"))

#---- Correct TectonicSetting Names ----
GEOROC_CPX_Stoich$TectonicSetting <- ifelse(GEOROC_CPX_Stoich$TectonicSetting == "ARCHEAN CRATON (INCLUDING GREENSTONE BELTS)", "ARCHEAN CRATON", GEOROC_CPX_Stoich$TectonicSetting)

#---- Reorder columns ----
GEOROC_CPX_Stoich <- GEOROC_CPX_Stoich %>%
  select("ReferenceNumber", "SampleName", "TectonicSetting", "Location", "Latitude",  "Longitude", "TAS_Series", "RockNameMaster",
         "RockNameWorking", "RockNameFinal", "RockTypeFinal", "Alteration", "Mineral", "Spot", "Crystal", "PrimarySecondary",
         "MaficityGroup", "SiO2", "TiO2", "Al2O3", "Cr2O3", "MgO", "FeOt", "NiO", "MnO", "CaO", "Na2O", "K2O", "BaO", "P2O5", "SrO",
         "ZrO2", "ZnO", "Total", "MgNum", "Evaluation", "Mg", "Fe2_a", "Fe2_b", "Mn", "Na", "En", "Fs", "Wo",
         "Group", "QJ", "TotalCations", "Fe2Sum", "Fe2MnSum")

#---- Filter data ----
GEOROC_CPX_Stoich_Filtered <- GEOROC_CPX_Stoich %>%
  filter(QJ < 2.1, Total > 96, TotalCations > 3.99 & TotalCations < 4.01,
         RockTypeFinal %in% c("Volcanic", "Subvolcanic", "Plutonic"))

########## 3. Export data files ##########
## Final data file
write_csv(GEOROC_CPX_Stoich_Filtered, "/Users/jackward/Documents/01_Projects/11_Brenainn/01_Data/04_Final/Brenainn_GEOROC_Final.csv")

## Metadata file
Metadata <- GEOROC_CPX_Stoich_Filtered %>%
  select(ReferenceNumber, SampleName, TectonicSetting, Location, Latitude, Longitude, RockNameMaster, RockNameWorking, RockNameFinal, RockTypeFinal) %>%
  dplyr::arrange(TectonicSetting, RockNameFinal)

write_csv(Metadata, "/Users/jackward/Documents/01_Projects/11_Brenainn/01_Data/04_Final/Brenainn_GEOROC_Final_Metadata.csv")

########## 4. Make figures ##########
#---- Supplementary rock name figures ----
SupplementaryRockName <- GEOROC_CPX_Stoich_Filtered %>%
  ggtern(aes(Mg, Na, Fe2MnSum, colour = RockTypeFinal)) +
  geom_point(shape = 16, size = 1, alpha = 0.3) +
  scale_colour_manual(values = c("#cc00ffff", "#5555ffd4", "#55ddffff")) +
  labs(x  = "Mg", y  = "Na", z  = expression("Mn+Fe"^"2+")) +
  theme_bw(base_size = 7) +
  theme(legend.position = "none") +
  facet_wrap(vars(RockNameFinal))

ggsave("/Users/jackward/Documents/01_Projects/11_Brenainn/03_Figures/01_Supplementary_Figures/Supplementary_Rock_Name.pdf", plot = SupplementaryRockName, width = 190, height = 190, units = "mm")



#---- Ternary - MaficityGroup ----
## MaficityGroup vs. RockTypeFinal
MaficityGroup_VolcPlut_GD <- GEOROC_CPX_Stoich_Filtered %>%
  ggtern(aes(Mg, Na, Fe2MnSum, colour = RockTypeFinal)) +
  geom_point(shape = 16, size = 1, alpha = 0.3) +
  scale_colour_manual(values = c("#cc00ffff", "#5555ffd4", "#55ddffff")) +
  theme_bw(base_size = 7) +
  facet_wrap(vars(MaficityGroup)) +
  ggtitle("Maficity group - volcanic, subvolcanic, plutonic")

ggsave("/Users/jackward/Documents/01_Projects/11_Brenainn/03_Figures/01A_MaficityGroup_VolcPlut_GD.pdf", plot = MaficityGroup_VolcPlut_GD, width = 190, height = 190, units = "mm")

## MaficityGroup vs. TectonicSetting
MaficityGroup_TectonicSetting <- GEOROC_CPX_Stoich_Filtered %>%
  filter(TectonicSetting %in% c("CONTINENTAL FLOOD BASALT", "CONVERGENT MARGIN", "INTRAPLATE VOLCANICS",
                                "OCEAN ISLAND", "RIFT VOLCANICS")) %>%
  ggtern(aes(Mg, Na, Fe2MnSum, colour = TectonicSetting)) +
  geom_point(shape = 16, size = 1, alpha = 0.5) +
  scale_colour_npg() +
  theme_bw(base_size = 7) +
  facet_grid(RockTypeFinal ~ MaficityGroup, switch = "y") +
  ggtitle("Maficity group - tectonic setting") +
  theme(legend.position = "bottom")

ggsave("/Users/jackward/Documents/01_Projects/11_Brenainn/03_Figures/01B_MaficityGroup_TectonicSetting.pdf", plot = MaficityGroup_TectonicSetting, width = 190, height = 190, units = "mm")

#---- Ternary - RockNameFinal ----
## All rock types
RockName_VolcPlut_GD <- GEOROC_CPX_Stoich_Filtered %>%
  ggtern(aes(Mg, Na, Fe2MnSum, colour = RockTypeFinal)) +
  geom_point(shape = 16, size = 1, alpha = 0.3) +
  scale_colour_manual(values = c("#cc00ffff", "#5555ffd4", "#55ddffff")) +
  theme_bw(base_size = 7) +
  facet_wrap(vars(RockNameFinal)) +
  ggtitle("Reclassified rock name - volcanic, subvolcanic, plutonic")

ggsave("/Users/jackward/Documents/01_Projects/11_Brenainn/03_Figures/02A_RockName_GD.pdf", plot = RockName_VolcPlut_GD, width = 190, height = 190, units = "mm")

## Volcanic
RockName_Volc_GD <- GEOROC_CPX_Stoich_Filtered %>%
  filter(RockTypeFinal == "Volcanic") %>%
  ggtern(aes(Mg, Na, Fe2MnSum)) +
  geom_point(shape = 16, size = 1, alpha = 0.3, colour = "#55ddffff") +
  theme_bw(base_size = 7) +
  facet_wrap(vars(RockNameFinal)) +
  ggtitle("Reclassified rock name - volcanic")

ggsave("/Users/jackward/Documents/01_Projects/11_Brenainn/03_Figures/02B_RockName_Volcanic_GD.pdf", plot = RockName_Volc_GD, width = 190, height = 190, units = "mm")

## Plutonic
RockName_Plut_GD <- GEOROC_CPX_Stoich_Filtered %>%
  filter(RockTypeFinal == "Plutonic") %>%
  ggtern(aes(Mg, Na, Fe2MnSum)) +
  geom_point(shape = 16, size = 1, alpha = 0.3, colour = "#cc00ffff") +
  theme_bw(base_size = 7) +
  facet_wrap(vars(RockNameFinal)) +
  ggtitle("Reclassified rock name - plutonic")

ggsave("/Users/jackward/Documents/01_Projects/11_Brenainn/03_Figures/02C_RockName_Plutonic_GD.pdf", plot = RockName_Plut_GD, width = 190, height = 190, units = "mm")

## Subvolcanic
RockName_Hypa_GD <- GEOROC_CPX_Stoich_Filtered %>%
  filter(RockTypeFinal == "Subvolcanic") %>%
  ggtern(aes(Mg, Na, Fe2MnSum)) +
  geom_point(shape = 16, size = 1, alpha = 0.3, colour = "#5555ffd4") +
  theme_bw(base_size = 7) +
  facet_wrap(vars(RockNameFinal)) +
  ggtitle("Reclassified rock name - subvolcanic")

ggsave("/Users/jackward/Documents/01_Projects/11_Brenainn/03_Figures/02D_RockName_Hypa_GD.pdf", plot = RockName_Hypa_GD, width = 190, height = 190, units = "mm")

#---- Ternary - TectonicSetting ----
## All rock types
TecSet_VolcPlut_GD <- GEOROC_CPX_Stoich_Filtered %>%
  ggtern(aes(Mg, Na, Fe2MnSum, colour = RockTypeFinal)) +
  geom_point(shape = 16, size = 1, alpha = 0.3) +
  scale_colour_manual(values = c("#cc00ffff", "#5555ffd4", "#55ddffff")) +
  theme_bw(base_size = 7) +
  facet_wrap(vars(TectonicSetting)) +
  ggtitle("Tectonic setting - volcanic, subvolcanic, plutonic")

ggsave("/Users/jackward/Documents/01_Projects/11_Brenainn/03_Figures/03A_TecSet_GD.pdf", plot = TecSet_VolcPlut_GD, width = 190, height = 130, units = "mm")

## Volcanic
TecSet_Volc_GD <- GEOROC_CPX_Stoich_Filtered %>%
  filter(RockTypeFinal == "Volcanic") %>%
  ggtern(aes(Mg, Na, Fe2MnSum)) +
  geom_point(shape = 16, size = 1, alpha = 0.3, colour = "#55ddffff") +
  theme_bw(base_size = 7) +
  facet_wrap(vars(TectonicSetting)) +
  ggtitle("Tectonic setting - volcanic rocks")

ggsave("/Users/jackward/Documents/01_Projects/11_Brenainn/03_Figures/03B_TecSet_Volcanic_GD.pdf", plot = TecSet_Volc_GD, width = 190, height = 130, units = "mm")

## Plutonic
TecSet_Plut_GD <- GEOROC_CPX_Stoich_Filtered %>%
  filter(RockTypeFinal == "Plutonic") %>%
  ggtern(aes(Mg, Na, Fe2MnSum)) +
  geom_point(shape = 16, size = 1, alpha = 0.3, colour = "#cc00ffff") +
  theme_bw(base_size = 7) +
  facet_wrap(vars(TectonicSetting)) +
  ggtitle("Tectonic setting - plutonic rocks")

ggsave("/Users/jackward/Documents/01_Projects/11_Brenainn/03_Figures/03C_TecSet_Plutonic_GD.pdf", plot = TecSet_Plut_GD, width = 190, height = 130, units = "mm")

## Subvolcanic
TecSet_Hypa_GD <- GEOROC_CPX_Stoich_Filtered %>%
  filter(RockTypeFinal == "Subvolcanic") %>%
  ggtern(aes(Mg, Na, Fe2MnSum)) +
  geom_point(shape = 16, size = 1, alpha = 0.3, colour = "#5555ffd4") +
  theme_bw(base_size = 7) +
  facet_wrap(vars(TectonicSetting)) +
  ggtitle("Tectonic setting - subvolcanic rocks")

ggsave("/Users/jackward/Documents/01_Projects/11_Brenainn/03_Figures/03D_TecSet_Hypa_GD.pdf", plot = TecSet_Hypa_GD, width = 190, height = 130, units = "mm")

#---- Ternary - TAS_Series ----
## All rock types
TAS_Series_GD <- GEOROC_CPX_Stoich_Filtered %>%
  ggtern(aes(Mg, Na, Fe2MnSum, colour = RockTypeFinal)) +
  geom_point(shape = 16, size = 1, alpha = 0.3) +
  scale_colour_manual(values = c("#cc00ffff", "#5555ffd4", "#55ddffff")) +
  theme_bw(base_size = 7) +
  facet_wrap(vars(TAS_Series)) +
  ggtitle("TAS series - volcanic, subvolcanic, plutonic")

ggsave("/Users/jackward/Documents/01_Projects/11_Brenainn/03_Figures/04_TAS_Series_GD.pdf", plot = TAS_Series_GD, width = 190, height = 130, units = "mm")













