# source('./Code/Fisheries.R')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                     LIBRARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(magrittr)
library(tidyverse)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   DOWNLOAD DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The data used to characterize comes from DFO and cannot be shared
# For more information read the repo's README.md document.

# Output location for downloaded data
output <- './Data/ZIF-Fisheries-2010-2015/Data/RawData'

# Data will need to be archived to Zenodo with restricted access and downloaded
# using an access token.
# Eventually it would ideally be part of the SLGO web portal

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   IMPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# File name
fileName <- dir(output, pattern = '.zip')

# Unzip kmz file
unzip(zipfile = paste0(output, '/', fileName),
      exdir = output)

# Import data !(could be done in a single line of command)
zif0 <- read.csv(file = paste0(output, '/2010_ZIF_impactGSL.csv'), header = TRUE, sep = ",", dec = '.', stringsAsFactors = F)
zif1 <- read.csv(file = paste0(output, '/2011_ZIF_impactGSL.csv'), header = TRUE, sep = ",", dec = '.', stringsAsFactors = F)
zif2 <- read.csv(file = paste0(output, '/2012_ZIF_impactGSL.csv'), header = TRUE, sep = ",", dec = '.', stringsAsFactors = F)
zif3 <- read.csv(file = paste0(output, '/2013_ZIF_impactGSL.csv'), header = TRUE, sep = ",", dec = '.', stringsAsFactors = F)
zif4 <- read.csv(file = paste0(output, '/2014_ZIF_impactGSL.csv'), header = TRUE, sep = ",", dec = '.', stringsAsFactors = F)
zif5 <- read.csv(file = paste0(output, '/2015_ZIF_impactGSL.csv'), header = TRUE, sep = ",", dec = '.', stringsAsFactors = F)

# Add year column !(could be done in a single line of command)
zif0$year <- 2010
zif1$year <- 2011
zif2$year <- 2012
zif3$year <- 2013
zif4$year <- 2014
zif5$year <- 2015

# Merge dasets
# Check that all column names are the same between datasets
# cNames <- data.frame(colnames(zif0), colnames(zif1), colnames(zif2), colnames(zif3), colnames(zif4), colnames(zif5))
# all(apply(cNames, 1, function(x) length(unique(x)) == 1))
zif <- rbind(zif0, zif1, zif2, zif3, zif4, zif5)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   FORMAT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------ #
# Change names #
# ------------ #
# Gear names
  zif[, 'Engin'] <- gsub('Plong\x8ee', 'Plongee', zif[, 'Engin'])
  zif[, 'Engin'] <- gsub('Senne danoise ou \x8ecossaise', 'Senne Danoise ou Ecossaise', zif[, 'Engin'])
  zif[, 'Engin'] <- gsub('P\x90che \x88 la ligne', 'Peche a la ligne', zif[, 'Engin'])


# Species list & adjusting species names in dataset
# Note that I only adjust names for scientific names
spList <- data.frame(Common = sort(unique(as.character(zif$Sp_capture))),
                     Scientific = sort(unique(as.character(zif$Sp_capture))),
                     stringsAsFactors = F)

spList$Scientific <- gsub("Oursin",'Strongylocentrotus sp.', spList$Scientific)
spList$Scientific <- gsub("Crabe des neiges",'Chionoecetes opilio', spList$Scientific)
spList$Scientific <- gsub("P\x8etoncle d'Islande",'Chlamys islandica', spList$Scientific)
spList$Scientific <- gsub("P\x8etoncle",'Chlamys islandica', spList$Scientific)
spList$Scientific <- gsub("Mactre atlantique",'Spisula solidissima', spList$Scientific)
spList$Scientific <- gsub("Crevette",'Pandalus borealis', spList$Scientific)
spList$Scientific <- gsub("Buccin",'Buccinum sp.', spList$Scientific)
spList$Scientific <- gsub("Fl\x8etan du Groenland",'Reinhardtius hippoglossoides', spList$Scientific)
spList$Scientific <- gsub("Fl\x8etan atlantique",'Hippoglossus hippoglossus', spList$Scientific)
spList$Scientific <- gsub("Morue franche",'Gadus morhua', spList$Scientific)
spList$Scientific <- gsub("Plie rouge",'Pseudopleuronectes americanus', spList$Scientific)
spList$Scientific <- gsub("Hareng",'Clupea harengus', spList$Scientific)
spList$Scientific <- gsub("Mactre de Stimpson",'Mactromeris polynyma', spList$Scientific)
spList$Scientific <- gsub("Plie canadienne",'Hippoglossoides platessoides', spList$Scientific)
spList$Scientific <- gsub("Concombre de mer",'Cucumaria frondosa', spList$Scientific)
spList$Scientific <- gsub("Limande \x88 queue jaune",'Limanda ferruginea', spList$Scientific)
spList$Scientific <- gsub("Turbot de sable",'Scophthalmus aquosus', spList$Scientific)
spList$Scientific <- gsub("S\x8ebaste",'Sebastes sp.', spList$Scientific)
spList$Scientific <- gsub("Merluche blanche",'Urophycis tenuis', spList$Scientific)
spList$Scientific <- gsub("Aiglefin",'Melanogrammus aeglefinus', spList$Scientific)
spList$Scientific <- gsub("Couteau de l'Atlantique",'Ensis leei', spList$Scientific)
spList$Scientific <- gsub("Chaboisseaux",'Cottidae', spList$Scientific)
spList$Scientific <- gsub("Goberge",'Pollachius virens', spList$Scientific)
spList$Scientific <- gsub("Maquereau",'Scomber scombrus', spList$Scientific)
spList$Scientific <- gsub("Homard",'Homarus americanus', spList$Scientific)
spList$Scientific <- gsub("Raie",'Rajidae', spList$Scientific)
spList$Scientific <- gsub("Lompe",'Cyclopterus lumpus', spList$Scientific)
spList$Scientific <- gsub("Plie grise",'Glyptocephalus cynoglossus', spList$Scientific)
spList$Scientific <- gsub("Baudroie",'Lophius americanus', spList$Scientific)
spList$Scientific <- gsub("Gaspereau",'Alosa pseudoharengus', spList$Scientific)
spList$Scientific <- gsub("Capelan",'Mallotus villosus', spList$Scientific)
spList$Scientific <- gsub("Esturgeon",'Acipenser oxyrinchus', spList$Scientific)
spList$Scientific <- gsub("Mara\x94che",'Lamna nasus', spList$Scientific)
spList$Scientific <- gsub("Requins",'Selachii', spList$Scientific)
spList$Scientific <- gsub("Crabe commun",'Cancer irroratus', spList$Scientific)
spList$Scientific <- gsub("Brosme",'Brosme brosme', spList$Scientific)
spList$Scientific <- gsub("Requin bleu",'Prionace glauca', spList$Scientific)
spList$Scientific <- gsub("Loup atlantique",'Anarhichas lupus', spList$Scientific)
spList$Scientific <- gsub("Loup de mer",'Anarhichadidae', spList$Scientific)
spList$Scientific <- gsub("Crabe araign\x8ee",'Hyas araneus', spList$Scientific)
spList$Scientific <- gsub("Poulamon",'Microgadus tomcod', spList$Scientific)
spList$Scientific <- gsub("Thon rouge",'Thunnus thynnus', spList$Scientific)
spList$Scientific <- gsub("Morue de roche",'Gadus ogac', spList$Scientific)
spList$Scientific <- gsub("Clovisse arctique",'Mesodesma arctatum', spList$Scientific)
spList$Scientific <- gsub("Mako \x88 nageoires courtes",'Isurus oxyrinchus', spList$Scientific)
spList$Scientific <- gsub("Gaspareau",'Alosa pseudoharengus', spList$Scientific)
spList$Scientific <- gsub("Loquette d'Am\x8erique",'Zoarces americanus', spList$Scientific)
spList$Scientific <- gsub("Mako \x88 courtes nageoires",'Isurus oxyrinchus', spList$Scientific)
spList$Scientific <- gsub("Anguille",'Anguilla rostrata', spList$Scientific)
spList$Scientific <- gsub("Aiguillat commun",'Squalus acanthias', spList$Scientific)
spList$Scientific <- gsub("Merluche \x8ecureuil",'Urophycis chuss', spList$Scientific)
spList$Scientific <- gsub("Argentine",'Argentina silus', spList$Scientific)
spList$Scientific <- gsub("Thon ventru",'Thunnus obesus', spList$Scientific)
spList$Scientific <- gsub("Germon",'Thunnus alalunga', spList$Scientific)
spList$Scientific <- gsub("Espadon",'Xiphias gladius', spList$Scientific)

spList <- spList[order(spList$Common, decreasing = T), ]

# Adjust in dataset
for(i in 1:nrow(spList)) zif$Sp_capture <-  gsub(spList[i,1], spList[i,2], zif$Sp_capture)
for(i in 1:nrow(spList)) zif$Sp_vise <-  gsub(spList[i,1], spList[i,2], zif$Sp_vise)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 GEAR CLASS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Divide fisheries data by gear class
zif$gearClass <- ""
zif$gearClass[zif$Engin == "Casier"]                     <- 'FisheriesDNH'
zif$gearClass[zif$Engin == "Chalut de fond"]             <- 'FisheriesDD'
zif$gearClass[zif$Engin == "Drague"]                     <- 'FisheriesDD'
zif$gearClass[zif$Engin == "Filet maillant"]             <- 'FisheriesPHB'
zif$gearClass[zif$Engin == "Peche a la ligne"]           <- 'FisheriesPLB'
zif$gearClass[zif$Engin == "Palangre"]                   <- 'FisheriesPHB'
zif$gearClass[zif$Engin == "Plongee"]                    <- 'FisheriesDNL'
zif$gearClass[zif$Engin == "Senne bourse"]               <- 'FisheriesPLB'
zif$gearClass[zif$Engin == "Senne Danoise ou Ecossaise"] <- 'FisheriesDNH'
zif$gearClass[zif$Engin == "Senne de rivage"]            <- 'FisheriesDNH'
zif$gearClass[zif$Engin == "Trappe"]                     <- 'FisheriesDNH'
zif$gearClass[zif$Engin == "Turlutte"]                   <- 'FisheriesPLB'
gearClass <- sort(unique(zif$gearClass))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 FORMAT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
zif <- zif %>%
       mutate(bycatch = ifelse(Sp_vise == Sp_capture, FALSE, TRUE)) %>%
       rename(species = Sp_capture) %>%
       select(-Date, -Sp_vise, -Pds_capture_kg, -Longitude, -Latitude, -year, -Engin) %>%
       unique() %>%
       arrange(species, gearClass, bycatch) %>%
       group_by(species, gearClass) %>%
       summarize(bycatch = !as.logical(sum(!bycatch))) %>%
       ungroup() %>%
       as.data.frame()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                             Species vulnerability
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('./Data/SpeciesList/SpeciesList.RData')

# Select only species in species list
zif <- zif[zif$species %in% sp$species, ]

# Vulnerability matrix
fisheries <- matrix(nrow = nrow(sp), ncol = length(gearClass), data = 0, dimnames = list(c(sp$species), gearClass))

# Vulnerability
# 0.75 for bycatch
# 1 for targetted species
zif$vulnerability <- ifelse(zif$bycatch, 0.75, 1)

# Add vulnerability
for(i in 1:nrow(zif)) {
  rid <- zif$species[i] == rownames(fisheries)
  cid <- zif$gearClass[i] == colnames(fisheries)
  fisheries[rid, cid] <- zif$vulnerability[i]
}

# Vulnerability for demersal destructive fisheries, a bit different.
# 0.25 for all taxa not targetted or bycatch
# Then consider mobility and environment
uid0 <- fisheries[, 'FisheriesDD'] == 0
dd <- fisheries[, 'FisheriesDD']
dd[uid0] <- 0.25

# Load trait data
load('./Data/SpeciesTraits/Environment.RData')
load('./Data/SpeciesTraits/Mobility.RData')

# Vulnerability due to the environment
env <- c(bathydemersal = 1.0,
         bathypelagic  = 0.0,
         benthic       = 1.0,
         benthopelagic = 0.5,
         demersal      = 1.0,
         pelagic       = 0.0)

# Vulnerability due to taxa mobility
mob <- c(sessile  = 1.00,
         crawler  = 0.75,
         swimmer  = 0.75,
         burrower = 0.75,
         mobile   = 0.25)

# Integrate to traits db
for(i in names(env)) environment[, i] <- environment[, i] * env[i]
for(i in names(mob)) mobility[, i] <- mobility[, i] * mob[i]

# For each taxa, select the maximum vulnerability of each trait
vulnerability <- data.frame(env = apply(environment, 1, max),
                            mob = apply(mobility, 1, max))

# Vulnerability
vulnerability <- vulnerability$env * vulnerability$mob

# Add to demersal destructive data
dd[uid0] <- vulnerability[uid0]

# Add to dataset
fisheries[, 'FisheriesDD'] <- dd

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  EXPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export object as .RData
save(fisheries, file = './Data/StressorVulnerability/Fisheries.RData')
