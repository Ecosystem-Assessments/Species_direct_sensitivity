# source('./Code/Fisheries.R')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                     LIBRARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(magrittr)
library(tidyverse)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   IMPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gear <- vroom::vroom("data/fisheries_logbooks-f2109e69/fisheries_logbooks-f2109e69_gear.csv")
species <- vroom::vroom("data/fisheries_logbooks-f2109e69/fisheries_logbooks-f2109e69_species_manual_modifs.csv") |>
  dplyr::select(ESP_STAT, DL_ESP)
logbooks <- vroom::vroom("data/fisheries_logbooks-f2109e69/fisheries_logbooks-f2109e69.csv")
load("Data/SpeciesList/SpeciesList.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 GEAR CLASS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# =~-~=~-~=~-~=~-~=~-~= #
# Classify gear types
# -------------------
# NOTE:
#
# Fishing activities are performed using a variety of gears types, e.g. trap,
# trawl, dredge, driftnet, hand line, longline, scuba diving, purse seine, seine,
# beach seine and jig fishing. Intensity of fishing activities was divided among
# gear types and based on their respective types of environmental impacts.
# Gear classification is done using the classification presented in Halpern
# et al. (2008) and Halpern et al. (2015a) and is broken down into 5 distinct
# classes:
#
#  - demersal destructive (DD),
#  - demersal, non-destructive, low-bycatch (DNL),
#  - demersal, non-destructive, high-bycatch (DNH),
#  - pelagic, low-bycatch (PLB),
#  - pelagic, high-bycatch (PHB),
# =~-~=~-~=~-~=~-~=~-~= #
# Select codes
# Run gear[gear$Codes %in% dd, ] to see which gear class is in which category
l <- list(
  data.frame(gearClass = "FisheriesDD", Codes = c(6, 9, 10, 11, 12, 16, 19, 71, 72, 72, 74, 77, 93)),
  data.frame(
    gearClass = "FisheriesDNH",
    Codes = c(3, 21, 22, 24, 25, 33, 45, 46, 47, 61, 62, 63, 65, 66, 67, 68, 69, 78, 79, 80, 84, 86, 87, 88, 89, 92, 98)
  ),
  data.frame(gearClass = "FisheriesDNL", Codes = c(70, 75, 81, 83, 85, 91, 94, 96)),
  data.frame(
    gearClass = "FisheriesPHB",
    Codes = c(4, 5, 13, 14, 15, 17, 18, 27, 28, 29, 30, 32, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 48, 50, 51, 52, 64)
  ),
  data.frame(gearClass = "FisheriesPLB", Codes = c(31, 53, 54, 55, 56, 58, 59, 60)),
  data.frame(gearClass = NA, Codes = c(0, 1, 2, 57, 73, 76, 90, 95, 99, 99))
)
gear <- dplyr::left_join(gear, dplyr::bind_rows(l), by = "Codes") |>
        dplyr::select(Codes, gearClass)

# -----
logbooks <- dplyr::left_join(logbooks, gear, by = c("engin" = "Codes")) |>
            dplyr::filter(!is.na(gearClass))
gearClass <- sort(unique(logbooks$gearClass))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 FORMAT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat <- dplyr::select(logbooks, prespcap, prespvis, gearClass) |>
       dplyr::filter(prespcap %in% species$ESP_STAT) |>
       mutate(bycatch = ifelse(prespvis == prespcap, FALSE, TRUE)) |>
       rename(species = prespcap) |>
       unique() |>
       arrange(species, gearClass, bycatch) |>
       group_by(species, gearClass) |>
       summarize(bycatch = !as.logical(sum(!bycatch))) |>
       ungroup() |>
       as.data.frame() |>
       dplyr::left_join(species, by = c("species" = "ESP_STAT")) |>
       dplyr::select(-species) |>
       dplyr::rename(species = DL_ESP)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                             Species vulnerability
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('./Data/SpeciesList/SpeciesList.RData')

# Select only species in species list
dat <- dat[dat$species %in% spList$species, ]

# Vulnerability matrix
fisheries <- matrix(
  nrow = nrow(spList), 
  ncol = length(gearClass), 
  data = 0, 
  dimnames = list(c(spList$species), gearClass)
)

# Vulnerability
# 0.75 for bycatch
# 1 for targetted species
dat$vulnerability <- ifelse(dat$bycatch, 0.75, 1)

# Add vulnerability
for(i in 1:nrow(dat)) {
  rid <- dat$species[i] == rownames(fisheries)
  cid <- dat$gearClass[i] == colnames(fisheries)
  fisheries[rid, cid] <- dat$vulnerability[i]
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
