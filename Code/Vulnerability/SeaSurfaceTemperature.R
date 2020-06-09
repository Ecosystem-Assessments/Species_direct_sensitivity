# source('./Code/Vulnerability/SeaSurfaceTemperature.R')
# We consider vulnerability to temperature anomalies to be the same for positive and negative anomalies
# Traits that we will take into account:
#   - Environment
#   - Mobility

# Load trait data
load('./Data/SpeciesTraits/Environment.RData')
load('./Data/SpeciesTraits/Mobility.RData')

# Vulnerability due to the environment
env <- c(bathydemersal = 0.0,
         bathypelagic  = 0.0,
         benthic       = 0.0,
         benthopelagic = 0.5,
         demersal      = 0.0,
         pelagic       = 1.0)

#
#   pelagic: occurring mainly in the water column between 0 and 200 m, not feeding on benthic organisms;
#   benthopelagic: living and/or feeding on or near the bottom, as well as in midwater, between 0 and 200 m;
#   demersal: living and/or feeding on or near the bottom, between 0 and 200 m;
#   reef-associated: living and/or feeding on or near reefs, between 0 and 200 m;
#   bathydemersal: living and/or feeding on or near the bottom, below 200 m.
#   bathypelagic: occurring mainly in open water below 200 m, not feeding on benthic organisms; and

# Vulnerability due to taxa mobility
mob <- c(sessile  = 1.00,
         crawler  = 0.75,
         swimmer  = 0.75,
         burrower = 0.75,
         mobile   = 0.00)

# Integrate to traits db
for(i in names(env)) environment[, i] <- environment[, i] * env[i]
for(i in names(mob)) mobility[, i] <- mobility[, i] * mob[i]

# For each taxa, select the maximum vulnerability of each trait
vulnerability <- data.frame(env = apply(environment, 1, max),
                            mob = apply(mobility, 1, max))

# Vulnerability to hypoxia
sst <- vulnerability$env * vulnerability$mob

# In matrix
sst <- matrix(data = sst,
              nrow = nrow(vulnerability), ncol = 2,
              dimnames = list(rownames(vulnerability), c('NegativeSST','PositiveSST')))

# Export object as .RData
save(sst, file = './Data/StressorVulnerability/SeaSurfaceTemperature.RData')
