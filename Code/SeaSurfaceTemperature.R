# source('./Code/SeaSurfaceTemperature.R')
# We consider vulnerability to temperature anomalies to be the same for positive and negative anomalies
# Traits that we will take into account:
#   - Environment
#   - Mobility
#   - Feeding type


# Load trait data
load('./Data/SpeciesTraits/Environment.RData')
load('./Data/SpeciesTraits/Mobility.RData')
load('./Data/SpeciesTraits/FeedingType.RData')


# Vulnerability due to the environment
env <- c(bathydemersal = 0.0,
         bathypelagic  = 0.0,
         benthic       = 0.0,
         benthopelagic = 0.5,
         demersal      = 0.0,
         pelagic       = 1.0,
         coastal       = 0.5,
         terrestrial   = 0.25)

# Vulnerability due to taxa mobility
mob <- c(sessile  = 1.00,
         crawler  = 0.75,
         swimmer  = 0.75,
         burrower = 0.75,
         mobile   = 0.00,
         flying   = 0.00
       )

# Vulnerability due to the feeding type
feed <- c(deposit     = 1.00,
          filter      = 0.00,
          grazer      = 1.00,
          parasite    = 0.00,
          plankton    = 0.50,
          predator    = 0.50,
          scavenger   = 0.50,
          suspension  = 1.00,
          xylophagous = 0.50)

# Integrate to traits db
for(i in names(env)) environment[, i] <- environment[, i] * env[i]
for(i in names(mob)) mobility[, i] <- mobility[, i] * mob[i]
for(i in names(feed)) feeding[, i] <- feeding[, i] * feed[i]


# For each taxa, select the maximum vulnerability of each trait
vulnerability <- data.frame(env = apply(environment, 1, max),
                            mob = apply(mobility, 1, max),
                            feed = apply(feeding, 1, max))


# Vulnerability to hypoxia
sst <- vulnerability$env * vulnerability$mob * vulnerability$feed

# In matrix
sst <- matrix(data = sst,
              nrow = nrow(vulnerability), ncol = 2,
              dimnames = list(rownames(vulnerability), c('NegativeSST','PositiveSST')))

# Export object as .RData
save(sst, file = './Data/StressorVulnerability/SeaSurfaceTemperature.RData')
