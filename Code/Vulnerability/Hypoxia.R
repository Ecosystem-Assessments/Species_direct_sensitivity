# source('./Code/Vulnerability/Hypoxia.R')
# Hypoxia occurs in bottom-waters of the St. Lawrence
# Traits that we will take into account:
#   - Environment
#   - Mobility

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
         mobile   = 0.00)

# Integrate to traits db
for(i in names(env)) environment[, i] <- environment[, i] * env[i]
for(i in names(mob)) mobility[, i] <- mobility[, i] * mob[i]

# For each taxa, select the maximum vulnerability of each trait
vulnerability <- data.frame(env = apply(environment, 1, max),
                            mob = apply(mobility, 1, max))

# Vulnerability to hypoxia
hyp <- vulnerability$env * vulnerability$mob

# In matrix
hypoxia <- matrix(data = hyp,
                  nrow = nrow(vulnerability), ncol = 1,
                  dimnames = list(rownames(vulnerability), c('Hypoxia')))

# Export object as .RData
save(hypoxia, file = './Data/StressorVulnerability/Hypoxia.RData')
