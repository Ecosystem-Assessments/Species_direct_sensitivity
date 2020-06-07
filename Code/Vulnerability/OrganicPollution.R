# source('./Code/Vulnerability/OrganicPollution.R')
# Traits that we will take into account for all pollution types:
#   - Feeding type

# Load trait data
load('./Data/SpeciesTraits/FeedingType.RData')

# Vulnerability due to the feeding type (see Ellis et al. 2017)
feed <- c(deposit     = 0.75,
          filter      = 0.00,
          grazer      = 0.00,
          parasite    = 0.00,
          plankton    = 0.00,
          predator    = 0.00,
          scavenger   = 0.50,
          suspension  = 1.00,
          xylophagous = 0.00)

# Integrate to traits db
for(i in names(feed)) feeding[, i] <- feeding[, i] * feed[i]

# For each taxa, select the maximum vulnerability of each trait
vulnerability <- data.frame(feed = apply(feeding, 1, max))

# In matrix
org <- matrix(data = vulnerability$feed,
                nrow = nrow(vulnerability), ncol = 1,
                dimnames = list(rownames(vulnerability), c('OrganicPollution')))

# Export object as .RData
save(org, file = './Data/StressorVulnerability/OrganicPollution.RData')
