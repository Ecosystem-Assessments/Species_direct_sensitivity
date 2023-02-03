# source('./Code/Vulnerability/Shipping.R')
# Traits that we will take into account for shipping:
#   - Environment: only species that live at or close to the surface affected
#   - Size: bigger species more affected

# Load trait data
load('./Data/SpeciesTraits/Environment.RData')
load('./Data/SpeciesTraits/Size.RData')

# Vulnerability due to the environment
env <- c(bathydemersal = 0.0,
         bathypelagic  = 0.0,
         benthic       = 0.0,
         benthopelagic = 0.5,
         demersal      = 0.0,
         pelagic       = 1.0,
         coastal       = 0.5,
         terrestrial   = 0.25)

# Vulnerability due to size
# For this iteration, we will consider that species vulnerability to marine
# traffic increases with maximum size and that only species that can reach
# 1 meter in length are affected by marine traffic.
# size < 1m: 0
# size >= 1m & < 2m: 0.5
# size >= 2m & < 3m: 0.75
# size >= 3m: 1

sz <- matrix(NA, nrow = 4, ncol = 3,
             dimnames = list(c('C1','C2','C3','C4'),
                             c('Minimum','Maximum','Vulnerability')))

sz[1, ] <- c(0, 100, 0) # size >= 1 & < 2: 0.5
sz[2, ] <- c(100, 200, 0.5) # size >= 1 & < 2: 0.5
sz[3, ] <- c(200, 300, 0.75) # size >= 2 & < 3: 0.75
sz[4, ] <- c(300, 50000, 1)# size >= 3: 1 (50000 > blue whale, just a max value)

# Integrate to traits db
for(i in names(env)) environment[, i] <- environment[, i] * env[i]

for(i in 1:nrow(sz)) {
  dat <- size[,'Size'] >= sz[i,'Minimum'] & size[,'Size'] < sz[i,'Maximum']
  size[dat, 'Size'] <- sz[i,'Vulnerability']
}

# For each taxa, select the maximum vulnerability of each trait
vulnerability <- data.frame(env = apply(environment, 1, max),
                            size = apply(size, 1, max))

# Vulnerability to hypoxia
shipping <- vulnerability$env * vulnerability$size

# In matrix
shipping <- matrix(data = shipping,
              nrow = nrow(vulnerability), ncol = 1,
              dimnames = list(rownames(vulnerability), 'Shipping'))

# Export object as .RData
save(shipping, file = './Data/StressorVulnerability/Shipping.RData')
