# source('./Code/Vulnerability/DirectHumanImpact.R')
# Traits that we will take into account:
#   - Mobility
#   - Size

# Load trait data
load('./Data/SpeciesTraits/Mobility.RData')
load('./Data/SpeciesTraits/Size.RData')

# Vulnerability due to taxa mobility
mob <- c(
  sessile  = 1.00,
  crawler  = 0.75,
  swimmer  = 0.75,
  burrower = 0.75,
  mobile   = 0.50,
  flying   = 0.25
)

# Vulnerability due to size
# For this iteration, we will consider that species vulnerability to coastal
# development increases with maximum size and that all species exposed are
# size < 1m: 0.25
# size >= 1m & < 2m: 0.5
# size >= 2m & < 3m: 0.75
# size >= 3m: 1

sz <- matrix(NA, nrow = 4, ncol = 3,
             dimnames = list(c('C1','C2','C3','C4'),
                             c('Minimum','Maximum','Vulnerability')))

sz[1, ] <- c(0, 100, 0.25) # size >= 1 & < 2: 0.5
sz[2, ] <- c(100, 200, 0.5) # size >= 1 & < 2: 0.5
sz[3, ] <- c(200, 300, 0.75) # size >= 2 & < 3: 0.75
sz[4, ] <- c(300, 50000, 1)# size >= 3: 1 (50000 > blue whale, just a max value)

# Integrate to traits db
for(i in names(mob)) mobility[, i] <- mobility[, i] * mob[i]

for(i in 1:nrow(sz)) {
  dat <- size[,'Size'] >= sz[i,'Minimum'] & size[,'Size'] < sz[i,'Maximum']
  size[dat, 'Size'] <- sz[i,'Vulnerability']
}

# For each taxa, select the maximum vulnerability of each trait
vulnerability <- data.frame(mob = apply(mobility, 1, max),
                            size = apply(size, 1, max))

# Vulnerability to hypoxia
DHI <- vulnerability$mob * vulnerability$size

# In matrix
DHI <- matrix(data = DHI,
              nrow = nrow(vulnerability), ncol = 1,
              dimnames = list(rownames(vulnerability), 'DirectHumanImpact'))

# Export object as .RData
save(DHI, file = './Data/StressorVulnerability/DirectHumanImpact.RData')
