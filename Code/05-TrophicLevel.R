# Load species list
load('./Data/SpeciesList/SpeciesList.RData')

# Load metaweb
load('./Data/Metaweb/metaweb.RData')

# Measure trophic level
library(NetIndices)
TL <- TrophInd(metaweb)

# Remove zooplankton and phytoplankton
TL <- TL[1:(nrow(TL)-2), 'TL']

# Trophic level matrix
TL <- matrix(data = TL, nrow = nrow(sp), ncol = 1, dimnames = list(sp$species, 'TrophicLevel'))

# Export
save(TL, file = './Data/SpeciesTraits/TrophicLevel.RData')
