# source('./Code/Vulnerability/Acidification.R')
# Acidification occurs in bottom-waters of the St. Lawrence
# Traits that we will take into account:
#   - Mobility
#   - Body composition
#   - Phylum
#   - Environment

# Load trait data
load('./Data/SpeciesTraits/Mobility.RData')
load('./Data/SpeciesTraits/BodyComposition.RData')
load('./Data/SpeciesTraits/Phylum.RData')
load('./Data/SpeciesTraits/Environment.RData')

# Vulnerability due to the environment
env <- c(bathydemersal = 1.0,
         bathypelagic  = 0.0,
         benthic       = 1.0,
         benthopelagic = 0.5,
         demersal      = 1.0,
         pelagic       = 0.0)

# Vulnerability due to taxa mobility, as a proxy of metabolism (Melzner et al. 2009)
mob <- c(sessile  = 1.00,
         crawler  = 0.75,
         swimmer  = 0.75,
         burrower = 0.75,
         mobile   = 0.25)

# Vulnerability due to body composition
bd <- c(biogenic_silica         = 0.0,
        bone                    = 0.0,
        cartilaginous           = 0.0,
        chitinous               = 0.0,
        non_calcifying          = 0.0,
        soft_aragonite          = 0.0,
        soft_calcite            = 0.0,
        soft_calcium_phosphate  = 0.0,
        soft_calcium_sulfate    = 0.0,
        solid_aragonite         = 0.9,
        solid_calcite           = 0.8,
        solid_calcium_phosphate = 0.8,
        solid_high_magnesium    = 1.0,
        solid_phosphatic        = 0.0)

# Vulnerability using phylum as proxy  (Kroker et al. 2013)
message('WARNING: Porifera are classified very broadly here and are considered tolerant to OA. However, there is likely a big tolerance difference between silicious and calcifying porifera. However, we do not capture it here.')
message('WARNING: Calcifying annelida are sensitive, but we only have non-calcifying taxa in our list.')
message('WARNING: For Cnidaria, it mostly depends on skeleton, so we will identify that phylum has highly sensitive and let the body composition trait decide fully determine vulnerability')
phy <- c(Annelida      = 0.0,
         Arthropoda    = 0.5,
         Brachiopoda   = 0.5,
         Bryozoa       = 1.0,
         Chordata      = 0.0,
         Cnidaria      = 1.0,
         Ctenophora    = 0.0,
         Echinodermata = 1.0,
         Echiura       = 0.0,
         Mollusca      = 1.0,
         Porifera      = 0.0,
         Sipuncula     = 0.0)

# Integrate to traits db
for(i in names(env)) environment[, i] <- environment[, i] * env[i]
for(i in names(mob)) mobility[, i] <- mobility[, i] * mob[i]
for(i in names(bd)) body[, i] <- body[, i] * bd[i]
for(i in names(phy)) phylum[, i] <- phylum[, i] * phy[i]

# For each taxa, select the maximum vulnerability of each trait
vulnerability <- data.frame(env = apply(environment, 1, max),
                            mob = apply(mobility, 1, max),
                            bd = apply(body, 1, max),
                            phy = apply(phylum, 1, max))

# Vulnerability to hypoxia
acid <- vulnerability$env * vulnerability$mob * vulnerability$phy * vulnerability$bd

# In matrix
acidification <- matrix(data = acid,
                  nrow = nrow(vulnerability), ncol = 1,
                  dimnames = list(rownames(vulnerability), c('Acidification')))

# Export object as .RData
save(acidification, file = './Data/StressorVulnerability/Acidification.RData')
