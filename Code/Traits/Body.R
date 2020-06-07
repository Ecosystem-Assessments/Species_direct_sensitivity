library(magrittr)
library(tidyverse)
# Load species
load('./Data/SpeciesList/SpeciesList.RData')
nSp <- nrow(sp)

# =-=-=-=-=-=-=-=-=-=- Species attributes from worms -=-=-=-=-=-=-=-=-=-= #
library(worrms)
wm_attr_aphia

x = wm_records_taxamatch('Gadus morhua')

x <- wm_records_names('Gadus morhua')
as.data.frame(x)

x=wm_attr_aphia(id = 4)
y=wm_attr_aphia(id = 4, offset = 50)


wm_attr_category(id = 7)

x <- wm_records_names(sp$species[c(1,4)]) %>%
     bind_rows() %>%
     as.data.frame()
uid <- x$AphiaID

as.data.frame(wm_record(uid))

x = wm_attr_aphia(uid[1])
wm_attr_aphia(1)
