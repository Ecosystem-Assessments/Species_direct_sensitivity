# Species vulnerability

Evaluation of the vulnerability of species in the St. Lawrence System based on simple trait matching rules.

## Rules

We infer the effects of drivers on species based on matching rules from expert knowledge and the scientific literature:

- Position in the water column – i.e. deep or surface-dwelling species – determines exposure to stressors.
  - Acidification, hypoxia and bottom temperature anomalies are widespread in the deep layers of the St. Lawrence
  - Surface temperature anomalies and shipping are prevalent in the surface layer

- Mobility determines vulnerability to hypoxia and temperature anomalies. Species with low mobility (score = 1) are assumed to be more affected than species with high mobility (score = .5).

- Ocean acidification affects carbonate-secreting organisms (e.g. mollusks and crustaceans; Kroeker et al. 2013);

- Shipping affects large surface-dwelling species such as marine mammals (Christiansen et al. 2013; Lesage et al. 2017);

- Fisheries affects captured species. Species affected are identified with landing data from the Department of Fisheries and Ocean’s Canada (DFO 2016). Score = 1 for targetted species, score = 0.75 for bycatch species.

- For demersal-destructive fisheries, perhaps add something with mobility for species exposed to fisheries. But perhaps bycatch sort of takes this into consideration already...
