# Gerrymandering in Mexico.
We attempt to quantify gerrymandering in the country extending the procedure used by Bangia et. al. (2017) to a 
multipartisan political system.

## Background
Mexico's National Electoral Institute (INE in Spanish) uses simmulated annealing to draw district lines following an 
openly available model that penalizes the usual measures of populational variance and compactness. We attempt to verify
if this algorithm is robust in the sense that slight variations should not yield very different election results.

## Data
The electoral data is available online from the INE. Geospacial data, however, has to be requested through a governmental
transparency agent. Here we include all raw data as well as commented `R` code used to clean it whenever necessary.
