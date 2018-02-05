# Gerrymandering in Mexico.
We attempt to quantify gerrymandering in the country extending the procedure
used by Bangia et. al. (2017) to a  multipartisan political system.

## Background
Mexico's National Electoral Institute (INE in Spanish) uses simmulated annealing
to draw district lines following an  openly available model that penalizes the
usual measures of populational variance and compactness. We attempt to verify
if this algorithm is robust in the sense that slight variations should not
yield very different election results.

## Data
The electoral data is available online from the INE. Geospacial data was taken
from the National Institute of Statistics and Geography's (INEGI in Spanish)
[Digital map of Mexico](http://gaia.inegi.org.mx/geoelectoral/viewer.html) and
was afterwards processed in ArcGis to remove gaps between polygons.
Here we include all the raw electoral data but only the clean map.

The districts that will be used for the upcoming election were scraped
from the INE's website using a script included here.
