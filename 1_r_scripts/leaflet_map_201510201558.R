## LEAFLET MAP EXPERIMENT

# LOAD PACKAGES ----------------------------------------------------------------------------------------

library(rgdal)    # for readOGR and others
library(sp)       # for spatial objects
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames
library(ggplot2)  # for plotting
library(tigris)
library(acs)
library(stringr) # to pad fips codes
library(purrr)

# GET SPATIAL DATA --------------------------------------------------------------------------------

PRSC_counties <- c("King", "Snohomish", "Kitsap", "Thurston") # create a vector of the counties

for (i in seq_along(PRSC_counties)){                                        # look up the county codes
        codes <- lookup_code(state = "Washington",county = PRSC_counties[i])
        print(codes)
}

### Just adding some test text


# GET THE TABULAR DATA ----------------------------------------------------------------------------




# MERGE -------------------------------------------------------------------------------------------



# CREATE THE LEAFLET MAP --------------------------------------------------------------------------



# SAVE MAP AS AN IMAGE ----------------------------------------------------------------------------
