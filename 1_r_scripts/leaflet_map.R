## LEAFLET MAP EXPERIMENT

# LOAD PACKAGES -----------------------------------------------------------------------------------

library(rgdal)    # for readOGR and others
library(sp)       # for spatial objects
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames
library(ggplot2)  # for plotting
library(tigris)
library(acs)
library(stringr) # to pad fips codes
library(purrr)
library(magrittr)

# GET SPATIAL DATA --------------------------------------------------------------------------------

PRSC_counties <- c("King", "Snohomish", "Kitsap", "Thurston") # create a vector of the counties

for (i in seq_along(PRSC_counties)){                                        # look up the county codes
        codes <- lookup_code(state = "Washington",county = PRSC_counties[i])
        print(codes)
}

counties <- c(33,61,35,67) # create a list of the county codes
names(counties) <- c("King", "Snohomish", "Kitsap", "Thurston") # name the codes, just so I don't forget

tracts <- tracts(state = "WA",
                 county = counties,
                 cb = TRUE)

# GET THE TABULAR DATA ----------------------------------------------------------------------------

geo <- geo.make(state = "WA",county = counties, tract = "*")    # create a geographic set to grab tabular data (acs)

income <- acs.fetch(endyear = 2012, span = 5, geography = geo, # fetch the corresponding census data
                  table.number = "B19001", col.names = "pretty")

income_df <- data.frame(paste0(str_pad(string = income@geography$state, width = 2,side = "left",pad = "0"), # create a column of GEOIDs
                               str_pad(string = income@geography$county, width = 3,side = "left",pad = "0"),
                               str_pad(string = income@geography$tract, width = 6,side = "left",pad = "0")),
                        income@estimate[,c("Household Income: Total:","Household Income: $200,000 or more")], # bind the two income columns 
                        stringsAsFactors = FALSE)

income_df %<>% select(1:3) # trim the dataframe to include GEOIDs and the two income variables

rownames(income_df)<-1:nrow(income_df) # simplify the row names

names(income_df)<-c("GEOID", "total", "over_200") # simplify the column names

income_df %<>% mutate(percent = 100*(over_200/total))

# MERGE -------------------------------------------------------------------------------------------

income_merged <- geo_join(tracts, income_df, "GEOID", "GEOID") # merge the spatial and tabular data by "GEOID"

income_merged <-  income_merged[income_merged$ALAND>0,] # filter out tracts with less than 1 'ALAND'

# CREATE THE LEAFLET MAP --------------------------------------------------------------------------

popup <- paste0("GEOID: ", income_merged$id, "<br>", "Percent of Households above $200k: ", round(income_merged$percent,2))


pal <- colorNumeric(
        palette = "YlGnBu",
        domain = income_merged$percent
)

map <- leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = income_merged, 
                    fillColor = ~pal(percent), 
                    color = "#b2aeae", # you need to use hex colors
                    fillOpacity = 0.5, 
                    weight = 1, 
                    smoothFactor = 0.2,
                    popup = popup) %>%
        addLegend(pal = pal, 
                  values = income_merged$percent, 
                  position = "bottomright", 
                  title = "Percent of Households<br>above $200k",
                  labFormat = labelFormat(suffix = "%")) 
map


# SAVE MAP AS AN IMAGE ----------------------------------------------------------------------------
