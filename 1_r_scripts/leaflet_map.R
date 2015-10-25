## LEAFLET MAP EXPERIMENT

# PROJECT SETTINGS --------------------------------------------------------------------------------

options(scipen=999)

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
library(downloader)
library(tmap)
library(rgeos)

# GET SPATIAL DATA --------------------------------------------------------------------------------

PRSC_counties <- c("King", "Snohomish", "Kitsap", "Pierce") # create a vector of the counties

for (i in seq_along(PRSC_counties)){                                        # look up the county codes
        codes <- lookup_code(state = "Washington",county = PRSC_counties[i])
        print(codes)
}

counties <- c(33,61,35,53) # create a list of the county codes
names(counties) <- c("King", "Snohomish", "Kitsap", "Pierce") # name the codes, just so I don't forget

tracts_orig <- tracts(state = "WA",
                      county = counties,
                      cb = TRUE)

# REMOVE WATERBODIES FROM TRACTS ------------------------------------------------------------------

crs_geog <- tracts_orig@proj4string # save the CRS of the tracts data (type: geographic coordinate system)
crs_proj <- "+init=epsg:3690" # save a projected coodinate system CRS 

url <- "ftp://www.ecy.wa.gov/gis_a/inlandWaters/NHD/NHDmajor.gdb.zip" # save the URL for the waterbodies data

temp <- tempfile() # create a temporary file to hold the compressed download

download(url, dest = temp, mode="wb") # download the file

unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder

path_gdb <- "./2_inputs/NHDMajor.gdb/" # path to the geodatabase folder

fc_list = ogrListLayers(path_gdb) # check the geodatabase contents
print(fc_list)

waterbodies.shp <- readOGR(dsn = path_gdb,      # create a waterbodies shape
                           layer = "NHD_MajorWaterbodies")

waterbodies.shp <- gBuffer(waterbodies.shp, byid=TRUE, width=0) # clean up self-intersecting polygons

waterbodies.shp <- spTransform(waterbodies.shp,CRSobj = crs_proj) # transform the projection to match the project projection

tracts_orig <- spTransform(tracts_orig,CRSobj = crs_proj) # change the CRS from geographic to projected

tracts_big <- gUnaryUnion(tracts_orig)

waterbodies_cntr <- gCentroid(spgeom = waterbodies.shp,byid = TRUE) #

sel <- over(x = waterbodies_cntr,y = tracts_big,returnList = TRUE) #

intersect <- which(sel == 1) # identify the overlapping waterbodies

waterbodies_sel.shp <- waterbodies.shp[intersect,] # refine the subset of the spatial data

waterbodies_sel.shp <- spTransform(waterbodies_sel.shp,CRSobj = crs_proj) # change the CRS from geographic to projected

waterbodies_sel.shp <- gUnaryUnion(waterbodies_sel.shp)

tracts <- gDifference(spgeom1 = tracts_orig, spgeom2 = waterbodies_sel.shp, byid = TRUE,drop_lower_td = TRUE)  # Remove the waterbodies from the tract shapes

tracts <- spTransform(tracts,CRSobj = crs_geog)

df <- tracts_orig@data

rn <- rownames(df)

tracts <- spChFIDs(obj = tracts,x = rn) # change the row IDs to match those in 'tracts_orig'

tracts <- SpatialPolygonsDataFrame(Sr = tracts,data = df)

tracts <- spTransform(tracts,CRSobj = crs_geog)


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

popup <- paste0("GEOID: ", income_merged$GEOID, "<br>", "Percent of Households above $200k: ", round(income_merged$percent,2))


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
