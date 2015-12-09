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
library(stringr)
library(shiny)

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

# To keep the app efficient, the code for downloading the waterbodies spatial data and
# clipping the census tracts is **only** run if the updated tracts file is not
# already present in the '2_inputs' folder

if(!file.exists("./2_inputs/tracts_clipped.shp")){ 
        
        if(!file.exists("./2_inputs/NHDMajor.gdb")){  # check if the file already exists, if not then download it
                url <- "ftp://www.ecy.wa.gov/gis_a/inlandWaters/NHD/NHDmajor.gdb.zip" # save the URL for the waterbodies data
                
                temp <- tempfile() # create a temporary file to hold the compressed download
                
                download(url, dest = temp, mode="wb") # download the file
                
                unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
                
                dateDownloaded <- date()
        }
        
        path_gdb <- "./2_inputs/NHDMajor.gdb/" # path to the geodatabase folder
        
        fc_list = ogrListLayers(path_gdb) # check the geodatabase contents
        print(fc_list)
        
        waterbodies.shp <- readOGR(dsn = path_gdb,      # create a waterbodies shape
                                   layer = "NHD_MajorWaterbodies")
        
        waterbodies.shp <- gBuffer(waterbodies.shp, byid=TRUE, width=0) # clean up self-intersecting polygons
        
        waterbodies.shp <- spTransform(waterbodies.shp,CRSobj = crs_proj) # transform the projection to match the project projection
        
        tracts_orig <- spTransform(tracts_orig,CRSobj = crs_proj) # change the CRS from geographic to projected
        
        tracts_big <- gUnaryUnion(tracts_orig) # simplify the tract polygons by merging them into one polygon
        
        waterbodies_cntr <- gCentroid(spgeom = waterbodies.shp,byid = TRUE) # create a set of center points for the waterbodies shapes
        
        intersect <- over(x = waterbodies_cntr,y = tracts_big,returnList = TRUE) %>%  # find the indices of all waterbodies whose center point overlaps the merged tracts shape
                .[which(. == 1)] %>% 
                names()
        
        waterbodies_sel.shp <- waterbodies.shp[intersect,] # refine the subset of the spatial data
        
        waterbodies_sel.shp <- spTransform(waterbodies_sel.shp,CRSobj = crs_proj) # change the CRS from geographic to projected
        
        waterbodies_sel.shp <- gUnaryUnion(waterbodies_sel.shp) # prep for 'gDifference' function: combine polygons into one multi-shape polygon
        
        tracts <- gDifference(spgeom1 = tracts_orig, spgeom2 = waterbodies_sel.shp, byid = TRUE,drop_lower_td = TRUE) %>%  # Remove the waterbodies from the tract shapes
                spTransform(.,CRSobj = crs_geog)  
        
        df <- tracts_orig@data
        
        rn <- rownames(df)
        
        tracts <- spChFIDs(obj = tracts,x = rn) %>% # change the row IDs to match those in 'tracts_orig'
                SpatialPolygonsDataFrame(Sr = .,data = df) %>% 
                spTransform(.,CRSobj = crs_geog)
        
        tracts <- SpatialPolygonsDataFrame(Sr = tracts,data = df)
        
        tracts <- spTransform(tracts,CRSobj = crs_geog)
        
        writeOGR(obj = tracts,
                 dsn = "./2_inputs/",
                 layer = "tracts_clipped",
                 driver = "ESRI Shapefile")
        
}

tracts <- readOGR(dsn = "./2_inputs/",layer = "tracts_clipped")

# GET THE TABULAR DATA ----------------------------------------------------------------------------

# The 'acs' package requires an authentication code to access the American Community Survey data. 
# Each user must get their own personal code prior to running this portion of the script.

if(!file.exists("./2_inputs/income.csv")){
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
        
        readr::write_csv(x = income_df,
                         path = "./2_inputs/income.csv"
        )
}

income_df <- readr::read_csv(file = "./2_inputs/income.csv")

# MERGE -------------------------------------------------------------------------------------------

income_merged <- geo_join(tracts, income_df, "GEOID", "GEOID") # merge the spatial and tabular data by "GEOID"

income_merged <-  income_merged[income_merged$ALAND>0,] # filter out tracts with less than 1 'ALAND'

# CREATE COUNTY SHAPES ----------------------------------------------------------------------------

counties_wa <- tigris::counties(state = "WA", cb = TRUE) # download WA counties shapefiles

counties_psrc <- as.character(counties) %>%          # reformat the counties codes
        str_pad(width = 3,side = "left",pad = "0")

counties.shp <- subset(counties_wa, COUNTYFP %in% counties_psrc)   # filter the counties shapefile to include only PSRC counties

counties.shp %<>% spTransform(CRSobj = crs_proj) %>% 
        gBuffer(byid=TRUE, width=0) %>% spTransform(CRSobj = crs_geog)

counties_cntr.shp <- gCentroid(spgeom = counties.shp,byid = TRUE) %>%     # create a centroid object for county labels 
        SpatialPointsDataFrame(.,data = as.data.frame(counties.shp@data))

# CREATE THE LEAFLET MAP --------------------------------------------------------------------------

popup <- paste0("GEOID: ", income_merged$GEOID, "<br>", "Percent of Households above $200k: ", round(income_merged$percent,2))

pal <- colorNumeric(
        palette = "YlGnBu",
        domain = income_merged$percent
)

strokepattern <- "3, 6" # defines the stroke dash pattern (used for the county boundaries)

map <- leaflet() %>% 
        addProviderTiles("CartoDB.Positron", group = "basemap") %>%
        addPolygons(data = income_merged, 
                    fillColor = ~pal(percent), 
                    color = "#b2aeae", # you need to use hex colors
                    fillOpacity = 1, 
                    weight = 1, 
                    smoothFactor = 0.2,
                    popup = popup,
                    group = "tracts") %>%
        addPolygons(data = counties.shp,
                    fill = FALSE,
                    stroke = TRUE,
                    dashArray = strokepattern,
                    weight = 3,
                    color = "#ADADAD",
                    opacity = 0.75,
                    smoothFactor = 0.2,
                    group = "counties") %>%
        addPopups(data= counties_cntr.shp,
                  popup = ~NAME,
                  options = popupOptions(minWidth = 20, closeOnClick = FALSE, closeButton = FALSE)) %>% 
        addLegend(pal = pal, 
                  values = income_merged$percent, 
                  position = "bottomleft", 
                  title = "Percent of Households<br>above $200k",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1) %>% 
        addLayersControl(overlayGroups = c("tracts","counties"),
                         options = layersControlOptions(collapsed = FALSE))

# SAVE MAP AS AN IMAGE ----------------------------------------------------------------------------

# [not yet written - will be added later]

# SHINY ---------------------------------------------------------------------------------------

ui <- bootstrapPage(div(class="outer",
                        tags$style(type = "text/css", ".outer {position: fixed; top: 0px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                        leafletOutput("map", width = "100%", height = "100%"),
                        absolutePanel(top = 60, right = 10, draggable=TRUE
                        ))
)

server <- function(input, output, session) {
        output$map <- renderLeaflet({
                map
        })
}

shinyApp(ui, server)
