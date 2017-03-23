library(leaflet)
# Read in points and convert to latlong

darwinpts <- read.csv(file = "darwin2.csv", stringsAsFactors = FALSE)
# should there be periods in the original file? 
darwinpts$lat <- sapply(darwinpts$S, function(x) {
  -1*(as.numeric(unlist(strsplit(x, " "))[1])+
        as.numeric(unlist(strsplit(x, " "))[2])/60 +
        as.numeric(unlist(strsplit(x, " "))[3])/3600)
})  

darwinpts$lon <- sapply(darwinpts$E, function(x) {
  as.numeric(unlist(strsplit(x, " "))[1])+
    as.numeric(unlist(strsplit(x, " "))[2])/60 +
    as.numeric(unlist(strsplit(x, " "))[3])/3600
})

# hurricane track 
library(stringi)
enawo_track <- 
  readLines("http://weather.unisys.com/hurricane/s_indian/2017/ENAWO/track.dat")

enawo_track <- read.table(textConnection(gsub("TROPICAL ", "TROPICAL_", enawo_track[3:length(enawo_track)])), 
                          header=TRUE, stringsAsFactors=FALSE)

# make storm type names prettier
enawo_track$STAT <- stri_trans_totitle(gsub("_", " ", enawo_track$STAT))

# make column names prettier
colnames(enawo_track) <- c("advisory", "lat", "lon", "time", "wind_speed", "pressure", "status")


# leaflet map

leaflet() %>% 
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("Esri.WorldImagery", group = "Esri base") %>%
  addMarkers(data=darwinpts, ~lon, ~lat, group = "villages as markers") %>%
  addCircleMarkers(data=darwinpts, ~lon, ~lat, group = "villages as circles",
                   stroke = FALSE, fillOpacity = 1) %>%
  # addPolylines(data=enawo_track, ~lon, ~lat, color = "yellow", group = "path") %>%
  addTiles(paste0('http://map1{s}.vis.earthdata.nasa.gov/wmts-webmerc/',
                  "VIIRS_SNPP_CorrectedReflectance_TrueColor",
                  '/default/',
                  "2017-03-07",'/',
                  "GoogleMapsCompatible_Level9",'/{z}/{y}/{x}.',"jpeg"),
           attribution = paste(
             '<a href="https://earthdata.nasa.gov/gibs">NASA EOSDIS GIBS</a>'
           ),
           options = list(
             # maxZoom = zoomLvl,
             minZoom = 1,
             tileSize = 256,
             subdomains = "abc",
             noWrap = "true",
             crs = "L.CRS.EPSG3857",    
             continuousWorld = "true",
             # Prevent Leaflet from retrieving non-existent tiles on the borders.
             bounds = list(list(-85.0511287776, -179.999999975),
                           list(85.0511287776, 179.999999975))
           ), group = "VIIRS") %>%
  addTiles(paste0('http://map1.vis.earthdata.nasa.gov/wmts-webmerc/',
                  "AMSR2_Surface_Precipitation_Rate_Day",
                  '/default/',
                  "2017-03-07",'/',
                  "GoogleMapsCompatible_Level6",
                  '/{z}/{y}/{x}.'
                  ,"png"),
           options = list(
             # maxZoom = 9,
             # maxNativeZoom = olzoomLvl[i],
             minZoom = 1,
             tileSize = 256,
             subdomains = "abc",
             noWrap = "true",
             crs = "L.CRS.EPSG3857",    
             continuousWorld = "true",
             opacity = 0.6,
             # Prevent Leaflet from retrieving non-existent tiles on the borders.
             bounds = list(list(-85.0511287776, -179.999999975),
                           list(85.0511287776, 179.999999975))
           ), group = "Surface Precip") %>%
  addTiles(paste0('http://map1.vis.earthdata.nasa.gov/wmts-webmerc/',
                  "AMSR2_Surface_Precipitation_Rate_Night",
                  '/default/',
                  "2017-03-07",'/',
                  "GoogleMapsCompatible_Level6",
                  '/{z}/{y}/{x}.'
                  ,"png"),
           options = list(
             # maxZoom = 9,
             # maxNativeZoom = olzoomLvl[i],
             minZoom = 1,
             tileSize = 256,
             subdomains = "abc",
             noWrap = "true",
             crs = "L.CRS.EPSG3857",    
             continuousWorld = "true",
             opacity = 0.6,
             # Prevent Leaflet from retrieving non-existent tiles on the borders.
             bounds = list(list(-85.0511287776, -179.999999975),
                           list(85.0511287776, 179.999999975))
           ), group = "Surface Precip") %>%
  addTiles(paste0('https://gibs.earthdata.nasa.gov/wmts/epsg3857/best/Coastlines/default/',
                  "GoogleMapsCompatible_Level9",
                  '/{z}/{y}/{x}.'
                  ,"png"),
           options = list(
             # maxZoom = 9,
             # maxNativeZoom = olzoomLvl[i],
             minZoom = 1,
             tileSize = 256,
             subdomains = "abc",
             noWrap = "true",
             crs = "L.CRS.EPSG3857",    
             continuousWorld = "true",
             opacity = 0.6,
             # Prevent Leaflet from retrieving non-existent tiles on the borders.
             bounds = list(list(-85.0511287776, -179.999999975),
                           list(85.0511287776, 179.999999975))
           ), group = "coast") %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "Esri base", "VIIRS"),
    overlayGroups = c("coast", "Surface Precip", "villages as markers", "villages as circles", "path"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(colors = rep("red",10), position='bottomright', values=1:10, title='CHM', labels = 1:10)


