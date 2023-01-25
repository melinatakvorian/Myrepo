source("setup.R")

#---- Initial Mapping Basics----
# import Colorado counties with tigris
counties <- counties(state = "CO")


# import roads for Larimer County
roads <- roads(state = "CO", county = "Larimer")

# set tmap mode to interactive
tmap_mode("view")



qtm(counties)+
  qtm(roads)

tm_shape(counties)+
  tm_polygons()+
  tm_shape(roads)+
  tm_lines()


# look at the class of counties
class(counties)

# filter to Poudre Canyon Highway
poudre_hwy <- roads %>% 
  filter(FULLNAME == "Poudre Canyon Hwy")

qtm(poudre_hwy)

# point data
poudre_points <- data.frame(name = c("Mishawaka", "Rustic", "Blue Lake Trailhead"),
                            long = c(-105.35634, -105.58159, -105.85563),
                            lat = c(40.68752, 40.69687, 40.57960))

# convert to spatial
poudre_points_sf <- st_as_sf(poudre_points, coords = c("long", "lat"), crs = 4326)
qtm(poudre_hwy)+
  qtm(poudre_points_sf)

# see the CRS in the header metadata:
counties

#return just the CRS (more detailed)
st_crs(counties)

# check if the objects have the same CRS
st_crs(counties) == st_crs(poudre_points_sf)

# project one object's CRS to another's
poudre_points_prj <- st_transform(poudre_points_sf, st_crs(counties))

# check that the CRS has changed
st_crs(poudre_points_prj) == st_crs(counties)

# raster data
elevation <- get_elev_raster(counties, z = 7)

qtm(elevation)

## making the scale box have useful labels
tm_shape(elevation)+
  tm_raster(style = "cont", title = "Elevation (m)")



# the terra package
## have to convert the elevation object because terra does not take RasterLayer, only spat objects
elevation <- rast(elevation)

## adding a name to the description if you print out the object in the console
names(elevation) <- "Elevation"

# now we want to crop the elevation layer to the boundary of CO
## need to check what the projection is first
st_crs(counties)

## need to check if 2 objects are in the same CRS
crs(counties) == crs(elevation)

## not the same, so need to project the data before cropping
elevation_prj <- terra::project(elevation, counties)


# crop elevation to counties extent
elevation_crop <- crop(elevation, ext(poudre_hwy))

# plotting
qtm(elevation_crop)

# final map with spatial data
tm_shape(elevation, bbox = st_bbox(poudre_hwy))+
  tm_raster(style = "cont", title = "Elevation (m)")+
  tm_shape(poudre_hwy)+
  tm_lines()+
  tm_shape(poudre_points_prj)+
  tm_dots(size = 0.2)

#---- read and write spatial data----

## save sf/vector data locally to the disc
write_sf(counties, "data/counties.shp")

## save raster data locally to the disc
writeRaster(elevation_crop, "data/elevation.tif")

# save RData
## this is useful because it is a smaller file and saves -
## from the environment pane. You can also save more than 1 object
## at once
save(counties, roads, file = "data/spatial_objects.RData")

## removing the two objects (THIS IS PERMANENT)
rm(counties, roads)
## seeing how saving RData allows you to load them back in
load("data/spatial_objects.RData")
## terra objects don't save .RData files well, so you have to do this:
saveRDS(elevation_crop, "data/elevation_crop.RDS")
readRDS("data/elevation_crop.RDS") %>% rast()

#----Read Shapefiles----
read_sf("data/poudre_hwy.shp")
rast("data/elevation_larimer.tif")

#---- 4. Exercises----
# filter out the counties data set to only include Larimer, Denver, and Pueblo
tricounty <- counties %>%
  relocate(NAME) %>% 
  filter(counties$NAME %in% c("Larimer", "Pueblo", "Denver"))

# make a map of the counties data colored by county area
tm_shape(tricounty)+
  tm_polygons(col = 'ALAND', )

# make a map of counties colored by their total area of water
tm_shape(tricounty)+
  tm_polygons(col = 'AWATER', )

# make a barplot comparing the elevation of your 3 points in the Poudre Canyon
elev <- extract(poudre_points[2,2], poudre_points[2,3], fun = NULL, method = "simple")
ggplot(poudre_points)+
  geom_bar(aes(x = elev, fill = name))

# why are there 4 features in our Poudre Canyon Highway variable instead of 1?
# Because it is an 'sf' or simple features and represents spatial features with a geometry column

