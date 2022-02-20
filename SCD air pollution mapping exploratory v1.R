getwd()
setwd("C:/Users/pegeorg/OneDrive - Emory University/Desktop/r-code/")

library(raster)
library(rgdal)
library(sf)
library(maps)
library(usmap)
library(mapdata)
library(devtools)
library(ggplot2)
library(dplyr)
library(exactextractr)
library(classInt)
library(leaflet)
library(tigris)
library(ggmap)
library(stringr)
library(plotly)


## Importing Raster data to R - starting with a few test cases 
     # note, for below, must download geotiff files from CEDAC website 
GDALinfo("C:/Users/pegeorg/OneDrive - Emory University/Desktop/r-code/GeoTiff/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-201610-geotiff/20161001.tif")
Oct012016 <- raster(x = "C:/Users/pegeorg/OneDrive - Emory University/Desktop/r-code/GeoTiff/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-201610-geotiff/20161001.tif")
Oct082016 <- raster(x = "C:/Users/pegeorg/OneDrive - Emory University/Desktop/r-code/GeoTiff/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-201610-geotiff/20161008.tif")
Oct312016 <- raster(x = "C:/Users/pegeorg/OneDrive - Emory University/Desktop/r-code/GeoTiff/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016-201610-geotiff/20161031.tif")
annual2016 <- raster(x = "C:/Users/pegeorg/OneDrive - Emory University/Desktop/r-code/GeoTiff/Annual-geotiff/2016.tif")
annual2010 <- raster(x = "C:/Users/pegeorg/OneDrive - Emory University/Desktop/r-code/GeoTiff/Annual-geotiff/2010.tif")

#Different ways to look at the coordinate systems of our data                     
Oct012016@extent
Oct012016@crs
nlayers(Oct012016)
st_crs(Oct012016)$proj4string
                     
# Plotting raster data using the base plot command 
plot(Oct012016)
plot(Oct082016)
plot(Oct312016)
plot(annual2010)

                     
# Here is using two separate dates to find change in PM2.5s over time frame, in this case from Oct 1 2016 to Oct 8 2016
Change_Oct012016_Oct082016 <- Oct012016 - Oct082016 
plot(Change_Oct012016_Oct082016)
plot(Change_Oct012016_Oct082016, 
     breaks = c(-10,-5,0,5,10,15, 20),
     col = c("green", "blue", "white", "yellow", "orange", "red", "darkred"))

# Below we look at a histogram                      
hist(Change_Oct012016_Oct082016,
     main = "Distribution of Cells",
     xlab = "Change of PM2.5", ylab = "Number of Pixels")


#Create county outlines    https://rdrr.io/cran/tigris/man/counties.html
ga_counties <- counties(state = 'GA')
plot(ga_counties$geometry)
ga_counties <- ga_counties$geometry
plot(ga_counties)

#review projection and plot, and note that they are different projection systems 
st_crs(Oct012016)$proj4string
st_crs(ga_counties)$proj4string

#change projection systems
crs(ga_counties)   
ga_counties_NAD83 <- st_transform(ga_counties, crs(Oct012016))
#ga_outline_NAD83 <- spTransform(ga_outline, crs(Oct012016))   #note the difference bw sp and st transform here (unsure why the difference, but it works)

ga_outline_1 <- plot_usmap(include = "GA")   #but this is not a shapefile 
#crs(ga_outline_1)

plot(Oct012016)
plot(ga_counties_NAD83, add=TRUE)
# plot(ga_outline_NAD83, add=TRUE) - having trouble with ga_outline 

plot(annual2016)
plot(annual2010)
plot(ga_counties_NAD83, add=TRUE)
plot(ga_outline_NAD83, add=TRUE)
                     
#will try to crop dataset using ga_counties_NAD83, stuck at this part
r2 <- crop(annual2010, extent(ga_counties_NAD83))


#change to dataframe for plotting in ggplot
Oct012016_pts <- rasterToPoints(Oct012016, spatial = TRUE)
Oct012016_df <- data.frame(Oct012016_pts)

annual2010_pts <- rasterToPoints(annual2010, spatial = TRUE)
annual2010_df <- data.frame(annual2010_pts)


#Looking at dataframe
glimpse(Oct012016_df)
head(Oct012016_df)
nrow(Oct012016_df)
summary(Oct012016_df)
sum(is.na(Oct012016_df$X20161001))

colnames(annual2010_df)
annual2010_df <- rename(annual2010_df, PM2.5 = X2010)
colnames(annual2010_df)

ggplot(data = annual2010_df) +
  geom_raster(aes(x=x, y=y, fill = PM2.5)) +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_gradient(low = "lightblue", high = "red4", na.value = NA)


p

ggplot() +
  geom_raster(data = Oct012016_df, aes(x=x, y=y, fill = Oct012016)) +
  theme_void()




                
ee <- exact_extract(annual2016,ga_outline_NAD83)
plot(ee)



plot(myshapefile)

crs(myshapefile)
crs(Oct012016)





#Create shape outlines
#ga_outline <- c("Georgia")
#ga_outline = us[match(toupper(Georgia_outline),toupper(us$NAME_1)),]
#plot(ga_outline)
#crs(ga_outline)





        
#Mapping, using usmaps package (which works with ggplot2)
Georgia <- 
                       
#Plot US map
                       plot_usmap(regions = "states") + 
                       labs(title = "U.S. States",
                            subtitle = "This is a blank map of the United States.") + 
                       theme(panel.background=element_blank())
                     
                     plot_usmap(regions = "counties") + 
                       labs(title = "U.S. counties",
                            subtitle = "This is a blank map of the United States.") + 
                       theme(panel.background=element_blank())
                     
                     plot_usmap(include = .south_region, exclude = c("VA"), labels = TRUE)
                     
                     plot_usmap(include = .south_atlantic, exclude = c("VA"), labels = TRUE)
                     
                     plot_usmap(include = c("GA", "AL", "FL", "SC", "TN", "NC")) +
                       labs(title = "Southwest") +
                       theme(panel.background = element_rect(color = "blue"))
                     
                     plot_usmap(data = countypov, values = "pct_pov_2014", include = c("GA"), color = "blue") + 
                       scale_fill_continuous(low = "white", high = "blue", name = "Poverty Percentage Estimates", label = scales::comma) + 
                       labs(title = "New England Region", subtitle = "Poverty Percentage Estimates for Georgia in 2014") +
                       theme(legend.position = "right")
                     
                     
                     #
                     df <- exact_extract()
                     
                     
                     
                     ##Old way of mapping 
                     map('state')
                     # add a title to your map
                     title('Map of the United States')
                     
                     Georgia <- map('county', regions = "Georgia")
                     
                     map('state', regions = "Georgia", col = "black", add = TRUE)
                     points(x = -84.386, y = 33.754, pch = 21, col = "violetred4", cex = 2)
                     points(x = -84.386, y = 33.754, pch = 8, col = "blue", cex = 1.3)
                     # add a title to your map
                     title('County Map of Georgia')
                     
                     plot(Oct012016, 
                          add = TRUE)
                     
                     
                     usa <- map_data("usa")
                     ggplot() +
                       geom_polygon(data = usa, aes(x = long, y = lat, group = group)) +
                       coord_fixed(1.3)
                     
                     
                     
                     
                     sts <- c("GA", "FL", "AL") 
                     combined <- rbind_tigris(
                       lapply(sts, function(x) {
                         tracts(x, cb = TRUE)
                       })
                     )
                     plot(combined)
                     
                     dfw <- tracts(state = 'TX', county = c('Dallas', 'Tarrant'))
                     plot(dfw)
                     
                     
                     
                     # Used this website for guidance  https://www.earthdatascience.org/courses/earth-analytics/lidar-raster-data-r/classify-raster/
                     
                     USGrid <- readRDS("USGridsite.rds")
                     
#state_boundary_US <- st_read("cb_2018_us_state_5m/cb_2018_us_state_5m.shp")
#ggplot() + 
#  geom_sf(data = state_boundary_US) + 
#  coord_sf()
                     