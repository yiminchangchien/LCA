library(dplyr)
library(raster)
library(sf)
library(readr)

##### 1. collect data
###### 1.1. download the visual and sensory aspect polygons for Wales
temp <- tempfile()
temp2 <- tempfile()
download.file("http://lle.gov.wales/catalogue/item/LandmapVisualSensory.zip", temp)
unzip(zipfile = temp, exdir = temp2)
"NRW_LandMap_Visual_SensoryPolygon.shp" %>%
  file.path(temp2, .) %>% 
  st_read(stringsAsFactors = FALSE) ->
  LCA
rm(list = c("temp", "temp2"))


###### 1.2. download Scenic-Or-Not dataset
bng <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 
        +ellps=airy +datum=OSGB36 +units=m +no_defs"
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

sc <- 
  read_tsv("http://scenicornot.datasciencelab.co.uk/votes.tsv",
           col_types = cols("ID" = col_number(),
                            "Lat" = col_double(),
                            "Lon" = col_double(),
                            "Average" = col_double(),
                            "Variance" = col_double(),
                            "Votes" = col_character(),
                            "Geograph URI" = col_character())) %>%
  as.data.frame %>%
  st_as_sf(coords=c("Lon","Lat"), crs=4326) %>%
  st_transform(crs=27700) %>%
  as("Spatial")

###### 1.3. load in the wildness components
setwd("/Users/Yi-Min/Rsession/ScenicOrNot/predictor variables/Wilderness Dimensions")
#wildness <- list("access","naturalness","remoteness","ruggedness") %>% lapply(raster)

#test <- lapply(list, raster)
     "access" %>% raster -> Abs
"naturalness" %>% raster -> Nat
 "remoteness" %>% raster -> Rem
 "ruggedness" %>% raster -> Rug

Wales.sp <- 
  raster::getData("GADM", country = "United Kingdom", level = 1) %>%
  subset(NAME_1 == "Wales") %>%
  spTransform(crs(Abs))

Abs[mask(is.na(Abs), rgeos::gBuffer(Wales.sp, byid=T, width=-100))] <- 19.125

LCA.sp <- 
  LCA %>%
  select("UID","VS_46") %>%
  as(., "Spatial")

names(LCA.sp)[2] <- "SQ"

LCA.sp$Sce <- over(LCA.sp, sc[,'Average'], fn = median) %>% as.vector() %>% unlist()
LCA.sp$Abs <- raster::extract(Abs, LCA.sp, fun = median, na.rm = TRUE) %>% as.vector()
LCA.sp$Nat <- raster::extract(Nat, LCA.sp, fun = median, na.rm = TRUE) %>% as.vector()
LCA.sp$Rem <- raster::extract(Rem, LCA.sp, fun = median, na.rm = TRUE) %>% as.vector()
LCA.sp$Rug <- raster::extract(Rug, LCA.sp, fun = median, na.rm = TRUE) %>% as.vector()

#Ordering the dependent variable
LCA.sp$SQ = factor(LCA.sp$SQ, levels = c("Low", "Moderate", "High", "Outstanding"), ordered = TRUE)

plot(LCA.sp$SQ, LCA.sp$Sce)

#Build ordinal logistic regression model
require(MASS)
model = polr(SQ ~ Sce , data = LCA.sp@data, Hess = TRUE)
summary(model)

# Performing the significance test of coefficients and intercepts
summary_table <- coef(summary(model))
pval <- pnorm(abs(summary_table[, "t value"]), lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval, 3))
summary_table

# # Fortunately, we can bypass the above mathematical calculation by using the predict function in R
# new_data <- data.frame("religion"= "yes","degree"="no","country"="Norway","age"=30,"gender"="male")
# round(predict(model_fit,new_data,type = "p"), 3)

require(car)
vif(model)

grd_2.5k <- 
  raster::getData("GADM", country = "United Kingdom", level = 1) %>%
  subset(NAME_1 == "Wales") %>%
  st_as_sf() %>%
  st_transform(crs = 27700) %>%
  st_geometry() %>%
  st_make_grid(cellsize = c(2500,2500), crs = 27700, what = "polygons", square = FALSE)

grd_5k <- 
  raster::getData("GADM", country = "United Kingdom", level = 1) %>%
  subset(NAME_1 == "Wales") %>%
  st_as_sf() %>%
  st_transform(crs = 27700) %>%
  st_geometry() %>%
  st_make_grid(cellsize = c(5000,5000), crs = 27700, what = "polygons", square = FALSE)

#hex_2.5k$Sce <- grd_2.5k %>% as("Spatial") %>% over(sc[,'Average'], fn = median) %>% as.vector() %>% unlist()
grd_2.5k <- as(grd_2.5k, "Spatial")
  grd_5k <- as(grd_5k, "Spatial")
  
grd_2.5k$Abs <- raster::extract(Abs, grd_2.5k, fun = median, na.rm = TRUE) %>% as.vector()
grd_2.5k$Nat <- raster::extract(Nat, grd_2.5k, fun = median, na.rm = TRUE) %>% as.vector()
grd_2.5k$Rem <- raster::extract(Rem, grd_2.5k, fun = median, na.rm = TRUE) %>% as.vector()
grd_2.5k$Rug <- raster::extract(Rug, grd_2.5k, fun = median, na.rm = TRUE) %>% as.vector()

grd_5k$Abs <- raster::extract(Abs, grd_5k, fun = median, na.rm = TRUE) %>% as.vector()
grd_5k$Nat <- raster::extract(Nat, grd_5k, fun = median, na.rm = TRUE) %>% as.vector()
grd_5k$Rem <- raster::extract(Rem, grd_5k, fun = median, na.rm = TRUE) %>% as.vector()
grd_5k$Rug <- raster::extract(Rug, grd_5k, fun = median, na.rm = TRUE) %>% as.vector()


