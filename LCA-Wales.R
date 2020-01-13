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

####### 2. Exploratory analysis
##### 2.1. boxplot LANDMAP Scenic Quality and Scenic-Or-Not ratings
plot(LCA.sp$SQ, LCA.sp$Sce)
require(ggplot2)
ggplot(LCA.sp@data, aes(x = SQ, y = Sce)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  #facet_grid(pared ~ public, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1))

#Build ordinal logistic regression model
require(MASS)
model = polr(SQ ~ Sce, data = LCA.sp@data, Hess = TRUE)
summary(model)

# Performing the significance test of coefficients (p-value for the coefficient estimates)
summary_table <- coef(summary(model))
pval <- pnorm(abs(summary_table[, "t value"]), lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval, 3))
tab <- summary_table

# Confidence Intervals:
# We can also get confidence intervals for the parameter estimates. 
# These can be obtained either by profiling the likelihood function or 
# by using the standard errors and assuming a normal distribution. 
# Note that profiled CIs are not symmetric (although they are usually close to symmetric). 
# If the 95% CI does not cross 0, the parameter estimate is statistically significant.

ci <- confint(model) # default method gives profiled CIs
confint.default(model)
# The coefficients from the model can be somewhat difficult to interpret 
# because they are scaled in terms of logs. 
# Another way to interpret logistic regression models is to convert the coefficients into odds ratios. 
# To get the OR and confidence intervals, we just exponentiate the estimates and confidence intervalse

#exp(cbind(OR = coef(model), ci))
cbind(OR = coef(model), t(as.matrix(ci))) %>% exp()

# Hmisc library
predict(model, data.frame(Sce = 7), type = "response")

# interpreting the results
# Apply inverse logit to transform to probabilities
#  (See Equation in the margin)
# exponentiate log odds (logit) to odds scale
exp(coef(model))
exp(model$zeta)
     prob.Sce <- 1 / (1 + exp(2.13))
     prob.Low <- 1 / (1 + exp(-1.4328))
prob.Moderate <- 1 / (1 + exp(-3.7215))
    prob.High <- 1 / (1 + exp(-6.1166))

#Compute confusion table and misclassification error
predictSQ = predict(model,LCA.sp@data)
tab1 <- table(LCA.sp$SQ, predictSQ)
summary(as.character(LCA.sp$SQ) != as.character(predictSQ))    
790/(790+921)
921/(790+921)

#Plotting the effects 
library("effects")
plot(Effect(focal.predictors = "Sce", model))
plot(Effect(focal.predictors = "Abs", model))
plot(Effect(focal.predictors = "Nat", model))
plot(Effect(focal.predictors = c("Sce", "Abs", "Nat"),model))
# # Fortunately, we can bypass the above mathematical calculation by using the predict function in R
# new_data <- data.frame("religion"= "yes","degree"="no","country"="Norway","age"=30,"gender"="male")
# round(predict(model_fit,new_data,type = "p"), 3)





##### 3.3. Geographically Weighted Ordinal Regression (GWOR)
## Functions for implementing GWOR
source("/Users/Yi-Min/Rsession/LANDMAP/supporting materials/orderedfit.R")
source("/Users/Yi-Min/Rsession/LANDMAP/supporting materials/bw.gwordered.R")
source("/Users/Yi-Min/Rsession/LANDMAP/supporting materials/gwr.ordered.R")
source("/Users/Yi-Min/Rsession/LANDMAP/supporting materials/MonteCarlo.R")

require(dplyr)

tmp -> LCA.sp

LCA.sp %>%
  st_as_sf() %>%
  dplyr::select(UID, SQ, Sce, Nat) %>%
  as("Spatial") %>%
  spatialEco::sp.na.omit(margin = 1) ->
  LCA.sp

summary(LCA.sp)  

###### find the centroids of the Aspect Area 
setwd("/Users/Yi-Min/Rsession/LANDMAP/Data/LandmapVisualSensory/")
sq <- 
  "NRW_LandMap_Visual_SensoryPolygon.shp" %>% 
  st_read() %>%
  st_centroid() %>%
  dplyr::select(UID) %>%
  left_join(., as.data.frame(LCA.sp)) %>%
  as("Spatial") %>%
  spatialEco::sp.na.omit(margin = 1)


library(maxLik)
library(MASS)
library(maptools)
library(geoR)
library(sp)
library(rgdal)
library(gstat)
library(GWmodel)
# filter(SQ %in% c("Low", "Moderate","High","Outstanding")) %>%
dMat <- gw.dist(dp.locat=coordinates(LCA.sp), focus=0, p=2, theta=0)
#dMat <- spDists(LCA.sp@coords)
#### Bandwidth selection
formula <- SQ ~ Sce

bw.fixed.logit <- bw.gwordered(formula, data=LCA.sp, kernel="bisquare", adaptive=FALSE, link="logit", dMat=dMat, fixed.vars=NULL)
#gwor.fixed.logit 
gwor.fixed.logit.int <- gwr.ordered(formula, data=LCA.sp, kernel="bisquare", adaptive=FALSE, bw=bw.fixed.logit.int, link="logit", dMat=dMat, fixed.vars=NULL)

bw.fixed.probit <- bw.gwordered(formula, data=LCA.sp, kernel="bisquare", adaptive=FALSE, link="probit", dMat=dMat, fixed.vars=NULL)
gwor.fixed.probit <- gwr.ordered(formula, data=LCA.sp, kernel="bisquare", adaptive=FALSE, bw=bw.fixed.logit, link="probit", dMat=dMat, fixed.vars=NULL)

# test the spatial non-stationary in local coefficients using Monte Carlo approach
 test.fixed.logit <- Monte.Carlo(formula, LCA.sp, kernel="bisquare", adaptive=FALSE, bw=bw.fixed.logit, link="logit", dMat=dMat, fixed.vars=NULL, nsim=99)
test.fixed.probit <- Monte.Carlo(formula, LCA.sp, kernel="bisquare", adaptive=FALSE, bw=bw.fixed.probit, link="probit", dMat=dMat, fixed.vars=NULL, nsim=99)
















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

setwd("/Users/Yi-Min/Rsession/LANDMAP/LCA-Wales")
save(list = c("grd_2.5k", "grd_5k", "LCA.sp", "sq", "LCA"), file = "20200107.RData")

