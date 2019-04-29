library(rgdal)
library(rgeos)
library(tidyverse)
library(raster)
library(sf)
library(sp)
library(raster)
library(dplyr)
library(spdep)
library(GISTools)
library(tmap)

##### 1. Data pre-processing for voxel viewshed analysis  
setwd("/Users/Yi-Min/R session/Lake District/")
dem <- readGDAL("DEM_25m")
class(dem)
plot(dem)

# 2.2 pre-processing LCM2015 25m land cover for voxel viewshed analysis 
setwd("/Users/Yi-Min/R session/Lake District/CEH LCM2015/")
"lcm-LakeDist-25m" %>% readGDAL() -> lcm
class(lcm)
dim(dem) == dim(lcm)

# reclassify lcm data
# reclassification matrix
m <- matrix(c(1, 1,  # 1. broadleaved woodland    -> 1. broadleaved  
              2, 2,  # 2. coniferous woodland     -> 2. coniferous
              3, 3,  # 3. arable and horticulture -> 3. agriculture
              4, 3,  # 4. improved grassland      -> 3. agriculture
              5, 4,  # 5. neutral grassland       -> 4. grassland
              6, 4,  # 6. calcareous grassland    -> 4. grassland
              7, 4,  # 7. acid grassland          -> 4. grassland
              8, 4,  # 8. fen, marsh and swamp    -> 4. grassland
              9, 5,  # 9. heather                 -> 5. moor
             10, 5,  # 10.heather grassland       -> 5. moor
             11, 5,  # 11.bog                     -> 5. moor 
             12, 5,  # 12.inland rock             -> 5. moor
             13, 6,  # 13.saltwater               -> 6. water
             14, 6,  # 14.freshwater              -> 6. water
             15, 0,  # 15.supra-littoral rock     -> 0. coast 
             16, 0,  # 16.supra-littoral sediment -> 0. coast
             17, 0,  # 17.littoral rock           -> 0. coast
             18, 0,  # 18.littoral sediment       -> 0. coast
             19, 0,  # 19.saltmarsh               -> 0. coast
             20, 7,  # 20.urban                   -> 7. settlement
             21, 7), # 21.suburban                -> 7. settlement
             ncol=2, byrow = TRUE)
lcm <- reclassify(raster(lcm), m)

summary(lcm)

setwd("/Users/Yi-Min/R session/Lake District/CEH LCM2015/LCM-LakeDist-25m-recl/")
writeRaster(lcm, filename = "LCM-recl.tif", format = "GTiff", datatype = "FLT4S", overwrite = T)
###### end of section 2.

###### 2. load in voxel viewshed variables
  
##### 3.1. load in Scenic-Or-Not dataset
setwd("/Users/Yi-Min/R session/Lake District")
np <- readOGR("NP.shp")
plot(np)

setwd("/Users/Yi-Min/R session/ScenicOrNot/ScenicOrNot dataset")
sc <- readOGR("ScenicOrNot_GB.shp")
sc <- sc[np,]
sc <- sc[,-9:-24]
for (i in 1:(length(sc))){
  sc@data$Vote_num[[i]] <- length(strsplit(as.character(sc@data$Votes[[i]]), ",")[[1]])
}
head(sc)


#### 3.2 load in vv variables
setwd("/Users/Yi-Min/R session/Lake District/VV variables/FLT/")
files <- list.files(path = ".",
					pattern = "\\.flt$", ignore.case = T, full.names = F)

vars <- gsub(".flt", "",files) 
vars <- unique(gsub("\\d", "", vars))
vars

vv <- vector("list", length(vars))
names(vv) <- vars
rm('vars')

# create a RasterLayer list for each voxelviewshed variable
for (i in 1:length(files)) {
	r <- raster(files[i])
	if (grepl("broadleaved", files[i])) {
        vv$broadleaved <- append(vv$broadleaved, r)
	}
	if (grepl("coniferous", files[i])) {
		vv$coniferous <- append(vv$coniferous, r)
	}
	if (grepl("agriculture", files[i])) {
		vv$agriculture <- append(vv$agriculture, r)
	}
	if (grepl("grassland", files[i])) {
		vv$grassland <- append(vv$grassland, r)
	}
	if (grepl("moor", files[i])) {
		vv$moor <- append(vv$moor, r)
	}
	if (grepl("water", files[i])) {
		vv$water <- append(vv$water, r)
	}
	if (grepl("settlement", files[i])) {
		vv$settlement <- append(vv$settlement, r)
	}
	if (grepl("coast", files[i])) {
		vv$coast <- append(vv$coast, r)
	}
}

vv$coast %>% do.call("merge",.) %>% as("SpatialGridDataFrame") -> coast
vv$broadleaved %>% do.call("merge",.) %>% as("SpatialGridDataFrame") -> broadleaved
vv$coniferous %>% do.call("merge",.) %>% as("SpatialGridDataFrame") -> coniferous
vv$agriculture %>% do.call("merge",.) %>% as("SpatialGridDataFrame") -> agriculture
vv$grassland %>% do.call("merge",.) %>% as("SpatialGridDataFrame") -> grassland
vv$moor %>% do.call("merge",.) %>% as("SpatialGridDataFrame") -> moor
vv$water %>% do.call("merge",.) %>% as("SpatialGridDataFrame") -> water
vv$settlement %>% do.call("merge",.) %>% as("SpatialGridDataFrame") -> settlement

 
# assign each variable's coordinate reference as the same with sc
crs(coast) <- crs(sc)
crs(broadleaved) <- crs(sc)
crs(coniferous) <- crs(sc)
crs(agriculture) <- crs(sc)
crs(grassland) <- crs(sc)
crs(moor) <- crs(sc)
crs(water) <- crs(sc)
crs(settlement) <- crs(sc) 

ol0 <- over(sc, coast)
ol1 <- over(sc, broadleaved)
ol2 <- over(sc, coniferous)
ol3 <- over(sc, agriculture)
ol4 <- over(sc, grassland)
ol5 <- over(sc, moor)
ol6 <- over(sc, water)
ol7 <- over(sc, settlement)
rm(list = c("coast", "broadleaved", "coniferous", "agriculture", "grassland", "moor", "water", "settlement"))

# combine to data frame
df <- data.frame(ol0, ol1, ol2, ol3, ol4, ol5, ol6, ol7)
rm(list = c("ol0", "ol1", "ol2", "ol3", "ol4", "ol5", "ol6", "ol7"))
head(df)

sc <- SpatialPointsDataFrame(sc, data = data.frame(sc, df))
head(sc)
names(sc)
sc <- sc[,-10:-12]
names(sc)[10:17] <- c("Coas", "Broa", "Coni", "Agri", "Gras", "Moor", "Wate", "Sett")
sc@data[is.na(sc@data)] <- 0
head(sc)
names(sc)
setwd("/Users/Yi-Min/R session/Lake District/")
save.image("LakeDistrict_v2.RData")

#### 3.2. wildness attributes
setwd("/Users/Yi-Min/R session/ScenicOrNot/predictor variables/Wilderness Dimensions")
load("wilderness.RData")
proj <- CRS(proj4string(sc))
proj2 <- CRS(proj4string(rg))
sc <- spTransform(sc, proj2)
# do overlays
ol1 <- over(sc, rg)
ol2 <- over(sc, na)
ol3 <- over(sc, ac)
ol4 <- over(sc, re)
rm(list = c("rg", "na", "ac", "re"))

# combine to df
df <- data.frame(ol1, ol2, ol3, ol4)
rm(list = c("ol1", "ol2", "ol3", "ol4"))
head(df)
names(df) <- c("Rug", "Nat", "Acc", "Rem")
# create spdf
sc <- SpatialPointsDataFrame(sc, data = data.frame(sc, df), proj4string = proj2)
sc <- spTransform(sc, proj)
names(sc)
sc <- sc[,-18:-20]

setwd("/Users/Yi-Min/R session/Lake District/")
save.image("LakeDistrict_v2.RData")

#### 3.3. LCM 1km raster
setwd("/Users/Yi-Min/R session/Lake District/CEH LCM2015/lcm-2015-1km_2521372/aggregate_class/")
"LCM2015_GB_1km_percent_cover_aggregate_class.tif" %>% readGDAL() -> lcm
np %>% spTransform(crs(lcm)) -> np
crs(np)
crs(lcm)
plot(lcm) + 
  plot(np, add = T, col = "white")
lcm@data$LCM3 <- lcm@data$band3 + lcm@data$band4
lcm@data$LCM6 <- lcm@data$band7 + lcm@data$band8
lcm@data <- lcm@data[c(1, 2, 11, 5, 6, 12, 9, 10)]/100
names(lcm) <- c("LCM1_Broa", "LCM2_Coni", "LCM3_Agri", "LCM4_Gras", "LCM5_Moor", "LCM6_Wate", "LCM7_Coas", "LCM8_Sett")
head(lcm)

proj <- CRS(proj4string(sc))
proj2 <- CRS(proj4string(lcm))
sc <- spTransform(sc, proj2)
# do overlays
ol1 <- over(sc, lcm)
rm(list = c("lcm"))


# create spdf
sc <- SpatialPointsDataFrame(sc, data = data.frame(sc, ol1), proj4string = proj2)
sc <- spTransform(sc, proj)
names(sc)
sc <- sc[,-23:-36]
setwd("/Users/Yi-Min/R session/Lake District/")
save.image("LakeDistrict_v3.RData")


## 3.1 Global regression model
setwd("/Users/Yi-Min/R session/Lake District/")
load("LakeDistrict_v3.RData")
head(sc)
names(sc)
sc@data[is.na(sc@data)] <- 0
sc@data$round <- round(sc@data$Average)
reg.mod <- as.formula(median ~ LCM1_Broa + LCM2_Coni + LCM3_Agri + LCM4_Gras + LCM5_Moor + LCM6_Wate + LCM7_Coas + LCM8_Sett + Rug + Nat + Acc + Rem)
reg.mod <- as.formula(median ~ LCM1_Broa + LCM2_Coni + LCM3_Agri + LCM4_Gras + LCM5_Moor + LCM6_Wate + LCM7_Coas + LCM8_Sett)

#reg.mod <- as.formula(median ~ Coas + Broa + Coni + Agri + Gras + Moor + Wate + Sett + Rug + Nat + Acc + Rem)

# 3.1.1 OLS
ols.m <- lm(reg.mod, data = sc)
summary(ols.m)
round(coef(summary(ols.m)), 3)
summary(stepAIC(ols.m, trace = 0))
round(coef(summary(stepAIC(ols.m, trace = 0))))
tab_OLS <- round(summary(ols.m)$coefficients[,c(1:4)], 3)
setwd("/Users/Yi-Min/R session/Lake District/Tabs")
write.csv(tab_OLS, "Tab3.csv")

s.resids = rstandard(ols.m) # to compute some of the regression (leave-one-out deletion) diagnostics for linear and generalized linear models discussed in Belsley, Kuh and Welsch (1980), Cook and Weisberg (1982)

resid.shades = shading(c(-2,2),c("red","grey","blue"))
cols = resid.shades$cols[1 + findInterval(s.resids, resid.shades$breaks)]
quartz()
plot(ols.m, col = cols)
abline(lm(reg.mod), col='red', lwd = 2, lty = 2)


resid.shades = shading(c(-2,2),c("red","grey","blue"))

setwd("/Users/Yi-Min/R session/Lake District/Figs")
png(filename = "f2.png", w = 5, h = 5, units = "in", res = 300)
par(mar=c(0,0,0,0)) 
choropleth(sc, s.resids, resid.shades, pch = 20)
plot(np, add = T)
#choro.legend(400000, 300000, resid.shades, fmt="%4.1g", cex = 0.5, title = 'Residuals Map')
# reset the plot margins
par(mar=c(5,4,4,2))
dev.off()

# 3.1.2 Genernalised Linear Model ??
lm.glm <- glm(rm.standard, family= poisson, data = sc) # fitting generalised 
summary(lm.glm)



# Autocorrelation tests on response and LM error data
moran.resid.i <- lm.morantest(lm.stand, listw = glw)


## 3.2 SAR model - simultaneous autoregressive

nb2listw
err.model.i = errorsarlm(rm.standard, sc@data, listw = glw, method = 'spam') # SAR error model 

















