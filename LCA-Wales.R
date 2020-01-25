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
### 2.1.1.
plot(LCA.sp$SQ, LCA.sp$Sce)

### 2.1.2.
boxplot(LCA.sp$Sce ~ LCA.sp$SQ, main="Fig 1. Boxplot of Scenic-Or-Not ratings vesus four SCENIC QUALITY")

### 2.1.3.
require(ggplot2)
ggplot(LCA.sp@data, aes(SQ, Sce)) + 
  geom_boxplot(aes(col=SQ)) + 
  labs(title="Boxplot of Scenic-Or-Not ratings vesus four SCENIC QUALITY")

### 2.1.4.
ggplot(LCA.sp@data, aes(x = SQ, y = Sce)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  #facet_grid(pared ~ public, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1))

##### 2.2 To find out the exact outliers or extreme observations
boxplot.stats(LCA.sp$Sce[LCA.sp$SQ == "Low"])
boxplot.stats(LCA.sp$Sce[LCA.sp$SQ == "Moderate"])
boxplot.stats(LCA.sp$Sce[LCA.sp$SQ == "High"])
boxplot.stats(LCA.sp$Sce[LCA.sp$SQ == "Outstanding"])

##### 2.3. Fit one-way Analysis of Variance (ANOVA)
### 2.3.1. Using lm function
### https://www.r-bloggers.com/one-way-analysis-of-variance-anova/
m.anova1 <- lm(Sce ~ SQ, data = LCA.sp@data)
anova(m.anova)
confint(m.anova)

### 2.3.2. Using aov function
### https://datascienceplus.com/one-way-anova-in-r/
m.anova2 <- aov(LCA.sp$Sce ~ LCA.sp$SQ)
summary(m.anova2)

##### 2.4. To find out the pair of levels of SCENIC QUALITY which differ
TukeyHSD(m.anova2, conf.level = 0.99)

### 2.4.1. 
plot(TukeyHSD(m.anova2, conf.level = 0.99), las = 1, col = "red")

### 2.4.2. Visualisation with gplot package
gplots::plotmeans(LCA.sp$Sce ~ LCA.sp$SQ, 
                  main="Fig 2: Mean Plot with 95% Confidence Interval", 
                  ylab = "Scenic-Or-Not ratings (1-10 score)", 
                  xlab = "SCENIC QUALITY of LANDMAP")

##### 2.5. Diagnostic Checking
par(mfrow=c(2,2))
plot(m.anova2)

####### 3. Build a global ordinal logistic regression model
##### 3.1. Base model (only one predictor i.e. Scenic-Or-Not)
require(MASS)
 model = polr(SQ ~ Sce, data = LCA.sp@data, Hess = TRUE)

### 3.1.1. Mapping the residual
LCA.sp$fitted <- 
  colnames(model$fitted.values)[max.col(model$fitted.values, ties.method="first")] %>%
  factor(., levels = c("Low", "Moderate", "High", "Outstanding"), ordered = TRUE)

require(dplyr)
# The confusion matrix from a single assessment set (i.e. fold)
cm <- 
  LCA.sp@data %>%
  yardstick::conf_mat(., SQ, fitted)
autoplot(cm, type = "heatmap")

LCA.sf 

LCA.sf <- 
  LCA.sp %>%
  st_as_sf() %>%
  mutate(cm = paste(SQ, fitted, sep="_"))

LCA.sf$cm <- factor(LCA.sf$cm, levels = c("Low_Outstanding",       "Outstanding_Low", 
                                                 "Low_High",  "Moderate_Outstanding",
                                                 "High_Low",  "Outstanding_Moderate",
                                             "Low_Moderate",         "Moderate_High", "High_Outstanding",
                                             "Moderate_Low",         "High_Moderate", "Outstanding_High",
                                                  "Low_Low",     "Moderate_Moderate",        "High_High", "Outstanding_Outstanding"), ordered = TRUE)
require(RColorBrewer)  
ggplot(LCA.sf) +
  geom_sf(aes(fill=cm), colour = NA) +
  scale_fill_brewer(palette = colorRampPalette(brewer.pal(16, "Set1"))) +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())  














##### 3.2. Include wildness components as additional predictors
model1 = polr(SQ ~ scale(Sce) + scale(Abs), data = LCA.sp@data, Hess = TRUE)
model2 = polr(SQ ~ scale(Sce) + scale(Abs) + scale(Nat), data = LCA.sp@data, Hess = TRUE)
model3 = polr(SQ ~ scale(Sce) + scale(Abs) + scale(Nat) + scale(Rem), data = LCA.sp@data, Hess = TRUE)
model4 = polr(SQ ~ scale(Sce) + scale(Abs) + scale(Nat) + scale(Rem) + scale(Rug), data = LCA.sp@data, Hess = TRUE)

summary(model)
summary(model1)
summary(model2)
summary(model3)
summary(model4)

AIC.scores <- c(AIC(model), AIC(model1), AIC(model2), AIC(model3), AIC(model4))
names(AIC.scores) <- c("model", "model1", "model2", "model3", "model4")
AICw <- geiger::aicw(AIC.scores)

table <- function(model){
  summary_table <- coef(model) %>% exp() %>% round(3) %>% as.data.frame() 
  pval <- 
    coef(summary(model))[1:length(model$coefficients), "t value"] %>% 
    abs() %>%
    pnorm(., lower.tail = FALSE)*2 
  tab <- cbind("Odds Ratio" = summary_table, "p value" = round(pval, 3))
  return(tab)
}

table(model)
table(model1)
table(model2)
table(model3)
table(model4)

## These weights can be interpreted as the probability of each model given the data, 
## as described in the ‘Methods’ section. This model comparison indicates that models 
## including crowdsourced geographic data from Flickr and OpenStreetMap provide more 
## accurate estimates of the scenicness of an area than models that only include objective
## measurements such as population density and whether an area is urban, suburban or rural (Table 3).
##
require(ggplot2)
LCA.sp %>%
  spatialEco::sp.na.omit(margin = 1) %>%
  data.frame(Fitted = fitted(m.anova), Residuals = resid(m.anova), SCENIC_QUALITY = .$SQ) %>%
  ggplot(., aes(Fitted, Residuals, colour = SCENIC_QUALITY)) + geom_point()

# Performing the significance test of coefficients (p-value for the coefficient estimates)
model = model1
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

ci <- confint(model4) # default method gives profiled CIs 

#confint.default(model)
# The coefficients from the model can be somewhat difficult to interpret 
# because they are scaled in terms of logs. 
# Another way to interpret logistic regression models is to convert the coefficients into odds ratios. 
# To get the OR and confidence intervals, we just exponentiate the estimates and confidence intervalse

# onely one predictor
exp(cbind(OR = coef(model4), t(ci)))
# more than one predictor
exp(cbind(OR = coef(model4), ci))

# Hmisc library
# Verifying both interpretations of the odds ratio using predicted probabilities
test <- predict(model4, data.frame(Sce = c(1,2,3,4,5,6,7,8,9,10)), type = "p") %>% round(.,3)

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
plot(Effect(focal.predictors = "Sce", model4))
plot(Effect(focal.predictors = "Abs", model4))
plot(Effect(focal.predictors = "Nat", model4))
plot(Effect(focal.predictors = "Rem", model4))
plot(Effect(focal.predictors = "Rug", model4))
plot(Effect(focal.predictors = c("Sce", "Abs", "Nat"),model))
# # Fortunately, we can bypass the above mathematical calculation by using the predict function in R
# new_data <- data.frame("religion"= "yes","degree"="no","country"="Norway","age"=30,"gender"="male")
# round(predict(model_fit,new_data,type = "p"), 3)


##### 3.3. Geographically Weighted Ordinal Regression (GWOR)
library(dplyr)
library(maxLik)
library(MASS)
library(maptools)
library(geoR)
library(sf)
library(sp)
library(rgdal)
library(gstat)
library(GWmodel)

## Functions for implementing GWOR
source("/Users/Yi-Min/Rsession/ScenicOrNot/scenicness/GWOR/orderedfit.R")
source("/Users/Yi-Min/Rsession/ScenicOrNot/scenicness/GWOR/bw.gwordered.R")
source("/Users/Yi-Min/Rsession/ScenicOrNot/scenicness/GWOR/gwr.ordered.R")
source("/Users/Yi-Min/Rsession/ScenicOrNot/scenicness/GWOR/MonteCarlo.R")
tmp -> LCA.sp

LCA.sp %>%
  st_as_sf() %>%
  #dplyr::select(UID, SQ, Sce, Abs, Nat, Rem, Rug) %>%
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

# filter(SQ %in% c("Low", "Moderate","High","Outstanding")) %>%
dMat <- gw.dist(dp.locat=coordinates(LCA.sp), focus=0, p=2, theta=0)
#dMat <- spDists(LCA.sp@coords)
#### Bandwidth selection
formula <- SQ ~ scale(Sce) + scale(Abs)
formula <- SQ ~ scale(Sce) + scale(Abs) + scale(Nat)
formula <- SQ ~ scale(Sce) + scale(Abs) + scale(Nat) + scale(Rem) + scale(Rug)

# gwor.fixed.logit 
bw.fixed.logit <- bw.gwordered(formula, data=LCA.sp, kernel="bisquare", adaptive=FALSE, link="logit", dMat=dMat, fixed.vars=NULL)
gwor.fixed.logit.int <- gwr.ordered(formula, data=LCA.sp, kernel="bisquare", adaptive=FALSE, bw=bw.fixed.logit.int, link="logit", dMat=dMat, fixed.vars=NULL)

# gwor.fixed.probit
bw.fixed.probit <- bw.gwordered(formula, data=LCA.sp, kernel="bisquare", adaptive=FALSE, link="probit", dMat=dMat, fixed.vars=NULL)
gwor.fixed.probit <- gwr.ordered(formula, data=LCA.sp, kernel="bisquare", adaptive=FALSE, bw=bw.fixed.logit, link="probit", dMat=dMat, fixed.vars=NULL)

bw.fixed.probit.wild2 <- bw.gwordered(formula, data=LCA.sp, kernel="bisquare", adaptive=FALSE, link="probit", dMat=dMat, fixed.vars=scale(LCA.sp$Rem))
gwor.fixed.wild.probit2 <- gwr.ordered(formula, data=LCA.sp[,c(-1,-8)], kernel="bisquare", adaptive=FALSE, bw=bw.fixed.probit.wild2, link="probit", dMat=dMat, fixed.vars=NULL)

LCA.sp <- LCA.sp[,c(-1,-8)]
gwor.fixed.wild.probit3 <- gwr.ordered(formula, data=LCA.sp, kernel="bisquare", adaptive=FALSE, bw=bw.fixed.probit.wild2, link="probit", dMat=dMat, fixed.vars=NULL)

# test the spatial non-stationary in local coefficients using Monte Carlo approach
 test.fixed.logit <- Monte.Carlo(formula, LCA.sp, kernel="bisquare", adaptive=FALSE, bw=bw.fixed.logit, link="logit", dMat=dMat, fixed.vars=NULL, nsim=99)
test.fixed.probit <- Monte.Carlo(formula, LCA.sp, kernel="bisquare", adaptive=FALSE, bw=bw.fixed.probit, link="probit", dMat=dMat, fixed.vars=NULL, nsim=99)

##### 3.3.2. To map the coefficient estimate of GWOR

LCA.sp <- SpatialPolygonsDataFrame(LCA.sp, data = data.frame(LCA.sp, gwor.fixed.wild.probit3$betas))

ggplot(st_as_sf(LCA.sp)) +
  geom_sf(aes(fill=scale.Sce.), colour = NA) +
  scale_fill_distiller(type = "seq", palette = "GnYlBu", direction = 1) +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())  

########### Trial run for zonation ############
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

