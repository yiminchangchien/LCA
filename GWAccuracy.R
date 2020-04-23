library(ggplot2)
library(GWmodel)
library(openxlsx)
library(SnowballC)
library(sf)
library(sp)
library(spatialEco)
library(tidyr)
library(tm)
library(topicmodels)
library(wordcloud)


#### Data preparation

sc.subject <-
  read.xlsx("/Users/Yi-Min/Rsession/ScenicOrNot/ScenicOrNot dataset/ScenicOrNot_Geograph_v8.xlsx") %>%
  st_as_sf(coords=c("sb_east","sb_north"), crs=27700)

sc.viewpoint <-
  read.xlsx("/Users/Yi-Min/Rsession/ScenicOrNot/ScenicOrNot dataset/ScenicOrNot_Geograph_v8.xlsx") %>%
  st_as_sf(coords=c("vp_east","vp_north"), crs=27700) 

sc.reference <-
  read.xlsx("/Users/Yi-Min/Rsession/ScenicOrNot/ScenicOrNot dataset/ScenicOrNot_Geograph_v8.xlsx") %>%
  drop_na(grd_east, grd_north) %>%
  st_as_sf(coords=c("grd_east","grd_north"), crs=27700) 

st_write(sc.subject, "//Users/Yi-Min/Rsession/ScenicOrNot/ScenicOrNot dataset/sc.subject.shp", delete_layer = TRUE)
st_write(sc.viewpoint, "//Users/Yi-Min/Rsession/ScenicOrNot/ScenicOrNot dataset/sc.viewpoint.shp", delete_layer = TRUE)
st_write(sc.reference, "//Users/Yi-Min/Rsession/ScenicOrNot/ScenicOrNot dataset/sc.reference.shp", delete_layer = TRUE)

Wales <- 
  st_read("/Users/Yi-Min/Rsession/ScenicOrNot/Wales boundary/wales_ol_2001.shp") %>% 
  st_transform(st_crs(sc.subject)) %>%
  st_combine()

# rgdal::writeOGR(sc.viewpoint %>% as("Spatial"), "/Users/Yi-Min/Rsession/ScenicOrNot/ScenicOrNot dataset/", "ScenicOrNot_Geograph_viewpoint.shp", driver="ESRI Shapefile")

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

sc.Wales <- 
  sc.subject[Wales, ] %>%
  st_join(., LCA, join = st_intersects) %>%
  mutate(., Layman = cut(mean, c(0, 2.5, 5, 7.5, 10)))

levels(sc.Wales$Layman) = c("Low","Moderate","High","Outstanding")
sc.Wales <- sc.Wales[ ,c(10,5:9,11:27,32,104,95,33:103)]
names(sc.Wales)[25:26] <- c("Layman", "SQ")

data <- sc.Wales
data$SQ = factor(data$SQ, levels = c("Low", "Moderate", "High", "Outstanding"), ordered = FALSE)
data$Layman = factor(data$Layman, levels = c("Low", "Moderate", "High", "Outstanding"), ordered = FALSE)
summary(factor(data$SQ))
summary(factor(data$Layman))

tab <- base::table(data$SQ, data$Layman, useNA = "no")
class.names.long <- c("Low", "Moderate", "High", "Outstanding")
rownames(tab) <- class.names.long
colnames(tab) <- class.names.long
tab <- cbind(tab, rowSums(tab))
tab <- rbind(tab, colSums(tab))
rownames(tab)[length(class.names.long)+1] <- "Total"
colnames(tab)[length(class.names.long)+1] <- "Total"

num_class = length(class.names.long)
# Users accuracy
tmp <- vector(mode = "numeric", length = num_class+1)
for (i in 1:length(class.names.long)) {
  tmp[i] <- tab[i,i] / tab[i,num_class+1]
}
tab <- cbind(tab, zapsmall(tmp, 3))
colnames(tab)[num_class+2] <- "Users"

# Producers accuracy
tmp <- vector(mode = "numeric", length = num_class+2)
for (i in 1:num_class) {
  tmp[i] <- tab[i,i] / tab[num_class+1,i]
}
tab <- rbind(tab, zapsmall(tmp, 3))
rownames(tab)[num_class+2] <- "Producers"

tab[num_class+2,num_class+2] <- 
  sum(diag(base::table(data$SQ, data$Layman, useNA = "no")))/sum(base::table(data$SQ, data$Layman, useNA = "no"))

round(tab, 2)

dMat <-
  data %>% 
  drop_na(SQ) %>%
  as("Spatial") %>% 
  coordinates() %>%
  gw.dist()

## User Accuracy
UserAccuracy <- function(data = data, i = 1, dMat = dMat, bw = bw) {
  # #  1. Create a data.frame
  class.list <- unique(data$SQ)[order(unique(data$SQ))]
  # Look at "Outstanding" first
  class <- class.list[i]
  # have a look!
  class
  data$Expert <- (data$SQ == class) * 1
  data$Nonexpert <- (data$Layman == class) * 1

  # 2. GLM for User Accuracy
  glm(Nonexpert ~ Expert, data = data, family = binomial) ->
  mod
  
  # 3. Define and apply the alogit function
  alogit <- function(x){exp(x)/(1+exp(x))}
  mod.coefs <- mod$coefficients
  mod.coefs[2] <- sum(mod.coefs)
  # P(y = 1|x = 1)
  mod.user <- alogit(mod.coefs[2])
  cat("user accuracy:", round(mod.user, 3))
  
  # 4. GW User Accuracy
  # drop_na(data,SQ) %>%
  #   as("Spatial") %>%
  #   bw.ggwr(Nonexpert ~ Expert,
  #           data = .,
  #           family = "binomial",
  #           approach = "AIC",
  #           kernel = "gaussian",
  #           adaptive = FALSE,
  #           dMat = dMat) ->
  # bw
  
  drop_na(data,SQ) %>%
    as("Spatial") %>%
    ggwr.basic(Nonexpert ~ Expert,
               data = .,
               bw = bw,
               family = "binomial",
               kernel = "gaussian",
               adaptive = FALSE,
               cv = FALSE, ######  Error in if (abs((old.llik - llik)/llik) < tol) break : missing value where TRUE/FALSE needed
               dMat = dMat) ->
    gwr.mod

  coefs <- data.frame(gwr.mod$SDF)[,2:3]
  coefs[,2] <- rowSums(coefs)
  # P(x = 1|y = 1)
  gwr.user <- alogit(coefs[,2])
  return(list("Global User Accuracy" = mod.user, 
                               "GLM" = mod,
                         "Bandwidth" = bw,
                              "data" = st_drop_geometry(data),
                  "GW User Accuracy" = gwr.user, 
                            "GW-GLM" = gwr.mod))
}

## Producer Accuracy
ProducerAccuracy <- function(data = data, i = 1, dMat = dMat, bw = bw) {
  # #  1. Create a data.frame
  class.list <- unique(data$SQ)[order(unique(data$SQ))]
  # Look at "Outstanding" first
  class <- class.list[i]
  # have a look!
  class
  data$Expert <- (data$SQ == class) * 1
  data$Nonexpert <- (data$Layman == class) * 1
  
  # 2. GLM for Producer Accuracy
  mod <- glm(Expert ~ Nonexpert, data = data, family = binomial)

  # 3. Define and apply the alogit function
  alogit <- function(x){exp(x)/(1+exp(x))}
  mod.coefs <- mod$coefficients
  mod.coefs[2] <- sum(mod.coefs)
  # P(y = 1|x = 1)
  mod.producer <- alogit(mod.coefs[2])
  cat("Producer accuracy:", round(mod.producer, 3))
  
  # 4. GW Producer Accuracy
  # bw <- 
  #   drop_na(data,SQ) %>%
  #   as("Spatial") %>%
  #   bw.ggwr(Expert ~ Nonexpert,
  #           data = .,
  #           family = "binomial",
  #           approach = "AIC",
  #           kernel = "gaussian",
  #           adaptive = FALSE,
  #           dMat = dMat)
  
  drop_na(data,SQ) %>%
    as("Spatial") %>%
    ggwr.basic(Expert ~ Nonexpert,
               data = .,
               bw = bw,
               family = "binomial",
               kernel = "gaussian",
               adaptive = FALSE,
               cv = FALSE,
               dMat = dMat) ->
    gwr.mod
  
  coefs <- data.frame(gwr.mod$SDF)[,2:3]
  coefs[,2] <- rowSums(coefs)
  # P(x = 1|y = 1)
  gwr.user <- alogit(coefs[,2])
  return(list("Global Producer Accuracy" = mod.user, 
              "GLM" = mod,
              "Bandwidth" = bw,
              "data" = st_drop_geometry(data),
              "GW Producer Accuracy" = gwr.user, 
              "GW-GLM" = gwr.mod))
}

        Low.user <- UserAccuracy(data = data, i = 1, dMat = dMat, bw = 10000)
   Moderate.user <- UserAccuracy(data = data, i = 2, dMat = dMat, bw = 10000)
       High.user <- UserAccuracy(data = data, i = 3, dMat = dMat, bw = 10000)
Outstanding.user <- UserAccuracy(data = data, i = 4, dMat = dMat, bw = 10000)

        Low.producer <- ProducerAccuracy(data = data, i = 1, dMat = dMat, bw = 10000)
   Moderate.producer <- ProducerAccuracy(data = data, i = 2, dMat = dMat, bw = 10000)
       High.producer <- ProducerAccuracy(data = data, i = 3, dMat = dMat, bw = 10000)
Outstanding.producer <- ProducerAccuracy(data = data, i = 4, dMat = dMat, bw = 10000)

#         Low.user <- User.Accuracy.10km[[1]]
#    Moderate.user <- User.Accuracy.10km[[2]]
#        High.user <- User.Accuracy.10km[[3]]
# Outstanding.user <- User.Accuracy.10km[[4]]
# 
#         Low.producer <- Producer.Accuracy.10km[[1]]
#    Moderate.producer <- Producer.Accuracy.10km[[2]]
#        High.producer <- Producer.Accuracy.10km[[3]]
# Outstanding.producer <- Producer.Accuracy.10km[[4]]

c(Low.user$Bandwidth, Moderate.user$Bandwidth, High.user$Bandwidth, Outstanding.user$Bandwidth)
# Error: object 'Low.user' not found
# [1] 4303.917 3789.011 7898.202
c(Low.producer$Bandwidth, Moderate.producer$Bandwidth, High.producer$Bandwidth, Outstanding.producer$Bandwidth)
# Error: object 'Low.producer' not found
# [1] 1852.408 1784.965 1988.548

### Data Preparation
Outstanding.user.df <- 
  as.data.frame(Outstanding.user$`GW-GLM`$SDF) %>%
  cbind(., Outstanding = Outstanding.user$`GW User Accuracy`, drop_na(Outstanding.user$data, SQ))
Outstanding.producer.df <- 
  as.data.frame(Outstanding.producer$`GW-GLM`$SDF) %>%
  cbind(., Outstanding = Outstanding.producer$`GW Producer Accuracy`, drop_na(Outstanding.producer$data, SQ))
# Outstanding.inaccurate <- Outstanding.df[which(Outstanding.df$Outstanding < 0.35), ]

High.user.df <- 
  as.data.frame(High.user$`GW-GLM`$SDF) %>%
  cbind(., High = High.user$`GW User Accuracy`, drop_na(High.user$data, SQ))
High.producer.df <- 
  as.data.frame(High.producer$`GW-GLM`$SDF) %>%
  cbind(., High = High.producer$`GW User Accuracy`, drop_na(High.producer$data, SQ))
# High.inaccurate <- High.df[which(High.df$High < 0.35), ]

Moderate.user.df <- 
  as.data.frame(Moderate.user$`GW-GLM`$SDF) %>%
  cbind(., Moderate = Moderate.user$`GW User Accuracy`, drop_na(Moderate.user$data, SQ))
Moderate.producer.df <- 
  as.data.frame(Moderate.producer$`GW-GLM`$SDF) %>%
  cbind(., Moderate = Moderate.producer$`GW User Accuracy`, drop_na(Moderate.producer$data, SQ))
# Moderate.inaccurate <- Moderate.df[which(Moderate.df$Moderate < 0.35), ]
  
Low.user.df <- 
  as.data.frame(Low.user$`GW-GLM`$SDF) %>%
  cbind(., Low = Low.user$`GW User Accuracy`, drop_na(Low.user$data, SQ))
Low.producer.df <- 
  as.data.frame(Low.producer$`GW-GLM`$SDF) %>%
  cbind(., Low = Low.producer$`GW User Accuracy`, drop_na(Low.producer$data, SQ))
# Low.inaccurate <- Low.df[which(Low.df$Low < 0.35), ]

wal.df <- broom::tidy(Wales.sp)
# np.df <- tidy(np.shp)
# aonb.df <- tidy(aonb.shp)

## plot function
ggplot.func <- function(data.i = Outstanding.df, i = 2, palette = "GnBu", tab = "User Accuracy") { #TV = t_value,
  tit = names(data.i)[i]
  val = data.i[,i]
  index_Expert <- which(data.i$Expert == 1)
  index_Nonexpert <- which(data.i$Nonexpert == 1)
  tit = paste0(tab, ": ", tit)
  p <- ggplot(wal.df) + 
    geom_polygon(aes(x = long, y = lat, group = group), 
                 colour="grey", fill="grey") +
    coord_equal() + 
    geom_point(data = data.i, aes(x = coords.x1, y = coords.x2, colour = val), size = 0.8) +
    # geom_point(data = data.i[index_Expert, ], aes(x = coords.x1, y = coords.x2), size = 0.5, shape = 1) + #alpha = 0.2
    # geom_point(data = data.i[index_Nonexpert, ], aes(x = coords.x1, y = coords.x2), size = 0.5, shape = 3, color = "red") +
    labs(x = NULL, y = NULL, title = tit, size = 1) +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    scale_colour_distiller(type = "seq", 
                           palette = palette, 
                           name = NULL, 
                           direction = 1,
                           guide = guide_colourbar(title.position = "top",
                                                   title.hjust = 0.5,
                                                   title.vjust = 0.8,
                                                   barwidth = 15))
  p
}

## multiplot function
multiplot <- function(plot.list, file, cols=3, layout=NULL) {
  library(grid)
  numPlots = length(plot.list)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plot.list[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
    }
  }
}
## FIGURE 3. mapping GWR and MGWR estimates for the wildness covariates
p1 <- ggplot.func(data.i = Outstanding.user.df[,-2], i = 11, palette =   "GnBu", tab = "User Accuracy") #  TV = gwr_tv,
p2 <- ggplot.func(data.i =        High.user.df[,-2], i = 11, palette =   "RdPu", tab = "User Accuracy") 
p3 <- ggplot.func(data.i =    Moderate.user.df[,-2], i = 11, palette =   "YlGn", tab = "User Accuracy") 
p4 <- ggplot.func(data.i =         Low.user.df[,-2], i = 11, palette = "YlOrBr", tab = "User Accuracy") 

p5 <- ggplot.func(data.i = Outstanding.producer.df[,-2], i = 11, palette =   "GnBu", tab = "Producer Accuracy") #  TV = gwr_tv,
p6 <- ggplot.func(data.i =        High.producer.df[,-2], i = 11, palette =   "RdPu", tab = "Producer Accuracy") 
p7 <- ggplot.func(data.i =    Moderate.producer.df[,-2], i = 11, palette =   "YlGn", tab = "Producer Accuracy") 
p8 <- ggplot.func(data.i =         Low.producer.df[,-2], i = 11, palette = "YlOrBr", tab = "Producer Accuracy") 

## map together
# setwd("/Users/Yi-Min/Rsession/ScenicOrNot/MGWRonScenicness")
# png(filename = "GWR_MGWR_wilderness.png", w = 10*2, h = 6*2, units = "in", res = 300)
par(mar = c(0,0,0,0))
multiplot(list(p1, p2, p3, p4), cols = 4)
par(mar = c(0,0,0,0))
multiplot(list(p5, p6, p7, p8), cols = 4)
# dev.off()

### inset map
st_bbox(Wales)
roi = st_bbox(c(xmin = 292000, xmax = 353000,
                ymin = 290000, ymax = 350000),
              crs = st_crs(Wales)) %>%
      st_as_sfc()

Outstanding.producer.df %>%
  st_as_sf(coords=c("coords.x1","coords.x2"), crs=27700) ->
  Outstanding.producer.sf

High.producer.df %>%
  st_as_sf(coords=c("coords.x1","coords.x2"), crs=27700) ->
  High.producer.sf

Moderate.producer.df %>%
  st_as_sf(coords=c("coords.x1","coords.x2"), crs=27700) ->
  Moderate.producer.sf

Low.producer.df %>%
  st_as_sf(coords=c("coords.x1","coords.x2"), crs=27700) ->
  Low.producer.sf

# require(tmap)
# Outstanding.producer_Wales = 
#   tm_shape(Wales) + 
#   tm_polygons(col = "grey") +
#   tm_shape(Outstanding.producer.sf) + 
#   tm_symbols("Outstanding", shape = 19, style = "cont", pal = "GnBu", size = 0.1) +
#   tm_shape(roi) + 
#   tm_borders(col = "red", lwd = 1, lty = "solid", alpha = NA, group = NA) +
#   # tm_scale_bar(position = c("left", "bottom")) +
#   tm_layout(frame = TRUE)
# 
# Outstanding.producer_map = 
#   tm_shape(roi) + 
#   tm_borders(col = "grey", lwd = 1, lty = "solid", alpha = NA, group = NA) + 
#   tm_shape(Outstanding.producer.sf[roi,]) + 
#   tm_symbols("Outstanding", shape = 19, style = "cont", pal = "GnBu", size = 0.3)

library(cowplot)
library(grid)
require(rcartocolor)

Outstanding.producer_map
print(Outstanding.producer_Wales, vp = viewport(0.8, 0.27, width = 0.3, height = 0.4))

ggm1 = ggplot() + 
  geom_sf(data = Wales, fill = "white") + 
  geom_sf(data = roi, fill = NA, color = "red", size = 1) +
  theme_void()

ggm2 = ggplot() + 
  geom_sf(data = LCA[which(LCA$VS_46 == "Outstanding"), ], fill = NA) +
  geom_sf(data = Outstanding.producer.sf, aes(col = Outstanding)) +
  geom_sf(data = Outstanding.producer.sf[which(Outstanding.producer.sf$Expert == 1),], shape = 1) + 
  geom_sf(data = Outstanding.producer.sf[which(Outstanding.producer.sf$Nonexpert.1 == 1), ], 
          shape = 3, 
          col = "red") +
  geom_sf_text(data = Outstanding.producer.sf[which(Outstanding.producer.sf$Nonexpert.1 == 1), ],
               aes(label = class),
               # position = "identity",
               nudge_x = 2000,
               nudge_y = 1000) +
  # geom_text_repel(data = Outstanding.producer.sf[which(Outstanding.producer.sf$Nonexpert.1 == 1), ],
  #                 aes(label = class), 
  #                     # x = st_coordinates(geometry)[1],
  #                     # y = st_coordinates(geometry)[2]),
  #                 arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "first"),
  #                 direction = "y", 
  #                 xlim  = 1000) +
  
  # geom_text_repel(
  #   nudge_x      = -0.35,
  #   direction    = "y",
  #   hjust        = 1,
  #   segment.size = 0.2
  # )
  # labs(title  = "class") +
  # labs(title = "")
  #                 aes(label = class, x = 200, y = 100)) +
  # geom_text_repel() + labs(title = "geom_text_repel()")
  coord_sf(st_bbox(roi)[c(1,3)], st_bbox(roi)[c(2,4)]) + 
  # scale_fill_carto_c(palette = "Mint") +
  scale_colour_distiller(type = "seq", 
                         palette = "GnBu", 
                         name = NULL, 
                         direction = 1,
                         guide = guide_colourbar(title.position = "top",
                                                 title.hjust = 0.5,
                                                 title.vjust = 0.8,
                                                 barwidth = 15)) +
  # theme_void() +
  # theme(legend.position = c(0.4, 0.05),
  #       legend.direction = "horizontal",
  #       legend.key.width = unit(10, "mm"))
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
  
gg_inset_map1 = ggdraw() +
  draw_plot(ggm2, x = 0.2, y = 0.2,) +
  draw_plot(ggm1, x = 0, y = 0, width = 0.3, height = 0.5)

gg_inset_map1
ggm2



#data <- data.frame(test.expert)
data <- 
  Outstanding.producer.sf[roi,] %>% 
  dplyr::select(image_id, SQ, Layman, mean, class, title, tags, comment, Expert, Nonexpert = Nonexpert.1) %>%
  st_drop_geometry() %>%
  dplyr::filter(Nonexpert == 1 & Expert == 0)
dim(data)

require(magrittr)
test <- Outstanding.producer.sf
test  %<>% st_drop_geometry()

data <- 
  High.producer.sf[which(High.producer.sf$High < 0.5), ] %>%
  dplyr::select(image_id, SQ, Layman, mean, class, title, tags, comment, Expert, Nonexpert = Nonexpert.1) %>%
  st_drop_geometry() %>%
  dplyr::filter(Nonexpert == 1 & Expert == 0)

# data <- mutate(data, 'text' = paste(title, comment)) 
# function to clean text removing markup
# remove_HTML_markup <- function(s) tryCatch({
#   doc <- htmlTreeParse(paste("<!DOCTYPE html>", s),
#                        asText = TRUE, trim = FALSE)
#   xmlValue(xmlRoot(doc))
# }, error = function(s) s)

##### This has changed! create a clean, stemmed, corpus 
corpus <- VCorpus(VectorSource(data[, 5]))  # sapply(data[, 8], remove_HTML_markup)
corpus <- tm_map(corpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# corpus <- tm_map(corpus, removeWords, c("social media","information","data","paper",
#                                         "research","study","approach","analysis",
#                                         "result","also", "springer", "elsevier",
#                                         "among", "user", "throughout", "copyright"))
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus <- tm_map(corpus, stripWhitespace)
#corpus <- tm_map(corpus, PlainTextDocument)
#corpus <- Corpus(VectorSource(corpus))

##### Figure: - create word cloud for ALL the data
# create document term matrix 
# simple implementation: dtm <- DocumentTermMatrix(corpus)
dtm <- DocumentTermMatrix(corpus, control = list(stemming = TRUE, 
                                                 stopwords = TRUE, 
                                                 wordLengths = c(4, 20), 
                                                 removeNumbers = TRUE, 
                                                 removePunctuation = TRUE))  

m <- as.matrix(dtm)  # convert the DTM into a mathematical matrix 
dim(m)
# https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/#cut_to_chase???????
# calculate the frequency of occurrence of each word in the corpus
v <- sort(colSums(m), decreasing = TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
wordcloud(d$word, d$freq, min.freq= 2, random.order = FALSE, rot.per = 0.35, color = brewer.pal(8, "Dark2")) # change min.freq


##### Figure: Single wordcloud plot for EACH YEAR #######################################################
# set up loop functions for individual wordclouds ****************************
create.corpus <- function(data) {                      # create corpus functional programming
  # remove_HTML_markup <- function(s) tryCatch({        
  #   doc <- htmlTreeParse(paste("<!DOCTYPE html>", s), 
  #                        asText = TRUE, trim = FALSE)
  #   xmlValue(xmlRoot(doc))
  # }, error = function(s) s)    
  
  # create clean, stemmed, corpus
  corpus <- VCorpus(VectorSource(data[, 5])) #sapply(data[, 1], remove_HTML_markup)
  # corpus <- tm_map(corpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  # corpus <- tm_map(corpus, removePunctuation)
  # corpus <- tm_map(corpus, removeWords, stopwords("english"))
  # corpus <- tm_map(corpus, removeWords, c("social media","information","data","paper",
  #                                         "research","study","approach","analysis",
  #                                         "result","also", "springer", "elsevier",
  #                                         "among", "user", "throughout", "copyright"))
  # corpus <- tm_map(corpus, stemDocument, language = "english")                       
  corpus <- tm_map(corpus, stripWhitespace)
  #corpus <- tm_map(corpus, PlainTextDocument)                  # Converting to Plain Text Documents ?
  return(corpus) 
}
#*****************************************************************

unique(data[,'SQ']) %>%
  .[order(., decreasing = F)] %>%
  as.character() ->
  SQ

# data <- mutate(data, LaymanCut = cut(Mean, c(1,2,3,4,5,6,7,8,9,10)))
# years <- unique(data[, 'LaymanCut'])
# years <- years[order(years, decreasing = F)] 
# years <- as.character(years)

# writes to a png file
png("WC_years.png", width = 8, height = 3, units = 'in', res = 300)  ### res --> nominal resolution in ppi
par(mfrow = c(1, length(SQ)))   # change the layout of wordcloud plots for all years
for (i in 1: length(SQ)) {
  index <- data[,2] == SQ[i]             
  data.i <- data[index,]                    
  corpus.i <- create.corpus(data.i)         
  
  dtm <- DocumentTermMatrix(corpus.i, control = list(stemming = FALSE, 
                                                     stopwords = TRUE, 
                                                     wordLengths = c(4, 20), 
                                                     removeNumbers = TRUE, 
                                                     removePunctuation = TRUE)) 
  m <- as.matrix(dtm)
  # https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/#cut_to_chase
  v <- sort(colSums(m), decreasing = TRUE)  
  myNames <- names(v)                       
  d <- data.frame(word = myNames, freq=v)    
  par(mar = c(9, 0.25, 0.25, 0.5))
  # c(bottom, left, top, right) 
  p <- wordcloud(d$word, 
                 d$freq, 
                 max.words = 100, 
                 rot.per = 0.35, 
                 color = brewer.pal(8, "Dark2"), #brewer.pal(8,"Set3"),
                 random.order = FALSE) # use.r.layout = T, scale=c(2,.25),	
  par(adj = 0.5)
  title(main = "", sub = list(SQ[i], cex = 1, col = "red", font = 4), line = 0)
}
dev.off()

##### Part 2 Part 2 Part 2 Part 2 Part 2- Text Mining analysis
# cluster data around years to be able look at changes and links topics
# reformat the data

res = matrix(nrow = length(SQ), ncol = 2)
for (i in 1: length(SQ)) {
  index <- data[,2] == SQ[i]
  data.i <- data[index,]
  data.i <- paste0(data.i[,'class'], collapse = " ")
  res[i,] <- cbind(data.i, SQ[i])
}

# overwite the corpus and dtm defined above with the reformatted data
corpus <- VCorpus(VectorSource(res[, 1])) #sapply(res[, 1], remove_HTML_markup)
#corpus <- tm_map(corpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
#corpus <- Corpus(VectorSource(corpus))
corpus <- tm_map(corpus, content_transformer(tolower))
# corpus <- tm_map(corpus, removeNumbers)
# corpus <- tm_map(corpus, removePunctuation)
# corpus <- tm_map(corpus, removeWords, stopwords("english"))  
# corpus <- tm_map(corpus, removeWords, c("social media","information","data","paper",
#                                         "research","study","approach","analysis",
#                                         "result","also", "springer", "elsevier",
#                                         "among", "user", "throughout", "copyright"))
# corpus <- tm_map(corpus, stemDocument, language = "english")                       
corpus <- tm_map(corpus, stripWhitespace)
#corpus <- tm_map(corpus, PlainTextDocument)
##############################################################################################

# create document term matrix 
dtm <- 
  DocumentTermMatrix(corpus, control = list(stemming = FALSE,
                                            stopwords = TRUE,
                                            #wordLengths = c(4, 20), 
                                            removeNumbers = TRUE, 
                                            removePunctuation = TRUE))

library(ldatuning)
# optimal topic number
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 50, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result)

library(topicmodels)
#dtm <- DocumentTermMatrix(corpus)
##### Do LDA
# Normally run the LDA as below. But for speed and consistency load the data created earlier
# when you are happy with what is being done below, you could try running yourself
##### I changed this to 5 based on the probabilities that were coming out below
##### This is not an exact science
k = 4 # arbitrary number of topics (there are ways to optimise this) 
TM <- LDA(dtm, k, method = "Gibbs", control = list(iter = 2000, seed = 622)) # make topic model
# 
# save(TM, file = "TM.RData")
# load("TM.RData")

# make data frame where rows are documents, columns are topics and cells
# are posterior probabilities of topics  
topic_df <- setNames(as.data.frame(TM@gamma),  paste0("T_",1:k)) 
# add row names that link each document to a human-readible bit of data
# in this case we'll just use a few words of the title of each paper 
## Table 1
mytable <- terms(TM, 50)
write.csv(mytable, "TermsTopicsTable.csv")

# # working with theta
# theta <- as.data.frame(posterior(TM)$topics)
# x <- as.data.frame(row.names(theta), stringsAsFactors = FALSE)
# colnames(x) <- c("SQ")
# x$SQ <- as.numeric(x$SQ)
# theta2 <- cbind(x, theta)
# theta2 <- left_join(theta2, FirstCategorybyLesson, by = "SQ")
# # Returns column means grouped by category
# theta.mean.by <- by(theta2[, 2:28], theta2$Category, colMeans)
# theta.mean <- do.call("rbind", theta.mean.by)
# 
# library(corrplot)
# c <- cor(theta.mean)
# corrplot(c, method = "circle")

##### Figure: Topic term probability / explanation and grouping of topics
# excellent site - http://stats.stackexchange.com/questions/63026/topic-stability-in-topic-models
# some discussion http://stats.stackexchange.com/questions/37405/natural-interpretation-for-lda-hyperparameters
### want to generate a table with the font size peroprtationa to the post prob
require(gplots)
post <- posterior(TM)
post <- post[[1]]
post[, 1:3]
term.no <- k 
t.mat <- terms(TM, term.no)  
topic.term.prob <- matrix(ncol = k, nrow = term.no) 
for (i in 1:k) { 
  for (j in 1: term.no) {
    term <- t.mat[i,j]  
    term.index <- match(term, colnames(post))
    topic.term.prob[i,j] <- post[j, term.index]	
  }
}
colnames(topic.term.prob) <- colnames(t.mat)
if (.Platform$GUI == "AQUA") {
  quartz(h = 4.5, w = 10) } else  {
    x11( h = 4.5, w = 10) }
tmp <- brewer.pal(9,"Blues")
tmp <- tmp[c(2:8)]
heatmap.2(x = zapsmall(topic.term.prob, 2), Rowv = F, Colv = T, dendrogram = "column",
          cellnote = t.mat, notecex = 1.5, notecol = "black",
          trace = "none", key = T, density.info = "none", col=tmp, margins = c(7, 3))
png(paste("heatmapTermsTopics_",k,"Top.png"), width = 20, height = 9, units = 'in', res = 300)
heatmap.2(x = zapsmall(topic.term.prob, 2), Rowv = F, Colv = T, dendrogram = "column",
          cellnote = t.mat, notecex = 1.5, notecol = "black",
          trace = "none", key = T, density.info = "none", col=tmp, margins = c(7, 3))
dev.off()

########### Figure: plot the connectedness of topics and years 
### now calulate distances between topics
rownames(topic_df) <- SQ          
res = matrix(nrow = 0, ncol = 3) 
for (i in 1:length(SQ)) {
  for (j in 1:k) {
    #if (zapsmall(topic_df[i, j], 0) > 0) {
    if (topic_df[i, j] > min(topic_df[i,]) + sd(topic_df[i,])) {  
      col.1 <- as.character(rownames(topic_df[i,]))    
      col.2 <- as.character(colnames(topic_df)[j])
      res <- rbind(res, cbind(col.1, col.2, topic_df[i, j]))
    }
  }
}
colnames(res) <- c("SQ", "Topic", "Weight")
# example plot
require(igraph)
g2 <- graph.edgelist(res[,1:2])
layout2 <- layout.fruchterman.reingold(g2, niter=100000, dim = 2)
plot(g2, layout=layout2, edge.curved = TRUE, vertex.size = 12,  edge.arrow.size = 0.4)
# enter V(g2) to see them and then adjust the lists that are assigned the colour "tomato"
V(g2)$shape <- "circle"
names(V(g2))
V(g2)$shape[grep("T_", names(V(g2)))] <- "csquare"
V(g2)$color <- "tomato"
V(g2)$color[grep("T_", names(V(g2)))] <- "yellow"
V(g2)$label.cex = 0.9
# assign edge weights from the matrix 'res' created above
E(g2)$width <- as.numeric(res[,3]) * 3
par(mar = c(0,0,0,0))
plot.igraph(g2, layout = layout2, 
            edge.arrow.size = 0.6, 
            edge.curved = TRUE, 
            edge.color = "black")

# plot(g2, layout = layout2, 
#             edge.arrow.size = 0.6, 
#             edge.curved = TRUE, 
#             edge.color = "black", 
#             edge.width = E(g2)$width*100)

# write.graph(g2, file="yearstopics.graphml", format="graphml") 
png(paste("TopicYears_",k,"Top.png"), width = 8, height = 8, units = 'in', res = 300)
par(mar = c(0,0,0,0))
plot.igraph(g2, layout=layout2, edge.arrow.size = 0.6, edge.curved = TRUE, edge.color = "black")
dev.off()

##### Figure: how similar are different years
# Change row values to zero if less than row minimum plus row standard deviation
# This is how Jockers subsets the distance matrix to keep only 
# closely related documents and avoid a dense spagetti diagram 
# that's difficult to interpret (hat-tip: http://stackoverflow.com/a/16047196/1036500)
# in this case a fudge factor of -0.05 has been included   
topic_df <- setNames(as.data.frame(TM@gamma),  paste0("T_",1:k))

topic_df_dist <- as.matrix(daisy(topic_df, metric = "euclidean", stand = TRUE))  ## Euclidean distance matrix http://stackoverflow.com/questions/16004847/visualise-distances-between-texts ??????????????????????????????????????????????????Lex?????????????????????????????????????????????????????
# Change row values to zero if less than row minimum plus row standard deviation
# This is how Jockers subsets the distance matrix to keep only 
# closely related documents and avoid a dense spagetti diagram 
# that's difficult to interpret (hat-tip: http://stackoverflow.com/a/16047196/1036500)
topic_df_dist[ sweep(topic_df_dist, 1, (apply(topic_df_dist,1,min) + apply(topic_df_dist,1,sd) -0.05 )) > 0 ] <- 0

####### Visualization
g <- as.undirected(graph.adjacency(topic_df_dist, weighted = TRUE))
E(g)$width <- E(g)$weight * 0.8
V(g)$label.cex = 0.9
par(mar = c(0,0,0,0))
layout1 <- layout_nicely(g, dim = 2)
plot(g, layout=layout1, edge.curved = TRUE, edge.color = "black", vertex.color= "yellow", edge.arrow.size = 0.4,  vertex.label = years)
png(paste("Year2YearSim_",k,"Top.png"), w = 4.5, height = 4.5, units = 'in', res = 300)
par(mar = c(0,0,0,0))
layout1 <- layout_nicely(g, dim = 2)
plot(g, layout=layout1, edge.curved = TRUE, edge.color = "black", vertex.color= "yellow", edge.arrow.size = 0.4,  vertex.label = years)
dev.off() 

#### New TF.IDF here
# remake corpus but with concatenated words
# consider stemming: "Ambient geographic information" --> "ambient geograph information",

# tf.idf? from http://www.tfidf.com
# Tf-idf stands for term frequency-inverse document frequency, and the tf-idf weight is a weight often used in information retrieval and text mining. This weight is a statistical measure used to evaluate how important a word is to a document in a collection or corpus. The importance increases proportionally to the number of times a word appears in the document but is offset by the frequency of the word in the corpus. Variations of the tf-idf weighting scheme are often used by search engines as a central tool in scoring and ranking a document's relevance given a user query.
# Typically, the tf-idf weight is composed by two terms: the first computes the normalized Term Frequency (TF), aka. the number of times a word appears in a document, divided by the total number of words in that document; the second term is the Inverse Document Frequency (IDF), computed as the logarithm of the number of the documents in the corpus divided by the number of documents where the specific term appears. See below for a simple example:
# Consider a document containing 100 words wherein the word cat appears 3 times. The term frequency (i.e., tf) for cat is then (3 / 100) = 0.03. Now, assume we have 10 million documents and the word cat appears in one thousand of these. Then, the inverse document frequency (i.e., idf) is calculated as log(10,000,000 / 1,000) = 4. Thus, the Tf-idf weight is the product of these quantities: 0.03 * 4 = 0.12.
# Consider a document (2013) containing 277630 words wherein the word 'citizen science' appears 182 times. The term frequency (i.e., tf) for 'citizen science' is then (182 / 277630) = 0.0006555488. Now, assume we have 24 documents and the word 'citizen science' appears in 16 of these. Then, the inverse document frequency (i.e., idf) is calculated as log(24 / 16) = 0.1760913. Thus, the Tf-idf weight is the product of these quantities: 0.0006555488 * 0.1760913 = 0.0001154364.


# from http://stackoverflow.com/questions/3548090/facet-grid-problem-input-string-1-is-invalid-in-this-locale
# log10(10000000 / 1000)

# word.list <- c("Ambient geographic information", "Citizen science", "Collaborative mapping", "Collaboratively contributed geographic information", "Contributed Geographic Information", "Crowdsourcing", "Extreme citizen science", "Geocollaboration", "Geographic citizen science", "GeoWeb", "GeoSpatialWeb", "Geographic World Wide Web", "Involuntary geographic information", "Map Hacking", "Map Hacks", "Mashup", "Neogeography", "Participatory sensing", "Public participation in scientific research", "Science 2.0", "Ubiquitous cartography", "User generated content", "Volunteered Geographic Information", "Wikinomics")

# word.list2 <- c("Ambient_geographic_information","Citizen_science","Collaborative_mapping","Collaboratively_contributed_geographic_information","Contributed_Geographic_Information","Crowdsourcing","Extreme_citizen_science","Geocollaboration","Geographic_citizen_science","GeoWeb","GeoSpatialWeb","Geographic_World_Wide_Web","Involuntary_geographic_information","Map_Hacking","Map_Hacks","Mashup","Neogeography","Participatory_sensing","Public_participation_in_scientific_research","Science_2.0","Ubiquitous_cartography","User_generated_content","Volunteered_Geographic_Information","Wikinomics")

word.list3 <- c("Facebook", "Youtube", "Instagram", "Twitter", "Reddit", "Vine", "Pinterest", "Ask.fm", "Tumblr", "Flickr", "Google+", "LinkedIn", "Vk", "ClassMates", "Meetup") 

word.list4 <- c("Facebook", "Youtube", "Instagram", "Twitter", "Reddit", "Vine", "Pinterest", "Ask.fm", "Tumblr", "Flickr", "Google+", "LinkedIn", "Vk", "ClassMates", "Meetup")

# word.list3 <- c("Flood", "Earthquake", "Hurricane", "Typhoon", "Tsunami", "Drought", "Landslides", "Hail", "Wildfire", "Tornadoes", "Volcano", "Nuclear", "Chemical", "Explosion", "Radioactive", "Famine") 

# word.list4 <- c("Flood", "Earthquake", "Hurricane", "Typhoon", "Tsunami", "Drought", "Landslides", "Hail", "Wildfire", "Tornadoes", "Volcano", "Nuclear", "Chemical", "Explosion", "Radioactive", "Famine")

# word.list3 <- c("Geotagged", "Geographic", "Geolocated", "Localized", "", "Drought", "Landslides", "Hail", "Wildfire", "Tornadoes", "Volcano", "Nuclear", "Chemical", "Explosion", "Radioactive", "Famine") 

# word.list4 <- c("Flood", "Earthquake", "Hurricane", "Typhoon", "Tsunami", "Drought", "Landslides", "Hail", "Wildfire", "Tornadoes", "Volcano", "Nuclear", "Chemical", "Explosion", "Radioactive", "Famine")

# extract data
years <- unique(data[,2])
years <- years[order(years, decreasing = F)]
nyears <- length(years)
document.year = matrix(nrow = nyears, ncol = 2)
for (i in 1: nyears) {
  year <- years[i]
  index <- data[,2] == year
  data.i <- data[index,]
  data.i <- paste0(data.i[,1], collapse = " ")
  document.year[i,] <- cbind(data.i, year)
}
# substitute desired terms
for (i in 1:length(word.list3)) {
  old <- word.list3[i]
  new <- word.list4[i]
  for (j in 1: nyears){
    document.year[j,1] <- gsub(old, new, document.year[j,1], ignore.case = T)
  }
  cat(i, "\t")
}
# create a frequency matrix
freq.m = matrix(ncol = nyears, nrow = length(word.list3))
for (i in 1:length(word.list4)) {
  term <- word.list4[i]
  freq.m[i,] <- sapply(gregexpr(term,document.year[,1],ignore.case = T), function(x)if(x[[1]]!=-1) length(x) else 0)
}
colnames(freq.m) <- document.year[,2]
rownames(freq.m) <- word.list3
# determine total number of terms in each document / year
# Sys.setlocale(locale="C") 
tot.words <- matrix(ncol=1, nrow = 0)
for (i in nyears:1) {
  tmp <- gsub(' {2,}',' ',document.year[i,1])
  tot.words <- rbind(as.vector(length(strsplit(as.vector(tmp),' ')[[1]])), tot.words)
}
colnames(tot.words) <- "Total"
# caluclate tf.idf
nterms = length(word.list4)
tf.idf <- matrix(ncol = nyears, nrow = nterms)
for (i in 1:nterms) {
  for (j in 1:nyears) {
    tf <- freq.m[i,j] / tot.words[j]
    idf <- log10(nyears / sum(freq.m[i,] !=0))
    tf.idf[i,j] <- tf * idf
  }
}
index <- is.na(tf.idf)
tf.idf[index] <- 0
colnames(tf.idf) <- document.year[,2]
rownames(tf.idf) <- word.list3
zapsmall(tf.idf, 2)
# plot - from http://epub.wu.ac.at/3558/1/main.pdf

##### Figure: frequency of terms used 
# "social media" AND ("disaster" OR "hazard" OR  "emergency")
if (.Platform$GUI == "AQUA") {
  quartz(h = 5, w = 7) } else  {
    x11( h = 5, w = 7) }
par(mar = c(0.25, 0.25, 0.25, 0.25))
levelplot(t(freq.m), col.regions = gray(20:0/20), 
          cuts = 15, xlab = NULL, ylab = NULL, 
          scales=list(x=list(cex=.5),y=list(cex=.8)), 
          panel = function (...) {
            panel.levelplot(...);
            # the grid:
            panel.abline(h = seq(1.5, 23.5), col = "lightgray");
            panel.abline(v = seq(1.5, 23.5), col = "lightgray");
          }
)
png("SM_disaster_hazard_emergency.png", w = 7, height = 4.5, units = 'in', res = 300)
par(mar = c(0.25, 0.25, 0.25, 0.25))
levelplot(t(freq.m), col.regions = gray(20:0/20), 
          cuts = 15, xlab = NULL, ylab = NULL, 
          scales=list(x=list(cex=.5),y=list(cex=.8)), 
          panel = function (...) {
            panel.levelplot(...);
            # the grid:
            panel.abline(h = seq(1.5, 23.5), col = "lightgray");
            panel.abline(v = seq(1.5, 23.5), col = "lightgray");
          })
dev.off()
##### Figure:  TFIDF figire of  
# "social media" AND ("disaster" OR "hazard" OR  "emergency")
if (.Platform$GUI == "AQUA") {
  quartz(h = 5, w = 7) } else  {
    x11( h = 5, w = 7) }
par(mar = c(0.25, 0.25, 0.25, 0.25))
levelplot(sqrt(t(tf.idf)), col.regions = gray(20:0/20), 
          cuts = 15, xlab = NULL, ylab = NULL, 
          scales=list(x=list(cex=.5),y=list(cex=.8), tck = 1), 
          panel = function (...) {
            panel.levelplot(...);
            # the grid:
            panel.abline(h = seq(1.5, 23.5), col = "lightgray");
            panel.abline(v = seq(1.5, 23.5), col = "lightgray");
          })
png("TfIdf_SM_disaster_hazard_emergency.png", w = 7, height = 4.5, units = 'in', res = 300)
par(mar = c(0.25, 0.25, 0.25, 0.25))
levelplot(sqrt(t(tf.idf)), col.regions = gray(20:0/20), 
          cuts = 15, xlab = NULL, ylab = NULL, 
          scales=list(x=list(cex=.5),y=list(cex=.8), tck = 1), 
          panel = function (...) {
            panel.levelplot(...);
            # the grid:
            panel.abline(h = seq(1.5, 23.5), col = "lightgray");
            panel.abline(v = seq(1.5, 23.5), col = "lightgray");
          })
dev.off()


### END


