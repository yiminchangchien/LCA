library(dplyr)
library(rnrfa)
library(rvest)
library(xml2)


sc_tab1 <- openxlsx::read.xlsx("/Users/Yi-Min/Rsession/ScenicOrNot/ScenicOrNot dataset/ScenicOrNot_Geograph_v7.xlsx")

openxlsx::write.xlsx(sc_tab, "/Users/Yi-Min/Rsession/ScenicOrNot/ScenicOrNot dataset/ScenicOrNot_Geograph_v3.xlsx")

'https://www.geograph.org.uk/photo/342448' %>%
  as.character() %>%
  read_html() %>%
  html_nodes('body') %>%
  # xml_find_all("//div/div/div[contains(@itemprop, 'description')]") %>%
  # xml_find_all("//span/span[contains(@class, 'tag')]") %>%
  # xml_find_all("//h2/a") %>%
  xml_find_all("//span[contains(@itemprop, 'exifData')]") %>%
  html_text() -> tmp


readUrl <- function(url) {
  out <- tryCatch(
    {
      message("This is the 'try' part")
      url %>% 
        as.character() %>% 
        read_html() %>% 
        html_nodes('body') %>% 
        # xml_find_all("//div/div/div[contains(@class, 'caption640')]") %>% 
        # xml_find_all("//div/div/div[contains(@itemprop, 'description')]") %>%
        # xml_find_all("//div/div/div[contains(@about, 'description')]") %>%
        # xml_find_all("//span/span[contains(@class, 'tag')]") %>%
        # xml_find_all("//h2/a") %>%
        xml_find_all("//span[contains(@itemprop, 'exifData')]") %>%
        html_text() 
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", url))
      message("Here's the original error message:")
      message(cond)
      return(NA)
    }
  )
  return(out)
}

index <- which(is.na(sc_tab$imageclass))
index2 <- which(is.na(sc_tab$title))
index3 <- which(is.na(sc_tab$title))
index4 <- which(is.na(sc_tab$comment))
index5 <- which(is.na(sc_tab$tags))
index6 <- which(is.na(sc_tab$grid_reference))
index7 <- which(is.na(sc_tab$grid_reference))
index8 <- which(is.na(sc_tab$imagetaken))
length(index8)

for (i in 1:length(index8)){
  # print(i)
  sc_tab[index8[i], 'Geograph.URI'] %>%
    as.character() %>%
    readUrl() ->
    tmp

  # sc_tab[index4[i], 'title'] <- tmp[1]
  # if (length(tmp) > 1) {
  #   sc_tab[index4[i], 'comment'] <- tmp[2]
  #}
  
  if (length(tmp) == 0){
    sc_tab[index8[i], 'imagetaken'] <- ""
  } else {
    sc_tab[index8[i], 'imagetaken'] <- paste(tmp, collapse = ',')
  }
}

length(which(is.na(sc_tab$imagetaken)))

require(rgdal)
require(sf)
require(sp)
sc_tab %>%
  st_as_sf(coords=c("viewpoint_eastings","viewpoint_northings"), crs=27700) %>%
  as("Spatial") %>%
  writeOGR("/Users/Yi-Min/Rsession/ScenicOrNot/ScenicOrNot dataset", "ScenicOrNot_Viewpoint.shp", driver="ESRI Shapefile")

for (i in 1:dim(test)[1]){
  test[i, 'Geograph.URI'] %>%
    as.character() %>%
    readUrl() ->
    tmp
}


df = read.csv("~/dir/dir/points.csv", stringsAsFactors=F)

# -------------------------------------
# Get easting and northing

# If required, remove spaces in the NGR
df <- data.frame(gridimage_id = sc_tab$gridimage_id, grid_reference = sc_tab$grid_reference)

# Convert NGR to easting and northing
for (i in 187632:dim(sc_tab)[1]) {
  # i = 187631
  tmp <- osg_parse(sc_tab[i, 'grid_reference'])
  sc_tab[i, 'grid_east'] <- tmp[[1]]
  sc_tab[i, 'grid_north'] <- tmp[[2]]
}

df$eastings <- apply(sc_tab$grid_reference, MARGIN = 1, FUN = osg_parse) 

x = osg_parse(sc_tab[1,]$grid_reference)

# Extract easting and northing from the list, x
df$east = x[[1]]
df$north = x[[2]]

names(sc_tab) <- c("id", "Lat", "Lon", "votes", "median", "mean", 
                   "IQR", "variance", "URI", "image_id", "user_id", "name", 
                   "title", "class", "comment", "tags", "date", "status", 
                   "grd_ref", "grd_east", "grd_north", "sb_east", "sb_north", "sb_precise", 
                   "vp_east", "vp_north", "vp_precise", "angle", "use6fig", "x",
                   "y", "lat", "long", "ref_ind")

summary(factor(sc_tab$sb_precise))
summary(factor(sc_tab$vp_precise))

sc_tab$sb_precise <- plyr::mapvalues(sc_tab$sb_precise, from = c(4,6,8,10), to = c(1000, 100, 10, 1))
sc_tab$vp_precise <- plyr::mapvalues(sc_tab$vp_precise, from = c(0,4,6,8,10), to = c(2000, 1000, 100, 10, 1))

openxlsx::write.xlsx(sc_tab, "/Users/Yi-Min/Rsession/ScenicOrNot/ScenicOrNot dataset/ScenicOrNot_Geograph_v8.xlsx")








