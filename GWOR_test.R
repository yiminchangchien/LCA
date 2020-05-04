library(maxLik)
library(MASS)
library(maptools)
library(geoR)
library(sp)
library(rgdal)
library(gstat)

source("/Users/Yi-Min/Rsession/LANDMAP/supporting materials/orderedfit.R")
source("/Users/Yi-Min/Rsession/LANDMAP/supporting materials/bw.gwordered.R")
source("/Users/Yi-Min/Rsession/LANDMAP/supporting materials/gwr.ordered.R")
source("/Users/Yi-Min/Rsession/LANDMAP/supporting materials/MonteCarlo.R")


## Simulate 400 spatial points
coords_x <- seq(0.1, 2, 0.1)
coords_y <- seq(0.1, 2, 0.1)

coords <- expand.grid(coords_x, coords_y)
n.total <- nrow(coords)
## Convert to a SpatialPoints object for visualisation purpose only!
grid <- SpatialPoints(coords)
grid$obs <- 1:nrow(coords)
plot.pixel <- SpatialPixels(grid)
## Create two vectors of local coefficients for x1 and x2
coe_x1 <- grf(n.total, grid@coords, cov.model = "gaussian", cov.pars = c(1, 2))$data
coe_x1 <- coe_x1 - mean(coe_x1)
coe_x2 <- grf(n.total, grid@coords, cov.model = "gaussian", cov.pars = c(0.8, 2))$data
coe_x2 <- coe_x2 - mean(coe_x2)
coe_mat <- cbind(coe_x1, coe_x2)
## Creat independent variables x1 and x2
X <- matrix(runif(2*n.total,1,10), n.total)
## Calculate the linear predictor or the latent variable
eta <- rowSums(X * coe_mat) + rlogis(n.total)
## The cut points are assumed to be the quantiles of the linear predictor
cut_points <- as.numeric(quantile(eta))
## Get the ordinal categorical responses
y <- as.integer(cut(eta, breaks = cut_points, include.lowest = TRUE))

# true coefficient surface on \beta_2
spplot(plot.pixel, c("coe_x2_true"))

## The data
grid@data <- data.frame(x1 = X[,1], x2 = X[,2], y=factor(y))
## Calcuate the distance matrix only once
d_Mat <- spDists(grid@coords)
## Model formula
formula <- y ~ x1 + x2
## Calibrate the optimum bandwidth
bw.fixed <- bw.gwordered(formula,
data=grid,
kernel="bisquare",
adaptive=FALSE,
link="logit",
dMat=d_Mat,
fixed.vars = NULL
)

## Estimate local coefficients
res.fixed <- gwr.ordered(formula,
data=grid,
kernel="bisquare",
adaptive=FALSE,
bw=0.599,
link="logit",
dMat=d_Mat,
fixed.vars=NULL)
##