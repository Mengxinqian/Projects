#Part B: mlogit Analysis
#--explore realtionship between different questions, as well as longitude and latitude

# According to Part A, we choose Q73, Q80 to analyze their relationship with geography and each other.
library(GISTools)
library(maps)
library(RColorBrewer)
ling.data <- read.table('binary-ling-data.data', header=T)
ling.responses <- ling.data[,7:ncol(ling.data)]

# map of Q73
set.seed(47)
samp.size <- 40000
sample <- list()
sample$idcs <- sample(1:nrow(ling.data), samp.size)
m <- which(colnames(ling.responses) == 'Q073.1')
n <- which(colnames(ling.responses) == 'Q074.1')
if(n-m <= 9){n.Q73 <- n-m}else{n.Q73 <- 9}
ling.responses.Q73 <- ling.responses[m:(n-1)]
sample$responses  <- ling.responses[sample$idcs,]
sample$coords <- ling.data[sample$idcs, c('long', 'lat')]

colors <- brewer.pal(n.Q73, 'Set1')
colors <-  add.alpha(colors, alpha=0.9) # add transparency
map('state', col='black', fill=F)
points(sample$coords, col=colors, pch=20, cex=.5)

# map of Q80
set.seed(47)
samp.size <- 40000
sample <- list()
sample$idcs <- sample(1:nrow(ling.data), samp.size)
m <- which(colnames(ling.responses) == 'Q080.1')
n <- which(colnames(ling.responses) == 'Q081.1')
if(n-m <= 9){n.Q80 <- n-m}else{n.Q80 <- 9}
ling.responses.Q80 <- ling.responses[m:(n-1)]
sample$responses  <- ling.responses[sample$idcs,]
sample$coords <- ling.data[sample$idcs, c('long', 'lat')]

colors <- brewer.pal(n.Q80, 'Set1')
colors <-  add.alpha(colors, alpha=0.9) # add transparency
map('state', col='black', fill=F)
points(sample$coords, col=colors, pch=20, cex=.5)

# mlogit analysis
#install.packages('mlogit')
library(mlogit)
ling.data.Q73 <- cbind(ling.data[,1:6],ling.responses.Q73)
col.name <- colnames(ling.responses.Q73)
mode = apply(ling.responses.Q73, 1, function(row) (col.name[which(row==1)]))
mode = unlist(mode)[1:1000]
ling.data.Q73.subset <- cbind(mode,ling.data.Q73[1:1000,], ling.responses.Q80[1:1000,])
mldata <- mlogit.data(ling.data.Q73.subset, choice='mode', shape = "wide")

#The relationship between Q73 and Q80, normized base on Q73.2
summary(mlogit(mode ~ 1 | Q080.1, data = mldata, reflevel = "Q073.2"))
#The relationship between Q73 and longitude and latitude, normized base on Q73.2
summary(mlogit(mode ~ 1 | lat, data = mldata, reflevel = "Q073.2"))
summary(mlogit(mode ~ 1 | long, data = mldata, reflevel = "Q073.2"))

# Visualize the prediction above on map

# map of Q73.6 which the coefficient is significant-the realtionship with Q80.1
indx <- ling.data.Q73$Q073.6 == 1
coords <- ling.data.Q73[indx, c('long', 'lat')]
map('state', col='black', fill=F)
points(coords, col='green', pch=20, cex=.5)

indx <- ling.data.Q73.subset$Q080.1 == 1
coords <- ling.data.Q73.subset[indx, c('long', 'lat')]
map('state', col='black', fill=F)
points(coords, col='pink', pch=20, cex=.5)

# map of Q73.3 which the coefficient is significant-the relationship with lantitude
indx <- ling.data.Q73$Q073.3 == 1
coords <- ling.data.Q73[indx, c('long', 'lat')]
map('state', col='black', fill=F)
points(coords, col='red', pch=20, cex=.5)

# map of Q73.1 which the coefficient is significant-the relationship with longtitude
indx <- ling.data.Q73$Q073.1 == 1
coords <- ling.data.Q73[indx, c('long', 'lat')]
map('state', col='black', fill=F)
points(coords, col='blue', pch=20, cex=.4)


