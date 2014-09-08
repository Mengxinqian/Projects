# Part A: PCA Analysis
#--reduce dimension and find the questions which give more weight in different pricipal components

#load data
ling.data <- read.table('binary-ling-data.data', header=T)
ling.responses <- ling.data[,7:ncol(ling.data)]

# dimension reduction using PCA
data.pca <- prcomp(ling.responses,tol=0.8)

# certain questions which give more weight in different pricipal components
index.frame <- apply(data.pca$rotation, 2, function(col){which(col >= sort(col, decreasing=T)[4], arr.ind=TRUE)})
index.frame <- data.frame(index.frame)
row.name <- rownames(data.pca$rotation)
PC1 <- row.name[index.frame$PC1]
PC2 <- row.name[index.frame$PC2]
PC3 <- row.name[index.frame$PC3]
weight.questions <- data.frame(cbind(PC1,PC2,PC3))


