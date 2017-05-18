setwd("E:/Spring 2017/Advanced BA/Assignment_4")
getwd()
library(data.table)
data <- fread("hospital_ortho.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?","Inf"))
nc_data <- data[(data$state == "NC") | (data$state == "SC") | (data$state == "VA") | (data$state == "GA") | (data$state == "TN")]
nc_data <- na.omit(nc_data)

###Step 3
install.packages("cluster", repos = "https://cran.r-project.org")
install.packages("clustertend", repos = "https://cran.r-project.org")
install.packages("dbscan", repos = "https://cran.r-project.org")
head(nc_data)
is.na(nc_data)
str(nc_data)
summary(nc_data)
class(nc_data$hid)
lapply(nc_data, class)
nc_data$zip <- NULL
nc_data$hid<- NULL
nc_data$state<- NULL
nc_data$city <- NULL
nc_data$th <- NULL
nc_data$trauma <- NULL
nc_data$rehab <- NULL

head(nc_data)

colnames(nc_data)

if (TRUE){
  df <- scale(nc_data) # Standardize the data
} else{
  df <- nc_data 
}

head(df)

k.means.fit <- kmeans(df, 3)
attributes(k.means.fit)

k.means.fit$centers
k.means.fit$cluster

k.means.fit$size
library(clustertend)
hopkins(df, n = nrow(df)-1)


withinssplot <- function(df, nc=15, seed=456){
  wss <- (nrow(df)-1)*sum(apply(df,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(df, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
withinssplot(df, nc=10) 

## step 3.3

k.means.fit <- kmeans(df, 2)
k.means.fit$cluster
k.means.fit$size

## Step 3.4

library(cluster)
clusplot(df, k.means.fit$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)

###Step 4.1

d <- dist(df, method = "euclidean")
H.single <- hclust(d, method="single")
plot(H.single)

H.complete <- hclust(d, method="complete")
plot(H.complete)

H.average <- hclust(d, method="average")
plot(H.average)

H.ward <- hclust(d, method="ward.D2")
plot(H.ward)

par(mfrow=c(2,2))
plot(H.single)
plot(H.complete)
plot(H.average)
plot(H.ward)
par(mfrow=c(1,1))
##Step  4.4

groups <- cutree(H.ward, k=4)
plot(H.ward)
rect.hclust(H.ward, k=4, border="red") 

clusplot(df, groups, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

####Step 5

library(dbscan)
kNNdistplot(df, k =4)
abline(h=4, col="red")

data <- df[complete.cases(df),]
pca <- prcomp(data, center = TRUE, scale. = TRUE) # Variables will be zero-centered and will have unit variance in the PCA
print(pca)
summary(pca)

db <- dbscan(df, eps=4, minPts=4)
db
db$cluster

clusplot(df, db$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

### Step 6

data <- nc_data[complete.cases(nc_data),]
pca <- prcomp(data, center = TRUE, scale. = TRUE) # Variables will be zero-centered and will have unit variance in the PCA
print(pca)
summary(pca)

pca_data <- predict(pca, newdata = nc_data)
pc_df <- as.data.frame(scale(pca_data[,c(1:3)]))  # replace n_pc with the number of PCs you recommend
head(pc_df)
nrow(pc_df)
###Step 8.1

k.means.fit <- kmeans(pc_df, 3)
attributes(k.means.fit)

k.means.fit$centers
k.means.fit$cluster

k.means.fit$size
library(clustertend)
hopkins(df, n = nrow(pc_df)-1)


withinssplot <- function(pc_df, nc=15, seed=123){
  wss <- (nrow(pc_df)-1)*sum(apply(pc_df,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(pc_df, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
withinssplot(pc_df, nc=10) 


k.means.fit <- kmeans(pc_df, 3)
k.means.fit$cluster
k.means.fit$size

library(cluster)
clusplot(pc_df, k.means.fit$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)

### Step 6.2
d <- dist(pc_df, method = "euclidean")
H.single <- hclust(d, method="single")
plot(H.single)

H.complete <- hclust(d, method="complete")
plot(H.complete)

H.average <- hclust(d, method="average")
plot(H.average)

H.ward <- hclust(d, method="ward.D2")
plot(H.ward)

par(mfrow=c(2,2))
plot(H.single)
plot(H.complete)
plot(H.average)
plot(H.ward)
par(mfrow=c(1,1))

groups <- cutree(H.ward, k=4)
plot(H.ward)
rect.hclust(H.ward, k=4, border="red") 

clusplot(pc_df, groups, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

### Step 6.3

library(dbscan)
kNNdistplot(pc_df, k =4)
abline(h=1, col="red")


db <- dbscan(pc_df, eps=1, minPts=4)
db
db$cluster

clusplot(pc_df, db$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

### Step 7

pc_df$kmeans <- k.means.fit$cluster
pc_df $hclust <- groups # these groups are created in hierarchical clustering
pc_df $db <- db$cluster
pc_df $hid <- nc_data$hid # Add hospital id to pc_df data
colnames(nc_data)
final_data <- merge(x=pc_df, y=nc_data, key="hid")
aggregate(final_data[,c("sales12","rbeds","hip12","knee12","femur12")], list(final_data$kmeans), mean)
aggregate(final_data[,c("sales12","rbeds","hip12","knee12","femur12")], list(final_data$hclust), mean)
aggregate(final_data[,c("sales12","rbeds","hip12","knee12","femur12")], list(final_data$db), mean)

