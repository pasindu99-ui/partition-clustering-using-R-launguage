library(readxl)
library(magrittr)
library(ggplot2)
library(dplyr)
library(factoextra)
library(NbClust)
install.packages("clusGap", version = "1.4.4")

# Load data and select variables


vehicles <- read_excel("F:/digree/Second year/second sem/Machine Learning and Data Mining/coursework/vehicles.xlsx")
vehicles
vehicles_labels=vehicles$Class #class type
vehicle_data<-vehicles[2:19] #attributes
vehicle_data
summary(vehicle_data)

boxplot(vehicles[2:19]) #plot before normalizing data

vehicle_data <- as.data.frame(vehicle_data)

D <- c() # empty vector to store outliers in each column
for (i in 1:18) {
  x <- vehicle_data[,i] # access every column in data set
  y <- x %in% boxplot.stats(x)$out # records which data are outliers and which are not
  
  d <- which(y == TRUE) # record the outliers of the current column 
  D <- c(D,d) # add every set of outliers to a vector which stores all outliers
}



# print the indices of the outliers in each column
print(D)

eleminate <- unique(D) # remove the duplicated rows
eleminate <- sort(eleminate) # sort the outlire rows in order

vehicle_data <- vehicles[-eleminate,] # store the final rows after removing the outlires for attributes

normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

vehicle_data_norm <- as.data.frame(lapply(vehicle_data[,-20], normalize))# normalized data
scale_vehicle_data_norm <- data.frame(scale(vehicle_data_norm)) # scale the normalized data

boxplot(scale_vehicle_data_norm) #plot before normalizing data




# Manual Tools - calling k-means function 
kc <- kmeans(scale_vehicle_data_norm,4)
table(vehicle_data$Class,kc$cluster)
# Plotting the clusters for K-Means Output
library(ggpubr)
library(factoextra)
# Plot
fviz_cluster(kc, data = vehicle_data_norm,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#b800e7"),
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


#how many clusters need - elbow method
# Automated Tools - Elbow Method
k = 1:10 # number of possible clusters

set.seed(42)

WSS = sapply(k, function(k) {kmeans(scale_vehicle_data_norm, centers=k)$tot.withinss})

plot(k, WSS, type = "b", pch = 19, frame=FALSE, xlab= "Number of k", ylab="Within sum of squares")
fviz_nbclust(scale_vehicle_data_norm, kmeans, method = "wss")+
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

#NNBclust
nbcluster_no<-NbClust(scale_vehicle_data_norm,distance = "euclidean", 
        min.nc=2, max.nc=15, method = "kmeans", 
        index = "all")

#using faxtoextra
fviz_nbclust(scale_vehicle_data_norm,kmeans,method = "silhouette")

#gap statatic
set.seed(123)
gap <- clusGap(vehicle_data_scale, FUN = kmeans, nstart = 25, K.max = 10, B = 50)






#k-means 
km.out2<- kmeans(scale_vehicle_data_norm,2)
print(km.out2)

#visualize
fviz_cluster(km.out2, data = scale_vehicle_data_norm,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#b800e7"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

kc2_centers <- km.out2$centers
kc2_bss <- km.out2$betweenss
kc2_tss <- km.out2$tot.withinss
kc2_wss <- km.out2$withinss
bss_tss_ratio <- kc2_bss / kc2_tss
table(vehicle_data$Class,km.out2$cluster)#Confusion Matrix

cat("BSS:", kc2_bss, "\n")
cat("WSS:", kc2_wss, "\n")
cat("tSS:", kc2_tss, "\n")
cat("BSS/TSS Ratio:", bss_tss_ratio, "\n")

library(cluster)
library(ggplot2)

sil_width <- silhouette(km.out2$cluster, dist(scale_vehicle_data_norm))
plot(sil_width)

# Calculate the average silhouette width
avg_sil_width <- mean(sil_width[,3])
cat("Average silhouette width:", avg_sil_width)


#k-means 
km.out3<- kmeans(scale_vehicle_data_norm,3)
print(km.out3)

#visualize
fviz_cluster(km.out3, data = scale_vehicle_data_norm,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#b800e7"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

kc3_centers <- km.out3$centers
kc3_bss <- km.out3$betweenss
kc3_tss <- km.out3$tot.withinss
kc3_wss <- km.out3$withinss
bss_tss_ratio <- kc3_bss / kc3_tss
table(vehicle_data$Class,km.out3$cluster)#Confusion Matrix

cat("BSS:", kc3_bss, "\n")
cat("WSS:", kc3_wss, "\n")
cat("tSS:", kc3_tss, "\n")
cat("BSS/TSS Ratio:", bss_tss_ratio, "\n")

#k-means 
km.out4<- kmeans(scale_vehicle_data_norm,4)
print(km.out4)

#visualize
fviz_cluster(km.out4, data = scale_vehicle_data_norm,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#b800e7"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

kc4_centers <- km.out4$centers
kc4_bss <- km.out4$betweenss
kc4_tss <- km.out4$tot.withinss
kc4_wss <- km.out4$withinss
bss_tss_ratio <- kc4_bss / kc4_tss
table(vehicle_data$Class,km.out4$cluster)#Confusion Matrix

cat("BSS:", kc4_bss, "\n")
cat("WSS:", kc4_wss, "\n")
cat("tSS:", kc4_tss, "\n")
cat("BSS/TSS Ratio:", bss_tss_ratio, "\n")



#PCA 
#create new PCA data set

transformed_vehicle_data <- prcomp(scale_vehicle_data_norm, center = TRUE, scale. = TRUE) 
summary(transformed_vehicle_data)

transformed_vehicle_data$loadings[, 7:19]

new_transformed <- transformed_vehicle_data$x[, 7:19]
transformed_kmeans <- kmeans(new_transformed,2)
transformed_kmeans

fviz_cluster(transformed_kmeans, data = new_transformed,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#b800e7"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

transformed_kmeans_bss <- transformed_kmeans$betweenss
transformed_kmeans_tss <- transformed_kmeans$tot.withinss
transformed_kmeans_wss <- transformed_kmeans$withinss

fviz_eig(transformed_vehicle_data, addlabels = TRUE)

#how many clusters need - elbow method
fviz_nbclust(new_transformed,kmeans,method = "wss")+labs(subtitle = "elbow method")

#NNBclust
nbcluster_no<-NbClust(new_transformed,distance = "euclidean", 
                      min.nc=2, max.nc=15, method = "kmeans", 
                      index = "all")
fviz_nbclust(vehicle_data_scale,kmeans,method = "silhouette")


#k-means 
km.out2<- kmeans(new_transformed,2)
print(km.out2)

#visualize
fviz_cluster(km.out2, data = new_transformed,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#b800e7"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

kc2_centers <- km.out2$centers
kc2_bss <- km.out2$betweenss
kc2_tss <- km.out2$tot.withinss
kc2_wss <- km.out2$withinss
bss_tss_ratio <- kc2_bss / kc2_tss
table(vehicles$Class,km.clusters)#Confusion Matrix

cat("BSS:", kc2_bss, "\n")
cat("WSS:", kc2_wss, "\n")
cat("tSS:", kc2_tss, "\n")
cat("BSS/TSS Ratio:", bss_tss_ratio, "\n")

#k-means 
km.out3<- kmeans(new_transformed,3)
print(km.ou3t)

#visualize
fviz_cluster(km.out3, data = new_transformed,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#b800e7"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

kc3_centers <- km.out3$centers
kc3_bss <- km.out3$betweenss
kc3_tss <- km.out3$tot.withinss
kc3_wss <- km.out3$withinss
bss_tss_ratio <- kc3_bss / kc3_tss
table(vehicles$Class,km.clusters)#Confusion Matrix

cat("BSS:", kc3_bss, "\n")
cat("WSS:", kc3_wss, "\n")
cat("tSS:", kc3_tss, "\n")
cat("BSS/TSS Ratio:", bss_tss_ratio, "\n")

#k-means 
km.out4<- kmeans(new_transformed,4)
print(km.out4)

#visualize
fviz_cluster(km.out4, data = new_transformed,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#b800e7"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

kc4_centers <- km.out4$centers
kc4_bss <- km.out4$betweenss
kc4_tss <- km.out4$tot.withinss
kc4_wss <- km.out4$withinss
bss_tss_ratio <- kc4_bss / kc4_tss
table(vehicles$Class,km.clusters)#Confusion Matrix

cat("BSS:", kc4_bss, "\n")
cat("WSS:", kc4_wss, "\n")
cat("tSS:", kc4_tss, "\n")
cat("BSS/TSS Ratio:", bss_tss_ratio, "\n")


fviz_nbclust(scale_vehicle_data_norm, kmeans, method = "silhouette", k.max = 24) + theme_minimal() + 
  ggtitle("The Silhouette Plot")
install.packages("clValid")
library(clValid)
ch_score <- clusterCrit(scale_vehicle_data_norm, km.clusters, criterion = "ch")

