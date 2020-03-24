# Hierarchical clustering

flight <- read.csv(file.choose())
View(flight)
# Normalizing continuous columns to bring them under same scale
normalized_data<-scale(flight[,-1]) #excluding the ID column before normalizing
?dist
d <- dist(normalized_data, method = "euclidean") # distance matrix
?hclust
fit <- hclust(d, method="complete")
?hclust
plot(fit) # display dendrogram
plot(fit, hang=-2)

?cutree
rect.hclust(fit, k=3, border="red")
?rect.hclust
groups <- cutree(fit, k=5) # cut tree into 5 clusters

install.packages("pvclust")
library(pvclust)
fit <- pvclust(flight, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(flight, membership)

View(final)

write.csv(final, file="final.csv",row.names = F)

aggregate(flight[,-1],by=list(final$membership),mean)



plot(flight)
plot(normalized_data)

km <- kmeans(flight,4) #kmeans clustering - 4 clusters; k ~ sqrt(n/2)
#install.packages("animation")
library(animation)

km <- kmeans.ani(flight, 4)
km$centers

km_8 <- kmeans.ani(flight,8)
km$centers

for (x in 1:20) 
  {
  km <- kmeans(normalized_data,4)
  wss<-(nrow(normalized_data)-1)*sum(apply(normalized_data,2,var))
  for(i in 2:15) wss[i]<-sum(kmeans(normalized_data,centers = i)$withinss)
 
  
}
km <- kmeans.ani(normalized_data, 4)
km_8 <- kmeans.ani(normalized_data,8)
plot(1:15,wss,type = "b", xlab = "No of clusters",ylab = "Avg distance")
