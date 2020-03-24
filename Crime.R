crime <- read.csv(file.choose())
View(crime)
# Normalizing continuous columns to bring them under same scale
normalized_data<-scale(crime[,-1]) #excluding the city names before normalizing
?dist
View(normalized_data)
d <- dist(normalized_data, method = "euclidean") # distance matrix
?hclust
fit <- hclust(d, method="complete")
?hclust
plot(fit) # display dendrogram
plot(fit, hang=-2)

?cutree
rect.hclust(fit, k=3, border="red")
?rect.hclust
groups <- cutree(fit, k=3) # cut tree into 5 clusters

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime, membership)

View(final)

write.csv(final, file="final.csv",row.names = F)

aggregate(crime[,-1],by=list(final$membership),mean)
