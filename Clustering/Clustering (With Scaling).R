library(rgl)
library(dbscan)
library(ggplot2)
library(plotly)
library(mclust)

data2 <- read.csv("./BDH.csv", header = FALSE)
data<-data.frame(V1 = data2$V1/max(data2$V1), V2 = data2$V2/max(data2$V2), V3 = data2$V3/max(data2$V3))

#-----------K Means---------Deciding k----
sse<-c(1:10)
for (i in 1:10) {
  km <- kmeans(data, centers = i, iter.max = 100, nstart = 10)
  sse[i]=km$tot.withinss
}
plot(c(1:10), sse, xlab = "K Values", ylab = "SSE", main = "SSE vs K-Values")
sse_k<-plot_ly(x = c(1:10), y = sse, type = "scatter", mode = c("markers", "lines"))%>%layout(title = "SSE vs k values")

#-------K means---k=3---
km <- kmeans(data, centers = 3, iter.max = 100, nstart = 10)
col_c<-km$cluster
plot3d(data, col = col_c, pch =1)
bgplot3d({
  plot.new()
  title(main = 'K Means', line = 3)
})
rgl.points(km$centers[1,1], km$centers[1,2], km$centers[1,3], pch = 19, col = "Yellow")
rgl.points(km$centers[2,1], km$centers[2,2], km$centers[2,3], pch = 19, col = "Yellow")
rgl.points(km$centers[3,1], km$centers[3,2], km$centers[3,3], pch = 19, col = "Yellow")
k_p<-plot_ly(x = data$V1,y = data$V3, z=data$V2, type = 'scatter3d', mode = 'markers', color = col_c, marker = list(size=3))%>%layout(title = "K means with k = 3")


#-----Hierarchical Clustering---------
distance_matrix<-dist(data, method = "euclidean")
hc<-hclust(distance_matrix)
max_diff<-(-1)
delta<-0
for (i in 2:length(hc$height)) {
  if(hc$height[i]-hc$height[i-1] > max_diff){
    max_diff = hc$height[i]-hc$height[i-1]
    delta<-((hc$height[i] + hc$height[i-1])/2)
  }
}
plot(hc, xlab = "Points") #---dendrogram plot-----
abline(h=delta, col = "red")
clusterCut <- cutree(hc, h = delta)
#print(clusterCut)
h_rgl<-plot3d(data, col = clusterCut)
bgplot3d({
  plot.new()
  title(main = 'Hierarchical', line = 3)
})
h_p<-plot_ly(x = data$V1,y = data$V3, z=data$V2, type = 'scatter3d', mode = 'markers', color = clusterCut, marker = list(size=3))%>%layout(title = "Hierarchical")


#----------------DBSCAN--------------------
#---Deciding Epsilon-------for minPts=3
dist_matrix<-as.matrix(dist(data))
data_length<- length(data[,1])

r<-c(1:data_length)

for (i in 1:data_length) {
  r[i] = sort(dist_matrix[i,])[4]
}
r<-sort(r, decreasing = TRUE)
plot(c(1:625), r, main = "Distance vs data points for minPts 3", pch=".")
db_r_p<-plot_ly(x = c(1:625), y = r, type = "scatter", mode = c("markers"))%>%layout(title = "Distance vs Data Points for minPts=3")
db_r_p
grid(10,10)
abline(h=10, col="red")

dbs<-dbscan(data, 0.06, 3)
tmp<-(dbs$cluster)+1
dbs_p<-plot_ly(x = data$V1,y = data$V3, z=data$V2, type = 'scatter3d', mode = 'markers', color = dbs$cluster, marker = list(size=3))%>%layout(title = "DBSCAN : Epsilon = 0.06, minPts=3")
print(dbs_p)



for (i in 1:data_length) {
  r[i] = sort(dist_matrix[i,])[5]
}
r<-sort(r, decreasing = TRUE)
plot(c(1:625), r, main = "Distance vs data points for minPts 4")
dbs<-dbscan(data, 0.08, 4)
tmp<-(dbs$cluster)+1
dbs_p<-plot_ly(x = data$V1,y = data$V3, z=data$V2, type = 'scatter3d', mode = 'markers', color = dbs$cluster, marker = list(size=3))%>%layout(title = "DBSCAN : Epsilon = 0.08, minPts=4")
print(dbs_p)

for (i in 1:data_length) {
  r[i] = sort(dist_matrix[i,])[6]
}
r<-sort(r, decreasing = TRUE)
plot(c(1:625), r, main = "Distance vs data points for minPts 5")
dbs<-dbscan(data, 0.09, 5)
tmp<-(dbs$cluster)+1
dbs_p<-plot_ly(x = data$V1,y = data$V3, z=data$V2, type = 'scatter3d', mode = 'markers', color = dbs$cluster, marker = list(size=3))%>%layout(title = "DBSCAN : Epsilon = 0.09, minPts=5")
print(dbs_p)

for (i in 1:data_length) {
  r[i] = sort(dist_matrix[i,])[7]
}
r<-sort(r, decreasing = TRUE)
plot(c(1:625), r, main = "Distance vs data points for minPts 6")
dbs<-dbscan(data, 0.1, 6)
tmp<-(dbs$cluster)+1
dbs_p<-plot_ly(x = data$V1,y = data$V3, z=data$V2, type = 'scatter3d', mode = 'markers', color = dbs$cluster, marker = list(size=3))%>%layout(title = "DBSCAN : Epsilon = 0.1, minPts=6")
print(dbs_p)

for (i in 1:data_length) {
  r[i] = sort(dist_matrix[i,])[8]
}
r<-sort(r, decreasing = TRUE)
plot(c(1:625), r, main = "Distance vs data points for minPts 7")
dbs<-dbscan(data, 0.08, 7)
tmp<-(dbs$cluster)+1
dbs_p<-plot_ly(x = data$V1,y = data$V3, z=data$V2, type = 'scatter3d', mode = 'markers', color = dbs$cluster, marker = list(size=3))%>%layout(title = "DBSCAN : Epsilon = 0.08, minPts=7")
print(dbs_p)



#-----GMM clustering--------
mc<-Mclust(data, G=2)
mc_p<-plot_ly(x = data$V1,y = data$V3, z=data$V2, type = 'scatter3d', mode = 'markers', color = ((mc$classification)+30), marker = list(size=3))%>%layout(title = "Gausian Mixture Decomposition with 3 models")
print(mc_p)

mc<-Mclust(data, G=3)
mc_p<-plot_ly(x = data$V1,y = data$V3, z=data$V2, type = 'scatter3d', mode = 'markers', color = ((mc$classification)+30), marker = list(size=3))%>%layout(title = "Gausian Mixture Decomposition with 3 models")
print(mc_p)

