#install.packages("TTR")
library("TTR")
library("forecast")
data <- read.csv("/Users/Darshak/Downloads/IoT\ P3/backupdate.csv")
train<- data[1:1500,]
test<- data[1501:2000,]
pdf("iot p3.pdf")

#-------Simple Moving average----
n<-1500
rmse<-c(1:20)
i<-1
m<-2
predicted<-c(1:1500)
for (m in 1:70) {
  ra<-c((m+1) : 1500)
  for (j in ra) {
    l<-j-m
    r<-j-1
    predicted[j]<-sum(train[l:r])/m
  }
  error<-(train[(m+1):1500]-predicted[(m+1):1500])
  sse<-sum(error^2)
  rmse[i]<- sqrt(sse/n)
  i<-i+1
}
plot(c(1:70),rmse,  xlab = "m", main = "RMSE vs m", ylab = "RMSE")
print("Best m value based on rmse is : ")
print(which.min(rmse))

m<-1
ra<-c((m+1) : 1500)
for (j in ra) {
  l<-j-m
  r<-j-1
  predicted[j]<-sum(train[l:r])/m
}


matplot(c(20:1500), cbind(train[20:1500],predicted[20:1500]), pch=c(0,1), col = c("Red", "Blue"), main=paste("Original and Predicted values for m = ", 1), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.75), lt = c(1,1))
legend("topleft", lty = c(1, 1), col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)

matplot(c(500:600), cbind(train[500:600],predicted[500:600]), pch=c(0,1), col = c("Red", "Blue"), main=paste("Original and Predicted values for m = ", 1, "For index (500 to 600)"), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.9), lt = c(1,1))
legend("topleft", lty = c(1, 1), col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)



#----------------Exponential-------------
a_values<-seq(0.01,1,0.01)
ctr<-1
for (i in a_values) {
  a<-i
  predicted[19] <- train[19]
  for (j in 20:1500) {
    predicted[j] <- (a* train[j-1]) + ((1-a)* predicted[j-1])
  }
  error<-(train[20:1500] - predicted[20:1500])
  sse<-sum(error^2)
  rmse_e[ctr]<-sqrt(sse/n)
  ctr<-ctr+1
}
plot(seq(0.01,1,0.01), rmse_e[1:100], xlab="Alpha Values", ylab = "RMSE", main = "RMSE vs Alpha")
print("Best Alpha value based on rmse is : ")
print(which.min(rmse_e)/100)

predicted<-0
a=0.54
predicted[19]<-train[19]
for (j in 20:1500) {
  predicted[j] <- (a* train[j-1]) + ((1-a)* predicted[j-1])
}

matplot(c(20:1500), cbind(train[20:1500],predicted[20:1500]), pch=c(0,1), col = c("Red", "Blue"), main=paste("Original and Predicted values for alpha = ", 0.54), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.75), lt = c(1,1))
legend("topleft", lty = c(1, 1), col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)

matplot(c(500:600), cbind(train[500:600],predicted[500:600]), pch=c(0,1), col = c("Red", "Blue"), main=paste("Original and Predicted values for alpha = ", 0.54, "For index (500 to 600)"), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.9), lt = c(1,1))
legend("topleft", lty = c(1, 1), col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)


#--------AR(p)------
pacf(train, main = "PACF")
for (i in 1:10) {
  model<-ar(train, aic = FALSE, order.max = i)
  error<-residuals(model)[(i+1):1500]
  sse<-sum(error^2)
  rmse_ar[i]<-sqrt(sse/n)
}
plot(c(1:10), rmse_ar, xlab = "p", ylab = "RMSE", main = "RMSE vs p")
print("From the pacf p :")
print(3)

model<-ar(train, aic = FALSE, order.max = 3)
print(model)

matplot(c(20:1500), cbind(train[20:1500], fitted(model)[20:1500]), pch=c(0,1), col = c("Red", "Blue"), main=paste("Ar(p) : Original and Predicted values for p = ", 3), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.75), lt = c(1,1))
legend("topleft", lty = c(1, 1), col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)

matplot(c(500:600), cbind(train[500:600], fitted(model)[500:600]), pch=c(0,1), col = c("Red", "Blue"), main=paste("Ar(p) : Original and Predicted values for p = ", 3), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.75), lt = c(1,1))
legend("topleft", lty = c(1, 1), col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)

error<-residuals(model)[(3+1):1500]
sse<-sum(error^2)
rmse_ar<-sqrt(sse/n)
print(paste("RMSE is ", rmse_ar))

#---------Test data------

#-----SMA-----
m<-1
ra<-c((m+1) : 500)
tpredicted<-c(1:500)
for (j in ra) {
  l<-j-m
  r<-j-1
  tpredicted[j]<-sum(test[l:r])/m
}
matplot(c(2:500), cbind(test[2:500],tpredicted[2:500]), pch=c(0,1), col = c("Red", "Blue"), main=paste("SMA : Original and Predicted values for m = ", 1, "For test data"), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.9), lt = c(1,1))
legend("topleft", lty = c(1, 1), col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)
matplot(c(200:300), cbind(test[200:300],tpredicted[200:300]), pch=c(0,1), col = c("Red", "Blue"), main=paste("SMA : Original and Predicted values for m = ", 1, "For test data"), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.9), lt = c(1,1))
legend("topleft", lty = c(1, 1), col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)


terror<-(test[(m+1):500]-tpredicted[(m+1):500])
sse<-sum(terror^2)
trmse<-sqrt(sse/n)
print(paste("SMA : RMSE for test data is ", trmse))


#-----ESA-----

tpredicted<-0
a=0.54
tpredicted[1]<-test[1]
for (j in 2:500) {
  tpredicted[j] <- (a* test[j-1]) + ((1-a)* tpredicted[j-1])
}
matplot(c(2:500), cbind(test[2:500],tpredicted[2:500]), pch=c(0,1), col = c("Red", "Blue"), main=paste("Expo. : Original and Predicted values for alpha = ", 0.54, "For test data"), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.9), lt = c(1,1))
legend("topleft", lty = c(1, 1), col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)

matplot(c(200:300), cbind(test[200:300],tpredicted[200:300]), pch=c(0,1), col = c("Red", "Blue"), main=paste("Expo. : Original and Predicted values for alpha = ", 0.54, "For test data"), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.9), lt = c(1,1))
legend("topleft", lty = c(1, 1), col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)


terror<-(test[(m+1):500]-tpredicted[(m+1):500])
sse<-sum(terror^2)
trmse<-sqrt(sse/n)
print(paste("ESA : RMSE for test data is ", trmse))

#----AR(p)-----
model<-ar(test, aic = FALSE, order.max = 3)

matplot(c(1:500), cbind(test[1:500], fitted(model)), pch=c(0,1), col = c("Red", "Blue"), main=paste("Ar(p) : Original and Predicted values for p = ", 3, "For test data"), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.75), lt = c(1,1))
legend("topleft", lty = c(1, 1), col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)

matplot(c(200:300), cbind(test[200:300], fitted(model)[200:300]), pch=c(0,1), col = c("Red", "Blue"), main=paste("Ar(p) : Original and Predicted values for p = ", 3, "For test data"), ylab = "Values", xlab = "Index", type = "l", lw = c(1,0.75), lt = c(1,1))
legend("topleft", lty = c(1, 1), col = c("Red", "Blue"), legend = c("Original Values", "Predicted Values"), box.lwd = 0)

terror<-residuals(model)[(3+1):500]
sse<-sum(terror^2)
rmse_ar<-sqrt(sse/n)
print(paste("RMSE is ", rmse_ar))
dev.off()

