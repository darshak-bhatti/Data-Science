library(corrplot)
library(rgl)
library(plotrix)
library(ggplot2)
library(randtests)
library(nortest)

data = read.csv("./BDH.csv")

X1<-data$X1
X2<-data$X2
X3<-data$X3
X4<-data$X4
X5<-data$X5
Y<-data$Y

#----------Task 1 -----------------

m<-c()
v<-c()
for (i in 1:5) {
  hist(data[,i], xlab = paste("X",i,sep = ""), main = paste("X",i,sep = ""))
  m[i] = mean(data[,i])
  v[i] = var(data[,i])
}
col1 <- colorRampPalette(c("#67001F", "#B2182B", "#4393C3", "#2166AC", "#053061"))
corrplot::corrplot(cor(data), method = "number", title="Correl", col = col1(1000))
print("Means")
print(m)
print("Variances")
print(v)

#----------Task 2 -----------------
x1fit<-lm(Y ~ X1)
x1resid<-resid(x1fit)
x1pred<-predict(x1fit)
plot(X1,Y, main = expression('Regression model :'~Y== X[1]))
abline(x1fit, col = "Red", lwd=1.5)
legend("topleft", pch=c(1,NA,NA), lwd=c(NA,1,1), legend = c("Predicted Values","Regression Line", "Residual"), lty=1, col = c("black","red","blue"), box.lwd = 0)
print(summary(x1fit))
print("Variance of Predicted Value(Y-cap)")
print(var(x1pred))
for (i in 1:length(x1pred)) {
  lines(c(X1[i], X1[i]), c(Y[i], x1pred[i]), col = "blue")
}

plot(x1pred, x1resid, xlab = "Predicted Values", ylab = "Residuals", main = "Residuals vs Predicted Values")
abline(h=0, col='red')
acf(x1resid, main = expression('Autocorrelogram for Residuals in model'~Y==X[1]))
qqnorm(x1resid, main = expression('Q-Q plot for Residuals in model'~Y==X[1]))
qqline(x1resid, col="red")
hist(x1resid, main = expression('Histogram for Residuals in model'~Y==X[1]))
print(pearson.test(x1resid))
print(bartels.rank.test(x1resid))

x1pol2<-lm(Y ~ X1 + I(X1^2))
x1pol3<-lm(Y ~ X1 + I(X1^2) + I(X1^3))
plot(X1, Y, main = expression('Regression lines for various models'))
lines(sort(X1), fitted(x1pol3)[order(X1)], col = "Blue", lwd = 2)
lines(sort(X1), fitted(x1pol2)[order(X1)], col = "Red", lwd = 2)
abline(x1fit, col = "green", lwd = 2)
#dev.off()
print(summary(x1pol2))
print("Variance of Predicted Value(Y-cap)")
print(var(predict(x1pol2)))
print(summary(x1pol3))
print("Variance of Predicted Value(Y-cap)")
print(var(predict(x1pol3)))
legend("topleft", lwd=2, cex = 1,y.intersp = 1.5, legend =c(expression(Y==X[1] + X[1]^2 + X[1]^3),expression(Y==X[1] + X[1]^2),expression(Y==X[1])), lty=1, col = c("blue", "red", "green"), box.lwd = 0)

#----------Task 3 -----------------

mlr<-lm(Y ~ X1 + X2 + X3 + X4 + X5)
mlr_resid<-resid(mlr)
qqnorm(mlr_resid, main = expression('Residual Q-Q plot :'~Y == X[1] + X[2] + X[3] + X[4] + X[5]))
qqline(mlr_resid, col = "red")
plot(predict(mlr), mlr_resid, main = expression('Residual scatter plot :'~Y == X[1] + X[2] + X[3] + X[4] + X[5]), xlab = "Predicted values", ylab = "e")
abline(h = 0, col = "red")
acf(mlr_resid, main = expression('Autocorrelogram for Residuals in model'~Y == X[1] + X[2] + X[3] + X[4] + X[5]))
print(summary(mlr))
print("Variance of Predicted Value(Y-cap)")
print(var(predict(mlr)))
print(pearson.test(mlr_resid))


improved_mlr<-lm(Y ~ X1 + X2 + X4 + X5)
i_mlr_resid<-resid(improved_mlr)
qqnorm(i_mlr_resid, main = expression('Residual Q-Q plot :'~Y == X[1] + X[2] + X[4] + X[5]))
qqline(i_mlr_resid, col = "red")
plot(predict(improved_mlr), i_mlr_resid, main = expression('Residual scatter plot :'~Y == X[1] + X[2] + X[4] + X[5]), xlab = "Predicted values", ylab = "e")
abline(h = 0, col = "red")
acf(i_mlr_resid, main = expression('Autocorrelogram for Residuals in model'~Y == X[1] + X[2] + X[4] + X[5]))
print(summary(improved_mlr))
print("Variance of Predicted Value(Y-cap)")
print(var(predict(improved_mlr)))
print(pearson.test(i_mlr_resid))

mlr2<-lm(Y ~ X1 + X4 + X5)
print(summary(mlr2))
print("Variance of Predicted Value(Y-cap)")
print(var(predict(mlr2)))




