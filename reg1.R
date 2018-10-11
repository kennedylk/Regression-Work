#Kennedy Kelly
#Regression and data manipulation ex1
#16 batches of plastic were made, and then tested at a certain time level
#Xi is the time level and Yi is measured hardness

#Create data frame; print and plot
{yi<- c(199.0,205.0,196.0,200.0,218.0,220.0,215.0,223.0,237.0,234.0,235.0,230.0,250.0,248.0,253.0,246.0)
xi<- c(16.0,16.0,16.0,16.0,24.0,24.0,24.0,24.0,32.0,32.0,32.0,32.0,40.0,40.0,40.0,40.0)
df = data.frame(yi,xi) 
print(df, row.names = FALSE) 
summary(df) 
plot(yi~xi, data = df, xlab = "Xi", ylab = "Yi", main = "Question A", pch = 20)
abline(lm(yi~xi), col="red")}
#Question: Based on regression line, what can we predict Yi to be at 30?
{plot(yi~xi, data = df, xlab = "Xi", ylab = "Yi", main = "Question B", pch = 20)
linmod<- lm(yi~xi) 
summary(linmod)
yi.pred<-(predict(linmod,data.frame(xi = c(30))))
cat("Prediction xi = 30", yi.pred)
lab<- "(230,30)"
abline(lm(yi~xi), col="gray")
points(30, yi.pred, col = "blue", pch = 8)
text(30, yi.pred, labels=lab, cex= 0.7, pos=3)}
#Question: If we increase Xi by one, how will that affect both the model and the prediction
{xi.shift<- xi+1
yi.shift<-(predict(linmod,data.frame(xi.shift)))
linmod2<- lm(yi.shift~xi.shift)
df2<-data.frame(yi.shift~xi.shift)
plot(yi.shift~xi.shift, data = df2,xlab = "Xi+1", ylab = "Yi new", main = "Question C", pch = 20)
abline(lm(yi.shift~xi.shift), col="blue")
abline(lm(yi~xi), col="gray")
yi.pred2<-(predict(linmod2,data.frame(xi.shift = c(30)))) 
cat("Prediction xi = 30 is yi =", yi.pred)
cat("New prediction xi = 30 is yi=", yi.pred2)
points(30, yi.pred2, col = "red", pch = 8)
points(30, yi.pred, col = "gray", pch = 8)}
#Obtain residuals
{res = residuals(linmod)
print(res)}
#calculate sigma and sigma squared
{sig = sigma(linmod)
sig.square<-c(sig*sig)
cat("Estimated sigma = ", sig)
cat("Estimated sigma squared = ", sig.square)}
  
