#Regression Diagnostics
library(car)
library(MASS)
library(readxl)

## Set working directory
setwd("~/Library/CloudStorage/OneDrive-EmoryUniversity/Documents/Work/BIOS 591P 2023/3 MLR/Data")
## Read in the data
bwt <- read_xlsx("Spring 2023 birthweight.xlsx")

bwt.lm <- lm(BWT ~ AGE + WT + SMOKE + HT, data=bwt)

## Check normality of residuals
hist(residuals(bwt.lm),main="Histogram of Residuals",xlab="Residuals")

# Partial plots
crPlots(bwt.lm,line=FALSE,smooth=FALSE)

# Residuals versus predicted values
plot(predict(bwt.lm),residuals(bwt.lm),pch=19,main="Residuals vs. Predicted Values",xlab="Predicted Values",ylab="Residuals")
abline(h=0)

# QQ plot of residuals
qqnorm(residuals(bwt.lm))
qqline(residuals(bwt.lm)) # adds the normality line to the q-q plot

# Handling assumption violations
# Commonly used transformations is the log transformation
# Fake data. Air pollution vs. city population in thousands:
x <- runif(50,10,30) # X drawn between 10 and 30
e <- rnorm(50,0,1)
b <- 0.1
y <- exp(b*x + e)

airdata <- data.frame(x,y)

plot(airdata$x,airdata$y,xlab="Population (thousands)",ylab="AQI",main="Air Quality Index vs. Population")

untransformed <- lm(y~x,data=airdata)

plot(x,y,xlab="Population (thousands)",ylab="AQI",main="Air Quality Index vs. Population")
abline(untransformed)

# Check residuals

hist(residuals(untransformed))
qqnorm(residuals(untransformed))
qqline(residuals(untransformed))

# Positively skewed residuals suggest a log transformation might help. See textbook.

plot(x,log(y),xlab="Population (thousands)",ylab="log(Air Quality Index)")

airdata$logy <- log(airdata$y)

transformed <- lm(logy ~ x,data=airdata)

plot(airdata$x,airdata$logy,xlab="Population (thousands)",ylab="log(Air Quality Index)")
abline(transformed)

# Check residuals again

hist(residuals(transformed))
qqnorm(residuals(transformed))
qqline(residuals(transformed))

# Better! Not perfect.
# Interpretation of slope?

## Next week we will cover outlier detection in regression diagnostics
# Leverage: hatvalues()
lev.vals <- hatvalues(bwt.lm)

lev.threshold <- 2*(4+1)/length(lev.vals)

hist(lev.vals)

abline(v=lev.threshold)

lev.vals > lev.threshold

which(lev.vals > lev.threshold)

bwt[which(lev.vals > lev.threshold),]

## Cook's distance
cooksd <- cooks.distance(bwt.lm)

cooksd > 4/length(cooksd)

which(cooksd > 4/length(cooksd))

bwt[which(cooksd > 4/length(cooksd)),]

## Jackknife Residuals
jackres <- studres(bwt.lm)

jackres > 2

which(jackres > 2)

bwt[which(jackres > 2),]

## Correlation
cor(bwt)

## VIFs
vif(bwt.lm)

## Replicate SAS fit diagnostics plots
# par(mfrow=c(3,3)) # This sets the parameter for plots to a grid with 3 rows and 3 columns, if you want all plots in the same image
# Predicted vs. residuals
plot(predict(bwt.lm),residuals(bwt.lm),pch=19,xlab="Predicted Value",ylab="Residual")
abline(h=0,col="blue") # Horizontal line at 0

# Predicted vs. Studentized residuals
plot(predict(bwt.lm),jackres,pch=19,xlab="Predicted Value",ylab="RStudent")
abline(h=c(-2,2),col="blue") #Recall that c(-2,2) creates a list. Place horizontal lines at -2 and 2

# Leverage vs. Studentized residuals
plot(lev.vals,jackres,pch=19,xlab="Leverage",ylab="RStudent")
abline(h=c(-2,2),col="blue")
abline(v=lev.threshold,col="blue") # Vertical line at the leverage threshold

# Q-Q Plot
qqnorm(residuals(bwt.lm),pch=19)
qqline(residuals(bwt.lm),col="blue")

# Predicted vs. Actual values
plot(predict(bwt.lm),bwt$BWT,pch=19,xlab="Predicted Values",ylab="Birthweight")
abline(a=0,b=1,col="blue") # Intercept = 0, slope = 1. If a point is on the line, then the predicted value is close to the actual value

# Cook's distance
plot(cooksd,xlab="Observation",ylab="Cook's D", pch=19)
abline(h=4/length(cooksd),col="blue") #This plot is harder to interpret for a lot of observations...

# Histogram of residuals
hist(residuals(bwt.lm),xlab="Residuals",probability=T,ylim=c(0,6e-04)) # Probability = T gives us percent instead of frequency. We set the ylim so that we can see the whole density line.
curve(dnorm(x, mean=mean(residuals(bwt.lm)), sd=sd(residuals(bwt.lm))), 
      col="blue", lwd=2, add=TRUE) # Adding a normal density with the same mean and standard deviation as the residuals

# par(mfrow=c(1,1)) # Set the plot area to have only one plot again
