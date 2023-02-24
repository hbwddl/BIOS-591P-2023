#Regression Diagnostics
library(car)
library(MASS)
library(readxl)

## Set working directory
setwd("~/Library/CloudStorage/OneDrive-EmoryUniversity/Documents/Work/BIOS 591P 2023/3 MLR/Data")
## Read in the data
bwt <- read_xlsx("Spring 2023 birthweight.xlsx")

# Fit multiple linear regression model
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

# Explore data

plot(airdata$x,airdata$y,xlab="Population (thousands)",ylab="AQI",main="Air Quality Index vs. Population")

# Fit linear model on untransformed data

untransformed <- lm(y~x,data=airdata)

plot(x,y,xlab="Population (thousands)",ylab="AQI",main="Air Quality Index vs. Population")
abline(untransformed)

# Check residuals

hist(residuals(untransformed))
qqnorm(residuals(untransformed))
qqline(residuals(untransformed))

# Positively skewed residuals suggest a log transformation might help. See textbook.

plot(x,log(y),xlab="Population (thousands)",ylab="log(Air Quality Index)")

# Transform data

airdata$logy <- log(airdata$y)

# Fit transformed model

transformed <- lm(logy ~ x,data=airdata)

summary(transformed)

plot(airdata$x,airdata$logy,xlab="Population (thousands)",ylab="log(Air Quality Index)")
abline(transformed)

# Check residuals again

hist(residuals(transformed))
qqnorm(residuals(transformed))
qqline(residuals(transformed))

# Better! Not perfect.
# Interpretation of slope?
# For each increase of city population by 1, the log(AQI) increases by ___, on average
# OR
# For each increase of city population by 1, the AQI increases by (exp(beta) - 1)*100% on average

(exp(transformed$coefficients[2])-1)*100

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

