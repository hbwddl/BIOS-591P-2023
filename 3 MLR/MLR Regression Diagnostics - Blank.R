#Regression Diagnostics
library(car)
library(MASS)
library(readxl)

## Set working directory
setwd("~/Library/CloudStorage/OneDrive-EmoryUniversity/Documents/Work/BIOS 591P 2023/3 MLR/Data")
## Read in the data
bwt <- read_xlsx("Spring 2023 birthweight.xlsx")

# Fit multiple linear regression model

# Check normality of residuals

# Partial plots

# Residuals versus predicted values

# QQ plot of residuals

# Handling assumption violations
# Commonly used transformations is the log transformation
# Fake data. Air pollution vs. city population in thousands:
x <- runif(50,10,30) # X drawn between 10 and 30
e <- rnorm(50,0,1)
b <- 0.1
y <- exp(b*x + e)

airdata <- data.frame(x,y)

# Explore data

# Fit linear model on untransformed data

# Check residuals

# Transform data

# Fit transformed model

# Check residuals again

# Interpretation of slope?

## Next week we will cover outlier detection in regression diagnostics


