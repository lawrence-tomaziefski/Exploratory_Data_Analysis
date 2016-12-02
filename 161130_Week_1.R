require(dplyr)
class = c("numeric", "character", "factor", "numeric", "numeric")
pollution = read.csv("avgpm25.csv", colClasses = class)
summary(pollution$pm25)
boxplot(pollution$pm25, col = "blue")
hist(pollution$pm25, col = "green")
rug(pollution$pm25)
hist(pollution$pm25, col = "green", breaks = 100)
rug(pollution$pm25)
hist(pollution$pm25, col = "green")
abline(v = 12, lwd = 2)
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)

data(airquality)
with(airquality, {
         plot(Temp, Ozone)
         lines(loess.smooth(Temp, Ozone))
})

require(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 
         1))

colors()  #returns the colors avalible in R

###### Making a scatter plot
library(datasets)
## Make the initial plot
with(airquality, plot(Wind, Ozone))

 ## Add a title
title(main = "Ozone and Wind in New York City")

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City")) # makes same plot as above

with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue")) # adds some color
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", type = "n")) #makes an empty plot
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))


with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", pch = 20))
## Fit a simple linear regression model
model <- lm(Ozone ~ Wind, airquality)
## Draw regression line on plot
abline(model, lwd = 2)
