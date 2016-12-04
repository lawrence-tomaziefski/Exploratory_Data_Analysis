#####Lattice Plots 
library(lattice)
library(datasets)
## Simple scatterplot
xyplot(Ozone ~ Wind, data = airquality)

library(nlme)
xyplot(weight ~ Time | Diet, BodyWeight)


##ggplot
library(ggplot2)

str(mpg)
#qplot scatterplot
qplot(displ, hwy, data = mpg)
qplot(displ, hwy, data = mpg, color = drv)
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"))

#qplot histogram
qplot(hwy, data = mpg, fill = drv, binwidth = 2)

#qplot boxplot
qplot(drv,hwy, data = mpg, geom = "boxplot", color = manufacturer)



#facets
qplot(displ, hwy, data = mpg, facets = . ~ drv)
qplot(hwy, data = mpg, facets = drv ~ ., binwidth = 2)
qplot(displ, hwy, data = mpg, facets = . ~ drv)
qplot(displ, hwy, data = mpg, facets = . ~ drv, geom = c("point", "smooth"), theme_bw())
qplot(hwy, data = mpg, facets = manufacturer ~ ., binwidth = 2, fill = drv)

f = qplot(displ, hwy, data = mpg, facets = . ~ drv, geom = c("point", "smooth")) + theme_bw()

##ggplot building up in layers 
g <- ggplot(mpg,aes(displ,hwy))
 g+geom_point()
 g+geom_point() +geom_smooth()
 g+geom_point() +geom_smooth(method = "lm")
 g+geom_point() +geom_smooth(method = "lm") +facet_grid(.~drv)
 ##g +geom_point() +geom_smooth(method = "lm")+facet_grid(manufacturer~drv) <----just playing around
  g + geom_point() +geom_smooth(method = "lm") +facet_grid(.~drv)+ggtitle("Swirl Rules!")
  g + geom_point(color = "pink", size = 4, alpha = 1/2)
  g + geom_point(size = 4, alpha = 1/2, aes(color = drv))
  g + geom_point(aes(color = drv)) +labs(title = "Swirl Rules!") +labs(x = "Displacement",y = "Hwy Mileage")
  g + geom_point(size = 2, alpha = 1/2, aes(color = drv)) +geom_smooth(size = 4, linetype = 3,method = "lm", se=FALSE)
  g + geom_point(aes(color = drv)) + theme_bw(base_family="Times")
  g + geom_point(aes(color = drv)) + theme_bw(base_family="Helvetica")
  
 ##ggplot line 
g <- ggplot(testdat, aes(x = myx, y = myy))
g + geom_line()
g + geom_line() + ylim(-3,3)
g + geom_line() + coord_cartesian(ylim = c(-3,3))
  
g <- ggplot(mpg, aes(x = displ, y = hwy, color = factor(year))) 
g + geom_point()
g + geom_point()+facet_grid(drv~cyl, margins = TRUE)
g + geom_point()+facet_grid(drv~cyl, margins = TRUE) + geom_smooth(method = lm,se = FALSE, size = 2, color = "black")
g + geom_point()+facet_grid(drv~cyl, margins = TRUE) + geom_smooth(method = lm,se = FALSE, size = 2, color = "black") + labs(x = "Displacement",y = "Hwy Mileage", title = "Swirl Rules!")                          

#Another example of qplots using the diamond data set
qplot(price, data = diamonds)
range(diamonds$price)
qplot(price, data = diamonds,binwidth = 18497/30)
qplot(price, data = diamonds,binwidth = 18497/30, fill = cut)
qplot(price, data = diamonds,binwidth = 18497/30, geom = "density")    
qplot(price, data = diamonds,binwidth = 18497/30, geom = "density",color = cut)  

qplot(carat,price, data = diamonds)
qplot(carat,price, data = diamonds, shape = cut)
qplot(carat,price, data = diamonds, color = cut)
qplot(carat,price, data = diamonds, color = cut) + geom_smooth(method = "lm")
qplot(carat,price, data = diamonds, color = cut,facets = .~cut) + geom_smooth(method = "lm")  

#Another example of ggplots using the diamond data set
g <- ggplot(diamonds,aes(depth,price))
g + geom_point(alpha=1/3)
cutpoints <- quantile(diamonds$carat,seq(0,1,length = 4),na.rm = TRUE)
diamonds$car2 = cut(diamonds$carat,cutpoints)
g <- ggplot(diamonds,aes(depth,price))
g + geom_point(alpha = 1/3) + facet_grid(cut~car2)
g + geom_point(alpha = 1/3) + facet_grid(cut~car2) + geom_smooth(method = "lm",size = 3, color = "pink")

ggplot(diamonds,aes(carat,price)) + geom_boxplot() + facet_grid(.~cut)
