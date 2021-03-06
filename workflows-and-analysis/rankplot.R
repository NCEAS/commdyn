library(ggplot2)
library(reshape2)

## Generate some sample data 
x = 1977:2003
y1 = .3*sin(x) + rnorm(length(x), 0, .1) + -.05*x + 150
y2 = .2*sin(x) + rnorm(length(x), 0, .1) + -.01*x + 70
y3 = .2*sin(x) + rnorm(length(x), 0, .1) + .01*x + 30

# Reshape the data: year, variable (y1,y2,y3), value
dat = melt(data.frame(x,y1, y2, y3), "x")

# 
ggplot(dat, aes(x,value,color=variable)) + geom_line()
ggplot(dat, aes(x,value,color=variable)) + geom_line() + coord_polar() 



