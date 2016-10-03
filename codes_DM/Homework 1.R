########### Homework 1 ##############

########### Problem 1 ##############

# Load the dataset
FFires <- read.csv(file.choose())
head(FFires)

# Sorting the levels of month column in our FFires dataframe
FFires$month <- factor(FFires$month, levels = c("jan", "feb", "mar",
                                                "apr", "may", "jun",
                                                "jul", "aug", "sep",
                                                "oct", "nov", "dec"))
levels(FFires$month)

############# Plot a ################

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

library(ggplot2)
library(dplyr)
mymultiplot <- function(){
  
  # for Jan
  a1 <- ggplot(data=monthFn("jan"), aes(x=temp, y=area)) + geom_point(color="Blue")
  a2 <- ggplot(data=monthFn("jan"), aes(x=month, y=area), color="Green") + geom_point(color="Green")
  a3 <- ggplot(data=monthFn("jan"), aes(x=DC, y=area), color="Red") + geom_point(color="Red")
  a4 <- ggplot(data=monthFn("jan"), aes(x=RH, y=area), color="Purple") + geom_point(color="Purple")
  
  # for Feb
  a5 <- ggplot(data=monthFn("feb"), aes(x=temp, y=area)) + geom_point(color="Blue")
  a6 <- ggplot(data=monthFn("feb"), aes(x=month, y=area), color="Green") + geom_point(color="Green")
  a7 <- ggplot(data=monthFn("feb"), aes(x=DC, y=area), color="Red") + geom_point(color="Red")
  a8 <- ggplot(data=monthFn("feb"), aes(x=RH, y=area), color="Purple") + geom_point(color="Purple")
  
  # for Mar
  a9 <- ggplot(data=monthFn("mar"), aes(x=temp, y=area)) + geom_point(color="Blue")
  a10 <- ggplot(data=monthFn("mar"), aes(x=month, y=area), color="Green") + geom_point(color="Green")
  a11 <- ggplot(data=monthFn("mar"), aes(x=DC, y=area), color="Red") + geom_point(color="Red")
  a12 <- ggplot(data=monthFn("mar"), aes(x=RH, y=area), color="Purple") + geom_point(color="Purple")
  
  # for Apr        
  a13 <- ggplot(data=monthFn("apr"), aes(x=temp, y=area)) + geom_point(color="Blue")
  a14 <- ggplot(data=monthFn("apr"), aes(x=month, y=area), color="Green") + geom_point(color="Green")
  a15 <- ggplot(data=monthFn("apr"), aes(x=DC, y=area), color="Red") + geom_point(color="Red")
  a16 <- ggplot(data=monthFn("apr"), aes(x=RH, y=area), color="Purple") + geom_point(color="Purple")
  
  # for May
  a17 <- ggplot(data=monthFn("may"), aes(x=temp, y=area)) + geom_point(color="Blue")
  a18 <- ggplot(data=monthFn("may"), aes(x=month, y=area), color="Green") + geom_point(color="Green")
  a19 <- ggplot(data=monthFn("may"), aes(x=DC, y=area), color="Red") + geom_point(color="Red")
  a20 <- ggplot(data=monthFn("may"), aes(x=RH, y=area), color="Purple") + geom_point(color="Purple")
  
  # for Jun
  a21 <- ggplot(data=monthFn("jun"), aes(x=temp, y=area)) + geom_point(color="Blue")
  a22 <- ggplot(data=monthFn("jun"), aes(x=month, y=area), color="Green") + geom_point(color="Green")
  a23 <- ggplot(data=monthFn("jun"), aes(x=DC, y=area), color="Red") + geom_point(color="Red")
  a24 <- ggplot(data=monthFn("jun"), aes(x=RH, y=area), color="Purple") + geom_point(color="Purple")
  
  # for Jul
  a25 <- ggplot(data=monthFn("jul"), aes(x=temp, y=area)) + geom_point(color="Blue")
  a26 <- ggplot(data=monthFn("jul"), aes(x=month, y=area), color="Green") + geom_point(color="Green")
  a27 <- ggplot(data=monthFn("jul"), aes(x=DC, y=area), color="Red") + geom_point(color="Red")
  a28 <- ggplot(data=monthFn("jul"), aes(x=RH, y=area), color="Purple") + geom_point(color="Purple")
  
  # for Aug
  a29 <- ggplot(data=monthFn("aug"), aes(x=temp, y=area)) + geom_point(color="Blue")
  a30 <- ggplot(data=monthFn("aug"), aes(x=month, y=area), color="Green") + geom_point(color="Green")
  a31 <- ggplot(data=monthFn("aug"), aes(x=DC, y=area), color="Red") + geom_point(color="Red")
  a32 <- ggplot(data=monthFn("aug"), aes(x=RH, y=area), color="Purple") + geom_point(color="Purple")
  
  # for Sep
  a33 <- ggplot(data=monthFn("sep"), aes(x=temp, y=area)) + geom_point(color="Blue")
  a34 <- ggplot(data=monthFn("sep"), aes(x=month, y=area), color="Green") + geom_point(color="Green")
  a35 <- ggplot(data=monthFn("sep"), aes(x=DC, y=area), color="Red") + geom_point(color="Red")
  a36 <- ggplot(data=monthFn("sep"), aes(x=RH, y=area), color="Purple") + geom_point(color="Purple")
  
  # for Oct
  a37 <- ggplot(data=monthFn("oct"), aes(x=temp, y=area)) + geom_point(color="Blue")
  a38 <- ggplot(data=monthFn("oct"), aes(x=month, y=area), color="Green") + geom_point(color="Green")
  a39 <- ggplot(data=monthFn("oct"), aes(x=DC, y=area), color="Red") + geom_point(color="Red")
  a40 <- ggplot(data=monthFn("oct"), aes(x=RH, y=area), color="Purple") + geom_point(color="Purple")
  
  # for Nov
  a41 <- ggplot(data=monthFn("nov"), aes(x=temp, y=area)) + geom_point(color="Blue")
  a42 <- ggplot(data=monthFn("nov"), aes(x=month, y=area), color="Green") + geom_point(color="Green")
  a43 <- ggplot(data=monthFn("nov"), aes(x=DC, y=area), color="Red") + geom_point(color="Red")
  a44 <- ggplot(data=monthFn("nov"), aes(x=RH, y=area), color="Purple") + geom_point(color="Purple")
  
  # for Dec
  a45 <- ggplot(data=monthFn("dec"), aes(x=temp, y=area)) + geom_point(color="Blue")
  a46 <- ggplot(data=monthFn("dec"), aes(x=month, y=area), color="Green") + geom_point(color="Green")
  a47 <- ggplot(data=monthFn("dec"), aes(x=DC, y=area), color="Red") + geom_point(color="Red")
  a48 <- ggplot(data=monthFn("dec"), aes(x=RH, y=area), color="Purple") + geom_point(color="Purple")
  
  multiplot(a1,a2,a3,a4, a5,a6,a7,a8, a9,a10,a11,a12, a13,a14,a15,a16, 
            a17,a18,a19,a20, a21,a22,a23,a24, a25,a26,a27,a28, a29,a30,a31,a32, 
            a33,a34,a35,a36, a37,a38,a39,a40, a41,a42,a43,a44, a45,a46,a47,a48, 
            cols=12)
}

monthFn <- function(strng){
  newDF <- FFires
  newDF1 <- FFires
  newDF1 <- subset(newDF, newDF$month== strng)
  return(newDF1)
}

mymultiplot()

############### Plot b ##################

##### 1st Way of doing it for every month from jan to dec #####
b1 <- ggplot(data=FFires, aes(x=wind, col=I("black"), fill=month))
b1 <- b1 + geom_histogram(binwidth=1) + facet_grid(month~. , scales = "free")
b1 <- b1 + theme(
  axis.title.x = element_text(size=20),
  axis.title.y = element_text(size =20),
  
  axis.text.x = element_text(size=20),
  axis.text.y = element_text(size=10),
  
  legend.title = element_text(size=20),
  legend.text = element_text(size=15)
)
b1

##### 2nd way of doing it directly without considering the months #####
b2 <- ggplot(data=FFires, aes(x=wind, col=I("black"))) +
  geom_histogram(binwidth=0.5, aes(fill=I('white')))
b2 + theme(
  axis.title.x = element_text(size=20),
  axis.title.y = element_text(size =20),
  
  axis.text.x = element_text(size=20),
  axis.text.y = element_text(size=20)
)


############## Plot c ##################

###### Ask the TA #####
summary(b)

############## Plot d #################

##### 1st way of doing it for every chart of wind from jan to dec #####
d1 <- ggplot(data=FFires, aes(x=wind, col=I("black"), fill=month))
d1 <- d1 + geom_histogram(binwidth=1, aes(y=..density..)) + geom_density(fill=NA) + facet_grid(month~. , scales = "free")
d1 <- d1 + theme(
  axis.title.x = element_text(size=20),
  axis.title.y = element_text(size =20),
  
  axis.text.x = element_text(size=20),
  axis.text.y = element_text(size=10),
  
  legend.title = element_text(size=20),
  legend.text = element_text(size=15)
)
d1

##### 2nd way of doing it for one chart of wind #####
d2 <- ggplot(data=FFires, aes(x=wind, col=I("black"))) +
  geom_histogram(aes(y = ..density.., fill=I('white')), binwidth=0.5) + geom_density(fill=NA)
d2 + theme(
  axis.title.x = element_text(size=20),
  axis.title.y = element_text(size =20),
  
  axis.text.x = element_text(size=20),
  axis.text.y = element_text(size=20)
)

############## Plot e #################

e <- ggplot(data=FFires, aes(x=month, fill=month))
e + geom_density() +
  theme(
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size =20),
    
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=20),
    
    legend.title = element_text(size=20),
    legend.text = element_text(size=15)
)
                 
############### Plot f #################

pairs(FFires[,c(9, 10, 7, 6)], col="dark green")

############## Plot g ##################


##### 1st Way: Boxplot of wind over months #####
g1 <- ggplot(FFires, aes(x=month, y=wind, col=month)) +
  geom_boxplot(size=1) + geom_jitter(size=1, alpha=0.4)

# Boxplot of ISI over months
g2 <- ggplot(FFires, aes(x=month, y=ISI, col=month)) +
  geom_boxplot(size=1) + geom_jitter(size=1, alpha=0.4)

# Boxplot of DC over months
g3 <- ggplot(FFires, aes(x=month, y=DC, col=month)) +
  geom_boxplot(size=1) + geom_jitter(size=1, alpha=0.4)

# Combining g1, g2, g3
multiplot(g1, g2, g3)

##### 2nd Way: Independent boxplots #####
attach(FFires)
par(mfrow=c(2,2))
boxplot(FFires$wind, main="Boxplot of Wind", xlab="Wind")
boxplot(FFires$ISI, main="Boxplot of ISI", xlab="ISI")
boxplot(FFires$DC, main="Boxplot of DC", xlab="DC")

############# Plot h ##################

# Histogram of DMC
h1 <- ggplot(FFires, aes(DMC, col=I("black"), fill=I("white"))) +
  geom_histogram(binwidth=5, aes(y=..density..)) + geom_density(fill=NA, size=1)

# Histogram of log(DMC)
h2 <- ggplot(FFires, aes(log(DMC), col=I("black"), fill=I("white"))) +
  geom_histogram(binwidth=0.1, aes(y=..density..)) + geom_density(fill=NA, size=1)

# Combining h1 and h2 for easy comparison 
multiplot(h1, h2)


############### THE END #############
