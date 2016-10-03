############ Homework 1 - Twitter Data ##############

# Load the dataset
twitter <- read.csv(file.choose())

# Getting to know the dataset
head(twitter)
ncol(twitter)
nrow(twitter)
str(twitter)

########## Plot a #############
boxplot(log((twitter$friends_count)))
library(ggplot2)
ggplot(Dcount, aes(y=..density.., x=((log(Dcount$Fcount))))) + 
  geom_histogram(binwidth = 0.1, aes(col=I("black")), fill='white') + geom_density(size=1)
D10count
Fcount <- twitter$friends_count[twitter$friends_count <= 10]
F10count <- Dcount$Fcount[Dcount$Fcount <= 10]
F10count
D10count <- data.frame(F10count)
twitterFcount <- twitter$friends_count
Dcount <- data.frame(Fcount)
rm(Fcount)
is.data.frame(Fcount)
nrow(twitter)

max(twitter$friends_count)
summary(twitter$friends_count)
range(twitter$friends_count)
sort(twitter$friends_count)
library(Amelia)
missing(twitter$friends_count)
########## Plot b #############
summary(twitter$friends_count)

########## Plot c ############
head(twitter)
missmap(twitter[,c(5,7)])
######### Plot d ############
scatterplot3d::scatterplot3d(twitter$created_at_year, 
                             twitter$education,
                             twitter$age, type='h',
                             pch=16,
                             highlight.3d = T, main = '3DScatterPlot')
legend("topleft", inset=0.5, bty = 'n', cex=0.5,
       title = 'Created_at vs Education vs Age',
       c(twitter$created_at_year, twitter$education, twitter$age), fill = c(
         'red', 'blue', 'green')
)
####################### addition
scatterplot3d(disp, wt, mpg,        # x y and z axis
              color=pcolor, pch=19,        # circle color indicates no. of cylinders
              type="h", lty.hplot=2,       # lines to the horizontal plane
              scale.y=.75,                 # scale y axis (reduce by 25%)
              main="3-D Scatterplot Example 4",
              xlab="Displacement (cu. in.)",
              ylab="Weight (lb/1000)",
              zlab="Miles/(US) Gallon")
s3d.coords <- s3d$xyz.convert(disp, wt, mpg)
text(s3d.coords$x, s3d.coords$y,     # x and y coordinates
     labels=row.names(mtcars),       # text to plot
     pos=4, cex=.5)                  # shrink text 50% and place to right of points)
# add the legend
legend("topleft", inset=.05,      # location and inset
       bty="n", cex=.5,              # suppress legend box, shrink text 50%
       title="Number of Cylinders",
       c("4", "6", "8"), fill=c("red", "blue", "darkgreen"))
)
########################## fin
########## Plot e #############
levels(twitter$country)
str(twitter)
count <- c(0,0,0,0,0)
contry <- c("United Kingdom", "Canada", "India", "Australia", "USA")
for (i in twitter$country){
  if (i == contry[1]){
    count[1] <- count[1] + 1
  }
  if (i == contry[2]){
    count[2] <- count[2] + 1
  }
  if (i == contry[3]){
    count[3] <- count[3] + 1
  }
  if (i == contry[4]){
    count[4] <- count[4] + 1
  }
  if (i == contry[5]){
    count[5] <- count[5] + 1
  }
}
count
levels(twitter$country)
ggplot(data=twitter, aes(x=country, y=count(country))) +
  geom_bar()


hist(twitter$country)


barplot(twitter$country)

countries <- data.frame(mat1)

countrydata <- c(650, 1000, 900, 300, 14900)
mat1 <- matrix(countrydata, nrow = 1, byrow = T)
mat1

colnames(mat1) <- contry
mat1

ggplot(data=countries, aes()) +
  coord_polar(theta='y')

p23 <- pie(Country,
        labels = contry,
         main = "Pie chart of countries",
         col = rainbow(5))

# 3D pie charts
countries <- c("UK", "Canada", "India", "Australia", "USA")
countries
Country <- countrydata/sum(countrydata)*100
pie3D(Country, labels=countries, explode = 0.1, main="Pie chart of countries", theta=pi/3)
?pie3D
library(plotrix)
attach(DFmat1)
par=(mfrow=c(1,2))
pie3D(DFmat1$countrydata, labels=DFmat1$countries, explode = 0.1, main="Pie chart of countries", theta=pi/3)
pie(DFmat1$countrydata, labels = DFmat1$countries, main = "Pie chart of countries", col = rainbow(5))


## Pie charts ##
attach(DFmat1)
par=(mfrow=c(1,2))
pie3D(DFmat1$countrydata, labels=DFmat1$countrydata, explode = 0.1, main="Pie chart of countries", theta=pi/3, col = rainbow(5))
pie(DFmat1$countrydata, labels=DFmat1$countrydata, main = "Pie chart of countries", col = rainbow(5))
legend("topright", inset=0.5, title='Countries', DFmat1$countries, fill=rainbow(5), legend=DFmat1$countries)

is.numeric(DFmat1$countrydata)

mat1 <- cbind(countries, countrydata)
is.numeric(mat1[,2])
DFmat1 <- data.frame(mat1)
DFmat1$Country <- as.numeric(DFmat1$Country)
is.numeric(DFmat1$countrydata)
DFmat1$countrydata <- as.integer(DFmat1$countrydata)
DFmat1$countrydata <- round(DFmat1$countrydata, 2)
sum(DFmat1$Country)

############## Plot f #################
?density()
range(twitter$created_at_year)
plot(density(twitter$created_at_year), bty='n', lwd=2, ylim = c(0, 0.30), xlab = 'Created at Year', ylab = '', xlim=c(2004,2016), main = 'Kernel Density plot of Created at Year')
density(twitter$created_at_year)


############ THE END ##############
