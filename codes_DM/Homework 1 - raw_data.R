############# Homework 1 - Problem 3 ###########

# Load the dataset
rawData <- read.csv(file.choose())

# Looking up rawData dataset
head(rawData)
str(rawData)
summary(rawData)

###### a. #######

# 1st way of normalizing the data to a standard normal distribution
# Creating normalized data in Ndata
Ndata <- rawData

normalize <- function(data1, data2){
  data1 <- (data2 - mean(data2))/sd(data2)
  return(data1)
}

Ndata$A <- normalize(Ndata$A, rawData$A)
Ndata$B <- normalize(Ndata$B, rawData$B)
Ndata$C <- normalize(Ndata$C, rawData$C)
Ndata$D <- normalize(Ndata$D, rawData$D)

head(Ndata$A)
head(Ndata)

# ran <- function(data){
#   r <- range(data)
#   return(abs(r[1]-r[2]))
# }


########## b. and c. ###########

# Creating boxplots of all variables in rawData

boxData <- function(data, var, L, ON){
           b <-  ggplot(data, aes(x=factor(0), y=var)) +
              geom_boxplot(size=2) + geom_jitter(alpha=0.3) +
              geom_point(alpha=0.5, size=2) + labs(x=L, y=ON) +
              theme(
                  axis.title.x = element_text(size=20),
                  axis.title.y = element_text(size =20),
    
                  axis.text.x = element_text(size=20),
                  axis.text.y = element_text(size=20)
              )
           return(b)
}

multiplot(boxData(rawData, rawData$A), boxData(rawData, rawData$B),
          boxData(rawData, rawData$C), boxData(rawData, rawData$D), col=4)

multiplot(boxData(Ndata, Ndata$A), boxData(Ndata, Ndata$B), 
          boxData(Ndata, Ndata$C), boxData(Ndata, Ndata$D))

multiplot(boxData(rawData, rawData$A, 'A', 'Original'), boxData(rawData, rawData$B, 'B', 'Original'),
          boxData(rawData, rawData$C, 'C', 'Original'), boxData(rawData, rawData$D, 'D', 'Original'), cols=2)
          
multiplot(boxData(Ndata, Ndata$A, 'A', 'Normalized'), boxData(Ndata, Ndata$B, 'B', 'Normalized'), 
          boxData(Ndata, Ndata$C, 'C', 'Normalized'), boxData(Ndata, Ndata$D, 'D', 'Normalized'), cols=2)

multiplot(boxData(rawData, rawData$A, 'A', 'Original'), boxData(rawData, rawData$B, 'B', 'Original'),
          boxData(rawData, rawData$C, 'C', 'Original'), boxData(rawData, rawData$D, 'D', 'Original'),

          boxData(Ndata, Ndata$A, 'A', 'Normalized'), boxData(Ndata, Ndata$B, 'B', 'Normalized'),
          boxData(Ndata, Ndata$C, 'C', 'Normalized'), boxData(Ndata, Ndata$D, 'D', 'Normalized'), cols=4)
summary(Ndata) == summary(rawData)
str(Ndata) == str(rawData)
Ndata == rawData

sumNdata <- summary(Ndata)
sumrawData <- summary(rawData)
(sumNdata$A[5] - sumNdata$A[2]) == (sumrawData$A[5] - sumrawData$A[2])
########## e. ###########

rangeNA <- range(Ndata$A)
rangeRA <- range(rawData$A)
(rangeNA[2]-rangeNA[1]) == (rangeRA[2]-rangeRA[1])
mean(Ndata$A) == mean(rawData$A)
head(Ndata)
head(rawData)



# Scatter plot of variables A and B

ggplot(data=rawData, aes(x=A, y=B, color=C)) +
  geom_point(size=4) + labs(x = 'A', y='B', title='Scatterplot of A and B') + 
  theme(
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size =20),
    
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=20),
    
    legend.text = element_text(size=20),
    legend.title = element_text(size=20)
    
  )
cor(rawData$A, rawData$B)


############## THE END ###############
