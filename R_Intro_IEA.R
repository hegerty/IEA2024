#############################################
# Introduction to R in Research and Teaching
# Scott W. Hegerty
# Illinois Economics Association Workshop
# October 25, 2024
# Please see www.github.com/hegerty/IEA2024
############################################


#### 1. Some Basic Operations in R
2 + 3 #(addition, powers, etc)
2*3
2^3
(12+3)/5


# Groups and objects
c1<-c(3,5,7,9)
c1
c1[4] # select an element or elements
c1[2:3] # a range

# Combining rows and columns; removing subsets
cbind(c1,c1)
rbind(c1[-1],c1[-3])

#### 2. Matrix Operations and subsetting

matrix(1:30,nrow = 5,ncol=6) # Create a matrix (5x6)
matrix1<-matrix(1:30,nrow = 5,ncol=6) # Store as an object
t(matrix1) # transpose the matrix
matrix2<-matrix1 # make a copy
dim(matrix2)<-c(2,15) # can reshape!
matrix2
matrix1

# Subsetting rows, columns
matrix1
matrix1[,1] # column 1
matrix1[1,] # row 1
matrix1[-1,] # Omit row 1
matrix1[2:3,]
matrix1[,c(1,3,5)]

# subset based on a condition: > , <, <=, >=
matrix1>7 # returns a logical statement
sum(matrix1>7) # reads [T,F] as [0,1] 
matrix1>=7
sum(matrix1>=7)

#### 3. Opening and examining data
# Open a dataset (this one I randomly generated to work specifically)
data=read.csv("https://raw.githubusercontent.com/hegerty/IEA2024/refs/heads/main/TestDataIEA24.csv")

head(data) # first five; can customize
tail(data,3)
colnames(data)
ncol(data)
nrow(data)
dim(data)
dim(data)[2] # second of rows, columns

# plot data: various identical ways
plot(data$x,data$y)
plot(data[,c(4,2)]) 

#### 4, Basic statistical operations
attach(data) # works directly with this dataset
mean(x)
sd(x)
median(x)
min(x)
quantile(x,.35) #pick whichever!
summary(x) # note that previous value is between 25 and 50%

mean(y)
sd(y)
summary(y)

mean(z)
round(c(mean(x),mean(y),mean(z)),3) # can add additional techniques

## T-test
t.test(x,y)
t.test(z,y)
ttest1<-t.test(z,y)
ttest1$p.value

## Split y into two halves
length(y)
y1<-y[1:length(y)/2] # Specifoc values of y
y2<-y[(length(y)/2+1):length(y)]
mean(y1)
mean(y2)
t.test(y1,y2)

## Correlations and scatterplots
plot(x,y,pch=20,col="blue", xlab="Independent Variable", ylab = "Dependent Variable", main="Scatterplot #1")
# Can customize plots
cor(data[,-1])
round(cor(data[,-1]),3)


#### 5. Regression Analysis
## Bivariate regression
reg1<-lm(y~x) # "linear model"
summary(reg1)
abline(reg1) # regression line (a + bx)

## Multivariate regression 
reg2<-lm(y~x+z+z2)
summary(reg2)
plot(z,y,pch=18,col="#003DA7", xlab="Independent Variable z2", ylab = "Dependent Variable y", main="Scatterplot #2")
abline(reg2, lwd=5,lty=2, col="#FDB813")

# Can use output as object, subset, etc.
summary(reg2)[4]

# 6. Use a package for a nonparametric bivariate regression

# "Brute force" method at 
# https://github.com/hegerty/ECON346/blob/main/Lec09b_nonparbeta.R

# install.packages("mblm")
library(mblm)
y<-as.vector(y[1:100]); x = as.vector(x[1:100]) # less computationally intensive
np2<-mblm(y~x,repeated = FALSE)
summary(np2)
summary(lm(y~x)) # regular OLS

#### Extra topics/customization


