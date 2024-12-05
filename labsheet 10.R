#Level 1

require(foreign)
require(MASS)
mydata<- 
  read.csv("http://www.karlin.mff.cuni.cz/~pesta/prednasky/NMFM404/Data/binary.csv") 
head(mydata) 
summary(mydata) 
sapply(mydata, sd) 
xtabs(~admit + rank, data = mydata) 
mydata$rank <- factor(mydata$rank) 
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial") 
summary(mylogit)
null_model <- glm(admit ~ 1, data = mydata, family = "binomial") 
anova(null_model, mylogit, test = "Chisq") 

#Level 2

install.packages("pscl")

library(pscl) 
pR2(mylogit) 

multiple_regression_model <-  lm(gpa ~ gre + rank, data = mydata)
summary(multiple_regression_model)
