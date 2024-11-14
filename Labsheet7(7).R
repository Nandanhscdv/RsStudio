library(BSDA)
library(ggplot2)
prop_success<-130
n <- 150 
p0 <- 0.80
prop_test_result <- prop.test(prop_success, n, p = p0, alternative = "two.sided", correct = FALSE) 
print(prop_test_result)
data <- data.frame( 
  Outcome = c("Passed", "Failed"), 
  Count = c(prop_success, n - prop_success) 
) 

ggplot(data, aes(x = Outcome, y = Count, fill = Outcome)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  labs(title = "Student Outcomes with New Teaching Method", 
       x = "Outcome", y = "Number of Students") + 
  geom_text(aes(label = Count), vjust = -0.5) + 
  scale_fill_manual(values = c("Passed" = "green", "Failed" = "red"))

#Two-sample z proportion test

library(BSDA) 
library(ggplot2) 

prop1 <- 90 
n1 <- 150 

prop2 <- 70 
n2 <- 130 

prop_test_2sample <- prop.test(c(prop1, prop2), c(n1, n2), correct = 
                                 FALSE) 

print(prop_test_2sample) 

data <- data.frame( 
  Campaign = c("A", "B"), 
  Successes = c(prop1, prop2), 
  Failures = c(n1 - prop1, n2 - prop2) 
) 

library(reshape2) 
data_melt <- melt(data, id.vars = "Campaign") 
 
ggplot(data_melt, aes(x = Campaign, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "stack") + 
  theme_minimal() + 
  labs(title = "Success and Failure Counts by Campaign", 
       x = "Campaign", 
       y = "Count") + 
  geom_text(aes(label = value), position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values = c("Successes" = "blue", "Failures" = 
                                 "red"), 
                    name = "Outcome")

#Coorelation

set.seed(123) 
data <- data.frame( 
  HoursExercise = rnorm(50, mean = 5, sd = 2),
  
  BodyFatPercentage = rnorm(50, mean = 25, sd = 5) 
) 
 
cor_test_result <- cor.test(data$HoursExercise, 
                            data$BodyFatPercentage, method = "pearson") 
 
print(cor_test_result) 

ggplot(data, aes(x = HoursExercise, y = BodyFatPercentage)) + 
  geom_point(color = "blue") + 
  geom_smooth(method = "lm", col = "red") + 
  theme_minimal() + 
  labs(title = "Correlation between Hours of Exercise and Body Fat Percentage", 
       x = "Hours of Exercise per Week", 
       y = "Body Fat Percentage") + 
  stat_cor(method="pearson",label.x=3,label.y=35)


#one-way ANOVA

aov_result <- aov(Score ~ Method, data = data) 

summary(aov_result) 
 
ggplot(data, aes(x = Method, y = Score, fill = Method)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(title = "Test Scores by Teaching Method", 
       x = "Teaching Method", 
       y = "Test Score") + 
  scale_fill_brewer(palette = "Set2")

