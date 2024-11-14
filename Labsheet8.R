#Chi-Squared Test 

library(ggplot2) 
 
data <- matrix(c(30, 10, 20, 40), nrow = 2) 
rownames(data) <- c("Group 1", "Group 2") 
colnames(data) <- c("Category A", "Category B") 
 
chisq_test_result <- chisq.test(data) 
print(chisq_test_result) 
 
data_df <- as.data.frame(as.table(data)) 
ggplot(data_df, aes(Var1, Freq, fill = Var2)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() + 
  labs(title = "Chi-Squared Test", 
       x = "Group", 
       y = "Count") + 
  scale_fill_manual(values = c("Category A" = "blue", "Category B" = "red"))


#fisher exact test

data<-matrix(c(5, 1, 3, 6), nrow = 2) 
rownames(data) <- c("Treatment", "Control") 
colnames(data) <- c("Success", "Failure") 

fisher_test_result <- fisher.test(data) 
print(fisher_test_result) 

data_df <- as.data.frame(as.table(data)) 
ggplot(data_df, aes(Var1, Freq, fill = Var2)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() + 
  labs(title = "Fisher Exact Test", 
       x = "Group", 
       y = "Count") + 
  scale_fill_manual(values = c("Success" = "green", "Failure" = "orange"))

#wilcoxon rank sum test(Mann-Whitney U Test)

group1 <- c(5, 7, 8, 5, 6) 
group2 <- c(6, 9, 10, 4, 7) 
wilcox_test_result <- wilcox.test(group1, group2) 
print(wilcox_test_result) 
 
group <- factor(c(rep("Group 1", length(group1)), rep("Group 2", 
                                                      length(group2)))) 
values <- c(group1, group2) 
data_df <- data.frame(group, values) 
ggplot(data_df, aes(x = group, y = values, fill = group)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(title = "Wilcoxon Rank Sum Test", 
       x = "Group", 
       y = "Values") + 
  scale_fill_manual(values = c("Group 1" = "purple", "Group 2" = "yellow")) 


#Wilcoxon Signed Rank Test


before <- c(85, 78, 92, 88, 76) 
after <- c(89, 81, 95, 85, 79) 

wilcox_signed_rank_test_result <- wilcox.test(before, after, paired = TRUE) 
print(wilcox_signed_rank_test_result) 
 
data_df <- data.frame( 
  Time = rep(c("Before", "After"), each = length(before)), 
  Value = c(before, after) 
) 
ggplot(data_df, aes(x = Time, y = Value, fill = Time)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(title = "Wilcoxon Signed Rank Test", 
       x = "Time", 
       y = "Values") + 
  scale_fill_manual(values = c("Before" = "cyan", "After" = "magenta")) 
5. Kruskal-Wallis Test 
Purpose: Compare more than two independent groups. 

groupA <- c(5, 7, 6, 8) 
groupB <- c(6, 9, 10, 4) 
groupC <- c(7, 8, 7, 6) 
 
data <- data.frame( 
  values = c(groupA, groupB, groupC), 
  groups = factor(rep(c("A", "B", "C"), each = 4)) 
) 
 
kruskal_test_result <- kruskal.test(values ~ groups, data = data) 
print(kruskal_test_result) 

ggplot(data, aes(x = groups, y = values, fill = groups)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(title = "Kruskal-Wallis Test", 
       x = "Group", 
       y = "Values") + 
  scale_fill_manual(values = c("A" = "pink", "B" = "lightblue", "C" = 
                                 "lightgreen"))
