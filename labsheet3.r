Data_Frame <- data.frame (  
Training = c("Strength", "Stamina", "Other"),  
Pulse = c(100, 150, 120),  
Duration = c(60, 30, 45)  
)  
print(Data_Frame)  
vec1 = c(1,2,3)  
vec2 = c("R","Scilab","Java")  
vec3 = c("For prototyping","For prototyping","For Scaleup")  
df = data.frame(vec1,vec2,vec3)  
print(df) 
print(df[1:2,]) 
pd=data.frame("Name"=c("Senthil","  
Senthil","Sam","Sam"),  
"Month"=c("Jan","Feb","Jan","Feb"),  
"BS" = c(141.2,139.3,135.2,160.1),  
"BP" = c(90,78,80,81))  
pd2 = subset(pd,Name=="Senthil" | BS> 150 )  
print("new subset pd2")  
print(pd2) 
my_list <- list(a = 1, b = c(2, 3, 4), c = "Hello") 
print(my_list[[1]])
print(my_list$b)           
print(my_list[["c"]]) 
print(my_list[c(1, 2)])   
my_matrix <- matrix(1:9, nrow = 3, byrow = TRUE) 
print(my_matrix[2, 3] )     
print(my_matrix[1, ] )     
print(my_matrix[, 2])
list1 <- list(a = 1:3, b = c("A", "B", "C")) 
list2 <- list(c = c(4, 5, 6), d = c("X", "Y", "Z")) 
combined_cbind <- cbind(list1, list2) 
print("Combined Lists using cbind():") 
print(combined_cbind)