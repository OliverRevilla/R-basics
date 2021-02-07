##### Basic Structures #############
library(dslabs)
datos <-data(murders)
class(datos)
class(murders)
str(murders) #function str shows us the structure of the object.
head(murders) #shows the first six lines
length(murders$state)#shows the length of the vector

# Levels
region <- murders$region
value <- murders$total
region <- reorder(region,value, FUN = mean) #levels according its standard deviation.


#Working with matrices
mat <- matrix(1:20,5,4) #first version
print(mat)

A <- matrix(c(5,7,13,4,1,7,14,3,11), 
            nrow = 3,ncol = 3, byrow = FALSE) #second version
#nrow : quantity of rows
#ncol : quantity of columns
#byrow : how do i want to fill the data
dimnames(A) = list(c("Blanco","Negro","Rojo"),
                   c("Toyota","Audi","Nissan"))
#function dinnames
print(A)

library(Matrix)
#to aggregate rows and columns, we use the fuctions rbind() and 
# cbind() respectively.
Azul <- c(8,5,7)
Hyundai <- c(2,7,3,5)
A <- rbind(A,Azul)
A <- cbind(A,Hyundai)
print(A)

#Transposed
print(t(A)) # use t

#determinant
print(det(A))

#inverse
print(solve(A)) #use solve()

#diagonal
print(diag(A))

#To convert matrices into data frames
as.data.frame(mat)

murders[2:3,]
table(murders$region) #The function table takes a vector and returns the frecuency

###### Vectors #####
temp <- c(35,46,45,47,48,50)
city <- c("Beijing","Lagos","París"
          ,"Río de Janeiro","San Juan","Toronto")
names(temp) <- city

print(temp)
temp[c("Beijing","Lagos")]

vector <- c(12:73)
impares <- seq(1,100,5)
impares
barplot(impares)

################## Data Frames ###################
temp <- c(35,88,42,84,81,30)
city <- c("Beijing","Lagos","Paris","Rio de Janeiro","San Juan","Toronto")
city_temps <- data.frame(name = city, temperatura = temp)
View(city_temps)







