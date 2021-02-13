#########Cargar Datos########
#install.packages("readr")
library(readr)
iris <- read_csv("~/Documents/UPS/Estadistica/iris.csv", col_names = FALSE)
iris = data.frame(iris)

#####Escalamiento Multidimensional######
d = dist(iris[,1:4], method = "euclidean")
fit = cmdscale(d,eig=TRUE, k=3) # k es el numero de dimensiones
x = fit$points[,1] 
y = fit$points[,2]
plot(x,y)
text(x, y, labels = row.names(iris), cex=1)

#Identificaci?n de las Clases
clase = as.factor(iris$X5)
plot(x,y,col=c("red","green3","blue")[clase], main = "Iris Dataset Original")

install.packages('corrplot')
library('corrplot')
