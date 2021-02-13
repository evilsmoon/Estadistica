#Deber 10
library(readr)
house_prediction <- read_csv("house_prediction.csv")
View(house_prediction)
datos <- data.frame(house_prediction)

#limpieza de los datos
sapply(datos,function(x) sum(is.na(x))) 
#busca en todas las variables los valores faltantes (NA)

#Verificacion de Datos NA
sum(is.na(datos)) # suma total de datos faltantes (NA)
datoslimpios =na.omit(datos) #eliminar los NA

#Limpieza de Datos 
mediaLimpios = mean(datoslimpios$MasVnrArea) #media de los datos limpios
datos$MasVnrArea[is.na(datos$MasVnrArea)] = mediaLimpios #reemplazamos la media en NA
sapply(datos,function(x) sum(is.na(x))) #busca en todas las variables los valores
#faltantes (NA)
sum(is.na(datos)) # suma total de datos faltantes (NA)

y = datos$SalePrice
x1 =datos$MSSubClass
x2 = datos$LotArea
x3 = datos$OverallQual
x4 = datos$OverallCond
x5 = datos$YearBuilt
x6 = datos$YearRemodAdd
x7 = datos$MasVnrArea
x8 = datos$BsmtFinSF1
x9 = datos$BsmtUnfSF
x10 = datos$TotalBsmtSF
x11 = datos$X1stFlrSF
x12 = datos$X2ndFlrSF
x13 = datos$LowQualFinSF
x14 = datos$GrLivArea

reg=lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)
summary(reg)

#Matriz de Variables Independientes
matriz = cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14)
pca = prcomp(matriz, center = TRUE, scale. = TRUE) 
print(pca)
plot(pca, type = "l")
summary(pca)

# Componentes Principales
cp = predict(pca,matriz)
cp = as.data.frame(cp)

#Regresion con pca
reg_2=lm(y ~ cp$PC1+cp$PC2+cp$PC3+cp$PC4+cp$PC5+
             cp$PC6+cp$PC7+cp$PC8+cp$PC9+cp$PC10+
             cp$PC11+cp$PC12)
summary(reg_2)

#Comprobacion
#Regresion hacia atras 
reg_atras=step(lm(y~x1+x2+x3+x4+x5+x6+x7+x8+
                    x9+x10+x11+x12+x13+x14),direction = "backward")
#Regresion hacia atras con 12 variables
reg_12=lm(y~x10+x13+x4+x6+x2+x7+x1+x8+x5+x11+x3+x12)
summary(reg_12)

