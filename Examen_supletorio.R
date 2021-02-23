library(readr)

# 1. Consumir el archivo wine.data en formato CSV. 
# Todos los literales posteriores trabajarán con este dataset
wine <- read_csv("wine.data" ,col_names = FALSE)
View(wine)

# 2. Obtenga la ecuación de regresión estimada en la que 
# se emplee Malic acid y Total phenols para predecir el Color intensity

x1 = wine$X2 # Malic acid
x2 = wine$X6 # Total phenols
y  = wine$X10 # Color intensity

reg = lm(y~x1+x2)

yest=reg$coefficients[1]+reg$coefficients[2]*x1+reg$coefficients[3]*x2
# La bondad de ajuste del modelo generado en el literal
# 2. Haga un comentario sobre la bondad del ajuste
 summary(reg)


# Multiple R-squared:  0.0616
 
# Existe un 6.167% por lo cuando nuestra bonda de ajunte es mala

# ¿Indica la prueba t que hay una relación significante entre las variables
# independientes y dependiente del modelo del literal 2?. Use un nivel de 
# significancia del 95% y el método del valor crítico


# prubea de x1 t 1.645 > 0.30401 [1] TRUE
# prueba de x2 t 1.645 > 0.00521 [1] TRUE
 
# 5. Para el literal anterior, determine el intervalo de confianza del
# coeficiente de regresión que representa a la pendiente, del modelo de regresión 
# lineal (0.5 puntos).

b1= 0.055289  
sb1=0.053631  
b2= 0.008624   
sb2= 0.003048   

i_conf_b1_pos = b1+1.645*(sb1) # 0.143512
i_conf_b1_neg =  b1-1.645*(sb1)# -0.032934

i_conf_b2_pos = b2+1.645*(sb2) # 0.01363796
i_conf_b2_neg =  b2-1.645*(sb2) # 0.00361004

# 6. El modelo del literal 2, 
# tiene homocedasticidad? 
plot(reg)
#Su varianza es constante 

# 7. Para el literal 2, se cumple que las perturbaciones siguen una 
# distribución normal. Justifique su respuesta, gráficamente. 

# dentro de la grafica normal Q-Q se puede obsevar permutaciones en el dato 51 111 y 125

# 8. Existe algún outlier, para el literal 2?. Utilice todos los métodos posibles 
# justifique sus respuestas. Emplee como nivel de significancia 0.05 (1 punto). 
plot(reg)
rs= rstandard(reg)
sort(rs)
# hay outlaier en la pos :  
# 3          14          96          15         100          51         125         111 
# superal el umbral de -2 a 2 esto se puede ver de mejor mejor con el grafico Normal Q-Q

#  9. Existe algún valor influyente, para el literal 2?. 
# Utilice todos los métodos posibles y justifique sus respuestas (1 punto). 

inf=influence(reg)
#datos influyentes
hi=inf$hat

n = length(y)

umbral  = 3*(2+1)/n 
hi > umbral
# no hay niguun valor influyente 

# 10. Obtenga la ecuación de regresión estimada en la que se emplee Flavanoids,
# Magnesium y Alcohol para predecir el Color intensity  (1 punto).


x11 = wine$X7
x12 = wine$X5
x13 = wine$X1

reg1 = lm(y~x11+x12+x13)

yest1=reg1$coefficients[1]+reg1$coefficients[2]*x11+reg1$coefficients[3]*x12+reg1$coefficients[4]*x13

#11. Calcule la bondad de ajuste e indique la variable que 
# aporta más información en el modelo del literal 10 

summary(reg1)

#la variable que mas aporta x11 0.475521   0.078618   6.048 8.71e-09 ***
# Multiple R-squared:  0.3838
# Se puede no es un buen modelo ya que mi bondad de ajunte no supera el 0.7

# 12. Para el literal 10, indique si existe un outlier considerando 
# el criterio de los residuales estadarizados (1 punto).
plot(reg1)

rs1= rstandard(reg1)
sort(rs1)
# Si hay outlayers en las posisicones

#  159          100          125           51 
# 70           79          111           96 


# 13. Eliminando la categoría 3 de la variable "Alcohol" Genere un modelo de regresión que le permita predecir las dos clases de alcohol en función de Flavanoids y Magnesium. 
# Indique los coeficientes de regresión del modelo (no dividir el dataset) 

wine2 <- wine[-3,] #Delete ID
wine2 <- wine[,-14] #Delete ID

View(wine2)

######### Training (70% de los datos)

X1 =  wine2$X1
x2 =  wine2$X2
X3 =  wine2$X3
X4 =wine2$X4
X5 =wine2$X5
X6 =wine2$X6
X7 =wine2$X7
X8 =wine2$X8
X9 =wine2$X9
X10 =wine2$X10
X11 =wine2$X11
X12 =wine2$X12
X13 =wine2$X13
set.seed(7)

rows <- sample(nrow(wine2))

wine2<-wine2[rows,]
wine2
d = dim(wine2)

training  = round(d[1]*0.7)

x1_tr <- X1[1: training ]
x2_tr <- X2[1: training ]
x3_tr <- X3[1: training ]
x4_tr <- X4[1: training ]
x5_tr <- X5[1: training ]
x6_tr <- x6[1: training ]
x7_tr <- X7[1: training ]
x8_tr <- x8[1: training ]
x9_tr <- x9[1: training ]
x10_tr <- X10[1: training ]
x11_tr <- X11[1: training ]
x12_tr <- X12[1: training ]
x13_tr <- X13[1: training ]




# 16. Calcular la matriz de correlaciones y de distancias de todo el dataset original e
# indicar cuáles son las dimensiones de ambas matrices 



M <- cbind(wine$X1,wine$X2,wine$X3,wine$X4,wine$X5,wine$X6,wine$X7,wine$X8,wine$X9,wine$X10,wine$X11,wine$X12,wine$X13)
C <- cor(M)

#Multicolinealidad
MC <- C
MC[MC == 1] <- 0

max(MC)
## el max 0.9765953
min(C)

## el minimo -0.8474975




# 17. Realizar una serie de tiempo con promedios móviles (ventana 3)
# para la tercera variable y calcular su CME (1 punto).


n1 = 3
ma <- function(y,n1){filter(y,rep(1/n1,n1), sides=1)} 
wind = ma(y,n1) 
pr = wind[n1:(length(wind)-1)]
err_pr = y[n1+1:(length(y)-n1)]-pr
sum_err_pr = sum(err_pr)
err_pr2 = err_pr^2
CME1 = sum(err_pr2)/length(pr)
wind
