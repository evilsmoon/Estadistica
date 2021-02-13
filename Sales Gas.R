####################################################
##############    Ventas Gasolina    ############### 
####################################################

#Datos del Ejercicio:
y = c(17,21,19,23,18,16,20,18,22,20,15,22) #vector de ventas
x = c(1:length(y)) #vector de semanas


#Grafico de Dispersi�n
par(mfrow=c(2,2)) # varios gr�ficos en una sola pantalla
plot(x,y, type="l", pch = 19, col="red",main="Serie de Tiempo Venta Gasolina") #grafico de dispersion

#########################################
############ Promedio Movil #############
#########################################

n = 6 # Tama�o de la ventana
ma <- function(y,n){filter(y,rep(1/n,n), sides=1)} # funcion que permite mover la ventana por la serie de tiempo
wind = ma(y,n) # promedios moviles en la serie de tiempo  
pr = wind[n:(length(wind)-1)] #pronostico con el promedio movil
err_pr = y[n+1:(length(y)-n)]-pr #error del pronostico
sum_err_pr = sum(err_pr) #suma de error de pronostico
err_pr2 = err_pr^2 #error de pronostico al cuadrado
CME = sum(err_pr2)/length(pr) #cuadrado medio debido al error

plot(x,y, type="l", pch = 19, col="red",main="Serie de Tiempo Venta Gasolina y Pronosticos Moviles") #grafico de dispersion
par(new=TRUE)
lines(x[n+1:(length(wind)-n)],pr, type="l", pch = 19, col="blue") #grafico de dispersion del pronostico con el promedio movil

#########################################
###### Suavizamiento Exponencial ########
#########################################

alpha = 0.16 #valor de la constante de suavizamiento
sua = HoltWinters(y, alpha, beta=FALSE, gamma=FALSE) #suavizamiento exponencial
s1 = sua$fitted[,1] #valores del suavizamiento exponencial
err_pr_sua = y[2:(length(y))]-s1 #error del pronostico con suavizamiento
err_pr_sua2 = err_pr_sua^2 #error de pronostico con suavizamiento al cuadrado
CME_s = sum(err_pr_sua2)/length(s1) #cuadrado medio debido al error con suavizamiento

plot(x,y, type="l", pch = 19, col="red",main="Serie de Tiempo Venta Gasolina y Suvizamiento Exponencial") #grafico de dispersion
par(new=TRUE)
lines(x[2:(length(y))],s1, type="l", pch = 19, col="blue") #grafico de dispersion del pronostico con suavizamiento exponencial