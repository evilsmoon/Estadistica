x = c(1,2,3,4,5,6,7,8,9,10,11,12)
y = c(105,135,120,105,90,120,145,140,100,80,100,110)

### Pregunta 1 ###

#Promedio Movil n = 3

n1 = 3
ma <- function(y,n1){filter(y,rep(1/n1,n1), sides=1)} 
wind = ma(y,n1) 
pr = wind[n1:(length(wind)-1)]
err_pr = y[n1+1:(length(y)-n1)]-pr
sum_err_pr = sum(err_pr)
err_pr2 = err_pr^2
CME1 = sum(err_pr2)/length(pr)
wind

# para el mes 13 = 96.66

#Promedio Movil n = 4
n2 = 4
ma <- function(y,n2){filter(y,rep(1/n2,n2), sides=1)} 
wind = ma(y,n2) 
pr = wind[n2:(length(wind)-1)]
err_pr = y[n2+1:(length(y)-n2)]-pr
sum_err_pr = sum(err_pr)
err_pr2 = err_pr^2
CME2 = sum(err_pr2)/length(pr)
wind
print(CME2)
# para el mes 13 = 97.50

### Pregunta 2 ###
print(CME1)
print(CME2)
#el mejor CME tenmos con la ventan 4 ya que nos da un menor error es decir tenemos un resultado de :709.57

### Pregunta 3 ###

#Suavizamiento Exponencial
alpha1 = 0.3
sua = HoltWinters(y, alpha1, beta=FALSE, gamma=FALSE)
s1 = sua$fitted[,1]
err_pr_sua = y[2:(length(y))]-s1
err_pr_sua2 = err_pr_sua^2
CME_s1 = sum(err_pr_sua2)/length(s1)
sua
print("sua con 0.3 = 106.39")
### Pregunta 4 ###


### Pregunta 5 ###
alpha2 = 0.5
sua = HoltWinters(y, alpha2, beta=FALSE, gamma=FALSE)
s1 = sua$fitted[,1]
err_pr_sua = y[2:(length(y))]-s1
err_pr_sua2 = err_pr_sua^2
CME_s2 = sum(err_pr_sua2)/length(s1)
sua
print("sua con 0.5 = 104.61")
#El mejor pronostico que se pude tomar es con el alpha = 0.3 ya que tiene un coeficiente de 106.39
#y al revisar lo CME tenemos un error menor que con el alplha=0.5

### Pregunta 6 ###
print(CME1)
print(CME_s1)

#Al revisar los promedios de cada metodo tenemos:
#promedios moviles = 96.66 con un CME de = 811.41
#suavizamiento exponencial = 106.36 con un CME = 510.28
#en conclusión el mejor metodo de series de tiempo es con el suavizamiento exponencial ya que nos da
#un error minimo que con el de promedios moviles 
