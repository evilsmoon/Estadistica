m=c(98,98,98,93.5,93.5,96,90,94,96,92,91) 
h=c(400,340,400,340,320,350,310,310,350,330,330);
v=c(47.5,44.9,47.3,44.5,44.5,42.5,45.8,42.8,43.2,45.3,47.7) 

n = length(m)

reg =lm(m~v)
summary(reg)

yest = reg$coefficients[1]+reg$coefficients[2]*v
error = m-yest
SCE = sum(error^2)
gl = n-2
ECM = SCE/gl 
s = sqrt(ECM)
x_med = mean(m)
x_xmed = m-x_med
cuad=x_xmed^2
## 1. Calcule el ancho de una lancha cuando 
## la velocidad es de 30 millas por hora.

yest1 = reg$coefficients[1]+ reg$coefficients[2]*30


# 2. La bondad de ajuste del modelo generado en el literal
# 1. Haga un comentario sobre la bondad del ajuste.

# Multiple R-squared:  0.00108,

# Al obtener el summary de nuestra variable reg podemos decir que nuestra 
# bondad de ajuste es optima para poder medir los coeficientes de correlacion

# 3. Calcule un estimador insesgado de la varianza para el literal 1.

s_cuad = ECM

# 4. ¿Indica la prueba t que haya una relación significante entre potencia y 
# velocidad?. Use un nivel de significancia del 95% y el método del 
# valor crítico.

reg_4 = lm(h~v)

summary(reg_4)

## con summry sacamos los valores b_{1} =  7.676  y S_{b}_{1} =  4.972

s_b_1=4.972  
b_1=7.676 

t= b_1/s_b_1 # valor t
alfa = 0.05

# p-value: 0.157

# Dado a conocer mi p-value con nuestro comando summary()
# nos damos cuenta que nuestro 0.157 es mayor que 0.05
# deducimos que nuestra hipotecis es nula por ende aceptamos

# 5. Para el literal anterior, determine el intervalo de confianza 
# del coeficiente de regresión que representa a la pendiente, del 
# modelo de regresión lineal simple.



# 6. Pruebe si la relación entre ancho de la lancha y velocidad 
# es significante usando la prueba F, con un nivel de significancia 
# del 99%. Use el método del p-value (indique el valor). ¿Cuál es la conclusión?

reg_6 = lm(v~m)

summary(reg_6)


# concluimos que no hay significancia por que nuestro p-value 
# es mayor que nuestro alfa por ende aceptamos la hipotesis nula

# 7. Para el literal anterior, indique el error estándar de estimación 
# del coeficiente de regresión que representa al corte con el eje y, del
# modelo de regresión lineal simple.


# Residual standard error: 1.951 on 9

# 8. La revista Water Ski, estimó que el costo (en USD) de cada modelo se 
# puede determinar por la función: LaTeX: c\left(x\right)=3x^3+1c ( x ) = 3 x 3 + 1,  
# donde la variable independiente representa la velocidad. Determine cuál
# es el costo de todos los sky para todos los fabricantes.

costo= 3*((v)^3)+1
reg_9=lm(m~h+costo);
summary(reg_9);
yest_9 = reg_9$coefficients[1] + reg_9$coefficients[2]*h + reg_9$coefficients[3]*costo
# 9. Calcule, el ancho de una lancha cuando la potencia 
# es de 400 HP y el costo es de 200 000 USD.

yest_9 = reg_9$coefficients[1] + reg_9$coefficients[2]*400 + reg_9$coefficients[3]*200000


# 10. Cuál es la bondad de ajuste del modelo generado en el literal 9. y 
# haga un comentario sobre la bondad del ajuste.

# Multiple R-squared:  0.7657,

# Podemos decir que la  bondad de ajuste es optima
# para  ser medida por el coeficiente de determinacion

# 11. Para el literal anterior, se cumple que las perturbaciones 
# siguen una distribución normal. Justifique su respuesta, gráficamente. 

plot(m,h)
# Hay una perturbacion ya que existe un valor
# influyente que se encuentra fuera del rango 

# 12. Indique la ecuación de regresión que permite predecir la potencia 
# en función del ancho y velocidad.

reg_12=lm(h~m+v);


yest_12 = reg_12$coefficients[1] 
+ reg_12$coefficients[2]*m 
+ reg_12$coefficients[3]*v

# 13. Para el literal anterior, basados en el criterio del residual
# estandarizado? Existe algún outlier?

rs = rstandard(reg_12)
  
# hay un outlier en la posicion 2 que sobre pasa el umbral de 2 y -2

# 14. Indique los residuales eliminados estudentizados, del literal 12. 
# Empleando como nivel de significancia 0.05, ¿puede clasificarse 
# cualquiera de estas observaciones como observación atípica? Explique.


# los residuales eliminados son en le manga= 91 , 
# EN HP =330 Y velocidad=47.7 este valor era influyente 
# se encontraba fuera del rango

# 15. Cuáles son los valores de influencia, del literal 12. 
# ¿Parece haber alguna observación influyente en estos datos? Explique.

hi =influence(reg_12)$hat

# 16. Calcule la distancia de Cook para la regresión del literal 12.
# Qué conclusión se obtiene?

cook = cooks.distance(reg_12) 
