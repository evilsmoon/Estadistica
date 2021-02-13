
# #1. Si se desea predecir el ancho de una lancha en función de 
# la potencia del motor. Determine si existe algún valor influyente.
# Justifique su respuesta indicando el treshold.

x = c(98,98,98,93.5,93.5,96,90,94,96,92,91)
y = c(400,340,400,340,320,350,310,310,350,330,330)

n = length(x)
reg = lm(y~x)
hi = influence(reg)$hat

lum = 6/n

hi > lum
# como se puede observar no supera mi lumbral de 0.5454
# En el diagrama de dispersión del literal anterior,
# se observa algún outlier?. Justifique su respuesta, numéricamente. 
rs = rstandard(reg)
plot(rs~x)
# En en grafico se puede observar que mis datos no sobrepoasa el umbral de -2 y 2 

# 3. Si se desea predecir la velocidad de la lancha tomando en cuenta 
# el ancho máximo de la lancha. Compruebe gráficamente si se cumple con 
# la propiedad de homocedasticidad. 
x3 = c(98,98,98,93.5,93.5,96,90,94,96,92,91)

y3 = c(47.5,44.9,47.3,44.5,44.5,42.5,45.8,42.8,43.2,45.3,47.7)

reg3= lm(y3~x3)

rs3 = rstandard(reg3)
plot(rs3~x3)

# se puede deducir que mi varianza no es constante ya que en la grafica se 
# puede obsevar en forma de una campana

# 5. Indique el cuál es el valor del residual estandarizado
# de la Mastercraft X-1, para el literal 3.

## en la posicion 7 se encuentra Mastercraft X-1=0.38705854

# 6. Si se desea conocer cuál es la velocidad de una lancha tomando en cuenta 
# u potencia. Indique el vector de los valores influyentes y en qué gráfica podría
# identificar la existencia de un valor influyente.


x6 = c(400,340,400,340,320,350,310,310,350,330,330)
y6 = c(47.5,44.9,47.3,44.5,44.5,42.5,45.8,42.8,43.2,45.3,47.7)

n6 = length(x6)
reg6 = lm(y6~x6)
hi6 = influence(reg6)$hat

lum6 = 6/n6

hi6 > lum6

plot(hi6~x6)
