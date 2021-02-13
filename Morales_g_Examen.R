library(readr)
wdbc <- read_csv("wdbc.data" ,col_names = FALSE)
#View(wdbc)
datos <- as.data.frame(wdbc)


datos2 <- datos[,-1] #Delete ID

# convirtiendo los tumores Malignos en 1 
# y los Benignos en 0
datos2$X2[datos2$X2=="M"] <- 1
datos2$X2[datos2$X2=="B"] <- 0


X2 <- datos2$X2
X3 <- datos2$X3
X4 <- datos2$X4
X5 <- datos2$X5
X6 <- datos2$X6
X7 <- datos2$X7
X8 <- datos2$X8
X9 <- datos2$X9
X10<- datos2$X10
X11<- datos2$X11
X12<- datos2$X12
X13<- datos2$X13
X14<- datos2$X14
X15<- datos2$X15
X16<- datos2$X16
X17<- datos2$X17
X18<- datos2$X18
X19<- datos2$X19
X20<- datos2$X20
X21<- datos2$X21
X22<- datos2$X22
X23<- datos2$X23
X24<- datos2$X24
X25<- datos2$X25
X26<- datos2$X26
X27<- datos2$X27
X28<- datos2$X28
X29<- datos2$X29
X30<- datos2$X30
X31<- datos2$X31
X32<- datos2$X32


M <-  cbind(x2,X3 ,X4 ,X5 ,X6 ,X7 ,X8 ,X9 ,X10 ,X11 ,X12 ,X13 ,X14 ,X15 ,X16 ,X17 ,X18 ,X19 ,X20 ,X21 ,X22 ,X23 ,X24 ,X25 ,X26 ,X27 ,X28 ,X29 ,X30 ,X31 ,X32)


C <- cor(M)

#Multicolinealidad
MC <- C
MC[MC == 1] <- 0

max(MC)
## el max 0.9978553
min(C)
## el min -0.3116308

dim(C)
 # dimension de 30 x 30

#3

d = dist(datos2, method = "euclidean")
max(d)
## el max 4739.089
min(d)
## el min 3.815967
# dimension de 30 x 30


# 4

Vx5 <- as.integer(X5) #Variable 5
y<- c(1:length(Vx5)) 

n = 5 # ventana

ma <- function(y,n){filter(y,rep(1/n,n), sides=1)} 
wind = ma(y,n) 
pr = wind[n:(length(wind)-1)] 
err_pr = y[n+1:(length(y)-n)]-pr 
sum_err_pr = sum(err_pr) 
err_pr2 = err_pr^2 
CME = sum(err_pr2)/length(pr) 

# Resp CME 9


# 5

y2 <- as.numeric(X2)


reg = glm(y2~X3+X4+X5+X6+X7,family = binomial())


yest = exp(reg$coefficients[1]+reg$coefficients[2]*X3+
             reg$coefficients[3]*X4+reg$coefficients[4]*X5+
             reg$coefficients[5]*X6+reg$coefficients[6]*X7)/
              (1+exp(reg$coefficients[1]+reg$coefficients[2]*X3+
                       reg$coefficients[3]*X4+reg$coefficients[4]*X5+
                       reg$coefficients[5]*X6+reg$coefficients[6]*X7))


min_max <- sort(yest)
# min : 0.1064938
min(min_max)
# max : 0.4845855
max(min_max)
#literal 10
yest1 = round(yest)

error = ((yest1==y2)*1) #10
porc_acert = sum(error)/length(y2) # accuracy --- exactitud

# resp accuracy : 0.0228471



library('REdaS')

kmo = KMOS(x=datos2)


# 7

matriz = cbind(X3 ,X4 ,X5 ,X6 ,X7 ,X8 ,X9 ,X10 ,X11 ,X12 ,X13 ,X14 ,X15 ,X16 ,X17 ,X18 ,X19 ,X20 ,X21 ,X22 ,X23 ,X24 ,X25 ,X26 ,X27 ,X28 ,X29 ,X30 ,X31 ,X32)
pca = prcomp(matriz, center = TRUE, scale. = TRUE) 
plot(pca, type = "l")
summary(pca)

# 8


# Componentes Principales
cp = predict(pca,matriz)
cp = as.data.frame(cp)


reg_2=lm(y ~ cp$PC1+cp$PC2+cp$PC3+cp$PC4+cp$PC5+cp$PC6+cp$PC7+cp$PC8+cp$PC9+cp$PC10+cp$PC11+cp$PC12+cp$PC13+cp$PC14+cp$PC15+cp$PC16+cp$PC17+cp$PC18+cp$PC19+cp$PC20+cp$PC21+cp$PC22+cp$PC23+cp$PC24+cp$PC25+cp$PC26+cp$PC27+cp$PC28+cp$PC29+cp$PC30)
summary(reg_2)
# 9 