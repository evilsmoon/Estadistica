library(readr)

abalone <- read_csv("/home/evils/Documents/UPS/Estadistica/abalone.csv", 
                    col_names = FALSE)
View(abalone)

abalone = abalone[1:100,]

abalone2=abalone[abalone$X1!="I",]

length(abalone2)
length(abalone2$X1)

abalone$X1[abalone$X1=="I"] <- NA

datos = which(is.na(abalone$X1))

#################################################################3


x1 <- abalone2$X1
x2 <- abalone2$X2
x3 <- abalone2$X3
x4 <- abalone2$X4
x5 <- abalone2$X5
x6 <- abalone2$X6
x7 <- abalone2$X7
x8 <- abalone2$X8
x9 <- abalone2$X9

#############################3
#mean_x1 <- mean(x1)
mean_x2 <- mean(x2)
mean_x3 <- mean(x3)
mean_x4 <- mean(x4)
mean_x5 <- mean(x5)
mean_x6 <- mean(x6)
mean_x7 <- mean(x7)
mean_x8 <- mean(x8)
mean_x9 <- mean(x9)
#############################

na_x1 <- which(is.na(x1))
na_x2 <- which(is.na(x2))
na_x3 <- which(is.na(x3))
na_x4 <- which(is.na(x4))
na_x5 <- which(is.na(x5))
na_x6 <- which(is.na(x6))
na_x7 <- which(is.na(x7))
na_x8 <- which(is.na(x8))
na_x9 <- which(is.na(x9))

#############################

x1 = ((x1=='M')*1) # cambio a ceros y unos


reg4 = glm(x1~x2+x3+x4+x5+x6+x7+x8+x9,family = binomial())

#yest4 = reg4$coefficients[1]+reg4$coefficients[2]*x2+reg4$coefficients[3]*x3+reg4$coefficients[4]*x4+reg4$coefficients[5]*x5+reg4$coefficients[6]*x6+reg4$coefficients[7]*x7+reg4$coefficients[8]*x8+reg4$coefficients[9]*x9

#yest4

#summary(reg4)

mm = cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)
mc = cor(mm)

dimension = dim(mc)

#Multicolinealidad
mc2 = mc
mc2[mc2 == 1] <- 0

max(mc2)
min(mc)
######### Training (70% de los datos)
abalone2$X1 = ((abalone2$X1=='M')*1)

set.seed(7)

rows <- sample(nrow(abalone2))

abalone2<-abalone2[rows,]
abalone2
d = dim(abalone2)

trainning = round(d[1]*0.7)


x1_tr <- x2[1:trainning]
x2_tr <- x3[1:trainning]
x3_tr <- x4[1:trainning]
x4_tr <- x5[1:trainning]
x5_tr <- x6[1:trainning]
x6_tr <- x7[1:trainning]
x7_tr <- x8[1:trainning]
x8_tr <- x9[1:trainning]

y_tr <- x1[1:trainning]
reg = glm(y_tr~x1_tr+x2_tr+x3_tr+x4_tr+x5_tr+x6_tr+x7_tr+x8_tr,family = binomial('logit'))

######### Test (30% de los datos)
test = round(d[1]*0.3)

x1_ts <- x2[1:test]
x2_ts <- x3[1:test]
x3_ts <- x4[1:test]
x4_ts <- x5[1:test]
x5_ts <- x6[1:test]
x6_ts <- x7[1:test]
x7_ts <- x8[1:test]
x8_ts <- x9[1:test]

y_ts <- x1[1:test]
  

yest = exp(reg$coefficients[1]+reg$coefficients[2]*x1_ts+reg$coefficients[3]*x2_ts+reg$coefficients[4]*x3_ts+reg$coefficients[5]*x4_ts+reg$coefficients[6]*x5_ts+reg$coefficients[7]*x6_ts+reg$coefficients[8]*x7_ts+reg$coefficients[9]*x8_ts)/(1+exp(reg$coefficients[1]+reg$coefficients[2]*x1_ts+reg$coefficients[3]*x2_ts+reg$coefficients[4]*x3_ts+reg$coefficients[5]*x4_ts+reg$coefficients[6]*x5_ts+reg$coefficients[7]*x6_ts+reg$coefficients[8]*x7_ts+reg$coefficients[9]*x8_ts))

yest

yest1 = round(yest)
yest1
error = ((yest1==y_ts)*1) #10
porc_acert = sum(error)/length(y_ts) # accuracy --- exactitud

sort(yest)



