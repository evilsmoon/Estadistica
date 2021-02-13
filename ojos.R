library(readxl)
Compilado <- read_excel("C:/Users/carlo/Downloads/Compilado.xlsx")

data = Compilado
data <- data[-(1:2),,drop=FALSE]#Quitamos los titulos
###################################
#Cejas
###################################
cejas_i_cal <- data[,6,drop=FALSE]
cejas_d_cal <- data[,14,drop=FALSE]

cejas_i_cal <- lapply(cejas_i_cal, as.numeric)
cejas_d_cal <- lapply(cejas_d_cal, as.numeric)

cejas_i_cal <- as.data.frame(cejas_i_cal)
cejas_d_cal <- as.data.frame(cejas_d_cal)

cejas_i_pre <- data[,10,drop=FALSE]
cejas_d_pre <- data[,18,drop=FALSE]

cejas_i_pre <- lapply(cejas_i_cal, as.numeric)
cejas_d_pre <- lapply(cejas_d_cal, as.numeric)

cejas_i_pre <- as.data.frame(cejas_i_pre)
cejas_d_pre <- as.data.frame(cejas_d_pre)

cejas_cal = rep.int(0, dim(cejas_d_cal)[1])

cejas_pre = rep.int(0, dim(cejas_d_pre)[1])

#Valor de las cejas derecha e izquierda de la diagonal en promedio del calibrador
cejas_cal = (cejas_i_cal+cejas_d_cal)/2
#Valor de las cejas derecha e izquierda de la diagonal en promedio del predicho
cejas_pre = (cejas_i_pre+cejas_d_pre)/2

###################################
#Ojos
###################################
ojos_i_cal <- data[,22,drop=FALSE]
ojos_d_cal <- data[,30,drop=FALSE]

ojos_i_cal <- lapply(ojos_i_cal, as.numeric)
ojos_d_cal <- lapply(ojos_d_cal, as.numeric)

ojos_i_cal <- as.data.frame(ojos_i_cal)
ojos_d_cal <- as.data.frame(ojos_d_cal)

ojos_i_pre <- data[,26,drop=FALSE]
ojos_d_pre <- data[,34,drop=FALSE]

ojos_i_pre <- lapply(ojos_i_pre, as.numeric)
ojos_d_pre <- lapply(ojos_d_pre, as.numeric)

ojos_i_pre <- as.data.frame(ojos_i_pre)
ojos_d_pre <- as.data.frame(ojos_d_pre)

ojos_cal = rep.int(0, dim(ojos_d_cal)[1])

ojos_pre = rep.int(0, dim(ojos_d_pre)[1])

#Valor de las cejas derecha e izquierda de la diagonal en promedio del calibrador
#-------------------------------------#
ojos_cal = (ojos_i_cal+ojos_d_cal)/2
#-------------------------------------#
#Valor de las cejas derecha e izquierda de la diagonal en promedio del predicho
#-------------------------------------#
ojos_pre = (ojos_i_pre+ojos_d_pre)/2
#-------------------------------------#

###################################
#Nariz
###################################
nariz_cal <- data[,38,drop=FALSE]

nariz_cal <- lapply(nariz_cal, as.numeric)

nariz_pre <- data[,42,drop=FALSE]

nariz_pre <- lapply(nariz_pre, as.numeric)


#-------------------------------------#
nariz_cal <- as.data.frame(nariz_cal)
#-------------------------------------#

#-------------------------------------#
nariz_pre <- as.data.frame(nariz_pre)
#-------------------------------------#

###################################
#Boca
###################################
boca_cal <- data[,46,drop=FALSE]

boca_cal <- lapply(boca_cal, as.numeric)

boca_pre <- data[,50,drop=FALSE]

boca_pre <- lapply(boca_pre, as.numeric)


#-------------------------------------#
boca_cal <- as.data.frame(boca_cal)
#-------------------------------------#

#-------------------------------------#
boca_pre <- as.data.frame(boca_pre)
#-------------------------------------#

#######################################
etnia <- data[,2,drop=FALSE]

#####             BOCA

boca_cal = c(boca_cal,etnia)
boca_pre = c(boca_pre,etnia)

boca_cal <- as.data.frame(boca_cal)
boca_pre <- as.data.frame(boca_pre)

boca_cal <- boca_cal[,1:2,drop=FALSE]
boca_pre <- boca_pre[,1:2,drop=FALSE]

M_b = ((boca_cal$...2 == 'M')*1)*boca_cal$...46
B_M_c = M_b
M_b = M_b[M_b!=0]
M_b = mean(M_b)

I_b = ((boca_cal$...2 == 'I')*1)*boca_cal$...46
B_I_c = I_b
I_b = I_b[I_b!=0]
I_b = mean(I_b)

B_b = ((boca_cal$...2 == 'B')*1)*boca_cal$...46
B_B_c = B_b
B_b = B_b[B_b!=0]
B_b = mean(B_b)

AFC_b = ((boca_cal$...2 == 'AFC')*1)*boca_cal$...46
B_AFC_c = AFC_b
AFC_b = AFC_b[AFC_b!=0]
AFC_b = mean(AFC_b)

M1_b = ((boca_pre$...2 == 'M')*1)*boca_pre$...50
B_M_p = M1_b
M1_b = M1_b[M1_b!=0]
M1_b = mean(M1_b)

I1_b = ((boca_pre$...2 == 'I')*1)*boca_pre$...50
B_I_p = I1_b
I1_b = I1_b[I1_b!=0]
I1_b = mean(I1_b)

B1_b = ((boca_pre$...2 == 'B')*1)*boca_pre$...50
B_B_p = B1_b
B1_b = B1_b[B1_b!=0]
B1_b = mean(B1_b)

AFC1_b = ((boca_pre$...2 == 'AFC')*1)*boca_pre$...50
B_AFC_p = AFC1_b
AFC1_b = AFC1_b[AFC1_b!=0]
AFC1_b = mean(AFC1_b)

#####             OJOS

ojos_cal = c(ojos_cal,etnia)
ojos_pre = c(ojos_pre,etnia)

ojos_cal <- as.data.frame(ojos_cal)
ojos_pre <- as.data.frame(ojos_pre)

ojos_cal <- ojos_cal[,1:2,drop=FALSE]
ojos_pre <- ojos_pre[,1:2,drop=FALSE]

M_o = ((ojos_cal$...2 == 'M')*1)*ojos_cal$...22
M_o = M_o[M_o!=0]
M_o = mean(M_o)

I_o = ((ojos_cal$...2 == 'I')*1)*ojos_cal$...22
I_o = I_o[I_o!=0]
I_o = mean(I_o)

B_o = ((ojos_cal$...2 == 'B')*1)*ojos_cal$...22
B_o = B_o[B_o!=0]
B_o = mean(B_o)

AFC_o = ((ojos_cal$...2 == 'AFC')*1)*ojos_cal$...22
AFC_o = AFC_o[AFC_o!=0]
AFC_o = mean(AFC_o)

M1_o = ((ojos_pre$...2 == 'M')*1)*ojos_pre$...26
M1_o = M1_o[M1_o!=0]
M1_o = mean(M1_o)

I1_o = ((ojos_pre$...2 == 'I')*1)*ojos_pre$...26
I1_o = I1_o[I1_o!=0]
I1_o = mean(I1_o)

B1_o = ((ojos_pre$...2 == 'B')*1)*ojos_pre$...26
B1_o = B1_o[B1_o!=0]
B1_o = mean(B1_o)

AFC1_o = ((ojos_pre$...2 == 'AFC')*1)*ojos_pre$...26
AFC1_o = AFC1_o[AFC1_o!=0]
AFC1_o = mean(AFC1_o)

#####             NARIZ

nariz_cal = c(nariz_cal,etnia)
nariz_pre = c(nariz_pre,etnia)

nariz_cal <- as.data.frame(nariz_cal)
nariz_pre <- as.data.frame(nariz_pre)

nariz_cal <- nariz_cal[,1:2,drop=FALSE]
nariz_pre <- nariz_pre[,1:2,drop=FALSE]

M_n = ((nariz_cal$...2 == 'M')*1)*nariz_cal$...38
M_n = M_n[M_n!=0]
M_n = mean(M_n)

I_n = ((nariz_cal$...2 == 'I')*1)*nariz_cal$...38
I_n = I_n[I_n!=0]
I_n = mean(I_n)

B_n = ((nariz_cal$...2 == 'B')*1)*nariz_cal$...38
B_n = B_n[B_n!=0]
B_n = mean(B_n)

AFC_n = ((nariz_cal$...2 == 'AFC')*1)*nariz_cal$...38
AFC_n = AFC_n[AFC_n!=0]
AFC_n = mean(AFC_n)

M1_n = ((nariz_pre$...2 == 'M')*1)*nariz_pre$...42
M1_n = M1_n[M1_n!=0]
M1_n = mean(M1_n)

I1_n = ((nariz_pre$...2 == 'I')*1)*nariz_pre$...42
I1_n = I1_n[I1_n!=0]
I1_n = mean(I1_n)

B1_n = ((nariz_pre$...2 == 'B')*1)*nariz_pre$...42
B1_n = B1_n[B1_n!=0]
B1_n = mean(B1_n)

AFC1_n = ((nariz_pre$...2 == 'AFC')*1)*nariz_pre$...42
AFC1_n = AFC1_n[AFC1_n!=0]
AFC1_n = mean(AFC1_n)

#####             CEJAS

cejas_cal = c(cejas_cal,etnia)
cejas_pre = c(cejas_pre,etnia)

cejas_cal <- as.data.frame(cejas_cal)
cejas_pre <- as.data.frame(cejas_pre)

cejas_cal <- nariz_cal[,1:2,drop=FALSE]
cejas_pre <- nariz_pre[,1:2,drop=FALSE]

M_cejas = ((cejas_cal$...2 == 'M')*1)*cejas_cal$...38
Cejas_M_c = M_cejas
M_cejas = M_cejas[M_cejas!=0]
M_cejas = mean(M_cejas)

I_cejas = ((cejas_cal$...2 == 'I')*1)*cejas_cal$...38
Cejas_I_c = I_cejas
I_cejas = I_cejas[I_cejas!=0]
I_cejas = mean(I_cejas)

B_cejas = ((cejas_cal$...2 == 'B')*1)*cejas_cal$...38
Cejas_B_c = B_cejas
B_cejas = B_cejas[B_cejas!=0]
B_cejas = mean(B_cejas)

AFC_cejas = ((cejas_cal$...2 == 'AFC')*1)*cejas_cal$...38
Cejas_AFC_c = AFC_cejas
AFC_cejas = AFC_cejas[AFC_cejas!=0]
AFC_cejas = mean(AFC_cejas)

M1_cejas = ((cejas_pre$...2 == 'M')*1)*cejas_pre$...42
Cejas_M_p = M1_cejas
M1_cejas = M1_cejas[M1_cejas!=0]
M1_cejas = mean(M1_cejas)

I1_cejas = ((cejas_pre$...2 == 'I')*1)*cejas_pre$...42
Cejas_I_p = I1_cejas
I1_cejas = I1_cejas[I1_cejas!=0]
I1_cejas = mean(I1_cejas)

B1_cejas = ((cejas_pre$...2 == 'B')*1)*cejas_pre$...42
Cejas_B_p = B1_cejas
B1_cejas = B1_cejas[B1_cejas!=0]
B1_cejas = mean(B1_cejas)

AFC1_cejas = ((cejas_pre$...2 == 'AFC')*1)*cejas_pre$...42
Cejas_AFC_p = AFC1_cejas
AFC1_cejas = AFC1_cejas[AFC1_cejas!=0]
AFC1_cejas = mean(AFC1_cejas)

library(ggplot2)
cl = c('M','I','B','AFC')
Type = c('Calibrador','Calibrador','Calibrador','Calibrador','Predicho','Predicho','Predicho','Predicho')#Vector para 4 grupos individuales y 4 familiares

boca_c = c(M_b,I_b,B_b,AFC_b)#Porcentaje total de personas de familias, sumadas todas da 100%
boca_p = c(M1_b,I1_b,B1_b,AFC1_b)#Porcentaje total de familias, sumadas todas da 100%
Taxonomy = c(cl,cl)
Percentage = c(boca_c,boca_p)#Junta los 4 grupos de familias y personas
datf <- data.frame(Percentage, Taxonomy, Type)#Dataframe con los datos organizados
Taxonomy2 = factor(datf$Taxonomy, cl)#Marca los niveles existentes osea los tipos de familias que hay

ggplot(data=datf, aes(x=Taxonomy2, y=Percentage, fill=Type)) + 
  labs(y="Promedio boca", x = "Etnia") + geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()

################################

ojos_c = c(M_o,I_o,B_o,AFC_o)#Porcentaje total de personas de familias, sumadas todas da 100%
ojos_p = c(M1_o,I1_o,B1_o,AFC1_o)#Porcentaje total de familias, sumadas todas da 100%
Taxonomy = c(cl,cl)
Percentage = c(ojos_c,ojos_p)#Junta los 4 grupos de familias y personas
datf <- data.frame(Percentage, Taxonomy, Type)#Dataframe con los datos organizados
Taxonomy2 = factor(datf$Taxonomy, cl)#Marca los niveles existentes osea los tipos de familias que hay

ggplot(data=datf, aes(x=Taxonomy2, y=Percentage, fill=Type)) + 
  labs(y="Promedio ojos", x = "Etnia") + geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()

################################

nariz_c = c(M_n,I_n,B_n,AFC_n)#Porcentaje total de personas de familias, sumadas todas da 100%
nariz_p = c(M1_n,I1_n,B1_n,AFC1_n)#Porcentaje total de familias, sumadas todas da 100%
Taxonomy = c(cl,cl)
Percentage = c(nariz_c,nariz_p)#Junta los 4 grupos de familias y personas
datf <- data.frame(Percentage, Taxonomy, Type)#Dataframe con los datos organizados
Taxonomy2 = factor(datf$Taxonomy, cl)#Marca los niveles existentes osea los tipos de familias que hay

ggplot(data=datf, aes(x=Taxonomy2, y=Percentage, fill=Type)) + 
  labs(y="Promedio nariz", x = "Etnia") + geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()

################################

cejas_c = c(M_cejas,I_cejas,B_cejas,AFC_cejas)#Porcentaje total de personas de familias, sumadas todas da 100%
cejas_p = c(M1_cejas,I1_cejas,B1_cejas,AFC1_cejas)#Porcentaje total de familias, sumadas todas da 100%
Taxonomy = c(cl,cl)
Percentage = c(cejas_c,cejas_p)#Junta los 4 grupos de familias y personas
datf <- data.frame(Percentage, Taxonomy, Type)#Dataframe con los datos organizados
Taxonomy2 = factor(datf$Taxonomy, cl)#Marca los niveles existentes osea los tipos de familias que hay

ggplot(data=datf, aes(x=Taxonomy2, y=Percentage, fill=Type)) + 
  labs(y="Promedio cejas", x = "Etnia") + geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()

####################################################################
#           BOCA

B_M_c = (B_M_c>=1)*1
B_I_c = (B_I_c>=1)*2
B_B_c = (B_B_c>=1)*3
B_AFC_c = (B_AFC_c>=1)*4

B_M_p = (B_M_c>=1)*5
B_I_p = (B_I_c>=1)*6
B_B_p = (B_B_c>=1)*7
B_AFC_p = (B_AFC_c>=1)*8

B_total_c = B_M_c+B_I_c+B_B_c+B_AFC_c

B_total_p = B_M_p+B_I_p+B_B_p+B_AFC_p

Class = c(B_total_c,B_total_p)

vec1 = c(boca_cal$...46,boca_pre$...50)

df <- data.frame(vec1, Class)

library(dplyr)#Resumen del dataframe
group_by(df, Class) %>%
  summarise(
    count = n(),
    mean = mean(vec1, na.rm = TRUE),
    sd = sd(vec1, na.rm = TRUE)
  )

library("ggpubr")
levels(df$Class) <- c("Mc","Ic","Bc","AFCc","Mp","Ip","Bp","AFCp")

ggboxplot(df, x = "Class", y = "vec1", 
          color = "Class", palette = c("#00AFBB", "#E7B800", "#FC4E07","#D82E2E", "#00AFBB", "#E7B800", "#FC4E07","#D82E2E"),
          ylab = "Diagonal Boca", xlab = "Grupo etnico", add = "jitter")
df[, 'Class'] <- as.factor(df[, 'Class'])
res.aov1 <- aov(vec1 ~ Class, data = df)
summary(res.aov1)

ggboxplot(df, x = "Class", y = "vec1", 
          color = "Class", palette = c("#00AFBB", "#E7B800", "#FC4E07","#D82E2E", "#00AFBB", "#E7B800", "#FC4E07","#D82E2E"),
          ylab = "Diagonal Boca", xlab = "Grupo etnico", add = "jitter")
df[, 'Class'] <- as.factor(df[, 'Class'])
res.aov1 <- aov(vec1 ~ Class, data = df)
summary(res.aov1)


####################################################################
#           Ceja

Cejas_M_c = (Cejas_M_c>=1)*1
Cejas_I_c = (Cejas_I_c>=1)*2
Cejas_B_c = (Cejas_B_c>=1)*3
Cejas_AFC_c = (Cejas_AFC_c>=1)*4

Cejas_M_p = (Cejas_M_c>=1)*5
Cejas_I_p = (Cejas_I_c>=1)*6
Cejas_B_p = (Cejas_B_c>=1)*7
Cejas_AFC_p = (Cejas_AFC_c>=1)*8

Cejas_total_c = Cejas_M_c+Cejas_I_c+Cejas_c+Cejas_AFC_c

Cejas_total_p = Cejas_M_p+Cejas_I_p+Cejas_p+Cejas_AFC_p

Class = c(B_total_c,B_total_p)

vec1 = c(boca_cal$...46,boca_pre$...50)

df <- data.frame(vec1, Class)

library(dplyr)#Resumen del dataframe
group_by(df, Class) %>%
  summarise(
    count = n(),
    mean = mean(vec1, na.rm = TRUE),
    sd = sd(vec1, na.rm = TRUE)
  )

library("ggpubr")
levels(df$Class) <- c("Mc","Ic","Bc","AFCc","Mp","Ip","Bp","AFCp")

ggboxplot(df, x = "Class", y = "vec1", 
          color = "Class", palette = c("#00AFBB", "#E7B800", "#FC4E07","#D82E2E", "#00AFBB", "#E7B800", "#FC4E07","#D82E2E"),
          ylab = "Diagonal Boca", xlab = "Grupo etnico", add = "jitter")
df[, 'Class'] <- as.factor(df[, 'Class'])
res.aov1 <- aov(vec1 ~ Class, data = df)
summary(res.aov1)

ggboxplot(df, x = "Class", y = "vec1", 
          color = "Class", palette = c("#00AFBB", "#E7B800", "#FC4E07","#D82E2E", "#00AFBB", "#E7B800", "#FC4E07","#D82E2E"),
          ylab = "Diagonal Boca", xlab = "Grupo etnico", add = "jitter")
df[, 'Class'] <- as.factor(df[, 'Class'])
res.aov1 <- aov(vec1 ~ Class, data = df)
summary(res.aov1)