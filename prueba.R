library(readr)
ecuador <- read_csv("ecuador.csv",col_names = FALSE,skip = 1)
#View(ecuador)

data <- as.data.frame(ecuador)
y = col(data)
x = c(1:length(y))

## literal 1
prom = c(3.09504901286289,3.03070306911397,-2.49102075763332,1.90164677546638	,1.94639824444045	,1.81887357109554,	-0.171087541220487)

sum_prom = sum(prom)/length(prom)

# literal 2

y = c(-11.3204429668097	,-9.99376530753057	,17.6854254328587	,14.5277813325836	,2.99457830022945	,2.11715397114871	,0.473187460475643	,-0.775245444753679	,15.1482025792391, -13.9363806349476, -9.47897032339398, 10.1496458021336, 7.19748416465127, 52.4795588797229, 5.57582893748767, 9.49371359594669, 19.3607180667517, 2.28989724406483, 14.6144609501441, 21.6356921889666, 15.4914458591029, -9.18217401724412, -13.6448252637681, -3.92135491716465, -2.44013963945865, -13.690424395023, -8.70101398503682, -11.6138011997674, 5.36797841354645, 5.81362920255557, 6.88932038287776, 4.30322942836226, 2.64177088971913, 15.0087193477126, 5.22255322839003, 1.49015481451174, 7.00617304202626, -3.7826792354184, -26.2999928320854, -7.71406676367923, 28.4142784567463, 12.0852744211313, 10.5930870401158, 4.26184570076396, 7.73271101107305, 8.00091082171883, 6.65050227293493, 13.8473188994265, 0.655537559193846, 7.46507996966288, 5.66265674725217, 4.98525306124122, 3.09504901286289, 3.03070306911397, -2.49102075763332, 1.90164677546638, 1.94639824444045, 1.81887357109554, -0.171087541220487
)
x = c(1:length(y))

num_sem = 6 

prom_movil <- function (y,num_semana){
  
  v_CME=c()
  cont=1
  
  cat("---------inicio for de las" ,num_semana ,"primeras semanas------",'\n')
  for (n in num_semana:length(y)-1) {
    
    ma <- function(y,n){filter(y,rep(1/n,n), sides=1)} 
    
    wind = ma(y,n) 
    pr = wind[n:(length(wind)-1)] 
    err_pr = y[n+1:(length(y)-n)]-pr 
    sum_err_pr = sum(err_pr) 
    cuad_err_pr = err_pr^2 
    CME = sum(cuad_err_pr)/length(pr) 
    v_CME[cont]=CME
    cont=cont+1
    print(c("# Ventana:",n,"respuesta CME :",CME))
    
    
  }
  cat("---------fin for des la ",num_semana, "primeras semanas------",'\n')
  
  menor=v_CME[1]
  
  for (x in 1:length(v_CME)) {
    if(v_CME[x]<menor){
      menor=v_CME[x]
      pos=x
    }
  }

  
  cat("-------------------------------RESPUESTA---------------------------------\n")
  cat("En la posicion",pos,"con un CME menor es  : ", menor,' \n')
  cat("-------------------------------------------------------------------------\n")
  
  
  
}

prom_movil(y,num_sem)


# literal 3

alpha = 0.4 #valor de la constante de suavizamiento
sua = HoltWinters(y, alpha, beta=FALSE, gamma=FALSE) #suavizamiento exponencial
s1 = sua$fitted[,1] #valores del suavizamiento exponencial
err_pr_sua = y[2:(length(y))]-s1 #error del pronostico con suavizamiento
err_pr_sua2 = err_pr_sua^2 #error de pronostico con suavizamiento al cuadrado
CME_s = sum(err_pr_sua2)/length(s1) #cuadrado medio debido al error con suavizamiento

# literal 4

suavizamiento <- function(y){
  
  v_CME=c()
  alpha=c()
  cont=1
  cont_alpha=1
  
  for(x in seq(0,1,by=0.01)){
    alpha_x = x
    alpha[cont_alpha]=x
    cont_alpha=cont_alpha+1
    
    if(alpha_x != 0){
      sua = HoltWinters(y, alpha_x, beta=FALSE, gamma=FALSE) 
      s_1 = sua$fitted[,1] 
      err_pr_sua = y[2:(length(y))]-s_1 
      cuad_err_sua = err_pr_sua^2 
      CME_s = sum(cuad_err_sua)/length(s_1)
      v_CME[cont]=CME_s
      
      print(c("# Ventana:",x,"respuesta CME :",CME_s))
      cont=cont+1
    }
    
  }
  
  menor=v_CME[1]
  
  
  for (x in 1:length(v_CME)) {
    if(v_CME[x]<menor){
      menor=v_CME[x]
      pos=x
    }
  }

  cat("-------------------------------RESPUESTA---------------------------------\n")
  cat('---      El menor CME_S es de :',menor,'con un alpha de : ',alpha[pos+1],'     ---\n')
  cat("-------------------------------------------------------------------------\n")
  
  
  
}

suavizamiento(y) 





  
  
