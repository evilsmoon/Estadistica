y = c(17,21,19,23,18,16,20,18,22,20,15,22) #Ventas

x = c(1:length(y)) #Semanas


num_sem = 3 

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
  pos_menor =pos+1
  
 cat("-------------------------------RESPUESTA---------------------------------\n")
  cat("En la posicion",pos_menor,"con un CME menor   : ", menor,'\n')
  cat("-------------------------------------------------------------------------\n")
  
  
  
}

prom_movil(y,num_sem)

  