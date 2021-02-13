y = c(17,21,19,23,18,16,20,18,22,20,15,22)#Ventas

x = c(1:length(y)) #Semanas


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
