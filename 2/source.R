#Functions

survivorship_F <- function(f=0,M,waa,mat,sel,message=T){
  if(length(M)==1){
    message(paste0("Constant M = ",M," used for all ages"))
    M=rep(M,length(waa))
  }
  if(f==0){
    sel=rep(0,length(waa))
    if(message==T){message("Assumed F = 0, unfished survivorship")}
  }
  if(length(waa) != length(mat) | length(waa) != length(sel)){
    message("at-age vectors are not the same length")
    return(NA)
  }
  if(length(waa) == length(mat) & length(waa) == length(sel)){
    l_age <- rep(NA,length(waa)) 
    l_age[1] <- 1
    for(a in 2:(length(waa)-1)){
      l_age[a] <- l_age[a-1]* exp(-M[a-1]-f*sel[a-1])
    }
    l_age[length(waa)] <- l_age[length(waa)-1]*exp(-M[a-1]-f*sel[a-1])/(1-exp(-M[a]-f*sel[a]))
    return(l_age)
  }
}

MSYcalc <- function(M,waa,mat,sel,a,b){

  output <- list()
  
  if(length(M)==1){
    message(paste0("Constant M = ",M," used for all ages"))
    M=rep(M,length(waa))
  }  
  if(length(waa) != length(mat) | length(waa) != length(sel)){
    message("at-age vectors are not the same length")
    return(NA)
  }
  
  f <- seq(0,5,0.01)
  phi_f <- rep(NA,length(f))
  YPR_a <- SURV_a <- matrix(NA,ncol=length(waa),nrow=length(f))
  rownames(SURV_a)<-rownames(YPR_a)<-f
  
  if(length(waa) == length(mat) & length(waa) == length(sel)){
    for(i in 1:length(f)){
      SURV_a[i,] <- survivorship_F(f=f[i],M,waa,mat,sel,message=F)
      for(j in 1:ncol(YPR_a)){
        YPR_a[i,j] <- SURV_a[i,j]*waa[j]*f[i]*sel[j]*(1-exp(-f[i]*sel[j]-M[j]))/(f[i]*sel[j]+M[j])
      }
      phi_f[i] <- sum(SURV_a[i,]*waa*mat)
    }  
   ypr <- rowSums(YPR_a) 
   eq_rec_f <- (1/b*(a-1/phi_f))
   eq_rec_f[eq_rec_f<0] <- 0
   yield <- ypr*eq_rec_f
   output[[1]] <- f_msy <- f[which(yield==max(yield))]
   output[[2]] <- msy <- yield[which(yield==max(yield))]
   output[[3]] <- ssb_msy <- eq_rec_f[which(yield==max(yield))]/(a-eq_rec_f[which(yield==max(yield))]*b)

   names(output) <- c("Fmsy","msy","SSBmsy")
   return(output)
   
  } 
}
