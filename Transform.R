# Make transformation functions############
logit.transform = function(x){  #This one looks better (Phil Dixon's suggestion)
    #  x is a vector of values between 0 and 1 inclusive
    eps=min(x[x!=0])
    x[x==0]=0.5*eps
    x[x==1]=1-0.5*eps
    return(log(x/(1-x)))
}
INVlogit.transform=function(alpha){
    #alpha is a vector of real number
    #if(alpha>705) return(1) #otherwise too big to exp()
    alpha[alpha>709]=709   #This is largest number R will take to 1.  Otherwise NaN
    return(exp(alpha)/(exp(alpha)+1))
}
logit.transform.eps = function(x, eps){
    #  x is a vector of values between 0 and 1
    eps=min(x[x!=0])
    return(log((x+eps/(1-x+eps))))
    
}

logw0=function(x){ 
    #  x is a vector of values >=0
  # The following transformation preserves orders of magnitudes and results in 0 when original data is zero. 
  c=floor(log(min(x[x>0], na.rm=T)))
  d=exp(c)
  return(log(x+d)-c)
}
