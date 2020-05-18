numagents<- 10
  df<-read.csv("Toy Example.csv", stringsAsFactors=FALSE,header=TRUE)
  n=20 # Matches that day
  m=2  # Bookmakers
  x<-read.csv("Init.csv", stringsAsFactors=FALSE,header=TRUE)
  x<-x[,-c(1,2,3)]
  o <- array(0, dim = c(3, n, m))
  p <- array(0, dim = c(3, n))
  time<-0
  maxtime<-10^4
  status <- array(1, dim=numagents)
  lb<-array(o, dim = 3*n*m)
  ub<-array(1, dim = 3*n*m)
  minexp<-20
  pmin=.25
 
  
  #objective function
  objfun=function(x,agent){
  r=-1
  t=0
  
    for (i in 1:n){
      for (j in 1:m){
        for (k in 1:3){
   t=1-      (1-o[k,i,j])*x[(n*m)*(k-1)+m*(i-1)+j,agent]       
        
   r=r*t 
   }
    
      }
    }
    
    return(r)
  }
  
  #constraint function
  confun=function(x, agent){
    r=1
    t=0
    
    for (i in 1:n){
      for (j in 1:m){
        for (k in 1:3){
          t=1-  (1-p[k,i])*x[(n*m)*(k-1)+m*(i-1)+j, agent]           
          
          r=r*t 
       
        }
        
      }
    }
  
 return(r)
  }
  

  Exp=function(x){
  exp=0  
  for (i in 1:numagents){
    
   if (exp< objfun(x,i)*confun(x,i) ){
     
     exp<-objfun(x,i)*confun(x,i)
     
   } 
    
  }
 return(exp)   
  }
  
  set <- 1:numagents
  while (time<=maxtime && Exp(x)<=minexp)
  {
    #Test
    for (i in set){
      
      j<- sample(set, 1)
      
      if (objfun(x,i) <= objfun(x,j) &&  confun(x,i) <=confun(x,j)){
        status[i]<--1 #inefficient
      }
      else if (objfun(x,i)*confun(x,i) <= objfun(x,j)*confun(x,j)){
        
        status[i]<-0 #inactive 
      }
    }
    #Diffusion
    for (i in set){
      if (status[i]==-1){ 
        pmin=pmin-.01
        opt<-JDEoptim(lb, ub, -objfun(x,i), pmin-confun(x,i))
        x[,i]<-opt$par
      }
      else if (status[i] == 0) {
        j<- sample(set, 1)
        if (status[j] == 1){
          for (a in 1:n){
            for (b in 1:m){
              for (c in 1:3){
                
                if (runif(1)>.5){
                x[(n*m)*(c-1)+m*(a-1)+b,i]    <- x[(n*m)*(c-1)+m*(a-1)+b,j]  
                }
              }
            }
          }
         }
        }
      
      else {
        pmin=pmin-.01
        opt<-JDEoptim(lb, ub, objfun(x,i), pmin-confun(x,i))
        x[,i]<-opt$par
      }
    }
    time=time+1
  }
