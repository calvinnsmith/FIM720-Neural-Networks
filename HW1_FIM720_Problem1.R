

N <- 120

### Function for creating a matrix of patterns.
### @noOfPatterns: number of patterns to create (number of columns in matrix)
### @N: Number of bits in the patters (number of rows in matrix)
create_patterns <- function(noOfPatterns,N){
  
  patterns <- matrix(1,nrow = N,ncol = noOfPatterns)
  for(i in 1:N){
    for(j in 1:noOfPatterns){
      p <- runif(1,0,1)
      if(p <= 0.5){
        patterns[i,j] <- -1
      }
    }
  }
  
return(patterns)  
}

### Function for creating weight matrix W according to Hebb's rule
### @patterns: matrix of patterns
hebbs_rule <- function(patterns){
  N <- length(patterns[,1])
  noOfPatterns <- ncol(patterns)
  W <- matrix(0,nrow = N,ncol= N)
  for(i in 1:N){
    for(j in 1:N){
       W[i,j] <- patterns[i,]%*%patterns[j,]
      }
    }
  W <- W/N
  return(W)
}


trials <- 10^5

### Function for estimating the one step error probability
### @trials: number of iterations
### @p: number of patterns
one_step <- function(trials,p){
  error_count <- 0
  N <- 120
  for(i in 1:trials){
    patterns <- create_patterns(p,N)
    W <- hebbs_rule(patterns)
    s_0 <- patterns[,1]
    index <- sample(1:N,1)
    
    s_update <- sign(W[index,]%*%s_0)
    if(s_update == 0){
      s_update <- 1
    }
    if(s_update != s_0[index]){
      error_count <- error_count + 1 
    }
    
  }
  
  prob_error <- error_count/trials
  return(prob_error)
  
}



nr <- c(12,24,48,70,100,120)
error_prob <-numeric(length(nr))
for(i in 1:length(nr)){
  error_prob[i] <- one_step(trials,nr[i])
  print(nr[i])
}

