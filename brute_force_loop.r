
for(p in 1:4){
  for (q in 0:3){
    for (P in 0:4){
      for (Q in 0:3){
        #cat(p,q,P,Q,"\n")
        try(m <- Arima(unem.train.ts, order = c(p, 0, q), seasonal = list(order = c(P, 1, Q), period = 12)),
            silent = TRUE)
        
        if(m$aic < bestAIC) # update if this model attain better aic
        { bestAIC = m$aic
        bestFit = m
        bestModel = c( p, q, P, Q)
        #cat(p,q,P,Q,as.numeric(bestAIC), "\n")
        unem.diff12.df = rbind(unem.diff12.df, 
                               data.frame("p" = p, "q" = q, "P" = P, "Q" = Q, "aic" = bestAIC))} 
      }
    }
  }
}

write.csv(x = unem.diff12.df, file = "unem.diff12.df.csv")



# First Differenced Series: SARIMA(9,1,0:3)(0:3,0,0:3) 

bestAIC <- 10000 

unem.diff.df = data.frame("p" = 0, "q" = 0, "P" = 0, "Q" = 0, "aic" = bestAIC)

for(p in 1:9){
  for (q in 0:3){
    for (P in 0:3){
      for (Q in 0:3){
        #cat(p,q,P,Q,"\n")
        try(m <- Arima(unem.train.ts, order = c(p, 0, q), seasonal = list(order = c(P, 1, Q), period = 12)),
            silent = TRUE)
        
        if(m$aic < bestAIC) # update if this model attain better aic
        { bestAIC = m$aic
        bestFit = m
        bestModel = c( p, q, P, Q)
        #cat(p,q,P,Q,as.numeric(bestAIC), "\n")
        unem.diff.df = rbind(unem.diff12.df, 
                               data.frame("p" = p, "q" = q, "P" = P, "Q" = Q, "aic" = bestAIC))} 
      }
    }
  }
}

write.csv(x = unem.diff.df, file = "unem.diff.df.csv")


# First and Seasonal Differenced Series: SARIMA(0:5,1,0:3)(0:3,1,0:3) 

bestAIC <- 10000 

unem.diff.diff12.df = data.frame("p" = 0, "q" = 0, "P" = 0, "Q" = 0, "aic" = bestAIC)

for(p in 0:5){
  for (q in 0:3){
    for (P in 0:3){
      for (Q in 0:3){
        #cat(p,q,P,Q,"\n")
        try(m <- Arima(unem.train.ts, order = c(p, 0, q), seasonal = list(order = c(P, 1, Q), period = 12)),
            silent = TRUE)
        
        if(m$aic < bestAIC) # update if this model attain better aic
        { bestAIC = m$aic
        bestFit = m
        bestModel = c( p, q, P, Q)
        #cat(p,q,P,Q,as.numeric(bestAIC), "\n")
        unem.diff.diff12.df = rbind(unem.diff12.df, 
                             data.frame("p" = p, "q" = q, "P" = P, "Q" = Q, "aic" = bestAIC))} 
      }
    }
  }
}

write.csv(x = unem.diff.diff12.df, file = "unem.diff.diff12.df.csv")