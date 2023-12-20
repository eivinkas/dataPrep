library(cobs)
# TODO: Can implement a way to plot directly in ggplot that is compatible with facet_wrap

pf <- function(x,y,newx = x,stat="median",lambda=0,freeZ=0,tau=0.5) {
 library(cobs)
 # free = 0 : pf(1)  = 1  (curve fixed to (1,1)  )
 # free = 1 : pf(1) >= 1 (curve free but > 0)

  curve <- c()
  
  if(stat=="median"){
    curve <- pfmedian(x,y,newx,lambda=lambda,freeZ=freeZ,tau=tau)
   }
  else {
  #  curve <- pfavg(x,y,newx,lambda=lambda,freeZ) 
     curve <- pfmedian(x,y,newx,lambda=lambda,freeZ) 
    
  }
  return(curve)
}  
  
pfmedian <- function(x,y,newx = x, lambda=0, freeZ=0,tau=0.5) {
  #  B-Spline (median) regression with 2 constraints: 
  #    a) ppfun(1) = 1 : pointwise=rbind(c(0,1,1))  
  #    b) monotomically increasing: constraint="increase"
  #
  # (x,y):    original data
  # newx:     evaluated x points (default = x)
  # returns:  y for each newx
  # free = 0 : pf(1)  = 1  (curve fixed to (1,1)  )
  # free = 1 : pf(1) >= 1 (curve free but > 0)
  
  
  #
  # from the cobs package

  RBS <- cobs(x , y, lambda = lambda, # smoothing
              #constraint="increase", # monotonically increasing
              pointwise=rbind(c(freeZ,1,1)),# pf(1) = 1 if freeZ = 0
              print.mesg = FALSE,nknots=20,tau=tau) # Run B-Spline regression with automatic selection of knots
  result <- predict(RBS, newx)
  fitted <- result[,2] # retrieve only the fitted y values
  
  return(fitted)
  
}

#pfavg <- function(x,y,newx = x, lambda=0, freeZ=0, tau=0.5) {
  #  B-Spline (median) regression with 2 constraints: 
  #    a) ppfun(1) = 1 : pointwise=rbind(c(0,1,1))  
  #    b) monotomically increasing: constraint="increase"
  #
  # First averages for each x and then performs regression.
  # More robust estimate of the mean curve than loess, especially for the small, precise x.
  #
  # (x,y):    original data
  # newx:     evaluated x points (default = x)
  # returns:  y for each newx
  # free = 0 : pf(1)  = 1  (curve fixed to (1,1)  )
  # free = 1 : pf(1) >= 1 (curve free but > 0)
  
  #
  # from the cobs package
  
 # df <- data.frame(x=x,y=y) %>%
  #  group_by(x) %>%
  #  summarise(z = mean(y))
  
  #RBS <- cobs(df$x , df$z, lambda = lambda,constraint="increase",pointwise=rbind(c(freeZ,1,1)),print.mesg = FALSE,nknots=20,tau=tau) # Run B-Spline regression with automatic selection of knots
  
  #result <- predict(RBS, newx)
 # fitted <- result[,2] # retrieve only the fitted y values
  
  #return(fitted)
  
#}
