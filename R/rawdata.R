rawdata <- function(df) {
  
  dout <- data.frame(id = df$`browserCheck:workerID`,
                      x = as.numeric(df$`dotmaskTrial:numerosity`),
                      y = as.numeric(df$`dotmaskTrial:answer`),
                      rt = as.numeric(df$`dotmaskTrial:rt`),
                      s = 99)
  
  if ("strategyMC:response" %in% names(df)) {
    dout$s = as.numeric(df$`strategyMC:response`)+1 # avoid 0
  }
  
  # N=1 is subitizing by definition:
  dout$s[dout$x==1] <- 1
 
  # Relabel order from javascript notation
  neworder <- c(1,6,5,2,7,4,3,9,8)
  dout$s <- as.numeric(neworder[dout$s] )

  
  # old labels    
  # cardLabels = c("Subitize",
  #                "Compare & Adjust",
  #                "Equal Groups",
  #                "Count All",
  #                "Don't know",
  #                "Unequal Groups",
  #                "Count & Adjust",
  #                "Error",
  #                "Other")

  # sc <- cardLabels[dout$s]
  # dout$sc <- cardLabels[dout$s]
  
  
  newLabels = c("1.Subitize",
                "2.Count All", 
                "3.Count & Adjust",
                "4.Unequal Groups",
                "5.Equal Groups",
                "6.Compare & Adjust",
                "7.Don't know",
                "8.Other",
                "9.Error")
  
  
 
  dout$sc <- newLabels[dout$s]
     
  
  
  return(dout)
}