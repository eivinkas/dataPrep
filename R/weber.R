weberMeasure = function(df,
                            numerosity = "dots",
                            answer = "answer",
                            central = "median",
                            plot = TRUE) {

  # Setup data frame
  dat = data.frame(numerosity = df[,colnames(df) == numerosity])
  dat$answer = df[,colnames(df) == answer]

  # Setup empty output frame
  dots = sort(unique(df[,(colnames(df) == numerosity)]))
  output = data.frame(n = dots)
  output$perceivedN = 0
  output$CV = 0
  output$Weber = 0

  for (i in 1:length(dots)) {
    newDat = dat[dat$numerosity == dots[i],]
    getSD = sd(newDat$answer)
    if (central == "median") m = median(newDat$answer)
    if (central == "mean") m = mean(newDat$answer)
    output$perceivedN[i] = m
    output$CV[i] = getSD/dots[i]
    output$Weber[i] = getSD/m
  }

  # Plot
  if (plot) {
    library(ggplot2)
    plotDat1 = output[,1:2]
    plotDat2 = output[,c(1,3)]
    names(plotDat1) = c("n", "cv")
    names(plotDat2) = c("n", "cv")
    plotDat1$method = "cv"
    plotDat2$method = "weber"
    plotDat = rbind(plotDat1, plotDat2)

    q1 = ggplot(data = plotDat, aes(x = n, y = cv, group = method, fill = method)) +
      geom_smooth() +
      geom_line() +
      theme_classic()
    
    plot(q1)
  }

  return(output)
}
