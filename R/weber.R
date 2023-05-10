weberMeasure = function(df,
                            numerosity = "numerosity",
                            answer = "answer",
                            central = "median",
                            plot = "both",
                            smooth = TRUE,
                            se = TRUE,
                            line = TRUE) {

  # Setup data frame
  dat = data.frame(numerosity = df[,colnames(df) == numerosity])
  dat$answer = df[,colnames(df) == answer]

  # Setup empty output frame
  dots = sort(unique(df[,(colnames(df) == numerosity)]))
  output = data.frame(n = dots)
  output$CV = 0
  output$Weber = 0
  output$perceivedN = 0

  for (i in 1:length(dots)) {
    newDat = dat[dat$numerosity == dots[i],]
    getSD = sd(newDat$answer)
    if (central == "mean") m = mean(newDat$answer)
    if (central == "median") m = median(newDat$answer)
    output$CV[i] = getSD/dots[i]
    output$Weber[i] = getSD/m
    output$perceivedN[i] = m
  }

  # Plot
  if (plot == "both") {
    library(ggplot2)
    plotDat1 = output[,1:2]
    plotDat2 = output[,c(1,3)]
    names(plotDat1) = c("n", "cv")
    names(plotDat2) = c("n", "cv")
    plotDat1$method = "cv"
    plotDat2$method = "weber"
    plotDat = rbind(plotDat1, plotDat2)

    q1 = ggplot(data = plotDat, aes(x = n, y = cv, group = method, fill = method))
    if (smooth) q1 = q1 +  geom_smooth(se = se, method = "loess", method.args = list(family = "symmetric"))
    if (line) q1 = q1 + geom_line()
    q1 = q1 + theme_classic()
    q1 = q1 + xlab("n / m") + ylab("cv / weber")


    plot(q1)
  }

  if (plot == "cv") {
    library(ggplot2)
    plotDat1 = output[,1:2]
    names(plotDat1) = c("n", "cv")
    plotDat1$method = "cv"
    plotDat = plotDat1
    q1 = ggplot(data = plotDat, aes(x = n, y = cv))
    if (smooth) q1 = q1 +  geom_smooth(se = se, method = "loess", method.args = list(family = "symmetric"))
    if (line) q1 = q1 + geom_line()
    q1 = q1 + theme_classic()
    plot(q1)
  }

  if (plot == "weber") {
    library(ggplot2)
    plotDat2 = output[,c(1,3)]
    names(plotDat2) = c("m", "weber")
    plotDat2$method = "weber"
    plotDat = plotDat2
    q1 = ggplot(data = plotDat2, aes(x = m, y = weber))
    if (smooth) q1 = q1 +  geom_smooth(se = se, method = "loess", method.args = list(family = "symmetric"))
    if (line) q1 = q1 + geom_line()
    q1 = q1 + theme_classic()
    plot(q1)
  }

  return(output)
}
