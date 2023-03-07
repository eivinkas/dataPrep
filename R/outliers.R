removeOutliers = function(df,
                          numerosity = "numerosity",
                          answer = "answer",
                          rt = "rt",
                          workerID = "workerID",
                          minRT = 500,
                          maxRT = Inf,
                          minCor = 0.3,
                          iqrFactor = 4,
                          printSummary = TRUE,
                          plotOutliers = TRUE
                          ) {

  # Setup dummy data frame
  dat = data.frame(workerID = df[,colnames(df) == workerID])
  dat$numerosity = as.numeric(df[,colnames(df) == numerosity])
  dat$answer = as.numeric(df[,colnames(df) == answer])
  dat$rt = as.numeric(df[,colnames(df) == rt])

  # Start count of persons and reponses removed
  totalPersons = length(unique(dat$workerID))
  totalResponses = length(dat[,1])
  removePerson = NULL

  # Add median and IQR response column
  dat$median = 0
  dat$iqr= 0
  num = unique(dat$numerosity)
  for (i in 1:length(num)) {
    numDat = dat[dat$numerosity == num[i],]
    dat[dat$numerosity == num[i],]$median = median(numDat$answer)
    dat[dat$numerosity == num[i],]$iqr = IQR(numDat$answer)
  }

  # Remove NA
  removeNA = length(dat[is.na(dat$answer),1])
  dat = dat[!is.na(dat$answer),]
  df = df[!is.na(dat$answer),]

  # Remove persons with too low numerosity-answer correlation
  remove = NULL
  workers = unique(dat$workerID)
  for (i in 1:length(workers)) {
    workerDat = dat[dat$workerID == workers[i], ]
    if (!is.na(cor(workerDat$numerosity, workerDat$answer))) {
      if (cor(workerDat$numerosity, workerDat$answer) < minCor) {
       removePerson = c(removePerson, workers[i])
      }
    }
  }
  for (i in 1:length(dat[,1])) {
    if (dat$workerID[i] %in% removePerson) remove = c(remove, i)
  }

  # Plot extreme persons
  if (plotOutliers == TRUE && (length(remove) > 0)) {
    library(ggplot2)
    plotDat = dat[remove,]
    if (length(remove) > 1) {
      q = ggplot(dat = plotDat, aes(x = numerosity, y = answer)) +
        geom_point() +
        geom_abline(slope = 1, intercept = 0) +
        facet_wrap(workerID)
    } else {
      q = ggplot(dat = plotDat, aes(x = numerosity, y = answer)) +
        geom_point() +
        geom_abline(slope = 1, intercept = 0)
    }

    plot(q)
  }
  if (length(remove) > 0) {
    dat = dat[-remove,]
    df = df[-remove,]
  }


  # Remove responses with RT outside range
  remove = NULL
  for (i in 1:length(dat[,1])) {
    if (dat[i,]$rt > maxRT | dat[i,]$rt < minRT) remove = c(remove, i)
  }
  if (length(remove) > 0) {
    dat = dat[-remove,]
    df = df[-remove,]
  }
  removeRT = length(remove)

  # Remove extreme responses
  remove = NULL
  for (i in 1:length(dat[,1])) {
    if (dat$answer[i] > dat$median[i] + iqrFactor*dat$iqr[i]) remove = c(remove, i)
  }
  if (length(remove) > 0) {
    dat = dat[-remove,]
    df = df[-remove,]
  }
  removeExtreme = length(remove)

  # Print summary
  if (printSummary == TRUE) {
    print(data.frame("RemovedPersons" = c(removePerson, "---", paste(round(length(removePerson)/totalPersons, 2), "%"), "---")))
    print(data.frame("RemovedItems" = c("NA",
                                        "RT",
                                        "Extreme",
                                        "---",
                                        "Tot.",
                                        "---"),
          percent = c(paste(round(removeNA/totalResponses, 3), "%"),
          paste(round(removeRT/totalResponses, 3), "%"),
          paste(round(removeExtreme/totalResponses, 3), "%"),
          "---",
          paste(round(((removeNA + removeRT + removeExtreme) /totalResponses), 3), "%"),
          "---"
          )))
  }

  # Return output
  return(df)
}
