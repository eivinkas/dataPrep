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
                          plotOutliers = TRUE,
                          positionOutliers = "top"
                          ) {

  # Add columns for removed persons and items
  df$personOutlier = NA
  df$itemOutlier = NA
  df$outlierType = NA

  # Make empty DF for removed persons and items
  df_removed = df[0,]

  # Setup dummy data frame
  dat = data.frame(workerID = df[,colnames(df) == workerID])
  dat$numerosity = as.numeric(df[,colnames(df) == numerosity])
  dat$answer = as.numeric(df[,colnames(df) == answer])
  dat$rt = as.numeric(df[,colnames(df) == rt])

  # Start count of persons removed
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

  # Remove persons with too low numerosity-to-answer correlation
  remove = NULL
  workers = unique(dat$workerID)

  for (i in 1:length(workers)) {
    # get df for every worker
    workerDat = dat[dat$workerID == workers[i], ]
    if (!is.na(cor(workerDat$numerosity, workerDat$answer))) {
      # add person to the remove-person list if correlation is too low
      if (cor(workerDat$numerosity, workerDat$answer) < minCor) {
       removePerson = c(removePerson, workers[i])
      }
    }
  }
  for (i in 1:length(dat[,1])) {
    # If a row contains a removed person, add the row to the romve-row-list
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
    # Remove rows from the dummy data frame
    dat = dat[-remove,]
    # Add removed rows to the df that stores removed rows
    dummy_removed = df[remove,]    # Write the type of outlier
    dummy_removed$outlierType = "person"
    # Move workerID and reponse to new column and replace with NA
    dummy_removed$personOutlier = dummy_removed[,(names(dummy_removed) == workerID)]
    dummy_removed$itemOutlier = dummy_removed[,(names(dummy_removed) == answer)]
    dummy_removed[,(names(dummy_removed) == workerID)] = NA
    dummy_removed[,(names(dummy_removed) == answer)] = NA
    df_removed = rbind(df_removed, dummy_removed)
    # Remove outliers from data frame
    df = df[-remove,]
  }

  # Remove responses with RT outside range
  remove = NULL
  for (i in 1:length(dat[,1])) {
    if (dat[i,]$rt > maxRT | dat[i,]$rt < minRT) remove = c(remove, i)
  }
  if (length(remove) > 0) {
    # Remove rows from the dummy data frame
    dat = dat[-remove,]
    # Add removed rows to the df that stores removed rows
    dummy_removed = df[remove,]    # Write the type of outlier
    dummy_removed$outlierType = "rt"
    # Move workerID and reponse to new column and replace with NA
    dummy_removed$personOutlier = dummy_removed[,(names(dummy_removed) == workerID)]
    dummy_removed$itemOutlier = dummy_removed[,(names(dummy_removed) == answer)]
    dummy_removed[,(names(dummy_removed) == workerID)] = NA
    dummy_removed[,(names(dummy_removed) == answer)] = NA
    df_removed = rbind(df_removed, dummy_removed)
    # Remove outliers from data frame
    df = df[-remove,]
  }
  removeRT = length(remove)

  # Remove extreme responses
  remove = NULL
  for (i in 1:length(dat[,1])) {
    if (dat$answer[i] > dat$median[i] + iqrFactor*dat$iqr[i]) remove = c(remove, i)
    if (dat$answer[i] < dat$median[i] - iqrFactor*dat$iqr[i]) remove = c(remove, i)
  }
  if (length(remove) > 0) {
    # Remove rows from the dummy data frame
    dat = dat[-remove,]
    # Add removed rows to the df that stores removed rows
    dummy_removed = df[remove,]    # Write the type of outlier
    dummy_removed$outlierType = "extreme response"
    # Move workerID and reponse to new column and replace with NA
    dummy_removed$personOutlier = dummy_removed[,(names(dummy_removed) == workerID)]
    dummy_removed$itemOutlier = dummy_removed[,(names(dummy_removed) == answer)]
    dummy_removed[,(names(dummy_removed) == workerID)] = NA
    dummy_removed[,(names(dummy_removed) == answer)] = NA
    df_removed = rbind(df_removed, dummy_removed)
    # Remove outliers from data frame
    df = df[-remove,]
  }
  removeExtreme = length(remove)

  # Merge OK data and outler data
  if (positionOutliers == "top") df = rbind(df_removed, df)
  if (positionOutliers == "bottom") df = rbind(df, df_removed)

  # Print summary
  if (printSummary == TRUE) {
    print(data.frame("RemovedPersons" = c(removePerson, "---", paste(round(length(removePerson)/totalPersons, 2), "%"), "---")))
    print(data.frame("RemovedItems" = c("RT",
                                        "Extreme",
                                        "---",
                                        "Tot.",
                                        "---"),
          percent = c(paste(round(removeRT/totalResponses, 3), "%"),
          paste(round(removeExtreme/totalResponses, 3), "%"),
          "---",
          paste(round(((removeRT + removeExtreme) /totalResponses), 3), "%"),
          "---"
          )))
  }

  # Return output
  return(df)
}
