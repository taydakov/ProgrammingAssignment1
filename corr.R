corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  source("complete.R")
  meases <- complete(directory);
  meases <- data.matrix(meases);
  meases <- meases[meases[, 2] > threshold, ]
  if (length(meases[, 1]) == 0)
  {
    return(vector());
  }
  res <- vector();
  for (measnum in 1:length(meases[, 1]))
  {
    # print(meases[measnum, 1]);
    id <- as.numeric(meases[measnum, 1]);
    data <- read.csv(paste(directory, "/", formatC(id, width = 3, flag = "0"), ".csv", sep = ""));
    data <- data[!is.na(data[, 2]) & !is.na(data[, 3]), ];
    res <- c(res, cor(data[, 2], data[, 3]));
    # print(cor(data[, 2], data[, 3]));
  }
  res
}