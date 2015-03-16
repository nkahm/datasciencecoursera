corr <-function(directory, threshold=0){
  
  fn <- function(fname) {
    frame <- read.csv(file.path(directory,fname))
    ok <- complete.cases(frame)
    if (sum(ok) > threshold) {
      return (cor(frame$nitrate, frame$sulfate, use="complete.obs"))
    }
  }
  tcorrs <- sapply(list.files(directory), fn) #get all correlations + NULLs
  tcorrs <- unlist(tcorrs[!sapply(tcorrs, is.null)],use.names = FALSE) #remove NULLs
  return (tcorrs)
}