complete <- function(directory, id=1:332) {
  as.data.frame(t(sapply(id, fn, directory)))}


fn <- function(id, directory) {
  zero <- sprintf("%03d", id)
  name <- paste(directory,"/",zero,".csv",sep="")
  frame <- read.csv(name)
  ok <- complete.cases(frame)
  return(c("id"=id,"nobs"=sum(ok))) }