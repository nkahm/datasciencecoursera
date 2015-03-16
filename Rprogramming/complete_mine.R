complete <- function(directory, id = 1:332){
  count = 1
  for (i in id){
    myfile <- paste(directory,sprintf("%03d",i),sep="/")
    mydata <- read.csv(paste(myfile,"csv",sep="."))
    if (count == 1){
      complete.dat <- rbind(mydata)
    }
    else{
      complete.dat <- rbind(complete.dat,mydata)
    }
    unlink(mydata)
    count <- count + 1
  }
  complete.df <- data.frame(complete.dat)
  mydf <- complete.cases(complete.df)
  nobs = numeric(length(id))
  count = 1
  for (i in id){
    resDF<-as.data.frame(t(c("id"=id,"nobs"=sum(mydf))))
  }
  resDF
}