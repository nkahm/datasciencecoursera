pollutantmean <- function(directory, pollutant, id = 1:332){
  options(digits=4)
  nc <- if (pollutant == 'sulfate'){2} else if (pollutant == 'nitrate') {3}
  count <- 1
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
  mean(complete.dat[,nc], na.rm = TRUE)
}