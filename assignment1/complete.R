complete <- function(directory, id = 1:332) {
        string <- sprintf("%03d.csv",id)
        files <- paste(directory,string,sep ="/")
        nobs <- NULL
        for (file in files ) {
                content <- read.csv(file)
                need <- complete.cases(content$sulfate, content$nitrate)
                nobs <- c(nobs, length(content$Date[need]))
        }
        data.frame(id,nobs)
}