corr <- function(directory, threshold = 0) {
        csvs <- list.files(directory)
        files <- paste(directory,csvs,sep ="/")
        nobs <- NULL
        vect <- NULL
        for (file in files ) {
                content <- read.csv(file)
                need <- complete.cases(content$sulfate, content$nitrate)
                if (length(content$Date[need]) > threshold) {
                        sul <- content$sulfate[need]
                        nit <- content$nitrate[need]
                        vect <- c(vect, cor(sul, nit) )
                }
        }
        vect
}