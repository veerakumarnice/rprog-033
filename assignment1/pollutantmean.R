pollutantmean <- function(directory, pollutant, id = 1:332) {
        string <- sprintf("%03d.csv",id)
        files <- paste(directory,string,sep ="/")
        ret <- c(NULL)
        for(file in files) {
                content <- read.csv(file)
                our <- content[pollutant]
                bad <- is.na(our)
                need <- our[!bad]
                ret <- c(ret, need)
        }
        mean(ret)
}