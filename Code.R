## Functions used by Code.Rmd

loadData <- function(FILENM) {
    require(gdata)
    data = read.xls(FILENM, sheet = 1, header = TRUE, blank.lines.skip=TRUE)
    return (data)
}

cleanData <- function(data, FILENM) {
    data$X.ID <- NULL
    colnames(data) <- c("Code", "Dept","Name","Date","DOW","ShiftQty","ShiftDays","AbsenceCode")
    data$Date <- as.Date(data$Date)
    # make the DOW an ordered factor
    data$DOW <- ordered(data$DOW, levels=c("Mon", "Tue", "Wed", "Thu", "Fri"))
    
    # Generate information on whether the employee is currently employed or not
    dataEmp <- data.frame(unique(data[, c("Code", "Name")]))
    dataEmp <- within (dataEmp, {
        n <- gsub("^\\s+|\\s+$", "", as.character(Name))
        CurrEmp <- ifelse(substr(n, nchar(n)-3, nchar(n)) == " (L)", FALSE, TRUE)
        Name <- ifelse(substr(n, nchar(n)-3, nchar(n)) == " (L)", substr(n, 1, nchar(n)-4), n)
        rm(n)
    })
    
    data <- within (data, {
        n <- gsub("^\\s+|\\s+$", "", as.character(Name))
        Name <- ifelse(substr(n, nchar(n)-3, nchar(n)) == " (L)", substr(n, 1, nchar(n)-4), n)
        rm(n)
    })
    
    # save the data for future processing
    FileProcDt <- format(Sys.time(), "%a %d/%m/%Y at %H:%M")
    
    save(data, dataEmp, FileProcDt, file = FILENM)
    return (data)
}

## plots a bar chart from the passed data. Columns organized by Year
ggByYear <- function(data) {
    x <- strftime(data$Date, "%Y") 
    y <- rep(1, length(x))
    
    # Pre-Allocate the table
    minYr = min(as.numeric(strftime(data$Date, "%Y")))
    maxYr = min(as.numeric(strftime(data$Date, "%Y")))
    # The table will contain the number of months in a year.
    n <- (maxYr - minYr + 1)
    dt <- character(n)
    cnt <- numeric(n)
    for (i in minYr:maxYr) {
        lev <- i - minYr + 1
        dt[lev] <- as.character(i)
        cnt[lev] <- 0
    }
    tmp <- data.frame(dt, cnt, stringsAsFactors = FALSE)
    
    tmp <- rbind(tmp, data.frame(dt = x, cnt = y))
    tmp <- aggregate(cnt ~ dt, tmp, sum)
    
    g <- ggplot(tmp, (aes(x = dt, y = cnt)))
    g <- g + geom_bar(stat="identity") + ggtitle("Leave taken by Year") + xlab("Year") +
        ylab("Days") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return (g)
}

ggByMonYr <- function (data, REPSTRTDT, REPENDDT) {
    x <- strftime(data$Date, "%m-%Y") 
    y <- rep(1, length(x))
    
    RepStrtY <- as.numeric(format(REPSTRTDT, "%Y"))
    RepStrtM <- as.numeric(format(REPSTRTDT, "%m"))
    RepEndY <- as.numeric(format(REPENDDT, "%Y"))
    RepEndM <- as.numeric(format(REPENDDT, "%m"))
    
    # Pre-Allocate the table
    # The table will contain the number of months in a year.
    n <- (RepEndY - RepStrtY + 1) * 12
    dt <- character(n)
    cnt <- numeric(n)
    for (i in RepStrtY:RepEndY) {
        for (j in c("01","02","03","04","05","06","07","08","09","10","11","12")) {
            # Ignore of the date is larger than the processing date
            intj <- as.numeric(j)
            if ((i == RepStrtY) && (intj >= RepStrtM)) {
                if ((i == RepEndY) && (intj <= RepEndM)) {
                    lev <- (i - RepStrtY) * 12 + intj
                    dt[lev] <- paste0(j,"-",as.character(i))
                    cnt[lev] <- 0
                }
            }
        }
    }
    tmp <- data.frame(dt, cnt, stringsAsFactors = FALSE)
    
    tmp <- rbind(tmp, data.frame(dt = x, cnt = y))
    tmp <- aggregate(cnt ~ dt, tmp, sum)
    
    g <- ggplot(tmp, (aes(x = dt, y = cnt)))
    g <- g + geom_bar(stat="identity") + ggtitle("Leave taken by Month") + xlab("Month - Year") +
        ylab("Days") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return (g)
}
