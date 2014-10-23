data <- data.frame(Date = as.Date(c("01/01/2014","02/01/2014","03/03/2014","07/08/2014","08/08/2014","09/08/2014","10/10/2014"),  "%d/%m/%Y"))

x <- as.Date(data$Date)
y <- sample(10, length(x))
tmp <- data.frame(dt = format(x, "%Y-%m"), cnt = y, stringsAsFactors = FALSE)

# Pre-Allocate the table
minYr = min(as.numeric(strftime(data$Date, "%Y")))
maxYr = min(as.numeric(strftime(data$Date, "%Y")))
# The table will contain the number of months in a year.
n <- (maxYr - minYr + 1) * 12
dt <- character(n)
cnt <- numeric(n)
for (i in minYr:maxYr) {
    for (j in c("01","02","03","04","05","06","07","08","09","10","11","12")) {
        lev <- (i - minYr) * 12 + as.numeric(j)
        dt[lev] <- paste0(as.character(i),"-",j,"-01")
        cnt[lev] <- 0
    }
}
dt = as.Date(dt, format="%Y-%m-%d")
tmp <- data.frame(dt = format(dt, "%Y-%m"), cnt, stringsAsFactors = FALSE)
tmp <- rbind(tmp, data.frame(dt = format(x, "%Y-%m"), cnt = y, stringsAsFactors = FALSE))


tmp2 <- aggregate(cnt ~ dt, tmp, sum)

g <- ggplot(tmp2, (aes(x = dt, y = cnt)))
g + geom_bar(stat="identity")