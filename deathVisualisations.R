## Function that draws a histogram or homicides, month by month
drawHisto = function(startYear = 2007, lastYear = 2014) {
  library('ggplot2')
  years <- c(startYear:lastYear)
  baseURL = "http://data.baltimoresun.com/bing-maps/homicides/index.php?range=year&district=all&zipcode=all&age=all&gender=all&race=all&cause=all&article=all&show_results=Show+results"
  urls = vector()
  for (year in years) {
    urls = c(urls, gsub("year", year, baseURL))
  }
  ## Download htmls into various text files
  homicides <- vector()
  for (i in 1:length(years)) {
    download.file(urls[i], paste(years[i],".txt", sep="")) # Downloads data afresh
    homicides <- c(homicides, suppressWarnings(readLines(paste(years[i],".txt", sep=""))))
  }
  # Remove lines from homicides that contain [Cc]ause - these contain murder info
  homicides <- homicides[grepl("[Cc]ause", homicides)]
  print(homicides[sample(1:length(homicides),1)])
  print(homicides[1666])
  causes <- getCauses(homicides)
  dates <- getDates(homicides)
  genders <- getGender(homicides)
  race <- getRace(homicides)
  names <- getNames(homicides)
  ages <- getAges(homicides)
  addresses <- getAddresses(homicides)
  zipCodes <- getZipCodes(addresses)
  #   print(paste(length(homicides), length(causes), length(dates)), sep =" ")
  # Histogram of murders by month as per Roger's example
  dFr <- data.frame(causes = causes, dates = dates, genders = genders, race = race, names = names, ages = ages, addresses = addresses, zipCodes = zipCodes)
  dFr <- dFr[complete.cases(dFr[, "causes"]), ]
  write.csv(dFr, "out.csv")
  par(mfrow=c(1,1))
  foo <- as.numeric(zipCodes)
  foo <- foo[!is.na(foo)]
  print(table(foo))
  qplot(zipCodes)
  return(dFr)
  
  
#   Homicides histograms by time period
#   hist(dates, "day", freq=TRUE, main="Histogram of Homicides by Day", ylab="Homicides", col = "gray", xaxt='n')
#   abline(v = median(dates, na.rm = TRUE), col = "blue")
#   axis(side=1, at=seq(as.Date("2007/1/1"), as.Date("2014/1/1"), "days"), labels=seq(as.Date("2007/1/1"), as.Date("2014/1/1"), "days"))
#   hist(dates, "month", freq=TRUE, main="Histogram of Homicides by Month", ylab="Homicides", col = "blue", xaxt='n')
#   abline(v = median(dates, na.rm = TRUE), col = "blue")
#   axis(side=1, at=seq(as.Date("2007/1/1"), as.Date("2014/1/1"), "months"), labels=seq(as.Date("2007/1/1"), as.Date("2014/1/1"), "months"))
#   hist(dates, "year", freq=TRUE, main="Histogram of Homicides by Year", ylab="Homicides", col = "gray", xaxt='n')
#   abline(v = median(dates, na.rm = TRUE), col = "blue")
#   axis(side=1, at=seq(as.Date("2007/1/1"), as.Date("2014/1/1"), "years"), labels=seq(as.Date("2007/1/1"), as.Date("2014/1/1"), "years"), las=3)
}

## Return vector containing cause of homicide
getCauses <- function(homicides) {
  causeIndices <- regexec("<dd>[Cc]ause: (.*?)</dd>", homicides)
  causesDirty <- regmatches(homicides, causeIndices) 
  causes <- sapply(causesDirty, function(x) x[2])
  return(causes)
}

## Return vector containing gender of victim
getGender <- function(homicides) {
  genderIndices <- regexec("[Gg]ender: (.*?)<", homicides)
  genderDirty <- regmatches(homicides, genderIndices)
  gender <- sapply(genderDirty, function(x) x[2])
  return(gender)
}

## Return vector containing race of victim
getRace <- function(homicides) {
  raceIndices <- regexec("<dd>[Rr]ace: (.*?)<br", homicides)
  raceDirty <- regmatches(homicides, raceIndices)
  race <- sapply(raceDirty, function(x) x[2])
  return(race)
}

getNames <- function(homicides) {
  nameIndices <- regexec("\\\">(.*?)</a>", homicides)
  nameDirty <- regmatches(homicides, nameIndices)
  names <- sapply(nameDirty, function(x) x[2])
  return(names)
}

getAges <- function(homicides) {
  agesIndices <- regexec("[Aa]ge: (.*?) ", homicides)
  agesDirty <- regmatches(homicides, agesIndices)
  ages <- sapply(agesDirty, function(x) x[2])
  return(ages)
}

getAddresses <- function(homicides) {
  addressIndices <- regexec("class=\\\"address\\\">(.*?)</dd>", homicides)
  addressDirty <- regmatches(homicides, addressIndices)
  address <- sapply(addressDirty, function(x) x[2])
  address <- gsub("<br />", ", ", address)
  return(address)
}


getZipCodes <- function(addresses) {
  zipCodeIndices <- regexec("([0-9]+)$", addresses)
  zipCodeDirty <- regmatches(addresses, zipCodeIndices)
  zipCode <- sapply(zipCodeDirty, function(x) x[2])
  return(zipCode)
}

## Return dates of homicides - Found date. Date of homicide unknown
getDates <- function(homicides) {
  dateIndices <- regexec("<dd>[F|f]ound on (.*?)</dd>", homicides)
  dateStringDirty <- regmatches(homicides, dateIndices)
  dateStringClean <- sapply(dateStringDirty, function(x) x[2])
  dates <- as.Date(dateStringClean, "%B %d, %Y")
  return(dates)
}