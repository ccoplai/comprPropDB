# Web Scraping
# Final project, Database

# Cal Coplai


# Pull data from LoopNet, Zillow, and Trulia


# Zillow, use Import.IO for initial pull of 30 pages of results for "Kalamazoo"


# ZillowR package

kzoozill <- read.csv("finalZillow.csv", header = T, sep = ",")
View(kzoozill)

require(RCurl)
require(ZillowR)
require(XML)
require(dplyr)
require(plyr)

AddressTest <- as.character(kzoozill$Address[1])
AddressTest
set_zillow_web_service_id('X1-ZWz199ft08rabv_8b748')


# TEST
zillowProperty =
  function(address, citystatezip,
           zillowId = getOption("ZillowId", stop("need zillow id")), ...)
  {
    reply = getForm("http://www.zillow.com/webservice/GetDeepSearchResults.htm",
                    'zws-id' = zillowId, 'rentzestimate' = TRUE,
                    address = address,
                    citystatezip = citystatezip,
                    ...)
    doc = xmlParse(reply, asText = TRUE)
    zpid = xmlValue(doc[["//result/zpid"]])
    use = xpathSApply(doc, "//result", function(x) {
      if (xpathSApply(x, "boolean(./useCode)")) {
        xpathSApply(x, "./useCode", xmlValue)
      } else {
        NA
      }
    })
    yearBuilt = xpathSApply(doc, "//result", function(x) {
      if (xpathSApply(x, "boolean(./yearBuilt)")) {
        xpathSApply(x, "./yearBuilt", xmlValue)
      } else {
        NA
      }
    })
    lotSizeSqFt = xpathSApply(doc, "//result", function(x) {
      if (xpathSApply(x, "boolean(./lotSizeSqFt)")) {
        xpathSApply(x, "./lotSizeSqFt", xmlValue)
      } else {
        NA
      }
    })
    finishedSqFt = xpathSApply(doc, "//result", function(x) {
      if (xpathSApply(x, "boolean(./finishedSqFt)")) {
        xpathSApply(x, "./finishedSqFt", xmlValue)
      } else {
        NA
      }
    })
    bedrooms = xpathSApply(doc, "//result", function(x) {
      if (xpathSApply(x, "boolean(./bedrooms)")) {
        xpathSApply(x, "./bedrooms", xmlValue)
      } else {
        NA
      }
    })
    bathrooms = xpathSApply(doc, "//result", function(x) {
      if (xpathSApply(x, "boolean(./bathrooms)")) {
        xpathSApply(x, "./bathrooms", xmlValue)
      } else {
        NA
      }
    })
    estRent = doc[["//result/rentzestimate"]]
    add = doc[["//result/address"]]
    data.frame(zpid = as.numeric(zpid),
               rent = as.numeric(xmlValue(estRent[["amount"]])),
               lowRent = as.numeric(xmlValue(estRent[["valuationRange"]][["low"]])),
               highRent = as.numeric(xmlValue(estRent[["valuationRange"]][["high"]])),
               use = as.character(use),
               yearBuilt = as.numeric(yearBuilt),
               lotSizeSqFt = as.numeric(lotSizeSqFt),
               finishedSqFt = as.numeric(finishedSqFt),
               bathrooms = as.numeric(bathrooms),
               bedrooms = as.numeric(bedrooms),
               address = as.character(xmlValue(add[["street"]])),
               zip = as.numeric(xmlValue(add[["zipcode"]])),
               latitude = as.numeric(xmlValue(add[["latitude"]])),
               longitude = as.numeric(xmlValue(add[["longitude"]])),
               stringsAsFactors = FALSE
    )
  }


out = NULL
for(i in 1:nrow(kzoozill)){
  try(results <- zillowProperty(zillowId = 'X1-ZWz199ft08rabv_8b748', as.character(kzoozill$Address[i]), 'Kalamazoo, MI'))
  out = rbind(out, results)
}

out <- out[!duplicated(out),]

out$Type <- "Residential"
View(out)




# LoopNet

# Load listing file
kzoonet <- read.csv("ScrapeKzoo.csv", header = T, sep = ",")

# find link fields to cbind at the end
names(kzoonet)
keepcols <- c(5,12)
links <- kzoonet[,keepcols]

# parse out labels field
labels <- data.frame(do.call('rbind', strsplit(as.character(kzoonet$SubTitles), '\n', fixed = T)))

# parse out values field
values <- data.frame(do.call('rbind', strsplit(as.character(kzoonet$SubVals), '\n', fixed = T)))
# combine into single data.frame
combintest <- data.frame(labels, values)
# order A-Z
combinetest <- combintest[,order(colnames(combintest))]

# Add ID field for spread function
combinetest$ID <- c(1:nrow(combinetest))

# load tidyr
require(tidyr)
# run spread function on each pair of labels and values and remove extra columns
test <- spread(combinetest, X3, X3.1, fill = NA)
test <- subset(test, , -c(1:13))
test2 <- spread(combinetest, X1, X1.1, fill = NA)
test2 <- subset(test2, , -c(1:13))
test3 <- spread(combinetest, X2, X2.1, fill = NA)
test3 <- subset(test3, , -c(1:13))
test4 <- spread(combinetest, X4, X4.1, fill = NA)
test4 <- subset(test4, , -c(1:13))
test5 <- spread(combinetest, X5, X5.1,fill = NA)
test5 <- subset(test5, , -c(1:13))
test6 <- spread(combinetest, X6, X6.1,fill = NA)
test6 <- subset(test6, , -c(1:13))
test7 <- spread(combinetest, X7, X7.1, fill = NA)
test7 <- subset(test7, , -c(1:12))

# combine all together and order A-Z
finaldata <- data.frame(test, test2, test3, test4, test5, test6, test7)
finaldata <- finaldata[,order(colnames(finaldata))]

# remove unecessary columns
names(finaldata)
keepcols <- c(1:8, 10:15, 18:22)
finaldata <- finaldata[, keepcols]

# merge together duplicate columns
# BldgSize
finaldata$Bldg..Size. <- as.character(finaldata$Bldg..Size.)
finaldata$Bldg..Size..1 <- as.character(finaldata$Bldg..Size..1)

finaldata$BldgSizeSF <- ifelse(is.na(finaldata$Bldg..Size.), finaldata$Bldg..Size..1, finaldata$Bldg..Size.)
finaldata$BldgSizeSF <- substr(finaldata$BldgSizeSF, 1, nchar(finaldata$BldgSizeSF) - 3)

# Cap Rate
finaldata$Cap.Rate. <- as.character(finaldata$Cap.Rate.)
finaldata$Cap.Rate..1 <- as.character(finaldata$Cap.Rate..1)

finaldata$CapRate <- ifelse(is.na(finaldata$Cap.Rate.), finaldata$Cap.Rate..1, finaldata$Cap.Rate.)

# Primary Type
finaldata$Primary.Type. <- as.character(finaldata$Primary.Type.)
finaldata$Primary.Type..1 <- as.character(finaldata$Primary.Type..1)
finaldata$Primary.Type..2 <- as.character(finaldata$Primary.Type..2)

finaldata$PrimaryType <- ifelse(is.na(finaldata$Primary.Type.), finaldata$Primary.Type..1, finaldata$Primary.Type.)
finaldata$PrimaryType <- ifelse(is.na(finaldata$PrimaryType), finaldata$Primary.Type..2, finaldata$PrimaryType)

# Sub Type
finaldata$Sub.Type. <- as.character(finaldata$Sub.Type.)
finaldata$Sub.Type..1 <- as.character(finaldata$Sub.Type..1)
finaldata$Sub.Type..2 <- as.character(finaldata$Sub.Type..2)

finaldata$SubType <- ifelse(is.na(finaldata$Sub.Type.), finaldata$Sub.Type..1, finaldata$Sub.Type.)
finaldata$SubType <- ifelse(is.na(finaldata$SubType), finaldata$Sub.Type..2, finaldata$SubType)

# Remove duplicate columns
names(finaldata)
keepcols <- c(5:8, 12:14, 18:23)
finaldata <- finaldata[, keepcols]

# Get address field from link URLs
addresses <- data.frame(do.call('rbind', strsplit(as.character(kzoonet$LinkURLs_link), '/', fixed = T)))
addresses <- gsub("-", " ", addresses$X6)

finaldata <- cbind(addresses, finaldata)
finaldata <- cbind(finaldata, links)


# Remove "Kalamazoo, MI" into separate field from address
finaldata$addresses <- gsub(" Kalamazoo MI", "", finaldata$addresses)
finaldata$City <- "Kalamazoo"
finaldata$State <- "MI"
finaldata$Type <- "Commercial"
View(finaldata)







# TRULIA

truliakzoo <- read.csv("finaltrulia.csv", head = T, sep = ",")

# field adjustments
# Remove + from prices
truliakzoo$Price.Num <- gsub("\\+", "", truliakzoo$Price.Num)

# Separate out Details fields
truliakzoo$Details <- ifelse(truliakzoo$Details == "", ";", as.character(truliakzoo$Details))
details <- data.frame(do.call('rbind', strsplit(as.character(truliakzoo$Details), ';', fixed = T)))

# remove duplicates for fields without a sq ft field
details$X3 <- ifelse(as.character(details$X3) == details$X1, "", as.character(details$X3))

# sub out text from fields
details$X1 <- gsub("bd", "", details$X1)
details$X2 <- gsub("ba", "", details$X2)
details$X3 <- gsub(" sqft", "", details$X3)

colnames(details)[1] <- "Bdrms"
colnames(details)[2] <- "Bath"
colnames(details)[3] <- "Sq Ft"

# remove city/state from neighb and just keep neighb
truliakzoo$NeighbCityState <- gsub("Kalamazoo, MI", "", truliakzoo$NeighbCityState)
truliakzoo$NeighbCityState <- gsub(", ", "", truliakzoo$NeighbCityState)

# Re-add in Kalamazoo and MI fields
truliakzoo$City <- "Kalamazoo"
truliakzoo$State <- "MI"
truliakzoo$Type <- "Residential"

# Modify desc field
truliakzoo$Desc <- ifelse(truliakzoo$Desc == "", ";", as.character(truliakzoo$Desc))
desc <- data.frame(do.call('rbind', strsplit(as.character(truliakzoo$Desc), ';', fixed = T)))

desc <- desc$X1
desc

# Combine details and desc back together with data
truliakzoo <- cbind(truliakzoo, details, desc)

# remove unnecessary columns
names(truliakzoo)
keepcols <- c(2, 4, 6:14)
truliakzoo <- truliakzoo[,keepcols]
View(truliakzoo)




#  MERGE ALL TOGETHER

allprop <- merge(truliakzoo, out, by.x = "Address", by.y = "address", all = T)
allprop <- merge(allprop, finaldata, by.x = "Address", by.y = "addresses", all = T)

# Remove any duplicates
allprop <- subset(allprop, select = -ID)
allprop <- allprop[!duplicated(allprop),]
View(allprop)



# Test getting lat long
require(ggmap)

latlong <- geocode(paste(allprop$Address, "Kalamazoo, MI"))
View(latlong)

allprop <- cbind(allprop, latlong)
View(allprop)




# Tie in acs.R?
require(acs)
help(acs)













# This function works in printing each out, but need to figure out how to combine into one results set that can then be 
# fed into the results / out functions
List = list()

for(i in 1:nrow(kzoozill)){
  Tests <- GetDeepSearchResults(address = as.character(kzoozill$Address[i]), citystatezip = 'Kalamazoo, MI', rentzestimate = T, url = "http://www.zillow.com/webservice/GetDeepSearchResults.htm")
  List[[length(List)+1]] = Tests
  print(Tests)
}



List[[1:2]]$response
Listset <- getNodeSet(List, "/results")
List[[16]][[3]]
List[[15]][[3]] <- NA

ldply(testlist, data.frame)
# Good there, got all results into XML form

List2 = list()
for(i in 1:452){
  x <- ifelse(is.null(List[[i]][[3]]), 1, List[[i]][[3]])
  List2[[length(List2)+1]] = x
}

List2[[2]]
which(is.null(List$response[["results"]]))
List$response[["results"]]

testfunction <- function(input){
  
  x <- ifelse(List[[]])
}

xmlValue(List[[4]][[3]])
# Could try to change this piece and iterate at "response" to go through each item's results
testlist[[nums]][[3]] == "NULL" <- NA

nums <- c(1:3)
resultsTest <- xmlToList(List[[1]][[3]])
resultsTest
?xmlToList
results <- xmlToList(Tests$response[["results"]])
results

getValRange <- function(x, hilo) {
  ifelse(hilo %in% unlist(dimnames(x)), x["text",hilo][[1]], NA)
}

out <- apply(results, MAR=2, function(property) {
  zpid <- property$zpid
  links <- unlist(property$links)
  address <- unlist(property$address)
  FIPS <- property$FIPScounty
  useCode <- property$useCode
  taxAssessYear <- property$taxAssessmentYear
  taxAssessVal <- property$taxAssessment
  yearBuilt <- property$yearBuilt
  lotsizeSqft <- property$lotSizeSqFt
  finishedSqft <- property$finishedSqFt
  bath <- property$bathrooms
  bedrooms <- property$bedrooms
  rooms <- property$totalRooms
  z <- property$zestimate
  y <- property$rentzestimate
  neighb <- unlist(property$localRealEstate)
  zestdf <- list(
    amount=ifelse("text" %in% names(z$amount), z$amount$text, NA),
    lastupdated=z$"last-updated",
    valueChange=ifelse(length(z$valueChange)==0, NA, z$valueChange),
    valueLow=getValRange(z$valuationRange, "low"),
    valueHigh=getValRange(z$valuationRange, "high"),
    percentile=z$percentile)
  rentzestdf <- list(
    rentamount=ifelse("text" %in% names(y$amount), y$amount$text, NA),
    rentlastupdated=y$"last-updated",
    rentvalueChange=ifelse(length(y$valueChange)==0, NA, y$valueChange),
    rentvalueLow=getValRange(y$valuationRange, "low"),
    rentvalueHigh=getValRange(y$valuationRange, "high")
  )
  list(id=zpid, links, address, FIPS, useCode, taxAssessYear, taxAssessVal, 
       yearBuilt, lotsizeSqft, finishedSqft, bath, bedrooms, rooms, zestdf, rentzestdf, neighb)
})


data <- as.data.frame(do.call(rbind, lapply(out, unlist)), 
                      row.names=seq_len(length(out)))

View(data)








# WORKS for 1
Test <- GetSearchResults(address = as.character(kzoozill$Address[1]), citystatezip = 'Kalamazoo, MI', rentzestimate = T, url = "http://www.zillow.com/webservice/GetSearchResults.htm")
Test


results <- xmlToList(Test$response[["results"]])
results

getValRange <- function(x, hilo) {
  ifelse(hilo %in% unlist(dimnames(x)), x["text",hilo][[1]], NA)
}

out <- apply(results, MAR=2, function(property) {
  zpid <- property$zpid
  links <- unlist(property$links)
  address <- unlist(property$address)
  z <- property$zestimate
  y <- property$rentzestimate
  zestdf <- list(
    amount=ifelse("text" %in% names(z$amount), z$amount$text, NA),
    lastupdated=z$"last-updated",
    valueChange=ifelse(length(z$valueChange)==0, NA, z$valueChange),
    valueLow=getValRange(z$valuationRange, "low"),
    valueHigh=getValRange(z$valuationRange, "high"),
    percentile=z$percentile)
  rentzestdf <- list(
    rentamount=ifelse("text" %in% names(y$amount), y$amount$text, NA),
    rentlastupdated=y$"last-updated",
    rentvalueChange=ifelse(length(y$valueChange)==0, NA, y$valueChange),
    rentvalueLow=getValRange(y$valuationRange, "low"),
    rentvalueHigh=getValRange(y$valuationRange, "high")
  )
  list(id=zpid, links, address, zestdf, rentzestdf)
})

data <- as.data.frame(do.call(rbind, lapply(out, unlist)), 
                      row.names=seq_len(length(out)))

View(data)



# WORKS FOR 2 BEFORE FUNCTION TEST

resultsTest <- xmlToList(List[[1]][[3]])
resultsTest

results <- xmlToList(Tests$response[["results"]])
results

getValRange <- function(x, hilo) {
  ifelse(hilo %in% unlist(dimnames(x)), x["text",hilo][[1]], NA)
}

out <- apply(results, MAR=2, function(property) {
  zpid <- property$zpid
  links <- unlist(property$links)
  address <- unlist(property$address)
  FIPS <- property$FIPScounty
  useCode <- property$useCode
  taxAssessYear <- property$taxAssessmentYear
  taxAssessVal <- property$taxAssessment
  yearBuilt <- property$yearBuilt
  lotsizeSqft <- property$lotSizeSqFt
  finishedSqft <- property$finishedSqFt
  bath <- property$bathrooms
  bedrooms <- property$bedrooms
  rooms <- property$totalRooms
  z <- property$zestimate
  y <- property$rentzestimate
  neighb <- unlist(property$localRealEstate)
  zestdf <- list(
    amount=ifelse("text" %in% names(z$amount), z$amount$text, NA),
    lastupdated=z$"last-updated",
    valueChange=ifelse(length(z$valueChange)==0, NA, z$valueChange),
    valueLow=getValRange(z$valuationRange, "low"),
    valueHigh=getValRange(z$valuationRange, "high"),
    percentile=z$percentile)
  rentzestdf <- list(
    rentamount=ifelse("text" %in% names(y$amount), y$amount$text, NA),
    rentlastupdated=y$"last-updated",
    rentvalueChange=ifelse(length(y$valueChange)==0, NA, y$valueChange),
    rentvalueLow=getValRange(y$valuationRange, "low"),
    rentvalueHigh=getValRange(y$valuationRange, "high")
  )
  list(id=zpid, links, address, FIPS, useCode, taxAssessYear, taxAssessVal, 
       yearBuilt, lotsizeSqft, finishedSqft, bath, bedrooms, rooms, zestdf, rentzestdf, neighb)
})
}

data <- as.data.frame(do.call(rbind, lapply(out, unlist)), 
                      row.names=seq_len(length(out)))

View(data)



