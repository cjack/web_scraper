## author: Yi Lin
## 2016.1.14
## scraper for gold coast


library(rvest)
require(RCurl)

csvname = "goldcoast.csv"
host <- "http://www.goldcoast.qld.gov.au"
output_filename = "output_goldcoast.txt"


if(!file.exists(output_filename)){
  file.create(output_filename)
  print("creating new output file")
}

remove_comma <- function(str){
    return(gsub(",", ".", str))
}
remove_space <- function(str){
    while(str != gsub("  ", " ", str)){
        str <- gsub("  ", " ", str)
    }
    return(str)
}

search_result <- function(url, token){
  res <- url %>%
    html_nodes(token) %>%
    html_text()
  if((length(res) == 0) && (typeof(res) == "character"))
    res <- ""
  res <- toString(res)
  res <- gsub("\r\n", "", res)
  res <- remove_space(res)
  print(res)
  return(res)
}

search_attr <- function(url, token, attr){
  cast <- html_nodes(url, token)
  res <- html_attr(cast, attr)
  if((length(res) == 0) && (typeof(res) == "character"))
    res <- ""
  res <- remove_comma(toString(res))
  return(toString(res))
}


pattern_match<- function(url, pattern, pre_sz = 0, suf_sz = 0){
  content <- toString(url)
  len <- nchar(content)
  res <- list()
  prefix <- gregexpr(pattern, content)

  for(i in 1:length(prefix[[1]])){
    sufix <- prefix[[1]][i] +  attr(prefix[[1]], "match.length")[i] - 1
    res <- c(res, substr(content, prefix[[1]][i] + pre_sz , sufix - suf_sz))
  }
  return(res)
}







processOnePage <- function(url){
    test = read_html(url)
    title_token = "h2"
    title = search_result(test, title_token)

    detailsToken = "div.event-body"
    details = search_result(test, detailsToken)

    eventsToken <- "div.event-details"
    events <- search_result(test, eventsToken)
    events <- gsub("\n \n", "", events)
    events

    ppf <- "When:"
    ##
    psf <- "Where:"                                                ##                                      ##
    pattern <- paste(ppf, ".*?", psf, sep = "")                    ##                                    ##
    ##                                    ##
    res <- pattern_match(events, pattern, nchar(ppf), nchar(psf)) ##                                    ##
    res <- gsub("<br/>", "\n", res)


    date <- res##                                    ##
    ##                                 ## ##
    ppf <- "Where:"
    ##
    psf <- "Cost:"                                                ##                                      ##
    pattern <- paste(ppf, ".*?", psf, sep = "")                    ##                                    ##
    ##                                    ##
    res <- pattern_match(events, pattern, nchar(ppf), nchar(psf)) ##                                    ##
    res <- gsub("<br/>", "\n", res)
    address <- res

    ppf <- "Cost:"
    ##
    psf <- "Type:"                                                ##                                      ##
    pattern <- paste(ppf, ".*?", psf, sep = "")                    ##                                    ##
    ##                                    ##
    res <- pattern_match(events, pattern, nchar(ppf), nchar(psf)) ##                                    ##
    res <- gsub("<br/>", "\n", res)
    cost <- res

    ppf <- "Organisation:"
    ##
    psf <- "Phone:"                                                ##                                      ##
    pattern <- paste(ppf, ".*?", psf, sep = "")                    ##                                    ##
    ##                                    ##
    res <- pattern_match(events, pattern, nchar(ppf), nchar(psf)) ##                                    ##
    res <- gsub("<br/>", "\n", res)
    Organizer <- res

    ppf <- "Phone:"
    ##
    psf <- "Email"                                               ##                                      ##
    pattern <- paste(ppf, ".*?", psf, sep = "")                    ##                                    ##
    ##                                    ##
    res <- pattern_match(events, pattern, nchar(ppf), nchar(psf)) ##                                    ##
    res <- gsub("<br/>", "\n", res)
    phone <- res

    ppf <- "Email:"
    ##
    psf <- "Web"                                               ##                                      ##
    pattern <- paste(ppf, ".*?", psf, sep = "")                    ##                                    ##
    ##                                    ##
    res <- pattern_match(events, pattern, nchar(ppf), nchar(psf)) ##                                    ##
    res <- gsub("<br/>", "\n", res)

    email <- res

    ppf <- "Web:"
    ##
    psf <- "Map"                                               ##                                      ##
    pattern <- paste(ppf, ".*?", psf, sep = "")                    ##                                    ##
    ##                                    ##
    res <- pattern_match(events, pattern, nchar(ppf), nchar(psf)) ##                                    ##
    res <- gsub("<br/>", "\n", res)
    web <- res

    raw <- readLines(url)
    ppf <- "<a href=\"/_images/"
    ##
    psf <- "\""                                               ##                                      ##
    pattern <- paste(ppf, ".*?", psf, sep = "")                    ##                                    ##
    ##                                    ##
    res <- pattern_match(raw, pattern, nchar(ppf) - nchar("/_images/"), nchar(psf)) ##                                    ##
    res <- gsub("<br/>", "\n", res)
    res <- paste(host, res, sep = "")
    imgUrl <- res


    ppf <- "\"latitude\" : \""
    ##
    psf <- "\","                                               ##                                      ##
    pattern <- paste(ppf, ".*?", psf, sep = "")                    ##                                    ##
    ##                                    ##
    res <- pattern_match(raw, pattern, nchar(ppf), nchar(psf)) ##                                    ##
    res <- gsub("<br/>", "\n", res)
    lat <- res

    ppf <- "\"longitude\" : \""
    ##
    psf <- "\","                                               ##                                      ##
    pattern <- paste(ppf, ".*?", psf, sep = "")                    ##                                    ##
    ##                                    ##
    res <- pattern_match(raw, pattern, nchar(ppf), nchar(psf)) ##                                    ##
    res <- gsub("<br/>", "\n", res)
    lon <- res

    res <- c(url, title, date, address, details, phone, email, web, lat, lon, imgUrl)

    print(res)


                                        #check if exist csv file, if not, add the col names                                         ##                                                            ##
    if(!file.exists(csvname)){                                                                  ##                                                            ##
        file.create(csvname)                                                                      ##                                                            ##
        col_name = matrix(res, nrow = 1, ncol = length(res))                                      ##                                                            ##
        colnames(col_name) <- c( "URL", "Name", "Date", "Address", "Details", "Phone", "Email", "Web", "lat", "lon", "ImageURL")                             ## ##
        write.table(col_name, file = csvname,sep = ",", append = T, row.names = F, col.names = T) ##                                                            ##
        print("creating new csv file")                                                            ##                                                            ##
    }else{                                                                                      ##                                                            ##
        Table = matrix(res, nrow = 1, ncol = length(res))                                         ##                                                            ##
        if(summary != "" && location != "")                                                       ##                                                            ##
            write.table(Table, file = csvname,sep = ",", append = T, row.names = F, col.names = F)  ##                                                            ##
    }                                                                                                                                                         ##

    }

processOneMain <- function(url){
    test <- read_html(url)
    urls <- search_attr(test, "h3.title a", "href")
    urls <- strsplit(urls, ". ")


    for(i in 1:length(urls[[1]])){
        link <- paste(host, "/", urls[[1]][i], sep = "")
        try(processOnePage(link))
    }
}


url = "http://www.goldcoast.qld.gov.au/community-events-24638.html?pg=1"
 
raw <- readLines(url)##
                               ##                                 ## ##
ppf <- "last active sprite\" href=\"?"
                                                                     ##
psf <- "\">last</a>"                                                ##                                      ##
pattern <- paste(ppf, ".*?", psf, sep = "")                    ##                                    ##
                                     ##                                    ##
res <- pattern_match(raw, pattern, nchar(ppf), nchar(psf)) ##                                    ##
res <- gsub("<br/>", "\n", res)
res <- sub("pg=", "", res)
res <- as.integer(res)
max <- res

for(i in 1:max){
    hh <- "http://www.goldcoast.qld.gov.au/community-events-24638.html?pg="
    url <- paste(hh, toString(i), sep = "")
    try(processOneMain(url))
}
