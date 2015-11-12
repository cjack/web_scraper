# Author: Yi Lin
# Main Points
# 1. generate the searching urls ("I am searching for") within 30 days, start from today^^
# 2. read the pages into raw.data ^^
# 3. search the pattern and find the content ^^
# 4. print out all urls and write into output file ^^
# 5. check each time with the output file to warrant an utterly new urls^^
# 6. process each page and write into csv files by appending^^
# 7. done with sample tests^^


#-------------- Pre-definitions ----------------
library(rvest)
require(RCurl)
require(XML)

csvname = "sydney_council.csv"
hostname <- "https://whatson.cityofsydney.nsw.gov.au"
output_filename = "output.txt"

##-----------------------------


main_page_generator <- function(length = 30){
  start_date <- Sys.Date()
  end_date <- start_date + length
  url = paste("https://whatson.cityofsydney.nsw.gov.au/search/advanced?date%5B%5D=", start_date, "&date%5B%5D=", end_date, sep = "")
  return(url)
}
page  <- main_page_generator()

content <- readLines(page)

pattern_match_naive <- function(url, pattern){
  #match the first one
  content <- toString(url)
  #prefix <- regexpr(pattern, content, TRUE)
  prefix <- regexpr(pattern, content, TRUE)
  sufix <- prefix + attr(prefix, "match.length")-1
  return(substr(content, prefix, sufix))
}

pa_prefix <- "href=\"/events/"
pa_sufix <- "\"data-event-type=\'events\'"
pattern <- paste(pa_prefix, ".*?", pa_sufix, sep = "")


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

res <- pattern_match(content, pattern, (nchar(pa_prefix) - 8), nchar(pa_sufix))

scraper_webpage <- function(url){
  library(rvest)
  test <- read_html(url)
  
  ##-----------------------------
  
  search_result <- function(url, token){
    res <- url %>%
      html_nodes(token) %>%
      html_text()
    return(res)
  }
  
  #-----------      Token Matching    ------------------
  title_token = "title"
  title = search_result(test, title_token)
  title
  content_token = "section.event-single-description"
  content = search_result(test, content_token)

  time_token = "dd.details-list-definition div"
  time = search_result(test, time_token)[1]
  
  location_token = "dd.details-list-definition a"
  location = search_result(test, location_token)[1]
  
  cost_token <- "dd.details-list-definition div span"
  cost <- search_result(test, cost_token)[1]
  more_info <- search_result(test, cost_token)[2]

  tag_token = "li.tag"
  tag = search_result(test, tag_token)
  tag <- tag[tag != ""]
  tag <- paste(tag, sep = "",collapse=":")
  
  cast <- html_nodes(test, "meta")
  attrs <- html_attr(cast, "content")
  latitude <- attrs[12]
  longitude <- attrs[13]
  geo <- paste(toString(latitude), toString(longitude), sep = ":")
  #
  
  res <- c(title, time, location, content, cost, more_info, tag, geo)
  Table = matrix(res, nrow = 1, ncol = length(res))
  length(res)
  Table
  #colnames(Table) <- c("Title", "Time", "Location", "Content", "Cost", "MoreInfo","Tags")
  write.table(Table, file = csvname,sep = ",", append = T, row.names = F, col.names = F)
}

for(i in 1:length(res)){
  check_existence <- function(line, lines){
    for(i in 1:length(lines)){
      if(identical(line, lines[i])){
        return(FALSE)
      }
    }
    return(TRUE)
  }
  res[[i]] <- paste(hostname, res[[i]], sep = "")
  pre_urls <- readLines(output_filename)
  if(check_existence(res[[i]], pre_urls)){
    print(paste("Found new pages", res[[i]]))
    write(res[[i]],file=output_filename,append=TRUE)
    scraper_webpage(res[[i]])
  }
}


