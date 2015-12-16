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

csvname = "sydney_council.csv"
hostname <- "https://whatson.cityofsydney.nsw.gov.au"
output_filename = "output_sydney_council.txt"

##-----------------------------

if(!file.exists(output_filename)){
  file.create(output_filename)
  print("creating new output file")
}

main_page_generator <- function(length = 30){
  start_date <- Sys.Date()
  end_date <- start_date + length
  url = paste("https://whatson.cityofsydney.nsw.gov.au/search/advanced?date%5B%5D=", start_date, "&date%5B%5D=", end_date, sep = "")
  return(url)
}
oneyear = 356
page  <- main_page_generator(oneyear)

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

searchXpath <- function(url, token){
    res <- html_nodes(url, xpath = token) %>%  html_text()
    if((length(res) == 0) && (typeof(res) == "character"))
        res <- ""
    print(res)
    return(res)
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

res <- pattern_match(content, pattern, (nchar(pa_prefix) - 8), nchar(pa_sufix))

  remove_comma <- function(str){
    return(gsub("\"", "'", str))
  }
  remove_space <- function(str){
    while(str != gsub("  ", " ", str)){
      str <- gsub("  ", " ", str)
    }
    return(str)
  }


scraper_webpage <- function(url){
  library(rvest)
  test <- read_html(url)
  
  search_result <- function(url, token){
      
    res <- url %>%
      html_nodes(token) %>%
      html_text()
    if((length(res) == 0) && (typeof(res) == "character"))
        res <- ""
    print(res)
    return(res)
  }  
  
  
  ##-----------------------------
  
  ############################################
  ## search_result <- function(url, token){ ##
  ##   res <- url %>%                       ##
  ##     html_nodes(token) %>%              ##
  ##     html_text()                        ##
  ##   return(res)                          ##
  ## }                                      ##
  ############################################
  
  #-----------      Token Matching    ------------------
  title_token = "title"
  title = search_result(test, title_token)
  title
  content_token = "section.event-single-description"
  content = search_result(test, content_token)
  
  time_token = "dd.details-list-definition" 
  time = search_result(test, time_token)[1] 
  
  locationToken <- "//dd[@itemprop='location']"
  location <- searchXpath(test, locationToken)

  #########################################################
  ## more_token <- "dd.details-list-definition div span" ##
  ## more_info <- search_result(test, cost_token)[1]     ##
  #########################################################
  cost <- search_result(test, time_token)[3]

  
  ###########################################################
  ## cost_token <- "dd.details-list-definition div div" ## ##
  ## cost <- search_result(test, cost_token)[1]            ##
  ###########################################################
  ##
  more_token = "dd.details-list-definition span a" 
  more_info = search_result(test, more_token)[2]
 # more_info <- search_result(test, time_token)[4]     ##
                                                      ##
   ## moreToken= "dd.details-list-definition"            ## ##
   ## more_info = search_result(test, moreToken)            ##
    ##
  

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
  empty = ""
  res <- c(empty, empty, empty, tag, url, title, time, location, content, cost, more_info, geo)

   #check if exist csv file, if not, add the col names
  if(!file.exists(csvname)){
    file.create(csvname)
    col_name = matrix(res, nrow = 1, ncol = length(res))
    colnames(col_name) <- c("Kids Related", "Comment", "Tags", "Tag in website", "URL", "Title", "Dates", "Location", "Content", "Cost", "Phone", "Geo")
    write.table(col_name, file = csvname,sep = ",", append = T, row.names = F, col.names = T)
    print("creating new csv file")
  }else{
    Table = matrix(res, nrow = 1, ncol = length(res))
    #if(summary != "" && location != "")
      write.table(Table, file = csvname,sep = ",", append = T, row.names = F, col.names = F)
  }

  ##############################################################################################
  ## Table = matrix(res, nrow = 1, ncol = length(res))                                        ##
  ## length(res)                                                                              ##
  ## Table                                                                                    ##
  ## #colnames(Table) <- c("Title", "Time", "Location", "Content", "Cost", "MoreInfo","Tags") ##
  ## write.table(Table, file = csvname,sep = ",", append = T, row.names = F, col.names = F)   ##
  ##############################################################################################
}

for(i in 1:length(res)){
  check_existence <- function(line, lines){
    for(i in 1:length(lines)){
      if(identical(line, lines[i])){
        return(TRUE)
      }
    }
    return(FALSE)
  }
  res[[i]] <- paste(hostname, res[[i]], sep = "")
  pre_urls <- readLines(output_filename)
  if(!check_existence(res[[i]], pre_urls)){
    print(paste("Found new pages", res[[i]]))
    write(res[[i]],file=output_filename,append=TRUE)
    scraper_webpage(res[[i]])
    #try(scraper_webpage(res[[i]]))
  }
}




