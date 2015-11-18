# Author: Yi Lin
# 2015.11.18


#-------------- Pre-definitions ----------------
library(rvest)
require(RCurl)


csvname = "sydney.csv"
hostname <- "http://www.sydney.com"
output_filename = "output.txt"

##-----------------------------

if(!file.exists(output_filename)){
  file.create(output_filename)
  print("creating new output file")
}

pa_prefix <- "href=\"/events/"
pa_sufix <- "\">"
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
##-----------------------------

search_result <- function(url, token){
  res <- url %>%
    html_nodes(token) %>%
    html_text()
  return(res)
}


scraper_webpage <- function(url){
  library(rvest)
  test <- read_html(url)
  

  #-----------      Token Matching    ------------------
  title_token = "title "
  title = search_result(test, title_token)
  
  location_token = "div.adr"
  location = search_result(test, location_token)
  location = gsub('\n', "", location)
  location = gsub('\t', "", location)
  
  phone_token = "span.tel"
  phone <- search_result(test, phone_token)
  
  about_token = "div.about-block p"
  about <- search_result(test, about_token)
  summary <- about[2]
  
  dates <- about[1]
  dates
  
  cast <- html_nodes(test, "div.side-box a")
  link <- html_attr(cast, "title")
  
  match_token = "p.matches"
  MAX <- search_result(test, match_token)
  MAX <- as.numeric(substr(MAX, 1, nchar(MAX) - nchar(" matches")))

  
  #---------------------------------------------------
  
  res <- c(title, dates, location, summary, phone, link)
  Table = matrix(res, nrow = 1, ncol = length(res))
  length(res)
  Table
  #colnames(Table) <- c("Title", "Time", "Location", "Content", "Cost", "MoreInfo","Tags")
  write.table(Table, file = csvname,sep = ",", append = T, row.names = F, col.names = F)
}

# generate the searching pages, return a list

main_page_generator <- function(date = "12-12-2016", numPerPage = 20, MAX = 100){
  num <- 1
  url_list <- list()
  checked <- FALSE
  while(num < MAX){
    url = paste("http://www.sydney.com/events/search?&start_rank=", num,"&query=&meta_D_orsand=&date_from=&date_to=", date, "&meta_k_phrase_orsand=", sep = "")
    url_list <- c(url_list, url)
    num <- num + numPerPage
    if(checked == FALSE){
      #update the MAX
      match_token = "p.matches"
      MAX <- search_result(read_html(url), match_token)
      MAX <- as.numeric(substr(MAX, 1, nchar(MAX) - nchar(" matches")))
      checked = TRUE
    }
  }
  return(url_list)
}

main_process <- function(page_list){
  for(i in 1 : length(page_list)){
    page <- page_list[[i]]
    content <- readLines(page, warn = F)
    res <- unique(pattern_match(content, pattern, (nchar(pa_prefix) - 8), nchar(pa_sufix)))
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
      }
  }
  
  }
}
page_list  <- main_page_generator()
main_process(page_list)
