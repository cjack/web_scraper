# Author: Yi Lin
# 2015.11.18


#-------------- Pre-definitions ----------------
library(rvest)
require(RCurl)


csvname = "places.csv"
hostname <- "http://www.sydney.com"
output_filename = "output_places.txt"
num_of_info_per_page <- 20
category_names <- c("attractions-natural-attractions", "attractions-historical-sites-and-heritage-locations",
                    "attractions-galleries-museums-and-collections", "attractions-sports-and-recreation",
                    "attractions-entertainment", "attractions-science-and-technology")
category_tags <- c("Natural Attractions", "Historical Sites and Heritage Locations", 
                   "Galleries, Museums and Collections", "Sports and Recreation", 
                   "Entertainment", "Science and Technology")

##-----------------------------

if(!file.exists(output_filename)){
  file.create(output_filename)
  print("creating new output file")
}

pa_token <- "/destinations/"
pa_prefix <- "href=\"/destinations/"
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
  location = gsub("                ", " ", location)
  
  phone_token = "span.tel"
  phone <- search_result(test, phone_token)
  
  
  
  about_token = "div.about-block p"
  about <- search_result(test, about_token)
  
  summary <- about[length(about)]
  
  
  
  cast <- html_nodes(test, "div.side-box a")
  link <- html_attr(cast, "title")

  
  
  #---------------------------------------------------
  # Add three empty cols for "Kids Related", "Comment", "Tags"
  
  
  res <- c(title, location, summary, phone, link)
  

  
  Table = matrix(res, nrow = 1, ncol = length(res))
  
  
  
  write.table(Table, file = csvname,sep = ",", append = T, row.names = F, col.names = F)
}

# generate the searching pages, return a list

main_page_generator <- function(category_names = category_names, numPerPage = num_of_info_per_page, MAX = 100){
  
  url_list <- list()
  for(i in 1: length(category_names)){
    
    category <- category_names[i]
    num <- 1
    MAX <- 100
    #only for debug
    #checked <- TRUE
    checked <- FALSE
    while(num < MAX){
      url = paste("http://www.sydney.com/things-to-do/", 
                  category,"?&start_rank=", num, "&", sep = "")
      
      url_list <- c(url_list, url)
      num <- num + numPerPage
      if(checked == FALSE){
        #update the MAX
        match_token = "p.matches"
        MAX <- search_result(read_html(url), match_token)
        MAX <- as.numeric(substr(MAX, 1, nchar(MAX) - nchar(" matches")))
        checked = TRUE
        print(paste(toString(MAX), "matches in search"))
      }
    }
  }
  return(url_list)
  
}

main_process <- function(page_list){
  count <- 0 
  for(i in 1 : length(page_list)){
    page <- page_list[[i]]
    content <- readLines(page, warn = F)
    res <- unique(pattern_match(content, pattern, (nchar(pa_prefix) - nchar(pa_token)), nchar(pa_sufix)))
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
        count <- count + 1
        print(paste("Found new pages", res[[i]]))
        write(res[[i]],file=output_filename,append=TRUE)
        try(scraper_webpage(res[[i]]), TRUE)
      }
    }
    
  }
  print(paste("Total new pages added:", toString(count)))
}
page_list  <- main_page_generator(category_names = category_names, 20, 100)
main_process(page_list)
print("All done!")
