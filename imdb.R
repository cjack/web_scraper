# Author: Yi Lin
# 2015.12.02
# dig the IMDB Top 250
# by far only the title and rates, also has a column to mark whether has been 
# watched or not
# more details can be added: Directors, Stars, Summary, Tags

#--------------------      pre-definition    -----------------------------------
library(rvest)
require(RCurl)

host <- "http://www.imdb.com"
csvname = "imdb250.csv"
#-------------------------------------------------------------------------------

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
  #res <- remove_comma(toString(res))
  return(toString(res))
}

generate_links <- function(link){
  test <- read_html(link)
  link_token <- "td.titleColumn a"
  link <- search_attr(test, link_token, "href")
  links <- as.list(strsplit(link, ", ")[[1]])
  res <- list()
  for(i in 1:length(links)){
    res <- c(res, paste(host, links[i], sep = ""))
  }
  return(res)
}



process_each_page <- function(link){
  test <- read_html(link)
  title_token = "title"
  title = search_result(test, title_token)
  
  rates_token <- "div.star-box-details strong span"
  rates <- search_result(test, rates_token)
  
  watched <- "F"
  this_url <- paste(host, link, sep = "")
  res <- c(title, rates, watched, this_url)
  
  # write to file
  # check if exist csv file, if not, add the col names
  if(!file.exists(csvname)){
    file.create(csvname)
    col_name = matrix(res, nrow = 1, ncol = length(res))
    colnames(col_name) <- c( "Title", "Rates", "Watched", "Link")
    write.table(col_name, file = csvname,sep = ",", append = T, row.names = F, col.names = T)
    print("creating new csv file")
  }else{
    Table = matrix(res, nrow = 1, ncol = length(res))
    if(summary != "" && location != "")
      write.table(Table, file = csvname,sep = ",", append = T, row.names = F, col.names = F)
  }
}

main_process <- function(link){
  links <- generate_links(link)
  for(i in 1 : length(links)){
    try(process_each_page(links[[i]]))
  }
}

link <- "http://www.imdb.com/chart/top"

main_process(link)