# Author: Yi Lin
# 2015.11.26
# This program is to catch the events in http://www.mycommunitylife.com.au/Events-Activities

#-------------- Pre-definitions ----------------
library(rvest)
library("RSelenium")

csvname = "kingston.csv"
output_filename = "output_kingston.txt"

##-----------------------------

if(!file.exists(output_filename)){
  file.create(output_filename)
  print("creating new output file")
}

check_existence <- function(line, lines){
  for(i in 1:length(lines)){
    if(identical(line, lines[i])){
      return(TRUE)
    }
  }
  return(FALSE)
}

generate_links <- function(url){
  
  #----------------------------  Start the server     --------------------------
  
  library(RSelenium)
  checkForServer(dir = NULL, update = FALSE)
  RSelenium::startServer(dir = NULL, args = NULL, invisible = TRUE, log = TRUE)
  remDr <- remoteDriver()
  remDr$open()
  
  
  
  #----------------------------       Process         --------------------------
  
  
  
  remDr$navigate(url)
  #<a href="#page-3" class="page-link">3</a>
  #manual set the max pages
  
  
  #pagination
  MAX <- 11
  res_links <- list()
  for(page_num in 1:MAX){
    Sys.sleep(1)
    if(page_num > 1){
      page <- paste("#page-", toString(page_num), sep = "")
      Xpath_pattern <- paste("//*/a[@href = '", page, "']", sep = "")
      Xpath_pattern
      webElem <- remDr$findElement(using = "xpath", Xpath_pattern)
      # webElem <- remDr$findElement(using = "xpath", "//*/a[@href = '#page-1']")
      webElem$sendKeysToElement(list("\uE007"))
    }
    webElems <- remDr$findElements(using = "css selector", "div article a")
    
    #get links from search
    resLinks <- unlist(lapply(webElems, function(x){x$getElementAttribute("href")}))
    res_links <- c(res_links, resLinks)
  }
  remDr$closeServer()
  return(res_links)
}



process_each_page <- function(link){
  #print(link)
  raw_html = read_html(link)
  
  remove_comma <- function(str){
    return(gsub("\"", "'", str))
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
    #print(res)
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
  title_token = "title "
  title = search_result(raw_html, title_token)
  
  dates_token <- "div.content-box.event-date-main span"
  dates <- search_result(raw_html, dates_token)
  
  
  
  contact_name_token <- "li.contact-name"
  contact_name <- search_result(raw_html, contact_name_token)
  contact_name = gsub("                ", "", contact_name)
  
  phone_token <- "li.phone"
  phone <- search_result(raw_html, phone_token)
  
  email_token <- "li.email"
  email <- search_result(raw_html, email_token)
  
  
  location_token = "div.address p"
  location = search_result(raw_html, location_token)
  location <- sub("View Map", "", location)
  
  
  
  summary_token = "div.col-xs-12.body-content"
  summary <- search_result(raw_html, summary_token)
  summary <- remove_comma(summary)
  
  
  
  
  
  res <- c(link, title, dates, location, summary, contact_name, phone, email)
  
  
  
  
  
  #check if exist csv file, if not, add the col names
  if(!file.exists(csvname)){
    file.create(csvname)
    col_name = matrix(res, nrow = 1, ncol = length(res))
    colnames(col_name) <- c("URL", "Title", "Dates", "Location", "Content", "Contact_name", "Phone", "Email")
    write.table(col_name, file = csvname,sep = ",", append = T, row.names = F, col.names = T)
    print("creating new csv file")
  }else{
    Table = matrix(res, nrow = 1, ncol = length(res))
    if(summary != "" && location != "")
      write.table(Table, file = csvname,sep = ",", append = T, row.names = F, col.names = F)
  }
  
}

main_process <- function(url){
  links <- generate_links(url)
  pre_urls <- readLines(output_filename)
  for(i in 1:length(links)){
    current_link <- links[[i]]
    if(!check_existence(current_link, pre_urls)){
      print(paste("Found new pages", current_link))
      write(current_link,file=output_filename,append=TRUE)
      try(scraper_webpage(current_link), TRUE)
    }
    process_each_page(current_link)
  }
}

url = "http://www.mycommunitylife.com.au/Events-Activities"

main_process(url)




