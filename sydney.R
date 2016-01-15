    # Author: Yi Lin
    # 2015.11.18
    
    
    #-------------- Pre-definitions ----------------
    library(rvest)
    require(RCurl)
    
    
    csvname = "sydney.csv"
    hostname <- "http://www.sydney.com"
    output_filename = "output_sydney.txt"
    num_of_info_per_page <- 8
    
    ##-----------------------------
    
    if(!file.exists(output_filename)){
      file.create(output_filename)
      print("creating new output file")
    }
    
    
    pa_prefix <- "href=\"/events/"
    pa_sufix <- "\""
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
    
    remove_comma <- function(str){
        return(gsub("\"", "'", str))
    }
    remove_space <- function(str){
        while(str != gsub("  ", " ", str)){
            str <- gsub("  ", " ", str)
        }
        return(str)
    }
    remove_enter <- function(str){
        str = gsub("\n", "", str)
        str = gsub("\r\n", "", str)
        return(str)
    }
    normalzation <- function(str){
        str = remove_comma(str)
        str = remove_space(str)
        str = remove_enter(str)
        return(str)
    }
    search_result <- function(url, token){
        res <- url %>%
            html_nodes(token) %>%
            html_text()
        if((length(res) == 0) && (typeof(res) == "character"))
            res <- ""
        res <- toString(res)
        res <- normalzation(res)
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
    
    scraper_webpage <- function(url){
      library(rvest)
      
      test <- read_html(url)
      
      
      #-----------      Token Matching    ------------------
      title_token = "h1.main-heading"
      title = search_result(test, title_token)
      
      location_token = "div.adr"
      location = search_result(test, location_token)
      location = gsub('\n', "", location)
      location = gsub("                ", " ", location)
      location = gsub(" Address:  ", "", location)
      
      phonef_token = "span.tel span"
      phonef <- search_result(test, phonef_token)
      phone_token = "span.tel"
      phone <- search_result(test, phone_token)
      phone <- gsub(phonef, "", phone)
      
      dates_token = "h2.event-date-details"
      dates <- search_result(test, dates_token)
      
      
      about_token = "section.about-block p"
      about <- search_result(test, about_token)
      
      
      #search_attr(test, "meta.description", content)
      summary <- about[length(about)]

      email_token = "a.ga_URL_lead_email"
      email = search_result(test, email_token)
      email <- gsub("Send Email, Email: ", "", email)
      
      ppf <- "Website: </strong>"  ##
      psf <- "</a>"                                                ##
      pattern <- paste(ppf, ".*?", psf, sep = "")                    ##
      rawData <- readLines(url)                                      ##
      res <- pattern_match(rawData, pattern, nchar(ppf), nchar(psf)) ##
      res <- gsub("<br/>", "\n", res)                                ##
      res <- gsub("</p>", "\n", res) 
      website <- res
      
      cast <- "div.side-box a"
      link <- search_attr(test, cast, "title")
      match_token = "p.matches"
      MAX <- search_result(test, match_token)
      MAX <- as.numeric(substr(MAX, 1, nchar(MAX) - nchar(" matches")))
      
      
      #---------------------------------------------------
      # Add three empty cols for "Kids Related", "Comment", "Tags"
      
      res <- c(url, title, dates, location, summary, phone, email, website)
      
      #check if exist csv file, if not, add the col names
      if(!file.exists(csvname)){
          file.create(csvname)
          col_name = matrix(res, nrow = 1, ncol = length(res))
          colnames(col_name) <- c("URL","Name", "Date", "Address", "Details", "Phone", "Email", "Website")
          write.table(col_name, file = csvname,sep = ",", append = T, row.names = F, col.names = T)
          print("creating new csv file")
      }else{
          Table = matrix(res, nrow = 1, ncol = length(res))
        print(summary)
        
        
        if(summary != "")
            write.table(Table, file = csvname,sep = ",", append = T, row.names = F, col.names = F)
      }
     
    }
    
    # generate the searching pages, return a list
    
    
    #http://www.sydney.com/events/search?&start_rank=1&query=&meta_D_orsand=&date_from=&date_to=12-12-2016&meta_k_phrase_orsand=
    main_page_generator <- function(date = "12-12-2016", numPerPage = num_of_info_per_page, MAX = 100){
      num <- 1
      url_list <- list()
      #only for debug
      #checked <- TRUE
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
          print(paste(toString(MAX), "matches in search"))
        }
      }
      return(url_list)
    }
    
    main_process <- function(page_list){
      count <- 0 
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
            count <- count + 1
            print(paste("Found new pages", res[[i]]))
            write(res[[i]],file=output_filename,append=TRUE)
            try(scraper_webpage(res[[i]]), TRUE)
          }
        }
        
      }
      print(paste("Total new pages added:", toString(count)))
    }
    page_list  <- main_page_generator()
    main_process(page_list)
    print("All done!")
