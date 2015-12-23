"
This program is to read spreadsheet from part1
modify it and generate anew to part2
1. read the spreadsheet from part1
2. check if there is any column not in the current sheet
3. if so, add the blank column with that name into the sheet
4. reset the column order
5. write to the output file
"
############################################################

##  check packages 


if(!require("googlesheets")) install.packages("googlesheets", repos="http://cran.rstudio.com/")
if(!require("data.table")) install.packages("data.table", repos="http://cran.rstudio.com/")
############################################################

library("googlesheets")
library(data.table)
suppressPackageStartupMessages(library("dplyr"))

today <- Sys.Date()
today <- format(today, "%Y_%m_%d")


input <- "Part1"
output <- "Part2"
output <- paste(output, "_", today, sep = "")


colList <- c("ID","Tags","URL","Name","Date","startDateTime","endDateTime",
             "Date Time Summary","Venue name","Venue address","Price",
             "Organizer","Contact Summary","Contact name","Phone","Email",
             "Web","Booking","Details","Images","Comments")


############################################################

##########################
## Checking Output File ##
##########################

message("Checking Output File")
tryCatch(
{
    gs_title(output)
}, error = function(cond){
    message(paste("output file: ", output, " has not found"))
    message("Creating a new googlesheets")
    gs_new(output)
    message("Done")
}, warning = function(cond){
    message("There's a warning")
    message(cond)
}, finally = {
    message(paste("Checking completed: ", output))
}
)



checkWorksheet <- function(wsName, sheetName){
    message("Checking Worksheet")
    sheet <- gs_title(sheetName)
    tryCatch(
        {
            sheet %>% gs_read(ws = wsName)
        }, error = function(cond){
            message(paste("Worksheet:", wsName, "has not found"))
            message("Creating a new worksheet")
            sheet %>% gs_ws_new(wsName)
            message("Done")
        }, warning = function(cond){
            message("There's a warning")
            message(cond)
        }, finally = {
            message(paste("Checking completed:", wsName))
        }
    )
}
############################################################

## This function is to remove the unnecessary column in output
## bu comparing the input

removeUnnecessaryCol <- function(input, output){
    inSheet <-  gs_title(input)
    outSheet <- gs_title(output)
    inCol <- inSheet %>% gs_ws_ls()
    outCol <- outSheet %>% gs_ws_ls()
    for(i in 1:length(outCol)){
        sign <- 0
        for(j in 1:length(inCol)){
            if(identical(outCol[i], inCol[j])){
                sign <- 1;
                break
            }
        
        }
        if(identical(sign, 0)){
            outSheet %>% gs_ws_delete(ws = outCol[i])
        }

    }

}



## Generate the ID by the name of worksheet and the date
idFormat <- function(ws, wsName, today){
     for(i in 1:nrow(ws)){
        id <- paste(wsName, "_", today, "_", toString(i), sep = "")
        ws[i, "ID"] <- id
    }
    return(ws)
}

checkColExistence <- function(ID, worksheet){
    if(ID %in% colnames(worksheet)){
        return(TRUE)
    }else{
        return(FALSE)
    }
}


## check whether the current column is in the
## required list, if not, just remove it.

checkInList <- function(ws, colList){
    name <- colnames(ws)
    for(i in 1 : length(name)){
        if(!(name[i] %in% colList)){
            ws[,name[i]] <- NULL
        }
    }
    return(ws)
}




processOneWorkSheet <- function(wsName, sheetName, otherSheet, colList){
    sheet <- gs_title(sheetName)
    message(paste("Processing the worksheet: ", wsName))
    ws <- sheet %>% gs_read(ws = wsName)
    
    ws = subset(ws, Kids.Related == "y")
    
        #################################################################################
        ## colList <- c("ID","Tags","URL","Name","Date","startDateTime","endDateTime", ##
        ##          "Date Time Summary","Venue Name","Venue Address","Price",          ##
        ##          "Organizer","Contact Summary","Contact Name","Phone","Email",      ##
        ##          "Web","Booking","Details","Images","Comment")                      ##
        #################################################################################



    ## add ID column automatically
    ws <- idFormat(ws, wsName,today)
    colNames <- colnames(ws)
    ## check and change the col name
    if("Title" %in% colNames){
        index <- which(colnames(ws) %in% "Title")
        ##index <- getColnameIndex(a, "Title")
        colnames(ws)[index] <- "Name"
    }

    if("Comment" %in% colNames){
        index <- which(colnames(ws) %in% "Comment")
        ##index <- getColnameIndex(a, "Title")
        colnames(ws)[index] <- "Comments"
    }


    if("Dates" %in% colNames){
        index <- which(colnames(ws) %in% "Dates")
        ##index <- getColnameIndex(a, "Title")
        colnames(ws)[index] <- "Date"
    }

    if("Address" %in% colNames){
        index <- which(colnames(ws) %in% "Address")
        ##index <- getColnameIndex(a, "Title")
        colnames(ws)[index] <- "Venue address"
    }

    
##    print(colnames(ws))
    ##check if the colname not exist then create 
    for(i in 1:length(colList)){
        cl <- colList[i]
        ##print(cl)
       ## print(checkColExistence(cl, ws))
        if(!checkColExistence(cl, ws)){
            ws[,cl] <- ""
        }
    }

    
     ## check whether the current column is in the
    ## required list, if not, just remove it.
    ws <- checkInList(ws, colList)

    
    ## remove the NA 
    ws[is.na(ws)] <- ""
    require(data.table)
    x <- data.table(ws)
    setcolorder(x, colList)

    ##create another tab in other file

    checkWorksheet(wsName, otherSheet)
    other <- gs_title(otherSheet)
    
    ##other <- other %>% gs_ws_new(wsName)
    
    #other <- gs_title(otherSheet)
    other %>% gs_edit_cells(ws=wsName, input=x)
}

##processOneWorkSheet("new", "example", output, colList)

main <- function(input, output, colList){
    sheet <- gs_title(input) #get the sheet
    files = sheet %>% gs_ws_ls();
    print(files)
    for(i in 1:length(files)){
        processOneWorkSheet(files[i], input, output, colList)
    }
    removeUnnecessaryCol(input, output)
}


############################################################

main(input, output, colList)


##########################################################
## gap <- gs_title("example")                           ##
##                                                      ##
## ws <- gap %>% gs_read("new")                         ##
##                                                      ##
## a <- colnames(ws)                                    ##
##                                                      ##
##                                                      ##
##                                                      ##
##                                                      ##
## colnames(ws)                                         ##
## gap <- gap %>% gs_edit_cells(ws = "new", input = ws) ##
##########################################################
















