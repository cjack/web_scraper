"This program is to read spreadsheet from part2
modify and generate the part3, which is similar with the process
of immigrating part1 to part2
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


input <- "Part2"
output <- "Part3"
input <- paste(input, "_", today, sep = "")
output <- paste(output, "_", today, sep = "")


##################################################################################
## colList <- c("ID","Tags","URL","Title","Date","startDateTime","endDateTime", ##
##              "Date Time Summary","Venue Name","Address","Price",             ##
##              "Organizer","Contact Summary","Contact Name","Phone","Email",   ##
##              "Web","Booking","Details","Images","Comment")                   ##
##################################################################################

## required column names list


colList <- c("Added Date", "Scrapped Date", "ID", "URL", "Tags", "Name", "Date", "startDateTime", "endDateTime", "Date Time.Summary", "Venue name", "Address", "Modified Venue.Address", "lat", "lon", "Price", "Organizer", "Contact Summary", "Contact name", "Phone", "Email", "Web", "Booking", "Details", "Images", "Comments", "Source", "Editor Rating", "Editor Title", "SuburbCountryOnly")



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

## check if the worksheet is existed or not
## if not then create it

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
   
    

    ## check and add the miss column
    for(i in 1:length(colList)){
        cl <- colList[i]
        ##print(cl)
       ## print(checkColExistence(cl, ws))
        if(!checkColExistence(cl, ws)){
            ws[,cl] <- ""
        }
    }
    ## remove the invaild row
    ws <- subset(ws, Name != "")

    ws <- checkInList(ws, colList)
    
    ## remove the NA 
    ws[is.na(ws)] <- ""

    print(colnames(ws))
    
    require(data.table)
    x <- data.table(ws)
    setcolorder(x, colList)

    colNames <- colnames(x)

    ## change the colname from Address to Venue address
    if("Address" %in% colNames){
        index <- which(colnames(x) %in% "Address")
        ##index <- getColnameIndex(a, "Title")
        colnames(x)[index] <- "Venue address"
    }
  

    checkWorksheet(wsName, otherSheet)
    other <- gs_title(otherSheet)

    ##create another tab in other file
    ##other <- other %>% gs_ws_new(wsName)
    
    #other <- gs_title(otherSheet)
    other %>% gs_edit_cells(ws=wsName, input=x)
}

##processOneWorkSheet("Cardinia", input, output, colList)

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


##################################################
##   gap <- gs_title("example")                 ##
##   ws <- gap %>% gs_read(ws = "new")          ##
##                                              ##
## ws                                           ##
## gap <- gap %>% gs_edit_cells(ws = "new", ws) ##
##################################################









