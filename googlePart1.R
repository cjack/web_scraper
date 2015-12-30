############################################################

##  check packages 

if(!require("googlesheets")) install.packages("googlesheets", repos="http://cran.rstudio.com/")
if(!require("data.table")) install.packages("data.table", repos="http://cran.rstudio.com/")
library("googlesheets")
library(data.table)
suppressPackageStartupMessages(library("dplyr"))

############################################################





######################################################################################
## This function's purpose is to read the csv files and then add three column names ##
## and the names are "Kids.Related", "Comment", "Tags"                              ##
######################################################################################

addThreeCols <- function(ws){
    ## ws <- sheet %>% gs_read(ws = wsName)
    cn <- colnames(ws)
    nn <-  c("Kids.Related", "Comment", "Tags")
    ws["Kids.Related"] <- ""
    ws["Comment"] <- ""
    ws["Tags"] <- ""

    return(setcolorder(ws, c(nn, cn)))
    
    ##sheet %>% gs_edit_cells(ws = wsName, input = ws)
}
#addThreeCols(gap, "new")

#####################################################################################



checkSheet <- function(sheetName){
    message("Checking sheet File")
    tryCatch(
    {
        gs_title(sheetName)
    }, error = function(cond){
        message(paste("sheet file: ", sheetName, " has not found"))
        message("Creating a new googlesheets")
        gs_new(sheetName)
        message("Done")
    }, warning = function(cond){
        message("There's a warning")
        message(cond)
    }, finally = {
        message(paste("Checking completed: ", sheetName))
    }
    )
}

checkWorksheet <- function(wsName, sheet){
    message("Checking Worksheet")
    ## sheet <- gs_title(sheetName)
    tryCatch(
        {
            sheet %>% gs_read(ws = wsName)
        }, error = function(cond){
            message(paste("Worksheet:", wsName, "has not found"))
            message("Creating a new worksheet")
            sheet <- sheet %>% gs_ws_new(wsName)
            message("Done")
        }, warning = function(cond){
            message("There's a warning")
            message(cond)
        }, finally = {
            message(paste("Checking completed:", wsName))
        }
    )
}

"
upload one csv to a specific googlesheet
1. read the csv file
2. open the googlesheet
3. add to the correponding worksheet
"

uploadOneCSV <- function(filename, sheetName, sheet){
    ## check the filenae, which should be ended with csv
    
    ## read local csv file
    ws <- read.csv(filename)

    ## add 3 cols
    ws <- addThreeCols(ws)


    ## get the filename as the worksheet's name
    wsName <- sub(".csv", "", filename)

    ## check the worksheet
    checkWorksheet(wsName, sheet)
    #sheet <- sheet %>% gs_ws_new(wsName)

    ## update the worksheet
    sheet <- sheet %>% gs_edit_cells(ws = wsName, input = ws)
}


## get the csv file from current directory

getFileNames <- function(dir = ".", suffix = ".csv"){
    return(dir(dir, suffix))
}



test <- function(){
    
    filename <- "kingston.csv"
    sheetName <- "part1"
    sheet <- gs_title(sheetName)
    ##filename <- sub(".csv", "", filename)
    ##filename

    ##typeof(dir(".", "*.csv"))

    uploadOneCSV(filename, sheetName, sheet)
}

googlePart1 <- function(){
    sheetName <- "part1"
        ## check and open the google spreadsheet
    checkSheet(sheetName)
    sheet <- gs_title(sheetName)
    fileNames <- getFileNames()
    print(fileNames)
    lapply(fileNames, function(x){
        try(uploadOneCSV(x, sheetName, sheet))
    } 
    )
}

## test the function

## test()

googlePart1()