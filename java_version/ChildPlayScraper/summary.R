# for checking the csv file and generate the summary

filename = "childplay.csv"
r = read.csv(filename)

# -----------------------------------------------------------------
raw = subset(r, select = "Brand")
raw = as.matrix(table(raw), ncol = 1)


# -----------------------------------------------------------------
price = subset(r, select = "Price", drop = F)


priceToInt <- function(p){
  p = toString(p)
  return(as.numeric(substr(p, 2, nchar(p))))
}
price = lapply(price[,1], priceToInt)

# check the validation of the prices
for(i in 1:length(price)){
  p = price[i]
  if(p > 1000 || p < 0){
    warning(paste("some prices are invalid", p), immediate. = T)
  }
}

main <- function(){
  print (paste("The total number of the items is ", nrow(r)))
  print ("The counters for the brand")
  print(raw)
  pp = sapply(price, max)
  print (paste("The maximal price is", max(pp)))
  print (paste("The average price is", mean(pp))) 
  print (paste("The minimal x price is", min(pp))) 
}

main()
# -----------------------------------------------------------------