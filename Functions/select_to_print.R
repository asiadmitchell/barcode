select_to_print <- function(n) {
  library(dplyr)
  master <- readRDS('Data/MasterList.rds')
  
  #Find last label number used
  c <- master %>% select(Label, Primary_ID, Secondary_ID, Date) %>% filter(!is.na(Label))
  c <- max(c$Label)+1
  
  #Select barcodes to print by filtering for ID's without a date & Secondary_ID len == 5
  selection <- master %>% select(Label, Primary_ID, Secondary_ID, Date) %>% filter(is.na(Date) & nchar(Secondary_ID) == 5)
  
  #Select n number of barcodes
  selection <- selection[1:n,]
  
  #Set label number for selected barcodes to print
  selection$Label <- seq(c,c+(n-1),1)
  
  #Set date for barcodes to print
  selection$Date <- format(Sys.Date(), "%Y%m%d")
  
  if (sum(duplicated(selection$Primary_ID)) > 0) { 
    print("Duplicates in barcode labels to print.")
    ### PLEASE ADD CODE TO PRINT OUT DUPLICATE BARCODES & FIND NEW UNIQUE BARCODES ###
  }
  else {
#    print("No duplicates in barcode labels to print.")
#    sprintf("Print Date: %s", format(Sys.Date(), "%Y%m%d"))
    return(selection)
  }
}

