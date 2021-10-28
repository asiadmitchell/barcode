check_for_duplicates <- function(scanned_file) {
  master <- readRDS('Data/MasterList.rds')
  #Select all barcodes used
  master_sub= master %>% select(Label, Primary_ID, Secondary_ID, Date) %>% filter(!is.na(Date))
  #print(master_sub)
  
  #Make a master tmp dataframe to check for duplicates, join used barcodes and scanned into single dataframe
  master_tmp=rbind(master_sub,scanned_file)
  master_tmp= master_tmp %>% select(Label, Primary_ID, Secondary_ID, Date) %>% filter(!is.na(Date))
  #Checking for primary ID duplictes
  if (sum(duplicated(master_tmp$Primary_ID)) > 0){
    print("Found duplicates in Primary_ID")
    print(sum(duplicated(master_tmp$Primary_ID)))
    #Identify duplicates and store Primary_ID of duplicates
    primary_dup=master_tmp[duplicated(master_tmp[,c("Primary_ID")]),]
    
    #Return table with duplicates
    for ( i in 1:length(primary_dup$Primary_ID)){
      if (i==1){
        duplicated_table=master_tmp[master_tmp$Primary_ID==primary_dup$Primary_ID[i],]
      } else{
        duplicated_table=rbind(dupicated_table,master_tmp[master_tmp$Primary_ID==primary_dup$Primary_ID[i],])
      }
      return(duplicated_table)
    } # If no duplicates make new master file with addition of printed Label and Date
  } else{
    print("No Primar_ID duplicates found")
    master_new=merge(master,scanned_file,by=c("Primary_ID","Secondary_ID"),all=T)
    master_new=transform(master_new, Label = ifelse(is.na(Label.x),Label.y, Label.x))
    master_new=transform(master_new, Date = ifelse(is.na(Date.x),Date.y, Date.x))
    master=master_new[,c("Primary_ID","Secondary_ID","Label","Date")]
    master=master[order(master$Label),]
    return(master)
    #    saveRDS(master,file='MasterList.rds')
  }
}

