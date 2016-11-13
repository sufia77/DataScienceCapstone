setwd("c:/capstone7")
#install JRE
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\re1.8.0_102') # for 64-bit version
#install.packages("qdap")
#install.packages("stringr")
#install.packages("iterators")
library(rJava)                 # Need JRE to be installed on the system
library(qdap)                  # required for colsplit2d() function
library(tidyr)                 # required for unite() function
library(stringr)
#===============================================
#-----------------------------------------------
makeNGTable<- function(ngram, n, cols){
  ###############################
  # ngram<- "five"; n<- 5; cols<- c("X1","X2","X3","X4","lastTerm")
  ###############################
  
  nGName<- paste0(ngram, "G.RDS")
  nGTableName<- paste0(ngram, "GTable.RDS")
  
  nG<- readRDS(nGName)
  tempdf <- colsplit2df(nG, splitcols = 1, cols, sep = " ")
  switch(as.character(n),
         "2" = { nGTable <- tempdf %>% unite(firstTerm, X1, sep = " ", remove = TRUE) },
         "3" = { nGTable <-  tempdf %>% unite(firstTerm, X1,X2, sep = " ", remove = TRUE) },
         "4" = { nGTable <-  tempdf %>% unite(firstTerm, X1,X2,X3, sep = " ", remove = TRUE) },
         "5" = { nGTable <-  tempdf %>% unite(firstTerm, X1,X2,X3,X4, sep = " ", remove = TRUE) },
               {print( "'n' out of range. Variable 'nGTable' is NULL." )  }
  )
  
  saveRDS(nGTable, file = nGTableName)
  
  if(file.exists(nGTableName)){
    print(paste(nGTableName,"has been created"))
  }else{
    print(paste("file", nGTableName,"might not have been created. Please check before proceeed."))
  }

}
#-----------------------------------------------
tempcols<- c("X1","X2","X3","X4","lastTerm")
makeNGTable("five", 5, tempcols)

tempcols<- c("X1","X2","X3","lastTerm")
makeNGTable("four", 4, tempcols)

tempcols<- c("X1","X2","lastTerm")
makeNGTable("three", 3, tempcols)

tempcols<- c("X1","lastTerm")
makeNGTable("two", 2, tempcols)
#-----------------------------------------------

#===============================================
makeNGTableFinal<- function(ngram, n){
  ###############################
  # ngram<- "five"; n<- 5
  ###############################
  nGTableName<- paste0(ngram, "GTable.RDS")
  uniqueNGTableName<- paste0("unique", as.character(n), "GTable.RDS")
  finalNGTableName<- paste0("final", as.character(n), "GTable.RDS")
  
  nGTable<- readRDS(nGTableName)
  
  print(paste0("Finalizing ", ngram, " gram table ..."))
  
  #totrow<- 10000
  totrow<- nrow(nGTable)
  print(paste(Sys.time(), ": Process started."))
  print(paste("Total lines to be processed:", totrow))
  
  attach(nGTable)
  sortedNGTable<- nGTable[order(firstTerm, -freq), ]
  detach(nGTable)
  
  uniqueNGTable <- sortedNGTable[0,]
  tempNGTable <- uniqueNGTable
  tempNGTable2 <- uniqueNGTable
  
  tempSingleRows <- data.frame(firstTerm=character(),lastTerm=character())
  finalNGTable <- tempSingleRows   
  
  searchTerm<- ""
  rowfound<- 0
  srCount<- 0
  #--------------------------------------------------
  library(iterators)    
  sortedNGList<- iter(sortedNGTable, by = "row")
  #--------------------------------------------------
  
  for(i in 1:totrow){
    #i<-0
    #i<- i+1
    t<- nextElem(sortedNGList)
    if (searchTerm != t$firstTerm){
      #----------------------------------------------
      if (rowfound > 0){
        x<- tempNGTable2$lastTerm
        x<- str_c(x,collapse=" ")
        x<- data.frame(firstTerm=searchTerm, lastTerm=x)
        tempSingleRows<- rbind(tempSingleRows, x)
        srCount<- srCount+1
        if(srCount==500){
          finalNGTable<- rbind(finalNGTable,tempSingleRows)
          tempSingleRows<- tempSingleRows[0,]
          srCount<- 0
        }
        tempNGTable2<- tempNGTable2[0,]
      }
      #----------------------------------------------
      searchTerm<- (t$firstTerm)
      rowfound<- 1
    }else{
      rowfound<- rowfound+1
    }
    
    if (rowfound < 4){
      tempNGTable<- rbind(tempNGTable, t)
      tempNGTable2<- rbind(tempNGTable2, t)
    }
    
    if(i%%5000==0 | i==totrow){
      uniqueNGTable<- rbind(uniqueNGTable, tempNGTable)
      tempNGTable <- tempNGTable[0,]
    }
    
    if(i%%1000==0 | i==totrow){ print(paste0(Sys.time(), ":: ",nGTableName,":: ", i, " of ", totrow, " lines processed")) }
  }
  #----------------------------------------------
  if (nrow(tempNGTable2) > 0){
    x<- tempNGTable2$lastTerm
    x<- str_c(x,collapse=" ")
    x<- data.frame(firstTerm=searchTerm, lastTerm=x)
    tempSingleRows<- rbind(tempSingleRows, x)
    
    finalNGTable<- rbind(finalNGTable,tempSingleRows)
    tempSingleRows<- tempSingleRows[0,]
    
    tempNGTable2<- tempNGTable2[0,]
  }
  #----------------------------------------------
  print(paste0(Sys.time(), nGTableName, ":: Process completed."))
  print(paste0(Sys.time(), nGTableName, ":: Saving two tables:."))
  print(paste0("  ", uniqueNGTableName))
  print(paste0("  ", finalNGTableName))
  
  saveRDS(uniqueNGTable, file=uniqueNGTableName)
  saveRDS(finalNGTable, file=finalNGTableName)
  
}
#=============================================================
#-------------------------------------------------------------
makeNGTableFinal("five", 5)
makeNGTableFinal("four", 4)
makeNGTableFinal("three", 3)
makeNGTableFinal("two", 2)
#-------------------------------------------------------------
#=============================================================
##############################################################
####################### End of Project #######################



    
