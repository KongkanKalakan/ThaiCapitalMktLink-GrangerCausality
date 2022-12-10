# VLGranger Utils

install.packages('VLTimeCausality')
install.packages('readxl')
install.packages('tidyverse')

library(VLTimeCausality)
library(readxl)
library(dplyr)

readFile <- function(PATH)
{
  data <- read_excel(PATH)
  data$Date <- as.Date(data$Date)
  # Removing NA
  data <- na.omit(data) 
  data
}

runGranger <- function(data, from = data$Date[1], to = data$Date[nrow(data)], 
                       dayLag = 11, variableLag = TRUE)
{
  # Slicing data from specific date 
  Seq <- seq.Date(as.Date(from),as.Date(to),by="day")
  sliced_data <- data[data$Date %in% Seq,]
  data_WO_date <- subset(sliced_data, select = -c(Date) )
  
  attach(data_WO_date)
  cols <- colnames(data_WO_date)
  allComb <- apply(combn(cols,2),2,paste,collapse='_')
  result <- data.frame(matrix(ncol = 9, nrow = 0))
  cols <- c('Y','X','ftest','pval','XgCsy','XgCsy_ftest','XgCsy_BIC',
            'maxLag','BICDiffRatio')
  colnames(result) <- cols
  
  for (comb in allComb)
  { 
    for (i in (strsplit(comb,split = '_')))
  { 
      TS1 <- i[1] ; TS2 <- i[2] 
      
      if (variableLag == FALSE)
       {
        model <- GrangerFunc(unlist(data[,TS1]), unlist(data[,TS2]),
                               maxLag = dayLag, autoLagflag = FALSE)
      
        model2 <- GrangerFunc(unlist(data[,TS2]), unlist(data[,TS1]), 
                                maxLag = dayLag, autoLagflag = FALSE)
      }
      else if (variableLag == TRUE)
      {
        model <- VLGrangerFunc(unlist(data[,TS1]), unlist(data[,TS2]),
                               maxLag = 11, autoLagflag = FALSE)
        
        model2 <- VLGrangerFunc(unlist(data[,TS2]), unlist(data[,TS1]), 
                                maxLag = 11, autoLagflag = FALSE)
      }
      
      res <- c(TS1,TS2,model$ftest,model$p.val,model$XgCsY,model$XgCsY_ftest,
               model$XgCsY_BIC,model$maxLag,model$BICDiffRatio)
      result[nrow(result)+1,] <- res 
  
      res2 <- c(TS2,TS1,model2$ftest,model2$p.val,model2$XgCsY,model2$XgCsY_ftest,
                model2$XgCsY_BIC,model2$maxLag,model2$BICDiffRatio)
      result[nrow(result)+1,] <- res2 }
  }
  result
}

# Example
PATH = "Desktop/CMDF/InputVariable(11Jan22).xlsx"
data <- readFile(PATH)

Granger_ex <- runGranger(data = data,from = '2011-01-06', to = '2015-12-30', 
                         dayLag = 6, variableLag = FALSE)
VLGranger_ex <- runGranger(data = data,from = '2011-01-06', to = '2015-12-30', 
                          dayLag = 11, variableLag = TRUE)


