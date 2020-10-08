##Read the CSV files
##OPTION 1: Default read
data <- read.csv('finalsrstfunc.csv')
##OPTION 2: Open from saved workspace (function_srst_final.RData)

##Removing columns w/ same values (not counting NAs as a value/level)
data <- data[sapply(data, function(x) length(levels(x))>1)]

##Find the index of pfcode column & create a column of pfcode
PFCODEPOS <- which(colnames(data)=='pfcode')
pfcodeColumn = rank(data[,PFCODEPOS], na.last = 'keep')

##Create a binary column for pfcode 
##(i.e. not 0000 -> 1, else 0)
pfcodeBinaryColumn <- as.numeric()
for(i in 1:nrow(data)){
  if(data[i,PFCODEPOS] != '0000') pfcodeBinaryColumn[i] <- 1
  else pfcodeBinaryColumn[i] <- 0
}

##Create a data frame & specifying their data types
results <- data.frame(column_name=character(), 
                 spearman_normal=numeric(), 
                 spearman_binary=numeric(), 
                 cramer_normal=numeric(), 
                 cramer_binary=numeric(),
                 stringsAsFactors = FALSE)

##Chi-square + Cramer's V function
func_cramers_v <- function(x,y) {
  #Remove pairs containing NAs (pfcode/'x' doesn't have any NAs so remove based on 'y' instead)
  x = x[!is.na(y)]
  y = y[!is.na(y)]
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  return(as.numeric(CV))
}

##Comparing pfcode (first column i=1) w/ the remaining columns
##using Spearman correlation (comparing rank/"strings turned into numbers")
##and Chi-square & Cramer's V.
pb <- txtProgressBar(PFCODEPOS+1, ncol(data), style = 3)
for(i in PFCODEPOS+1:ncol(data)){
  setTxtProgressBar(pb, i)
  currentColumn = rank(data[,i], na.last = 'keep')
  #column_name,spearman_normal,spearman_binary,cramer_normal,cramer_binary
  results[i-PFCODEPOS,1] <- names(data[i])
  #use in cor:  -everything = NA if any NAs exist in either column
  #             -complete.obs = remove any *rows* if an NA is present
  #                           = only complete pairs of 2 columns will be calculated
  results[i-PFCODEPOS,2] <- cor(pfcodeColumn, currentColumn,
                           use = "complete.obs", method = "spearman")
  results[i-PFCODEPOS,3] <- cor(pfcodeBinaryColumn, currentColumn,
                           use = "complete.obs", method = "spearman")
  results[i-PFCODEPOS,4] <- func_cramers_v(pfcodeColumn, currentColumn)
  results[i-PFCODEPOS,5] <- func_cramers_v(pfcodeBinaryColumn, currentColumn)
}
close(pb)

##Sort results by normal correlation value in descending order (do this in excel instead?)
##results <- results[order(-results$spearman_normal, -results$cramer_normal),]

##Write data frame to new CSV file
write.csv(results,'Correlation_results.csv', row.names = FALSE, quote = FALSE)



##########################################################################################
#Test area for understanding code, data & stuff
sapply(data, function(x) sum(is.na(x)))
sum(duplicated(colnames(data)))
options(max.print=100000)
#rank the column, keeping NA values
ex <- rank(data$ib01_hsidiskw_6800, na.last = 'keep')
ex <- ex[!is.na(ex)] #remove NAs in numeric()
ex <- ex[-which(is.na(ex))] #other than numeric()
anyNA(ex) #
