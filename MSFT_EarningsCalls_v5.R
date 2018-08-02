#---------------------------
#---------------------------Load Libraries
libs <- c("docxtractr", "stringr","tidytext","tidyverse","glue","tm")
lapply(libs, require, character.only = T)

#---------------------------
#---------------------------Load Data
fileName <- "C:/Users/tesr/Documents/R/Data/MSFT/100_Transcript.docx"
fileText <- read_docx(fileName)

File_Name <- c(100,101,102,103,104,105,106,107,108,109,110,111,112)
quarter <- c("15Q4","16Q1","16Q2","16Q3","16Q4","17Q1",
             "17Q2","17Q3","17Q4","18Q1","18Q2","18Q3","18Q4")
QuarterIndex <- cbind(File_Name,quarter)
StockData <- read.csv("MSFT_Stock_Prices.csv")
StockData <- merge(StockData, QuarterIndex, by = "File_Name")

#---------------------------
#---------------------------Tokenize
tokens <- data_frame(text = fileText) %>% unnest_tokens(word,text)
start_row <- round(.3 * nrow(tokens),digits=0)  #the intro & end are noise
end_row <- round(.9 * nrow(tokens), digits=0)
tokens <- tokens[start_row:end_row,]

#---------------------------
#---------------------------Gather sentiment from first text
df <- tokens %>%
    ##add code to stem
    inner_join(get_sentiments("bing")) %>% #pull only sentiment words
    dplyr::count(sentiment) %>% #count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
df$Percent_Positive <- round(df$positive / (df$positive + df$negative), digits = 2)
df$File <- "100_Transcript.docx"
final_df <- df

#---------------------------
#---------------------------Gather sentiment from remaining files & append to df
for(i in 101:112) {
  setwd("C:/Users/tesr/Documents/R/Data/MSFT")
  fileName <- paste(i, "_Transcript.docx", sep = "")
  fileText <- read_docx(fileName)
  #fileText <- as.character(fileText$text)
  tokens <- data_frame(text = fileText) %>% unnest_tokens(word,text)
  start_row <- round(.3 * nrow(tokens),digits=0)  #the intro & end are noise
  end_row <- round(.9 * nrow(tokens), digits=0)
  tokens <- tokens[start_row:end_row,]
  
  df <- tokens %>%
    ##add code to stem
    inner_join(get_sentiments("bing")) %>% #pull only sentiment words
    dplyr::count(sentiment) %>% #count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  df$Percent_Positive <- round(df$positive / (df$positive + df$negative), digits = 2)
  df$File <- fileName
  final_df <- rbind(final_df,df) #rbind/append to final df
}

final_df$File_Name <- substr(final_df$File, 0, 3)
final_df <- merge(final_df, StockData, by = "File_Name") #Merge sentiment & stock dataframes for further analysis

final_df <- final_df[order(final_df$Percent_Positive, decreasing = T),]

final_df$Decision <- ifelse(final_df$Percent_Positive > mean(final_df$Percent_Positive), "Buy", "Don't Buy") #Buy if sentiment is greater than average

Results <- aggregate.data.frame(final_df$Average_2_Day_Change, by = list(x = final_df$Decision), FUN = mean)
colnames(Results)[1] <- "Decision"
colnames(Results)[2] <- "Average Percentage Change"

scatter.smooth(final_df$Percent_Positive,final_df$Difference_MSFT_S.P500,
               xlab = "Percent Positive",
               ylab = "MSFT vs S&P500 Spread")
