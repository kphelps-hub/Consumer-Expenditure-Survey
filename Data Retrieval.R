#Script to pull data from Bureau of Labor statistics Consumer Expenditure Survey
#For more information see https://www.bls.gov/developers/home.htm

#load necessary packages
library(devtools)
install_github("mikeasilva/blsAPI")
library(jsonlite)
library(httr)
library(tidyverse)

#data to pass into request
api_key <- ""
url <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"

#all data is stored in series ID's over time. I will generate series ID's
#to pass into the API. For the purpose of this study, I want to breakdown by
#age and item_code

#consumer expenditure survey
survey_code <- "CX"
#unadjusted for season
seasonal_adj <- "U"
#item breakdowns
item_code <- c("ALCBEVG",
               "APPAREL",
               "CASHCONT",
               "EDUCATN",
               "ENTRTAIN",
               "FOODTOTL",
               "HEALTH",
               "HOUSING",
               "INSPENSN",
               "MISC",
               "PERSCARE",
               "READING",
               "TOBACCO",
               "TOTALEXP",
               "TRANS")
#which demographic slice. LB04 is by age
dem_code <- "LB04"
#different age ranges
chr_code <- c("01","02","03","04","05","06","07","08","09")
process_code <-"M"
series_id_df <- data.frame()

#for loop to create the series ID's
for (i in 1:length(item_code)){
  for (j in 1:length(chr_code)){
    series_id <- paste(survey_code,seasonal_adj,item_code[i],
                       dem_code,chr_code[j],process_code,sep = "")
    series_id_df <- rbind(series_id_df,
                          c(series_id,item_code[i],chr_code[j]))
  }
}

#rename the column names to make sense
colnames(series_id_df) <- c("series_id",
                            "item_code",
                            "chr_code")

#creating df to store response data
df <- data.frame()

#can only send a request of 25 series at a time
for (i in 1:ceiling(nrow(series_id_df)/25)){
  start <- i*25-24
  end <- min(i*25,nrow(series_id_df))
  payload_series_id_ary <- series_id_df$series_id[start:end]
  payload <- list('seriesid' = payload_series_id_ary,
                  'startyear' = '2015',
                  'endyear' = '2023'
  )
  response <- blsAPI::blsAPI(payload)
  json <- fromJSON(response)
  #each response we want to save off into the larger data frame
  for (j in 1:25){
    #break if going above the total number of series
    index <- (i-1)*25+j
    if (index > nrow(series_id_df)) break
    #want to make sure to add columns for the demographic and item breakdowns
    series_df <- json$Results$series$data[[j]] %>%
      mutate(series_id = json$Results$series$seriesID[[j]],
             chr_code = series_id_df$chr_code[[index]],
             item_code = series_id_df$item_code[[index]]
      )
    df <- rbind(df,series_df)
  }
}

#footnotes have 2 different columns, so extracting those and adding them
footnote_df <- data.frame()
for (i in 1:nrow(df)){
  footnote <- unlist(df$footnotes[[i]])
  if (!is.null(footnote)){
    footnote_row <- c(df$series_id[[i]],df$year[[i]],footnote)
    footnote_df <- rbind(footnote_df,footnote_row)
  }
}
colnames(footnote_df) <- c("series_id",
                           "year",
                           "footnote_code",
                           "footnote_text")

df <-df %>%
  left_join(footnote_df)

#remove original footnote column
df <- df %>%
  subset(select = -c(footnotes))

write.csv(df,
          file = "2023_Consumer_Exp_Data.csv",
          na="NA",
          row.names = FALSE
          )
