library(bigrquery)
library(tidyverse)

project <- 'motus-mpg'

bq_sql <-
  "
  SELECT *
  FROM 
    `motus-mpg.wblake_datadump.filtered`
  "
df_bq <- bq_project_query(project, bq_sql)
df_tb <- bq_table_download(df_bq)
df <- as.data.frame(df_tb) 

df %>% head()
