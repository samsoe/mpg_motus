library(bigrquery)
library(tidyverse)

project <- 'motus-mpg'

bq_sql_wb <-
  "
  SELECT *
  FROM 
    `motus-mpg.wblake_datadump.filtered`
  "

df_bq_wb <- bq_project_query(project, bq_sql_wb)
df_tb_wb <- bq_table_download(df_bq_wb)
df_wb <- as.data.frame(df_tb_wb) 

df_wb %>% head()

bq_sql_ms <-
  "
  SELECT *
  FROM 
    `motus-mpg.mscofield_motus.filtered`
  "

df_bq_ms <- bq_project_query(project, bq_sql_ms)
df_tb_ms <- bq_table_download(df_bq_ms)
df_ms <- as.data.frame(df_tb_ms) 

df_ms %>% head()