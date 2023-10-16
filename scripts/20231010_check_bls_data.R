library(aws.s3)
library(blsR)
library(readr)
library(tidyverse)


# download most recent data from BLS API ####

jolts <- data.frame(get_series_tables(
  series_ids = list(ldl = "JTS000000000000000LDL",  # layoffs and discharges levels
                    ldr = "JTS000000000000000LDR",  # layoffs and discharges rate
                    ql  = "JTS000000000000000QUL",  # quit levels
                    qr  = "JTS000000000000000QUR",  # quit rate
                    hl  = "JTS000000000000000HIL",  # hires levels
                    hr  = "JTS000000000000000HIR",  # hires rate
                    tsl = "JTS000000000000000TSL",  # total separation level
                    tsr = "JTS000000000000000TSR",  # total separation rate
                    jol = "JTS000000000000000JOL",  # job openings level
                    jor = "JTS000000000000000JOR"   # job openings rate
  ),
  start_year = 2013,
  end_year   = 2023,
  # enable API line if BLS API key available.
  #api_key = bls_get_key(),
)) %>%
  
  # select a column for year, period (month), periodName (month name), and all the
  # columns that contain the values we want to keep for each series' data
  select("year" = ldl.year, "period" = ldl.period, "periodName" = ldl.periodName,
         "ldl" = ldl.value, "ldr" = ldr.value, "ql" = ql.value,
         "qr" = qr.value, "hl" = hl.value, "hr" = hr.value,  "tsl" = tsl.value,
         "tsr" = tsr.value, "jol" = jol.value, "jor" = jor.value) %>%
  
  # create Date column
  mutate(date = as.Date(readr::parse_date(paste0(year, "-", periodName, "-", "1"),
                                          "%Y-%B-%d"))) %>%
  # pivot to long data
  pivot_longer(
    cols = c(ldl, ldr, ql, qr, hl, hr, tsl, tsr, jol, jor),
    names_to = "series",
    values_to = "value"
  ) %>%
  
  # group by series and month before calculating median and mean for each series
  group_by(series, period) %>%
  mutate(md_rate = median(value),
         mn_rate = round(mean(value),2)) %>%
  ungroup() %>%
  arrange(desc(date))

# retrieve last known update of BLS data that has been saved to AWS S3 bucket ####


bucket <- "jolts-data-public" # name of bucket where jolts file is hosted
folder <- "jolts-current-data"
region <- "us-east-2" # bucket assigned to us-east-2 region


# old_jolts: download data the current jolts data saved in AWS S3 bucket for comparison to current data available on BLS API

save_object(
  object = "current_jolts.csv", # object name in S3 bucket
  bucket = bucket,
  region = region,
  file = "clean_data/AWS_jolts.csv" ) # local location and name for file 

old_jolts <- read_csv("clean_data/AWS_jolts.csv")
# if data is different, write to clean_data folder ####

if(identical(jolts[1,4], old_jolts[1,4]) == F) write_csv(jolts, "clean_data/new_jolts.csv")

# if data is different, send new data to AWS S3 bucket #### 
if(identical(jolts[1,4], old_jolts[1,4]) == F) put_object(
  file = "clean_data/new_jolts.csv",  # local file
  object = "current_jolts.csv",  # name of file going to S3 bucket
  bucket = bucket,
  region = region
) 



