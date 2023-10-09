library(blsR)
library(tidyverse)


# existing jolts data stored in public AWS S3 bucket
old_jolts <- read_csv("https://jolts-data-public.s3.us-east-2.amazonaws.com/jolts-current-data/jolts.csv",
                      col_types = c("iDcccnnn"))
 


# import layoffs and discharges, quit, hires and job opening data from most recent JOLTS
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
  #api_key = bls_key,   # enable line if BLS API key available.
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
         mn_rate = mean(value)) %>% 
  ungroup() %>% 
  arrange(desc(date))








