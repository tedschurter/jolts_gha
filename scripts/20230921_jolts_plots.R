library(blsR)
library(tidyverse)
library(ggtext)
library(scales)


# import JOLTS data for the last ten years for layoffs and discharges, quits and hires

bls_key <- # load bls API key if available. Registered API users get higher data limits:
# https://www.bls.gov/developers/api_faqs.htm

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
  api_key = bls_key,   # enable line if BLS API key available.
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


# export as csv 
#write_csv(jolts, file = "clean_data/jolts.csv")

#  

# dataframe for last 12 months
jolts_yr <- jolts %>% 
  group_by(series) %>% 
  slice(1:12)


# dataframe for 10-year average
jolts_avg <- 
  jolts %>% 
  group_by(series) %>% 
  arrange(desc(date), series) %>% 
  slice(1:120)


# labels for 12 month plots
labs <- jolts_yr %>% 
  group_by(series) %>% 
  ungroup() %>% 
  slice(1:12) %>% 
  select(date, periodName)

# dataframe for series plot label values
series_labs <- data.frame(x = max(as.Date(jolts_yr$date)),#+ days(7),
                          value = jolts %>% 
                            filter(date == max(jolts$date)) %>%
                            group_by(series) %>%
                            slice(1) %>% ungroup() %>% select(value),
                          series = jolts %>% group_by(series) %>% 
                            slice(10) %>% ungroup() %>% select(series)) %>% 
  mutate(series_lab = 
           case_when(series == "ldr" ~  "Layoffs and\ndischarges",
                     series == "ldl" ~  "Layoffs and\ndischarges",
                     series == "qr"  ~ "Quits",
                     series == "ql"  ~ "Quits",
                     series == "tsr" ~ "Total\nseparations",
                     series == "tsl" ~ "Total\nseparations",
                     series == "hr" ~ "Hires",
                     series == "hl" ~ "Hires",
                     series == "jol" ~ "Job\nopenings",
                     series == "jor" ~ "Job\nopenings")) 

# assign colors to series, background


tsr_c <- NA
jo_c <-  "#018571"
hr_c <-  "#80cdc1"
qr_c <-  "#a6611a"  
ldr_c <- "#dfc27d"
bg_c <-  "#fbf9f6" # background color

#  colors for serie rates and levels
series_cols <- c(tsr  = tsr_c,
                 tsl  = qr_c,
                 qr   = qr_c, 
                 ql   = qr_c,
                 ldr  = ldr_c,
                 ldl  = ldr_c,
                 hr   = hr_c,
                 hl   = hr_c,
                 bg_c = bg_c,
                 jol  = jo_c,
                 jor  = jo_c)


# dataframe for dummy data to build legend for linetypes
blank <- data.frame(
  date = rep(c(as.Date("2021-8-1"),
               as.Date("2021-8-2"),
               as.Date("2021-8-3")),2),
  type = rep(c("rate", "mean"), 3),
  value = c(2,1,4,3,6,5),
  color = rep("bg_c", 3)
)




# dataframe for y axis grid lines and labels for rate for 10 year plots
y_scale_seg <- 
  data.frame(
    x    = rep(as.Date(min(jolts$date)),5),
    xend = rep(as.Date(max(jolts$date)),5),
    y    = c(0,
      plyr::round_any(max(jolts$value[jolts$series == "jor"])/4, accuracy = 1)*1,
      plyr::round_any(max(jolts$value[jolts$series == "jor"])/4, accuracy = 1)*2,
      plyr::round_any(max(jolts$value[jolts$series == "jor"])/4, accuracy = 1)*3,
      plyr::round_any(max(jolts$value[jolts$series == "jor"])/4, accuracy = 1)*4
    ),
    yend = 
      c(0,
        plyr::round_any(max(jolts$value[jolts$series == "jor"])/4, accuracy = 1)*1,
        plyr::round_any(max(jolts$value[jolts$series == "jor"])/4, accuracy = 1)*2,
        plyr::round_any(max(jolts$value[jolts$series == "jor"])/4, accuracy = 1)*3,
        plyr::round_any(max(jolts$value[jolts$series == "jor"])/4, accuracy = 1)*4
      ),
    labels = 
      c("0%",
        paste0(plyr::round_any(max(jolts$value[jolts$series == "jor"])/4, accuracy = 1)*1, "%"),
        paste0(plyr::round_any(max(jolts$value[jolts$series == "jor"])/4, accuracy = 1)*2, "%"),
        paste0(plyr::round_any(max(jolts$value[jolts$series == "jor"])/4, accuracy = 1)*3, "%"),
        paste0(plyr::round_any(max(jolts$value[jolts$series == "jor"])/4, accuracy = 1)*4, "%")
      ))

# y scale for 10-year levels
y_scale_lev <- data.frame(
  scale = 
    c(0,
      plyr::round_any(max(jolts$value[jolts$series == "tsl"]),accuracy = 1000)*.2,
      plyr::round_any(max(jolts$value[jolts$series == "tsl"]),accuracy = 1000)*.4,
      plyr::round_any(max(jolts$value[jolts$series == "tsl"]),accuracy = 1000)*.6,
      plyr::round_any(max(jolts$value[jolts$series == "tsl"]),accuracy = 1000)*.8,
      plyr::round_any(max(jolts$value[jolts$series == "tsl"]),accuracy = 1000)),
  labels = 
    c(0,
      prettyNum(plyr::round_any(max(jolts$value[jolts$series == "tsl"]),accuracy = 1000)*.2, big.mark = ","),
      prettyNum(plyr::round_any(max(jolts$value[jolts$series == "tsl"]),accuracy = 1000)*.4, big.mark = ","),
      prettyNum(plyr::round_any(max(jolts$value[jolts$series == "tsl"]),accuracy = 1000)*.6, big.mark = ","),
      prettyNum(plyr::round_any(max(jolts$value[jolts$series == "tsl"]),accuracy = 1000)*.8, big.mark = ","),
      prettyNum(plyr::round_any(max(jolts$value[jolts$series == "tsl"]),accuracy = 1000), big.mark = ","))
)


# y scale and labels for 12 month rate
y_scale_seg_mo_rate <- data.frame(
  x    = rep(as.Date(min(jolts_yr$date)),5),
  xend = rep(as.Date(max(jolts_yr$date)),5),
  y =
    c(
      0, 1,
      plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jor"]*.5), 1),
      plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jor"]*.75), 1),
      plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jor"]), 1)),
  yend =
    c(
      0, 1,
      plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jor"]*.5), 1),
      plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jor"]*.75), 1),
      plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jor"]), 1)
    ),
  labels =
    c("0%",
      "1%",
      paste0(plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jor"]*.5), 1), "%"),
      paste0(plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jor"]*.75), 1), "%"),
      paste0(plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jor"]), 1), "%")))


# y scale for levels for 12 month plots
y_scale_seg_mo_lev <- data.frame(
  x    = rep(as.Date(min(jolts_yr$date)),5),
  xend = rep(as.Date(max(jolts_yr$date)),5),
  y = c(0,plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.25,
        plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.5,
        plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.75,
        plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)),
  yend =  
    c(0,plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.25,
      plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.5,
      plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.75,
      plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000))
)



# plots ####

# set custom theme ####

# set custom theme ####

# background color 
bg_c <-  "#fbf9f6" 

# theme
t_theme <- function(base_size = 10){
  
  theme_classic() %+replace%
    
    theme(
      axis.title = element_blank(),
      axis.text.x   = element_text(rel(x = .5), color = "#636363"),
      axis.text.y = element_text(rel(x = .5), color = "#636363", margin=margin(0,-20,0,0)),
      axis.line = element_blank(),
      axis.ticks.x = element_line(color = "#636363"),
      axis.ticks.y = element_blank(),
      legend.background = element_rect(fill = NA, color = NA),
      legend.key.width= unit(1.25, 'cm'),
      legend.position = c(.15,.95),
      legend.text = element_text(size = 9),
      legend.title = element_blank(),
      legend.key = element_blank(),
      plot.title.position = "plot",
      plot.margin = margin(.1,.25,.1,.25, "in"),
      plot.background = element_rect(fill  = bg_c, color = NA),
      panel.background = element_rect(fill = bg_c, color = NA),
      plot.title = element_textbox(
        family = "serif", 
        size = rel(x = 1.5),
        width = unit(1, "npc"),
        padding = margin(4, 4, 1, 4),
        margin = margin(8, 0, 0, 0),
        lineheight = 1
      ),
      plot.subtitle = element_textbox(
        family = "sans", 
        size = rel(x = 1),
        width = unit(1, "npc"),
        padding = margin(4, 4, 4, 4),
        margin  = margin(0, 0, 4, 0),
        lineheight = 1
      ),
      plot.caption = element_text(margin = margin(10, 0, 0, 0), hjust = 1, size = rel(x = .8))
    )
}

# caption for plots
caption <- paste0("Data:www.bls.gov/jlt ", expression(" *"), "July 2023 data is preliminary.\nData currrent as of ", Sys.Date())

# plots 

# 12 month BLS hires, quits, layoffs and discharge levels
ggplot()+
  
  # add dummy data in order to create simple legend for dashed/solid lines
  geom_line(data = blank,
            aes(x = date+years(1), y = 1000, group = type, linetype = type, color = color))+
  
  
  # scale for linetypes
  scale_linetype_manual(values = c("rate" = "solid",
                                   "mean" = "dashed"),
                        labels = c(
                          "10-year monthly average",
                          "Past 12 months"),
                        guide_legend(title = ""))+
  
  # remove guide for color
  guides(color = "none")+
  
  # add dotted lines for axis lines that stop with data (don't extend over labels like gridlines)
  geom_segment(data  = y_scale_seg_mo_lev,
               aes(x = x, xend = xend, 
                   y = y, yend = yend), color = "lightgray", 
               linetype = "dotted")+
  
  # add lines for each series
  geom_line(data = jolts_yr %>% filter(
    series == "ql"|
      series == "ldl"|
      series == "hl"|
      series == "jol"),
    aes(date, value, group = series, color = series),
    size = 1.5, show.legend = F)+
  
  # add points for each series
  # geom_point(data = jolts_yr %>% filter(
  #   series == "ql"|
  #     series == "ldl"|
  #     series == "hl"|
  #     series == "jol"),
  #   aes(date, value, group = series, color = series),
  #   size = 2.75, show.legend = F)+
  
  # add dashed lines for each series average rate
  geom_line(data =  jolts %>% 
              group_by(series) %>% 
              arrange(desc(date), series) %>% 
              slice(1:120) %>% 
              filter(series == "ql"|
                       series == "ldl"|
                       series == "hl"|
                       series == "jol"),
            aes(date, mn_rate, group = series, color = series),
            linetype = "dashed", size = .5,
            show.legend = F)+
  
  # label series lines directly
  geom_text(data = series_labs %>% filter(series == "ql"|
                                            series == "ldl"|
                                            series == "hl"|
                                            series == "jol"),
            aes(x = as.Date(ymd("2023-7-7")), y = value, label = series_lab),
            size = rel(x = 3.25), 
            hjust = 0,
            show.legend = F, 
            color = "#636363", 
            lineheight = 1
  )+
  
  
  scale_color_manual(values = series_cols)+
  
  scale_y_continuous(limits = c(0, max(jolts_yr$value)+1000),
                     breaks = 
                       c(0,
                         plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.25,
                         plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.5,
                         plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.75,
                         plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)),
                     labels = 
                       c("0",
                         format(plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.25, big.mark = ","),
                         format(plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.5, big.mark = ","),
                         format(plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.75, big.mark = ","),
                         format(plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000), big.mark = ",")))+
  

  
  scale_x_date(date_breaks = "1 month",
               labels = date_format("%b"),
               limits = c(min(jolts_yr$date), max(jolts_yr$date)+months(1)),
  )+
  # add titles, captions    
  labs(
    title = paste0("Seasonally adjusted monthly level of job openings, hires, quits, layoffs and discharges from ", 
                   month(min(jolts_yr$date), label = T, abbr = F), " ", year(min(jolts_yr$date)),
                   " to ", 
                   month(max(jolts_yr$date), label = T, abbr = F), " ", 
                   year(max(jolts_yr$date)), ", and the 10-year year monthly average.*"), 
                  
    subtitle = "*Job opening:* A specific, full, part-time, permanent, or seasonal position that could be started within 30 days with active recruitment for the position from outside the offering establishment. *Hires*: all additions to the payroll; *quits*:
    employees who left voluntarily, excluding retirements and transfers; 
    *layoffs and discharges*: involuntary separations initiated by the employer.",
    
    caption = caption
  )+
  
  # set theme
  t_theme()
  
#ggsave("plots/bls_12_mo_levels.svg", width = 3000, height = 1800, unit = "px", plot = last_plot())



# layoffs and discharge, quits and hires rates in the last 12 months
ggplot()+
  
  # add dummy data in order to create simple legend for dashed/solid lines
  geom_line(data = blank,
            aes(x = date, y = value, group = type, color = type, linetype = type))+
  
  # scale for linetypes
  scale_linetype_manual(values = c("rate" = "solid",
                                   "mean" = "dashed"),
                        labels = c(
                          "10-year monthly average",
                          "Past 12 months"),
                        guide_legend(title = ""))+
  
  # remove guide for color
  guides(color = "none")+
  
  # add dotted lines for axis lines that stop with data (don't extend over labels like gridlines)
  geom_segment(data  = y_scale_seg_mo_rate,
               aes(x = x, xend = xend, 
                   y = y, yend = yend), color = "lightgray", 
               linetype = "dotted")+
  
  # add lines for each series
  geom_line(data = jolts_yr %>% filter(
    series == "qr"|
      series == "ldr"|
      series == "hr"|
      series == "jor"),
    aes(date, value, group = series, color = series),
    size = 1.5, show.legend = F)+
  
  # add points for each series
  # geom_point(
  #   data = jolts_yr %>% filter(
  #     series == "qr"|
  #       series == "ldr"|
  #       series == "hr"|
  #       series == "jor"),
  #   aes(date, value, group = series, color = series),
  #   size = 2.75, show.legend = F)+
  
  # add dashed lines for each series average rate
  geom_line(data = jolts %>% 
              group_by(series) %>% 
              arrange(desc(date), series) %>% 
              slice(1:120) %>% 
              filter(
                series == "qr"|
                  series == "ldr"|
                  series == "hr"|
                  series == "jor"),
            aes(date, mn_rate, group = series, color = series),
            linetype = "dashed", size = .5,
            show.legend = F)+
  
  # label series lines directly
  geom_text(data = series_labs %>% filter(series != "tsl" & series != "tsr"),
            aes(x = as.Date(ymd("2023-7-7")), y = value, label = series_lab),
            size = rel(x = 3.25), 
            hjust = 0,
            show.legend = F, 
            color = "#636363", 
            lineheight = 1
  )+
  
  
  scale_color_manual(values = series_cols)+
  
  scale_y_continuous(limits = c(0, max(jolts$value[jolts$series == "jor"])+.5),
                     breaks = y_scale_seg_mo_rate$y,
                     labels = y_scale_seg_mo_rate$labels)+
  
  scale_x_date(date_breaks = "1 month",
               labels = date_format("%b"),
               limits = c(min(jolts_yr$date), max(jolts_yr$date)+months(1)),
  )+
  # add titles, captions    
  labs(
    title = paste0("Seasonally adjusted monthly rate of job openings, hires, quits, layoffs and discharges from ", 
                   month(min(jolts_yr$date), label = T, abbr = F), " ", year(min(jolts_yr$date)),
                   " to ", 
                   month(max(jolts_yr$date), label = T, abbr = F), " ", 
                   year(max(jolts_yr$date)), ", and the 10-year year monthly average.*"), 
    
    subtitle = "*Job opening:* A specific, full, part-time, permanent, or seasonal position that could be started within 30 days with active recruitment for the position from outside the offering establishment. *Hires*: all additions to the payroll; *quits*:
    employees who left voluntarily, excluding retirements and transfers; 
    *layoffs and discharges*: involuntary separations initiated by the employer.",
    
    caption = caption
  )+
  
  # set theme, make adjustments    
  t_theme()
  
#ggsave("plots/bls_12_mo_rate.svg", width = 3000, height = 1800, unit = "px", plot = last_plot())


# plot showing levels of hires, quits, layoffs and discharges 2013 to 2023

ggplot()+
  
  # add lines for each series: hires, quits, layoffs and discharges
  geom_line(data = jolts %>% filter(series == "ql"|
                                      series == "ldl"|
                                      series == "hl"| 
                                      series == "jol"),
            aes(x = date, value, group = series, color = series),
            size = 1, show.legend = F)+
  
  # add dotted lines for axis lines that stop with data (don't extend over labels like gridlines)
  geom_segment(data = y_scale_lev %>% filter(scale >1),
               aes(x = as.Date(min(jolts$date)), xend = as.Date(max(jolts$date)), 
                   y = scale, yend = scale), color = "lightgray",
               linetype = "dotted")+
  
  # label series lines directly
  geom_text(data = series_labs %>% filter(series == "ql"|
                                            series == "ldl"|
                                            series == "hl"| 
                                            series == "jol"),
            aes(x = x + months(1),            #as.POSIXct(ymd("2023-7-5")), 
                y = value, label = series_lab),
            size = rel(x = 3), 
            hjust = 0,
            show.legend = F, 
            color = "#636363", 
            lineheight = 1
  )+
  
  # add labels for years and extend xaxis to accomodate directly labeling lines  
  scale_x_date(date_breaks = "1 year",
               labels = date_format("%Y"),
               limits = c(min(jolts$date), max(jolts$date)+months(4)),
  )+
  
  # customize colors  
  scale_color_manual(values = series_cols)+
  
  #  custom y scale  
  scale_y_continuous(
    limits = c(0, max(jolts$value[jolts$series != "tsl"])),
    breaks = y_scale_lev$scale,
    labels = y_scale_lev$labels)+
  
  # add titles, captions    
  labs(
    title = paste0("Seasonally adjusted levels of job openings, hires, quits, layoffs and discharges from ", 
                   year(min(jolts$date)),
                   " to ", 
                   month(max(jolts$date), label = T, abbr = F), " ",
                   year(max(jolts$date)), ".*"),
    subtitle = "*Job opening:* A specific, full, part-time, permanent, or seasonal position that could be started within 30 days with active recruitment for the position from outside the offering establishment. *Hires*: all additions to the payroll; *quits*:
    employees who left voluntarily, excluding retirements and transfers; 
    *layoffs and discharges*: involuntary separations initiated by the employer.",
    
    caption = caption
  )+
  
  # set theme, make adjustments    
  t_theme()+
  
  # extra clearance needed between title, subtitle
  theme(
    plot.subtitle = element_textbox(
      family = "sans", 
      size = rel(x = 1),
      width = unit(1, "npc"),
      padding = margin(4, 4, 4, 4),
      margin  = margin(5, 0, 4, 0),
      lineheight = 1
    ),
  )
  
#ggsave("plots/bls_yrs_level.svg", width = 3000, height = 1800, unit = "px", plot = last_plot())
##


# plot showing rate of hires, quits and layoffs through 2023
ggplot()+
  
  # add lines for each series: hires, quits, layoffs and discharges
  geom_line(data = jolts %>% filter(series == "qr"|
                                      series == "ldr"|
                                      series == "hr"|
                                      series == "jor"),
            aes(x = as.Date(date), value, group = series, color = series),
            size = 1, show.legend = F)+
  
  # add dotted lines for axis lines that stop with data (don't extend over labels like gridlines)
  geom_segment(data = y_scale_seg,
               aes(x = x, xend = xend, y = y, yend = yend), color = "lightgray", 
               linetype = "dotted")+
  
  # label series lines directly
  geom_text(data = series_labs %>% filter(series == "qr"|
                                            series == "ldr"|
                                            series == "hr"|
                                            series == "jor"),
            aes(x = x + months(1),           
                y = value, label = series_lab),
            size = rel(x = 3), 
            hjust = 0,
            show.legend = F, 
            color = "#636363", 
            lineheight = 1
  )+
  
  # add labels for years and extend xaxis to accomodate directly labeling lines  
  scale_x_date(date_breaks = "1 year",
               labels = date_format("%Y"),
               limits = c(min(jolts$date), max(jolts$date)+months(4)),
  )+
  
  # customize colors  
  scale_color_manual(values = series_cols)+
  
  # custom y scale  
  scale_y_continuous(
    limits = c(0, plyr::round_any(max(jolts$value[jolts$series != "jol" &
                                                    jolts$series != "ql"  &
                                                    jolts$series != "tsl" &
                                                    jolts$series != "hl"  &
                                                    jolts$series != "ldl" &
                                                    jolts$series != "tsr"
    ]), accuracy = 1)),
    breaks = c(y_scale_seg$y),
    labels = c(y_scale_seg$labels))+
  
  # add titles, captions    
  labs(
    title = paste0("Seasonally adjusted rate of job openings, hires, quits, layoffs and discharges from ", 
                   year(min(jolts$date)),
                   " to ", 
                   month(max(jolts$date), label = T, abbr = F), ", ", 
                   year(max(jolts$date)), ".*"),
    subtitle = "*Job opening:* A specific, full, part-time, permanent, or seasonal position that could be started within 30 days with active recruitment for the position from outside the offering establishment. *Hires*: all additions to the payroll; *quits*:
    employees who left voluntarily, excluding retirements and transfers; 
    *layoffs and discharges*: involuntary separations initiated by the employer.",
    
    caption = caption
  )+
  
  # set theme, make adjustments    
  t_theme()+
  
  # extra clearance needed between title, subtitle
  theme(
    plot.subtitle = element_textbox(
      family = "sans", 
      size = rel(x = 1),
      width = unit(1, "npc"),
      padding = margin(4, 4, 4, 4),
      margin  = margin(5, 0, 4, 0),
      lineheight = 1
    ),
  )

#ggsave("plots/bls_yrs_rate.svg", width = 3000, height = 1800, unit = "px", plot = last_plot())


# separating plots for 1.)job openings and total separations, and 2.) hires, quits, layoffs and discharges


# 12 month BLS hires, quits, layoffs and discharge levels ####
ggplot()+
  
  # add dummy data in order to create simple legend for dashed/solid lines
  geom_line(data = blank,
            aes(x = date+years(1), y = 1000, group = type, linetype = type, color = color))+
  
  
  # scale for linetypes
  scale_linetype_manual(values = c("rate" = "solid",
                                   "mean" = "dashed"),
                        labels = c(
                          "10-year monthly average",
                          "Past 12 months"),
                        guide_legend(title = ""))+
  
  # remove guide for color
  guides(color = "none")+
  
  # add dotted lines for axis lines that stop with data (don't extend over labels like gridlines)
  geom_segment(data  = y_scale_seg_mo_lev,
               aes(x = x, xend = xend, 
                   y = y, yend = yend), color = "lightgray", 
               linetype = "dotted")+
  
  # add lines for each series
  geom_line(data = jolts_yr %>% 
              filter(
                series == "tsl"|
                  series == "jol"),
            aes(date, value, group = series, color = series),
            size = 1.5, show.legend = F)+
  
  # add points for each series
  # geom_point(data = jolts_yr %>% filter(
  #   series == "tsl"|
  #     series == "jol"),
  #   aes(date, value, group = series, color = series),
  #   size = 2.75, show.legend = F)+
  
  # add dashed lines for each series average rate
  geom_line(data =  jolts %>% 
              group_by(series) %>% 
              arrange(desc(date), series) %>% 
              slice(1:120) %>% 
              filter(
                series == "tsl"|
                  series == "jol"),
            aes(date, mn_rate, group = series, color = series),
            linetype = "dashed", size = .5,
            show.legend = F)+
  
  # label series lines directly
  geom_text(data = series_labs %>% 
              filter(
                series == "tsl"|
                  series == "jol"),
            aes(x = as.Date(ymd("2023-7-7")), y = value, label = series_lab),
            size = rel(x = 3.25), 
            hjust = 0,
            show.legend = F, 
            color = "#636363", 
            lineheight = 1
  )+
  
  
  scale_color_manual(values = series_cols)+
  
  scale_y_continuous(limits = c(0, max(jolts_yr$value)+1000),
                     breaks = 
                       c(0,
                         plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.25,
                         plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.5,
                         plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.75,
                         plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)),
                     labels = 
                       c("0",
                         format(plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.25, big.mark = ","),
                         format(plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.5, big.mark = ","),
                         format(plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000)*.75, big.mark = ","),
                         format(plyr::round_any(max(jolts_yr$value[jolts_yr$series == "jol"]), 1000), big.mark = ",")))+
  
  #scales::label_number(big.mark = ","))+ 
  
  scale_x_date(date_breaks = "1 month",
               labels = date_format("%b"),
               limits = c(min(jolts_yr$date), max(jolts_yr$date)+months(1)),
  )+
  # add titles, captions    
  labs(
    title = paste0("Seasonally adjusted levels of monthly job openings and total separations from ", 
                   month(min(jolts_yr$date), label = T, abbr = F), " ", year(min(jolts_yr$date)),
                   " to ", 
                   month(max(jolts_yr$date), label = T, abbr = F), " ", 
                   year(max(jolts_yr$date)), ", and the 10-year monthly average.", expression("*")),
    subtitle = "*Job opening:* A specific, full, part-time, permanent, or seasonal position that could be started within 30 days with active recruitment for the position from outside the offering establishment. *Total separations:* quits, layoffs and discharges, and other separations.",
    
    caption = caption
  )+
  
  # set theme, make adjustments    
  t_theme()

#ggsave("plots/bls_12_mo_levels_jots.svg", width = 3000, height = 1800, unit = "px", plot = last_plot())
# 

# layoffs and discharge, quits and hires rates in the last 12 months ####
ggplot()+
  
  # add dummy data in order to create simple legend for dashed/solid lines
  geom_line(data = blank,
            aes(x = date, y = value, group = type, color = type, linetype = type))+
  
  # scale for linetypes
  scale_linetype_manual(values = c("rate" = "solid",
                                   "mean" = "dashed"),
                        labels = c(
                          "10-year monthly average",
                          "Past 12 months"),
                        guide_legend(title = ""))+
  
  # remove guide for color
  guides(color = "none")+
  
  # add dotted lines for axis lines that stop with data (don't extend over labels like gridlines)
  geom_segment(data  = y_scale_seg_mo_rate,
               aes(x = x, xend = xend, 
                   y = y, yend = yend), color = "lightgray", 
               linetype = "dotted")+
  
  # add lines for each series
  geom_line(data = jolts_yr %>% filter(
    # series == "qr"|
    # series == "ldr"|
    # series == "hr"|
    series == "jor"|
      series == "tsr"),
    aes(date, value, group = series, color = series),
    size = 1.5, show.legend = F)+
  
  # add points for each series
  # geom_point(
  #   data = jolts_yr %>% filter(
  #       # series == "qr"|
  #       # series == "ldr"|
  #       # series == "hr"|
  #       series == "jor"|
  #       series == "tsr"),
  #   aes(date, value, group = series, color = series),
  #   size = 2.75, show.legend = F)+

# add dashed lines for each series average rate
geom_line(data = jolts %>% 
            group_by(series) %>% 
            arrange(desc(date), series) %>% 
            slice(1:120) %>% 
            filter(
              # series == "qr"|
              #   series == "ldr"|
              #   series == "hr"|
              series == "jor"|
                series == "tsr"),
          aes(date, mn_rate, group = series, color = series),
          linetype = "dashed", size = .5,
          show.legend = F)+
  
  # label series lines directly
  geom_text(data = series_labs %>% filter(series == "tsr" |
                                            series == "jor"),
            aes(x = as.Date(ymd("2023-7-7")), y = value, label = series_lab),
            size = rel(x = 3.25), 
            hjust = 0,
            show.legend = F, 
            color = "#636363", 
            lineheight = 1
  )+
  
  
  scale_color_manual(values = series_cols)+
  
  scale_y_continuous(limits = c(0, max(jolts$value[jolts$series == "jor"])+.5),
                     breaks = c(0, y_scale_seg_mo_rate$y),
                     labels = c("", y_scale_seg_mo_rate$labels))+
  
  scale_x_date(date_breaks = "1 month",
               labels = date_format("%b"),
               limits = c(min(jolts_yr$date), max(jolts_yr$date)+months(1)),
  )+
  # add titles, captions    
  labs(
    title = paste0("Seasonally adjusted rate of monthly job openings and total separations from ", 
                   month(min(jolts_yr$date), label = T, abbr = F), " ", year(min(jolts_yr$date)),
                   " to ", 
                   month(max(jolts_yr$date), label = T, abbr = F), " ", 
                   year(max(jolts_yr$date)), ", and the 10-year monthly average.*"),
    
    subtitle = "*Job opening:* A specific, full, part-time, permanent, or seasonal position that could be started within 30 days with active recruitment for the position from outside the offering establishment. *Total separations:* quits, layoffs and discharges, and other separations.",
    
    caption = caption
  )+
  
  
  # set theme, make adjustments
  t_theme()
  

#ggsave("plots/bls_12_mo_rate_jots.svg", width = 3000, height = 1800, unit = "px", plot = last_plot())

# plot showing rate of hires, quits and layoffs 2013 to 2023 ####
ggplot()+
  
  # add lines for each series: hires, quits, layoffs and discharges
  geom_line(data = jolts %>% filter(series == "tsr"|
                                      series == "jor"),
            aes(x = as.Date(date), value, group = series, color = series),
            size = 1, show.legend = F)+
  
  # add dotted lines for axis lines that stop with data (don't extend over labels like gridlines)
  geom_segment(data = y_scale_seg,
               aes(x = x, xend = xend, y = y, yend = yend), color = "lightgray", 
               linetype = "dotted")+
  
  # label series lines directly
  geom_text(data = series_labs %>% filter(series == "tsr"|
                                            series == "jor"),
            aes(x = x + months(1),           
                y = value, label = series_lab),
            size = rel(x = 3), 
            hjust = 0,
            show.legend = F, 
            color = "#636363", 
            lineheight = 1
  )+
  
  # add labels for years and extend xaxis to accomodate directly labeling lines  
  scale_x_date(date_breaks = "1 year",
               labels = date_format("%Y"),
               limits = c(min(jolts$date), max(jolts$date)+months(4)),
  )+
  
  # customize colors  
  scale_color_manual(values = series_cols)+
  
  # custom y scale  
  scale_y_continuous(
    limits = c(0, plyr::round_any(max(jolts$value[jolts$series != "jol" &
                                                    jolts$series != "ql"  &
                                                    jolts$series != "tsl" &
                                                    jolts$series != "hl"  &
                                                    jolts$series != "ldl"]), 
                                  accuracy = 1)),
    breaks = c(0, y_scale_seg$y),
    labels = c("0%", y_scale_seg$labels))+
  
  # add titles, captions    
  labs(
    title = paste0("Seasonally adjusted rate of job openings and total separations from ", 
                   year(min(jolts$date)),
                   " to ", 
                   month(max(jolts$date), label = T, abbr = F), " ", year(max(jolts$date)), ".*"),
    subtitle = "*Job opening:* A specific, full, part-time, permanent, or seasonal position that could be started within 30 days with active recruitment for the position from outside the offering establishment. *Total separations:* quits, layoffs and discharges, and other separations.",
    
    caption = caption 
    )+
  
  # set theme, make adjustments    
  t_theme()


#ggsave("plots/bls_10_yr_rate_jots.svg", width = 3000, height = 1800, unit = "px", plot = last_plot())


# job openings and total separation levels from 2013 to 2023 ####
ggplot()+
  
  # add lines for each series: hires, quits, layoffs and discharges
  geom_line(data = jolts %>% 
              filter(series == "tsl"| 
                       series == "jol"),
            aes(x = date, value, group = series, color = series),
            size = 1, show.legend = F)+
  
  # add dotted lines for axis lines that stop with data (don't extend over labels like gridlines)
  geom_segment(data = y_scale_lev,# %>% filter(scale >1),
               aes(x = as.Date(min(jolts$date)), xend = as.Date(max(jolts$date)), 
                   y = scale, yend = scale), color = "lightgray",
               linetype = "dotted")+
  
  # label series lines directly
  geom_text(data = series_labs %>% 
              filter(series == "tsl"| 
                       series == "jol"),
            aes(x = x + months(1),            
                y = value, label = series_lab),
            size = rel(x = 3), 
            hjust = 0,
            show.legend = F, 
            color = "#636363", 
            lineheight = 1
  )+
  
  scale_x_date(date_breaks = "1 year",
               labels = date_format("%Y"),
               limits = c(min(jolts$date), max(jolts$date)+months(4)),
  )+
  
  # customize colors  
  scale_color_manual(values = series_cols)+
  
  #  custom y scale  
  scale_y_continuous(
    limits = c(0, max(jolts$value[jolts$series == "tsl"])),
    breaks = y_scale_lev$scale,
    labels = y_scale_lev$labels)+
  
  # add titles, captions    
  labs(
    title = paste0("Seasonally adjusted levels of job openings and total separations from ", 
                   year(min(jolts$date)),
                   " to ", 
                   month(max(jolts$date), label = T, abbr = F), " ", year(max(jolts$date)), ".*"),
    
    subtitle = "*Job opening:* A specific, full, part-time, permanent, or seasonal position that could be started within 30 days with active recruitment for the position from outside the offering establishment. *Total separations:* quits, layoffs and discharges, and other separations.",
    
    caption = caption
  )+
  
  # set theme, make adjustments    
  t_theme()
 

ggsave("plots/bls_10_yr_levels_jots.svg", width = 3000, height = 1800, unit = "px", plot = last_plot())

