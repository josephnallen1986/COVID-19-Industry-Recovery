library(scales)       # Additional graphics functions
library(RColorBrewer) # Color ramps for graphs and maps
library(gridExtra)    # Functions for arranging multiple plots on a page
library(RSocrata)     # Download or Upload 'Socrata' Data Sets
library(Cairo)        # Create high-quality vector (PDF, PostScript and SVG) and bitmap output
library(tidyverse)    # Collection of R packages designed for data science
library(zoo)          # Moving averages     
library(lubridate)    # Makes it easier to work with dates and times.

great_recession <- read.socrata(
  "https://data.edd.ca.gov/resource/r4zm-kdcg.json?$where=seasonally_adjusted = 'N' AND year BETWEEN '2007' AND '2015'") %>%
  filter(
    area_type == "Metropolitan Area" & (
      series_code == "20000000" | series_code == "30000000" | series_code == "40000000" |
        series_code == "50000000" | series_code == "55000000" | series_code == "60000000" |
        series_code == "70000000" | series_code == "80000000" | series_code == "0")) %>%
  select(msa = area_name, month = date, industry_title, estimated_employment = current_employment) %>%
  mutate (
    estimated_employment = as.numeric(estimated_employment),
    month = as.Date(month))

covid <- read.socrata(
  "https://data.edd.ca.gov/resource/r4zm-kdcg.json?$where=seasonally_adjusted = 'N' AND year >= '2020'") %>%
  filter(
    area_type == "Metropolitan Area" & (
      series_code == "20000000" | series_code == "30000000" | series_code == "40000000" |
        series_code == "50000000" | series_code == "55000000" | series_code == "60000000" |
        series_code == "70000000" | series_code == "80000000" | series_code == "0") &
      date >= "2020-02-01") %>%
  select(msa = area_name, month = date, industry_title, estimated_employment = current_employment) %>%
  mutate (
    estimated_employment = as.numeric(estimated_employment),
    month = as.Date(month))

gr_start <- great_recession %>% 
  group_by(msa, industry_title) %>%
  filter(year(month) == 2007) %>%
  filter(estimated_employment == max(estimated_employment)) %>%
  filter(month == min(month)) %>%
  rename(gr_start_month = month, gr_start_emp = estimated_employment)

gr <- great_recession %>%
  inner_join(gr_start, by = c("msa" = "msa", "industry_title" = "industry_title")) %>%
  mutate(
    months = interval(gr_start_month, month) %/% months(1), 
    peak = estimated_employment / gr_start_emp * 100)  %>%
  filter(months >= 0,
         str_detect(msa, "Riverside|San Diego|Los Angeles|Anaheim"),
         peak <= 101)

covid_end <- covid %>%
  filter(month == max(month)) %>%
  rename(covid_end_month = month, covid_end_emp = estimated_employment)

covid_period <- covid %>% 
  filter(month == "2020-02-01") %>%
  inner_join(covid_end, by = c("msa" = "msa", "industry_title" = "industry_title")) %>%
  rename(covid_start_month = month, covid_start_emp = estimated_employment)

corona <- covid %>%
  inner_join(covid_period, by = c("msa" = "msa", "industry_title" = "industry_title"))  %>%
  mutate(
    months = interval(covid_start_month, month) %/% months(1),
    peak = estimated_employment / covid_start_emp * 100) %>%
  filter(
    str_detect(msa, "Riverside|San Diego|Los Angeles-Long Beach-Glendale MD|Anaheim"),
    peak <= 101)

d <- paste(getwd(),"/Output/",format(max(covid$month), "%y-%m")," ",month.abb[month(max(covid$month))],sep="")
dir.create(d, showWarnings = FALSE)

industry_list <- unique(corona$industry_title)

colors <- c("The Great Recession"= "#7439A4","COVID-19"="#FA1414")

for (industry in industry_list) { 
  
  df1 <- gr %>%
    arrange(msa,industry_title,month) %>%
    filter(industry_title == industry)
  
  df2 <- corona %>%
    arrange(msa,industry_title,month) %>%
    filter(industry_title == industry) 
  
  recovery <- ggplot() +
    geom_line(
      data = df1, 
      aes(
        x = months,
        y = peak, 
        color ="The Great Recession")) +
    geom_point(
      data = df1, 
      colour="#C9A5E0", 
      size = 1, 
      alpha = 0.8, 
      aes(x = months,y = peak)) +
    geom_line(
      data = df2, 
      aes(
        x = months,
        y = peak, 
        color ="COVID-19")) +
    scale_color_manual(values = colors, ) +
    geom_point(
      data = df2, 
      colour="#FC0202",
      alpha = 0.8, 
      size = 1, 
      shape = 17,
      aes(x = months,y = peak)) +
    scale_y_continuous(
      labels = function(x) format(x, scientific = FALSE),
      breaks = seq(0, 100, by = 5))+
    scale_x_continuous(
      breaks = seq(0, max(df1$months), by = 5))+
    labs(
      title = paste(industry," Business Cycles",sep=""),
      subtitle = paste("Dated by Peaks and Troughs\nNot Seasonaly Adjsuted",sep=""),
      x ="Number of Months Since Employment Peak", 
      y = "Peak in Employment = 100") +
    theme(text = element_text(colour = "#000000"),
          title = element_text(color = "#00587C"),
          axis.text = element_text(size = 12),
          panel.background = element_rect(fill = NA),
          plot.background = element_rect(fill = "#FFFFFF"),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.position = "top",
          panel.grid.major.y = element_line(colour = "#F0F0F0"),
          plot.title = element_text(
            hjust = 0.5,
            color = "#00587C", 
            size = 18,
            face="bold"),
          plot.subtitle = element_text(
            hjust = 0.5,
            color = "#00587C", 
            size = 16,
            face="bold"),
          legend.text=element_text(size=14),
          strip.text = element_text(size = 14)) +
    facet_wrap(vars(msa), ncol=2) 
  
  file_name <- paste(d,"/",industry,".png",sep="")  
  
  ggsave(recovery, filename = file_name, dpi = 300, type = 'cairo',
         width = 13, height = 8.5, units = 'in')
  
  print(file_name)
}

