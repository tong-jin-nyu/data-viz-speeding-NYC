### A3SR Data Visualization Competition

# Created on: 100220
# Modified on: 100220
# Author: Tong Jin

# Project Name: 
#   Surge in speeding under COVID-19, compare NYC's traffic speed during labor 
#   day weekend, 2019 vs. 2020
# Data set: 
#   Real-Time Traffic Speed Data (API/JSON)
# Source: 
#   New York City Open Data
#   https://data.cityofnewyork.us/Transportation/DOT-Traffic-Speeds-NBE/i4gi-tjb9
# Agency: 
#   City of New York Department of Transportation (DOT)

# Caption: 
#   Drivers in New York City slammed on their gas pedals during COVID-19 pandemic. Will they slow down after quarantine? Our study compared the traffic speed data during 2020 Labor day weekend in five boroughs with last year's. Results reveal a trend of surge in speeding after quarantine. Despite that the city lowered its speed limit to 25 mph for urban and 45 for highway (marked with dotted lines), New Yorkers in all five boroughs drive above the speed limit. Brooklyn and Bronx are leading the surge. Even Manhattan drivers are putting more efforts in fighting traffic.

# Dependencies
# ------------------------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(plyr)
library(httr)
library(jsonlite)
library(rjson)
library(RCurl)
# ------------------------------------------------------------------------------
# Main script

# Import the data set ----------------------------------------------------------
# Labor's day 2020 -------------------------------------------------------------
base_url <- "https://data.cityofnewyork.us/resource/i4gi-tjb9.json?"
full_url <- paste0(
  base_url,
  "$where=data_as_of between '2020-09-05T00:00:00.000' and '2020-09-08T00:00:00.000'",
  "&", "$select=speed,data_as_of,borough",
  "&", "$limit=300000"
)
full_url <- URLencode(full_url)
# Import the data from API and convert to data frame
dat2020 <- fromJSON(getURL(full_url))
dat2020 <- ldply(dat2020, data.frame)

# Summary: data 2020
# Number of observations: 97,927
# Numebr of variables: 3

# Labor's day 2019 -------------------------------------------------------------
base_url <- "https://data.cityofnewyork.us/resource/i4gi-tjb9.json?"
full_url <- paste0(
  base_url,
  "$where=data_as_of between '2019-08-31T00:00:00.000' and '2019-09-03T00:00:00.000'",
  "&", "$select=speed,data_as_of,borough",
  "&", "$limit=300000"
)
full_url <- URLencode(full_url)
# Import the data from API and convert to data frame
dat2019 <- fromJSON(getURL(full_url))
dat2019 <- ldply(dat2019, data.frame)

# Summary: data 2019
# Number of observations: 116,639
# Numebr of variables: 3

# Data cleaning ----------------------------------------------------------------
# data 2020
data2020 <- dat2020
for (i in 1:nrow(data2020)) {
  if (data2020$borough[i] == "Staten island") {
    data2020$borough[i] <- "Staten Island"
  }
}
data2020$speed <- as.numeric(as.character(data2020$speed))

# data 2019
data2019 <- dat2019
for (i in 1:nrow(data2019)) {
  if (data2019$borough[i] == "Staten island") {
    data2019$borough[i] <- "Staten Island"
  }
}
data2019$speed <- as.numeric(as.character(data2019$speed))

# Data Processing --------------------------------------------------------------
data2020$year <- "2020"
data2019$year <- "2019"
data_viz <- rbind(data2019, data2020)

# Color Palette ----------------------------------------------------------------
blue <- "#81b9db"
yellow <- "#ffb74a"
gray <- "#f0eee6"
dark <- "#292f36"

# Functions --------------------------------------------------------------------
theme_fancy_gray <- function() {
  theme_grey() + 
    theme(
      panel.background = element_rect(
        fill = gray,
        color = gray
      ),
      plot.title = element_text(
        family = "Helvetica",
        size = 14,
        face = "bold",
        margin = margin(t = 5, b = 5)     # Margins: top, buttom
      ),
      plot.subtitle = element_text(
        family = "Helvetica",
        size = 12,
        margin = margin(b = 10)
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.text = element_text(
        family = "Helvetica",
        size = 12
      ),
      strip.text = element_text(
        family = "Helvetica",
        size = 11, 
        face = "bold"
      ),
      axis.text.x = element_text(
        family = "Helvetica",
        size = 11,
        face = "bold"
      ),
      axis.text.y = element_text(           # Speed value
        family = "Helvetica",
        size = 10
      ),
      axis.title.y = element_text(          # "Speed"
        family = "Helvetica",
        size = 11,
        margin = margin(r = 5)
      ),
      legend.position = c(.5, .95),
      legend.direction = "horizontal",
      legend.key = element_blank()
    )
}
# Data visualization -----------------------------------------------------------
ggplot(
  data = data_viz, 
  aes(
    x = borough, 
    y = speed,
    fill = year
  )
) + 
  geom_boxplot(
    stat = "boxplot",
    position = "dodge2",
    outlier.shape = NA,
    na.rm = TRUE,
    color = dark
  ) + 
  scale_fill_manual(values = c(blue, yellow)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 80, by = 15)) +
  labs(
    title = "Surge in Speeding under COVID-19",
    subtitle = "Compare NYC's Traffic Speed during Labor Day Weekend, 2019 vs. 2020",
    fill = "",
    x = "",
    y = "Speed"
  ) +
  theme_fancy_gray() + 
  # Annotate
  geom_hline(
    aes(yintercept = 25), 
    color = dark,
    size = .3,
    linetype = "dashed"
  ) + 
  geom_hline(
    aes(yintercept = 45), 
    color = dark,
    size = .3,
    linetype = "dashed"
  )

# Output -----------------------------------------------------------------------
# 1:1
ggsave(
filename = "ratio_1-1_Jin_Tong_A3SR_Stu_Viz_Competition.jpg",
plot = p,
device = "jpg",
width = 6,
height = 6,
units = "in",
dpi = 300
)
# 4:3
ggsave(
filename = "ratio_4-3_Jin_Tong_A3SR_Stu_Viz_Competition.jpg",
plot = p,
device = "jpg",
width = 8,
height = 6,
units = "in",
dpi = 300
)
# 5:4
ggsave(
filename = "ratio_5-4_Jin_Tong_A3SR_Stu_Viz_Competition.jpg",
plot = p,
device = "jpg",
width = 10,
height = 8,
units = "in",
dpi = 300
)
# 8:5
ggsave(
filename = "ratio_8-5_Jin_Tong_A3SR_Stu_Viz_Competition.jpg",
plot = p,
device = "jpg",
width = 8,
height = 5,
units = "in",
dpi = 300
)
# 16:9
ggsave(
filename = "ratio_16-9_Jin_Tong_A3SR_Stu_Viz_Competition.jpg",
plot = p,
device = "jpg",
width = 16,
height = 9,
units = "in",
dpi = 300
)
