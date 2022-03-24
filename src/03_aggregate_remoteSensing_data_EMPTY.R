## Purpose of this script:
## 1. Download raw Google Earth Engine (GEE) data (see: "get_remote_data.R" in src folder for access to GEE algorithm)
## 2. Aggregate data into a single data frame

#load libraries
#load libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(plotly)
library(zoo)
library(here)
library(janitor)

