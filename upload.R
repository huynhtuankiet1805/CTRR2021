#Khai báo thư viện
  library(xlsx)
  library(readxl)
  library(readr)
  library(ggplot2)
  library(dplyr)
#B1: upload dữ liệu từ file xlsx
file_GK <- read_xlsx("201_CO1007.xlsx",sheet = "GK", range = "A5:AC162")
file_CK <- read_xlsx("201_CO1007.xlsx",sheet = "CK", range = "A5:AP168")
