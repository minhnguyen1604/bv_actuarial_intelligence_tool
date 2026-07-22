library(shiny)
library(shinydashboard)
library(dplyr)
library(htmltools)
library(plotly)
library(shinyjs)
library(readxl)
library(DT)
library(tidyr)
library(stringr)
library(stringi)
library(openxlsx)
library(rhandsontable)
library(later)
library(future)
library(lubridate)
library(htmlwidgets)
library(writexl)
# if (.Platform$OS.type == "windows") {
#   library(RDCOMClient)
#   # Code sử dụng COM ở đây
# }
# library(RDCOMClient)
library(promises)


#data <- readRDS("data.rds")
# Map Line -> icon
line_icons <- list(
  "Total"               = icon("chart-pie"),
  "Healthcare"          = icon("notes-medical"),
  "Travel"              = icon("plane-departure"),
  "Healthcare + Travel" = icon("hospital"),
  "Personal Accident"   = icon("user-shield"),
  "HC & PA & Travel"    = icon("suitcase-medical"),
  "Motor Vehicles"      = icon("car"),
  "Engineering"         = icon("tools"),
  "Fire and Misc."      = icon("fire"),
  "General Liability"   = icon("balance-scale"),
  "Cargo in transit"    = icon("ship"),
  "Hull & PI"           = icon("anchor"),
  "Aviation & Oil"      = icon("plane"),
  "Agriculture"         = icon("seedling")
)

# Định nghĩa thứ tự mong muốn
line_order <- c(
  "Total",
  "Healthcare",
  "Travel",
  "Healthcare + Travel",
  "Personal Accident",
  "HC & PA & Travel",
  "Motor Vehicles",
  "Engineering",
  "Fire and Misc.",
  "General Liability",
  "Cargo in transit",
  "Hull & PI",
  "Aviation & Oil",
  "Agriculture"
)

line_order2 <- c(
  "Motor Vehicles",
  "Healthcare + Travel",
  "Travel",
  "Healthcare",
  "Personal Accident",
  "HC & PA & Travel",
  "Cargo in transit",
  "Hull & PI",
  "Aviation & Oil",
  "Fire and Misc.",
  "Engineering",
  "General Liability",
  "Agriculture",
  "Total"
)
# Bảng mapping
line_map <- tribble(
  ~Line,                ~`Nghiệp vụ`,         ~`Nhóm NV`,
  "Motor Vehicles",     "Xe cơ giới",         "Nhóm XCG, YT, CN",
  "Healthcare + Travel","Y tế + Du lịch",     "Nhóm XCG, YT, CN",
  "Travel",             "Du lịch",            "Nhóm XCG, YT, CN",
  "Healthcare",         "Y tế",               "Nhóm XCG, YT, CN",
  "Personal Accident",  "Con người",          "Nhóm XCG, YT, CN",
  "HC & PA & Travel",   "YT & CN & DL",       "Nhóm XCG, YT, CN",
  "Cargo in transit",   "Hàng hóa",           "Nhóm Tàu, hàng",
  "Hull & PI",          "Thân tàu",           "Nhóm Tàu, hàng",
  "Aviation & Oil",     "DK & HK",            "Nhóm DKKH, NN",
  "Fire and Misc.",     "Tài sản",            "Nhóm TS, KT, TN",
  "Engineering",        "Kỹ thuật",           "Nhóm TS, KT, TN",
  "General Liability",  "Trách nhiệm",        "Nhóm TS, KT, TN",
  "Agriculture",        "Nông nghiệp",        "Nhóm DKKH, NN",
  "Total",              "Tổng",               "Tổng"
)

