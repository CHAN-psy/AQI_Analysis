# app.R
library(shiny)
library(bs4Dash)
library(ggplot2)
library(plotly)
library(forcats)
library(lubridate)
library(dtplyr)
library(DBI)
library(RSQLite)

# 工具函數
purrr::walk(list.files("utils", pattern = "\\.R$", full.names = TRUE), source)

# Module（必須在 ui.R 之前）
source("R/modules/plotly_module.R")

# UI & Server
source("R/ui.R")
source("R/server.R")

options(shiny.reactlog = TRUE)
shinyApp(ui, server)