#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This comes largely from Jonathan Regenstein
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Libs --------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(tidyquant)
library(timetk)
library(formattable)
library(scales)
library(fredr)
library(gt)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Data --------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cpi_components_manual <- 
  tribble(
    ~symbol, ~title,
    "CPILFESL", "Core",
    "CPIAUCSL", "All Items",
    "CPIAPPSL", "Apparel",
    "CPIMEDSL", "Medical Care",
    "CPIHOSSL", "Housing",
    "CPIFABSL", "Food and Beverage", 
    "CUSR0000SAC", "Commodities",
    "CUSR0000SAS", "Services", 
    "CPITRNSL", "Transportation",
    "CUUR0000SAE1", "Education",
    "CPIEDUSL", "Education and Comms",
    "CPIRECSL", "Recreation"
  )

cpi_yoy <- 
  cpi_components_manual %>% 
  pull(symbol) %>% 
  tq_get(get = "economic.data", from =  "2019-01-01") 


cpi_for_chart <- 
  cpi_yoy %>% 
  left_join(
    cpi_components_manual, 
    by = "symbol"
  ) %>% 
  select(title, 
         fred_code = symbol, 
         date, 
         value = price) %>% 
  group_by(fred_code) %>%
  mutate(year_change = value/lag(value, 12) - 1  %>% formattable::percent(digits = 3),
         percent_label = scales::percent(round(year_change, 2)),
         `series name` = title) %>% 
  drop_na() %>% 
  ungroup() %>% 
  select( `series name`, date, year_change) %>% 
  pivot_wider(names_from =  `series name`, values_from = year_change)  %>% 
  mutate(date = as.yearmon(date, "%Y %m")) %>% 
  arrange(desc(date))

col_vars <- 
  colnames(cpi_for_chart[,-1]) 

cpi_for_chart %>% 
  filter(date >= "2021-01-01") %>% 
  gt(rowname_col = "date") %>% 
  data_color(
    columns = vars(col_vars),
    colors = scales::col_numeric(
      colorspace::diverge_hcl(n = 20,  palette = "Blue-Red 3") %>% rev(), 
      domain = c(-.2, .23)))  %>% 
  tab_source_note(html("Data from <a href='https://fred.stlouisfed.org/'>St. Louis Fed - FRED</a>")) %>% 
  tab_header("YoY CPI Component Changes") %>% 
  tab_options(
    data_row.padding = px(2)
  ) %>% 
  cols_width(
    everything() ~ px(80)
  )