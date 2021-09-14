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
library(here)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Data --------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Make a key table connecting the technical name and human readable name
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

# Use tq_get to pull data for the above series
#These are the CPI indeces for the above categories
cpi_yoy <- 
  cpi_components_manual %>% 
  pull(symbol) %>% 
  tq_get(get = "economic.data", from =  "2019-01-01") 

# Put the CPI data into a nice format
cpi_for_chart  <- 
  #Connect series names to the human readable names
  cpi_yoy %>% 
  left_join(
    cpi_components_manual, 
    by = "symbol"
  ) %>% 
  #Grab just the cols we want
  select(title, 
         fred_code = symbol, 
         date, 
         value = price) %>% 
  #Look at each category
  group_by(fred_code) %>%
  #Get the percent year change, format it nicely for a table
  mutate(year_change = (value-lag(value,12))/lag(value, 12) %>% formattable::percent(digits = 3),
         percent_label = scales::percent(round(year_change, 2)),
         `series name` = title) %>% 
  #Drop Nas and select cols of interest
  drop_na() %>% 
  ungroup() %>% 
  select( `series name`, date, year_change) %>% 
  # Use zoo's as.yearmon the make the date into year month
  mutate(date = as.yearmon(date, "%Y %m")) %>% 
  #arrange by date
  arrange(date) %>% 
  filter(date >= "2021-01-01") %>% 
  pivot_wider(names_from = 'date', values_from = "year_change") %>% 
  rename("Component"=`series name`)

#For the columns, use all but the category column name
col_vars <- 
  colnames(cpi_for_chart[,-1]) 

(table <- cpi_for_chart %>% 
  gt(rowname_col = "component") %>% 
  data_color(
    columns = c(col_vars),
    colors = scales::col_numeric(
      colorspace::diverge_hcl(n = 20,  palette = "Blue-Red 3"), 
      domain = c(-.2, .23)))  %>% 
  fmt_percent(columns = c(col_vars_JS),
             rows = everything(),
             decimals = 2) %>% 
  tab_source_note(html("Data from <a href='https://fred.stlouisfed.org/'>St. Louis Fed - FRED</a>")) %>% 
  tab_header("YoY CPI Component Changes") %>% 
  tab_options(
    data_row.padding = px(2)
  ))
  
gtsave(table,here("CPI_Inflation_Tracking/figures/CPI_components_table.png"))
