
# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(tidyquant)
library(janitor)

# Load Data --------------------------------------------------

#Expectation data
url <- "https://www.newyorkfed.org/medialibrary/interactives/sce/sce/downloads/data/frbny-sce-data.xlsx"
destfile <- here("CPI_Inflation_Tracking/frbny_sce_data.xlsx")
curl <- curl::curl_download(url, destfile)

frbny_sce_data <- 
  readxl::read_excel(destfile, sheet = 4, skip = 3) %>% 
  rename(date = 1) 

#CPI components data
#Connect the series IDs and human readable names
cpi_pce_components_manual <- 
  tribble(
    ~symbol, ~title,
    "CPILFESL", "Core CPI",
    "CPIAUCSL", "CPI All Items",
    "PCE", "PCE All Items",
    "PCEPILFE", "Core PCE"
  )

# Use tq_get to pull data for the above series
#These are the CPI indeces for the above categories
cpi_pce_yoy <- 
  cpi_pce_components_manual %>% 
  pull(symbol) %>% 
  tq_get(get = "economic.data", from =  "2012-06-01") 

  # Put the CPI data into a nice format
cpi_pce  <- 
  #Connect series names to the human readable names
  cpi_pce_yoy %>% 
  left_join(
    cpi_pce_components_manual, 
    by = "symbol"
  ) 


# Clean Data --------------------------------------------------------------
# Make the date column and clean names for frbny expectations
frbny_sce_data <- readxl::read_excel(destfile, sheet = 4, skip = 3) %>% 
  rename(date = 1) %>% 
  mutate(date = lubridate::parse_date_time((date), orders = "%Y%m")) %>% 
  janitor::clean_names()

#Get CPI inflation rate
cpi_pce <- cpi_pce %>% 
  #Look at each category
  group_by(symbol) %>%
  #Get the percent year change, format it nicely for a table
  mutate(per_change_annual = (price-lag(price,12))/lag(price, 12)*100) %>%
  rename(component = title) %>% #Drop Nas and select cols of interest
  na.omit() %>% 
  ungroup() %>% 
  select(component, date, per_change_annual)

cpi_pce <- cpi_pce %>% 
  pivot_wider(names_from = "component", values_from = "per_change_annual") %>% 
  clean_names()

#Join frb and cpi
joined <- frbny_sce_data %>% 
  left_join(cpi_pce)



# Plot 3-year exp with uncertainty ----------------------------------------
frbny_sce_data %>% 
  ggplot(aes(date)) +
  geom_ribbon(aes(ymin = x25th_percentile_three_year_ahead_expected_inflation_rate,
                  ymax = x75th_percentile_three_year_ahead_expected_inflation_rate), 
              color = "grey70", 
              alpha = 0.5) +
  geom_line(aes(y = median_three_year_ahead_expected_inflation_rate),
            color = "blue",
            lwd = 1) +
  theme_minimal()


# Plot 3 versus 1 year exp ----------------------------------------------------
frbny_sce_data %>% 
  select(date, 
         median_three_year_ahead_expected_inflation_rate,
         median_one_year_ahead_expected_inflation_rate) %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(date, y = value, color = name)) +
  geom_line() +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal()


# Plot 3 vs 1 year vs core cpi --------------------------------------------
joined %>% 
  select(date, 
         median_three_year_ahead_expected_inflation_rate,
         median_one_year_ahead_expected_inflation_rate, 
         core_cpi:core_pce, -pce_all_items) %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(date, y = value, color = name)) +
  geom_line() +
  #scale_color_manual(values = c("red", "blue", "green")) +
  theme_minimal()

