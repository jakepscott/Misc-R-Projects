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
  #Get the percent year change, format it nicely for a table
  mutate(year_change = (value-lag(value,12))/lag(value, 12) %>% formattable::percent(digits = 3),
         percent_label = scales::percent(round(year_change, 2)),
         `series name` = title) %>% 
  drop_na() %>% 
  ungroup() %>% 
  janitor::clean_names() %>% 
  select(series_name, date, year_change) %>% 
  ggplot(aes(date,year_change, color = series_name)) +
  geom_hline(yintercept = 0) +
  geom_line() 
  
