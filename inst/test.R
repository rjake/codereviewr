# this scipt is only used to test the code_to_df function

library(tidyverse)

raw_data <- mpg

df <-
  raw_data %>% 
  group_by(manufacturer) %>% 
  summarise(
    n_distinct_model = n_distinct(model)
  ) %>% 
  ungroup() %>% 
  mutate(manufacturer = fct_reorder(manufacturer, n_distinct_model))
  
df$test <- FALSE

df %>% 
  ggplot(aes(manufacturer, n_distinct_model)) +
  geom_col() +
  coord_flip()

for(i in 1:5) {
  x <- paste(i, Sys.time())
  
  print(x)
  
  Sys.sleep(0.5)
}

