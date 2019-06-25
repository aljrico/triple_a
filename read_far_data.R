list_of_feathers_np <- list.files(path = 'data_far/data', pattern="*.feather")
list_of_feathers_p <- list.files(path = 'data_far/prices', pattern="*.feather")

fins <- lapply(paste0('data_far/data/', list_of_feathers_np), read_feather) %>% 
  reduce(rbind) %>% 
  data.table()

all_prices <- lapply(paste0('data_far/prices/', list_of_feathers_p), read_feather) %>% 
  reduce(rbind) %>% 
  data.table()

all_prices %>% 
  mutate(date = date %>% as.Date() %>% ymd()) %>%
  select(date, name, `Adj Close`) %>% 
  filter(date == '2010-01-05') %>% 
  rename(price = `Adj Close`) %>% 
  data.table() %>% 
  left_join(
    uncomplete_data %>% 
      mutate(date = date %>% as.Date() %>% ymd()) %>% 
      filter(date == '2010-01-04') %>% 
      data.table()
  ) %>% 
  data.table()


stocks <- unique(all_prices$name)
tidy_merged <- tibble()
merged <- all_prices %>% 
  mutate(date = date %>% as.Date() %>% ymd()) %>% 
  data.table() %>% 
  left_join(
    fins %>% 
      mutate(date = date %>% as.Date() %>% ymd()) %>% 
      data.table()
    )


for(s in stocks){
  df <- merged %>% filter(name == s)
  df <- na.locf(df) %>%
    na.omit() %>%
    distinct() %>%
    as_tibble()
  tidy_merged <- tidy_merged %>% rbind(tidy_merged,df)
}

tidy_merged$date <- as.Date(tidy_merged$date)
tidy_merged$name <- as.factor(tidy_merged$name)
cols <- colnames(tidy_merged)
for(c in cols) if(is.character(tidy_merged[[c]])) tidy_merged[[c]] <- as.numeric(tidy_merged[[c]])

tidy_merged <- tidy_merged %>%  distinct()

tidy_merged %>% fwrite('all_data')
  

