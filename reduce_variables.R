reduce_variables <- function(df, size = 0.5){
  variables <- df %>% select(-target) %>% colnames()
  distances <- c()
  for(i in seq_along(variables)){
    x <- df[target == 0,][[variables[[i]]]]
    y <- df[target == 1,][[variables[[i]]]]
    distances[[i]] <- ks.test(x,y)$statistic[[1]]
  }
  
  new_l <- (length(distances)*size) %>% floor()
  
  important_vars <- tibble(distances) %>% 
    mutate(n = 1:n()) %>% 
    arrange(desc(distances)) %>% 
    top_n(new_l, distances) %>% 
    .$n
  
  new_vars <- paste0('var_', important_vars) %>% c('target')
  
  df %>% select(new_vars) %>% return()
}
