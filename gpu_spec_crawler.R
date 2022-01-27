library(tidyverse)
library(rvest)
library(Rcrawler)

# Load Data 

gpu_url <- read_csv('gpu_url.csv')
gpu_url <- gpu_url %>% unlist() %>% as_tibble()

# Crawl through webpages and extract wanted information. -----------------------

# Test subset
# gpu_subset <- gpu_url[1:20]

gpu_specs <- purrr::map(gpu_url[1:20], function(x){
  
    Z <- read_html(x)
  
    Sys.sleep(35) # Be nice and don't request too quickly. :)
    
# Parse HTML for desired elements.
    gpu_name <- Z %>% html_element('h1') %>% html_text2() %>% as_tibble()
    print(gpu_name)
    gpu_spec_field <- Z %>% html_elements('dt') %>% html_text2() %>%  as_tibble()
    
    gpu_spec_record <- Z %>%  html_elements('dd') %>% html_text2() %>% as_tibble()
    #print(gpu_spec_record)
    
    gpu_df <- cbind(gpu_spec_field, gpu_spec_record)
    gpu_df <- gpu_df %>% janitor::clean_names() %>% distinct()
    
    
    gpu_df <- gpu_df %>% pivot_wider(names_from = value, values_from = value_2)
    
    gpu_df <- gpu_df %>% mutate(
      Name = gpu_name,
      .before = everything()
    )
    
})

# Convert data to dataframe and perform some cleaning.
gpu_spec_df <- gpu_specs %>% as_tibble()
gpu_spec_df <- pivot_longer(gpu_spec_df, cols = everything())


gpu_spec_df7 <- gpu_spec_df7 %>%
  filter(!is.na(value$Name))

# If errors occur due to list and character conflict
# use this, and rerun pivot_longer
#gpu_spec_df11$value227<- toString(gpu_spec_df11$value227) %>% as_tibble()



complete_gpu_spec_df <-  bind_rows(gpu_spec_df)


complete_gpu_spec_df <- complete_gpu_spec_df %>% select(!(c('name')))

# Make copy of data incase overwritten.
keep <- complete_gpu_spec_df


complete_gpu_spec_df <- complete_gpu_spec_df %>% 
  filter(!is.na(value$Name))


complete_gpu_spec_df <- complete_gpu_spec_df$value %>% as_tibble()

# Save as CSV
write_csv(complete_gpu_spec_df, 'E:/Documents/R Scripts/GPU_Analysis/gpu_df.csv')


