library(tidyverse)
library(DataExplorer)

# Load data.
gpu_df <- read_csv('clean_gpu_df.csv')

# Add Release Year Column. 
gpu_df <- 
  gpu_df %>% mutate(Year = str_sub(gpu_df$`Release Date`, 1, 4 ) %>% as.numeric(), .before = Cores)

# Remove columns that contain over 90% NAs. 
gpu_df <- gpu_df[1:65]


# Nvidia and AMD consumer cards since 2016 
nvidia_series <- c('GeForce 10', 'GeForce 16', 'GeForce 20', 'GeForce 30')
amd_series <- c("Navi\n(RX 5000)", "Navi II\n(RX 6000)", "Radeon Pro\n(Navi II Series)", "Navi III\n(RX 7000)")


# Compare Nvidia and AMD GPUs series of consumer cards 
geforce_navi_gpu_df <- gpu_df %>% filter(Generation %in% c(nvidia_series, amd_series))



# Take some initial looks at the data and 
gpu_df %>% DataExplorer::introduce()
gpu_df %>% DataExplorer::plot_missing()


# Order by launch price.
geforce_navi_gpu_df %>% select(Name, `Launch Price (USD)`, Year) %>% arrange(desc(`Launch Price (USD)`))




# Create line graph of change in gpu specs over the years.

variables <- c('Cores', 
               'Memory Size (GB)', 
               'Process Size (nm)', 
               'Base Clock (MHz)', 
               'Boost Clock (MHz)', 
               'TDP (W)')


geforce_sub_wide <- 
geforce_navi_gpu_df %>% select(Year, c(variables))

# Converts data to long format with Year as the base column.
geforce_long <- geforce_sub_wide %>% reshape2::melt(id = 'Year') 



ggplot(geforce_long,
       aes(x = Year,
           y = value,
           color = variable)) +
  geom_line()



ggplot(geforce_navi_gpu_df,
       aes(x = Year,
           y = `Base Clock (MHz)`,
           #label = Name
           )) +
  geom_boxplot()



geforce_navi_gpu_df %>% 
ggplot(aes(x = Year,
           y = `Base Clock (MHz)`,
           fill = Year,
           group = Year)) +
  geom_violin()

geforce_navi_gpu_df %>% 
  ggplot(aes(x = Year,
             y = `Base Clock (MHz)`,
             fill = Year,
             group = Year)) +
  geom_boxplot()



geforce_navi_gpu_df %>% 
  ggplot(aes(x = Year,
             y = Cores,
             fill = Year,
             group = Year)) +
  geom_violin()









