library(tidyverse)

# Load data. Remember to set working directory.
gpu_df <- read_csv('gpu_df.csv')


# ------------------------------------------------------------------------------
# Begin Cleaning
# Remove rows of base graphics processor with no interface.
gp_remove <- gpu_df %>% filter(is.na(`Graphics Processor`)) # Find NAs
gpu_df <- gpu_df %>% filter(!Name %in% gp_remove$Name) # Remove NAs


# Coerce 'unknown' and other similar entries to NA
coerce_to_na <- 
  c('Unknown', 'unknown', 'Never Released', 'System Shared', 'System Dependent')

gpu_df <- gpu_df %>% 
  mutate(across(where(is.character), ~ replace(., . %in% coerce_to_na, NA)))
                  
# Coerce date formats to YYYY-MM-DD
gpu_df$`Release Date` <- 
  lubridate::parse_date_time(gpu_df$`Release Date`, c('mdy', 'my', 'y')) 


# -------------------------------------------------------------------------------
# Columns that need certain attention. 
# Remove units from row entries and place in column names.

# Memory Size Column
# Will need to covert entries with units from MB to GB, Divide by 1000
gpu_df <- rename(gpu_df, `Memory S  ize (GB)` = `Memory Size`) # Rename column.

gpu_df$`Memory Size (GB)` <- 
  gsub("[^0-9.-]", "", gpu_df$`Memory Size (GB)`) %>%       # Replace non digits.
  as.numeric()                                              # Change class type.

gpu_df <- gpu_df %>%                                        # Convert MB to GB
  mutate(`Memory Size (GB)` = case_when(`Memory Size (GB)`  > 100 ~ `Memory Size (GB)` / 1000, 
                                        TRUE ~ as.numeric(`Memory Size (GB)`)))

# FP32 (float) performance (TFLOPS) Column
gpu_df <- 
  gpu_df %>% rename(`FP32 (float) performance (TFLOPS)` = `FP32 (float) performance`)

# This could introduce incorrect entries in the tens decimal place.
gpu_df$`FP32 (float) performance (TFLOPS)` <- parse_number(gpu_df$`FP32 (float) performance (TFLOPS)`) %>% as.numeric()

gpu_df <- # Convert GFLOPS to TFLOPS
  gpu_df %>% mutate(`FP32 (float) performance (TFLOPS)` =
                      case_when(`FP32 (float) performance (TFLOPS)` > 100 ~ `FP32 (float) performance (TFLOPS)` / 1000,
                                TRUE ~ as.numeric(`FP32 (float) performance (TFLOPS)`)))


# L2 Cache Column
gpu_df <- rename(gpu_df, `L2 Cache (KB)` = `L2 Cache`)

gpu_df$`L2 Cache (KB)` <- 
  gsub("[^0-9.-]", "", gpu_df$`L2 Cache (KB)`) %>% as.numeric()

gpu_df <- gpu_df %>% # Covert MB to KB
  mutate(`L2 Cache (KB)` = case_when(`L2 Cache (KB)` < 100 ~ `L2 Cache (KB)` * 1000,
                                     TRUE ~ as.numeric(`L2 Cache (KB)`)))

# Memory Clock Column 
# Retain only the first set of digits in entry.
gpu_df$`Memory Clock` <- str_extract(gpu_df$`Memory Clock`, "^([0-9]+)") %>% as.numeric()


# Have to use a different method on Length, Width, and Height.
# Will need to split strings into mm and inches and into 2 different columns. 

# Length Column
#gpu_df$Length %>% separate(gpu_df$Length, c('Length(mm)', 'Length(in)'), ' mm ')


gpu_df <- 
gpu_df %>% mutate(`Length (mm)` = 
                    parse_number(gpu_df$Length) %>%  
                    as.numeric(), 
                  .before = Length)

gpu_df <- 
gpu_df %>% mutate(`Length (in)` = 
                    str_extract(gpu_df$Length, '(?<!mm )\\D\\d+\\.\\d+') %>% 
                    parse_number() %>% 
                    as.numeric(), 
                  .before= Length)


# Width Column
#gpu_df$Width %>% separate(Width, c('Width(mm)', 'Width(in)'), ' mm ')


gpu_df <- 
  gpu_df %>% mutate(`Width (mm)` = 
                      parse_number(gpu_df$Width) %>%  
                      as.numeric(), 
                    .before = Width)

gpu_df <- 
  gpu_df %>% mutate(`Width (in)` = 
                      str_extract(gpu_df$Width, '(?<!mm )\\D\\d+\\.\\d+') %>% 
                      parse_number() %>% 
                      as.numeric(), 
                    .before= Width)


# Height Column
#gpu_df$Height %>% separate(Height, c('Height(mm)', 'Height(in)'), ' mm ')


gpu_df <- 
  gpu_df %>% mutate(`Height (mm)` = 
                      parse_number(gpu_df$Height) %>%  
                      as.numeric(), 
                    .before = Height)

gpu_df <- 
  gpu_df %>% mutate(`Height (in)` = 
                      str_extract(gpu_df$Height, '(?<!mm )\\D\\d+\\.\\d+') %>% 
                      parse_number() %>% 
                      as.numeric(), 
                    .before= Height)



# Remove unnecessary columns.
cols_remove <- c('Reviews', 
                 'Current Price', 
                 'Codename', 
                 'Length',
                 'Width', 
                 'Height')

gpu_df <- gpu_df %>% select(!cols_remove)
# -------------------------------------------------------------------------------
# The rest that don't need unit conversion will go here.
# Rename old columns with new names, and strip unit measurement from all entries.
old_col_names <- c('Bus Width', 
                   'Process Size', 
                   'Transistors', 
                   'Die Size',
                   'Base Clock',
                   'Boost Clock',
                   'Memory Bus',
                   'Bandwidth',
                   'L0 Cache',
                   'L1 Cache',
                   'L3 Cache',
                   'Pixel Rate',
                   'Texture Rate',
                   'TDP',
                   'Suggested PSU',
                   'Launch Price',
                   'Game Clock',
                   'GPU Clock',
                   'Memory Clock'
                   )

new_col_names <- c('Bus Width (bit)', 
                   'Process Size (nm)', 
                   'Transistors (millions)', 
                   'Die Size (mm²)',
                   'Base Clock (MHz)',
                   'Boost Clock (MHz)',
                   'Memory Bus (bit)',
                   'Bandwidth (GB/s)',
                   'L0 Cache (KB per WGP)',
                   'L1 Cache (KB)',
                   'L3 Cache (MB)',
                   'Pixel Rate (GPixel/s)',
                   'Texture Rate (GTexel/s)',
                   'TDP (W)',
                   'Suggested PSU (W)',
                   'Launch Price (USD)',
                   'Game Clock (MHz)',
                   'GPU Clock (MHz)',
                   'Memory Clock (MHz)'
                   )
# Renames all old columns to new names.
gpu_df <- 
gpu_df %>% rename_at(vars(old_col_names), ~ new_col_names) 


 # Gets rid of units of measurement across all new col names.
gpu_df <- 
gpu_df %>% 
  mutate(across(.cols = new_col_names,
                .fns = ~ gsub(pattern = '[^0-9]',  replacement = '', .) %>% 
                  as.numeric()
                )
         )


# Change class of columns that should be numeric.
gpu_df$TMUs <- gpu_df$TMUs %>% as.numeric()
gpu_df$ROPs <- gpu_df$ROPs %>% as.numeric()
gpu_df$`Shading Units` <- gpu_df$`Shading Units` %>% as.numeric()
gpu_df$`Compute Units` <- gpu_df$`Compute Units` %>% as.numeric()
gpu_df$`RT Cores` <- gpu_df$`RT Cores` %>% as.numeric()

# Add Release Year Column. 
gpu_df <- 
gpu_df %>% mutate(Year = str_sub(gpu_df$`Release Date`, 1, 4 ) %>% as.numeric(), .before = Cores)


write_csv(gpu_df, 'E:/Documents/R Scripts/GPU_Analysis/clean_gpu_df.csv')


















