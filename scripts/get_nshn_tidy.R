

library(here)
library(dplyr)
library(readxl)


#### Importing and prepping data ---

# Importing data ----
df = data.table::fread(here("data", "COVID-19 Nursing Home Data 10.31.2021.csv"),
                       check.names = TRUE)
df = readr::type_convert(df)
df = as_tibble(df)


# Cleaning up column names ----
names(df) = gsub("\\.+", "_", tolower(names(df)))

# Reading excel names doc and creating new_name column (sas compatible names)
col_names = read_excel(here("data", "column_name_crosswalk.xlsx")) %>% 
  mutate(new_names = coalesce(new_names, old_names)) 

# Renaming dataframe, grabbing cols to keep from excel, selecting columns
names(df) = col_names$new_names

keep_cols = col_names %>% 
  filter(keep == 1) %>%
  pull(new_names) 

df = df %>%
  select(keep_cols)


# Creating / editing needed variables ----

# Creating a monthly variable to group by

df = df %>%
  mutate(month_start = as.Date(sub("(\\d{2}).*(\\d{2})", "20\\2-\\1-01", week_ending), format = "%Y-%m-%d")) %>%
  select(ccn, week_ending, month_start, everything()) # reordering



# Checking on pct complete ----

is.na(c(1:10, NA))



prop_nonmiss = function(x) {
  mean(!is.na(x))
}

pct_complete = data.frame(col = names(df))
pct_complete$pct_all = round(sapply(df, prop_nonmiss), 5)
pct_complete$july_on = round(sapply(df[df$month_start > as.Date("2021-07-01"), ], prop_nonmiss), 3)
pct_complete$september_on = round(sapply(df[df$month_start > as.Date("2021-09-01"), ], prop_nonmiss), 3)


# Appending original names and desctiptions
pct_complete$original_name = col_names$original_name[match(pct_complete$col, col_names$new_names)]
pct_complete$description = col_names$column_desription[match(pct_complete$col, col_names$new_names)]


writexl::write_xlsx(pct_complete, here("output", "pct_complete.xlsx"))



# Aggregating data ----

out = df %>%
  group_by(ccn, month_start) %>%
  summarise(res_wk_confirmed_cov19 = sum(res_wk_confirmed_cov19))



out %>%
  arrange(ccn, month_start) %>%
  group_by(ccn) %>%
  summarise(month_start,
            res_wk_confirmed_cov19,
            z = scale(res_wk_confirmed_cov19)[, 1],
            sum_nonzero = sum(res_wk_confirmed_cov19 != 0))


