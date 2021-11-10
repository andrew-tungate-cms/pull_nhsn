


library(here)
library(data.table)
library(readxl)

# Importing and prepping data ---

# Importing data
dt = fread(here("data", "COVID-19 Nursing Home Data 08.22.2021.csv"), 
           check.names = TRUE)
setnames(dt, gsub("\\.+", "_", tolower(names(dt))))

# Renaming and select columns
col_names = read_excel(here("data", "column_name_crosswalk.xlsx"))
setDT(col_names)[, new_names := fcoalesce(new_names, old_names)]
setnames(dt, col_names$new_names)
keep_cols = col_names[keep == 1]$new_names

dt = dt[, ..keep_cols] # selecting columns

# Cleaning up character columns with empty strings
idx <- which(sapply(dt, is.character))
for(j in idx) {
  set(dt, i = which(dt[[j]] == ""), j = j, value = NA_character_)
  set(dt, i = which(dt[[j]] == "Y"), j = j, value = 1L)
  set(dt, i = which(dt[[j]] == "N"), j = j, value = 0L)
  set(dt, j = j, value = type.convert(dt[[j]], as.is = TRUE))
}


# Creating a monthly variable to group by
dt[, month_start := as.IDate(sub("(\\d{2}).*(\\d{2})", "20\\2-\\1-01", week_ending), format = "%Y-%m-%d")]
dt[, .(week_ending, month_start)]
setcolorder(dt, c("ccn", "week_ending", "month_start", setdiff(names(dt), c("ccn", "week_ending", "month_start"))))


# Create CCN / Month combo ---- 
out = dt[, lapply(.SD, mean, na.rm = TRUE)
         , .(ccn, month_start)
         , .SDcols = is.numeric]


for(j in seq_len(length(out))) {
  set(out, i = which(is.na(out[[j]])), j = j, value = NA)
}

# Ready for export
out




# Checking on pct complete ----
pct_complete_all = dt[, lapply(.SD, function(x) mean(!is.na(x)))]

pct_complete_julyon = dt[month_start >= as.Date("2021-07-01"), lapply(.SD, function(x) mean(!is.na(x)))]
pct_complete_julyon = rbind(as.data.frame(pct_complete_julyon),
                            col_names$column_desription[match(names(pct_complete_julyon), col_names$new_names)],
                            col_names$original_name[match(names(pct_complete_julyon), col_names$new_names)])

pct_complete = rbindlist(list(
  all = pct_complete_all,
  july_on = pct_complete_julyon
), id = "type")



writexl::write_xlsx(pct_complete, here("output", "pct_complete.xlsx"))





















names(dt)

test = dt[, .(n_beds = median(n_beds),
              n_beds_occupied_median = median(n_beds_occupied),
              n_beds_occupied_mean = mean(n_beds_occupied),
              res_total_confirmed_cov19 = mean(res_total_confirmed_cov19, na.rm = TRUE)), .(ccn, month_start)]
test









