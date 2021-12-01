

library(httr)
library(dplyr)

base_url = "https://data.cdc.gov/resource/n8mc-b4w4.json?$query="

fips = unique(tidycensus::fips_codes$state_code)
fips = split(fips, rep(1:6, each = 10))
fips = sapply(fips, function(x) paste0("state_fips_code = '", x, "'", collapse = " OR "))


query = paste0("
SELECT 
  case_month
  , county_fips_code
  , count(hosp_yn) AS hosp_yes
WHERE
  hosp_yn = 'Yes'
  AND (", fips, ") ", " 
GROUP BY
  case_month
  ,county_fips_code
LIMIT 
  50000
")
query = gsub("\\s+", " ", query)
query = gsub("\\s+,", ",", trimws(query))

queries = paste0(base_url, query)


query_result = vector("list", length(queries))

for(i in seq_along(queries)) {
  query_text = httr::GET(queries[i], content = "text")
  query_result[[i]] = bind_rows(content(query_text))
  print(paste0(i, " out of ", length(queries)))
}

out = bind_rows(query_result)












