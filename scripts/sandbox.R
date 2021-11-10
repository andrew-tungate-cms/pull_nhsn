


library(here)
library(httr)
library(dplyr)

api_req = GET("https://data.cms.gov/data-api/v1/dataset-type")

nhsn_id = content(api_req) %>%
  bind_rows() %>%
  filter(name == "COVID-19 Nursing Home Data") %>%
  select(name, last_updated, slug, latest_version_uuid)

nhsn_id


base_url = "https://data.cdc.gov/resource/n8mc-b4w4.json?$query="

fips = unique(tidycensus::fips_codes$state_code)
fips = split(fips, rep(1:6, each = 10))
fips = sapply(fips, function(x) paste0("state_fips_code = '", x, "'", collapse = "\n  OR   "))

# todo add fips state code filtering. start iwth one, go from there. 
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
  , county_fips_code
LIMIT 
  50000
")

cat(query[1])

query = gsub("\\s+", " ", query)

queries = paste0(base_url, query)

l_out = vector("list", length(queries))


for(i in seq_along(l_out)) {
  
  if(i > 1) {
    Sys.sleep(3L) # trying to be a good citizen
  }
  json_text = httr::GET(queries[i], content = "text")
  
  l_out[[i]] = bind_rows(content(json_text))
  print(paste0(i, "/6; sleeping 3 seconds"))

}

out = bind_rows(l_out)
out[-2] = type.convert(out[-2], as.is = TRUE)
out$case_month = as.Date(paste0(out$case_month, "-01"))
out





























