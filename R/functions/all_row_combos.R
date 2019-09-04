library(purrr)

### Custom function to create df of all row combinations between 2 dfs
# based on cbind, but outputs a row for every row combo between dfs
# used in compare_hh_rosters() and other code
all_row_combos <- function(df1, df2){
  bind_rows(purrr::map2(split(df1, row.names(df1)), list(rep(df2, nrow(df1))), cbind, row.names = NULL, stringsAsFactors = F))
}