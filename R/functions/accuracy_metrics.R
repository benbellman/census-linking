get_tpr <- function(test){
  all_pos <- nrow(filter(test, true_match == "Yes"))
  true_pos <- nrow(filter(test, auto_match == 1 & true_match == "Yes"))
  true_pos / all_pos
}

get_ppv <- function(test){
  matches <- nrow(filter(test, auto_match == 1))
  true_pos <- nrow(filter(test, auto_match == 1 & true_match == "Yes"))
  true_pos / matches
}