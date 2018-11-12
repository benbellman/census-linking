library(dplyr)

# function inputs are the relate values for the focal/comparison "record"s and the focal/comparison "roster" members
relate_pt <- function(roster1, roster2, record1, record2){
  
  # if comparison record is also household head in t1, give point if roster member has same relationship
  if(record1 == "Head/householder"){
    if_else(roster2 == roster1, 1, 0)
  }
  
  # if comparison record is a child in t1, give point when relate is consistent for different kinds of relationships
  if(record1 == "Child"){
    case_when(
      # spouse
      roster2=="Spouse" & roster1=="Child-in-law" ~ 1,
      # child
      roster2=="Child" & roster1=="Grandchild" ~ 1,
      # parent
      roster2=="Parent" & (roster1=="Head/householder" |  roster1=="Spouse") ~ 1,
      # sibling
      roster2=="Sibling" & roster1=="Child" ~ 1,
      # any other case, we're not confident about
      TRUE ~ 0
    )
  }
  
  # now same process if focal record was sibling in t1
  if(record1 == "Sibling"){
    case_when(
      # spouse
      roster2=="Spouse" & roster1=="Sibling-in-law" ~ 1,
      # child
      roster2=="Child" & roster1=="Nephew, niece" ~ 1,
      # parent
      roster2=="Parent" & roster1=="Parent" ~ 1,
      # sibling
      roster2=="Sibling" & (roster1=="Head/householder" |  roster1=="Sibling") ~ 1,
      # any other case, we're not confident about
      TRUE ~ 0
    )
  }
  
  # any other case, we're not confident about
  else 0
}