* Code for running all discrete choice models


log using "deadline-models.log", replace

**********


*** Black sample, 1910-1920 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/final-phl_discrete_choice_black_20.csv", clear
duplicates drop

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq imm1_pblack_ed imm1_pblack_ed_sq sei1_pblack_ed sei1_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq bla1_pfrnbrn_ed bla1_pfrnbrn_ed_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei1_pfrnbrn_ed sei1_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq bla1_msei_ed bla1_msei_ed_sq imm1_msei_ed imm1_msei_ed_sq sei1_msei_ed sei1_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq, group(serial1)
*clogit, or

* generate predicted probabilities of household destination choices
predict phat

* save data with predictions
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/obsv-black-20-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples-final.csv", clear

* predictions for hypothetical ED choice sets
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/ex-black-20-preds.csv", replace









*** Black sample, 1920-1930 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/final-phl_discrete_choice_black_30.csv", clear
duplicates drop

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq imm1_pblack_ed imm1_pblack_ed_sq sei1_pblack_ed sei1_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq bla1_pfrnbrn_ed bla1_pfrnbrn_ed_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei1_pfrnbrn_ed sei1_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq bla1_msei_ed bla1_msei_ed_sq imm1_msei_ed imm1_msei_ed_sq sei1_msei_ed sei1_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq, group(serial1)
*clogit, or

* generate predicted probabilities of household destination choices
predict phat

* save data with predictions
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/obsv-black-30-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples-final.csv", clear

* predictions for hypothetical ED choice sets
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/ex-black-30-preds.csv", replace











*** Black sample, 1930-1940 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/final-phl_discrete_choice_black_40.csv", clear
duplicates drop

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq imm1_pblack_ed imm1_pblack_ed_sq sei1_pblack_ed sei1_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq bla1_pfrnbrn_ed bla1_pfrnbrn_ed_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei1_pfrnbrn_ed sei1_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq bla1_msei_ed bla1_msei_ed_sq imm1_msei_ed imm1_msei_ed_sq sei1_msei_ed sei1_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq, group(serial1)
*clogit, or

* generate predicted probabilities of household destination choices
predict phat

* save data with predictions
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/obsv-black-40-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples-final.csv", clear

* predictions for hypothetical ED choice sets
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/ex-black-40-preds.csv", replace









*************


*** White immigrant sample, 1910-1920 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/final-phl_discrete_choice_wimm_20.csv", clear
duplicates drop

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq imm1_pblack_ed imm1_pblack_ed_sq sei1_pblack_ed sei1_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq bla1_pfrnbrn_ed bla1_pfrnbrn_ed_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei1_pfrnbrn_ed sei1_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq bla1_msei_ed bla1_msei_ed_sq imm1_msei_ed imm1_msei_ed_sq sei1_msei_ed sei1_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq, group(serial1)
*clogit, or

* generate predicted probabilities of household destination choices
predict phat

* save data with predictions
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/obsv-wimm-20-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples-final.csv", clear

* predictions for hypothetical ED choice sets
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/ex-wimm-20-preds.csv", replace










*** White immigrant sample, 1920-1930 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/final-phl_discrete_choice_wimm_30.csv", clear
duplicates drop

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq imm1_pblack_ed imm1_pblack_ed_sq sei1_pblack_ed sei1_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq bla1_pfrnbrn_ed bla1_pfrnbrn_ed_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei1_pfrnbrn_ed sei1_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq bla1_msei_ed bla1_msei_ed_sq imm1_msei_ed imm1_msei_ed_sq sei1_msei_ed sei1_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq, group(serial1)
*clogit, or

* generate predicted probabilities of household destination choices
predict phat

* save data with predictions
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/obsv-wimm-30-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples-final.csv", clear

* predictions for hypothetical ED choice sets
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/ex-wimm-30-preds.csv", replace











*** White immigrant sample, 1930-1940 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/final-phl_discrete_choice_wimm_40.csv", clear
duplicates drop

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq imm1_pblack_ed imm1_pblack_ed_sq sei1_pblack_ed sei1_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq bla1_pfrnbrn_ed bla1_pfrnbrn_ed_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei1_pfrnbrn_ed sei1_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq bla1_msei_ed bla1_msei_ed_sq imm1_msei_ed imm1_msei_ed_sq sei1_msei_ed sei1_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq, group(serial1)
*clogit, or

* generate predicted probabilities of household destination choices
predict phat

* save data with predictions
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/obsv-wimm-40-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples-final.csv", clear

* predictions for hypothetical ED choice sets
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/ex-wimm-40-preds.csv", replace









************

*** White native-born sample, 1910-1920 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/final-phl_discrete_choice_wnb_20.csv", clear
duplicates drop

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq imm1_pblack_ed imm1_pblack_ed_sq sei1_pblack_ed sei1_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq bla1_pfrnbrn_ed bla1_pfrnbrn_ed_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei1_pfrnbrn_ed sei1_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq bla1_msei_ed bla1_msei_ed_sq imm1_msei_ed imm1_msei_ed_sq sei1_msei_ed sei1_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq, group(serial1)
*clogit, or

* generate predicted probabilities of household destination choices
predict phat

* save data with predictions
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/obsv-wnb-20-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples-final.csv", clear

* predictions for hypothetical ED choice sets
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/ex-wnb-20-preds.csv", replace








*** White native-born sample, 1920-1930 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/final-phl_discrete_choice_wnb_30.csv", clear
duplicates drop

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq imm1_pblack_ed imm1_pblack_ed_sq sei1_pblack_ed sei1_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq bla1_pfrnbrn_ed bla1_pfrnbrn_ed_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei1_pfrnbrn_ed sei1_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq bla1_msei_ed bla1_msei_ed_sq imm1_msei_ed imm1_msei_ed_sq sei1_msei_ed sei1_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq, group(serial1)
*clogit, or

* generate predicted probabilities of household destination choices
predict phat

* save data with predictions
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/obsv-wnb-30-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples-final.csv", clear

* predictions for hypothetical ED choice sets
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/ex-wnb-30-preds.csv", replace










*** White native-born sample, 1930-1940 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/final-phl_discrete_choice_wnb_40.csv", clear
duplicates drop

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq imm1_pblack_ed imm1_pblack_ed_sq sei1_pblack_ed sei1_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq bla1_pfrnbrn_ed bla1_pfrnbrn_ed_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei1_pfrnbrn_ed sei1_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq bla1_msei_ed bla1_msei_ed_sq imm1_msei_ed imm1_msei_ed_sq sei1_msei_ed sei1_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq, group(serial1)
*clogit, or

* generate predicted probabilities of household destination choices
predict phat

* save data with predictions
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/obsv-wnb-40-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples-final.csv", clear

* predictions for hypothetical ED choice sets
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc-final-preds/ex-wnb-40-preds.csv", replace






log close
