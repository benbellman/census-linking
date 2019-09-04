* Code for running all discrete choice models


log using "diff-discrete-choice-models.log", replace

**********


*** Black sample, 1910-1920 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_black_20_diff.csv", clear
duplicates drop

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq sei_pblack_ed sei_pblack_ed_sq diff_ed_pct_black diff_ed_pct_black_sq sei_diff_pblack_ed sei_diff_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq diff_ed_pct_frnbrn diff_ed_pct_frnbrn_sq sei_diff_pfrnbrn_ed sei_diff_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei_msei_ed sei_msei_ed_sq diff_ed_mean_sei diff_ed_mean_sei_sq sei_diff_msei_ed sei_diff_msei_ed_sq dist dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or


* try without squared terms
*clogit choice dest_ed_pct_black sei_pblack_ed diff_ed_pct_black sei_diff_pblack_ed dest_ed_pct_frnbrn sei_pfrnbrn_ed diff_ed_pct_frnbrn sei_diff_pfrnbrn_ed dest_ed_mean_sei sei_msei_ed diff_ed_mean_sei sei_diff_msei_ed dist sei_dist, group(serial1)
*clogit, or


* try without HH SEI interactions
*clogit choice dest_ed_pct_black dest_ed_pct_black_sq diff_ed_pct_black diff_ed_pct_black_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq diff_ed_pct_frnbrn diff_ed_pct_frnbrn_sq dest_ed_mean_sei dest_ed_mean_sei_sq diff_ed_mean_sei diff_ed_mean_sei_sq dist dist_sq, group(serial1)
*clogit, or

* try without interactions or squared terms
clogit choice dest_ed_pct_black dest_ed_pct_frnbrn diff_ed_pct_frnbrn dest_ed_mean_sei dist, group(serial1)
clogit, or



* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-black-20-preds_diff.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-black-20-preds_diff.csv", replace









*** Black sample, 1920-1930 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_black_30_diff.csv", clear
duplicates drop

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq sei_pblack_ed sei_pblack_ed_sq diff_ed_pct_black diff_ed_pct_black_sq sei_diff_pblack_ed sei_diff_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq diff_ed_pct_frnbrn diff_ed_pct_frnbrn_sq sei_diff_pfrnbrn_ed sei_diff_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei_msei_ed sei_msei_ed_sq diff_ed_mean_sei diff_ed_mean_sei_sq sei_diff_msei_ed sei_diff_msei_ed_sq dist dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-black-30-preds_diff.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-black-30-preds_diff.csv", replace








*** Black sample, 1930-1940 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_black_40_diff.csv", clear
duplicates drop

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq sei_pblack_ed sei_pblack_ed_sq diff_ed_pct_black diff_ed_pct_black_sq sei_diff_pblack_ed sei_diff_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq diff_ed_pct_frnbrn diff_ed_pct_frnbrn_sq sei_diff_pfrnbrn_ed sei_diff_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei_msei_ed sei_msei_ed_sq diff_ed_mean_sei diff_ed_mean_sei_sq sei_diff_msei_ed sei_diff_msei_ed_sq dist dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-black-40-preds_diff.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-black-40-preds_diff.csv", replace




*************


*** White immigrant sample, 1910-1920 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_wimm_20_diff.csv", clear
duplicates drop

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq sei_pblack_ed sei_pblack_ed_sq diff_ed_pct_black diff_ed_pct_black_sq sei_diff_pblack_ed sei_diff_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq diff_ed_pct_frnbrn diff_ed_pct_frnbrn_sq sei_diff_pfrnbrn_ed sei_diff_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei_msei_ed sei_msei_ed_sq diff_ed_mean_sei diff_ed_mean_sei_sq sei_diff_msei_ed sei_diff_msei_ed_sq dist dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-wimm-20-preds_diff.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-wimm-20-preds_diff.csv", replace








*** White immigrant sample, 1920-1930 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_wimm_30_diff.csv", clear
duplicates drop

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq sei_pblack_ed sei_pblack_ed_sq diff_ed_pct_black diff_ed_pct_black_sq sei_diff_pblack_ed sei_diff_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq diff_ed_pct_frnbrn diff_ed_pct_frnbrn_sq sei_diff_pfrnbrn_ed sei_diff_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei_msei_ed sei_msei_ed_sq diff_ed_mean_sei diff_ed_mean_sei_sq sei_diff_msei_ed sei_diff_msei_ed_sq dist dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-wimm-30-preds_diff.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-wimm-30-preds_diff.csv", replace






*** White immigrant sample, 1930-1940 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_wimm_40_diff.csv", clear
duplicates drop

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq sei_pblack_ed sei_pblack_ed_sq diff_ed_pct_black diff_ed_pct_black_sq sei_diff_pblack_ed sei_diff_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq diff_ed_pct_frnbrn diff_ed_pct_frnbrn_sq sei_diff_pfrnbrn_ed sei_diff_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei_msei_ed sei_msei_ed_sq diff_ed_mean_sei diff_ed_mean_sei_sq sei_diff_msei_ed sei_diff_msei_ed_sq dist dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-wimm-40-preds_diff.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-wimm-40-preds_diff.csv", replace






************

*** White native-born sample, 1910-1920 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_wnb_20_diff.csv", clear
duplicates drop

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq sei_pblack_ed sei_pblack_ed_sq diff_ed_pct_black diff_ed_pct_black_sq sei_diff_pblack_ed sei_diff_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq diff_ed_pct_frnbrn diff_ed_pct_frnbrn_sq sei_diff_pfrnbrn_ed sei_diff_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei_msei_ed sei_msei_ed_sq diff_ed_mean_sei diff_ed_mean_sei_sq sei_diff_msei_ed sei_diff_msei_ed_sq dist dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-wnb-20-preds_diff.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-wnb-20-preds_diff.csv", replace





*** White native-born sample, 1920-1930 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_wnb_30_diff.csv", clear
duplicates drop

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq sei_pblack_ed sei_pblack_ed_sq diff_ed_pct_black diff_ed_pct_black_sq sei_diff_pblack_ed sei_diff_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq diff_ed_pct_frnbrn diff_ed_pct_frnbrn_sq sei_diff_pfrnbrn_ed sei_diff_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei_msei_ed sei_msei_ed_sq diff_ed_mean_sei diff_ed_mean_sei_sq sei_diff_msei_ed sei_diff_msei_ed_sq dist dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-wnb-30-preds_diff.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-wnb-30-preds_diff.csv", replace





*** White native-born sample, 1930-1940 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_wnb_40.csv_diff", clear
duplicates drop

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq sei_pblack_ed sei_pblack_ed_sq diff_ed_pct_black diff_ed_pct_black_sq sei_diff_pblack_ed sei_diff_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq diff_ed_pct_frnbrn diff_ed_pct_frnbrn_sq sei_diff_pfrnbrn_ed sei_diff_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei_msei_ed sei_msei_ed_sq diff_ed_mean_sei diff_ed_mean_sei_sq sei_diff_msei_ed sei_diff_msei_ed_sq dist dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-wnb-40-preds_diff.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-wnb-40-preds_diff.csv", replace






log close
