* Code for running all discrete choice models


log using "int-discrete-choice-models.log", replace

**********


*** Black sample, 1910-1920 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_black_20.csv", clear

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq sei_pblack_ed sei_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei1_msei_ed sei1_msei_ed_sq sei_msei_ed sei_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-black-20-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-black-20-preds.csv", replace






*** Black sample, 1920-1930 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_black_30.csv", clear

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq sei_pblack_ed sei_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei1_msei_ed sei1_msei_ed_sq sei_msei_ed sei_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-black-30-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-black-30-preds.csv", replace






*** Black sample, 1930-1940 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_black_40.csv", clear

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq sei_pblack_ed sei_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei1_msei_ed sei1_msei_ed_sq sei_msei_ed sei_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-black-40-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-black-40-preds.csv", replace




*************


*** White immigrant sample, 1910-1920 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_wimm_20.csv", clear

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq sei_pblack_ed sei_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei1_msei_ed sei1_msei_ed_sq sei_msei_ed sei_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-wimm-20-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-wimm-20-preds.csv", replace








*** White immigrant sample, 1920-1930 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_wimm_30.csv", clear

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq sei_pblack_ed sei_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei1_msei_ed sei1_msei_ed_sq sei_msei_ed sei_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-wimm-30-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-wimm-30-preds.csv", replace




*** White immigrant sample, 1930-1940 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_wimm_40.csv", clear

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq sei_pblack_ed sei_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei1_msei_ed sei1_msei_ed_sq sei_msei_ed sei_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-wimm-40-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-wimm-40-preds.csv", replace






************

*** White native-born sample, 1910-1920 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_wnb_20.csv", clear

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq sei_pblack_ed sei_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei1_msei_ed sei1_msei_ed_sq sei_msei_ed sei_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-wnb-20-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-wnb-20-preds.csv", replace




*** White native-born sample, 1920-1930 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_wnb_30.csv", clear

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq sei_pblack_ed sei_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei1_msei_ed sei1_msei_ed_sq sei_msei_ed sei_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-wnb-30-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-wnb-30-preds.csv", replace





*** White native-born sample, 1930-1940 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_wnb_40.csv", clear

* replace small scientific distance values with 1 (all very close to 0)
*replace dist_s = "1" if regexm(dist, "e")
*replace dist = "1" if regexm(dist, "e")
*destring dist, replace

*drop if regexm(dist, "NA")

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq sei_pblack_ed sei_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei1_msei_ed sei1_msei_ed_sq sei_msei_ed sei_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-wnb-40-preds.csv", replace

* load example data
import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* generate two kinds of predictions for observed data
* first set assumes one choice per individual chocie set
* second doesn't
predict phat
predict phat2, pu0

* over-write model data with predictions attached
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-wnb-40-preds.csv", replace






log close
