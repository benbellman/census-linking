* Code for running all discrete choice models


log using "test-dc-cats-20.log", replace

**********


*** Black sample, 1910-1920 movers

* load observed data
import delim using "/home/bbellman/census-linking/data/for_models/phl_discrete_choice_black_20-cats.csv", clear
duplicates drop

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq sei_pblack_ed sei_pblack_ed_sq bla1_cat2_pblack_ed bla1_cat2_pblack_ed_sq bla1_cat3_pblack_ed bla1_cat3_pblack_ed_sq sei_bla1_cat2_pblack_ed sei_bla1_cat2_pblack_ed_sq sei_bla1_cat3_pblack_ed sei_bla1_cat3_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq imm1_cat2_pfrnbrn_ed imm1_cat2_pfrnbrn_ed_sq imm1_cat3_pfrnbrn_ed imm1_cat3_pfrnbrn_ed_sq sei_imm1_cat2_pfrnbrn_ed sei_imm1_cat2_pfrnbrn_ed_sq sei_imm1_cat3_pfrnbrn_ed sei_imm1_cat3_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq sei_msei_ed sei_msei_ed_sq sei1_cat2_msei_ed sei1_cat2_msei_ed_sq sei1_cat3_msei_ed sei1_cat3_msei_ed_sq sei_sei1_cat2_msei_ed sei_sei1_cat2_msei_ed_sq sei_sei1_cat3_msei_ed sei_sei1_cat3_msei_ed_sq dist dist_sq sei_dist sei_dist_sq bla1_cat2_dist bla1_cat2_dist_sq bla1_cat3_dist bla1_cat3_dist_sq imm1_cat2_dist imm1_cat2_dist_sq imm1_cat3_dist imm1_cat3_dist_sq sei1_cat2_dist sei1_cat2_dist_sq sei1_cat3_dist sei1_cat3_dist_sq sei_bla1_cat2_dist sei_bla1_cat2_dist_sq sei_bla1_cat3_dist sei_bla1_cat3_dist_sq sei_imm1_cat2_dist sei_imm1_cat2_dist_sq sei_imm1_cat3_dist sei_imm1_cat3_dist_sq sei_sei1_cat2_dist sei_sei1_cat2_dist_sq sei_sei1_cat3_dist sei_sei1_cat3_dist_sq, group(serial1)
clogit, or

* generate predicted probabilities of household destination choices
predict phat

* save data with predictions
export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/obsv-black-20-preds-cats.csv", replace

* load example data
*import delim using "/home/bbellman/census-linking/data/for_models/dc-pred-examples.csv", clear
*destring dist, replace

* predictions for hypothetical households in real EDs
*predict phat

* over-write model data with predictions attached
*export delim using "/home/bbellman/census-linking/data/for_models/dc_example_preds/ex-black-20-preds-cats.csv", replace

