set more off

log using "test_07-19.log", replace



*** White US-born

* load data
clear
import delim using "/home/bbellman/census-linking/data/for_models/test-stata-wnb20-jul19.csv"

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq dest_ed_mean_sei dest_ed_mean_sei_sq dist dist_sq bla1_pblack_ed bla1_pblack_ed_sq bla1_dist bla1_dist_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq imm1_dist imm1_dist_sq sei1_msei_ed sei1_msei_ed_sq sei1_dist sei1_dist_sq sei_pblack_ed sei_pblack_ed_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq sei_msei_ed sei_msei_ed_sq sei_dist sei_dist_sq, group(serial1)
clogit, or

* add predicted values to data
* assuming conditional outcome
predict phat
* removing assumption of single positive group outcome
predict phat2, pu0

* export file
export delim using "/home/bbellman/census-linking/data/for_models/test-stata-wnb20-jul19-phat.csv"

log close
