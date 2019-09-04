
*** Black sample, 1910-1920 movers

* load data
clear
import delim using "~/Documents/Computer Backup/Repos/census-linking/data/for_models/test-stata-b20-athome.csv"

* run conditional logit model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq dest_ed_mean_sei dest_ed_mean_sei_sq dist dist_sq bla1_pblack_ed bla1_pblack_ed_sq bla1_dist bla1_dist_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq imm1_dist imm1_dist_sq sei1_msei_ed sei1_msei_ed_sq sei1_dist sei1_dist_sq sei_pblack_ed sei_pblack_ed_sq sei_pfrnbrn_ed sei_pfrnbrn_ed_sq sei_msei_ed sei_msei_ed_sq sei_dist sei_dist_sq, group(serial1)

* generate two kinds of predictions (from clogit example)
* both assume the group fixed effect (1 positive outcome per household)
predict phat
predict phat2 if e(sample)
predict phat3, pu0

* save for exploring with ggplot in R
export delim using "~/Documents/Computer Backup/Repos/census-linking/data/for_models/test-stata-b20-athome-preds.csv"

