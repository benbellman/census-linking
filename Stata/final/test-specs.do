*** Testing model specifications

log using "test-specs.log", replace

* load observed data (white immigrants, 1930-40)
import delim using "/home/bbellman/census-linking/data/for_models/final-phl_discrete_choice_wimm_40.csv", clear
duplicates drop

* 1. base model
clogit choice dest_ed_pct_black dest_ed_pct_frnbrn dest_ed_mean_sei dist, group(serial1)

* 2. base + quadratic terms
clogit choice dest_ed_pct_black dest_ed_pct_black_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq dest_ed_mean_sei dest_ed_mean_sei_sq dist dist_sq, group(serial1)

* 3. base + ED1 interactions
clogit choice dest_ed_pct_black bla1_pblack_ed imm1_pblack_ed sei1_pblack_ed dest_ed_pct_frnbrn bla1_pfrnbrn_ed imm1_pfrnbrn_ed sei1_pfrnbrn_ed dest_ed_mean_sei bla1_msei_ed imm1_msei_ed sei1_msei_ed dist bla1_dist imm1_dist sei1_dist, group(serial1)

* 4. base + quadratic terms + ED1 interactions
clogit choice dest_ed_pct_black dest_ed_pct_black_sq bla1_pblack_ed bla1_pblack_ed_sq imm1_pblack_ed imm1_pblack_ed_sq sei1_pblack_ed sei1_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq bla1_pfrnbrn_ed bla1_pfrnbrn_ed_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei1_pfrnbrn_ed sei1_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq bla1_msei_ed bla1_msei_ed_sq imm1_msei_ed imm1_msei_ed_sq sei1_msei_ed sei1_msei_ed_sq dist dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq, group(serial1)

* 5. base + ED1 interactions + SEI interactions
clogit choice dest_ed_pct_black s_pblack_ed bla1_pblack_ed imm1_pblack_ed sei1_pblack_ed s_bla1_pblack_ed s_imm1_pblack_ed s_sei1_pblack_ed dest_ed_pct_frnbrn s_pfrnbrn_ed bla1_pfrnbrn_ed imm1_pfrnbrn_ed sei1_pfrnbrn_ed s_bla1_pfrnbrn_ed s_imm1_pfrnbrn_ed s_sei1_pfrnbrn_ed dest_ed_mean_sei s_msei_ed bla1_msei_ed imm1_msei_ed sei1_msei_ed s_bla1_msei_ed s_imm1_msei_ed s_sei1_msei_ed dist dist_sq s_dist bla1_dist imm1_dist sei1_dist s_bla1_dist s_imm1_dist s_sei1_dist, group(serial1)

* 6. full model
clogit choice dest_ed_pct_black dest_ed_pct_black_sq s_pblack_ed s_pblack_ed_sq bla1_pblack_ed bla1_pblack_ed_sq imm1_pblack_ed imm1_pblack_ed_sq sei1_pblack_ed sei1_pblack_ed_sq s_bla1_pblack_ed s_bla1_pblack_ed_sq s_imm1_pblack_ed s_imm1_pblack_ed_sq s_sei1_pblack_ed s_sei1_pblack_ed_sq dest_ed_pct_frnbrn dest_ed_pct_frnbrn_sq s_pfrnbrn_ed s_pfrnbrn_ed_sq bla1_pfrnbrn_ed bla1_pfrnbrn_ed_sq imm1_pfrnbrn_ed imm1_pfrnbrn_ed_sq sei1_pfrnbrn_ed sei1_pfrnbrn_ed_sq s_bla1_pfrnbrn_ed s_bla1_pfrnbrn_ed_sq s_imm1_pfrnbrn_ed s_imm1_pfrnbrn_ed_sq s_sei1_pfrnbrn_ed s_sei1_pfrnbrn_ed_sq dest_ed_mean_sei dest_ed_mean_sei_sq s_msei_ed s_msei_ed_sq bla1_msei_ed bla1_msei_ed_sq imm1_msei_ed imm1_msei_ed_sq sei1_msei_ed sei1_msei_ed_sq s_bla1_msei_ed s_bla1_msei_ed_sq s_imm1_msei_ed s_imm1_msei_ed_sq s_sei1_msei_ed s_sei1_msei_ed_sq dist dist_sq s_dist s_dist_sq bla1_dist bla1_dist_sq imm1_dist imm1_dist_sq sei1_dist sei1_dist_sq s_bla1_dist s_bla1_dist_sq s_imm1_dist s_imm1_dist_sq s_sei1_dist s_sei1_dist_sq, group(serial1)


log close
