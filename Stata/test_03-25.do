set processors 8
set more off

log using "test_03-25.log", replace


*** run for each of three data files (white, wimm, black)

*** White US-born

* load data
clear
import delim using "/home/bbellman/census-linking/data/for_models/test-stata-white_03-25.csv"

* convert distance vars to numeric values
replace dist = "" if dist == "NA"
replace dist2 = "" if dist2 == "NA"
destring dist, replace
destring dist2, replace

* set all interactions
gen edb1_edb2 = ed_pct_black1 * dest_ed_pct_black
gen edb1_edf2 = ed_pct_black1 * dest_ed_pct_frnbrn
gen edb1_edsei2 = ed_pct_black1 * dest_ed_mean_sei
gen edb1_dist = ed_pct_black1 * dist
gen edb1_dist2 = ed_pct_black1 * dist2

gen edf1_edb2 = ed_pct_frnbrn1 * dest_ed_pct_black
gen edf1_edf2 = ed_pct_frnbrn1 * dest_ed_pct_frnbrn
gen edf1_edsei2 = ed_pct_frnbrn1 * dest_ed_mean_sei
gen edf1_dist = ed_pct_frnbrn1 * dist
gen edf1_dist2 = ed_pct_frnbrn1 * dist2

gen edsei1_edb2 = ed_mean_sei1 * dest_ed_pct_black
gen edsei1_edf2 = ed_mean_sei1 * dest_ed_pct_frnbrn
gen edsei1_edsei2 = ed_mean_sei1 * dest_ed_mean_sei
gen edsei1_dist = ed_mean_sei1 * dist
gen edsei1_dist2 = ed_mean_sei1 * dist2


* run conditional logit model
clogit choice ed_pct_black1 ed_pct_frnbrn1 ed_mean_sei1 dest_ed_pct_black dest_ed_pct_frnbrn dest_ed_mean_sei dist dist2 edb1_edb2 edb1_edf2 edb1_edsei2 edb1_dist edb1_dist2 edf1_edb2 edf1_edf2 edf1_edsei2 edf1_dist edf1_dist2 edsei1_edb2 edsei1_edf2 edsei1_edsei2 edsei1_dist edsei1_dist2, group(serial1)
clogit, or

*** White foreign-born

* load data
clear
import delim using "/home/bbellman/census-linking/data/for_models/test-stata-wimm_03-25.csv"

* convert distance vars to numeric values
replace dist = "" if dist == "NA"
replace dist2 = "" if dist2 == "NA"
destring dist, replace
destring dist2, replace

* set all interactions
gen edb1_edb2 = ed_pct_black1 * dest_ed_pct_black
gen edb1_edf2 = ed_pct_black1 * dest_ed_pct_frnbrn
gen edb1_edsei2 = ed_pct_black1 * dest_ed_mean_sei
gen edb1_dist = ed_pct_black1 * dist
gen edb1_dist2 = ed_pct_black1 * dist2

gen edf1_edb2 = ed_pct_frnbrn1 * dest_ed_pct_black
gen edf1_edf2 = ed_pct_frnbrn1 * dest_ed_pct_frnbrn
gen edf1_edsei2 = ed_pct_frnbrn1 * dest_ed_mean_sei
gen edf1_dist = ed_pct_frnbrn1 * dist
gen edf1_dist2 = ed_pct_frnbrn1 * dist2

gen edsei1_edb2 = ed_mean_sei1 * dest_ed_pct_black
gen edsei1_edf2 = ed_mean_sei1 * dest_ed_pct_frnbrn
gen edsei1_edsei2 = ed_mean_sei1 * dest_ed_mean_sei
gen edsei1_dist = ed_mean_sei1 * dist
gen edsei1_dist2 = ed_mean_sei1 * dist2


* run conditional logit model
clogit choice ed_pct_black1 ed_pct_frnbrn1 ed_mean_sei1 dest_ed_pct_black dest_ed_pct_frnbrn dest_ed_mean_sei dist dist2 edb1_edb2 edb1_edf2 edb1_edsei2 edb1_dist edb1_dist2 edf1_edb2 edf1_edf2 edf1_edsei2 edf1_dist edf1_dist2 edsei1_edb2 edsei1_edf2 edsei1_edsei2 edsei1_dist edsei1_dist2, group(serial1)
clogit, or

*** Black

* load data
clear
import delim using "/home/bbellman/census-linking/data/for_models/test-stata-black_03-25.csv"

* convert distance vars to numeric values
replace dist = "" if dist == "NA"
replace dist2 = "" if dist2 == "NA"
destring dist, replace
destring dist2, replace

* set all interactions
gen edb1_edb2 = ed_pct_black1 * dest_ed_pct_black
gen edb1_edf2 = ed_pct_black1 * dest_ed_pct_frnbrn
gen edb1_edsei2 = ed_pct_black1 * dest_ed_mean_sei
gen edb1_dist = ed_pct_black1 * dist
gen edb1_dist2 = ed_pct_black1 * dist2

gen edf1_edb2 = ed_pct_frnbrn1 * dest_ed_pct_black
gen edf1_edf2 = ed_pct_frnbrn1 * dest_ed_pct_frnbrn
gen edf1_edsei2 = ed_pct_frnbrn1 * dest_ed_mean_sei
gen edf1_dist = ed_pct_frnbrn1 * dist
gen edf1_dist2 = ed_pct_frnbrn1 * dist2

gen edsei1_edb2 = ed_mean_sei1 * dest_ed_pct_black
gen edsei1_edf2 = ed_mean_sei1 * dest_ed_pct_frnbrn
gen edsei1_edsei2 = ed_mean_sei1 * dest_ed_mean_sei
gen edsei1_dist = ed_mean_sei1 * dist
gen edsei1_dist2 = ed_mean_sei1 * dist2


* run conditional logit model
clogit choice ed_pct_black1 ed_pct_frnbrn1 ed_mean_sei1 dest_ed_pct_black dest_ed_pct_frnbrn dest_ed_mean_sei dist dist2 edb1_edb2 edb1_edf2 edb1_edsei2 edb1_dist edb1_dist2 edf1_edb2 edf1_edf2 edf1_edsei2 edf1_dist edf1_dist2 edsei1_edb2 edsei1_edf2 edsei1_edsei2 edsei1_dist edsei1_dist2, group(serial1)
clogit, or


log close
