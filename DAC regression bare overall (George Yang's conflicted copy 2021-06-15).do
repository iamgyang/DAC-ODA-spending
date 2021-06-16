// JUST RUN THIS BARE REGRESSION AND CALL IT DONE

// DAC regressions further robustness checks:

*** clear workspaces
clear
cls

*** use up more computer memory for the sake of accurate numbers:
set type double, perm

*** ========== Macros ============
foreach user in "`c(username)'" {
	global root "/Users/`user'/Dropbox/CGD/Projects/DAC_ODA_2019_spending/"
	global output "/Users/`user'/Dropbox/CGD/Projects/DAC_ODA_2019_spending/output/"
	global input "/Users/`user'/Dropbox/CGD/Projects/DAC_ODA_2019_spending/input/"
}

*** name of output regression files:

global regout1 "$input/total_regressions59.xls"
global regout2 "$input/pov_regressions59.xls"

clear all
pause off
set more off

local Y_outcome usd_commitment

*** Just get the main regression:
local varlist usd_commitment usd_grantequiv gdppc GDP Population refugeepop_orig refugeepop_dest wgi pov1_9 pov3_8 distcap colony exports
					foreach var of local varlist {
						sort iso3c iso3c_d year
						bysort iso3c iso3c_d: carryforward `var', gen(`var'n)
						replace `var' = `var'n
						drop `var'n
						}
keep if year==2019

*** we define a macro "collapse_type" because we want to make the 
*** code generalizable to any time frame and any type of regression. 
*** So, if the year is only 2019, it doesn't really matter if we 
*** take a sum or a mean
	local collapse_type sum

	if strpos("`regr'", "OLS positive") {
					drop if `Y_outcome' <= 0
				}

	collapse (`collapse_type') `Y_outcome' GDP Pop refugeepop_orig refugeepop_dest total exports wgi pov1_9 pov3_8 (max) distcap colony, by (iso3c_d iso3c)

*** Label variables
	label variable colony "Bilat. Colony Dummy"
	label variable distcap "Bilat. Distance btwn. Capitals (km)"
	label variable exports "Bilat. exports (Percent of Total Donor exports)"
	label variable GDP "GDP (current USD)"
	label variable Population "Population"
	label variable refugeepop_dest "Recip. Refugee Inflows"
	label variable refugeepop_orig "Recip. Refugee Outflows"
	label variable total "Recip. Total Freedom House Democracy Score (0 to 100)"
	label variable wgi "Recip. World Governance Indicator (mean) (-3 to 3)"
	label variable pov1_9 "Poverty headcount under $1.90"
	label variable pov3_8 "Poverty headcount under $3.80"

	if "`Y_outcome'" == "usd_commitment" {
		label variable usd_commitment "Commitments (% of total ODA)"
	}

*** Generate per capita variables; relabel as per capita.

	foreach var of varlist refugeepop_orig refugeepop_dest GDP {
	replace `var' = `var'/ Pop
	loc lab: variable label `var'
	di "`lab'"
	label variable `var' "`lab' per capita"
	}
			
/* For each donor, get the total ODA (to any country), which we'll 
divide individual donor-country pairs to get a proportion for the 
non-logistic regressions. */

	bysort iso3c_d: egen totoda_donor = total(`Y_outcome')
	egen totoda = total(`Y_outcome')

/* for the DAC regressions, we want to treat all of the 
donor countries as 1 big country. SO, we cannot keep
any of the bilateral variables anymore. */

collapse (sum) `Y_outcome' (mean) totoda GDP Pop ///
pov1_9 pov3_8 refugeepop_dest refugeepop_orig total ///
wgi, by (iso3c)


replace `Y_outcome' = `Y_outcome' / totoda

*** make sure that the proportion is actually a proportion 
*** (i.e. less than 1)
	preserve
	assert `Y_outcome'<1
	collapse (sum) `Y_outcome'
	assert abs(`Y_outcome'-1)<=0.02
	restore

*** IF we are in a DAC regression, then add a local
*** to say that we SHOULD log distcap

	local tolog_non_DAC ""

*** Create logged variables. Relabel these variables to indicate that 
*** they've been logged. I added `Y_outcome' to loop if for logged 
*** outcome variable.	
	foreach var of varlist Population GDP `tolog_non_DAC' `addtl_to_log' {
		replace `var' = ln(`var')
		loc lab: variable label `var'
		di "`lab'"
		label variable `var' "Log `lab'"
	}

foreach x of varlist `Y_outcome' GDP Pop refugeepop_dest ///
	refugeepop_orig total wgi `bilateral_vars' {
	drop if (`x' == .)
}

qui:`regression_type' `Y_outcome' GDP Pop refugeepop_dest ///
refugeepop_orig total wgi distcap i.colony exports ///
`regression_options'











