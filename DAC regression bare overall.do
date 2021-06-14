*** DAC regressions further robustness checks:

*** clear workspaces
	clear all
	pause off
	set more off
	cls

*** use up more computer memory for the sake of accurate numbers:
	set type double, perm

*** macros
foreach user in "`c(username)'" {
	global root "/Users/`user'/Dropbox/CGD/Projects/DAC_ODA_2019_spending/"
	global output "/Users/`user'/Dropbox/CGD/Projects/DAC_ODA_2019_spending/output/"
	global input "/Users/`user'/Dropbox/CGD/Projects/DAC_ODA_2019_spending/input/"
}

*** name of output regression files:
	global regout1 "$input/total_regressions68.xls"
	global regout2 "$input/pov_regressions68.xls"

local Y_outcome usd_commitment

use "$input\dac.dta", clear

*** Just get the main regression:
local varlist gdppc GDP Population refugeepop_orig refugeepop_dest wgi pov1_9 ///
pov3_8 distcap colony exports

/* carryforward / fill downwards these variables: (ensures that 
if we're missing poverty level for 2019, then we get it from 
a prior year) */
	foreach var of local varlist {
		sort iso3c iso3c_d year
		bysort iso3c iso3c_d: carryforward `var', gen(`var'n)
		replace `var' = `var'n
		drop `var'n
	}

keep if year==2019

/* we define a macro "collapse_type" because we want to make the 
code generalizable to any time frame and any type of regression. 
So, if the year is only 2019, it doesn't really matter if we 
take a sum or a mean */

// local collapse_type sum
//
// collapse (`collapse_type') usd_commitment GDP Pop refugeepop_orig refugeepop_dest total exports wgi pov1_9 pov3_8 (max) distcap colony, by (iso3c_d iso3c)

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

	if "usd_commitment" == "usd_commitment" {
		label variable usd_commitment "Commitments (% of total ODA)"
	}
	
/* For each donor-country pair, get the total amount of non-donor ODA 
to that country divided by GDP: */
	bysort iso3c: egen totoda_recip = total(usd_commitment)
	gen other_oda = totoda_recip - usd_commitment
	gen other_oda_div_gdp = other_oda / GDP * (10^6)
	label variable other_oda_div_gdp "total ODA from other donors besides donor country per 1m GDP"
	
/* For each donor-country pair, get the total amount received by non-donor 
countries divided by ODA from donor countries: */
	*** get the total ODA OVERALL
		egen totoda = total(usd_commitment)
		bysort iso3c: egen donor_oda = total(usd_commitment)
		gen totoda_less_donor = totoda - donor_oda
	*** Now, this gives us the Share of the country in the other donors' ODA contributions
		gen other_oda_div_totoda_less_donor = other_oda / totoda_less_donor
		label variable other_oda_div_totoda_less_donor ///
		"Share of the country in the other donors' ODA contributions"
		
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
	bysort iso3c_d: egen totoda_donor = total(usd_commitment)
	replace usd_commitment = usd_commitment / totoda_donor

/* make sure that the proportion is actually a proportion 
(i.e. less than 1) */
	preserve
	assert usd_commitment<1
	collapse (sum) usd_commitment, by(iso3c_d)
	assert abs(usd_commitment-1)<=0.02
	restore

*** take a log of distance
	local tolog_non_DAC "distcap"
	
/* Create logged variables. Relabel these variables to indicate that 
they've been logged. I added usd_commitment to loop if for logged 
outcome variable. */
	foreach var of varlist Population GDP `tolog_non_DAC' { //`addtl_to_log'
		replace `var' = ln(`var')
		loc lab: variable label `var'
		di "`lab'"
		label variable `var' "Log `lab'"
	}

local bilateral_vars "dist colony exports"
local extra_vars_nonDAC "i.colony exports distcap"

/* Make sure that we have 1 recip only for the DAC regressions, but only 
1 donor-recip pair for the non-DAC regressions */
	preserve
		gen num = 1
		collapse (sum) num, by (iso3c_d iso3c) 
		assert abs(num-1)<=0.0001
	restore	

/* for our regression, omit the countries where we do not have data */
	foreach x of varlist usd_commitment GDP Population refugeepop_dest ///
		refugeepop_orig total wgi `bilateral_vars' other_oda_div_gdp ///
		other_oda_div_totoda_less_donor{
		drop if (`x' == .)
	}

/* drop HICs */
	mmerge iso3c using "$input/income_groups.dta"
	keep if _m==3
	drop if income == "High income"
	
	save "$input/overall_regression_input.dta", replace

	
*** Now define a local with the biggest 10 donors:
	clear all	
		input str40 generous_countries
		"AUS"
		"CAN"
		"FRA"
		"DEU"
		"JPN"
		"KOR"
		"NLD"
		"NOR"
		"USA"
		"GBR"
		end
	levelsof generous_countries, local(countries_toloop)

clear

*** Do regressions:
foreach donor_c of local countries_toloop {
	use "$input/overall_regression_input.dta", clear
	keep if iso3c_d == "`donor_c'"
	qui: regress usd_commitment GDP Pop refugeepop_dest refugeepop_orig total wgi ///
	other_oda_div_gdp `extra_vars_nonDAC' other_oda_div_totoda_less_donor, robust
	outreg2 using "$regout1", append ctitle("`donor_c'") label dec(7)
}























