// clear workspace
cls

// use up more computer memory for the sake of accurate numbers:
set type double, perm

// ========== Macros ============
foreach user in "`c(username)'" {
	global root "/Users/`user'/Dropbox/CGD/Projects/DAC_ODA_2019_spending/"
	global output "/Users/`user'/Dropbox/CGD/Projects/DAC_ODA_2019_spending/output/"
	global input "/Users/`user'/Dropbox/CGD/Projects/DAC_ODA_2019_spending/input/"
}

// name of output regression files:
global regout2 "$input/outreg2_regressions.tex"
global regout1 "$input/estout_regressions.tex"

clear all
set more off 

// ============ Packages ============
// ssc install hilo, replace
// ssc install mdesc, replace
// ssc install outreg2, replace
// ssc install estout, replace
// ssc install elabel, replace


// ============ Cleaning ============
use "$input\dac.dta", clear

// We're dropping gni and poverty, since there are too many missing variables:
	mdesc

// Generate per capita variables and LOG transform some variables:
	collapse (sum) usd_commitment usd_grantequiv, by (GDP Pop refugeepop_orig refugeepop_dest remitperscurrent total distcap colony exports wgi donor recip)
	bysort donor: egen totoda = total(usd_grantequiv)	
	replace usd_grantequiv = usd_grantequiv / totoda
	
// label variabless
	label variable colony "Bilat. Colony Dummy"
	label variable distcap "Bilat. Distance btwn. Capitals (km)"
	label variable exports "Bilat. exports (Percent of Total Donor exports)"
	label variable usd_commitment "Commitments"
	label variable usd_grantequiv "Grant Equivalent (% of total ODA)"
	label variable GDP "GDP (current USD)"
	label variable Population "Population"
	label variable refugeepop_dest "Recip. Refugee Inflows"
	label variable refugeepop_orig "Recip. Refugee Outflows"
	label variable remitperscurrent "Recip. Remittance Inflows (current USD)"
	label variable total "Recip. Total Freedom House Democracy Score (0 to 100)"
	label variable wgi "Recip. World Governance Indicator (mean) (-3 to 3)"

// Create per capita variables; relabel as per capita.	
	foreach var of varlist refugeepop_orig refugeepop_dest remitperscurrent GDP {
		replace `var' = `var'/ Pop
		loc lab: variable label `var'
		di "`lab'"
		label variable `var' "`lab' per capita"
	}

// Create logged variables; relabel as logged
	foreach var of varlist Population GDP usd_com usd_grantequiv distcap {
		gen ln_`var' = ln(`var')
		loc lab: variable label `var'
		di "`lab'"
		label variable `var' "Log `lab'"
	}
	
save "$input/cleaned.dta", replace

// =========================================================
// Regressions:	============================================
// =========================================================

// Option 1: use outreg2============

// For the biggest 10 donors; do a regression and output
clear all
use "$input/cleaned.dta", clear
	#delimit ;
	keep if inlist(donor, "United States", "Germany", "United Kingdom", "France") 
		| inlist(donor, "Japan", "Australia", "Norway", "Canada", "Korea", "Netherlands")
	;
	#delimit cr
tempfile generous_cleaned
save `generous_cleaned'
	levels donor
	foreach lev in `r(levels)' {
	clear all
	use `generous_cleaned', clear
	keep if donor == "`lev'"
	eststo clear
	#delimit ;
	reg usd_grantequiv 
		GDP Pop dist refugeepop_orig 
		refugeepop_dest total colony exports wgi, 
		robust
	;
	#delimit cr

	outreg2 using "$regout2", append ctitle("`lev'") label
	
	}
	
// Option 2: Use esttab============

use "$input/cleaned.dta", clear
		eststo clear
		eststo drop r1 r2
		// First, regress on non-scaled estimates.
			#delimit ;
			eststo r1: 
			reg usd_grantequiv 
					GDP Pop dist refugeepop_orig 
					refugeepop_dest total colony exports wgi, 
					robust
			;
			#delimit cr
		// Get scaled estimates of everything for a second regression
			foreach var of varlist refugeepop_orig refugeepop_dest total exports wgi {
				egen s_`var' = std(`var')
				replace `var' = s_`var'
				drop s_`var'
			}
			#delimit ;
			eststo r2: reg usd_grantequiv 
					GDP Pop dist refugeepop_orig 
					refugeepop_dest total colony exports wgi, 
					robust
			;
			#delimit cr

		// Output regression to latex with standard errors and adjusted R^2:
		// taken from http://repec.org/bocode/e/estout/esttab.html

			#delimit ;
			esttab r1 r2 using "$regout1", 
			append
			se ar2 
			label title(ALL ODA Commitments 2019)
			nonumbers 
			mtitles(
			"(1) Log Grant-equivalent Disbursement" 
			"(2) Log Grant-equivalent Disbursement (indep. var scaled)"
			) 
			addnote("Each observation is a country-donor pair. For scaled regressions (2,4), log variables and dummy variables are not scaled. All variables labeled 'Recip.' indicates that the variable was matched to the recipient country only, and not bilaterally. Some countries has limited numbers of countries, such as Japan (Micronesia) or Australia (Papua New Guinea and Nauru), and so regression slope estimates are not robust.")
			;
			#delimit cr


exit

