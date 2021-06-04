// clear workspaces
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

global regout2 "$input/total_regressions21 - NOT SCALE - NOT LOGGED.xls"

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
	foreach var of varlist Population GDP usd_com distcap { // add usd_grantequiv if you want logged outcome variable
		replace `var' = ln(`var')
		loc lab: variable label `var'
		di "`lab'"
		label variable `var' "Log `lab'"
	}

gen inv_GDP_int_pop = (Pop/GDP) // recall that the GDP variable now is GDP per capita
label variable inv_GDP_int_pop "Log Population / Log GDP per capita"

// foreach var of varlist GDP Population inv_GDP_int_pop refugeepop_orig refugeepop_dest remitperscurrent total wgi distcap exports usd_commitment usd_grantequiv {
// 					egen s_`var' = std(`var')
// 					replace `var' = s_`var'
// 					drop s_`var'
// 	}
	
save "$input/cleaned.dta", replace

// Now sort by the biggest 10 donors; do a regression and output
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
		refugeepop_dest total i.colony exports wgi, robust;
		outreg2 using "$regout2", append ctitle("`lev'") label;
	reg usd_grantequiv 
		GDP Pop inv_GDP_int_pop dist refugeepop_orig 
		refugeepop_dest total i.colony exports wgi, robust;
		outreg2 using "$regout2", append ctitle("`lev'") label;
	reg usd_grantequiv GDP, robust;
		outreg2 using "$regout2", append ctitle("`lev'") label;
	reg usd_grantequiv GDP Pop, robust;
		outreg2 using "$regout2", append ctitle("`lev'") label;
	reg usd_grantequiv GDP Pop i.colony, robust;
		outreg2 using "$regout2", append ctitle("`lev'") label;
	reg usd_grantequiv GDP Pop i.colony distcap, robust;
		outreg2 using "$regout2", append ctitle("`lev'") label;
	reg usd_grantequiv refugeepop_dest refugeepop_orig, robust;
		outreg2 using "$regout2", append ctitle("`lev'") label;
	#delimit cr
	}
	
	
	clear all
	use "$input/cleaned.dta", clear
	eststo clear
	#delimit ;
	reg usd_grantequiv 
		GDP Pop dist refugeepop_orig 
		refugeepop_dest total i.colony exports wgi, robust;
		outreg2 using "$regout2", append ctitle("DAC") label;
	reg usd_grantequiv 
		GDP Pop inv_GDP_int_pop dist refugeepop_orig 
		refugeepop_dest total i.colony exports wgi, robust;
		outreg2 using "$regout2", append ctitle("DAC") label;
	reg usd_grantequiv GDP, robust;
		outreg2 using "$regout2", append ctitle("DAC") label;
	reg usd_grantequiv GDP Pop, robust;
		outreg2 using "$regout2", append ctitle("DAC") label;
	reg usd_grantequiv GDP Pop i.colony, robust;
		outreg2 using "$regout2", append ctitle("DAC") label;
	reg usd_grantequiv GDP Pop i.colony distcap, robust;
		outreg2 using "$regout2", append ctitle("DAC") label;
	reg usd_grantequiv refugeepop_dest refugeepop_orig, robust;
		outreg2 using "$regout2", append ctitle("DAC") label;
	#delimit cr
exit

	
	
	br
	
	
	
	
	
	
	
	
// Table of percent ODA spending by income group
	use "$input\dac.dta", clear
	collapse (sum) usd_grantequiv, by (donorname incomegroupname)
	bysort donor: egen tot = total(usd_grantequi*v)
	replace usd = usd / tot *100
	gsort -tot
	egen c = rank(-tot), track
	drop if c > 30
	drop tot c
	sort donor income
	
	graph hbar (asis) usd_grantequiv, over(incomegroupname, gap(*1.2) label(labsize(6-pt))) over(donorname, gap(*1.2) label(labsize(6-pt))) bar(1, fcolor(orange_red) lcolor(dkorange)) ytitle(Percent of Total 2019 Commitment (%)) ytitle(, size(6-pt)) ylabel(, labsize(6-pt)) ymtick(, labsize(huge)) scheme(plotplain)
	gr_edit .style.editstyle declared_ysize(4) editcopy
	gr_edit .style.editstyle declared_xsize(4) editcopy
	graph export "$output/bar_income_changeflows.pdf", replace
	graph export "$output/bar_income_changeflows.png", replace

exit


