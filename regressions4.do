// clear workspaces
clear
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

global regout1 "$input/total_regressions59.xls"
global regout2 "$input/pov_regressions59.xls"

clear all
pause off
set more off

// ============ Packages ============
// ssc install mdesc, replace all
// ssc install outreg2, replace all
// ssc install estout, replace all
// ssc install elabel, replace all
// ssc install blindschemes, replace all
// ssc install fitstat, replace all
// ssc install carryforward, replace all

// ============ Cleaning ============

// Define overarching locals for which I loop over:

// Regressions:

	clear all
		input str40 regressions
		"2019 OLS"
		"2014-19 OLS"
		"2014-19 OLS ln"
		"2019 OLS ln"
		"2019 OLS positive"
		"2019 Poisson positive"
		"2019 Logistic"
		end
	levelsof regressions, local(regressions_toloop)

// Now define a local with the biggest 10 donors:
	clear all	
		input str40 generous_countries
		"All DAC"
		"Top ten DAC"
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
		
// Now define a local with outcome variables:
	clear all	
		input str40 outcome_vars
		"usd_grantequiv"
		"usd_commitment"
		end
	levelsof outcome_vars, local(outcome_vars_local)
	
// Massive loop:
// For each outcome variable of USD Commitments and USD grant equivalents, 
// run these regressions, output to excel spreadsheets containing R 
// squared values, as well as regression coefficients, and graphs.


foreach Y_outcome of local outcome_vars_local {
	di "`Y_outcome'"
	
	

	foreach regr of local regressions_toloop {
		
		di "`regr'"
		
		use "$input\dac.dta", clear
		
		
		// Initial Data cleaning
			
			if strpos("`regr'", "2019"){
				// carryforward / fill downwards these variables: (ensures that 
				// if we're missing poverty level for 2019, then we get it from 
				// a prior year)
					
					local varlist usd_commitment usd_grantequiv gdppc GDP Population refugeepop_orig refugeepop_dest wgi pov1_9 pov3_8 distcap colony exports
					foreach var of local varlist {
						sort iso3c iso3c_d year
						bysort iso3c iso3c_d: carryforward `var', gen(`var'n)
						replace `var' = `var'n
						drop `var'n
						}
					
					keep if year==2019
					
				// we define a macro "collapse_type" because we want to make the 
				// code generalizable to any time frame and any type of regression. 
				// So, if the year is only 2019, it doesn't really matter if we 
				// take a sum or a mean
					
					local collapse_type sum
			}
			
			else if strpos("`regr'", "2014"){
				local collapse_type mean
			}

		// For 2014-19 regressions, we want to get the MEAN of each of these 
		// variables across 2014-2019:
			collapse (`collapse_type') `Y_outcome' GDP Pop refugeepop_orig refugeepop_dest total exports wgi pov1_9 pov3_8 (max) distcap colony, by (iso3c_d iso3c)
			

		// Modify the outcome variables to delete zeros or become categorical 
		// with logistic regression.
		
			if strpos("`regr'", "OLS positive") {
				drop if `Y_outcome' <= 0
			}
			if strpos("`regr'", "Poisson positive") {
				// For Poisson regression, we want to keep the zero values
				drop if `Y_outcome' < 0
			}
			if strpos("`regr'", "Logistic") {
				replace `Y_outcome' = `Y_outcome'>0
			}
		
		// Label variables
		
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
			else if "`Y_outcome'" == "usd_grantequiv" {
				label variable usd_grantequiv "Grant Equivalent (% of total ODA)"
			}
			
		// Generate per capita variables; relabel as per capita.
			
			foreach var of varlist refugeepop_orig refugeepop_dest GDP {
				replace `var' = `var'/ Pop
				loc lab: variable label `var'
				di "`lab'"
				label variable `var' "`lab' per capita"
			}
			
		// For the poverty headcount variables, divide by 1 M
		
			foreach var of varlist pov* {
				replace `var' = `var'/(10^6)
			}
		
		// for graphing (late on), we will need to know what the outcome variable label is
		
			local outcome_variable_label: variable label `Y_outcome'
		
		save "$input/cleaned_`regr'_`Y_outcome'.dta", replace
			
		// Now that we have a cleaned dataset for this specific regression and 
		// this specific outcome variable, we run our regressions.
		
		// First, create a dataframe which will store the regression R squared 
		// metrics for a series of stepwise regressions.
			
			tempfile coefficients
				clear
				gen country = ""
				gen pop_r2 = .
				gen pop_N=.
				gen ref_r2=.
				gen ref_N=.
				gen politics_r2=.
				gen politics_N=.
				gen full_r2=.
				gen full_N=.
			save `coefficients'
			clear
		
		
		// For each regression type, we define locals to indicate what regression 
		// options we want to use (robust), mcfadden's adjusted R squared
		
			if strpos("`regr'", "OLS") {
				local regression_type regress
				local regression_options ", robust"
				local r2_measure "e(r2)"
			} 
			else if strpos("`regr'", "Poisson") {
				local regression_type poisson
				local regression_options ", vce(robust)"
				local r2_measure "r(r2_mfadj)"
			} 
			else if strpos("`regr'", "Logistic") {
				local regression_type logistic
				local regression_options ""
				local r2_measure "r(r2_mfadj)"
			}
		
		
		// Now, we run a loop through each of these countries within the 
		// top DAC countries to do some further cleaning

			foreach i of local countries_toloop {
				use "$input/cleaned_`regr'_`Y_outcome'.dta", clear
								
				// Dependening on what country GROUP we're interested in, we 
				// will keep or delete some countries
					
					if (("`i'" != "All DAC") & ("`i'" != "Top ten DAC")) {
						keep if (iso3c_d == "`i'")
					}
					
					tab iso3c_d
					if ("`i'" == "Top ten DAC") {
						keep if (inlist(iso3c_d, "AUS", "CAN", "FRA", "DEU", "JPN") | ///
						inlist(iso3c_d, "KOR", "NLD", "NOR", "USA", "GBR"))
					}
					tab iso3c_d
				
				
				// For each donor, get the total ODA (to any country), which we'll 
				// divide individual donor-country pairs to get a proportion for the 
				// non-logistic regressions.
					
					bysort iso3c_d: egen totoda_donor = total(`Y_outcome')
					egen totoda = total(`Y_outcome')
					
					if !strpos("`i'", "DAC") {
						if (!strpos("`regr'", "Logistic")) {
								replace `Y_outcome' = `Y_outcome' / totoda_donor
							// make sure that the proportion is actually a proportion 
							// (i.e. less than 1)
								preserve
								assert `Y_outcome'<1
								collapse (sum) `Y_outcome', by (iso3c_d) 
								assert abs(`Y_outcome'-1)<=0.02
								restore
						}
						
						// IF we are in a non-DAC regression, then add a local
						// to say that we SHOULD log distcap
						
							local tolog_non_DAC "distcap"
							
					}
					
		
					
					else if strpos("`i'", "DAC") {
						// for the DAC regressions, we want to treat all of the 
						// donor countries as 1 big country. SO, we cannot keep
						// any of the bilateral variables anymore.
							
							collapse (sum) `Y_outcome' (mean) totoda GDP Pop ///
							pov1_9 pov3_8 refugeepop_dest refugeepop_orig total ///
							wgi, by (iso3c)
							
						if strpos("`regr'", "Logistic") {
						    
							// for logistic regression, we currently have a 
							// dataset of sums of counts (whether a donor gave 
							// money or did not); so, IF the outcome variable
							// is above 1, then replace it with 1, indicating 
							// that AT LEAST 1 donor country gave money to 
							// this country.
							
							replace `Y_outcome' = min(1, `Y_outcome')
						}
						if !strpos("`regr'", "Logistic") {
							
							replace `Y_outcome' = `Y_outcome' / totoda
							// make sure that the proportion is actually a proportion 
							// (i.e. less than 1)
								preserve
								assert `Y_outcome'<1
								collapse (sum) `Y_outcome'
								assert abs(`Y_outcome'-1)<=0.02
								restore
						}
						
						// IF we are in a DAC regression, then add a local
						// to say that we SHOULD log distcap
						
							local tolog_non_DAC ""
						
					}

				// Create logged variables. Relabel these variables to indicate that 
				// they've been logged. I added `Y_outcome' to loop if for logged 
				// outcome variable.
				
					if strpos("`regr'","OLS ln") {
						local addtl_to_log "`Y_outcome'"
					}
					else if !strpos("`regr'","OLS ln") {
						local addtl_to_log ""
					}
					
					foreach var of varlist Population GDP `tolog_non_DAC' `addtl_to_log' {
						replace `var' = ln(`var')
						loc lab: variable label `var'
						di "`lab'"
						label variable `var' "Log `lab'"
					}
				
				// Make sure that we have 1 recip only for the DAC
				// regressions, but only 1 donor-recip pair for the non-DAC 
				// regressions
					if strpos("`i'", "DAC") {
						preserve
							gen num = 1
							collapse (sum) num, by (iso3c) 
							assert abs(num-1)<=0.0001
						restore
					}
					if (!strpos("`i'", "DAC")) {
						preserve
							gen num = 1
							collapse (sum) num, by (iso3c_d iso3c) 
							assert abs(num-1)<=0.0001
						restore
					}
				
				save "$input/cleaned_`regr'_`Y_outcome'_`i'.dta", replace
		}
	
	
			
			foreach i of local countries_toloop {
				use "$input/cleaned_`regr'_`Y_outcome'_`i'.dta", clear
				
				
				// We apply some operations to DAC regressions and some operations to 
				// non-DAC regressions, so we have to define a local for these:
					if !strpos("`i'", "DAC") {
						local bilateral_vars "dist colony exports"
						local extra_vars_nonDAC "i.colony exports dist"
					}
					else if strpos("`i'", "DAC") {
						local bilateral_vars ""
						local extra_vars_nonDAC ""
					}
				
				di "`bilateral_vars'"

				// drop missing variables na.omit
					foreach x of varlist `Y_outcome' GDP Pop refugeepop_dest ///
					refugeepop_orig total wgi `bilateral_vars' {
						drop if (`x' == .)
					}
						
				// Now, we get each of the stepwise regressions, we save each of 
				// the variables into local macros, and then we input these local 
				// macros into the dataframe that we made previously.
						
						qui:`regression_type' `Y_outcome' GDP Pop `regression_options'
							qui: fitstat
							loc pop_r2 = ``r2_measure''
							loc pop_N  = `e(N)'
							
						qui:`regression_type' `Y_outcome' GDP Pop refugeepop_dest ///
						refugeepop_orig `regression_options'
							qui: fitstat
							loc ref_r2 = ``r2_measure''
							loc ref_N  = `e(N)'
							
						qui:`regression_type' `Y_outcome' GDP Pop refugeepop_dest ///
						refugeepop_orig total wgi `regression_options'
							qui: fitstat
							loc politics_r2 = ``r2_measure''
							loc politics_N  = `e(N)'
						
						// if it's not a DAC regression, then I can add 
						// the donor-colony relationships:
						
						if (!strpos("`i'", "DAC")) {
							qui:`regression_type' `Y_outcome' GDP Pop refugeepop_dest ///
							refugeepop_orig total wgi distcap i.colony exports `regression_options'
								qui: fitstat
								loc full_r2 = ``r2_measure''
								loc full_N  = `e(N)'
						}
						else if (strpos("`i'", "DAC")) {
						    loc full_r2 = .
							loc full_N  = .
						}
						
						clear
					
					// open up a new tempfile with coefficients. store these 
					// local macros, and then append it to the existing temp 
					// file.
						
						tempfile `i'_coefs
							set obs 1
							gen country = "`i'"
							gen pop_r2 = `pop_r2'
							gen pop_N=`pop_N'
							gen ref_r2=`ref_r2'
							gen ref_N=`ref_N'
							gen politics_r2=`politics_r2'
							gen politics_N=`politics_N'
							gen full_r2=`full_r2'
							gen full_N=`full_N'
						save ``i'_coefs'
					
					// check that for the full DAC regression, the R^2 is empty, 
					// since the full regression includes bilateral variables
					// that we can't run.
					
						if (strpos("`i'", "DAC")) {
							assert full_r2==.
							assert full_N==.
						}
						
						use `coefficients', clear
						append using ``i'_coefs'
						save `coefficients', replace	
			}
			
			

	// Do the same thing for the three poverty regressions (using GDP vs. the two
	// poverty headcount measures), except here, we do not care about deleting all
	// observations that are missing any  variable: (previously, with the stepwise
	// regressions, we wanted to make sure that the set of country-donors remains
	// the SAME for  each regression that we carry out.). Output these regressions 
    // as tables.

		foreach i of local countries_toloop{
			use "$input/cleaned_`regr'_`Y_outcome'_`i'.dta", clear
			
			// We apply some operations to DAC regressions and some operations to 
			// non-DAC regressions, so we have to define a local for these:
				if !strpos("`i'", "DAC") {
					local bilateral_vars "dist colony exports"
					local extra_vars_nonDAC "i.colony exports dist"
				}
				else if strpos("`i'", "DAC") {
					local bilateral_vars ""
					local extra_vars_nonDAC ""
				}
			
			
				#delimit ;
					`regression_type' `Y_outcome' 
						GDP Pop refugeepop_orig refugeepop_dest total wgi 
						`extra_vars_nonDAC' `regression_options';
						outreg2 using "$regout1", append ctitle("`i'_`regr'_`Y_outcome'") 
						label dec(7);
					
					`regression_type' `Y_outcome' 
						pov1_9 refugeepop_orig refugeepop_dest total 
						wgi `extra_vars_nonDAC' `regression_options';
						outreg2 using "$regout2", append ctitle("`i'_`regr'_`Y_outcome'") 
						label dec(7);
					
					`regression_type' `Y_outcome' 
						pov3_8 refugeepop_orig refugeepop_dest total 
						wgi `extra_vars_nonDAC' `regression_options';
						outreg2 using "$regout2", append ctitle("`i'_`regr'_`Y_outcome'")
						label dec(7);
				#delimit cr
				
				
		}
		
	// save the R squared values from my stepwise regressions into an 
	// excel sheet.
		use `coefficients', clear
		label variable country "Country"
		label variable pop_r2 "GDP per capita, Population"
		label variable ref_r2 "GDP per capita, Population, Refugee In/Outflows"
		label variable politics_r2 "GDP per capita, Population, Refugee In/Outflows, Governance Scores"
		label variable full_r2 "Full regression (distance, colony, exports)"
		export excel using "$input/r_squared_`regr'_`Y_outcome'.xlsx", firstrow(varlabels) replace

	// Aesthetics
	
		replace country = "All DAC" if country == "All DAC"
		replace country = "Australia" if country == "AUS"
		replace country = "Canada" if country == "CAN"
		replace country = "France" if country == "FRA"
		replace country = "Germany" if country == "DEU"
		replace country = "Japan" if country == "JPN"
		replace country = "Korea" if country == "KOR"
		replace country = "Netherlands" if country == "NLD"
		replace country = "Norway" if country == "NOR"
		replace country = "United States" if country == "USA"
		replace country = "United Kingdom" if country == "GBR"
		
}
}
exit


	// Graphing R squared values: ==============================
	
	
		
	// make sure that the number of observations per regression is around the 
	// same (give or take 2)
	
		assert abs(pop_N - ref_N) <=2
		assert abs(ref_N - politics_N) <=2
		assert abs(politics_N - full_N) <=2
	
		graph hbar (asis) pop_r2 ref_r2 politics_r2 full_r2, over(country, sort(full_N) descending axis(lcolor(none))) bar(1, lcolor(none)) bar(2, lcolor(none)) bar(3, lcolor(none)) bar(4, lcolor(none)) blabel(bar, size(tiny) format(%7.2g)) yscale(range(0 1)) yscale(noline) ylabel(0(.2)1, ticks nogrid) legend(position(12)) scheme(plotplainblind) graphregion(fcolor(none) ifcolor(none)) plotregion(fcolor(none) ifcolor(none)) title(`regr') subtitle(`outcome_variable_label')
		
		gr_edit .style.editstyle declared_xsize(80) editcopy
		gr_edit .style.editstyle declared_ysize(60) editcopy
		graph export "$output/rsquared_`regr'_`Y_outcome'.pdf", replace
		graph export "$output/rsquared_`regr'_`Y_outcome'.png", replace



// Table of percent ODA spending by income group
	use "$input\dac.dta", clear
	collapse (sum) `Y_outcome', by (donorname incomegroupname)
	bysort donor: egen tot = total(usd_grantequi*v)
	replace usd = usd / tot *100
	gsort -tot
	egen c = rank(-tot), track
	drop if c > 30
	drop tot c
	sort donor income
	
	graph hbar (asis) `Y_outcome', over(incomegroupname, gap(*1.2) label(labsize(6-pt))) over(donorname, gap(*1.2) label(labsize(6-pt))) bar(1, fcolor(orange_red) lcolor(dkorange)) ytitle(Percent of Total 2019 Commitment (%)) ytitle(, size(6-pt)) ylabel(, labsize(6-pt)) ymtick(, labsize(huge)) scheme(plotplain)
	gr_edit .style.editstyle declared_ysize(4) editcopy
	gr_edit .style.editstyle declared_xsize(4) editcopy
	graph export "$output/bar_income_changeflows.pdf", replace
	graph export "$output/bar_income_changeflows.png", replace

	

	
exit




