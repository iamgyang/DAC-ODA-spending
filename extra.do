

// Individual Country Level Regressions for ALL Countries ========

	use "$input/cleaned.dta", clear
			eststo clear
			eststo drop r1 r2
			// First, regress on non-scaled estimates. Then, regress on scaled.
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

			// Output regression to word with standard errors and adjusted R^2:
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
				addnote("Each observation is a country-donor pair. For scaled regressions (2,4), log variables and dummy variables are not scaled. All variables labeled 'Recip.' indicates that the variable was matched to the recipient country only, and not bilaterally. Some countries has limited numbers of colonies, such as Japan (Micronesia) or Australia (Papua New Guinea and Nauru, and so regression slope estimates are not robust.")
				;
				#delimit cr


// For each donor country, do a regression and output
	levelsof donor
	foreach lev in `r(levels)' {
		clear all
		use "$input/cleaned.dta", clear
		keep if donor == "`lev'"
		eststo clear
		eststo drop r1 r2
		// First, regress on non-scaled estimates. Then, regress on scaled.
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

		// Output regression to word with standard errors and adjusted R^2:
		// taken from http://repec.org/bocode/e/estout/esttab.html

			#delimit ;
			esttab r1 r2 using "$regout1", 
			append
			se ar2 
			label title(`lev' ODA Commitments 2019)
			nonumbers 
			mtitles(
			"(1) Log Grant-equivalent Disbursement" 
			"(2) Log Grant-equivalent Disbursement (indep. var scaled)"
			) 
				addnote("Each observation is a country-donor pair. For scaled regressions (2,4), log variables and dummy variables are not scaled. All variables labeled 'Recip.' indicates that the variable was matched to the recipient country only, and not bilaterally. Some countries has limited numbers of colonies, such as Japan (Micronesia) or Australia (Papua New Guinea and Nauru, and so regression slope estimates are not robust.")
			;
			#delimit cr
	clear results
	clear rngstream
	}

	exit

	

// REGRESSION DIAGNOSTICS =================================

// esttab, se ar2 label title(DAC ODA Commitments in 2019)
// replace frag label 
// b(a2) se(a2)
// addnote("Each observation is a country-donor pair.")
// nogaps nomtitles nodepvars nonum nolines



// get residuals
predict r
// check residuals are normal
histogram r, bin(50)
sort r
drop if r == .
// this indicates that France's ODA to Tonga and Australia's ODA to St. Vincent &
// the Grenadines are smaller than we'd expect.
// what we'd expect:
list donor recip r in 1/10
// this indicates that UK's ODA to India and Nigeria are very large compared to 
// what we'd expect:
list donor recip r in -10/l

hilo r state
list r crime pctmetro poverty single if abs(r) > 2
predict lev, leverage
stem lev
hilo lev state, show(5) high
// Generally, a point with leverage greater than (2k+2)/n should be carefully 
// examined. Here k is the number of predictors and n is the number of 
// observations. In our example, we can do the following.
display (2*3+2)/51
list crime pctmetro poverty single state lev if lev >.156
lvr2plot, mlabel(recipientname)
list state crime pctmetro poverty single if state=="dc" | state=="ms"
predict d, cooksd
list crime pctmetro poverty single state d if d>4/51
predict dfit, dfits
list crime pctmetro poverty single state dfit if abs(dfit)>2*sqrt(3/51)
dfbeta
list state DFpctmetro DFpoverty DFsingle in 1/5

