clear
set more off
set scheme uncluttered 
scalar drop _all
capture log close
log using hypotheticalwhite2020mortality.log, replace
set type double

* --------------------------------
* TITLE: hypotheticalwhite2020mortality.do
* CREATED BY: Elizabeth Wrigley-Field
* LAST CHANGED: June 26, 2020
* --------------------------------

/* 
Contents: How many additional deaths for whites could happen in 2020 to 
make it still true that blacks in every prior year have higher mortality than
whites in 2020?

This version current as of June 24, 2020 CDC Covid-19 data release.	
		
Detailed Contents:
1. Create numerators: race-specific life tables
2. Create denominators: annual age structures by race
3. Crude death rates
4. Indirectly age-standardized mortality (Comparative Mortality Ratio)
5. Directly age-standardized mortality, assuming 2020 excess mortality
	proportional to race-specific all-cause mortality
6. Recreates #5 with 10-year age units for comparison
7. Create age pattern of white Covid mortality, based on empirical CDC
	data, 10-year age units
8. Direct age standardization, empirical Covid age distribution, 
	10-year age units
9. Create age pattern of white Covid mortality in single-year age units,
	based on interpolation and extrapolation
10. Direct age standardization, empirical Covid age distribution, 
	single-year age units
11. Life expectancy, assuming 2020 excess mortality proportional to 
	race-specific all-cause mortality	
12. Life expectancy, empirical Covid age distribution, single-year 
	age units
13. Metrics comparing hypothetical deaths to current Covid mortality
14. Life expectancy, empirical Covid age distribution, 10-year age units
15. Graph of directly standardized mortality and life expectancy, 
	1900-2017 (NCHS data)
16. Graphs of main outcomes
*/

* --------------------------------
* 1. Create numerators: race-specific life tables
* --------------------------------

local r13 0 // white
local r16 1 // black

forvalues y=2006/2017 {
	foreach n in 13 16 {
			if `y'>2010 {
				import excel "Table`n'_`y'.xlsx", ///
					cellrange(A3:G104) firstrow
			}
			else if `y'==2010 {
				import excel "Table`n'_`y'.xlsx", ///
					cellrange(A7:G108) firstrow
			}
			else if `y'==2009 {
				import excel "Table`n'_`y'.xls", ///
					cellrange(A7:G108) firstrow
			}
			else if `y'==2008 {
				import excel "Table`n'_`y'.xls", ///
					cellrange(A3:G104) firstrow
				replace Ageyears = "79-80" if Ageyears=="70-80"
					// corrects error in NCHS spreadsheet (for both races)
			}
			else if `y'==2007 { 
				import excel "Table`n'_`y'.xls", ///
					cellrange(A4:G105) firstrow
			}
			else { // 2006
				local 2006_`n' = `n'-9 // 2006 uses Tables 4 and 9
				import excel "Table`2006_`n''_`y'.xls", ///
					cellrange(A3:G104) firstrow
			}
			gen black = `r`n''
			gen year = `y'
			gen version = 1
			gen yearnum = year + .1
			save "Table`n'_`y'.dta", replace
			clear
			
			if `y'>=2007 & `y'<=2009 {
				import excel "Table`n'_Intercensal_`y'.xlsx",  ///
					cellrange(A7:G108) firstrow
				gen black = `r`n''
				gen year = `y'
				gen version = 2
				gen yearnum = year + .2
				save "Table`n'_`y'_ver2.dta", replace
				clear
			}
			
			if `y'==2010 | `y'==2011 {
				import excel "table`n'_Updated_ClassificationRatios_`y'.xlsx", ///
					cellrange(A3:G104) firstrow
				gen black = `r`n''
				gen year = `y'
				gen version = 2
				gen yearnum = year + .2
				save "Table`n'_`y'_ver2.dta", replace
				clear
			}
	}
}
forvalues y=2006/2017 {
	foreach n in 13 16 {
		append using "Table`n'_`y'.dta"
		
		if `y'>=2007 & `y'<=2011 {
			append using "Table`n'_`y'_ver2.dta"
		}
	}
}

* clean age variable
foreach a in A Age Ageyears {
	replace `a' = "100" if `a'=="100 and over"
	replace `a' = subinstr(`a',"-","–",.) // hyphen vs n-dash
	split `a', parse("–") destring
}
ren A1 age
replace age = Ageyears1 if !missing(Ageyears1)
replace age = Age1 if !missing(Age1)
tab age black, m
tab yearnum black, m
drop A Age Age1 Ageyears1 Ageyears A2 Age2 Ageyears2

* Make consistent exposure and mortality
replace Lx = L if year==2011 & version==1
drop L
gen mx = dx/Lx

* Create open interval at age 94 (max age with good population estimates later)
* It's the deaths above 94 (dx above 94 = lx at 94) divided by
* person-years lived above age 94 (Tx)
replace mx = lx / Tx if age==94
sum mx if age==94
replace Lx = Tx if age==94
sum Lx if age==94
replace qx = 1 if age==94
drop if age>94

gen mx100k = mx * 100000

* generate nax. PHG p. 84: If age interval is short, option (b) is best	
* I use nax from main life table. In practice, changing mortality would
* change the nax values but with single-year age units this is essentially 
* irrelevant
sum age
scalar maxage_LT = r(max) // used to make formulas general
scalar n = 1 // used to make the nax formula general to other units
gen nax = (Lx - n*lx[_n+1])/dx
sum age
replace nax = Lx/lx if age==maxage_LT
table age, c(min nax max nax)

table yearnum black if age==0, c(p50 ex)
table year black if age==0, c(min ex max ex)

save lifetables.dta, replace

* --------------------------------
* 2. Create denominators: annual age structures by race
* --------------------------------

* do agestructure_ipums.do
	// creates agestructure_ipums.dta from raw IPUMS data
use agestructure_ipums.dta, clear

keep if hispan==0
keep if race<=2
gen black = (race==2)

collapse (count) pop=pernum [pweight=perwt], by(year black age)

* median ages in 2017
table black if year==2017 [fweight=pop], c(p50 age)

save population_age_race.dta, replace

tab age black
	// above age 94 is not populated in all years for Blacks

* Graph age distributions (across years)
* For talk in s1rcolor: black = cyan, white = pink
/* Color scheme:
White = rgb(37,102,118)
Black = rgb(113,211,244)
*/
twoway (hist age [fweight=pop] if black==0, color("37 102 118") ///
		width(5) legend(label(1 "White"))) ///
	(hist age [fweight=pop] if black==1, fcolor(none) ///
		lcolor("113 211 244") lwidth(thick) ///
		width(5) legend(label(2 "Black"))), ///
		legend(on) ///
		/// title("Age Distributions by Race") ///
		xtitle("") ytitle("") ///
		saving(agecomp.gph, replace)
		graph export agecomp.png, replace
	
* --------------------------------
* 3. Merge and create annual crude mortality by race
* --------------------------------

merge 1:m year black age using lifetables.dta
sort yearnum black age

* Graph age-specific death rates
gen lnmx100k = ln(mx100k)
line lnmx100k age if black==0 & year==2017 & age<=94, ///
		lcolor("113 211 244") lwidth(thick) ///
		legend(label(1 "White")) || ///
	line lnmx100k age if black==1 & year==2017 & age<=94, ///
		lcolor("37 102 118") lwidth(thick) ///
		legend(label(2 "Black")) ///
	legend(on) ///
	/// ytitle("Logged Deaths Per 100,000") xtitle("Age") ///
	ytitle("") xtitle("") ///
	/// title("Log Mortality by Race") ///
	saving(mx.gph, replace)
	graph export mx.png, replace

* Make population totals for the open interval
gen pop94andup_sep = pop if age>=94
by yearnum black: egen pop94andup = total(pop94andup_sep)
replace pop = pop94andup if age==94
drop pop94*

drop if age>94

*** Construct crude mortality
* get age distribution
by yearnum black: egen totalpop = total(pop)
gen c_a = pop / totalpop

* Crude Death Rate
gen cdr_piece = c_a * mx
replace cdr_piece = cdr_piece * 100000 // deaths per 100,000
by yearnum black: egen cdr = total(cdr_piece)

table yearnum black, c(min cdr)

* Minimum black CDR
sum cdr if black==1
scalar blackmin_cdr = r(min)
sum year if cdr==blackmin_cdr & black==1
scalar year_blackmin_cdr = r(max)

* Difference between 2017 [most recent] white mortality and minimum black
sum cdr if black==0 & year==2017
scalar white2017_cdr = r(mean)

scalar list blackmin_cdr white2017_cdr
	// Upshot: White CDRs are higher than black CDRs due to the age composition

save mortality_comparison.dta, replace

* --------------------------------
* 4. Create indirectly age-standardized annual mortality by race
* 		Also known as Comparative Morality Ratio
* --------------------------------
**** A. Create age standard
* Treat the whole pooled super-population, across years, as the population
* For the 5 years (2007-2011) with two distinct life tables, weight each by half
gen pop_weight = pop if year>2011 | year==2006
replace pop_weight = pop/2 if year>=2007 & year<=2011
bysort age: egen pop_age_pooled = total(pop_weight)
gen deaths_pop = pop * mx
gen deaths_pop_weighted = deaths_pop if year>2011 | year==2006
replace deaths_pop_weighted = deaths_pop/2 if year>=2007 & year<=2011 

by age: egen deaths_age_pooled = total(deaths_pop_weighted)
gen dr_standard = deaths_age_pooled / pop_age_pooled
drop pop_weight deaths_pop_weighted //

replace dr_standard = dr_standard * 100000
table age, c(min dr_standard)

**** B. Age standardize mortality
* Create expected CDR
gen cdr_expected_piece = c_a * dr_standard 
bysort yearnum black: egen cdr_expected = total(cdr_expected_piece)
drop cdr_expected_piece
gen cmr = cdr / cdr_expected
table yearnum black, c(min cdr min cdr_expected min cmr)

* Difference between 2017 white CMR and minimum black CMR
sum cmr if black==1 
scalar blackmin_cmr = r(min)
sum year if cmr==blackmin_cmr & black==1
scalar year_blackmin_cmr = r(max)
sum cmr if black==0 & year==2017
scalar white2017_cmr = r(mean)
scalar excessneeded_cmr = scalar(blackmin_cmr) - scalar(white2017_cmr) 
	// amount of excess white mortality
	// needed for it *not* to be the case that blacks have long had greater
	// mortality than whites do even with the excess
scalar list blackmin_cmr white2017_cmr excessneeded_cmr
	// CMR difference of .21 (1.18 vs. .97)

* How many excess *deaths* are needed to get white CMR up to black level?
	// Crude death rates are deaths / pop, so the CMR (ratio of two CDRs)
	// is also a ratio of actual to expected total deaths. 
sum cdr_expected if black==0 & year==2017
scalar cdrexpected_white2017 = r(mean)
scalar deathsneededper100k = excessneeded_cmr * cdrexpected_white2017
	// this is the additional deaths per 100,000 needed for whites
scalar list deathsneededper100k
	// 237.5403

scalar wnh_pct = .604 // percent of pop that is white non-Hispanic
scalar popUS = 328239523 // total US pop estimate, July 1, 2019
						// note this is an underestimate since a year old
scalar popw = wnh_pct * scalar(popUS)
scalar bnh_pct = .134 // excludes multi-racial
scalar popb = bnh_pct * popUS
	* all above from https://www.census.gov/quickfacts/fact/table/US/RHI825218

scalar deathsneeded_cmr = deathsneededper100k * scalar(popw) / 100000	
scalar list deathsneeded_cmr
	* 470,939.48

scalar deaths_US_2017 = 2813503	
	// from https://www.cdc.gov/nchs/data/nvsr/nvsr68/nvsr68_06-508.pdf
	// Table C, p. 9
scalar deaths_w_2017_NCHS = 2179857
	// same source Table D, p. 12
	// also available here: https://www.cdc.gov/nchs/fastats/white-health.htm
scalar deaths_w_2019hyp = white2017_cdr * (scalar(popw)/100000)
	// I use this estimate, which uses the 2017 CDR and the 2019 
	// population size to estimate 2019 white deaths, so that the
	// hypothetical increase in mortality from "2019" (2017) uses the same
	// denominators as used in estimating excess deaths needed
scalar list deaths_w_2017_NCHS deaths_w_2019hyp
disp deathsneeded_cmr / deaths_US_2017
disp deathsneeded_cmr / deaths_w_2019hyp
	// .21774194
	
save, replace	
	
* --------------------------------
* 5. Direct age standardization
* --------------------------------	
* Use the total US population as the age standard
/* In this metric, the age structure of covid deaths matters
An analytical solution (I originally derived based on the Vaupel and Zhang 
formula but it can be derived more simply) says that if white mortality increases
proportionally (so if Covid deaths are proportional to all-cause mortality),
so new mortality is m * normal white mortality, then m = the ratio of 
black to white standardized normal mortality.
*/
* --------------------------------
* A. Standard age composition
* --------------------------------	
	
use USagestructure_indlevel.dta, clear

drop if missing(age)

gen obs=1
collapse (count) popUS=obs [fweight=perwt], by(age)

save USagestructure.dta, replace

* smooth (minimally)
lowess popUS age, bwidth(.15) gen(popsmoothUS)
	// bandwidth of .8 (default) is too strong: smooths over baby bump
	// bandwidth chosen by trial and error to create age structure that captures
	// theoretically expected and consistent variation but smoothes year-to-year 
	// jaggedness
	// .3 is reasonable, .2 good, .5 too stringent, .1 maybe not enough

* Collapse ages above 94 [done after smoothing so the age units are equal]
replace age = 94 if age>94 // max age with coverage of small racial groups
collapse (sum) popUS=popUS popsmoothUS=popsmoothUS, by(age)

egen totalpopUS = total(popUS)
gen c_a_st_raw = popUS/totalpopUS
egen totalpopsmoothUS = total(popsmoothUS)
gen c_a_st = popsmoothUS/totalpopsmoothUS

list

save USagestructure_smooth.dta, replace

merge 1:m age using mortality_comparison.dta, gen(_merge_direct)
drop _merge_direct
	// should be all _merge==3; is -- EWF, 6/23/20

* --------------------------------
* B. Standardize mortality 
* --------------------------------	
sort yearnum black age

* CDR_standardized is sum of actual mx times standard c_a

gen cdr_st_piece = mx*c_a_st * 100000
by yearnum black: egen cdr_st = total(cdr_st_piece)

table yearnum black, c(p50 cdr_st)

sum cdr_st if black==1 
scalar blackmin_cdrst = r(min)
sum year if cdr_st==blackmin_cdrst & black==1
scalar year_blackmin_cdrst = r(max)

sum cdr_st if black==0 & year==2017
scalar white2017_cdrst = r(mean)

* --------------------------------
* C. Derive excess deaths required to equalize races using proportional
* 		mortality assumption (excess deaths proportional to all-cause)
* --------------------------------	

scalar excessmultiplier_stprop = blackmin_cdrst / white2017_cdrst
scalar whiteCDRneeded_stprop = excessmultiplier_stprop * white2017_cdr
scalar deathsneeded_stprop = (excessmultiplier_stprop - 1) * white2017_cdr * popw / 100000
scalar list	excessmultiplier_stprop whiteCDRneeded_stprop deathsneeded_stprop
	// excessmultiplier_stprop =  1.1800391
	// whiteCDRneeded_stprop =  1290.0899
	// deathsneeded_stprop =  390227.87

* Test this result: standardize this counterfactual white mortality and
* make sure it equals blackmin_st	
gen mx_cf_stprop = excessmultiplier_stprop * mx
gen cdr_st_test_piece = mx_cf_stprop * c_a_st * 100000
by yearnum black: egen cdr_st_test = total(cdr_st_test_piece)
sum cdr_st_test if black==0 & year==2017
disp r(mean)
scalar list blackmin_cdrst // should be the same; are. --EWF, 6/23/2020
	
disp (deathsneeded_stprop / popw) * 100000 // excess CDR, estimated from direct standardization	
	* 196.82963
	
disp deathsneeded_stprop / deaths_w_2019hyp
	* .18003913
	
gen truepopw2017 = scalar(popw) * c_a if black==0 & year==2017
	// used later, made now so it's in both versions of the file below
	
save mortality_comparison.dta, replace

keep if black==0 & year==2017
save mortality_comparison_whites2017.dta, replace
	// used when making Covid age distribution below
	
* --------------------------------
* 6. For comparison: directly standardized using 10-year units
* --------------------------------	
use mortality_comparison.dta, clear
recode age (0=0) (1/4=1), gen(agegroup)
replace agegroup = 10*floor((age+5)/10) - 5 if age>=5
replace agegroup = 85 if age>85 & age<.	
	
collapse (sum) popsmooth Lx dx ///
	(mean) totalpopsmooth, by(yearnum black agegroup)
gen year = floor(yearnum)
gen c_a_cdrst10 = popsmooth / totalpopsmooth
sort yearnum black agegroup
by yearnum black: egen testc = total(c_a_cdrst10)
sum testc // should equal 1; does. --EWF, 6/14/20
drop testc

gen mx = dx/Lx
gen mx100k = mx * 100000
gen cdr_st_piece = mx100k * c_a_cdrst10
by yearnum black: egen cdr_cdrst10 = total(cdr_st_piece)
sum cdr_cdrst10 if black==1
scalar blackmin_cdrst10 = r(min)
sum yearnum if cdr_cdrst10==blackmin_cdrst10 & black==1
scalar year_blackmin_cdrst10 = r(max)-.1
sum cdr_cdrst10 if black==0 & yearnum==2017.1
scalar white2017_cdrst10 = r(mean)

scalar excessmultiplier_cdrst10 = blackmin_cdrst10 / white2017_cdrst10
scalar whiteCDRneeded_cdrst10 = excessmultiplier_cdrst10 * white2017_cdrst10
scalar deathsneeded_cdrst10 = ///
	(excessmultiplier_cdrst10 - 1) * white2017_cdr * popw / 100000
scalar list	excessmultiplier_cdrst10 whiteCDRneeded_cdrst10 deathsneeded_cdrst10
	// excessmultiplier_cdrst10 =  1.1943056
	// whiteCDRneeded_cdrst10 =  1009.3357
	// deathsneeded_cdrst10 =  421,149.88

disp deathsneeded_cdrst10 / deaths_w_2019hyp
	// .1943056

* --------------------------------
* 7. Create Covid age structure based on empirical data
* --------------------------------	
clear

* Import CDC Covid data 
* Current version last updated June 24, 2020, based on data through June 20, 2020
import delimited "Deaths_involving_coronavirus_disease_2019__COVID-19__by_race_and_Hispanic_origin_group_and_age__by_state_2020-06-24.csv", encoding(ISO-8859-1)clear
save CDC_Covid_raw.dta, replace

* Get white totals by age; use Covid list
* Covid & pneumonia seems to be both; with added flu, it's explicitly "or flu"
collapse (sum) pneu=pneumoniadeaths flu=influenzadeaths ///
	covid=covid19deaths covidpneu= pneumoniaandcovid19deaths ///
	covidinfpneu=pneumoniainfluenzaorcovid19death totaldeaths, ///
	by(race agegroup)
	
drop if agegroup=="All Ages"	
replace agegroup = "0" if agegroup=="Under 1 year"
replace agegroup = "85" if agegroup=="85 years and over"
split agegroup, parse("-") destring	
drop agegroup agegroup2
ren agegroup1 age
sort age
list	
* Age units: 0, 1, then 5-85 in 10-year units
collapse (sum) pneu flu covid covidpneu covidinfpneu, by(age race)
sort race age

save CDC_Covid_all.dta, replace
	
keep if race=="Non-Hispanic White" | race=="Non-Hispanic Black"
gen black=(race=="Non-Hispanic Black")

/* 
keep if race=="Non-Hispanic White"
drop race
list
save CDC_Covid_whites.dta, replace
*/

* Merge in denominators
* Note that CDC data use 10-year age units
merge 1:m age black using mortality_comparison.dta, gen(_mergeCDC)
	* Unmatched are blacks and other age units
recode age (0=0) (1/4=1), gen(agegroup)
replace agegroup = 10*floor((age+5)/10) - 5 if age>=5
replace agegroup = 85 if age>85 & age<.
table agegroup, c(min age max age) // correct

bysort yearnum agegroup black: egen pop_agegroup = total(pop)
table agegroup black if year==2017, c(min pop_agegroup)

* Average age in each agegroup (used to interpolate Covid rates later)
preserve
collapse (mean) meanage=age [fweight=pop], by(year black agegroup)
	// the two "versions" of each year have the same IPUMS age distribution
save meanage_agegroup.dta, replace
restore
merge m:1 year black agegroup using meanage_agegroup, gen(_merge_meanage)
drop _merge_meanage

* Covid rates by agegroup
gen covidrate = covid / pop_agegroup if year==2017
sort year black agegroup
line covidrate agegroup if black==0 // 85+ age group over 1% mortality

gen covidrate100k = covidrate * 100000
table agegroup if black==0, c(p50 covidrate100k) // white covid mortality by age

save CDC_Covid_rates.dta, replace

* --------------------------------
* 8. Direct standardization with actual Covid age distribution,
* 10-year age units (as reported by the CDC)
* --------------------------------

/* Steps: Make c_a for age groups; standardize black and white mortality
using those age groups; do the proportional (m-method) above for comparison;
multiply covid rate by standard c_a; sum those up to get weighted covid rate,
weighted by standard age distribution; divide that by the difference between
standardized black and white mortality. That's the multiplier on the population
(or per 100,000 population) to get the deaths needed. */

use CDC_Covid_rates, clear

collapse (sum) dx Lx popsmooth c_a ///
	(min) totalpopsmooth covidrate100k, by(yearnum black agegroup)
gen year = floor(yearnum)
gen truepopw2017 = scalar(popw) * c_a if black==0 & year==2017

gen c_a_st = popsmooth/totalpopsmooth // age structure in 10-year units
bysort yearnum black: egen testc = total(c_a_st)
sum testc // should all equal 1; they do. --EWF, 6/23/20
drop testc
table age, c(p50 c_a_st p50 c_a)

* Make 5-year mortality, weighted by exposure
gen mx = dx/Lx
gen mx100k = mx * 100000
table agegroup year black, c(min mx)

* Standardize mortality using 5-year age units
gen temp_cdr_st_piece = mx100k * c_a_st
by yearnum black: egen cdr_cdrst10covid = total(temp_cdr_st_piece)
table yearnum black, c(min cdr_cdrst10covid)

* Get difference we need covid to contribute
sum cdr_cdrst10covid if black==1 
scalar blackmin_cdrst10covid = r(min)
sum year if cdr_cdrst10covid == blackmin_cdrst10covid
scalar year_blackmin_cdrst10covid = r(max)
sum cdr_cdrst10covid if black==0 & year==2017
scalar white2017_cdr_cdrst10covid = r(mean)
scalar diffneeded_cdr_cdrst10covid = blackmin_cdrst10covid - white2017_cdr_cdrst10covid
scalar list blackmin_cdrst10covid white2017_cdr_cdrst10covid diffneeded_cdr_cdrst10covid

* Make standardized contribution of covid mortality
gen temp_covid_piece = covidrate100k * c_a_st
by yearnum black: egen covidcontrib_cdrst10 = total(temp_covid_piece), missing
sum covidcontrib_cdrst10 if black==0 & year==2017
scalar covid_contrib_cdrst10 = r(mean)

* How many times does that contribution need to occur to raise white mortality
* to black levels?
scalar covid_units_cdrst10 = diffneeded_cdr_cdrst10covid / covid_contrib_cdrst10
scalar list covid_contrib_cdrst10 covid_units_cdrst10
	// covid_units 3.687642, meaning current Covid mortality has to be multiplied by ~3.7
gen covid_mx100k = covidrate100k * covid_units_cdrst10
list agegroup mx100k covidrate100k covid_mx100k if black==0 & year==2017

* Check this result
gen mx_cf = mx100k + covid_mx100k
gen temp_cdr_cf_piece = mx_cf * c_a_st
by yearnum black: egen cdr_st_cf_cdrst10covid = total(temp_cdr_cf_piece)
sum cdr_st_cf_cdrst10covid if black==0 & year==2017
disp r(mean)
scalar list blackmin_cdrst10covid // should be the same; is --EWF, 6/26/2020

* How many deaths is that?
gen deathsage_covid = truepopw2017 * covid_mx100k / 100000 if black==0 & year==2017
by yearnum black: egen deaths_cf = total(deathsage_covid)
sum deaths_cf if black==0 & year==2017
scalar deathsneeded_cdrst10covid = r(mean)
scalar list deathsneeded_cdrst10covid
	// 407,793.86 --> June 17 release
	// 407,772.37 --> June 24 release

disp deathsneeded_cdrst10covid / deaths_w_2019hyp
	// .18707368 --> June 17 release
	// .18706382 --> June 24 release
	
list age c_a truepopw2017 covidrate100k covid_mx deathsage_covid ///
	deaths_cf if black==0 & year==2017
list age c_a c_a_st if black==0 & year==2017

table age if black==0 & year==2017, ///
	c(min mx min covid_mx min mx_cf)

* age distribution of all-cause mortality vs. covid mortality
* for whites
foreach m in mx covid_mx {
	gen weight_`m' = round(`m' * 100000000) 
		// creates integer weights without loss of precision
}

recode agegroup (0/14=1) (15=2) (25=3) (35=4) (45=5) (55=6) ///
	(65=7) (75=8) (85=9), gen(agecat)
keep if black==0 & year==2017
gen obs=1
preserve
collapse (sum) deaths_2017=obs [fweight=weight_mx], by(agecat)
save mx_whites2017.dta, replace
restore
collapse (sum) deaths_covid=obs [fweight=weight_covid_mx], by(agecat)
merge 1:1 agecat using mx_whites2017.dta
twoway (hist agecat [fweight=deaths_covid], bin(9) ///
		color("67 220 197") ///
		legend(label(1 "Covid-19"))) ///
	(hist agecat [fweight=deaths_2017], bin(9) ///
		fcolor(none) lcolor("105 27 158") lwidth(thick) ///
		legend(label(2 "All-cause, pre-Covid-19"))), ///
	xlabel(1 "0-14" 2 "15-29" 3 "25-34" 4 "35-44" 5 "45-54" 6 "55-64" ///
			7 "65-74" 8 "75-84" 9 "85+") ///
		/// xtitle("Age") ytitle("Density") ///
		xtitle("") ytitle("") ///
	legend(on) ///
	/// title("Age Pattern of White Mortality Rates") ///
	saving(age_covid_other.gph, replace)
	graph export age_covid_other.png, replace

twoway (hist agecat [fweight=deaths_covid], bin(9) ///
		color("67 220 197") ///
		legend(label(1 "Covid-19"))) ///
	(hist agecat [fweight=deaths_2017], bin(9) ///
		fcolor(none) lcolor("105 27 158") lwidth(thick) ///
		legend(label(2 "All-cause, pre-Covid-19"))), ///
	xlabel("") ///
		xtitle("") /// ytitle("Density") ///
	ytitle("") ///	
	legend(off) ///
	/// title("Age Pattern of White Mortality Rates") ///
	saving(age_covid_other_nolabel.gph, replace)
	graph export age_covid_other_nolabel.png, replace	
	
* --------------------------------
* 9. Create single-year empirical Covid age distribution
* --------------------------------
use CDC_Covid_rates.dta, clear

** Assign covid rates to the average age of each age group and 
* take the natural log for the interpolation
* First, create a new observation for each age group to represent its
* mean age
expandby 2 if black==0 & year==2017, by(agegroup) gen(fake_meanage)
// drop if age==0 & fake_meanage==0 // keep duplicate age 0 since used differently
bysort yearnum black agegroup: egen tempcovid = mean(covidrate)
replace covidrate = tempcovid if fake_meanage==1
replace covidrate100k = covidrate * 100000 if fake_meanage==1
replace mx = . if fake_meanage==1
replace age = meanage if fake_meanage==1
replace c_a_st = . if fake_meanage==1
replace c_a = . if fake_meanage==1
replace truepopw2017 = . if fake_meanage==1
table agegroup, c(min meanage max meanage min covidrate max covidrate)

* Make log covid mortality only for the meanage observation
gen lncovidrate_meanage = ln(covidrate) if fake_meanage==1
sort yearnum black agegroup age
list agegroup age meanage fake_meanage covidrate lncovid if black==0 & year==2017
list age covidrate lncovid if black==0 & year==2017 & fake_meanage==1

* Interpolate covid rates
ipolate lncovidrate_meanage age if black==0 & year==2017, epolate gen(lncovid_interp)
gen covidrate_interp = exp(lncovid_interp)
gen covidrate_interp_100k = covidrate_interp * 100000
list age covidrate covidrate_interp* if black==0 & year==2017

* Graph logged Covid mortality, with and without interpolation
* and extrapolation
gen lncovidrate_interp_100k = ln(covidrate_interp_100k)
gen lncovidrate100k_meanage = ln(covidrate100k) if fake_meanage==1
line lncovidrate_interp_100k age if black==0, lwidth(thick) || ///
	scatter lncovidrate100k_meanage meanage if black==0, msize(large) ///
	ytitle("Deaths Per 100,000") xtitle("") ///
	ytitle("") ///
	/// title("Covid Mortality Among Whites by Age") ///
	saving(covid_age.gph, replace)
	graph export covid_age.png, replace
drop lncovidrate_interp_100k lncovidrate100k_meanage

* --------------------------------
* 10. Direct standardization with actual Covid age distribution,
* single-year age units interpolated/extrapolated from the CDC data
* --------------------------------	
* Standardize mortality using 5-year age units
table yearnum black, c(min cdr_st) // same as previous single-year standardization

* Get difference we need covid to contribute
* This replicates scalars used in prior single-age standardization.
* Naming convention: These scalars, which don't depend on assumption of
* Covid age pattern, are labeled _cdr_st; ones specific to this analysis
* are labeled _stcovid
sum cdr_st if black==1 
scalar blackmin_cdr_st = r(min)
sum year if cdr_st == blackmin_cdr_st
scalar year_blackmin_cdr_st = r(max)
sum cdr_st if black==0 & year==2017
scalar white2017_cdr_st = r(mean)
scalar diffneeded_cdr_st = blackmin_cdr_st - white2017_cdr_st

* Make standardized contribution of covid mortality
gen temp_covid_piece = covidrate_interp_100k * c_a_st if fake_meanage==0
by yearnum black: egen covidcontrib = total(temp_covid_piece), missing
sum covidcontrib
scalar covid_contrib_stcovid = r(mean)
	// This represents the standardized white deaths at CURRENT Covid rates

* How many times does that contribution need to occur to raise white mortality
* to black levels?
scalar covid_units_stcovid = diffneeded_cdr_st / covid_contrib_stcovid
gen covid_mx = covidrate_interp_100k * covid_units_stcovid if fake_meanage==0

* Check this result
gen mx_cf = mx100k + covid_mx if fake_meanage==0
gen temp_cdr_cf_piece = mx_cf * c_a_st if fake_meanage==0
by yearnum black: egen cdr_st_cf = total(temp_cdr_cf_piece)
sum cdr_st_cf if black==0 & year==2017
disp r(mean)
scalar list blackmin_cdrst // should be the same; is. --EWF, 6/26/2020

list age mx* covidrate* if black==0 & year==2017

* How many deaths is that?
gen deathsage_covid = truepopw2017 * covid_mx / 100000 if black==0 & year==2017
by yearnum black: egen deaths_cf = total(deathsage_covid)
sum deaths_cf if black==0 & year==2017
scalar deathsneeded_stcovid = r(mean)
scalar list covid_contrib_stcovid covid_units_stcovid deathsneeded_stcovid
	// Deaths: 399,327.39 --> June 17 release
	// Deaths: 399,310.45 --> June 24 release
	
disp deathsneeded_stcovid / deaths_w_2019hyp
	// .18318972 --> June 17 release
	// .18318195 --> June 24 release

list age c_a truepopw2017 covidrate_interp_100k covid_mx deathsage_covid ///
	deaths_cf if black==0 & year==2017
list age c_a c_a_st if black==0 & year==2017

save covid_age.dta, replace
	
* --------------------------------
* 11. Life expectancy
* --------------------------------
* Strategy:
* Get current life expectancy gap
* Simulate white mx* that is 2017 mx plus imagined Covid mortality
* Like with directly age-standardized mortality, do this two ways:
* a. assuming Covid mortality is proportional to all-cause mortality
* b. assuming Covid mortality follows empirical age pattern so far
* To do this, import an age structure of Covid mx and then solve for
* the multiplier needed to make it generate the right life expectancy gap

* How much life expectancy change is needed?
use lifetables.dta, clear

table yearnum black if age==0, c(p50 ex)
sum ex if black==1 & age==0
scalar blackmax_e0 = r(max)
sum year if black==1 & age==0 & ex==blackmax_e0
scalar year_blackmax_e0 = r(max)
sum ex if black==0 & year==2017 & age==0
scalar white2017_e0 = r(mean)
scalar diffneeded_e0 = white2017_e0 - blackmax_e0
scalar list diffneeded_e0
	* 3.2146301 years lowered white e0 needed

***** Proportional change in mortality
* Try arbitrary mortality factor, do trial and error
scalar m_e0prop = 1.3069449 
gen mxprime = mx*scalar(m_e0prop)
gen qxprime = scalar(n)*mxprime / (1 + (scalar(n)-nax)*mxprime)
replace qxprime = 1 if age==maxage_LT
gen lxprime = lx if age==0
gen dxprime = lxprime * qxprime if age==0
sort yearnum black age
forvalues age=1/`=maxage_LT' {
	replace lxprime = lxprime[_n-1] - dxprime[_n-1] if age==`age'
	replace dxprime = lxprime * qxprime if age==`age'
}
sort yearnum black age
gen Lxprime = lxprime[_n+1]*scalar(n) + dxprime * nax if age<maxage_LT
replace Lxprime = lxprime/mxprime if age==maxage_LT
gen Txprime = Lxprime if age==maxage_LT
sort yearnum black age
local a=maxage_LT-1
forvalues age=`a'(-1)0 {
	replace Txprime = Lxprime + Txprime[_n+1] if age==`age'
}
gen exprime = Txprime/lxprime

list age *prime if black==0 & year==2017
sum exprime if age==0 & black==0 & year==2017
disp r(max)
scalar list blackmax_e0
	// These should be equal; are. --EWF, 6/26/2020

* Translate the mortality function into deaths, based on 2017 white population
keep if black==0 & year==2017
merge 1:1 age using mortality_comparison_whites2017.dta, keepusing(c_a) gen(_c_a_merge)
drop _c_a_merge
gen deaths_e0prop_piece = (scalar(m_e0prop)-1)*mx * c_a ///
	if black==0 & year==2017
egen white2017_CDR_e0prop = total(deaths_e0prop_piece) if black==0 & year==2017
sum white2017_CDR_e0prop if black==0 & year==2017 // CDR associated with this counterfactual
scalar deaths_e0prop = popw * r(mean)
scalar list deaths_e0prop
	// 665,291.22

disp scalar(deaths_e0prop) / deaths_w_2019hyp
	// .3069449

* --------------------------------
* 12. Life expectancy, using single-year Covid estimates of age shape 
* of excess mortality
* --------------------------------
use covid_age.dta, clear
drop if fake_meanage==1

* Try arbitrary mortality factor, do trial and error
* scalar m_e0covid = 8.608276 // This matches the 6/17/20 data release 
scalar m_e0covid = 8.22573 // This matches the 6/24/20 data release
gen mx_covid_e0covid = scalar(m_e0covid)*covidrate_interp
gen mxprime = mx + mx_covid_e0covid
gen qxprime = scalar(n)*mxprime / (1 + (scalar(n)-nax)*mxprime)
replace qxprime = 1 if age==maxage_LT
gen lxprime = lx if age==0
gen dxprime = lxprime * qxprime if age==0
sort yearnum black age
quietly forvalues age=1/`=maxage_LT' {
	replace lxprime = lxprime[_n-1] - dxprime[_n-1] if age==`age'
	replace dxprime = lxprime * qxprime if age==`age'
}
sort yearnum black age
gen Lxprime = lxprime[_n+1]*scalar(n) + dxprime * nax if age<maxage_LT
replace Lxprime = lxprime/mxprime if age==maxage_LT
gen Txprime = Lxprime if age==maxage_LT
sort yearnum black age
local a=maxage_LT-1
quietly forvalues age=`a'(-1)0 {
	replace Txprime = Lxprime + Txprime[_n+1] if age==`age'
}
gen exprime = Txprime/lxprime

list age *prime if black==0 & year==2017
sum exprime if age==0 & black==0 & year==2017
disp r(max)
scalar list blackmax_e0
	// should be equal; are -- EWF, 6/26/20

* Translate the mortality function into deaths, based on 2017 white population
gen deaths_e0covid_piece = mx_covid_e0covid * c_a if black==0 & year==2017
egen deaths_e0covid_relative = total(deaths_e0covid_piece) if black==0 & year==2017
sum deaths_e0covid_relative if black==0 & year==2017
scalar deaths_e0covid = scalar(popw) * r(mean)
scalar list deaths_e0covid
	// 1,033,125.4 --> June 17 release
	// 1,032,268.5 --> June 24 release

* What proportion of 2017 white deaths is that?
disp scalar(deaths_e0covid) / deaths_w_2019hyp
	// .47394183 --> June 17 release
	// .47354871 --> June 24 release

* Alternative measure
gen deaths_e0covid_alt_piece = truepopw2017 * mx_covid_e0covid
egen deaths_e0covid_alt = total(deaths_e0covid_alt_piece) if black==0 & year==2017
sum deaths_e0covid_alt
	// should give same result and does --EWF, 6/26/20

* --------------------------------
* 13. Metrics comparing hypothetical deaths to current Covid mortality
* --------------------------------	
	
* How much do Covid deaths need to increase according to these models?
scalar deaths_truecovid_whites = 57630
scalar deaths_truecovid_blacks = 24868
	// These numbers as of the June 24, 2020 CDC release
	// https://data.cdc.gov/NCHS/Provisional-Death-Counts-for-Coronavirus-Disease-C/pj7m-y5uh
scalar min_multiplier = deathsneeded_stprop / scalar(deaths_truecovid_whites)
	// how many times current Covid deaths need to be scaled up to reach
	// minimum level (the proportional direct standardization)
scalar max_multiplier = deaths_e0covid / scalar(deaths_truecovid_whites)
scalar list min_multiplier max_multiplier
	// 6.8 to 17.9
scalar deaths_covidblack_min = deaths_truecovid_blacks * min_multiplier
scalar list deaths_covidblack_min
	// 168,387.76
disp deaths_truecovid_blacks * max_multiplier
	// 445,435.58

scalar deaths_truecovid_total = 57630 + 24868 + 738 + 5356 + 17921 + 1484
	// CDC, 6/24/20 data release
scalar deaths_covidtotal_min = deaths_truecovid_total * min_multiplier
scalar cdr_covidblack_min = (deaths_covidblack_min / popb) * 100000
scalar list deaths_truecovid_total deaths_covidtotal_min cdr_covidblack_min
	// deaths_truecovid_total =     107997
	// deaths_covidtotal_min =  731276.05
	// cdr_covidblack_min =  382.83783

scalar cdr_covid_nyc = 210.57
	// June 24, 2020 update from https://github.com/nychealth/coronavirus-data/blob/master/by-age.csv
disp cdr_covidblack_min / cdr_covid_nyc
	// 1.8181024
	
* --------------------------------
* 14. Life expectancy using Covid age distribution in 10-year units
* This one takes Covid estimates from the CDC with extrapolation/interpolation
* --------------------------------	
use CDC_Covid_rates.dta, clear

* Make 10-year lifetable
collapse (sum) dx10=dx Lx10=Lx c_a10=c_a (max) lx10=lx covidrate=covidrate, ///
	by(yearnum black agegroup)
gen truepopw2017 = scalar(popw) * c_a if black==0 & year==2017

* Create new mx and nax
gen mx10 = dx10 / Lx10
sum age
scalar maxage_LT10 = r(max) // used to make formulas general
gen n = 1 if agegroup==0
replace n = 4 if agegroup==1
replace n = 10 if agegroup>1
tab agegroup n, m
gen nax10 = (Lx10 - n*lx10[_n+1])/dx10
sum age
replace nax10 = Lx10/lx10 if age==maxage_LT10
table age, c(min nax10 max nax10)

* Create new Tx and ex
gen Tx10 = Lx10 if agegroup==85
sort yearnum black agegroup
forvalues a=75(-10)5 {
	replace Tx10 = Tx10[_n+1] + Lx10 if age==`a'
}
foreach a in 1 0 {
	replace Tx10 = Tx10[_n+1] + Lx10 if age==`a'
}
gen ex10 = Tx10/lx10

* How much change in life expectancy is needed?
table yearnum black if agegroup==0, c(p50 ex10)
sum ex10 if black==1 & age==0
scalar blackmax_e010 = r(max)
sum year if black==1 & age==0 & ex10==blackmax_e010
scalar year_blackmax_e010 = r(max) - .1
sum ex10 if black==0 & year==2017.1 & age==0
scalar white2017_e010 = r(mean)
scalar diffneeded_e010 = white2017_e010 - blackmax_e010
scalar list white2017_e0 white2017_e010 blackmax_e0 blackmax_e010 ///
	diffneeded_e0 diffneeded_e010
	// Essentially identical; the age units are not really
	// a distorting factor. --EWF, 6/24/20
	
* Estimate additional mortality with Covid, empirical age distribution
* Trial and error to estimate multiplier
* scalar m_e010 = 9.15017 // This matches the 6/17/20 data release
scalar m_e010 = 8.74451 // This matches the 6/24/20 data release
gen mxprime = mx10 + scalar(m_e010)*covidrate
gen qxprime = n*mxprime / (1 + (n-nax10)*mxprime)
replace qxprime = 1 if age==maxage_LT10
gen lxprime = lx10 if age==0
gen dxprime = lxprime * qxprime if age==0
replace lxprime = lx10[_n-1] - dxprime[_n-1] if age==1
replace dxprime = lxprime * qxprime if age==1
sort yearnum black age
forvalues age=5(10)`=maxage_LT10' {
	replace lxprime = lxprime[_n-1] - dxprime[_n-1] if age==`age'
	replace dxprime = lxprime * qxprime if age==`age'
}
sort yearnum black age
gen Lxprime = lxprime[_n+1]*n + dxprime * nax if age<maxage_LT10
replace Lxprime = lxprime/mxprime if age==maxage_LT10
gen Txprime = Lxprime if age==maxage_LT10
sort yearnum black age
forvalues age=75(-10)5 {
	replace Txprime = Lxprime + Txprime[_n+1] if age==`age'
}
foreach age in 1 0 {
	replace Txprime = Lxprime + Txprime[_n+1] if age==`age'
}
gen exprime = Txprime/lxprime

list age *prime if black==0 & yearnum==2017.1
sum exprime if age==0 & black==0 & yearnum==2017.1
disp r(max)
scalar list blackmax_e010
	// should be equal; are. --EWF, 6/26/20

* Translate the mortality function into deaths, based on 2017 white population
gen deaths_e010covid_piece = scalar(m_e010)*covidrate * c_a10 if black==0 & yearnum==2017.1
egen deaths_e010covid_relative = total(deaths_e010covid_piece) if ///
	black==0 & yearnum==2017.1
sum deaths_e010covid_relative if black==0 & yearnum==2017.1
scalar deaths_e010covid = popw * r(mean)
scalar list deaths_e010covid
	// 1,011,861.6 --> June 17 release
	// 1,011,205.2 --> June 24 release

disp deaths_e010covid / deaths_w_2019hyp
	// .46418713 --> June 17 release
	// .46388602 --> June 24 release
	
* --------------------------------
* 15. Graph NCHS e0 and CDR* from 1900-2017
* --------------------------------
import delimited "NCHS_-_Death_rates_and_life_expectancy_at_birth.csv", encoding(ISO-8859-1)clear
* from https://data.cdc.gov/NCHS/NCHS-Death-rates-and-life-expectancy-at-birth/w9j2-ggv5
ren averagelifeexpectancyyears e0
ren ageadjusteddeathrate CDRst
save NCHS_e0_CDRst.dta, replace

keep if sex=="Both Sexes"

gen lnCDRst = ln(CDRst)

foreach var in e0 CDRst lnCDRst {
	foreach y in 1918 1957 {
		sum `var' if year==`y' & race=="White"
		scalar white`y'_`var'_NCHS = r(mean)
	}
}
sum e0 if race=="Black"
scalar blackmax_e0_NCHS = r(max)
sum year if race=="Black" & e0==blackmax_e0_NCHS // 2014, as expected
sum CDRst if race=="Black"
scalar blackmin_CDRst_NCHS = r(min)
sum year if race=="Black" & CDRst==blackmin_CDRst_NCHS // 2014, as expected

sum lnCDRst if race=="Black"
scalar blackmin_lnCDRst_NCHS = r(min)

list year if race=="Black" & CDRst > white1918_CDRst_NCHS
	// all years 1900-1931 except 1921; also 1936
list year if race=="Black" & e0 < white1918_e0_NCHS
	// all years 1900-1918 except 1916
tab year if race=="White" & e0==blackmax_e0_NCHS
	// Black e0 in 2014 equals white in 1987 and 1988
sum year if race=="White" & CDRst >= blackmin_CDRst_NCHS
	// latest: 2000
tab year if race=="White" & CDRst==blackmin_CDRst_NCHS
	// none exact
sum year if race=="White" & CDRst <= blackmin_CDRst_NCHS
	// first: 1998
sum year if race=="White" & e0 <= blackmax_e0_NCHS
	// latest: 1988 (almost 30 years gap from 2017)
sum year if race=="White" & e0 >= blackmax_e0_NCHS
	// first: 1987

	
	
/* Color scheme:
White = rgb(37,102,118)
Black = rgb(113,211,244)
*/

sort race year
set scheme uncluttered
line e0 year if race=="Black", lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line e0 year if race=="White", lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") on) ///
	title("Life Expectancy") xtitle("") /// ytitle("Years of Life") ///
	xlabel(1900(20)2020) ///
	yline(`=scalar(white1918_e0_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	/// yline(`=scalar(white1957_e0_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	yline(`=scalar(blackmax_e0_NCHS)', lcolor("113 211 244") lpattern(dash)) ///
	/// ysize(6) xsize(6) ///
	xtitle("") ytitle("") ///
	saving(NCHS_e0_preprocessing.gph, replace)
	graph export NCHS_e0_preprocessing.png, replace
	
line CDRst year if race=="Black", lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line CDRst year if race=="White", lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") on) ///
	title("Age-Adjusted Mortality") xtitle("") /// ytitle("Deaths Per 100,000") ///
	xlabel(1900(20)2020) ///
	yline(`=scalar(white1918_CDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	/// yline(`=scalar(white1957_CDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	yline(`=scalar(blackmin_CDRst_NCHS)', lcolor("113 211 244") lpattern(dash)) ///
	/// ysize(6) xsize(6) ///
	xtitle("") ytitle("") ///
	saving(NCHS_CDRst_preprocessing.gph, replace)
	graph export NCHS_CDRst_preprocessing.png, replace	

grc1leg NCHS_CDRst_preprocessing.gph ///
		NCHS_e0_preprocessing.gph, ///
		ysize(12) xsize(12) ///
		saving(NCHS_e0CDRst_preprocessing.gph, replace)
	graph export NCHS_e0CDRst_preprocessing.png, replace
	
line e0 year if race=="Black", lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line e0 year if race=="White", lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") on) ///
	title("Life Expectancy") xtitle("") /// ytitle("Years of Life") ///
	xlabel(1900(20)2020) ///
	yline(`=scalar(white1918_e0_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	/// yline(`=scalar(white1957_e0_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	yline(`=scalar(blackmax_e0_NCHS)', lcolor("113 211 244") lpattern(dash)) ///
	legend(off) ///
	xtitle("") ytitle("") ///
	saving(NCHS_e0_preprocessing_nolegend.gph, replace)
	graph export NCHS_e0_preprocessing_nolegend.png, replace

line CDRst year if race=="Black", lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line CDRst year if race=="White", lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") on) ///
	title("Age-Adjusted Mortality") xtitle("") /// ytitle("Deaths Per 100,000") ///
	xlabel(1900(20)2020) ///
	yline(`=scalar(white1918_CDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	/// yline(`=scalar(white1957_CDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	yline(`=scalar(blackmin_CDRst_NCHS)', lcolor("113 211 244") lpattern(dash)) ///
	legend(off) ///
	xtitle("") ytitle("") ///
	saving(NCHS_CDRst_preprocessing_nolegend.gph, replace)
	graph export NCHS_CDRst_preprocessing_nolegend.png, replace		
	
line lnCDRst year if race=="Black", lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line lnCDRst year if race=="White", lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") on) ///
	title("Log Age-Adjusted Mortality") xtitle("") /// ytitle("Logged Deaths Per 100,000") ///
	xlabel(1900(20)2020) ///
	yline(`=scalar(white1918_lnCDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	/// yline(`=scalar(white1957_CDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	yline(`=scalar(blackmin_lnCDRst_NCHS)', lcolor("113 211 244") lpattern(dash)) ///
	/// ysize(6) xsize(6) ///
	legend(off) ///
	xtitle("") ytitle("") ///
	saving(NCHS_lnCDRst_preprocessing_nolegend.gph, replace)
	graph export NCHS_lnCDRst_preprocessing_nolegend.png, replace	
	
line lnCDRst year if race=="Black", lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line lnCDRst year if race=="White", lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") on) ///
	title("Log Age-Adjusted Mortality") xtitle("") /// ytitle("Logged Deaths Per 100,000") ///
	xlabel(1900(20)2020) ///
	yline(`=scalar(white1918_lnCDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	/// yline(`=scalar(white1957_CDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	yline(`=scalar(blackmin_lnCDRst_NCHS)', lcolor("113 211 244") lpattern(dash)) ///
	/// ysize(6) xsize(6) ///
	legend(on) ///
	xtitle("") ytitle("") ///
	saving(NCHS_lnCDRst_preprocessing.gph, replace)
	graph export NCHS_lnCDRst_preprocessing.png, replace	
		
	
grc1leg NCHS_lnCDRst_preprocessing.gph ///
		NCHS_e0_preprocessing.gph, ///
		ysize(12) xsize(12) ///
		saving(NCHS_e0lnCDRst_preprocessing.gph, replace)
	graph export NCHS_e0lnCDRst_preprocessing.png, replace
	* In graph editor, changed xsize to 7
	
list
	
* --------------------------------
* 16. Graphs of key outcomes
* --------------------------------
set scheme uncluttered

use mortality_comparison, clear

keep if age==0

gen cmr100 = cmr * 100
ren ex e0
gen lncdr_st = ln(cdr_st)
sum lncdr_st if black==1
scalar blackmin_lncdr_st = r(min)
sum lncdr_st if black==0 & year==2017
scalar white2017_lncdr_st = r(mean)

* average over multiple observations in years with two life tables,
* to simplify graph
foreach var in cmr100 cdr_st lncdr_st e0 {
	gen temp`var' = `var' if version==2
	bysort black year: egen `var'_v2 = max(temp`var')
	gen `var'_avg = (`var' + `var'_v2)/2 if version==1
	replace `var'_avg = `var' if year>2011 | year==2006
	drop temp* `var'_v2
}
	/*
line cmr100_avg year if black==1, lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line cmr100_avg year if black==0, lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") off ///
		size(small) ring(0) position(5)) ///
	xtitle("") ytitle("Percent of Expected Mortality") ///
	subtitle("Mortality Rates, Indirectly Age-Standardized") ///
	saving(cmr100.gph, replace)

line cdr_st_avg year if black==1, lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line cdr_st_avg year if black==0, lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") off size(small) position(5)) ///
	xtitle("") ytitle("Deaths Per 100,000") ///
	subtitle("Mortality Rates, Directly Age-Standardized")	///
	saving(cdr_st.gph, replace)

line e0_avg year if black==1 & age==0, lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line e0_avg year if black==0 & age==0, lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") off size(small) position(5)) ///
	xtitle("") ytitle("Years of Life") ///
	subtitle("Life Expectancy")	///
	saving(e0.gph, replace)

grc1leg cmr100.gph cdr_st.gph e0.gph, rows(2) ring(0) position(5) ///
	saving(all_2007-2017.gph, replace)
graph export all_2007-2017.png, replace
*/
foreach var in cmr100 cdr_st lncdr_st e0 {
	gen `var'_mark = `var' if (year==2014 & black==1) | (year==2017 & black==0)
}
/*
line cmr100_avg year if black==1, lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line cmr100_avg year if black==0, lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") off ///
		size(small) ring(0) position(5)) || ///
	scatter cmr100_mark year, mcolor(pink) msymbol(X) msize(medlarge) ///
	legend(label(3 "Comparison Year") order(1 2 3) rows(1)) ///
	xtitle("") ytitle("Percent of Expected Death Rate, Based on Ages") ///
	subtitle("Mortality Rates, Indirectly Age-Standardized") ///
	saving(cmr100_marked.gph, replace)

line cdr_st_avg year if black==1, lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line cdr_st_avg year if black==0, lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") off size(small) position(5)) || ///
	scatter cdr_st_mark year, mcolor(pink) msymbol(X) msize(medlarge) ///
	xtitle("") ytitle("Deaths Per 100,000 People") ///
	subtitle("Mortality Rates, Directly Age-Standardized")	///
	saving(cdr_st_marked.gph, replace)

grc1leg cmr100_marked.gph cdr_st_marked.gph, rows(1) ///
	saving(cmr_cdrst_marked.gph, replace)
graph export cmr_cdrst_marked.png, replace
*/
* With visualization of hypothetical
expandby 2 if year==2017, by(black) gen(fake_2020)
replace year = 2020 if fake_2020==1
replace cdr_st = blackmin_cdrst if fake_2020==1 & black==0
replace lncdr_st = blackmin_lncdr_st if fake_2020==1 & black==0
replace cmr100 = blackmin_cmr * 100 if fake_2020==1 & black==0
replace e0 = blackmax_e0 if fake_2020==1 & black==0 & age==0
replace cmr100_mark = . if fake_2020==1
replace cdr_st_mark = . if fake_2020==1
replace lncdr_st_mark = . if fake_2020==1
replace e0_mark = . if fake_2020==1
replace black = . if fake_2020==1 & black==1 // used to make vertical line
replace cdr_st = white2017_cdrst if fake_2020==1 & black==.
replace lncdr_st = white2017_lncdr_st if fake_2020==1 & black==.
replace cmr100 = white2017_cmr * 100 if fake_2020==1 & black==.
replace e0 = white2017_e0 if fake_2020==1 & black==. & age==0

foreach var in cmr100 cdr_st lncdr_st e0 {
	gen horiz_`var' = `var' if (black==0 & year==2020) | ///
		(black==1 & year==2014)
	gen vert_`var' = `var' if (black==0 & year==2020) | ///
		(black==. & year==2020)
	gen horiz2_`var' = `var' if (black==. & year==2020) | ///
		(black==0 & year==2017)
}
	
local title_cmr100 "Mortality Rates, Indirectly Age-Standardized"
local title_cdr_st "Age-Adjusted Mortality"
local title_lncdr_st "Log Age-Adjusted Mortality"
local title_e0 "Life Expectancy"
local ytitle_cmr100 "Percent of Expected Death Rate"
local ytitle_cdr_st "Deaths Per 100,000"
local ytitle_lncdr_st "Logged Deaths Per 100,000"
local ytitle_e0 "Years of Life"
local yscale_cdr_st 800(200)1200
local yscale_e0 70(2)80
	
sort black year	
foreach var in cmr100 cdr_st lncdr_st e0 {	
	line horiz_`var' year, /// lines first so markers are on top
			lcolor(gs8) lpattern(dash) lwidth(medium) || ///
		line horiz2_`var' year, ///
			lcolor(gs8) lpattern(dash) lwidth(medium) || ///
		line vert_`var' year, ///
			lcolor(black) lpattern(solid) lwidth(medium) || ///
		line `var'_avg year if black==1 & fake_2020==0, /// real data
			lcolor("113 211 244") lwidth(vthick) ///
			legend(label(4 "Non-Hispanic Black")) || ///
		line `var'_avg year if black==0 & fake_2020==0, ///
			lcolor("37 102 118") lwidth(vthick) ///
			legend(label(5 "Non-Hispanic White") off ///
			size(small)) || ///
		scatter `var'_mark year if black==1, ///
			mcolor("113 211 244") msize(large) || ///
		scatter `var'_mark year if black==0, ///
			mcolor("37 102 118") msize(large) || ///
		scatter `var'_mark year, ///
			mcolor(pink) msymbol(X) msize(vlarge) ///
			legend(label(8 "Comparison Year")) || ///
		scatter `var' year if black==0 & fake_2020==1, /// hypothetical year
			mcolor("37 102 118") msize(large) || ///
		scatter `var' year if black==0 & fake_2020==1, /// marker on hypothetical
			mcolor("gold") msymbol(X) msize(vlarge) ///
			legend(label(10 "Hypothetical 2020 White")) ///
		legend(order(4 5 8 10) rows(2) colfirst size(small)) ///
		xtitle("") /// ytitle("`ytitle_`var''") ///
		ytitle("") ///
		title("`title_`var''") ///
		saving(`var'_marked_white2020.gph, replace)
		
		line horiz_`var' year, /// lines first so markers are on top
			lcolor(gs8) lpattern(dash) lwidth(medium) || ///
		line horiz2_`var' year, ///
			lcolor(gs8) lpattern(dash) lwidth(medium) || ///
		line vert_`var' year, ///
			lcolor(black) lpattern(solid) lwidth(medium) || ///
		line `var'_avg year if black==1 & fake_2020==0, /// real data
			lcolor("113 211 244") lwidth(vthick) ///
			legend(label(4 "Non-Hispanic Black")) || ///
		line `var'_avg year if black==0 & fake_2020==0, ///
			lcolor("37 102 118") lwidth(vthick) ///
			legend(label(5 "Non-Hispanic White") ///
			size(small)) || ///
		scatter `var'_mark year if black==1, ///
			mcolor("113 211 244") msize(large) || ///
		scatter `var'_mark year if black==0, ///
			mcolor("37 102 118") msize(large) || ///
		scatter `var'_mark year, ///
			mcolor(pink) msymbol(X) msize(vlarge) ///
			legend(label(8 "Comparison Year")) || ///
		scatter `var' year if black==0 & fake_2020==1, /// hypothetical year
			mcolor("37 102 118") msize(large) || ///
		scatter `var' year if black==0 & fake_2020==1, /// marker on hypothetical
			mcolor("gold") msymbol(X) msize(vlarge) ///
			legend(label(10 "Hypothetical 2020 White")) ///
		legend(order(4 5 8 10) rows(2) colfirst size(small) on /* holes(4 6) */) ///
		xtitle("") /// ytitle("`ytitle_`var''") ///
		ytitle("") ///
		title("`title_`var''") ///
		saving(`var'_marked_white2020_legend.gph, replace)
}

grc1leg cmr100_marked_white2020.gph cdr_st_marked_white2020.gph ///
	e0_marked_white2020.gph, rows(2) /* holes(4 6) */ ///
	/// position(5) /// bmargin(large) rmargin(large) ///
	saving(all_marked_white2020_legend.gph, replace)
graph export all_marked_white2020_legend.png, replace
grc1leg cdr_st_marked_white2020.gph ///
	e0_marked_white2020.gph, rows(1) /* holes(4 6) */ ///
	saving(e0CDRst_marked_white2020_legend.gph, replace)
graph export e0CDRst_marked_white2020_legend.png, replace

grc1leg lncdr_st_marked_white2020.gph ///
	e0_marked_white2020.gph, rows(1) /* holes(4 6) */ ///
	saving(e0lnCDRst_marked_white2020_legend.gph, replace)
graph export e0lnCDRst_marked_white2020_legend.png, replace

graph use cmr100_marked_white2020_legend.gph
graph export cmr100_marked_white2020_legend.png, replace

/* 
grc1leg cmr100_marked_white2020.gph cdr_st_marked_white2020.gph ///
	e0_marked_white2020.gph, rows(2) /* holes(4 6) */ ///
	position(5) /// bmargin(large) rmargin(large) ///
	legend(off) ///
	saving(cmr_cdrst_marked_white2020_nolegend.gph, replace)
graph export cmr_cdrst_marked_white2020_nolegend.png, replace
*/

log close
* done!




* --------------------------------
* 1. Create numerators: race-specific life tables
* --------------------------------

local r13 0 // white
local r16 1 // black

forvalues y=2006/2017 {
	foreach n in 13 16 {
			if `y'>2010 {
				import excel "Table`n'_`y'.xlsx", ///
					cellrange(A3:G104) firstrow
			}
			else if `y'==2010 {
				import excel "Table`n'_`y'.xlsx", ///
					cellrange(A7:G108) firstrow
			}
			else if `y'==2009 {
				import excel "Table`n'_`y'.xls", ///
					cellrange(A7:G108) firstrow
			}
			else if `y'==2008 {
				import excel "Table`n'_`y'.xls", ///
					cellrange(A3:G104) firstrow
				replace Ageyears = "79-80" if Ageyears=="70-80"
					// corrects error in NCHS spreadsheet (for both races)
			}
			else if `y'==2007 { 
				import excel "Table`n'_`y'.xls", ///
					cellrange(A4:G105) firstrow
			}
			else { // 2006
				local 2006_`n' = `n'-9 // 2006 uses Tables 4 and 9
				import excel "Table`2006_`n''_`y'.xls", ///
					cellrange(A3:G104) firstrow
			}
			gen black = `r`n''
			gen year = `y'
			gen version = 1
			gen yearnum = year + .1
			save "Table`n'_`y'.dta", replace
			clear
			
			if `y'>=2007 & `y'<=2009 {
				import excel "Table`n'_Intercensal_`y'.xlsx",  ///
					cellrange(A7:G108) firstrow
				gen black = `r`n''
				gen year = `y'
				gen version = 2
				gen yearnum = year + .2
				save "Table`n'_`y'_ver2.dta", replace
				clear
			}
			
			if `y'==2010 | `y'==2011 {
				import excel "table`n'_Updated_ClassificationRatios_`y'.xlsx", ///
					cellrange(A3:G104) firstrow
				gen black = `r`n''
				gen year = `y'
				gen version = 2
				gen yearnum = year + .2
				save "Table`n'_`y'_ver2.dta", replace
				clear
			}
	}
}
forvalues y=2006/2017 {
	foreach n in 13 16 {
		append using "Table`n'_`y'.dta"
		
		if `y'>=2007 & `y'<=2011 {
			append using "Table`n'_`y'_ver2.dta"
		}
	}
}

* clean age variable
foreach a in A Age Ageyears {
	replace `a' = "100" if `a'=="100 and over"
	replace `a' = subinstr(`a',"-","–",.) // hyphen vs n-dash
	split `a', parse("–") destring
}
ren A1 age
replace age = Ageyears1 if !missing(Ageyears1)
replace age = Age1 if !missing(Age1)
tab age black, m
tab yearnum black, m
drop A Age Age1 Ageyears1 Ageyears A2 Age2 Ageyears2

* Make consistent exposure and mortality
replace Lx = L if year==2011 & version==1
drop L
gen mx = dx/Lx

* Create open interval at age 94 (max age with good population estimates later)
* It's the deaths above 94 (dx above 94 = lx at 94) divided by
* person-years lived above age 94 (Tx)
replace mx = lx / Tx if age==94
sum mx if age==94
replace Lx = Tx if age==94
sum Lx if age==94
replace qx = 1 if age==94
drop if age>94

gen mx100k = mx * 100000

* generate nax. PHG p. 84: If age interval is short, option (b) is best	
* I use nax from main life table. In practice, changing mortality would
* change the nax values but with single-year age units this is essentially 
* irrelevant
sum age
scalar maxage_LT = r(max) // used to make formulas general
scalar n = 1 // used to make the nax formula general to other units
gen nax = (Lx - n*lx[_n+1])/dx
sum age
replace nax = Lx/lx if age==maxage_LT
table age, c(min nax max nax)

table yearnum black if age==0, c(p50 ex)
table year black if age==0, c(min ex max ex)

save lifetables.dta, replace

* --------------------------------
* 2. Create denominators: annual age structures by race
* --------------------------------

* do agestructure_ipums.do
	// creates agestructure_ipums.dta from raw IPUMS data
use agestructure_ipums.dta, clear

keep if hispan==0
keep if race<=2
gen black = (race==2)

collapse (count) pop=pernum [pweight=perwt], by(year black age)

* median ages in 2017
table black if year==2017 [fweight=pop], c(p50 age)

save population_age_race.dta, replace

tab age black
	// above age 94 is not populated in all years for Blacks

* Graph age distributions (across years)
* For talk in s1rcolor: black = cyan, white = pink
/* Color scheme:
White = rgb(37,102,118)
Black = rgb(113,211,244)
*/
twoway (hist age [fweight=pop] if black==0, color("37 102 118") ///
		width(5) legend(label(1 "White"))) ///
	(hist age [fweight=pop] if black==1, fcolor(none) ///
		lcolor("113 211 244") lwidth(thick) ///
		width(5) legend(label(2 "Black"))), ///
		legend(on) ///
		title("Age Distributions by Race") ///
		saving(agecomp.gph, replace)
		graph export agecomp.png, replace
	
* --------------------------------
* 3. Merge and create annual crude mortality by race
* --------------------------------

merge 1:m year black age using lifetables.dta
sort yearnum black age

* Graph age-specific death rates
gen lnmx100k = ln(mx100k)
line lnmx100k age if black==0 & year==2017 & age<=94, ///
		lcolor("113 211 244") lwidth(thick) ///
		legend(label(1 "White")) || ///
	line lnmx100k age if black==1 & year==2017 & age<=94, ///
		lcolor("37 102 118") lwidth(thick) ///
		legend(label(2 "Black")) ///
	legend(on) ///
	ytitle("Logged Deaths Per 100,000") xtitle("Age") ///
	ytitle("") ///
	title("Log Mortality") ///
	saving(mx.gph, replace)
	graph export mx.png, replace

* Make population totals for the open interval
gen pop94andup_sep = pop if age>=94
by yearnum black: egen pop94andup = total(pop94andup_sep)
replace pop = pop94andup if age==94
drop pop94*

drop if age>94

*** Construct crude mortality
* get age distribution
by yearnum black: egen totalpop = total(pop)
gen c_a = pop / totalpop

* Crude Death Rate
gen cdr_piece = c_a * mx
replace cdr_piece = cdr_piece * 100000 // deaths per 100,000
by yearnum black: egen cdr = total(cdr_piece)

table yearnum black, c(min cdr)

* Minimum black CDR
sum cdr if black==1
scalar blackmin_cdr = r(min)
sum year if cdr==blackmin_cdr & black==1
scalar year_blackmin_cdr = r(max)

* Difference between 2017 [most recent] white mortality and minimum black
sum cdr if black==0 & year==2017
scalar white2017_cdr = r(mean)

scalar list blackmin_cdr white2017_cdr
	// Upshot: White CDRs are higher than black CDRs due to the age composition

save mortality_comparison.dta, replace

* --------------------------------
* 4. Create indirectly age-standardized annual mortality by race
* 		Also known as Comparative Morality Ratio
* --------------------------------
**** A. Create age standard
* Treat the whole pooled super-population, across years, as the population
* For the 5 years (2007-2011) with two distinct life tables, weight each by half
gen pop_weight = pop if year>2011 | year==2006
replace pop_weight = pop/2 if year>=2007 & year<=2011
bysort age: egen pop_age_pooled = total(pop_weight)
gen deaths_pop = pop * mx
gen deaths_pop_weighted = deaths_pop if year>2011 | year==2006
replace deaths_pop_weighted = deaths_pop/2 if year>=2007 & year<=2011 

by age: egen deaths_age_pooled = total(deaths_pop_weighted)
gen dr_standard = deaths_age_pooled / pop_age_pooled
drop pop_weight deaths_pop_weighted //

replace dr_standard = dr_standard * 100000
table age, c(min dr_standard)

**** B. Age standardize mortality
* Create expected CDR
gen cdr_expected_piece = c_a * dr_standard 
bysort yearnum black: egen cdr_expected = total(cdr_expected_piece)
drop cdr_expected_piece
gen cmr = cdr / cdr_expected
table yearnum black, c(min cdr min cdr_expected min cmr)

* Difference between 2017 white CMR and minimum black CMR
sum cmr if black==1 
scalar blackmin_cmr = r(min)
sum year if cmr==blackmin_cmr & black==1
scalar year_blackmin_cmr = r(max)
sum cmr if black==0 & year==2017
scalar white2017_cmr = r(mean)
scalar excessneeded_cmr = scalar(blackmin_cmr) - scalar(white2017_cmr) 
	// amount of excess white mortality
	// needed for it *not* to be the case that blacks have long had greater
	// mortality than whites do even with the excess
scalar list blackmin_cmr white2017_cmr excessneeded_cmr
	// CMR difference of .21 (1.18 vs. .97)

* How many excess *deaths* are needed to get white CMR up to black level?
	// Crude death rates are deaths / pop, so the CMR (ratio of two CDRs)
	// is also a ratio of actual to expected total deaths. 
sum cdr_expected if black==0 & year==2017
scalar cdrexpected_white2017 = r(mean)
scalar deathsneededper100k = excessneeded_cmr * cdrexpected_white2017
	// this is the additional deaths per 100,000 needed for whites
scalar list deathsneededper100k
	// 237.5403

scalar wnh_pct = .604 // percent of pop that is white non-Hispanic
scalar popUS = 328239523 // total US pop estimate, July 1, 2019
						// note this is an underestimate since a year old
scalar popw = wnh_pct * scalar(popUS)
scalar bnh_pct = .134 // excludes multi-racial
scalar popb = bnh_pct * popUS
	* all above from https://www.census.gov/quickfacts/fact/table/US/RHI825218

scalar deathsneeded_cmr = deathsneededper100k * scalar(popw) / 100000	
scalar list deathsneeded_cmr
	* 470,939.48

scalar deaths_US_2017 = 2813503	
	// from https://www.cdc.gov/nchs/data/nvsr/nvsr68/nvsr68_06-508.pdf
	// Table C, p. 9
scalar deaths_wh_2017 = 2179857
	// same source Table D, p. 12
	// also available here: https://www.cdc.gov/nchs/fastats/white-health.htm
disp deathsneeded_cmr / deaths_US_2017
disp deathsneeded_cmr / white2017_cdr
	
save, replace	
	
* --------------------------------
* 5. Direct age standardization
* --------------------------------	
* Use the total US population as the age standard
/* In this metric, the age structure of covid deaths matters
An analytical solution (I originally derived based on the Vaupel and Zhang 
formula but it can be derived more simply) says that if white mortality increases
proportionally (so if Covid deaths are proportional to all-cause mortality),
so new mortality is m * normal white mortality, then m = the ratio of 
black to white standardized normal mortality.
*/
* --------------------------------
* A. Standard age composition
* --------------------------------	
	
use USagestructure_indlevel.dta, clear

drop if missing(age)

gen obs=1
collapse (count) popUS=obs [fweight=perwt], by(age)

save USagestructure.dta, replace

* smooth (minimally)
lowess popUS age, bwidth(.15) gen(popsmoothUS)
	// bandwidth of .8 (default) is too strong: smooths over baby bump
	// bandwidth chosen by trial and error to create age structure that captures
	// theoretically expected and consistent variation but smoothes year-to-year 
	// jaggedness
	// .3 is reasonable, .2 good, .5 too stringent, .1 maybe not enough

* Collapse ages above 94 [done after smoothing so the age units are equal]
replace age = 94 if age>94 // max age with coverage of small racial groups
collapse (sum) popUS=popUS popsmoothUS=popsmoothUS, by(age)

egen totalpopUS = total(popUS)
gen c_a_st_raw = popUS/totalpopUS
egen totalpopsmoothUS = total(popsmoothUS)
gen c_a_st = popsmoothUS/totalpopsmoothUS

list

save USagestructure_smooth.dta, replace

merge 1:m age using mortality_comparison.dta, gen(_merge_direct)
drop _merge_direct
	// should be all _merge==3; is -- EWF, 6/23/20

* --------------------------------
* B. Standardize mortality 
* --------------------------------	
sort yearnum black age

* CDR_standardized is sum of actual mx times standard c_a

gen cdr_st_piece = mx*c_a_st * 100000
by yearnum black: egen cdr_st = total(cdr_st_piece)

table yearnum black, c(p50 cdr_st)

sum cdr_st if black==1 
scalar blackmin_cdrst = r(min)
sum year if cdr_st==blackmin_cdrst & black==1
scalar year_blackmin_cdrst = r(max)

sum cdr_st if black==0 & year==2017
scalar white2017_cdrst = r(mean)

* --------------------------------
* C. Derive excess deaths required to equalize races using proportional
* 		mortality assumption (excess deaths proportional to all-cause)
* --------------------------------	

scalar excessmultiplier_stprop = blackmin_cdrst / white2017_cdrst
scalar whiteCDRneeded_stprop = excessmultiplier_stprop * white2017_cdr
scalar deathsneeded_stprop = (excessmultiplier_stprop - 1) * white2017_cdrst * popw / 100000
scalar list	excessmultiplier_stprop whiteCDRneeded_stprop deathsneeded_stprop
	* excess multiplier: 1.1800391
	* Deaths: 320910.96
* Test this result: standardize this counterfactual white mortality and
* make sure it equals blackmin_st	
gen mx_cf_stprop = excessmultiplier_stprop * mx
gen cdr_st_test_piece = mx_cf_stprop * c_a_st * 100000
by yearnum black: egen cdr_st_test = total(cdr_st_test_piece)
sum cdr_st_test if black==0 & year==2017
disp r(mean)
scalar list blackmin_cdrst // should be the same; are. --EWF, 6/23/2020
	
disp (deathsneeded_stprop / popw) * 100000 // excess CDR, estimated from direct standardization	
	* 161.86641
	
disp deathsneeded_stprop / white2017_cdr
	* 14.7%
	
gen truepopw2017 = scalar(popw) * c_a if black==0 & year==2017
	// used later, made now so it's in both versions of the file below
	
save mortality_comparison.dta, replace

keep if black==0 & year==2017
save mortality_comparison_whites2017.dta, replace
	// used when making Covid age distribution below
	
* --------------------------------
* 6. For comparison: directly standardized using 10-year units
* --------------------------------	
use mortality_comparison.dta, clear
recode age (0=0) (1/4=1), gen(agegroup)
replace agegroup = 10*floor((age+5)/10) - 5 if age>=5
replace agegroup = 85 if age>85 & age<.	
	
collapse (sum) popsmooth Lx dx ///
	(mean) totalpopsmooth, by(yearnum black agegroup)
gen year = floor(yearnum)
gen c_a_cdrst10 = popsmooth / totalpopsmooth
sort yearnum black agegroup
by yearnum black: egen testc = total(c_a_cdrst10)
sum testc // should equal 1; does. --EWF, 6/14/20
drop testc

gen mx = dx/Lx
gen mx100k = mx * 100000
gen cdr_st_piece = mx100k * c_a_cdrst10
by yearnum black: egen cdr_cdrst10 = total(cdr_st_piece)
sum cdr_cdrst10 if black==1
scalar blackmin_cdrst10 = r(min)
sum yearnum if cdr_cdrst10==blackmin_cdrst10 & black==1
scalar year_blackmin_cdrst10 = r(max)-.1
sum cdr_cdrst10 if black==0 & yearnum==2017.1
scalar white2017_cdrst10 = r(mean)

scalar excessmultiplier_cdrst10 = blackmin_cdrst10 / white2017_cdrst10
scalar whiteCDRneeded_cdrst10 = excessmultiplier_cdrst10 * white2017_cdrst10
scalar deathsneeded_cdrst10 = ///
	(excessmultiplier_cdrst10 - 1) * white2017_cdrst10 * popw / 100000
scalar list	excessmultiplier_cdrst10 whiteCDRneeded_cdrst10 deathsneeded_cdrst10
	//  Deaths: 325561.69
	
disp deathsneeded_cdrst10 / white2017_cdr
	// .14935002

* --------------------------------
* 7. Create Covid age structure based on empirical data
* --------------------------------	
clear

* Import CDC Covid data 
* Current version last updated June 24, 2020, based on data through June 20, 2020
import delimited "Deaths_involving_coronavirus_disease_2019__COVID-19__by_race_and_Hispanic_origin_group_and_age__by_state_2020-06-24.csv", encoding(ISO-8859-1)clear
save CDC_Covid_raw.dta, replace

* Get white totals by age; use Covid list
* Covid & pneumonia seems to be both; with added flu, it's explicitly "or flu"
collapse (sum) pneu=pneumoniadeaths flu=influenzadeaths ///
	covid=covid19deaths covidpneu= pneumoniaandcovid19deaths ///
	covidinfpneu=pneumoniainfluenzaorcovid19death totaldeaths, ///
	by(race agegroup)
	
drop if agegroup=="All Ages"	
replace agegroup = "0" if agegroup=="Under 1 year"
replace agegroup = "85" if agegroup=="85 years and over"
split agegroup, parse("-") destring	
drop agegroup agegroup2
ren agegroup1 age
sort age
list	
* Age units: 0, 1, then 5-85 in 10-year units
collapse (sum) pneu flu covid covidpneu covidinfpneu, by(age race)
sort race age

save CDC_Covid_all.dta, replace
	
keep if race=="Non-Hispanic White" | race=="Non-Hispanic Black"
gen black=(race=="Non-Hispanic Black")

/* 
keep if race=="Non-Hispanic White"
drop race
list
save CDC_Covid_whites.dta, replace
*/

* Merge in denominators
* Note that CDC data use 10-year age units
merge 1:m age black using mortality_comparison.dta, gen(_mergeCDC)
	* Unmatched are blacks and other age units
recode age (0=0) (1/4=1), gen(agegroup)
replace agegroup = 10*floor((age+5)/10) - 5 if age>=5
replace agegroup = 85 if age>85 & age<.
table agegroup, c(min age max age) // correct

bysort yearnum agegroup black: egen pop_agegroup = total(pop)
table agegroup black if year==2017, c(min pop_agegroup)

* Average age in each agegroup (used to interpolate Covid rates later)
preserve
collapse (mean) meanage=age [fweight=pop], by(year black agegroup)
	// the two "versions" of each year have the same IPUMS age distribution
save meanage_agegroup.dta, replace
restore
merge m:1 year black agegroup using meanage_agegroup, gen(_merge_meanage)
drop _merge_meanage

* Covid rates by agegroup
gen covidrate = covid / pop_agegroup if year==2017
sort year black agegroup
line covidrate agegroup if black==0 // 85+ age group over 1% mortality

gen covidrate100k = covidrate * 100000
table agegroup if black==0, c(p50 covidrate100k) // white covid mortality by age

save CDC_Covid_rates.dta, replace

* --------------------------------
* 8. Direct standardization with actual Covid age distribution,
* 10-year age units (as reported by the CDC)
* --------------------------------

/* Steps: Make c_a for age groups; standardize black and white mortality
using those age groups; do the proportional (m-method) above for comparison;
multiply covid rate by standard c_a; sum those up to get weighted covid rate,
weighted by standard age distribution; divide that by the difference between
standardized black and white mortality. That's the multiplier on the population
(or per 100,000 population) to get the deaths needed. */

use CDC_Covid_rates, clear

collapse (sum) dx Lx popsmooth c_a ///
	(min) totalpopsmooth covidrate100k, by(yearnum black agegroup)
gen year = floor(yearnum)
gen truepopw2017 = scalar(popw) * c_a if black==0 & year==2017

gen c_a_st = popsmooth/totalpopsmooth // age structure in 10-year units
bysort yearnum black: egen testc = total(c_a_st)
sum testc // should all equal 1; they do. --EWF, 6/23/20
drop testc
table age, c(p50 c_a_st p50 c_a)

* Make 5-year mortality, weighted by exposure
gen mx = dx/Lx
gen mx100k = mx * 100000
table agegroup year black, c(min mx)

* Standardize mortality using 5-year age units
gen temp_cdr_st_piece = mx100k * c_a_st
by yearnum black: egen cdr_cdrst10covid = total(temp_cdr_st_piece)
table yearnum black, c(min cdr_cdrst10covid)

* Get difference we need covid to contribute
sum cdr_cdrst10covid if black==1 
scalar blackmin_cdrst10covid = r(min)
sum year if cdr_cdrst10covid == blackmin_cdrst10covid
scalar year_blackmin_cdrst10covid = r(max)
sum cdr_cdrst10covid if black==0 & year==2017
scalar white2017_cdr_cdrst10covid = r(mean)
scalar diffneeded_cdr_cdrst10covid = blackmin_cdrst10covid - white2017_cdr_cdrst10covid
scalar list blackmin_cdrst10covid white2017_cdr_cdrst10covid diffneeded_cdr_cdrst10covid

* Make standardized contribution of covid mortality
gen temp_covid_piece = covidrate100k * c_a_st
by yearnum black: egen covidcontrib_cdrst10 = total(temp_covid_piece), missing
sum covidcontrib_cdrst10 if black==0 & year==2017
scalar covid_contrib_cdrst10 = r(mean)

* How many times does that contribution need to occur to raise white mortality
* to black levels?
scalar covid_units_cdrst10 = diffneeded_cdr_cdrst10covid / covid_contrib_cdrst10
scalar list covid_contrib_cdrst10 covid_units_cdrst10
	// covid_units 3.687642, meaning current Covid mortality has to be multiplied by ~3.7
gen covid_mx100k = covidrate100k * covid_units_cdrst10
list agegroup mx100k covidrate100k covid_mx100k if black==0 & year==2017

* Check this result
gen mx_cf = mx100k + covid_mx100k
gen temp_cdr_cf_piece = mx_cf * c_a_st
by yearnum black: egen cdr_st_cf_cdrst10covid = total(temp_cdr_cf_piece)
sum cdr_st_cf_cdrst10covid if black==0 & year==2017
disp r(mean)
scalar list blackmin_cdrst10covid // should be the same; is --EWF, 6/26/2020

* How many deaths is that?
gen deathsage_covid = truepopw2017 * covid_mx100k / 100000 if black==0 & year==2017
by yearnum black: egen deaths_cf = total(deathsage_covid)
sum deaths_cf if black==0 & year==2017
scalar deathsneeded_cdrst10covid = r(mean)
scalar list deathsneeded_cdrst10covid
	// 407,793.86 --> June 17 release
	// 407,772.37 --> June 24 release

disp deathsneeded_cdrst10covid / white2017_cdr
	// .18707368 --> June 17 release
	// .18706382 --> June 24 release
	
list age c_a truepopw2017 covidrate100k covid_mx deathsage_covid ///
	deaths_cf if black==0 & year==2017
list age c_a c_a_st if black==0 & year==2017

table age if black==0 & year==2017, ///
	c(min mx min covid_mx min mx_cf)

* age distribution of all-cause mortality vs. covid mortality
* for whites
foreach m in mx covid_mx {
	gen weight_`m' = round(`m' * 100000000) 
		// creates integer weights without loss of precision
}

recode agegroup (0/14=1) (15=2) (25=3) (35=4) (45=5) (55=6) ///
	(65=7) (75=8) (85=9), gen(agecat)
keep if black==0 & year==2017
gen obs=1
preserve
collapse (sum) deaths_2017=obs [fweight=weight_mx], by(agecat)
save mx_whites2017.dta, replace
restore
collapse (sum) deaths_covid=obs [fweight=weight_covid_mx], by(agecat)
merge 1:1 agecat using mx_whites2017.dta
twoway (hist agecat [fweight=deaths_covid], bin(9) ///
		color("67 220 197") ///
		legend(label(1 "Covid-19"))) ///
	(hist agecat [fweight=deaths_2017], bin(9) ///
		fcolor(none) lcolor("105 27 158") lwidth(thick) ///
		legend(label(2 "All-cause, pre-Covid-19"))), ///
	xlabel(1 "0-14" 2 "15-29" 3 "25-34" 4 "35-44" 5 "45-54" 6 "55-64" ///
			7 "65-74" 8 "75-84" 9 "85+") ///
		xtitle("Age") ytitle("Density") ///
	legend(on) ///
	title("Age Pattern of White Mortality Rates") ///
	saving(age_covid_other.gph, replace)
	graph export age_covid_other.png, replace

twoway (hist agecat [fweight=deaths_covid], bin(9) ///
		color("67 220 197") ///
		legend(label(1 "Covid-19"))) ///
	(hist agecat [fweight=deaths_2017], bin(9) ///
		fcolor(none) lcolor("105 27 158") lwidth(thick) ///
		legend(label(2 "All-cause, pre-Covid-19"))), ///
	xlabel("") ///
		xtitle("") ytitle("Density") ///
	legend(off) ///
	title("Age Pattern of White Mortality Rates") ///
	saving(age_covid_other_nolabel.gph, replace)
	graph export age_covid_other_nolabel.png, replace	
	
* --------------------------------
* 9. Create single-year empirical Covid age distribution
* --------------------------------
use CDC_Covid_rates.dta, clear

** Assign covid rates to the average age of each age group and 
* take the natural log for the interpolation
* First, create a new observation for each age group to represent its
* mean age
expandby 2 if black==0 & year==2017, by(agegroup) gen(fake_meanage)
// drop if age==0 & fake_meanage==0 // keep duplicate age 0 since used differently
bysort yearnum black agegroup: egen tempcovid = mean(covidrate)
replace covidrate = tempcovid if fake_meanage==1
replace mx = . if fake_meanage==1
replace age = meanage if fake_meanage==1
replace c_a_st = . if fake_meanage==1
replace c_a = . if fake_meanage==1
replace truepopw2017 = . if fake_meanage==1
table agegroup, c(min meanage max meanage min covidrate max covidrate)

* Make log covid mortality only for the meanage observation
gen lncovidrate_meanage = ln(covidrate) if fake_meanage==1
sort yearnum black agegroup age
list agegroup age meanage fake_meanage covidrate lncovid if black==0 & year==2017
list age covidrate lncovid if black==0 & year==2017 & fake_meanage==1

* Interpolate covid rates
ipolate lncovid age if black==0 & year==2017, epolate gen(lncovid_interp)
gen covidrate_interp = exp(lncovid_interp)
gen covidrate_interp_100k = covidrate_interp * 100000
list age covidrate covidrate_interp* if black==0 & year==2017

line covidrate_interp_100k age if black==0, lwidth(thick) || ///
	scatter covidrate100k meanage if black==0, msize(large) ///
	ytitle("Deaths Per 100,000") xtitle("") ///
	title("Covid Mortality Among Whites by Age") ///
	saving(covid_age.gph, replace)
	graph export covid_age.png, replace

* --------------------------------
* 10. Direct standardization with actual Covid age distribution,
* single-year age units interpolated/extrapolated from the CDC data
* --------------------------------	
* Standardize mortality using 5-year age units
table yearnum black, c(min cdr_st) // same as previous single-year standardization

* Get difference we need covid to contribute
* This replicates scalars used in prior single-age standardization.
* Naming convention: These scalars, which don't depend on assumption of
* Covid age pattern, are labeled _cdr_st; ones specific to this analysis
* are labeled _stcovid
sum cdr_st if black==1 
scalar blackmin_cdr_st = r(min)
sum year if cdr_st == blackmin_cdr_st
scalar year_blackmin_cdr_st = r(max)
sum cdr_st if black==0 & year==2017
scalar white2017_cdr_st = r(mean)
scalar diffneeded_cdr_st = blackmin_cdr_st - white2017_cdr_st

* Make standardized contribution of covid mortality
gen temp_covid_piece = covidrate_interp_100k * c_a_st if fake_meanage==0
by yearnum black: egen covidcontrib = total(temp_covid_piece), missing
sum covidcontrib
scalar covid_contrib_stcovid = r(mean)
	// This represents the standardized white deaths at CURRENT Covid rates

* How many times does that contribution need to occur to raise white mortality
* to black levels?
scalar covid_units_stcovid = diffneeded_cdr_st / covid_contrib_stcovid
gen covid_mx = covidrate_interp_100k * covid_units_stcovid if fake_meanage==0

* Check this result
gen mx_cf = mx100k + covid_mx if fake_meanage==0
gen temp_cdr_cf_piece = mx_cf * c_a_st if fake_meanage==0
by yearnum black: egen cdr_st_cf = total(temp_cdr_cf_piece)
sum cdr_st_cf if black==0 & year==2017
disp r(mean)
scalar list blackmin_cdrst // should be the same; is. --EWF, 6/26/2020

list age mx* covidrate* if black==0 & year==2017

* How many deaths is that?
gen deathsage_covid = truepopw2017 * covid_mx / 100000 if black==0 & year==2017
by yearnum black: egen deaths_cf = total(deathsage_covid)
sum deaths_cf if black==0 & year==2017
scalar deathsneeded_stcovid = r(mean)
scalar list covid_contrib_stcovid covid_units_stcovid deathsneeded_stcovid
	// Deaths: 399,327.39 --> June 17 release
	// Deaths: 399,310.45 --> June 24 release
	
disp deathsneeded_stcovid / white2017_cdr
	// .18318972 --> June 17 release
	// .18318195 --> June 24 release

list age c_a truepopw2017 covidrate_interp_100k covid_mx deathsage_covid ///
	deaths_cf if black==0 & year==2017
list age c_a c_a_st if black==0 & year==2017

save covid_age.dta, replace
	
* --------------------------------
* 11. Life expectancy
* --------------------------------
* Strategy:
* Get current life expectancy gap
* Simulate white mx* that is 2017 mx plus imagined Covid mortality
* Like with directly age-standardized mortality, do this two ways:
* a. assuming Covid mortality is proportional to all-cause mortality
* b. assuming Covid mortality follows empirical age pattern so far
* To do this, import an age structure of Covid mx and then solve for
* the multiplier needed to make it generate the right life expectancy gap

* How much life expectancy change is needed?
use lifetables.dta, clear

table yearnum black if age==0, c(p50 ex)
sum ex if black==1 & age==0
scalar blackmax_e0 = r(max)
sum year if black==1 & age==0 & ex==blackmax_e0
scalar year_blackmax_e0 = r(max)
sum ex if black==0 & year==2017 & age==0
scalar white2017_e0 = r(mean)
scalar diffneeded_e0 = white2017_e0 - blackmax_e0
scalar list diffneeded_e0
	* 3.2146301 years lowered white e0 needed

***** Proportional change in mortality
* Try arbitrary mortality factor, do trial and error
scalar m_e0prop = 1.3069449 
gen mxprime = mx*scalar(m_e0prop)
gen qxprime = scalar(n)*mxprime / (1 + (scalar(n)-nax)*mxprime)
replace qxprime = 1 if age==maxage_LT
gen lxprime = lx if age==0
gen dxprime = lxprime * qxprime if age==0
sort yearnum black age
forvalues age=1/`=maxage_LT' {
	replace lxprime = lxprime[_n-1] - dxprime[_n-1] if age==`age'
	replace dxprime = lxprime * qxprime if age==`age'
}
sort yearnum black age
gen Lxprime = lxprime[_n+1]*scalar(n) + dxprime * nax if age<maxage_LT
replace Lxprime = lxprime/mxprime if age==maxage_LT
gen Txprime = Lxprime if age==maxage_LT
sort yearnum black age
local a=maxage_LT-1
forvalues age=`a'(-1)0 {
	replace Txprime = Lxprime + Txprime[_n+1] if age==`age'
}
gen exprime = Txprime/lxprime

list age *prime if black==0 & year==2017
sum exprime if age==0 & black==0 & year==2017
disp r(max)
scalar list blackmax_e0
	// These should be equal; are. --EWF, 6/26/2020

* Translate the mortality function into deaths, based on 2017 white population
keep if black==0 & year==2017
merge 1:1 age using mortality_comparison_whites2017.dta, keepusing(c_a) gen(_c_a_merge)
drop _c_a_merge
gen deaths_e0prop_piece = (scalar(m_e0prop)-1)*mxprime * c_a ///
	if black==0 & year==2017
egen white2017_CDR_e0prop = total(deaths_e0prop_piece) if black==0 & year==2017
sum white2017_CDR_e0prop if black==0 & year==2017 // CDR associated with this counterfactual
scalar deaths_e0prop = popw * r(mean)
scalar list deaths_e0prop
	// 869498.97

disp scalar(deaths_e0prop) / white2017_cdr
	// .3988789

* --------------------------------
* 12. Life expectancy, using single-year Covid estimates of age shape 
* of excess mortality
* --------------------------------
use covid_age.dta, clear
drop if fake_meanage==1

* Try arbitrary mortality factor, do trial and error
* scalar m_e0 = 8.608276 // This matches the 6/17/20 data release 
scalar m_e0 = 8.22573 // This matches the 6/24/20 data release
gen mx_covid_e0covid = scalar(m_e0)*covidrate_interp
gen mxprime = mx + mx_covid_e0covid
gen qxprime = scalar(n)*mxprime / (1 + (scalar(n)-nax)*mxprime)
replace qxprime = 1 if age==maxage_LT
gen lxprime = lx if age==0
gen dxprime = lxprime * qxprime if age==0
sort yearnum black age
quietly forvalues age=1/`=maxage_LT' {
	replace lxprime = lxprime[_n-1] - dxprime[_n-1] if age==`age'
	replace dxprime = lxprime * qxprime if age==`age'
}
sort yearnum black age
gen Lxprime = lxprime[_n+1]*scalar(n) + dxprime * nax if age<maxage_LT
replace Lxprime = lxprime/mxprime if age==maxage_LT
gen Txprime = Lxprime if age==maxage_LT
sort yearnum black age
local a=maxage_LT-1
quietly forvalues age=`a'(-1)0 {
	replace Txprime = Lxprime + Txprime[_n+1] if age==`age'
}
gen exprime = Txprime/lxprime

list age *prime if black==0 & year==2017
sum exprime if age==0 & black==0 & year==2017
disp r(max)
scalar list blackmax_e0
	// should be equal; are -- EWF, 6/26/20

* Translate the mortality function into deaths, based on 2017 white population
gen deaths_e0covid_piece = mx_covid_e0covid * c_a if black==0 & year==2017
egen deaths_e0covid_relative = total(deaths_e0covid_piece) if black==0 & year==2017
sum deaths_e0covid_relative if black==0 & year==2017
scalar deaths_e0covid = scalar(popw) * r(mean)
scalar list deaths_e0covid
	// 1,033,125.4 --> June 17 release
	// 1,032,268.5 --> June 24 release

* What proportion of 2017 white deaths is that?
disp scalar(deaths_e0covid) / white2017_cdr
	// .47394183 --> June 17 release
	// .47354871 --> June 24 release

* Alternative measure
gen deaths_e0covid_alt_piece = truepopw2017 * mx_covid_e0covid
egen deaths_e0covid_alt = total(deaths_e0covid_alt_piece) if black==0 & year==2017
sum deaths_e0covid_alt
	// should give same result and does --EWF, 6/26/20

* --------------------------------
* 13. Metrics comparing hypothetical deaths to current Covid mortality
* --------------------------------	
	
* How much do Covid deaths need to increase according to these models?
scalar deaths_truecovid_whites = 57630
scalar deaths_truecovid_blacks = 24868
	// These numbers as of the June 24, 2020 CDC release
	// https://data.cdc.gov/NCHS/Provisional-Death-Counts-for-Coronavirus-Disease-C/pj7m-y5uh
scalar min_multiplier = deathsneeded_stprop / scalar(deaths_truecovid_whites)
	// how many times current Covid deaths need to be scaled up to reach
	// minimum level (the proportional direct standardization)
scalar max_multiplier = deaths_e0covid / scalar(deaths_truecovid_whites)
scalar list min_multiplier max_multiplier
	// 5.6 to 17.9
scalar deaths_covidblack_min = deaths_truecovid_blacks * min_multiplier
scalar list deaths_covidblack_min
	// 138,476.73
disp deaths_truecovid_blacks * max_multiplier
	// 445,435.58

scalar deaths_truecovid_total = 57630 + 24868 + 738 + 5356 + 17921 + 1484
	// CDC, 6/24/20 data release
scalar deaths_covidtotal_min = deaths_truecovid_total * min_multiplier
scalar cdr_covidblack_min = (deaths_covidblack_min / popb) * 100000
scalar list deaths_truecovid_total deaths_covidtotal_min cdr_covidblack_min
	// deaths_truecovid_total =     107,997
	// deaths_covidtotal_min =  601,378.11
	// cdr_covidblack_min =  314.83363
	
scalar cdr_covid_nyc = 210.57
	// June 24, 2020 update from https://github.com/nychealth/coronavirus-data/blob/master/by-age.csv
disp cdr_covidblack_min / cdr_covid_nyc
	// 1.4951495
	
* --------------------------------
* 14. Life expectancy using Covid age distribution in 10-year units
* This one takes Covid estimates from the CDC with extrapolation/interpolation
* --------------------------------	
use CDC_Covid_rates.dta, clear

* Make 10-year lifetable
collapse (sum) dx10=dx Lx10=Lx c_a10=c_a (max) lx10=lx covidrate=covidrate, ///
	by(yearnum black agegroup)
gen truepopw2017 = scalar(popw) * c_a if black==0 & year==2017

* Create new mx and nax
gen mx10 = dx10 / Lx10
sum age
scalar maxage_LT10 = r(max) // used to make formulas general
gen n = 1 if agegroup==0
replace n = 4 if agegroup==1
replace n = 10 if agegroup>1
tab agegroup n, m
gen nax10 = (Lx10 - n*lx10[_n+1])/dx10
sum age
replace nax10 = Lx10/lx10 if age==maxage_LT10
table age, c(min nax10 max nax10)

* Create new Tx and ex
gen Tx10 = Lx10 if agegroup==85
sort yearnum black agegroup
forvalues a=75(-10)5 {
	replace Tx10 = Tx10[_n+1] + Lx10 if age==`a'
}
foreach a in 1 0 {
	replace Tx10 = Tx10[_n+1] + Lx10 if age==`a'
}
gen ex10 = Tx10/lx10

* How much change in life expectancy is needed?
table yearnum black if agegroup==0, c(p50 ex10)
sum ex10 if black==1 & age==0
scalar blackmax_e010 = r(max)
sum year if black==1 & age==0 & ex10==blackmax_e010
scalar year_blackmax_e010 = r(max) - .1
sum ex10 if black==0 & year==2017.1 & age==0
scalar white2017_e010 = r(mean)
scalar diffneeded_e010 = white2017_e010 - blackmax_e010
scalar list white2017_e0 white2017_e010 blackmax_e0 blackmax_e010 ///
	diffneeded_e0 diffneeded_e010
	// Essentially identical; the age units are not really
	// a distorting factor. --EWF, 6/24/20
	
* Estimate additional mortality with Covid, empirical age distribution
* Trial and error to estimate multiplier
* scalar m_e010 = 9.15017 // This matches the 6/17/20 data release
scalar m_e010 = 8.74451 // This matches the 6/24/20 data release
gen mxprime = mx10 + scalar(m_e010)*covidrate
gen qxprime = n*mxprime / (1 + (n-nax10)*mxprime)
replace qxprime = 1 if age==maxage_LT10
gen lxprime = lx10 if age==0
gen dxprime = lxprime * qxprime if age==0
replace lxprime = lx10[_n-1] - dxprime[_n-1] if age==1
replace dxprime = lxprime * qxprime if age==1
sort yearnum black age
forvalues age=5(10)`=maxage_LT10' {
	replace lxprime = lxprime[_n-1] - dxprime[_n-1] if age==`age'
	replace dxprime = lxprime * qxprime if age==`age'
}
sort yearnum black age
gen Lxprime = lxprime[_n+1]*n + dxprime * nax if age<maxage_LT10
replace Lxprime = lxprime/mxprime if age==maxage_LT10
gen Txprime = Lxprime if age==maxage_LT10
sort yearnum black age
forvalues age=75(-10)5 {
	replace Txprime = Lxprime + Txprime[_n+1] if age==`age'
}
foreach age in 1 0 {
	replace Txprime = Lxprime + Txprime[_n+1] if age==`age'
}
gen exprime = Txprime/lxprime

list age *prime if black==0 & yearnum==2017.1
sum exprime if age==0 & black==0 & yearnum==2017.1
disp r(max)
scalar list blackmax_e010
	// should be equal; are. --EWF, 6/26/20

* Translate the mortality function into deaths, based on 2017 white population
gen deaths_e010covid_piece = scalar(m_e010)*covidrate * c_a10 if black==0 & yearnum==2017.1
egen deaths_e010covid_relative = total(deaths_e010covid_piece) if ///
	black==0 & yearnum==2017.1
sum deaths_e010covid_relative if black==0 & yearnum==2017.1
scalar deaths_e010covid = popw * r(mean)
scalar list deaths_e010covid
	// 1,011,861.6 --> June 17 release
	// 1,011,205.2 --> June 24 release

disp deaths_e010covid / white2017_cdr
	// .46418713 --> June 17 release
	// .46388602 --> June 24 release
	
* --------------------------------
* 15. Graph NCHS e0 and CDR* from 1900-2017
* --------------------------------
import delimited "NCHS_-_Death_rates_and_life_expectancy_at_birth.csv", encoding(ISO-8859-1)clear
* from https://data.cdc.gov/NCHS/NCHS-Death-rates-and-life-expectancy-at-birth/w9j2-ggv5
ren averagelifeexpectancyyears e0
ren ageadjusteddeathrate CDRst
save NCHS_e0_CDRst.dta, replace

keep if sex=="Both Sexes"

gen lnCDRst = ln(CDRst)

foreach var in e0 CDRst lnCDRst {
	foreach y in 1918 1957 {
		sum `var' if year==`y' & race=="White"
		scalar white`y'_`var'_NCHS = r(mean)
	}
}
sum e0 if race=="Black"
scalar blackmax_e0_NCHS = r(max)
sum year if race=="Black" & e0==blackmax_e0_NCHS // 2014, as expected
sum CDRst if race=="Black"
scalar blackmin_CDRst_NCHS = r(min)
sum year if race=="Black" & CDRst==blackmin_CDRst_NCHS // 2014, as expected

sum lnCDRst if race=="Black"
scalar blackmin_lnCDRst_NCHS = r(min)

list year if race=="Black" & CDRst > white1918_CDRst_NCHS
	// all years 1900-1931 except 1921; also 1936
list year if race=="Black" & e0 < white1918_e0_NCHS
	// all years 1900-1918 except 1916
tab year if race=="White" & e0==blackmax_e0_NCHS
	// Black e0 in 2014 equals white in 1987 and 1988
sum year if race=="White" & CDRst >= blackmin_CDRst_NCHS
	// latest: 2000
tab year if race=="White" & CDRst==blackmin_CDRst_NCHS
	// none exact
sum year if race=="White" & CDRst <= blackmin_CDRst_NCHS
	// first: 1998
sum year if race=="White" & e0 <= blackmax_e0_NCHS
	// latest: 1988 (almost 30 years gap from 2017)
sum year if race=="White" & e0 >= blackmax_e0_NCHS
	// first: 1987

	
	
/* Color scheme:
White = rgb(37,102,118)
Black = rgb(113,211,244)
*/

sort race year
set scheme uncluttered
line e0 year if race=="Black", lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line e0 year if race=="White", lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") on) ///
	title("Life Expectancy") xtitle("") /// ytitle("Years of Life") ///
	xlabel(1900(20)2020) ///
	yline(`=scalar(white1918_e0_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	/// yline(`=scalar(white1957_e0_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	yline(`=scalar(blackmax_e0_NCHS)', lcolor("113 211 244") lpattern(dash)) ///
	/// ysize(6) xsize(6) ///
	xtitle("") ytitle("") ///
	saving(NCHS_e0_preprocessing.gph, replace)
	graph export NCHS_e0_preprocessing.png, replace
	
line CDRst year if race=="Black", lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line CDRst year if race=="White", lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") on) ///
	title("Age-Adjusted Mortality") xtitle("") /// ytitle("Deaths Per 100,000") ///
	xlabel(1900(20)2020) ///
	yline(`=scalar(white1918_CDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	/// yline(`=scalar(white1957_CDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	yline(`=scalar(blackmin_CDRst_NCHS)', lcolor("113 211 244") lpattern(dash)) ///
	/// ysize(6) xsize(6) ///
	xtitle("") ytitle("") ///
	saving(NCHS_CDRst_preprocessing.gph, replace)
	graph export NCHS_CDRst_preprocessing.png, replace	

grc1leg NCHS_CDRst_preprocessing.gph ///
		NCHS_e0_preprocessing.gph, ///
		ysize(12) xsize(12) ///
		saving(NCHS_e0CDRst_preprocessing.gph, replace)
	graph export NCHS_e0CDRst_preprocessing.png, replace
	
line e0 year if race=="Black", lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line e0 year if race=="White", lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") on) ///
	title("Life Expectancy") xtitle("") /// ytitle("Years of Life") ///
	xlabel(1900(20)2020) ///
	yline(`=scalar(white1918_e0_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	/// yline(`=scalar(white1957_e0_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	yline(`=scalar(blackmax_e0_NCHS)', lcolor("113 211 244") lpattern(dash)) ///
	legend(off) ///
	xtitle("") ytitle("") ///
	saving(NCHS_e0_preprocessing_nolegend.gph, replace)
	graph export NCHS_e0_preprocessing_nolegend.png, replace

line CDRst year if race=="Black", lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line CDRst year if race=="White", lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") on) ///
	title("Age-Adjusted Mortality") xtitle("") /// ytitle("Deaths Per 100,000") ///
	xlabel(1900(20)2020) ///
	yline(`=scalar(white1918_CDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	/// yline(`=scalar(white1957_CDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	yline(`=scalar(blackmin_CDRst_NCHS)', lcolor("113 211 244") lpattern(dash)) ///
	legend(off) ///
	xtitle("") ytitle("") ///
	saving(NCHS_CDRst_preprocessing_nolegend.gph, replace)
	graph export NCHS_CDRst_preprocessing_nolegend.png, replace		
	
line lnCDRst year if race=="Black", lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line lnCDRst year if race=="White", lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") on) ///
	title("Log Age-Adjusted Mortality") xtitle("") /// ytitle("Logged Deaths Per 100,000") ///
	xlabel(1900(20)2020) ///
	yline(`=scalar(white1918_lnCDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	/// yline(`=scalar(white1957_CDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	yline(`=scalar(blackmin_lnCDRst_NCHS)', lcolor("113 211 244") lpattern(dash)) ///
	/// ysize(6) xsize(6) ///
	legend(off) ///
	xtitle("") ytitle("") ///
	saving(NCHS_lnCDRst_preprocessing_nolegend.gph, replace)
	graph export NCHS_lnCDRst_preprocessing_nolegend.png, replace	
	
line lnCDRst year if race=="Black", lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line lnCDRst year if race=="White", lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") on) ///
	title("Log Age-Adjusted Mortality") xtitle("") /// ytitle("Logged Deaths Per 100,000") ///
	xlabel(1900(20)2020) ///
	yline(`=scalar(white1918_lnCDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	/// yline(`=scalar(white1957_CDRst_NCHS)', lcolor("37 102 118") lpattern(dash)) ///
	yline(`=scalar(blackmin_lnCDRst_NCHS)', lcolor("113 211 244") lpattern(dash)) ///
	/// ysize(6) xsize(6) ///
	legend(on) ///
	xtitle("") ytitle("") ///
	saving(NCHS_lnCDRst_preprocessing.gph, replace)
	graph export NCHS_lnCDRst_preprocessing.png, replace	
		
	
grc1leg NCHS_lnCDRst_preprocessing.gph ///
		NCHS_e0_preprocessing.gph, ///
		ysize(12) xsize(12) ///
		saving(NCHS_e0lnCDRst_preprocessing.gph, replace)
	graph export NCHS_e0lnCDRst_preprocessing.png, replace
	* In graph editor, changed xsize to 7
	
list
	
* --------------------------------
* 16. Graphs of key outcomes
* --------------------------------
set scheme uncluttered

use mortality_comparison, clear

keep if age==0

gen cmr100 = cmr * 100
ren ex e0
gen lncdr_st = ln(cdr_st)
sum lncdr_st if black==1
scalar blackmin_lncdr_st = r(min)
sum lncdr_st if black==0 & year==2017
scalar white2017_lncdr_st = r(mean)

* average over multiple observations in years with two life tables,
* to simplify graph
foreach var in cmr100 cdr_st lncdr_st e0 {
	gen temp`var' = `var' if version==2
	bysort black year: egen `var'_v2 = max(temp`var')
	gen `var'_avg = (`var' + `var'_v2)/2 if version==1
	replace `var'_avg = `var' if year>2011 | year==2006
	drop temp* `var'_v2
}
	/*
line cmr100_avg year if black==1, lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line cmr100_avg year if black==0, lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") off ///
		size(small) ring(0) position(5)) ///
	xtitle("") ytitle("Percent of Expected Mortality") ///
	subtitle("Mortality Rates, Indirectly Age-Standardized") ///
	saving(cmr100.gph, replace)

line cdr_st_avg year if black==1, lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line cdr_st_avg year if black==0, lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") off size(small) position(5)) ///
	xtitle("") ytitle("Deaths Per 100,000") ///
	subtitle("Mortality Rates, Directly Age-Standardized")	///
	saving(cdr_st.gph, replace)

line e0_avg year if black==1 & age==0, lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line e0_avg year if black==0 & age==0, lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") off size(small) position(5)) ///
	xtitle("") ytitle("Years of Life") ///
	subtitle("Life Expectancy")	///
	saving(e0.gph, replace)

grc1leg cmr100.gph cdr_st.gph e0.gph, rows(2) ring(0) position(5) ///
	saving(all_2007-2017.gph, replace)
graph export all_2007-2017.png, replace
*/
foreach var in cmr100 cdr_st lncdr_st e0 {
	gen `var'_mark = `var' if (year==2014 & black==1) | (year==2017 & black==0)
}
/*
line cmr100_avg year if black==1, lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line cmr100_avg year if black==0, lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") off ///
		size(small) ring(0) position(5)) || ///
	scatter cmr100_mark year, mcolor(pink) msymbol(X) msize(medlarge) ///
	legend(label(3 "Comparison Year") order(1 2 3) rows(1)) ///
	xtitle("") ytitle("Percent of Expected Death Rate, Based on Ages") ///
	subtitle("Mortality Rates, Indirectly Age-Standardized") ///
	saving(cmr100_marked.gph, replace)

line cdr_st_avg year if black==1, lcolor("113 211 244") lwidth(thick) ///
	legend(label(1 "Black")) || ///
	line cdr_st_avg year if black==0, lcolor("37 102 118") lwidth(thick) ///
	legend(label(2 "White") off size(small) position(5)) || ///
	scatter cdr_st_mark year, mcolor(pink) msymbol(X) msize(medlarge) ///
	xtitle("") ytitle("Deaths Per 100,000 People") ///
	subtitle("Mortality Rates, Directly Age-Standardized")	///
	saving(cdr_st_marked.gph, replace)

grc1leg cmr100_marked.gph cdr_st_marked.gph, rows(1) ///
	saving(cmr_cdrst_marked.gph, replace)
graph export cmr_cdrst_marked.png, replace
*/
* With visualization of hypothetical
expandby 2 if year==2017, by(black) gen(fake_2020)
replace year = 2020 if fake_2020==1
replace cdr_st = blackmin_cdrst if fake_2020==1 & black==0
replace lncdr_st = blackmin_lncdr_st if fake_2020==1 & black==0
replace cmr100 = blackmin_cmr * 100 if fake_2020==1 & black==0
replace e0 = blackmax_e0 if fake_2020==1 & black==0 & age==0
replace cmr100_mark = . if fake_2020==1
replace cdr_st_mark = . if fake_2020==1
replace lncdr_st_mark = . if fake_2020==1
replace e0_mark = . if fake_2020==1
replace black = . if fake_2020==1 & black==1 // used to make vertical line
replace cdr_st = white2017_cdrst if fake_2020==1 & black==.
replace lncdr_st = white2017_lncdr_st if fake_2020==1 & black==.
replace cmr100 = white2017_cmr * 100 if fake_2020==1 & black==.
replace e0 = white2017_e0 if fake_2020==1 & black==. & age==0

foreach var in cmr100 cdr_st lncdr_st e0 {
	gen horiz_`var' = `var' if (black==0 & year==2020) | ///
		(black==1 & year==2014)
	gen vert_`var' = `var' if (black==0 & year==2020) | ///
		(black==. & year==2020)
	gen horiz2_`var' = `var' if (black==. & year==2020) | ///
		(black==0 & year==2017)
}
	
local title_cmr100 "Mortality Rates, Indirectly Age-Standardized"
local title_cdr_st "Age-Adjusted Mortality"
local title_lncdr_st "Log Age-Adjusted Mortality"
local title_e0 "Life Expectancy"
local ytitle_cmr100 "Percent of Expected Death Rate"
local ytitle_cdr_st "Deaths Per 100,000"
local ytitle_lncdr_st "Logged Deaths Per 100,000"
local ytitle_e0 "Years of Life"
local yscale_cdr_st 800(200)1200
local yscale_e0 70(2)80
	
sort black year	
foreach var in cmr100 cdr_st lncdr_st e0 {	
	line horiz_`var' year, /// lines first so markers are on top
			lcolor(gs8) lpattern(dash) lwidth(medium) || ///
		line horiz2_`var' year, ///
			lcolor(gs8) lpattern(dash) lwidth(medium) || ///
		line vert_`var' year, ///
			lcolor(black) lpattern(solid) lwidth(medium) || ///
		line `var'_avg year if black==1 & fake_2020==0, /// real data
			lcolor("113 211 244") lwidth(vthick) ///
			legend(label(4 "Non-Hispanic Black")) || ///
		line `var'_avg year if black==0 & fake_2020==0, ///
			lcolor("37 102 118") lwidth(vthick) ///
			legend(label(5 "Non-Hispanic White") off ///
			size(small)) || ///
		scatter `var'_mark year if black==1, ///
			mcolor("113 211 244") msize(large) || ///
		scatter `var'_mark year if black==0, ///
			mcolor("37 102 118") msize(large) || ///
		scatter `var'_mark year, ///
			mcolor(pink) msymbol(X) msize(vlarge) ///
			legend(label(8 "Comparison Year")) || ///
		scatter `var' year if black==0 & fake_2020==1, /// hypothetical year
			mcolor("37 102 118") msize(large) || ///
		scatter `var' year if black==0 & fake_2020==1, /// marker on hypothetical
			mcolor("gold") msymbol(X) msize(vlarge) ///
			legend(label(10 "Hypothetical 2020 White")) ///
		legend(order(4 5 8 10) rows(2) colfirst size(small)) ///
		xtitle("") /// ytitle("`ytitle_`var''") ///
		ytitle("") ///
		title("`title_`var''") ///
		saving(`var'_marked_white2020.gph, replace)
		
		line horiz_`var' year, /// lines first so markers are on top
			lcolor(gs8) lpattern(dash) lwidth(medium) || ///
		line horiz2_`var' year, ///
			lcolor(gs8) lpattern(dash) lwidth(medium) || ///
		line vert_`var' year, ///
			lcolor(black) lpattern(solid) lwidth(medium) || ///
		line `var'_avg year if black==1 & fake_2020==0, /// real data
			lcolor("113 211 244") lwidth(vthick) ///
			legend(label(4 "Non-Hispanic Black")) || ///
		line `var'_avg year if black==0 & fake_2020==0, ///
			lcolor("37 102 118") lwidth(vthick) ///
			legend(label(5 "Non-Hispanic White") ///
			size(small)) || ///
		scatter `var'_mark year if black==1, ///
			mcolor("113 211 244") msize(large) || ///
		scatter `var'_mark year if black==0, ///
			mcolor("37 102 118") msize(large) || ///
		scatter `var'_mark year, ///
			mcolor(pink) msymbol(X) msize(vlarge) ///
			legend(label(8 "Comparison Year")) || ///
		scatter `var' year if black==0 & fake_2020==1, /// hypothetical year
			mcolor("37 102 118") msize(large) || ///
		scatter `var' year if black==0 & fake_2020==1, /// marker on hypothetical
			mcolor("gold") msymbol(X) msize(vlarge) ///
			legend(label(10 "Hypothetical 2020 White")) ///
		legend(order(4 5 8 10) rows(2) colfirst size(small) on /* holes(4 6) */) ///
		xtitle("") /// ytitle("`ytitle_`var''") ///
		ytitle("") ///
		title("`title_`var''") ///
		saving(`var'_marked_white2020_legend.gph, replace)
}

grc1leg cmr100_marked_white2020.gph cdr_st_marked_white2020.gph ///
	e0_marked_white2020.gph, rows(2) /* holes(4 6) */ ///
	/// position(5) /// bmargin(large) rmargin(large) ///
	saving(all_marked_white2020_legend.gph, replace)
graph export all_marked_white2020_legend.png, replace
grc1leg cdr_st_marked_white2020.gph ///
	e0_marked_white2020.gph, rows(1) /* holes(4 6) */ ///
	saving(e0CDRst_marked_white2020_legend.gph, replace)
graph export e0CDRst_marked_white2020_legend.png, replace

grc1leg lncdr_st_marked_white2020.gph ///
	e0_marked_white2020.gph, rows(1) /* holes(4 6) */ ///
	saving(e0lnCDRst_marked_white2020_legend.gph, replace)
graph export e0lnCDRst_marked_white2020_legend.png, replace

graph use cmr100_marked_white2020_legend.gph
graph export cmr100_marked_white2020_legend.png, replace

/* 
grc1leg cmr100_marked_white2020.gph cdr_st_marked_white2020.gph ///
	e0_marked_white2020.gph, rows(2) /* holes(4 6) */ ///
	position(5) /// bmargin(large) rmargin(large) ///
	legend(off) ///
	saving(cmr_cdrst_marked_white2020_nolegend.gph, replace)
graph export cmr_cdrst_marked_white2020_nolegend.png, replace
*/

log close
* done!
