*This code creates volatility proxies from daily data at quarterly frequency and merges in macroeconomic predictors.

*NOTE: Be sure to change working directory as appropriate
*/


*Set working directory*** 
*********CHANGE TO APPROPRIATE DIRECTORY BEFORE RUNNING!*************
cd "C:\Users\nurik\Desktop\trials\PAYE-data\commands"

*******Get daily returns*****************************************************
use dailyreturn2010.dta


****Sort data********************************************************************
sort year quarter day

drop if year > 2010		   
drop if year < 1886

summ

***********************************************************************************
*Merge in quarterly data on expected returns, risk-free rates
**********************************************************************************
#delimit;
merge year using mergequarterlyfinal.dta, 
keep(rfree fitret fitret1big fitret1small fitret2big fitret2small);
#delimit cr

*Repeat drop to get rid of pre-1886 observations from quarterly dataset
drop if year < 1886

*****************************************************************************
*Generate a variable with the number of trading days per quarter
****************************************************************************
egen numtradeday = count(day), by(year quarter)


drop if year > 2010
drop if year < 1886

#delimit;
local MeanList "fitret
fitret1big
fitret1small
fitret2big
fitret2small";
#delimit cr

******************************************************************************
*Below lines generate implied daily risk-free rate and daily excess return
******************************************************************************
gen drfree = (1+rfree)^(1/numtradeday)-1
gen eret = retd - drfree
gen eretnan = nanvwretd - drfree
//squared daily S&P 500 return; uses Schwert data up to when CRSP data record starts
gen retd_sq = retd^2
//squared daily NAN return from CRSP
gen nanretd_sq = nanvwretd^2
//excess return versions
gen eret_sq = eret^2
gen naneret_sq = eretnan^2


//Note: 1.2533 is correction factor used by Schwert (1989).

gen aret = 1.2533*abs(eret)*(numtradeday^(-1/2))


************************************************************************************
*These loops create daily squared excess returns for each filtered mean series
**************************************************************************************
foreach meanitem in `MeanList' {
//convert fitted conditional mean return to daily
gen d`meanitem' = (1+`meanitem')^(1/numtradeday) - 1
//compute daily shock as: daily return - (expected excess + rfree rate)
gen eret`meanitem' = retd - (drfree + d`meanitem')
//compute squared daily shocks
gen eret`meanitem'_sq = eret`meanitem'^2
}

****Prepare Covariance terms for FSS correction********************************
gen crossprodret = 2*(retd[_n]*retd[_n-1])
gen crossproderet = 2*(eret[_n]*eret[_n-1])

replace crossprodret = . if quarter[_n] != quarter[_n-1]
replace crossproderet = . if quarter[_n] != quarter[_n-1]
*These two steps generate cross product of adjacent returns w/in quarter

***********************************************************************
*Create quarterly realized variances***********************************
***********************************************************************
#delimit;
collapse (sum) retd_sq eret_sq nanretd_sq naneret_sq aret
eretfitret_sq
eretfitret1big_sq
eretfitret1small_sq
eretfitret2big_sq
eretfitret2small_sq
crossprodret
crossproderet
, by(year quarter);
#delimit cr

summ
sort year quarter

***************************************************************************
*Rename variables
**************************************************************************
rename retd_sq rvar
rename eret_sq rvarrf
rename nanretd_sq rvar_nan
rename naneret_sq rvarrf_nan
rename eretfitret_sq rvarfilter
rename eretfitret1big_sq rvarfilter1
rename eretfitret1small_sq rvarfilter1alt
rename eretfitret2big_sq rvarfilter2
rename eretfitret2small_sq rvarfilter2alt
rename aret avol

**************************************************************************
*Generate FSS-type realized variances
**************************************************************************
gen fssvar = rvar + crossprodret
gen fssvarrf = rvarrf + crossproderet

**************************************************************************
*annualize proxy based on absolute returns
**************************************************************************
replace avol = 2*avol

*************************************************************************************
*recode zero variances (pre-coverage) as missing for NAN and filtered variance measures
*************************************************************************************
replace rvar_nan = . if rvar_nan ==0
replace rvarrf_nan = . if rvarrf_nan ==0
replace rvarfilter = . if rvarfilter == 0
replace rvarfilter1 = . if rvarfilter1 == 0
replace rvarfilter1alt = . if rvarfilter1alt == 0
replace rvarfilter2 = . if rvarfilter2 ==0
replace rvarfilter2alt = . if rvarfilter2alt ==0


************************************************************************************************************
*Generate realized volatilities, log realized volatilities, standardized quantities
************************************************************************************************************
*NOTE: Paye multiplies the realized volatility by 2 = 4^(1/2) to annualize this quarterly quantity

gen rvol = 2*rvar^(1/2)
gen rvolrf = 2*rvarrf^(1/2)
gen rvol_nan = 2*rvar_nan^(1/2)
gen rvolrf_nan = 2*rvarrf_nan^(1/2)
gen fssvol = 2*fssvar^(1/2)
gen fssvolrf = 2*fssvarrf^(1/2)
gen rvolfilter = 2*rvarfilter^(1/2)
gen rvolfilter1 = 2*rvarfilter1^(1/2)
gen rvolfilter2 = 2*rvarfilter2^(1/2)
gen rvolfilter1alt = 2*rvarfilter1alt^(1/2)
gen rvolfilter2alt = 2*rvarfilter2alt^(1/2)

gen lrvol = ln(rvol)
gen lrvolrf = ln(rvolrf)
gen lrvol_nan = ln(rvol_nan)
gen lrvolrf_nan = ln(rvolrf_nan)
gen lfssvol = ln(fssvol)
gen lfssvolrf = ln(fssvolrf)
gen lrvolfilter = ln(rvolfilter)
gen lrvolfilter1 = ln(rvolfilter1)
gen lrvolfilter2 = ln(rvolfilter2)
gen lrvolfilter1alt = ln(rvolfilter1alt)
gen lrvolfilter2alt = ln(rvolfilter2alt)
gen lavol = ln(avol)

*This standardized volatilty is used in business cycle figures
egen zlrvol = std(lrvol)

sort year quarter

**************************************************************
*Merge back into quarterly data file and clean-up
***************************************************************
merge year quarter using mergequarterlyfinal

drop if year < 1886
sort year quarter
drop _merge

drop crossprodret crossproderet

************************************************************
*Save resulting dataset for further analysis
************************************************************
save quarterlydatafinal.dta, replace
**********************************************************
*Export results for subsequent Eviews analysis
************************************************************
outsheet using "quarterlydata.csv", comma replace



