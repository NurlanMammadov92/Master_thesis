/*
*This code creates volatility proxies from daily data at monthly frequency and merges in macroeconomic predictors.

*NOTE: Be sure to change working directory as appropriate
*/

*Set working directory*** 
*********CHANGE TO APPROPRIATE DIRECTORY BEFORE RUNNING!*************
cd "C:\Users\nurik\Desktop\trials\PAYE-data\commands"

*******Get daily returns*****************************************************
use dailyreturn2010.dta


****Sort data********************************************************************
sort year month day

drop if year > 2010		   
drop if year < 1886

summ

***********************************************************************************
*Merge in monthly data 
**********************************************************************************
merge year using mergemonthlyfinal.dta, keep(rfree fitret fitret1big fitret1small)


*Repeat drop to get rid of pre-1886 observations from quarterly dataset
drop if year < 1886

*****************************************************************************
*Generate a variable with the number of trading days per month
****************************************************************************
egen numtradeday = count(day), by(year month)


drop if year > 2010
drop if year < 1886

#delimit;
local MeanList "fitret
fitret1big
fitret1small";
#delimit cr


******************************************************************************
*Generate implied daily risk-free rate and daily excess return
******************************************************************************

*convert monthly rate to daily
gen drfree = (1+rfree)^(1/numtradeday)-1
*compute excess returns
gen eret = retd - drfree
gen eretnan = nanvwretd - drfree
//squared daily S&P 500 return; uses Schwert data up to when CRSP data record starts
gen retd_sq = retd^2
//squared daily NAN return from CRSP
gen nanretd_sq = nanvwretd^2
//excess return versions
gen eret_sq = eret^2
gen naneret_sq = eretnan^2


//Note: 1.2533 is adjustment factor used by Schwert (1989)
*****ARET variable corresponds to the absolute returns volatility proxy****

gen aret = 1.2533*abs(eret)*(numtradeday^(-1/2))

************************************************************************************
*These loops create daily squared excess returns for each filtered mean series
*****Alternative realized variance proxies based on the estimated regressions***
**************************************************************************************
**********1)First alternative corrresponds to the FILTt variable in the Unpublished Appendix.
foreach meanitem in `MeanList' {
//convert fitted conditional mean return to daily
gen d`meanitem' = (1+`meanitem')^(1/numtradeday) - 1
//compute daily shock as: daily return - (expected excess + rfree rate)
gen eret`meanitem' = retd - (drfree + d`meanitem')
//compute squared daily shocks
gen eret`meanitem'_sq = eret`meanitem'^2
}

****Prepare Covariance terms for FSS correction********************************
**********2)Second alternative corrresponds to the FSSt variable in the Unpublished Appendix.
gen crossprodret = 2*(retd[_n]*retd[_n-1])
gen crossproderet = 2*(eret[_n]*eret[_n-1])

replace crossprodret = . if month[_n] != month[_n-1]
replace crossproderet = . if month[_n] != month[_n-1]
*These two steps generate cross product of adjacent returns w/in month


***********************************************************************
*Create monthly realized variances***********************************
***********************************************************************
#delimit;
collapse (sum) retd_sq eret_sq nanretd_sq naneret_sq aret
eretfitret_sq
eretfitret1big_sq
eretfitret1small_sq
crossprodret
crossproderet
, by(year month);
#delimit cr

summ
sort year month


***************************************************************************
*Rename variables for easier interpretation************
****RF stands for risk free-excess returns************
*****3 various volatility proxies are calculated: 
**1) realized variance proxy;
***2)time-varying filtered realized variance proxy;
*****3)realized variance proxy based on absolute returns-avol.
**************************************************************************
rename retd_sq rvar
rename eret_sq rvarrf
rename nanretd_sq rvar_nan
rename naneret_sq rvarrf_nan
rename eretfitret_sq rvarfilter
rename eretfitret1big_sq rvarfilter1big
rename eretfitret1small_sq rvarfilter1small
rename aret avol

**************************************************************************
*Generate FSS-type realized variances
**************************************************************************
gen fssvar = rvar + crossprodret
gen fssvarrf = rvarrf + crossproderet

**************************************************************************
*annualize proxy based on absolute returns
**************************************************************************
replace avol = 12^(1/2)*avol


*************************************************************************************
*recode zero variances (pre-coverage) as missing for NAN and filtered variance measures
*************************************************************************************
replace rvar_nan = . if rvar_nan ==0
replace rvarrf_nan = . if rvarrf_nan ==0
replace rvarfilter = . if rvarfilter ==0
replace rvarfilter1big = . if rvarfilter1big ==0
replace rvarfilter1small = . if rvarfilter1small ==0

************************************************************************************************************
*Generate log realized variances, realized volatilities, log realized volatilities, standardized quantities
************************************************************************************************************

gen rvol = (12^(1/2))*rvar^(1/2)
gen rvolrf = (12^(1/2))*rvarrf^(1/2)
gen rvol_nan = (12^(1/2))*rvar_nan^(1/2)
gen rvolrf_nan = (12^(1/2))*rvarrf_nan^(1/2)
gen fssvol = (12^(1/2))*fssvar^(1/2)
gen fssvolrf = (12^(1/2))*fssvarrf^(1/2)
gen rvolfilter = (12^(1/2))*rvarfilter^(1/2)
gen rvolfilter1big = (12^(1/2))*rvarfilter1big^(1/2)
gen rvolfilter1small = (12^(1/2))*rvarfilter1small^(1/2)

gen lrvol = ln(rvol)
gen lrvolrf = ln(rvolrf)
gen lrvol_nan = ln(rvol_nan)
gen lrvolrf_nan = ln(rvolrf_nan)
gen lfssvol = ln(fssvol)
gen lfssvolrf = ln(fssvolrf)
gen lrvolfilter = ln(rvolfilter)
gen lrvolfilter1big = ln(rvolfilter1big)
gen lrvolfilter1small = ln(rvolfilter1small)
gen lavol = ln(avol)

*this standardized log vol. is used in business cycle figures
egen zlrvol = std(lrvol)

sort year month

**************************************************************
*Now merge back into monthly risk-free rate file and clean-up
***************************************************************
merge year month using mergemonthlyfinal.dta

drop if year < 1886
sort year month
drop _merge

*generate t = m(1934m1)+ _n - 1
format t %tm
tsset t

drop crossprodret crossproderet


************************************************************
*Save resulting dataset for further analysis
************************************************************
save monthlydatafinal.dta, replace
**********************************************************
*Export results for subsequent Eviews analysis
************************************************************
outsheet using "monthlydata.csv", comma replace



