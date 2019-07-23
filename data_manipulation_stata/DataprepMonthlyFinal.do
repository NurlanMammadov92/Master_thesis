*This code processes data to construct final predictor/macroeconomic variables at the monthly frequency.
*It saves the processed data as "mergemonthlyfinal.dta," which is used 
*by the program 'MonthlyRealVarComputeFinal.do' to create the final monthly dataset.

*NOTE: Be sure to change the working directory (path) as appropriate
***********************************************************************************

*Set working directory*** 
*********CHANGE TO APPROPRIATE DIRECTORY BEFORE RUNNING!*************
cd "C:\Users\nurik\Desktop\trials\PAYE-data\commands"

use rawmonthly2010.dta, clear
tsset, clear
generate t = m(1871m1)+ _n - 1
format t %tm
tsset t

gen foo = string(yyyymm)
gen monthstr = substr(foo,5,2)
destring monthstr, generate(month) force
gen yearstr = substr(foo,1,4)
destring yearstr, generate(year) force
drop monthstr yearstr yyyymm foo

****************************************************************************
*These lines generate annualized data from the quarterly Goyal-Welch rate data
*Note that other items in Goyal-Welch data (tbl, dfy, etc.)are already annualized 
***************************************************************************
gen infl_ann = (1+infl)^12 - 1  
drop infl
rename infl_ann infl
gen ltr_ann = (1+ltr)^12 - 1
drop ltr
rename ltr_ann ltr
gen corpr_ann = (1+corpr)^12 - 1
drop corpr
rename corpr_ann corpr
//generate default return spread using annualized measures
gen dfr = corpr - ltr


//Now generate returns and predictors
gen ret = ln(index+(d12/12))-ln(L.index)  //create cont. comp. returns **Note:D12 divided by 12 to get monthly return
gen lnrfree = ln(1+rfree)           //create logged risk-free rate 
gen exret = ret - lnrfree           //create excess returns
gen ret2 = (index+(d12/12))/(L.index) -1  //alt. return (no cont. compound)
gen exret2 = ret2 - rfree                //alt. excess return
gen dp = ln(d12) - ln(index)             //create div-price ratio
gen dy = ln(d12) - ln(L.index)           //create div yield
gen ep = ln(e12) - ln(index)
gen de = ln(d12) - ln(e12)
gen tms = lty - tbl				 //create term spread variable
gen dfy = baa - lty				 //create def. spread
gen lsvar = ln(svar)             //create logged variance
gen exret3 = crsp_spvw - rfree  //Final return variant
replace ppi = ppi/100              //restate ppi as decimal as opposed to percentage
replace ip = ip/100                //restate ip as decimal as opposed to percentage


****************************************************************************
*These lines generate volatility proxies for macro variables ppi and ip.
*The approach here follows Engle, Ghysels and Sohn (2008) and Schwert (1989)
***************************************************************************
*First create dummies for each month
gen m1 = 0
gen m2 = 0
gen m3 = 0
gen m4 = 0
gen m5 = 0
gen m6 = 0
gen m7 = 0
gen m8 = 0
gen m9 = 0
gen m10 = 0
gen m11 = 0
gen m12 = 0
replace m1 = 1 if month == 1
replace m2 = 1 if month == 2
replace m3 = 1 if month == 3
replace m4 = 1 if month == 4
replace m5 = 1 if month == 5
replace m6 = 1 if month == 6
replace m7 = 1 if month == 7
replace m8 = 1 if month == 8
replace m9 = 1 if month == 9
replace m10 = 1 if month == 10
replace m11 = 1 if month == 11
replace m12 = 1 if month == 12

*Regress ppi and ip on these dummies and 6 lags,i.e.,instead of 12 lags for the monthly variable, (no constant)
regress ppi m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 L1.ppi L2.ppi L3.ppi L4.ppi L5.ppi L6.ppi L7.ppi L8.ppi if tin(1891m2,2010m12), noconstant

*Get residuals and create time series of squared residuals as volatility proxy
predict double ppieps if e(sample), residual
gen ppivar = ppieps^2
gen ppivol = ppivar^(1/2)

*Repeat for ip
regress ip m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 L1.ip L2.ip L3.ip L4.ip L5.ip L6.ip L7.ip L8.ip if tin(1890m2,2010m12), noconstant
predict double ipeps if e(sample), residual
gen ipvar = ipeps^2
gen ipvol = ipvar^(1/2)    

//create standardized ads and ip growth (used in business cycle)
egen zads = std(ads)
egen zip = std(ip)

sort t

******************************************************************************************************
*Generate estimates of time-varying expected returns for filtering returns
******************************************************************************************************

*Two sample periods: 1886m1 - 2010m12; 1927m1 - 2010m12 


regress exret2 L.dp L.ep
predict double fitret if e(sample)

*Following Campbell and Thompson (2007), Paye imposes the constraint that the expected return is positive
replace fitret = 0 if fitret <0

*Now repeat for 1927-2010 sample period
*first model errors on the side of being inclusive
regress exret2 L.cp L.dfr L.dfy L.npy L.ppi L.tms if tin(1927m2,2010m12)
predict double fitret1big if e(sample)
replace fitret1big = 0 if fitret1big <0

*Next Paye estimates a conservative specification that includes only npy and tms (significant at 10% level) based on above regression w/ newey-west errors 
regress exret2 L.npy L.tms if tin(1927m2,2010m12)
predict double fitret1small if e(sample)
replace fitret1small = 0 if fitret1small <0

*clean-up
drop m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 ppieps ipeps

describe
summ

save monthlyfinalprocessed.dta, replace

sort year month

save mergemonthlyfinal.dta, replace		//used for merging with daily data to contruct RVOL measures
