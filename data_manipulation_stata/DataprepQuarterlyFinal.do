*This code processes data to construct final predictor/macroeconomic variables at the quarterly frequency.
*It saves the processed data as "mergequarterlyfinal.dta," which is used 
*by the program 'QuartRealVarComputeFinal.do' to create the final quarterly dataset.

*NOTE: Be sure to change the working directory as appropriate
***********************************************************************************

*Set working directory*** 
*********CHANGE TO APPROPRIATE DIRECTORY BEFORE RUNNING!*************
cd "C:\Users\nurik\Desktop\trials\PAYE-data\commands"

*Load quarterly data for predictor variables
use rawquarterly2010.dta, clear

generate t = q(1871q1)+ _n - 1
format t %tq
tsset t
gen foo = string(yyyyq)
gen quartstr = substr(foo,5,2)
destring quartstr, generate(quarter) force
gen yearstr = substr(foo,1,4)
destring yearstr, generate(year) force
drop quartstr yearstr yyyyq	foo

***********************************************************************************
*These lines generate annualized data from the quarterly Goyal-Welch rate data
*Other items in Goyal-Welch data (tbl, dfy, etc.) are already annualized 
***********************************************************************************
gen ltr_ann = (1+ltr)^4 - 1
drop ltr
rename ltr_ann ltr
gen corpr_ann = (1+corpr)^4 - 1
drop corpr
rename corpr_ann corpr
//generate default return spread using annualized measures
gen dfr = corpr - ltr

gen ret = ln(index+(d12/4))-ln(L.index)  //create cont. comp. returns **NOTE: D12 dividend by four to estimate quarterly dividend
gen lnrfree = ln(1+rfree) //create logged risk-free rate
gen exret = ret - lnrfree         //create excess returns
gen ret2 = (index+(d12/4))/(L.index) -1  //alt. return
gen exret2 = ret2 - rfree            //alt. excess return
gen crspexret = crsp_spvw - rfree  //alt. excess return based on CRSP returns
gen dp = ln(d12) - ln(index)    //create div-price ratio
gen dy = ln(d12) - ln(L.index)    //create div yield
gen crspdy = ln((1+crsp_spvw)/(1+crsp_spvwx)-1)  //alt. div yield based on CRSP returns
gen ep = ln(e12) - ln(index)	 //create earn-price ratio
gen de = ln(d12) - ln(e12)		 //create div-earn ratio
gen tms = lty - tbl				 //create term spread variable
gen dfy = baa - lty				 //create def. spread

//gen chlev = D.lev
gen chblev = D.blev
gen chik = D.ik
gen lsvar = ln(svar)             //create logged variance
replace ppi = ppi/100              //restate ppi as decimal as opposed to percentage
replace ip = ip/100                //restate ip as decimal as opposed to percentage
replace egdp = egdp*100            //restate in % terms for comparability with gdp


****************************************************************************
*These lines generate volatility proxies for macro variables ppi and ip.
*The approach here follows Engle, Ghysels and Sohn (2008) and Schwert (1989)
***************************************************************************
*First create dummies for each quarter
gen q1dum = 0
gen q2dum = 0
gen q3dum = 0
gen q4dum = 0
replace q1dum = 1 if quarter == 1
replace q2dum = 1 if quarter == 2
replace q3dum = 1 if quarter == 3
replace q4dum = 1 if quarter == 4

*Regress ppi and ip on these four dummies and four lags (no constant)
regress ppi q1dum q2dum q3dum q4dum L1.ppi L2.ppi L3.ppi L4.ppi if tin(1890q1,2010q4), noconstant

*Get residuals and create time series of squared residuals as volatility proxy
predict double ppieps if e(sample), residual
gen ppivar = ppieps^2
gen ppivol = ppivar^(1/2)

*Repeat for ip
regress ip q1dum q2dum q3dum q4dum L1.ip L2.ip L3.ip L4.ip if tin(1890q1,2010q4), noconstant
predict double ipeps if e(sample), residual
gen ipvar = ipeps^2
gen ipvol = ipvar^(1/2)

//standardized ads index and gdp are used in the business cycle figures
egen zads = std(ads)
egen zgdp = std(gdp)

sort t

******************************************************************************************************
*Generate estimates of time-varying expected returns for filtering returns
******************************************************************************************************

*Three sample periods: 1886Q1 - 2010Q4; 1927Q1 - 2010Q4; 1952Q1 - 2010Q4

regress exret2 L.dp L.ep
predict double fitret if e(sample)

*Following Campbell and Thompson (2007), Paye imposes the constraint that the expected return is positive
replace fitret = 0 if fitret <0

*Repeat for 1927-2010 sample period
*first model is errors on the side of being inclusive
regress exret2 L.cp L.dfr L.dfy L.npy L.ppi L.tms if tin(1927q2,2010q4)
predict double fitret1big if e(sample)
replace fitret1big = 0 if fitret1big <0

*Next Paye estimates a conservative specification using only variables significant at 10% level in previous regress
*using NW errors w/ 5 lags.
regress exret2 L.npy if tin(1927q2,2010q4)
predict double fitret1small if e(sample)
replace fitret1small = 0 if fitret1small <0


*Repeat for 1952-2010 sample period
regress exret2 L.cay L.cp L.dfr L.dfy L.egdp L.ik L.npy L.ppi L.tms if tin(1952q2,2010q4)
predict double fitret2big if e(sample)
replace fitret2big = 0 if fitret2big < 0

*As before, Paye estimates a more conservative model.
regress exret2 L.cay L.dfr L.egdp if tin(1952q2,2010q4)
predict double fitret2small if e(sample)
replace fitret2small = 0 if fitret2small < 0

describe 
summ


format t %tq
tsset t


save quarterlyfinalprocessed.dta, replace

sort year quarter
save mergequarterlyfinal.dta, replace		
