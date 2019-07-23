Data used in "Deja Vol: Predictive Regressions for Aggregate Stock Market Volatility Using Macroeconomic Variables"
The data has been provided by Prof. Paye per email request.

******Overview and comments**********************************************************************************************************************
This zip file contains the underlying data used in the empirical analysis of the thesis, and STATA code used to process the data for analysis.




******
NOTES:
To reproduce the the monthly and quarterly data do the following:

1. Run DataPrepMonthlyFinal.do (this produces intermediary Stata datasets) and then run MonthlyRealVarComputeFinal.do (this adds the realized variance measures and writes monthly CSV)
2. Run DataPrepQuarterlyFinal.do (this produces intermediary Stata datasets) and then run QuartRealVarComputeFinal.do (this adds the realized variance measures and writes quarterly CSV)

3. After manipulating the data, I copy relevant columns in STATA into Excel file and opened this excel file on Eviews to conduct the analyses.




**********************************************************************************************
*Goyal and Welch variables
*See Goyal and Welch (2008, RFS) for details 
**********************************************************************************************
index = S&P index level
d12 = 12-month moving sum of dividends on S&P 
e12 = 12-month moving sum of earnings on S&P
bm = book-to-market
tbl = Treasury-bill rate
baa = yield on Moody's baa-rated corporate bonds
aaa = yield on Moody's aaa-rated corporate bonds
lty = long term Government bond yield
cay = consumption-wealth-income ratio (Lettau and Ludvigson, 2001)
ntis = net equity expansion 
rfree = risk-free rate 
infl = inflation rate
svar = stock market variance proxy (sum of daily rsquared returns; similar to "rvar" variable)
ik = investiment-capital ratio from Cochrane
crsp_vwsp = CRSP VW return on S&P 500
crsp_vwspx = same as above excluding dividends


**********************************************************************************************
*The next set of variables are additional data (relative to Goyal-Welch) that Paye obtained
**********************************************************************************************
blev = bank leverage
egdp = expected GDP growth rate following Campbell and Diebold (2009)
ip = growth rate in Industrial production
cp - commercial paper to Treasury spread
ppi = inflation measure (PPI) -- used to construct variable "ppivol"
nan_exret = excess return on CRSP VW universe ("Nyse/Amex/Nasdaq")
npy = net payout yield
hfrvol = realized volatility using high-frequency data
lhfrvol = log of high-freq real. vol.
gdp = US GDP growth rate (annualized, seasonally adjusted, real)

***********************************************************************************************
*The next set of variables are constructed using the Stata files "DataprepQuarterlyFinal.do" 
************************************************************************************************
t = Stata time variable
ltr = annualized version of "ltr" (long term govt. bond returns) from Goyal and Welch dataset
corpr = annualized version of "corpr" (corporate bond returns) from Goyal-Welch dataset
dfr = default retrun (see Goyal and Welch definition) using annualized ltr and corpr 
ret = quarterly continuously compounded return based on index and D12 fields in Goyal and Welch data
lnrfree = log risk free rate
exret = quarterly cont. comp. excess return
ret2 = alternative simple return (index+(d12/4))/(L.index) -1 
exret2 = excess simple return
crspexret = alternative excess return based on CRSP VW S&P returns
dp = log dividend price ratio
dy = dividend yield
crspdy = alternative dividend yield using CRSP VW returns
ep = earnings-price ratio
de = dividend-earnings ratio
tms = term spread
dfy = "default spread"
odfy = more traditional default spread (baa - aaa)
chblev = changes in bank leverage
chik = changes in investment-capital ratio
q1dum, q2dum, q3dum, q4dum = quarter dummies
ppieps = residuals from model for PPI (see Unpublished Appendix and Engle, Ghysels, and Sohn paper) 
ppivar = PPI variance based on squared residuals
ppivol = PPI volatility (square root of above)
ipvar = IP variance based on squared residuals
ipvol = IP volatility (square root of above)
zads = standardized version of ads varible
zgdp = standardized gdp






