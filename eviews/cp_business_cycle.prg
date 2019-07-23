

                          
                                ''''''''''''''''''''''''''DELTA SE ANALYSIS'''''''''''''''''''''''''''

'SOME REMARKS:

'1) Before starting to analyse the series I have to copy the quarterly series for the forecasting  variable. Time period covers the years of 1947 through 2010.



smpl @all
smpl 1947:03 2010:04

series fe_UV=(zlrvol-yhat1)
series sq_fe_UV=(fe_UV)^2

series fe_ith=(zlrvol-yhat2)
series sq_fe_ith=(fe_ith)^2

series delta_se=sq_fe_UV-sq_fe_ith 
series cum_delta_se=@cumsum(delta_se)
line cum_delta_se

cum_se.draw(line, bottom, pattern(none) ) 1948:04 1953:02 1957:03 1960:02 1969:04 1970:04  1973:04 1975:01 1980:01 1981:03 1982:04 1990:03 2001:01 2007:04 2008:03 2009:02

cum_se.draw(line, bottom, pattern(none) ) 1948:04
cum_se.draw(line, bottom, pattern(none) ) 1953:02
cum_se.draw(line, bottom, pattern(none) ) 1957:03
cum_se.draw(line, bottom, pattern(none) )1960:02
cum_se.draw(line, bottom, pattern(none) )1969:04
cum_se.draw(line, bottom, pattern(none) )1973:04
cum_se.draw(line, bottom, pattern(none) )1980:01
cum_se.draw(line, bottom, pattern(none) )1981:03
cum_se.draw(line, bottom, pattern(none) )1990:03
cum_se.draw(line, bottom, pattern(none) )2001:01
cum_se.draw(line, bottom, pattern(none) )2007:04


 ''''''''''''''''''''''''''DELTA SE ANALYSIS REGRESSION'''''''''''''''''''''''''''



series z_delta_se=(delta_se-@mean(delta_se))/@stdev(delta_se)
'Standardize the variable before inserting it into the regression.

smpl 1947:03 2010:04
equation eq1
'estimate the regression
eq1.ls(cov=hac) z_delta_se z_gdp

scalar beta1=eq1.@coef(1)
scalar r_sqr1=(eq1.@r2)*100
scalar tstat1=eq1.@tstat(1)
show beta1
show r_sqr1
show tstat1




smpl 1972:03 2010:04

series fe_UV2=(zlrvol-yhat3)
series sq_fe_UV2=(fe_UV2)^2

series fe_cp2=(zlrvol-yhat4)
series sq_fe_cp2=(fe_cp2)^2

series delta_se2=sq_fe_UV2-sq_fe_cp2 
series cum_delta_se2=@cumsum(delta_se2)
line cum_delta_se2

series z_delta_se2=(delta_se2-@mean(delta_se2))/@stdev(delta_se2)
'Standardize the variable before inserting it into the regression.

smpl 1972:03 2010:04
equation eq2
'estimate the regression
eq2.ls(cov=hac) z_delta_se2 z_gdp

scalar beta2=eq2.@coef(1)
scalar r_sqr2=(eq2.@r2)*100
scalar tstats2=eq2.@tstat(1)
show beta2
show r_sqr2
show tstats2





                ''''NEW DELTA_SE WITH NEW STDZED VARIABLE CP''''''



                                ''''''''''''''''''''''''''DELTA SE ANALYSIS'''''''''''''''''''''''''''

'SOME REMARKS:

'1) Before starting to analyse the series I have to copy the quarterly series for the forecasting  variable. Time period covers the years of 1947 through 2010.

smpl @all
series z_cp=cp/@stdev(cp)
series z_gdp=gdp/@stdev(gdp)

smpl @all
smpl 1947:03 2010:04

series fe_UV=(zlrvol-yhat1)
series sq_fe_UV=(fe_UV)^2

series fe_ith=(zlrvol-yhat2)
series sq_fe_ith=(fe_ith)^2

series delta_se=sq_fe_UV-sq_fe_ith 
series cum_delta_se=@cumsum(delta_se)
line cum_delta_se

cum_se.draw(line, bottom, pattern(none) ) 1948:04 1953:02 1957:03 1960:02 1969:04 1970:04  1973:04 1975:01 1980:01 1981:03 1982:04 1990:03 2001:01 2007:04 2008:03 2009:02

cum_se.draw(line, bottom, pattern(none) ) 1948:04
cum_se.draw(line, bottom, pattern(none) ) 1953:02
cum_se.draw(line, bottom, pattern(none) ) 1957:03
cum_se.draw(line, bottom, pattern(none) )1960:02
cum_se.draw(line, bottom, pattern(none) )1969:04
cum_se.draw(line, bottom, pattern(none) )1973:04
cum_se.draw(line, bottom, pattern(none) )1980:01
cum_se.draw(line, bottom, pattern(none) )1981:03
cum_se.draw(line, bottom, pattern(none) )1990:03
cum_se.draw(line, bottom, pattern(none) )2001:01
cum_se.draw(line, bottom, pattern(none) )2007:04


 ''''''''''''''''''''''''''DELTA SE ANALYSIS REGRESSION'''''''''''''''''''''''''''



series z_delta_se=delta_se/@stdev(delta_se)
'Standardize the variable before inserting it into the regression.

smpl 1947:03 2010:04
equation eq1
'estimate the regression
eq1.ls(cov=hac) z_delta_se z_gdp

scalar beta1=eq1.@coef(1)
scalar r_sqr1=(eq1.@r2)*100
scalar tstat1=eq1.@tstat(1)
show beta1
show r_sqr1
show tstat1




smpl 1972:03 2010:04

series fe_UV2=(zlrvol-yhat3)
series sq_fe_UV2=(fe_UV2)^2

series fe_cp2=(zlrvol-yhat4)
series sq_fe_cp2=(fe_cp2)^2

series delta_se2=sq_fe_UV2-sq_fe_cp2 
series cum_delta_se2=@cumsum(delta_se2)
line cum_delta_se2

series z_delta_se2=delta_se2/@stdev(delta_se2)
'Standardize the variable before inserting it into the regression.

smpl 1972:03 2010:04
equation eq2
'estimate the regression
eq2.ls(cov=hac) z_delta_se2 z_gdp

scalar beta2=eq2.@coef(1)
scalar r_sqr2=(eq2.@r2)*100
scalar tstats2=eq2.@tstat(1)
show beta2
show r_sqr2
show tstats2


