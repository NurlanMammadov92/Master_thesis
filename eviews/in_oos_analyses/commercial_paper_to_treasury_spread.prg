

                               ''''''DESCRIPTIVE ANALYSIS of CP'''''


'Here I display the mean, std.deviation, kurtosis and skewness of the forecasting variable cp for the time period being considered.         
smpl 1952:02 2010:12
scalar mean_cp=@mean(cp)
scalar stdev_cp=@stdev(cp)
scalar skew_cp=@skew(cp)
scalar kurt_cp=@kurt(cp)

show mean_cp
show stdev_cp
show kurt_cp
show skew_cp



'Here I can calculate the first two autocorrelation coeff's of the variable 'cp'.
scalar cov1_cp=@cov(cp,cp(-1))
scalar var_cp=@var(cp)

'this command corresponds to rho(1).
scalar rho1_cp=cov1_cp/var_cp
show rho1_cp


'this command corresponds to rho(2).
scalar cov2_cp=@cov(cp,cp(-2))
scalar rho2_cp=cov2_cp/var_cp
show rho2_cp


''Extract the uroot test stats and the probability.
uroot(pp,const, lag=1, save=pp_stats) cp
show pp_stats
'Comment: R3 and R4 in the matrix pp_stats correspond to Zt test statistics and probability of Zt, respectively.



                         ''''''''''''''''''''''''''MONTHLY SERIES FOR NEW DATA'''''''''''''''''''''''''



'2) After transforming the series into their standardized versions I can estimate the in-sample regressions. Note: log volatility series has been converted to its standadized version before-''zlrvol''.

'Declare the equation for the estimation of the regression for the respective time period.
equation eq1

'Reset the sample to respective time period.
smpl 1927:02 2010:12

eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr1_ar6=eq1.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq2

'Reset the sample to respective time period.
smpl 1927:02 2010:12

eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6) z_cp(-1)
'Extract the beta coefficient for the respective forecating variable 'cp'.
scalar beta1=@coefs(7)
show beta1

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr1_aug=eq2.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg1_rsqr=((rsqr1_aug-rsqr1_ar6)/rsqr1_ar6)*100
show chg1_rsqr



                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1952:02 2010:12

'Declare the equation for the estimation of the regression for the respective time period.
equation eq3

'Reset the sample to respective time period.
smpl 1952:02 2010:12

eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr2_ar6=eq3.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq4

'Reset the sample to respective time period.
smpl 1952:02 2010:12

eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6) z_cp(-1)
'Extract the beta coefficient for the respective forecating variable 'cp'.
scalar beta2=@coefs(7)
show beta2

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr2_aug=eq4.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg2_rsqr=((rsqr2_aug-rsqr2_ar6)/rsqr2_ar6)*100
show chg2_rsqr




                      '''''''TIME PERIOD OF 1927-1951'''''''''''

smpl @all
smpl 1927:02 1951:12

'Declare the equation for the estimation of the regression for the respective time period.
equation eq5

'Reset the sample to respective time period.
smpl 1927:02 1951:12

eq5.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr3_ar6=eq5.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq6

'Reset the sample to respective time period.
smpl 1927:02 1951:12

eq6.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6) z_cp(-1)
'Extract the beta coefficient for the respective forecating variable 'cp'.
scalar beta3=@coefs(7)
show beta3

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr3_aug=eq6.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg3_rsqr=((rsqr3_aug-rsqr3_ar6)/rsqr3_ar6)*100
show chg3_rsqr


                      '''''''TIME PERIOD OF 1952-1985'''''''''''

smpl @all
smpl 1952:01 1985:12

'Declare the equation for the estimation of the regression for the respective time period.
equation eq7

'Reset the sample to respective time period.
smpl 1952:01 1985:12

eq7.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr4_ar6=eq7.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq8

'Reset the sample to respective time period.
smpl 1952:01 1985:12

eq8.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6) z_cp(-1)
'Extract the beta coefficient for the respective forecating variable 'cp'.
scalar beta4=@coefs(7)
show beta4

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr4_aug=eq8.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg4_rsqr=((rsqr4_aug-rsqr4_ar6)/rsqr4_ar6)*100
show chg4_rsqr




                      '''''''TIME PERIOD OF 1986-2010'''''''''''

smpl @all
smpl 1986:01 2010:12

'Declare the equation for the estimation of the regression for the respective time period.
equation eq9

'Reset the sample to respective time period.
smpl 1986:01 2010:12

eq9.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr5_ar6=eq9.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq10

'Reset the sample to respective time period.
smpl 1986:01 2010:12

eq10.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6) z_cp(-1)
'Extract the beta coefficient for the respective forecating variable 'cp'.
scalar beta5=@coefs(7)
show beta5

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr5_aug=eq10.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg5_rsqr=((rsqr5_aug-rsqr5_ar6)/rsqr5_ar6)*100
show chg5_rsqr






                          ''''''''''OUT-OF-SAMPLE ANALYSIS FOR NEW DATA'''''''''''''


smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
series yhat1_se            ' forecast std.err.

'here we can set the window size.
' set step size

' move sample !step obs at a time
for !i = 1  to 766
   ' set sample to estimation period
   smpl 1927m2+!i 1947m1+!i
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6)
   ' reset sample to forecast period
   smpl 1947m2+!i 1947m2+!i
   ' make forecasts in temporary series first
   eq1.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat1 = tmp_yhat
   yhat1_se = tmp_se
next


smpl @all
  
' declare equation for estimation
equation eq2
 
' declare series for final results
series yhat2               ' point estimates
series yhat2_se            ' forecast std.err.

' set step size

' move sample !step obs at a time
for !i = 1  to 766
   ' set sample to estimation period
   smpl 1927m2+!i 1947m1+!i
   ' estimate equation
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6) z_cp(-1)
   ' reset sample to forecast period
   smpl 1947m2+!i 1947m2+!i
   ' make forecasts in temporary series first
   eq2.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat2 = tmp_yhat
   yhat2_se = tmp_se
next


smpl 1947:03 2010:12
series dif1_sqr=(zlrvol-yhat1)^2
scalar sigma1=@mean(dif1_sqr)
series dif2_sqr=(zlrvol-yhat2)^2
scalar sigma2=@mean(dif2_sqr)

series mspe1=(yhat1-yhat2)^2
scalar adj_term1=@mean(mspe1)
scalar CW_stat1=((sigma1-sigma2)+adj_term1)*1000
show CW_stat1


                                              ''''1972-2010'''''


smpl @all
' declare equation for estimation
equation eq3
 
' declare series for final results
series yhat3               ' point estimates
series yhat3_se            ' forecast std.err.
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 466
   ' set sample to estimation period
   smpl 1952m2+!i 1972m1+!i
   ' estimate equation
    eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6)
   ' reset sample to forecast period
   smpl 1972m2+!i 1972m2+!i
   ' make forecasts in temporary series first
   eq3.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat3 = tmp_yhat
   yhat3_se = tmp_se
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'cp'.

smpl 1972:03 2010:12
 
' get size of workfile
!length = @obsrange
 
smpl @all
' declare equation for estimation
equation eq4
 
' declare series for final results
series yhat4               ' point estimates
series yhat4_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 466
   ' set sample to estimation period
   smpl 1952m2+!i 1972m1+!i
   ' estimate equation
  eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6) z_cp(-1)
   ' reset sample to forecast period
   smpl 1972m2+!i 1972m2+!i
   ' make forecasts in temporary series first
   eq4.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat4 = tmp_yhat
   yhat4_se = tmp_se
next

smpl 1972:03 2010:12
series dif3_sqr=(zlrvol-yhat3)^2
scalar sigma3=@mean(dif3_sqr)
series dif4_sqr=(zlrvol-yhat4)^2
scalar sigma4=@mean(dif4_sqr)

series mspe2=(yhat3-yhat4)^2
scalar adj_term2=@mean(mspe2)
scalar CW_stat2=((sigma3-sigma4)+adj_term2)*1000
show CW_stat2
' COMMENT ON THE RESULTS:.................

 

smpl @all
' declare equation for estimation
equation eq5
 
' declare series for final results
series yhat5               ' point estimates
series yhat5_se            ' forecast std.err.
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 346
   ' set sample to estimation period
   smpl 1962m2+!i 1982m1+!i
   ' estimate equation
   eq5.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6)
   ' reset sample to forecast period
   smpl 1982m2+!i 1982m2+!i
   ' make forecasts in temporary series first
   eq5.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat5 = tmp_yhat
   yhat5_se = tmp_se
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'cp'.

smpl @all

' declare equation for estimation
equation eq6
 
' declare series for final results
series yhat6               ' point estimates
series yhat6_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 346
   ' set sample to estimation period
   smpl 1962m2+!i 1982m1+!i
   ' estimate equation
   eq6.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6) z_cp(-1)
   ' reset sample to forecast period
   smpl 1982m2+!i 1982m2+!i
   ' make forecasts in temporary series first
   eq6.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat6 = tmp_yhat
   yhat6_se = tmp_se
next



smpl 1982:03 2010:12
series dif5_sqr=(zlrvol-yhat5)^2
scalar sigma5=@mean(dif5_sqr)
series dif6_sqr=(zlrvol-yhat6)^2
scalar sigma6=@mean(dif6_sqr)

series mspe3=(yhat5-yhat6)^2
scalar adj_term3=@mean(mspe3)
scalar CW_stat3=((sigma5-sigma6)+adj_term3)*1000
show CW_stat3



                                                  ''''''1972-2000'''''''


smpl @all
' declare equation for estimation
equation eq7
 
' declare series for final results
series yhat7            ' point estimates
series yhat7_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 346
   ' set sample to estimation period
   smpl 1952m2+!i 1972m1+!i
   ' estimate equation
   eq7.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6)
   ' reset sample to forecast period
   smpl 1972m2+!i 1972m2+!i
   ' make forecasts in temporary series first
   eq7.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat7 = tmp_yhat
   yhat7_se = tmp_se
next

smpl @all
' declare equation for estimation
equation eq8
 
' declare series for final results
series yhat8               ' point estimates
series yhat8_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 346
   ' set sample to estimation period
   smpl 1952m2+!i 1972m1+!i
   ' estimate equation
   eq8.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6) z_cp(-1)
   ' reset sample to forecast period
   smpl 1972m2+!i 1972m2+!i
   ' make forecasts in temporary series first
   eq8.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat8 = tmp_yhat
   yhat8_se = tmp_se
next

smpl 1972:03 2000:12
series dif7_sqr=(zlrvol-yhat7)^2
scalar sigma7=@mean(dif7_sqr)
series dif8_sqr=(zlrvol-yhat8)^2
scalar sigma8=@mean(dif8_sqr)

series mspe4=(yhat7-yhat8)^2
scalar adj_term4=@mean(mspe4)
scalar CW_stat4=((sigma7-sigma8)+adj_term4)*1000
show CW_stat4

'COMMENT ON THE RESULTS:....

                     ''''''NEW CW STATS BASED ON REGRESSIONS''''   


smpl 1947:03 2010:12
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-yhat2)^2

series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:12

series sigma3_sqr=(zlrvol-yhat3)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat3-yhat4)^2


series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:12
series sigma5_sqr=(zlrvol-yhat5)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-yhat6)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat5-yhat6)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:12
series sigma7_sqr=(zlrvol-yhat7)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-yhat8)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat7-yhat8)^2

series CW_stat44=1000*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:                                                

            

                  '''''''CP analysis based on the change in r-square'''''
          
smpl @all
' move sample !step obs at a time
for !i = 1  to 766
   ' set sample to estimation period
   smpl 1927m3 1947m2+!i
    series hm_mean1(239+!i)=@mean(zlrvol)
next

'On the second step we can calculate the MSEP for the Historical average.
smpl 1947:03 2010:12

series hm_mean_dif1=zlrvol-hm_mean1
series hm_msep1_sqr=hm_mean_dif1^2
scalar hm_msep1=@mean(hm_msep1_sqr)

scalar r_oos1=100*((sigma1-sigma2)/hm_msep1)
show r_oos1
'COMMENT:.......


smpl @all
' move sample !step obs at a time
for !i = 1  to 466
   ' set sample to estimation period
   smpl 1952m3 1972m2+!i
    series hm_mean2(239+!i)=@mean(zlrvol)
next

'On the second step we can calculate the MSEP for the Historical average.
smpl 1972:03 2010:12

series hm_mean_dif2=zlrvol-hm_mean2
series hm_msep2_sqr=hm_mean_dif2^2
scalar hm_msep2=@mean(hm_msep2_sqr)

scalar r_oos2=100*((sigma3-sigma4)/hm_msep2)
show r_oos2
'COMMENT:.......


'Historical average for the 1947:03-2010:12 time period.

smpl @all
' move sample !step obs at a time
for !i = 1  to 346
   ' set sample to estimation period
   smpl 1962m3 1982m2+!i
    series hm_mean3(239+!i)=@mean(zlrvol)
next

smpl 1982:03 2010:12
'On the second step we can calculate the MSEP for the Historical average.
series hm_mean_dif3=zlrvol-hm_mean3
series hm_msep3_sqr=hm_mean_dif3^2
scalar hm_msep3=@mean(hm_msep3_sqr)

scalar r_oos3=100*((sigma5-sigma6)/hm_msep3)
show r_oos3

'COMMENT: PROBLEMATIC!!!!!


smpl @all
' move sample !step obs at a time
for !i = 1  to 346
   ' set sample to estimation period
   smpl 1952m3 1972m2+!i
   series hm_mean4(239+!i)=@mean(zlrvol)
    smpl 2001m1 @last
     series hm_mean4=NA
next

'On the second step we can calculate the MSEP for the Historical average.
smpl 1972:03 2000:12

series hm_mean_dif4=zlrvol-hm_mean4
series hm_msep4_sqr=hm_mean_dif4^2
scalar hm_msep4=@mean(hm_msep4_sqr)

scalar  r_oos4=100*((sigma7-sigma8)/hm_msep4)
show r_oos4

'COMMENT:.....



             '''''''''''QUARTERLY SERIES TRIALS FOR INSAMPLE ANALYSIS''''''''''''''

'Standardize the blev variable.

smpl @all
series z_cp=(cp-@mean(cp))/@stdev(cp)



'Here I display the mean, std.deviation, kurtosis and skewness of the forecasting variable cp for the time period being considered.         
smpl 1952:02 2010:04
scalar mean_cp=@mean(cp)
scalar stdev_cp=@stdev(cp)
scalar skew_cp=@skew(cp)
scalar kurt_cp=@kurt(cp)

show mean_cp
show stdev_cp
show kurt_cp
show skew_cp



'Here I can calculate the first two autocorrelation coeff's of the variable 'cp'.
scalar cov1_cp=@cov(cp,cp(-1))
scalar var_cp=@var(cp)

'this command corresponds to rho(1).
scalar rho1_cp=cov1_cp/var_cp
show rho1_cp


'this command corresponds to rho(2).
scalar cov2_cp=@cov(cp,cp(-2))
scalar rho2_cp=cov2_cp/var_cp
show rho2_cp


''Extract the uroot test stats and the probability.
uroot(pp,const, lag=1, save=pp_stats) cp
show pp_stats

'Comment: R3 and R4 in the matrix pp_stats correspond to Zt test statistics and probability of Zt, respectively.


                         '''''''''''IN-SAMPLE ANALYSIS''''''''''

'Declare the equation for the estimation of the regression for the respective time period.

smpl @all
equation eq1

'Reset the sample to respective time period.
smpl 1927:02 2010:04

eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr1_ar2=eq1.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq2

'Reset the sample to respective time period.
smpl 1927:02 2010:04

eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)  z_cp(-1)
'Extract the beta coefficient for the respective forecating variable 'cp'.
scalar beta1=@coefs(3)
show beta1

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr1_aug=eq2.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg1_rsqr=((rsqr1_aug-rsqr1_ar2)/rsqr1_ar2)*100
show chg1_rsqr



                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1952:02 2010:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq3

'Reset the sample to respective time period.
smpl 1952:02 2010:04

eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr2_ar2=eq3.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq4

'Reset the sample to respective time period.
smpl 1952:02 2010:04

eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
'Extract the beta coefficient for the respective forecating variable 'cp'.
scalar beta2=@coefs(3)
show beta2

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr2_aug=eq4.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg2_rsqr=((rsqr2_aug-rsqr2_ar2)/rsqr2_ar2)*100
show chg2_rsqr


                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1927:02 1951:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq5

'Reset the sample to respective time period.
smpl 1927:02 1951:04

eq5.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr3_ar2=eq5.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq6

'Reset the sample to respective time period.
smpl 1927:02 1951:04

eq6.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
'Extract the beta coefficient for the respective forecating variable 'cp'.
scalar beta3=@coefs(3)
show beta3

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr3_aug=eq6.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg3_rsqr=((rsqr3_aug-rsqr3_ar2)/rsqr3_ar2)*100
show chg3_rsqr


                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1952:02 1985:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq7

'Reset the sample to respective time period.
smpl 1952:02 1985:04

eq7.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr4_ar2=eq7.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq8

'Reset the sample to respective time period.
smpl 1952:02 1985:04

eq8.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
'Extract the beta coefficient for the respective forecating variable 'cp'.
scalar beta4=@coefs(3)
show beta4

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr4_aug=eq8.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg4_rsqr=((rsqr4_aug-rsqr4_ar2)/rsqr4_ar2)*100
show chg4_rsqr


                     '''''''TIME PERIOD OF 1927-1951'''''''''''

smpl @all
smpl 1986:01 2010:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq9

'Reset the sample to respective time period.
smpl 1986:01 2010:04

eq9.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr5_ar2=eq9.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq10

'Reset the sample to respective time period.
smpl 1986:01 2010:04

eq10.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
'Extract the beta coefficient for the respective forecating variable 'cp'.
scalar beta5=@coefs(3)
show beta5

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr5_aug=eq10.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg5_rsqr=((rsqr5_aug-rsqr5_ar2)/rsqr5_ar2)*100
show chg5_rsqr




                 '''''''''QUARTERLY  OOS ANALYSIS USING NEW DATA''''''''''

'Standardize the blev variable.

smpl @all
series z_cp=(cp-@mean(cp))/@stdev(cp)

smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
series yhat1_se            ' forecast std.err.
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q2+!i 1947q1+!i    ''This calculation refers to rolling window estimation scheme.
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   eq1.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat1 = tmp_yhat
   yhat1_se = tmp_se
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'egdp'.

 
smpl @all
' declare equation for estimation
equation eq2
 
' declare series for final results
series yhat2               ' point estimates
series yhat2_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q2+!i 1947q1+!i
   ' estimate equation
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   eq2.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat2 = tmp_yhat
   yhat2_se = tmp_se
next

' SMALL NOTE: since we obtained the fitted values for the series stdlvol for both equations, I tried to create the group series in order to see the difference between the actual values of lvol series and its behavior against the fitted values that are denoted as yhat1 and yhat2. By checking the recessions and booms of the economy we could conclude that yhat2 series exhibit more volatile series than its counterpart (yhat1). This assert the observation that Paye has mentioned on his paper. The group is labelled as 'group_lvol_yhat1_yhat2'.

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-yhat2)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-yhat2)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
show cw_stat1
'COMMENT:.......

smpl 1947:03 2010:04
series sigma11_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma22_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-yhat2)^2

series CW_stat11=1000*((sigma11_sqr-sigma22_sqr)+adj_term_sqr1)
show cw_stat11

equation cwstat

cwstat.ls(cov=hac) cw_stat11 c
show cwstat
'COMMENT:.......







smpl @all
' declare equation for estimation
equation eq3
 
' declare series for final results
series yhat3               ' point estimates
series yhat3_se            ' forecast std.err.
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
   eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq3.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat3 = tmp_yhat
   yhat3_se = tmp_se
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'egdp'.


 
smpl @all
' declare equation for estimation
equation eq4
 
' declare series for final results
series yhat4               ' point estimates
series yhat4_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq4.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat4 = tmp_yhat
   yhat4_se = tmp_se
next

smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat3)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-yhat4)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat3-yhat4)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
show cw_stat2
'COMMENT:.......


smpl 1972:03 2010:04

series sigma33_sqr=(zlrvol-yhat3)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma44_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat3-yhat4)^2


series CW_stat22=1000*((sigma33_sqr-sigma44_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......



smpl @all
' declare equation for estimation
equation eq5
 
' declare series for final results
series yhat5            ' point estimates
series yhat5_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q2+!i 1982q1+!i
   ' estimate equation
   eq5.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   eq5.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat5 = tmp_yhat
   yhat5_se = tmp_se
next

smpl @all
' declare equation for estimation
equation eq6
 
' declare series for final results
series yhat6               ' point estimates
series yhat6_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q2+!i 1982q1+!i
   ' estimate equation
   eq6.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   eq6.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat6 = tmp_yhat
   yhat6_se = tmp_se
next

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat5)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-yhat6)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat5-yhat6)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=100*((sigma5-sigma6)+adj_term3)
show cw_stat3


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat5)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-yhat6)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat5-yhat6)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3
'Comment: 



smpl @all
' declare equation for estimation
equation eq7
 
' declare series for final results
series yhat7            ' point estimates
series yhat7_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
   eq7.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq7.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat7 = tmp_yhat
   yhat7_se = tmp_se
next

smpl @all
' declare equation for estimation
equation eq8
 
' declare series for final results
series yhat8               ' point estimates
series yhat8_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
   eq8.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq8.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat8 = tmp_yhat
   yhat8_se = tmp_se
next

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat7)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-yhat8)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat7-yhat8)^2
 scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4

'''NEW CW STATS''''

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat7)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-yhat8)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat7-yhat8)^2

series CW_stat44=1000*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment: 


                   ''''''''''''OOS analysis based on change in rsquare'''''''''''''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat3)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat5)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-yhat6)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat7)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-yhat8)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment:  



                 ''''RECURSIVE ESTIMATION METHOD-NEW DATA''''''

                 '''''''''QUARTERLY  OOS ANALYSIS USING NEW DATA''''''''''

'Standardize the blev variable.


smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
series yhat1_se            ' forecast std.err.
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q3 1947q1+!i    ''This calculation refers to rolling window estimation scheme.
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   eq1.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat1 = tmp_yhat
   yhat1_se = tmp_se
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'egdp'.

 
smpl @all
' declare equation for estimation
equation eq2
 
' declare series for final results
series yhat2               ' point estimates
series yhat2_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q3 1947q1+!i
   ' estimate equation
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   eq2.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat2 = tmp_yhat
   yhat2_se = tmp_se
next

' SMALL NOTE: since we obtained the fitted values for the series stdlvol for both equations, I tried to create the group series in order to see the difference between the actual values of lvol series and its behavior against the fitted values that are denoted as yhat1 and yhat2. By checking the recessions and booms of the economy we could conclude that yhat2 series exhibit more volatile series than its counterpart (yhat1). This assert the observation that Paye has mentioned on his paper. The group is labelled as 'group_lvol_yhat1_yhat2'.

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-yhat2)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-yhat2)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
show cw_stat1
'COMMENT:.......








smpl @all
' declare equation for estimation
equation eq3
 
' declare series for final results
series yhat3               ' point estimates
series yhat3_se            ' forecast std.err.
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
   eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq3.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat3 = tmp_yhat
   yhat3_se = tmp_se
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'egdp'.


 
smpl @all
' declare equation for estimation
equation eq4
 
' declare series for final results
series yhat4               ' point estimates
series yhat4_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq4.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat4 = tmp_yhat
   yhat4_se = tmp_se
next

smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat3)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-yhat4)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat3-yhat4)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
show cw_stat2
'COMMENT:.......




smpl @all
' declare equation for estimation
equation eq5
 
' declare series for final results
series yhat5            ' point estimates
series yhat5_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q3 1982q1+!i
   ' estimate equation
   eq5.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   eq5.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat5 = tmp_yhat
   yhat5_se = tmp_se
next

smpl @all
' declare equation for estimation
equation eq6
 
' declare series for final results
series yhat6               ' point estimates
series yhat6_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q3 1982q1+!i
   ' estimate equation
   eq6.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   eq6.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat6 = tmp_yhat
   yhat6_se = tmp_se
next

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat5)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-yhat6)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat5-yhat6)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=100*((sigma5-sigma6)+adj_term3)
show cw_stat3
'Comment: PROBLEMATIC!!!!!!!!!!!!!!???????????????



smpl @all
' declare equation for estimation
equation eq7
 
' declare series for final results
series yhat7            ' point estimates
series yhat7_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
   eq7.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq7.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat7 = tmp_yhat
   yhat7_se = tmp_se
next

smpl @all
' declare equation for estimation
equation eq8
 
' declare series for final results
series yhat8               ' point estimates
series yhat8_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
   eq8.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq8.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat8 = tmp_yhat
   yhat8_se = tmp_se
next

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat7)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-yhat8)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat7-yhat8)^2
 scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4
'Comment: 

           ''''''NEW CW STATS BASED ON REGRESSIONS''''   


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-yhat2)^2

series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat3)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat3-yhat4)^2


series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat5)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-yhat6)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat5-yhat6)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat7)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-yhat8)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat7-yhat8)^2

series CW_stat44=1000*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:           
 

                   ''''''''''''OOS analysis based on change in rsquare'''''''''''''''

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat3)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat5)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-yhat6)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat7)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-yhat8)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment:  



                      '''''''''''''TRIALS FOR MIDAS REGRESSIONS''''''''





             '''''''TRIALS FOR MIDAS REGRESSIONS'''''

'1) Here I implemented in-sample analysis for the midas regression compared to ar-type models.

                         '''''''''''IN-SAMPLE ANALYSIS''''''''''

'Declare the equation for the estimation of the regression for the respective time period.

smpl @all
equation eq1

'Reset the sample to respective time period.
smpl 1927:02 2010:04

eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr1_ar2=eq1.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl1

'Reset the sample to respective time period.
smpl 1927:02 2010:04

midaspdl1.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)
'Freeze the table to check the lag coefficients of MIDAS regressions.
freeze(midaspdl1_reg) midaspdl1

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr1_midas=midaspdl1.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg1_rsqr=((rsqr1_midas-rsqr1_ar2)/rsqr1_ar2)*100
show chg1_rsqr



                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1952:02 2010:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq2

'Reset the sample to respective time period.
smpl 1952:02 2010:04

eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr2_ar2=eq2.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl2

'Reset the sample to respective time period.
smpl 1952:02 2010:04

midaspdl2.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)
'Freeze the model to check the lag coefficients of the midas model for the time period 1952-2010.
freeze(midaspdl2_reg) midaspdl2

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr2_midas=midaspdl2.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg2_rsqr=((rsqr2_midas-rsqr2_ar2)/rsqr2_ar2)*100
show chg2_rsqr


                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1927:02 1951:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq3

'Reset the sample to respective time period.
smpl 1927:02 1951:04

eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr3_ar2=eq3.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl3

'Reset the sample to respective time period.
smpl 1927:02 1951:04

midaspdl3.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)

'Freeze the model to check the lag coefficients of the midas model for the time period 1927-1951.
freeze(midaspdl3_reg) midaspdl3

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr3_midas=midaspdl3.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg3_rsqr=((rsqr3_midas-rsqr3_ar2)/rsqr3_ar2)*100
show chg3_rsqr


                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1952:02 1985:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq4

'Reset the sample to respective time period.
smpl 1952:02 1985:04

eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr4_ar2=eq4.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl4

'Reset the sample to respective time period.
smpl 1952:02 1985:04

midaspdl4.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)

'Freeze the model to check the lag coefficients of the midas model for the time period 1927-1951.
freeze(midaspdl4_reg) midaspdl4

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr4_midas=midaspdl4.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg4_rsqr=((rsqr4_midas-rsqr4_ar2)/rsqr4_ar2)*100
show chg4_rsqr


                     '''''''TIME PERIOD OF 1927-1951'''''''''''

smpl @all
smpl 1986:01 2010:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq5

'Reset the sample to respective time period.
smpl 1986:01 2010:04

eq5.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr5_ar2=eq5.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl5

'Reset the sample to respective time period.
smpl 1986:01 2010:04

midaspdl5.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)

'Freeze the model to check the lag coefficients of the midas model for the time period 1927-1951.
freeze(midaspdl5_reg) midaspdl5

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr5_midas=midaspdl5.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg5_rsqr=((rsqr5_midas-rsqr5_ar2)/rsqr5_ar2)*100
show chg5_rsqr





'2) Here I run in-sample analysis for the MIDAS-AUGMENTED models. 

                         '''''''''''IN-SAMPLE ANALYSIS''''''''''

'Declare the equation for the estimation of the regression for the respective time period.

smpl @all
equation eq1

'Reset the sample to respective time period.
smpl 1927:02 2010:04

eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr1_ar2=eq1.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl1

'Reset the sample to respective time period.
smpl 1927:02 2010:04

midaspdl1.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)
'Freeze the table to check the lag coefficients of MIDAS regressions.
freeze(midaspdl1_reg) midaspdl1

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr1_midas=midaspdl1.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg1_rsqr=((rsqr1_midas-rsqr1_ar2)/rsqr1_ar2)*100
show chg1_rsqr



                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1952:02 2010:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq2

'Reset the sample to respective time period.
smpl 1952:02 2010:04

eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr2_ar2=eq2.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl2

'Reset the sample to respective time period.
smpl 1952:02 2010:04

midaspdl2.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)
'Freeze the model to check the lag coefficients of the midas model for the time period 1952-2010.
freeze(midaspdl2_reg) midaspdl2

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr2_midas=midaspdl2.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg2_rsqr=((rsqr2_midas-rsqr2_ar2)/rsqr2_ar2)*100
show chg2_rsqr


                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1927:02 1951:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq3

'Reset the sample to respective time period.
smpl 1927:02 1951:04

eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr3_ar2=eq3.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl3

'Reset the sample to respective time period.
smpl 1927:02 1951:04

midaspdl3.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)

'Freeze the model to check the lag coefficients of the midas model for the time period 1927-1951.
freeze(midaspdl3_reg) midaspdl3

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr3_midas=midaspdl3.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg3_rsqr=((rsqr3_midas-rsqr3_ar2)/rsqr3_ar2)*100
show chg3_rsqr


                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1952:02 1985:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq4

'Reset the sample to respective time period.
smpl 1952:02 1985:04

eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr4_ar2=eq4.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl4

'Reset the sample to respective time period.
smpl 1952:02 1985:04

midaspdl4.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)

'Freeze the model to check the lag coefficients of the midas model for the time period 1927-1951.
freeze(midaspdl4_reg) midaspdl4

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr4_midas=midaspdl4.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg4_rsqr=((rsqr4_midas-rsqr4_ar2)/rsqr4_ar2)*100
show chg4_rsqr


                     '''''''TIME PERIOD OF 1927-1951'''''''''''

smpl @all
smpl 1986:01 2010:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq5

'Reset the sample to respective time period.
smpl 1986:01 2010:04

eq5.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr5_ar2=eq5.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl5

'Reset the sample to respective time period.
smpl 1986:01 2010:04

midaspdl5.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)

'Freeze the model to check the lag coefficients of the midas model for the time period 1927-1951.
freeze(midaspdl5_reg) midaspdl5

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr5_midas=midaspdl5.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg5_rsqr=((rsqr5_midas-rsqr5_ar2)/rsqr5_ar2)*100
show chg5_rsqr



              ''''OOS ANALYSIS FOR MIDAS REGRESSIONS''''''



smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
series yhat1_se            ' forecast std.err.
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q2+!i 1947q1+!i    ''This calculation refers to rolling window estimation scheme.
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   eq1.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat1 = tmp_yhat
   yhat1_se = tmp_se
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'egdp'.

 
smpl @all
' declare equation for estimation
equation midaspdl
 
' declare series for final results
series midaspdlfits               ' point estimates
series midaspdlfits_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q2+!i 1947q1+!i 
   ' estimate equation
  midaspdl.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   midaspdl.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits = tmp_yhat
next

' SMALL NOTE: since we obtained the fitted values for the series stdlvol for both equations, I tried to create the group series in order to see the difference between the actual values of lvol series and its behavior against the fitted values that are denoted as yhat1 and yhat2. By checking the recessions and booms of the economy we could conclude that yhat2 series exhibit more volatile series than its counterpart (yhat1). This assert the observation that Paye has mentioned on his paper. The group is labelled as 'group_lvol_yhat1_yhat2'.

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-midaspdlfits)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=100*((sigma1-sigma2)+adj_term1)
show cw_stat1
'COMMENT:.......



smpl @all
' declare equation for estimation
equation eq2
 
' declare series for final results
series yhat2               ' point estimates
series yhat2_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq2.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat2 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl2
 
' declare series for final results
series midaspdlfits2               ' point estimates
series midaspdlfits2_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
  midaspdl2.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midaspdl2.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits2 = tmp_yhat
next



smpl 1972:03 2010:04
series sigma3_sqr=(zlrvol-yhat2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midaspdlfits2)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=100*((sigma3-sigma4)+adj_term2)
show cw_stat2
'COMMENT:.......




smpl @all
' declare equation for estimation
equation eq3
 
' declare series for final results
series yhat3                   ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q2+!i 1982q1+!i
   ' estimate equation
   eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   eq3.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat3 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl3
 
' declare series for final results
series midaspdlfits3               ' point estimates
series midaspdlfits3_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q2+!i 1982q1+!i
   ' estimate equation
  midaspdl3.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   midaspdl3.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits3 = tmp_yhat
next



smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midaspdlfits3)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=100*((sigma5-sigma6)+adj_term3)
show cw_stat3
'COMMENT:.......





smpl @all
' declare equation for estimation
equation eq4
 
' declare series for final results
series yhat4               ' point estimates
series yhat4_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 94
   ' set sample to estimation period
   smpl 1952q2+!i 1977q1+!i
   ' estimate equation
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1977q2+!i 1977q2+!i
   ' make forecasts in temporary series first
   eq4.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat4 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl4
 
' declare series for final results
series midaspdlfits4               ' point estimates
series midaspdlfits4_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
  midaspdl4.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midaspdl4.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits4 = tmp_yhat
next



smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midaspdlfits4)^2
 scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=100*((sigma7-sigma8)+adj_term4)
show cw_stat4
'COMMENT:.......

                              
                                 ''''''NEW CW STATS BASED ON REGRESSIONS''''   


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr1=(yhat1-midaspdlfits)^2

series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midaspdlfits2)^2


series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midaspdlfits3)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midaspdlfits4)^2

series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:                                                



                   ''''''''''''OOS analysis based on change in rsquare'''''''''''''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment:  


             ''''''AUGMENTED VS. MIDAS REGRESSIONS'''''





smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q2+!i 1947q1+!i    ''This calculation refers to rolling window estimation scheme.
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   eq1.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat1 = tmp_yhat
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'egdp'.

 
smpl @all
' declare equation for estimation
equation midaspdl1
 
' declare series for final results
series midaspdlfits1               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q2+!i 1947q1+!i
   ' estimate equation
  midaspdl1.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   midaspdl1.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits1 = tmp_yhat
next

' SMALL NOTE: since we obtained the fitted values for the series stdlvol for both equations, I tried to create the group series in order to see the difference between the actual values of lvol series and its behavior against the fitted values that are denoted as yhat1 and yhat2. By checking the recessions and booms of the economy we could conclude that yhat2 series exhibit more volatile series than its counterpart (yhat1). This assert the observation that Paye has mentioned on his paper. The group is labelled as 'group_lvol_yhat1_yhat2'.

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-midaspdlfits1)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=100*((sigma1-sigma2)+adj_term1)
show cw_stat1
'COMMENT:.......



smpl @all
' declare equation for estimation
equation eq2
 
' declare series for final results
series yhat2               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq2.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat2 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl2
 
' declare series for final results
series midaspdlfits2               ' point estimates 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
  midaspdl2.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midaspdl2.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits2 = tmp_yhat
next



smpl 1972:03 2010:04
series sigma3_sqr=(zlrvol-yhat2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midaspdlfits2)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=100*((sigma3-sigma4)+adj_term2)
show cw_stat2
'COMMENT:.......




smpl @all
' declare equation for estimation
equation eq3
 
' declare series for final results
series yhat3                   ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q2+!i 1982q1+!i
   ' estimate equation
   eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   eq3.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat3 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl3
 
' declare series for final results
series midaspdlfits3               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q2+!i 1982q1+!i
   ' estimate equation
  midaspdl3.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   midaspdl3.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits3 = tmp_yhat
next



smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midaspdlfits3)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=100*((sigma5-sigma6)+adj_term3)
show cw_stat3
'COMMENT:.......





smpl @all
' declare equation for estimation
equation eq4
 
' declare series for final results
series yhat4               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq4.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat4 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl4
 
' declare series for final results
series midaspdlfits4               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
  midaspdl4.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midaspdl4.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits4 = tmp_yhat
next



smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midaspdlfits4)^2
 scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=100*((sigma7-sigma8)+adj_term4)
show cw_stat4
'COMMENT:.......


                           ''''''NEW CW STATS BASED ON REGRESSIONS''''   


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr1=(yhat1-midaspdlfits1)^2

series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midaspdlfits2)^2


series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midaspdlfits3)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midaspdlfits4)^2

series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:                                                


                   ''''''''''''OOS analysis based on change in rsquare'''''''''''''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment:  



    ''''''Trials of MIDAS Regressions by using the Recursive Estimation Method'''''''




smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q3 1947q1+!i    ''This calculation refers to rolling window estimation scheme.
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   eq1.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat1 = tmp_yhat
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'egdp'.

 
smpl @all
' declare equation for estimation
equation midaspdl
 
' declare series for final results
series midaspdlfits               ' point estimates
series midaspdlfits_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q3 1947q1+!i
   ' estimate equation
  midaspdl.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   midaspdl.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits = tmp_yhat
next



smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-midaspdlfits)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=100*((sigma1-sigma2)+adj_term1)
show cw_stat1
'COMMENT:.......



smpl @all
' declare equation for estimation
equation eq2
 
' declare series for final results
series yhat2               ' point estimates 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq2.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat2 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl2
 
' declare series for final results
series midaspdlfits2               ' point estimates
series midaspdlfits2_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
  midaspdl2.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midaspdl2.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits2 = tmp_yhat
next



smpl 1972:03 2010:04
series sigma3_sqr=(zlrvol-yhat2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midaspdlfits2)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=100*((sigma3-sigma4)+adj_term2)
show cw_stat2
'COMMENT:.......




smpl @all
' declare equation for estimation
equation eq3
 
' declare series for final results
series yhat3                   ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q3 1982q1+!i
   ' estimate equation
   eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   eq3.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat3 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl3
 
' declare series for final results
series midaspdlfits3               ' point estimates
series midaspdlfits3_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q3 1982q1+!i
   ' estimate equation
  midaspdl3.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   midaspdl3.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits3 = tmp_yhat
next



smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midaspdlfits3)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=100*((sigma5-sigma6)+adj_term3)
show cw_stat3
'COMMENT:.......





smpl @all
' declare equation for estimation
equation eq4
 
' declare series for final results
series yhat4               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq4.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat4 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl4
 
' declare series for final results
series midaspdlfits4               ' point estimates
series midaspdlfits4_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
  midaspdl4.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midaspdl4.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits4 = tmp_yhat
next



smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midaspdlfits4)^2
 scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=100*((sigma7-sigma8)+adj_term4)
show cw_stat4
'COMMENT:.......

                                  ''''''NEW CW STATS BASED ON REGRESSIONS''''   


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr1=(yhat1-midaspdlfits)^2

series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midaspdlfits2)^2


series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midaspdlfits3)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midaspdlfits4)^2

series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:                                                



                   ''''''''''''OOS analysis based on change in rsquare'''''''''''''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment:  



    ''''''''ROBUSTNESS CHECK FOR REC.EST. WITH 'STEP' WEIGHTING''''''''


' HERE I CONDUCTED THE SAME ANALYSIS OF RECURSIVE ESTIMATION METHOD BY CONDUCTING DIFFERENT WEIGHTING SCHEME NAMELY 'STEP' WEIGHTING SCHEME!!!!!

smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q3 1947q1+!i    ''This calculation refers to rolling window estimation scheme.
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   eq1.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat1 = tmp_yhat
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'egdp'.

 
smpl @all
' declare equation for estimation
equation midaspdl
 
' declare series for final results
series midaspdlfits               ' point estimates
series midaspdlfits_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q3 1947q1+!i
   ' estimate equation
  midaspdl.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   midaspdl.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits = tmp_yhat
next

' SMALL NOTE: since we obtained the fitted values for the series stdlvol for both equations, I tried to create the group series in order to see the difference between the actual values of lvol series and its behavior against the fitted values that are denoted as yhat1 and yhat2. By checking the recessions and booms of the economy we could conclude that yhat2 series exhibit more volatile series than its counterpart (yhat1). This assert the observation that Paye has mentioned on his paper. The group is labelled as 'group_lvol_yhat1_yhat2'.

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-midaspdlfits)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=100*((sigma1-sigma2)+adj_term1)
show cw_stat1
'COMMENT:.......



smpl @all
' declare equation for estimation
equation eq2
 
' declare series for final results
series yhat2               ' point estimates 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq2.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat2 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl2
 
' declare series for final results
series midaspdlfits2               ' point estimates
series midaspdlfits2_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
  midaspdl2.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midaspdl2.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits2 = tmp_yhat
next



smpl 1972:03 2010:04
series sigma3_sqr=(zlrvol-yhat2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midaspdlfits2)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=100*((sigma3-sigma4)+adj_term2)
show cw_stat2
'COMMENT:.......




smpl @all
' declare equation for estimation
equation eq3
 
' declare series for final results
series yhat3                   ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q3 1982q1+!i
   ' estimate equation
   eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   eq3.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat3 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl3
 
' declare series for final results
series midaspdlfits3               ' point estimates
series midaspdlfits3_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q3 1982q1+!i
   ' estimate equation
  midaspdl3.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   midaspdl3.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits3 = tmp_yhat
next



smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midaspdlfits3)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=100*((sigma5-sigma6)+adj_term3)
show cw_stat3
'COMMENT:.......





smpl @all
' declare equation for estimation
equation eq4
 
' declare series for final results
series yhat4               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq4.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat4 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl4
 
' declare series for final results
series midaspdlfits4               ' point estimates
series midaspdlfits4_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
  midaspdl4.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midaspdl4.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits4 = tmp_yhat
next



smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midaspdlfits4)^2
 scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=100*((sigma7-sigma8)+adj_term4)
show cw_stat4
'COMMENT:.......


                                  ''''''NEW CW STATS BASED ON REGRESSIONS''''   


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr1=(yhat1-midaspdlfits)^2

series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midaspdlfits2)^2


series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midaspdlfits3)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midaspdlfits4)^2

series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:       



                   ''''''''''''OOS analysis based on change in rsquare'''''''''''''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment:  


                 ''''''AR(2) benchmark model versus MIDAS-type models-second comparison''''


'1) First I have implemented the comparison for the polynomial weighting scheme and with the rolling windows estimation procedure.




smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q2+!i 1947q1+!i    ''This calculation refers to rolling window estimation scheme.
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   eq1.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat1 = tmp_yhat
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'egdp'.

 
smpl @all
' declare equation for estimation
equation midaspdl1
 
' declare series for final results
series midaspdlfits1               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q2+!i 1947q1+!i
   ' estimate equation
  midaspdl1.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   midaspdl1.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits1 = tmp_yhat
next

' SMALL NOTE: since we obtained the fitted values for the series stdlvol for both equations, I tried to create the group series in order to see the difference between the actual values of lvol series and its behavior against the fitted values that are denoted as yhat1 and yhat2. By checking the recessions and booms of the economy we could conclude that yhat2 series exhibit more volatile series than its counterpart (yhat1). This assert the observation that Paye has mentioned on his paper. The group is labelled as 'group_lvol_yhat1_yhat2'.

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-midaspdlfits1)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=100*((sigma1-sigma2)+adj_term1)
show cw_stat1
'COMMENT:.......



smpl @all
' declare equation for estimation
equation eq2
 
' declare series for final results
series yhat2               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq2.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat2 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl2
 
' declare series for final results
series midaspdlfits2               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
  midaspdl2.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midaspdl2.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits2 = tmp_yhat
next



smpl 1972:03 2010:04
series sigma3_sqr=(zlrvol-yhat2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midaspdlfits2)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=100*((sigma3-sigma4)+adj_term2)
show cw_stat2
'COMMENT:.......




smpl @all
' declare equation for estimation
equation eq3
 
' declare series for final results
series yhat3                   ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q2+!i 1982q1+!i
   ' estimate equation
   eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   eq3.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat3 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl3
 
' declare series for final results
series midaspdlfits3               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q2+!i 1982q1+!i
   ' estimate equation
  midaspdl3.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   midaspdl3.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits3 = tmp_yhat
next



smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midaspdlfits3)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=100*((sigma5-sigma6)+adj_term3)
show cw_stat3
'COMMENT:.......





smpl @all
' declare equation for estimation
equation eq4
 
' declare series for final results
series yhat4               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq4.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat4 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl4
 
' declare series for final results
series midaspdlfits4               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
  midaspdl4.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midaspdl4.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits4 = tmp_yhat
next



smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midaspdlfits4)^2
 scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=100*((sigma7-sigma8)+adj_term4)
show cw_stat4
'COMMENT:.......

                                  ''''''NEW CW STATS BASED ON REGRESSIONS''''   


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr1=(yhat1-midaspdlfits1)^2

series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midaspdlfits2)^2


series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midaspdlfits3)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midaspdlfits4)^2

series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:  



                   ''''''''''''OOS analysis based on change in rsquare'''''''''''''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment: 


                                                        ''''STEP WEIGHTING SCHEME''''''

smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q2+!i 1947q1+!i    ''This calculation refers to rolling window estimation scheme.
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   eq1.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat1 = tmp_yhat
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'egdp'.

 
smpl @all
' declare equation for estimation
equation midaspdl1
 
' declare series for final results
series midaspdlfits1               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q2+!i 1947q1+!i
   ' estimate equation
  midaspdl1.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   midaspdl1.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits1 = tmp_yhat
next

' SMALL NOTE: since we obtained the fitted values for the series stdlvol for both equations, I tried to create the group series in order to see the difference between the actual values of lvol series and its behavior against the fitted values that are denoted as yhat1 and yhat2. By checking the recessions and booms of the economy we could conclude that yhat2 series exhibit more volatile series than its counterpart (yhat1). This assert the observation that Paye has mentioned on his paper. The group is labelled as 'group_lvol_yhat1_yhat2'.

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-midaspdlfits1)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=100*((sigma1-sigma2)+adj_term1)
show cw_stat1
'COMMENT:.......



smpl @all
' declare equation for estimation
equation eq2
 
' declare series for final results
series yhat2               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq2.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat2 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl2
 
' declare series for final results
series midaspdlfits2               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
  midaspdl2.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midaspdl2.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits2 = tmp_yhat
next



smpl 1972:03 2010:04
series sigma3_sqr=(zlrvol-yhat2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midaspdlfits2)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=100*((sigma3-sigma4)+adj_term2)
show cw_stat2
'COMMENT:.......




smpl @all
' declare equation for estimation
equation eq3
 
' declare series for final results
series yhat3                   ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q2+!i 1982q1+!i
   ' estimate equation
   eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   eq3.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat3 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl3
 
' declare series for final results
series midaspdlfits3               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q2+!i 1982q1+!i
   ' estimate equation
  midaspdl3.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   midaspdl3.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits3 = tmp_yhat
next



smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midaspdlfits3)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=100*((sigma5-sigma6)+adj_term3)
show cw_stat3
'COMMENT:.......





smpl @all
' declare equation for estimation
equation eq4
 
' declare series for final results
series yhat4               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq4.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat4 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl4
 
' declare series for final results
series midaspdlfits4               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
  midaspdl4.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midaspdl4.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits4 = tmp_yhat
next



smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midaspdlfits4)^2
 scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=100*((sigma7-sigma8)+adj_term4)
show cw_stat4
'COMMENT:.......


                                  ''''''NEW CW STATS BASED ON REGRESSIONS''''   


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr1=(yhat1-midaspdlfits1)^2

series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midaspdlfits2)^2


series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midaspdlfits3)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midaspdlfits4)^2

series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:  


                   ''''''''''''OOS analysis based on change in rsquare'''''''''''''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment: 



               ''''TRIALS FOR COMPARISON OF MIDAS with AR(2)-RECURSIVE ESTIMATION'''''


smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q3 1947q1+!i    ''This calculation refers to rolling window estimation scheme.
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   eq1.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat1 = tmp_yhat
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'egdp'.

 
smpl @all
' declare equation for estimation
equation midaspdl1
 
' declare series for final results
series midaspdlfits1               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q3 1947q1+!i
   ' estimate equation
  midaspdl1.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   midaspdl1.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits1 = tmp_yhat
next

' SMALL NOTE: since we obtained the fitted values for the series stdlvol for both equations, I tried to create the group series in order to see the difference between the actual values of lvol series and its behavior against the fitted values that are denoted as yhat1 and yhat2. By checking the recessions and booms of the economy we could conclude that yhat2 series exhibit more volatile series than its counterpart (yhat1). This assert the observation that Paye has mentioned on his paper. The group is labelled as 'group_lvol_yhat1_yhat2'.

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-midaspdlfits1)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=100*((sigma1-sigma2)+adj_term1)
show cw_stat1
'COMMENT:.......



smpl @all
' declare equation for estimation
equation eq2
 
' declare series for final results
series yhat2               ' point estimates 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq2.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat2 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl2
 
' declare series for final results
series midaspdlfits2               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
  midaspdl2.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midaspdl2.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits2 = tmp_yhat
next



smpl 1972:03 2010:04
series sigma3_sqr=(zlrvol-yhat2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midaspdlfits2)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=100*((sigma3-sigma4)+adj_term2)
show cw_stat2
'COMMENT:.......




smpl @all
' declare equation for estimation
equation eq3
 
' declare series for final results
series yhat3                   ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q3 1982q1+!i
   ' estimate equation
   eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   eq3.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat3 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl3
 
' declare series for final results
series midaspdlfits3               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q3 1982q1+!i
   ' estimate equation
  midaspdl3.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   midaspdl3.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits3 = tmp_yhat
next



smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midaspdlfits3)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=100*((sigma5-sigma6)+adj_term3)
show cw_stat3
'COMMENT:.......





smpl @all
' declare equation for estimation
equation eq4
 
' declare series for final results
series yhat4               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq4.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat4 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midaspdl4
 
' declare series for final results
series midaspdlfits4               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
  midaspdl4.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midaspdl4.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midaspdlfits4 = tmp_yhat
next



smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midaspdlfits4)^2
 scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=100*((sigma7-sigma8)+adj_term4)
show cw_stat4
'COMMENT:.......

        ''''''NEW CW STATS BASED ON REGRESSIONS''''   


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr1=(yhat1-midaspdlfits1)^2

series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midaspdlfits2)^2


series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midaspdlfits3)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midaspdlfits4)^2

series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:  


                   ''''''''''''OOS analysis based on change in rsquare'''''''''''''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midaspdlfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midaspdlfits3)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midaspdlfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment: 



       ''''''''ROBUSTNESS CHECK FOR REC.EST. WITH 'STEP' WEIGHTING''''''''


' HERE I CONDUCTED THE SAME ANALYSIS OF RECURSIVE ESTIMATION METHOD BY CONDUCTING DIFFERENT WEIGHTING SCHEME NAMELY 'STEP' WEIGHTING SCHEME!!!!!

smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q3 1947q1+!i    ''This calculation refers to rolling window estimation scheme.
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   eq1.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat1 = tmp_yhat
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'egdp'.

 
smpl @all
' declare equation for estimation
equation midasstep1
 
' declare series for final results
series midasstepfits1               ' point estimates

 
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q3 1947q1+!i
   ' estimate equation
  midasstep1.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   midasstep1.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midasstepfits1 = tmp_yhat
next

' SMALL NOTE: since we obtained the fitted values for the series stdlvol for both equations, I tried to create the group series in order to see the difference between the actual values of lvol series and its behavior against the fitted values that are denoted as yhat1 and yhat2. By checking the recessions and booms of the economy we could conclude that yhat2 series exhibit more volatile series than its counterpart (yhat1). This assert the observation that Paye has mentioned on his paper. The group is labelled as 'group_lvol_yhat1_yhat2'.

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midasstepfits1)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-midasstepfits1)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=100*((sigma1-sigma2)+adj_term1)
show cw_stat1
'COMMENT:.......



smpl @all
' declare equation for estimation
equation eq2
 
' declare series for final results
series yhat2               ' point estimates 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq2.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat2 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midasstep2
 
' declare series for final results
series midasstepfits2               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
  midasstep2.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midasstep2.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midasstepfits2 = tmp_yhat
next



smpl 1972:03 2010:04
series sigma3_sqr=(zlrvol-yhat2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midasstepfits2)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midasstepfits2)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=100*((sigma3-sigma4)+adj_term2)
show cw_stat2
'COMMENT:.......




smpl @all
' declare equation for estimation
equation eq3
 
' declare series for final results
series yhat3                   ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q3 1982q1+!i
   ' estimate equation
   eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   eq3.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat3 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midasstep3
 
' declare series for final results
series midasstepfits3               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q3 1982q1+!i
   ' estimate equation
  midasstep3.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   midasstep3.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midasstepfits3 = tmp_yhat
next



smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midasstepfits3)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midasstepfits3)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=100*((sigma5-sigma6)+adj_term3)
show cw_stat3
'COMMENT:.......





smpl @all
' declare equation for estimation
equation eq4
 
' declare series for final results
series yhat4               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq4.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat4 = tmp_yhat
next


smpl @all
' declare equation for estimation
equation midasstep4
 
' declare series for final results
series midasstepfits4               ' point estimates
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q3 1972q1+!i
   ' estimate equation
  midasstep4.MIDAS(fixedlag=4, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3) 	'Equation MIDAS PDL weighting
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   midasstep4.forecast(f=na) tmp_yhat 
   ' copy data in current forecast sample
   midasstepfits4 = tmp_yhat
next



smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midasstepfits4)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midasstepfits4)^2
 scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=100*((sigma7-sigma8)+adj_term4)
show cw_stat4
'COMMENT:.......

         ''''''NEW CW STATS BASED ON REGRESSIONS''''   


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midasstepfits1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr1=(yhat1-midasstepfits1)^2

series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midasstepfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat2-midasstepfits2)^2


series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midasstepfits3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat3-midasstepfits3)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midasstepfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat4-midasstepfits4)^2

series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:  



                   ''''''''''''OOS analysis based on change in rsquare'''''''''''''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midasstepfits1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-midasstepfits2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-midasstepfits3)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-midasstepfits4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment:



         '''''''''''  MIDAS TYPE REGRESSIONS: IN-SAMPLE ANALYSIS''''''''''

'Declare the equation for the estimation of the regression for the respective time period.
equation eq1

'Reset the sample to respective time period.
smpl 1927:02 2010:04

eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr1_ar2=eq1.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq2

'Reset the sample to respective time period.
smpl 1927:02 2010:04

eq2.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)
'Extract the beta coefficient for the respective forecating variable 'cp'.
scalar beta1=@coefs(3)
show beta1

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr1_aug=eq2.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg1_rsqr=((rsqr1_aug-rsqr1_ar2)/rsqr1_ar2)*100
show chg1_rsqr



                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1952:02 2010:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq3

'Reset the sample to respective time period.
smpl 1952:02 2010:04

eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr2_ar2=eq3.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq4

'Reset the sample to respective time period.
smpl 1952:02 2010:04

eq4.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)
'Extract the beta coefficient for the respective forecating variable 'cp'.
scalar beta2=@coefs(3)
show beta2

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr2_aug=eq4.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg2_rsqr=((rsqr2_aug-rsqr2_ar2)/rsqr2_ar2)*100
show chg2_rsqr


                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1927:02 1951:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq5

'Reset the sample to respective time period.
smpl 1927:02 1951:04

eq5.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr3_ar2=eq5.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq6

'Reset the sample to respective time period.
smpl 1927:02 1951:04

eq6.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)
'Extract the beta coefficient for the respective forecating variable 'cp'.
scalar beta3=@coefs(3)
show beta3

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr3_aug=eq6.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg3_rsqr=((rsqr3_aug-rsqr3_ar2)/rsqr3_ar2)*100
show chg3_rsqr


                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1952:02 1985:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq7

'Reset the sample to respective time period.
smpl 1952:02 1985:04

eq7.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr4_ar2=eq7.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq8

'Reset the sample to respective time period.
smpl 1952:02 1985:04

eq8.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)
'Extract the beta coefficient for the respective forecating variable 'cp'.
scalar beta4=@coefs(3)
show beta4

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr4_aug=eq8.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg4_rsqr=((rsqr4_aug-rsqr4_ar2)/rsqr4_ar2)*100
show chg4_rsqr


                     '''''''TIME PERIOD OF 1927-1951'''''''''''

smpl @all
smpl 1986:01 2010:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq9

'Reset the sample to respective time period.
smpl 1986:01 2010:04

eq9.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr5_ar2=eq9.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq10

'Reset the sample to respective time period.
smpl 1986:01 2010:04

eq10.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ cp_m_insmpl\z_cp(-3)
'Extract the beta coefficient for the respective forecating variable 'cp'.
scalar beta5=@coefs(3)
show beta5

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr5_aug=eq10.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg5_rsqr=((rsqr5_aug-rsqr5_ar2)/rsqr5_ar2)*100
show chg5_rsqr


    ''''NEW OOS ANALYSIS BASED ON NEW STDZED VARIABLE'''''



                 '''''''''QUARTERLY  OOS ANALYSIS USING NEW DATA''''''''''

'Standardize the blev variable.

smpl @all
series z_cp=(cp/@stdev(cp))

smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
series yhat1_se            ' forecast std.err.
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q2+!i 1947q1+!i    ''This calculation refers to rolling window estimation scheme.
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   eq1.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat1 = tmp_yhat
   yhat1_se = tmp_se
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'egdp'.

 
smpl @all
' declare equation for estimation
equation eq2
 
' declare series for final results
series yhat2               ' point estimates
series yhat2_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q2+!i 1947q1+!i
   ' estimate equation
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1947q2+!i 1947q2+!i
   ' make forecasts in temporary series first
   eq2.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat2 = tmp_yhat
   yhat2_se = tmp_se
next

' SMALL NOTE: since we obtained the fitted values for the series stdlvol for both equations, I tried to create the group series in order to see the difference between the actual values of lvol series and its behavior against the fitted values that are denoted as yhat1 and yhat2. By checking the recessions and booms of the economy we could conclude that yhat2 series exhibit more volatile series than its counterpart (yhat1). This assert the observation that Paye has mentioned on his paper. The group is labelled as 'group_lvol_yhat1_yhat2'.

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-yhat2)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-yhat2)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
show cw_stat1
'COMMENT: PROBLEMATIC CW STATS1!!!!!!!!!!!!!!!!!!!








smpl @all
' declare equation for estimation
equation eq3
 
' declare series for final results
series yhat3               ' point estimates
series yhat3_se            ' forecast std.err.
 
' set step size
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
   eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq3.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat3 = tmp_yhat
   yhat3_se = tmp_se
next

'The same estimation and forecasting procedure can also be applied to augmented model with the forecasting variable 'egdp'.


 
smpl @all
' declare equation for estimation
equation eq4
 
' declare series for final results
series yhat4               ' point estimates
series yhat4_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq4.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat4 = tmp_yhat
   yhat4_se = tmp_se
next

smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat3)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-yhat4)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat3-yhat4)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
show cw_stat2
'COMMENT:.......




smpl @all
' declare equation for estimation
equation eq5
 
' declare series for final results
series yhat5            ' point estimates
series yhat5_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q2+!i 1982q1+!i
   ' estimate equation
   eq5.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   eq5.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat5 = tmp_yhat
   yhat5_se = tmp_se
next

smpl @all
' declare equation for estimation
equation eq6
 
' declare series for final results
series yhat6               ' point estimates
series yhat6_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q2+!i 1982q1+!i
   ' estimate equation
   eq6.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1982q2+!i 1982q2+!i
   ' make forecasts in temporary series first
   eq6.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat6 = tmp_yhat
   yhat6_se = tmp_se
next

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat5)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-yhat6)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat5-yhat6)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
show cw_stat3
'Comment: PROBLEMATIC!!!!!!!!!!!!!!???????????????



smpl @all
' declare equation for estimation
equation eq7
 
' declare series for final results
series yhat7            ' point estimates
series yhat7_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
   eq7.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq7.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat7 = tmp_yhat
   yhat7_se = tmp_se
next

smpl @all
' declare equation for estimation
equation eq8
 
' declare series for final results
series yhat8               ' point estimates
series yhat8_se            ' forecast std.err.
 
 
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q2+!i 1972q1+!i
   ' estimate equation
   eq8.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_cp(-1)
   ' reset sample to forecast period
   smpl 1972q2+!i 1972q2+!i
   ' make forecasts in temporary series first
   eq8.forecast(f=na) tmp_yhat tmp_se   
   ' copy data in current forecast sample
   yhat8 = tmp_yhat
   yhat8_se = tmp_se
next

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat7)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-yhat8)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat7-yhat8)^2
 scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4
'Comment:

                ''''''NEW CW STATS BASED ON REGRESSIONS''''   


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-yhat2)^2

series CW_stat11=@abs(100*((sigma2_sqr-sigma1_sqr)+adj_term_sqr1))

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-yhat3)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(yhat3-yhat4)^2


series CW_stat22=@abs(100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2))

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-yhat5)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-yhat6)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(yhat5-yhat6)^2
 

series CW_stat33=@abs(100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3))
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-yhat7)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-yhat8)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(yhat7-yhat8)^2

series CW_stat44=@abs(100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4))
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:


