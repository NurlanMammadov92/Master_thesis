

               '''''''''''QUARTERLY SERIES TRIALS FOR INSAMPLE ANALYSIS''''''''''''''




'Standardize the blev variable.
f
smpl @all
series z_ppivol=(ppivol-@mean(ppivol))/@stdev(ppivol)
series z_blev=(blev_new-@mean(blev_new))/@stdev(blev_new)
series z_cay=(cay-@mean(cay))/@stdev(cay)
series z_cp=(cp-@mean(cp))/@stdev(cp)
series z_dfr=(dfr-@mean(dfr))/@stdev(dfr)
series z_dfy=(dfy-@mean(dfy))/@stdev(dfy)
series z_egdp=(egdp-@mean(egdp))/@stdev(egdp)
series z_gdp=(gdp-@mean(gdp))/@stdev(gdp)
series z_ik=(ik-@mean(ik))/@stdev(ik)
series z_ipvol=(ipvol-@mean(ipvol))/@stdev(ipvol)
series z_npy=(npy-@mean(npy))/@stdev(npy)
series z_tms=(tms-@mean(tms))/@stdev(tms)
series z_exret=(fitret1big-@mean(fitret1big))/@stdev(fitret1big)




'''''''''MONTHLY OOS ANALYSIS USING NEW DATA''''''''''


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
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6) z_tms(-1) z_cp(-1) z_dfr(-1) z_dfy(-1) z_exret(-1) z_ip(-1) z_ipvol(-1) z_npy(-1) z_ppivol(-1)
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
scalar CW_stat1=((sigma1-sigma2)+adj_term1)*100
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
  eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6) z_tms(-1) z_cp(-1) z_dfr(-1) z_dfy(-1) z_exret(-1) z_ip(-1) z_ipvol(-1) z_npy(-1) z_ppivol(-1)
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
scalar CW_stat2=((sigma3-sigma4)+adj_term2)*100
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
   eq6.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6) z_tms(-1) z_cp(-1) z_dfr(-1) z_dfy(-1) z_exret(-1) z_ip(-1) z_ipvol(-1) z_npy(-1) z_ppivol(-1)
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
scalar CW_stat3=((sigma5-sigma6)+adj_term3)*100
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
   eq8.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) zlrvol(-3) zlrvol(-4) zlrvol(-5) zlrvol(-6) z_tms(-1) z_cp(-1) z_dfr(-1) z_dfy(-1) z_exret(-1) z_ip(-1) z_ipvol(-1) z_npy(-1) z_ppivol(-1)
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
scalar CW_stat4=((sigma7-sigma8)+adj_term4)*100
show CW_stat4

'COMMENT ON THE RESULTS:....PROBLEMATIC!!!!!!!!!



       ''''''NEW CW STATS BASED ON REGRESSIONS''''   


smpl 1947:03 2010:12
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-yhat2)^2

series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

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


series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

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

series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:           




                               ''''OOS MONTHLY ANALSIS BASED ON DELTA RSQUARE''''''


smpl 1947:03 2010:12
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:12

series sigma3_sqr=(zlrvol-yhat3)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-yhat4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:12
series sigma5_sqr=(zlrvol-yhat5)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-yhat6)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:12
series sigma7_sqr=(zlrvol-yhat7)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-yhat8)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment:  





            '''''''''''IN-SAMPLE ANALYSIS''''''''''

'Declare the equation for the estimation of the regression for the respective time period.
equation eq1

'Reset the sample to respective time period.
smpl 1927:02 2010:04
scalar nobs1=@obs(zlrvol)

eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr_adj1_ar2=eq1.@rbar2
scalar rsqr1_ar2=eq1.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq2

'Reset the sample to respective time period.
smpl 1927:02 2010:04

eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)  z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1)  z_cp(-1) z_exret(-1)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr_adj1_ks=eq2.@rbar2
scalar rsqr1_ks=eq2.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg1_rsqr=((rsqr1_ks-rsqr1_ar2)/rsqr1_ar2)*100
show chg1_rsqr

'EXTRACT THE F STATS VALUE FOR COMPARING THE MODELS:
scalar fstats1=((rsqr1_ks-rsqr1_ar2)/8)/((1-rsqr1_ks)/(nobs1-10))
show fstats1


                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1952:02 2010:04
scalar nobs2=@obs(zlrvol)

'Declare the equation for the estimation of the regression for the respective time period.
equation eq3

'Reset the sample to respective time period.
smpl 1952:02 2010:04

eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr_adj2_ar2=eq3.@rbar2
scalar rsqr2_ar2=eq3.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq4

'Reset the sample to respective time period.
smpl 1952:02 2010:04

eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)  z_dfy(-1) z_dfr(-1) z_egdp(-1) z_gdp(-1) z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cay(-1) z_blev(-1) z_cp(-1) z_exret(-1) z_ik(-1)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr_adj2_ks=eq4.@rbar2
scalar rsqr2_ks=eq4.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg2_rsqr=((rsqr2_ks-rsqr2_ar2)/rsqr2_ar2)*100
show chg2_rsqr

'EXTRACT THE F STATS VALUE FOR COMPARING THE MODELS:
scalar fstats2=((rsqr2_ks-rsqr2_ar2)/13)/((1-rsqr2_ks)/(nobs2-15))
show fstats2

                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1927:02 1951:04
scalar nobs3=@obs(zlrvol)

'Declare the equation for the estimation of the regression for the respective time period.
equation eq5

'Reset the sample to respective time period.
smpl 1927:02 1951:04

eq5.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr_adj3_ar2=eq5.@rbar2
scalar rsqr3_ar2=eq5.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq6

'Reset the sample to respective time period.
smpl 1927:02 1951:04

eq6.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)  z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1) 

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr_adj3_ks=eq6.@rbar2
scalar rsqr3_ks=eq6.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg3_rsqr=((rsqr3_ks-rsqr3_ar2)/rsqr3_ar2)*100
show chg3_rsqr

'EXTRACT THE F STATS VALUE FOR KS REGRESSION:
scalar fstats3=((rsqr3_ks-rsqr3_ar2)/8)/((1-rsqr3_ks)/(nobs3-10))
show fstats3

                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1952:02 1985:04
scalar nobs4=@obs(zlrvol)
'Declare the equation for the estimation of the regression for the respective time period.
equation eq7

'Reset the sample to respective time period.
smpl 1952:02 1985:04

eq7.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr_adj4_ar2=eq7.@rbar2
scalar rsqr4_ar2=eq7.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation eq8

'Reset the sample to respective time period.
smpl 1952:02 1985:04

eq8.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)  z_dfy(-1) z_dfr(-1) z_egdp(-1) z_gdp(-1) z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cay(-1) z_blev(-1) z_cp(-1) z_exret(-1) z_ik(-1)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr_adj4_ks=eq8.@rbar2
scalar rsqr4_ks=eq8.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg4_rsqr=((rsqr4_ks-rsqr4_ar2)/rsqr4_ar2)*100
show chg4_rsqr

'EXTRACT THE F STATS VALUE FOR KS REGRESSION:
scalar fstats4=((rsqr4_ks-rsqr4_ar2)/13)/((1-rsqr4_ks)/(nobs4-15))
show fstats4

'COMMENT ON THE RESULTS: PROBLEMATIC DELTA RSQUARE!!!!!!!!!!!!!


                     '''''''TIME PERIOD OF 1927-1951'''''''''''

smpl @all
smpl 1986:01 2010:04
scalar nobs5=@obs(zlrvol)
'Declare the equation for the estimation of the regression for the respective time period.
equation eq9

'Reset the sample to respective time period.
smpl 1986:01 2010:04

eq9.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr_adj5_ar2=eq9.@rbar2
scalar rsqr5_ar2=eq9.@r2
'Declare the equation for the estimation of the regression for the respective time period.
equation eq10

'Reset the sample to respective time period.
smpl 1986:01 2010:04

eq10.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)  z_dfy(-1) z_dfr(-1) z_egdp(-1) z_gdp(-1) z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cay(-1) z_blev(-1) z_cp(-1) z_exret(-1) z_ik(-1)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr_adj5_ks=eq10.@rbar2
scalar rsqr5_ks=eq10.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg5_rsqr=((rsqr5_ks-rsqr5_ar2)/rsqr5_ar2)*100
show chg5_rsqr

'EXTRACT THE F STATS VALUE FOR KS REGRESSION:
scalar fstats5=((rsqr5_ks-rsqr5_ar2)/13)/((1-rsqr5_ks)/(nobs5-15))
show fstats5
'COMMENT ON THE RESULTS:..PROBLEMATIC!!!!!!!!!!!'

                 '''''''''QUARTERLY  OOS ANALYSIS USING NEW DATA''''''''''

'Standardize the blev variable.

smpl @all
series z_ppivol=(ppivol-@mean(ppivol))/@stdev(ppivol)
series z_blev=(blev_new-@mean(blev_new))/@stdev(blev_new)
series z_cay=(cay-@mean(cay))/@stdev(cay)
series z_cp=(cp-@mean(cp))/@stdev(cp)
series z_dfr=(dfr-@mean(dfr))/@stdev(dfr)
series z_dfy=(dfy-@mean(dfy))/@stdev(dfy)
series z_egdp=(egdp-@mean(egdp))/@stdev(egdp)
series z_gdp=(gdp-@mean(gdp))/@stdev(gdp)
series z_ik=(ik-@mean(ik))/@stdev(ik)
series z_ipvol=(ipvol-@mean(ipvol))/@stdev(ipvol)
series z_npy=(npy-@mean(npy))/@stdev(npy)
series z_tms=(tms-@mean(tms))/@stdev(tms)
series z_exret=(fitret1big-@mean(fitret1big))/@stdev(fitret1big)

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
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1) 
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

scalar CW_stat1=100*((sigma1-sigma2)+adj_term1)
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
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1) z_egdp(-1) z_gdp(-1) z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cay(-1) z_blev(-1) z_cp(-1) z_exret(-1) z_ik(-1)
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

scalar CW_stat2=100*((sigma3-sigma4)+adj_term2)
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
   eq6.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1) z_egdp(-1) z_gdp(-1) z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cay(-1) z_blev(-1) z_cp(-1) z_exret(-1) z_ik(-1)
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
   eq8.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1) z_egdp(-1) z_gdp(-1) z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cay(-1) z_blev(-1) z_cp(-1) z_exret(-1) z_ik(-1)
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

scalar CW_stat4=100*((sigma7-sigma8)+adj_term4)
show cw_stat4
  
  
      

                 ''''''NEW CW STATS BASED ON REGRESSIONS''''   


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-yhat2)^2

series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

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


series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

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

series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
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




             '''''Recursive estimation method for kitchen sink'''''''''''''''''''





                 '''''RECURSIVE ESTIMATION METHOD-NEW DATA''''''


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
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1) 
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

scalar CW_stat1=100*((sigma1-sigma2)+adj_term1)
show cw_stat1








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
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1) z_egdp(-1) z_gdp(-1) z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cay(-1) z_blev(-1) z_cp(-1) z_exret(-1) z_ik(-1)
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

scalar CW_stat2=100*((sigma3-sigma4)+adj_term2)
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
   eq6.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1) z_egdp(-1) z_gdp(-1) z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cay(-1) z_blev(-1) z_cp(-1) z_exret(-1) z_ik(-1)
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
   eq8.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1) z_egdp(-1) z_gdp(-1) z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cay(-1) z_blev(-1) z_cp(-1) z_exret(-1) z_ik(-1)
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

scalar CW_stat4=100*((sigma7-sigma8)+adj_term4)
show cw_stat4


   
  
  




                       ''''''NEW CW STATS BASED ON REGRESSIONS''''   


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-yhat2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(yhat1-yhat2)^2

series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

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


series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

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

series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
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




                '''''COMBINED FORECASTS OF INDIVIDUAL FORECASTS''''''''''''''


smpl @all
rename yhat2 blev1
rename yhat4 blev2
rename yhat6 blev3

smpl @all
rename yhat2 cay1
rename yhat4 cay2
rename yhat6 cay3

smpl @all
rename yhat2 cp1
rename yhat4 cp2
rename yhat6 cp3
rename yhat8 cp4

smpl @all
rename yhat2 dfr1
rename yhat4 dfr2
rename yhat6 dfr3
rename yhat8 dfr4

smpl @all
rename yhat2 dfy1
rename yhat4 dfy2
rename yhat6 dfy3
rename yhat8 dfy4


smpl @all
rename yhat2 egdp1
rename yhat4 egdp2
rename yhat6 egdp3

smpl @all
rename yhat2 exret1
rename yhat4 exret2
rename yhat6 exret3
rename yhat8 exret4



smpl @all
rename yhat2 gdp1
rename yhat4 gdp2
rename yhat6 gdp3



smpl @all
rename yhat2 ik1
rename yhat4 ik2
rename yhat6 ik3



smpl @all
rename yhat2 ipvol1
rename yhat4 ipvol2
rename yhat6 ipvol3
rename yhat8 ipvol4


smpl @all
rename yhat2 npy1
rename yhat4 npy2
rename yhat6 npy3
rename yhat8 npy4


smpl @all
rename yhat2 ppivol1
rename yhat4 ppivol2
rename yhat6 ppivol3
rename yhat8 ppivol4


smpl @all
rename yhat2 tms1
rename yhat4 tms2
rename yhat6 tms3
rename yhat8 tms4

smpl @all
rename yhat1 ar2_1
rename yhat3 ar2_2
rename yhat5 ar2_3
rename yhat7 ar2_4


'''''COMBINED FORECASTS-SIMPLE MEAN''''


smpl @all

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1)^2


series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1



smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2)^2

series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3)^2


series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(ar2_4-zlrvol_f4)^2


series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:   

'''''''''OOS ANALYSIS BASED ON DELTA_RSQR'''''''''''''''

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment:






'''''COMBINED FORECASTS-SIMPLE MEDIAN''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1)^2


series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2)^2

series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3)^2


series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(ar2_4-zlrvol_f4)^2


series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:   

                   '''''''''OOS ANALYSIS BASED ON DELTA_RSQR'''''''''
smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment:

                


'''''COMBINED FORECASTS OF INDIVIDUAL FORECASTS-RECURSIVE ESTIMATION''''''''''''''


smpl @all
rename yhat2 blev1
rename yhat4 blev2
rename yhat6 blev3

smpl @all
rename yhat2 cay1
rename yhat4 cay2
rename yhat6 cay3

smpl @all
rename yhat2 cp1
rename yhat4 cp2
rename yhat6 cp3
rename yhat8 cp4

smpl @all
rename yhat2 dfr1
rename yhat4 dfr2
rename yhat6 dfr3
rename yhat8 dfr4

smpl @all
rename yhat2 dfy1
rename yhat4 dfy2
rename yhat6 dfy3
rename yhat8 dfy4


smpl @all
rename yhat2 egdp1
rename yhat4 egdp2
rename yhat6 egdp3

smpl @all
rename yhat2 exret1
rename yhat4 exret2
rename yhat6 exret3
rename yhat8 exret4



smpl @all
rename yhat2 gdp1
rename yhat4 gdp2
rename yhat6 gdp3



smpl @all
rename yhat2 ik1
rename yhat4 ik2
rename yhat6 ik3



smpl @all
rename yhat2 ipvol1
rename yhat4 ipvol2
rename yhat6 ipvol3
rename yhat8 ipvol4


smpl @all
rename yhat2 npy1
rename yhat4 npy2
rename yhat6 npy3
rename yhat8 npy4


smpl @all
rename yhat2 ppivol1
rename yhat4 ppivol2
rename yhat6 ppivol3
rename yhat8 ppivol4


smpl @all
rename yhat2 tms1
rename yhat4 tms2
rename yhat6 tms3
rename yhat8 tms4

smpl @all
rename yhat1 ar2_1
rename yhat3 ar2_2
rename yhat5 ar2_3
rename yhat7 ar2_4


'''''COMBINED FORECASTS-SIMPLE MEAN RECURSIVE ESTIMATION''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1)^2


series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2)^2

series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3)^2


series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(ar2_4-zlrvol_f4)^2


series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:   

                   

                   ''''''''''''OOS analysis based on change in rsquare'''''''''''''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment:


         '''''COMBINED FORECASTS SIMPLE MEDIAN-RECURSIVE ESTIMATION'''''



smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1)^2


series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2)^2

series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3)^2


series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(ar2_4-zlrvol_f4)^2


series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:   

                   
                   ''''''''''''OOS analysis based on change in rsquare'''''''''''''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment:





        '''''''''''''TRIALS FOR MIDAS REGRESSIONS''''''''

'1) Here I implemented in-sample analysis for the midas regression compared to ar-type models.

                         '''''''''''IN-SAMPLE ANALYSIS''''''''''

'Declare the equation for the estimation of the regression for the respective time period.

smpl @all
equation eq1

'Reset the sample to respective time period.
smpl 1927:02 2010:04
scalar nobs1=@obs(zlrvol)

eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr1_ar2=eq1.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl1

'Reset the sample to respective time period.
smpl 1927:02 2010:04

midaspdl1.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3)
'Freeze the table to check the lag coefficients of MIDAS regressions.
freeze(midaspdl1_reg) midaspdl1

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr1_midas=midaspdl1.@r2


'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg1_rsqr=((rsqr1_midas-rsqr1_ar2)/rsqr1_ar2)*100
show chg1_rsqr

'EXTRACT THE F STATS VALUE FOR COMPARING THE MODELS:
scalar fstats1=((rsqr1_midas-rsqr1_ar2)/16)/((1-rsqr1_midas)/(nobs1-18))
show fstats1



                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1952:02 2010:04
scalar nobs2=@obs(zlrvol)

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

midaspdl2.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3)
'Freeze the model to check the lag coefficients of the midas model for the time period 1952-2010.
freeze(midaspdl2_reg) midaspdl2

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr2_midas=midaspdl2.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg2_rsqr=((rsqr2_midas-rsqr2_ar2)/rsqr2_ar2)*100
show chg2_rsqr

'EXTRACT THE F STATS VALUE FOR COMPARING THE MODELS:
scalar fstats2=((rsqr2_midas-rsqr2_ar2)/16)/((1-rsqr2_midas)/(nobs2-18))
show fstats2


                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1927:02 1951:04
scalar nobs3=@obs(zlrvol)

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

midaspdl3.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3)

'Freeze the model to check the lag coefficients of the midas model for the time period 1927-1951.
freeze(midaspdl3_reg) midaspdl3

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr3_midas=midaspdl3.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg3_rsqr=((rsqr3_midas-rsqr3_ar2)/rsqr3_ar2)*100
show chg3_rsqr

'EXTRACT THE F STATS VALUE FOR COMPARING THE MODELS:
scalar fstats3=((rsqr3_midas-rsqr3_ar2)/16)/((1-rsqr3_midas)/(nobs3-18))
show fstats3


                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1952:02 1985:04
scalar nobs4=@obs(zlrvol)

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

midaspdl4.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3)

'Freeze the model to check the lag coefficients of the midas model for the time period 1927-1951.
freeze(midaspdl4_reg) midaspdl4

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr4_midas=midaspdl4.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg4_rsqr=((rsqr4_midas-rsqr4_ar2)/rsqr4_ar2)*100
show chg4_rsqr

'EXTRACT THE F STATS VALUE FOR COMPARING THE MODELS:
scalar fstats4=((rsqr4_midas-rsqr4_ar2)/16)/((1-rsqr4_midas)/(nobs4-18))
show fstats4

                     '''''''TIME PERIOD OF 1927-1951'''''''''''

smpl @all
smpl 1986:01 2010:04
scalar nobs5=@obs(zlrvol)

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

midaspdl5.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3)

'Freeze the model to check the lag coefficients of the midas model for the time period 1927-1951.
freeze(midaspdl5_reg) midaspdl5

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr5_midas=midaspdl5.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg5_rsqr=((rsqr5_midas-rsqr5_ar2)/rsqr5_ar2)*100
show chg5_rsqr

'EXTRACT THE F STATS VALUE FOR COMPARING THE MODELS:
scalar fstats5=((rsqr5_midas-rsqr5_ar2)/16)/((1-rsqr5_midas)/(nobs5-18))
show fstats5




'2) Here I run in-sample analysis for the MIDAS-AUGMENTED models. 

                         '''''''''''IN-SAMPLE ANALYSIS''''''''''

'Declare the equation for the estimation of the regression for the respective time period.

smpl @all
equation eq1

'Reset the sample to respective time period.
smpl 1927:02 2010:04
scalar nobs1=@obs(zlrvol)

eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr1_ar2=eq1.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl1

'Reset the sample to respective time period.
smpl 1927:02 2010:04

midaspdl1.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 
'Freeze the table to check the lag coefficients of MIDAS regressions.
freeze(midaspdl1_reg) midaspdl1

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr1_midas=midaspdl1.@r2


'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg1_rsqr=((rsqr1_midas-rsqr1_ar2)/rsqr1_ar2)*100
show chg1_rsqr

'EXTRACT THE F STATS VALUE FOR COMPARING THE MODELS:
scalar fstats1=((rsqr1_midas-rsqr1_ar2)/8)/((1-rsqr1_midas)/(nobs1-18))
show fstats1



                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1952:02 2010:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq2
scalar nobs2=@obs(zlrvol)

'Reset the sample to respective time period.
smpl 1952:02 2010:04

eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr2_ar2=eq2.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl2

'Reset the sample to respective time period.
smpl 1952:02 2010:04

midaspdl2.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 
'Freeze the model to check the lag coefficients of the midas model for the time period 1952-2010.
freeze(midaspdl2_reg) midaspdl2

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr2_midas=midaspdl2.@r2


'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg2_rsqr=((rsqr2_midas-rsqr2_ar2)/rsqr2_ar2)*100
show chg2_rsqr

'EXTRACT THE F STATS VALUE FOR COMPARING THE MODELS:
scalar fstats2=((rsqr2_midas-rsqr2_ar2)/8)/((1-rsqr2_midas)/(nobs2-18))
show fstats2


                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1927:02 1951:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq3
scalar nobs3=@obs(zlrvol)


'Reset the sample to respective time period.
smpl 1927:02 1951:04

eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr3_ar2=eq3.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl3

'Reset the sample to respective time period.
smpl 1927:02 1951:04

midaspdl3.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 

'Freeze the model to check the lag coefficients of the midas model for the time period 1927-1951.
freeze(midaspdl3_reg) midaspdl3

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr3_midas=midaspdl3.@r2


'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg3_rsqr=((rsqr3_midas-rsqr3_ar2)/rsqr3_ar2)*100
show chg3_rsqr

'EXTRACT THE F STATS VALUE FOR COMPARING THE MODELS:
scalar fstats3=((rsqr3_midas-rsqr3_ar2)/8)/((1-rsqr3_midas)/(nobs3-18))
show fstats3


                      '''''''TIME PERIOD OF 1952-2010'''''''''''

smpl @all
smpl 1952:02 1985:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq4
scalar nobs4=@obs(zlrvol)


'Reset the sample to respective time period.
smpl 1952:02 1985:04

eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr4_ar2=eq4.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl4

'Reset the sample to respective time period.
smpl 1952:02 1985:04

midaspdl4.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 

'Freeze the model to check the lag coefficients of the midas model for the time period 1927-1951.
freeze(midaspdl4_reg) midaspdl4

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr4_midas=midaspdl4.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg4_rsqr=((rsqr4_midas-rsqr4_ar2)/rsqr4_ar2)*100
show chg4_rsqr

'EXTRACT THE F STATS VALUE FOR COMPARING THE MODELS:
scalar fstats4=((rsqr4_midas-rsqr4_ar2)/8)/((1-rsqr4_midas)/(nobs4-18))
show fstats4


                     '''''''TIME PERIOD OF 1927-1951'''''''''''

smpl @all
smpl 1986:01 2010:04

'Declare the equation for the estimation of the regression for the respective time period.
equation eq5
scalar nobs5=@obs(zlrvol)


'Reset the sample to respective time period.
smpl 1986:01 2010:04

eq5.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr5_ar2=eq5.@r2

'Declare the equation for the estimation of the regression for the respective time period.
equation midaspdl5

'Reset the sample to respective time period.
smpl 1986:01 2010:04

midaspdl5.MIDAS(fixedlag=4, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 

'Freeze the model to check the lag coefficients of the midas model for the time period 1927-1951.
freeze(midaspdl5_reg) midaspdl5

'Extract its rsquare: rsquare of ar(6) to compare it to the augmented model.
scalar rsqr5_midas=midaspdl5.@r2

'Calculate the change in rsquare between the ar(6) model and augmented model.
scalar chg5_rsqr=((rsqr5_midas-rsqr5_ar2)/rsqr5_ar2)*100
show chg5_rsqr

'EXTRACT THE F STATS VALUE FOR COMPARING THE MODELS:
scalar fstats5=((rsqr5_midas-rsqr5_ar2)/8)/((1-rsqr5_midas)/(nobs5-18))
show fstats5


                                          '''''OOS ANALYSIS ON MIDAS REGRESSIONS''''''


smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q2+!i 1947q1+!i    ''This calculation refers to rolling window estimation scheme.
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl1.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 	'Equation MIDAS PDL weighting
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

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
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
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl2.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 	'Equation MIDAS PDL weighting
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

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
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
   eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl3.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3)	'Equation MIDAS PDL weighting
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

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
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
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl4.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 	'Equation MIDAS PDL weighting
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

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4
'COMMENT:.......

''''''NEW CW STATS BASED ON REGRESSIONS''''''''              

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr1=(yhat1-midaspdlfits1)^2

series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

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


series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

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

series CW_stat44=1000*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:  





                   ''''''''''''OOS analysis based on change in rsquare'''''''''''''''


' On this analysis first I have to create historical mean for stdlvol_ff series in order to be able to compute the change in rsquared of oos analysis. 


'Historical average for the 1947:03-2010:12 time period.

smpl @all
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q3 1947q2+!i
    series hm_mean1(79+!i)=@mean(zlrvol)
next

'On the second step we can calculate the MSEP for the Historical average.
smpl 1947:03 2010:04

series hm_mean_dif1=zlrvol-hm_mean1
series hm_msep1_sqr=hm_mean_dif1^2
scalar hm_msep1=@mean(hm_msep1_sqr)

scalar r_oos1=100*((sigma1-sigma2)/hm_msep1)
show r_oos1
'COMMENT:.......


smpl @all
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q3 1972q2+!i
    series hm_mean2(79+!i)=@mean(zlrvol)
next

'On the second step we can calculate the MSEP for the Historical average.
smpl 1972:03 2010:04

series hm_mean_dif2=zlrvol-hm_mean2
series hm_msep2_sqr=hm_mean_dif2^2
scalar hm_msep2=@mean(hm_msep2_sqr)

scalar r_oos2=100*((sigma3-sigma4)/hm_msep2)
show r_oos2
'COMMENT:.......


'Historical average for the 1947:03-2010:12 time period.

smpl @all
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q3 1982q2+!i
    series hm_mean3(79+!i)=@mean(zlrvol)
next

smpl 1982:03 2010:04
'On the second step we can calculate the MSEP for the Historical average.
series hm_mean_dif3=zlrvol-hm_mean3
series hm_msep3_sqr=hm_mean_dif3^2
scalar hm_msep3=@mean(hm_msep3_sqr)

scalar r_oos3=100*((sigma5-sigma6)/hm_msep3)
show r_oos3

'COMMENT: PROBLEMATIC!!!!!


smpl @all
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q3 1972q2+!i
   series hm_mean4(79+!i)=@mean(zlrvol)
    smpl 2001q1 @last
     series hm_mean4=NA
next

'On the second step we can calculate the MSEP for the Historical average.
smpl 1972:03 2000:04

series hm_mean_dif4=zlrvol-hm_mean4
series hm_msep4_sqr=hm_mean_dif4^2
scalar hm_msep4=@mean(hm_msep4_sqr)

scalar  r_oos4=100*((sigma7-sigma8)/hm_msep4)
show r_oos4

'COMMENT:.....

 

             '''''AUGMENTED VS MIDAS_KS-RW WITH STEP WEIGHTING SCHEME'''''


smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q2+!i 1947q1+!i    ''This calculation refers to rolling window estimation scheme.
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl1.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 	'Equation MIDAS PDL weighting
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

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
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
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl2.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 	'Equation MIDAS PDL weighting
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

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
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
   eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl3.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3)	'Equation MIDAS PDL weighting
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

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
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
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl4.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 	'Equation MIDAS PDL weighting
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

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4
'COMMENT:.......


''''''NEW CW STATS BASED ON REGRESSIONS''''''''              

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr1=(yhat1-midaspdlfits1)^2

series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

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


series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

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

series CW_stat44=1000*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:  




                   ''''''''''''OOS analysis based on change in rsquare'''''''''''''''


' On this analysis first I have to create historical mean for stdlvol_ff series in order to be able to compute the change in rsquared of oos analysis. 


'Historical average for the 1947:03-2010:12 time period.

smpl @all
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q3 1947q2+!i
    series hm_mean1(79+!i)=@mean(zlrvol)
next

'On the second step we can calculate the MSEP for the Historical average.
smpl 1947:03 2010:04

series hm_mean_dif1=zlrvol-hm_mean1
series hm_msep1_sqr=hm_mean_dif1^2
scalar hm_msep1=@mean(hm_msep1_sqr)

scalar r_oos1=100*((sigma1-sigma2)/hm_msep1)
show r_oos1
'COMMENT:.......


smpl @all
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q3 1972q2+!i
    series hm_mean2(79+!i)=@mean(zlrvol)
next

'On the second step we can calculate the MSEP for the Historical average.
smpl 1972:03 2010:04

series hm_mean_dif2=zlrvol-hm_mean2
series hm_msep2_sqr=hm_mean_dif2^2
scalar hm_msep2=@mean(hm_msep2_sqr)

scalar r_oos2=100*((sigma3-sigma4)/hm_msep2)
show r_oos2
'COMMENT:.......


'Historical average for the 1947:03-2010:12 time period.

smpl @all
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q3 1982q2+!i
    series hm_mean3(79+!i)=@mean(zlrvol)
next

smpl 1982:03 2010:04
'On the second step we can calculate the MSEP for the Historical average.
series hm_mean_dif3=zlrvol-hm_mean3
series hm_msep3_sqr=hm_mean_dif3^2
scalar hm_msep3=@mean(hm_msep3_sqr)

scalar r_oos3=100*((sigma5-sigma6)/hm_msep3)
show r_oos3

'COMMENT: PROBLEMATIC!!!!!


smpl @all
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q3 1972q2+!i
   series hm_mean4(79+!i)=@mean(zlrvol)
    smpl 2001q1 @last
     series hm_mean4=NA
next

'On the second step we can calculate the MSEP for the Historical average.
smpl 1972:03 2000:04

series hm_mean_dif4=zlrvol-hm_mean4
series hm_msep4_sqr=hm_mean_dif4^2
scalar hm_msep4=@mean(hm_msep4_sqr)

scalar  r_oos4=100*((sigma7-sigma8)/hm_msep4)
show r_oos4

'COMMENT:.....




           ''''RECURSIVE ESTIMATION METHOD-MIDAS REGRESSIONS'''''


smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q3 1947q1+!i    ''This calculation refers to rolling window estimation scheme.
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl1.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 	'Equation MIDAS PDL weighting
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

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
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
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl2.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 	'Equation MIDAS PDL weighting
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

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
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
   eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl3.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3)	'Equation MIDAS PDL weighting
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

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
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
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl4.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 	'Equation MIDAS PDL weighting
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

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4
'COMMENT:.......

''''''NEW CW STATS BASED ON REGRESSIONS''''''''              

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr1=(yhat1-midaspdlfits1)^2

series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

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


series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

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

series CW_stat44=1000*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:  






                   ''''''''''''OOS analysis based on change in rsquare'''''''''''''''


' On this analysis first I have to create historical mean for stdlvol_ff series in order to be able to compute the change in rsquared of oos analysis. 


'Historical average for the 1947:03-2010:12 time period.

smpl @all
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q3 1947q2+!i
    series hm_mean1(79+!i)=@mean(zlrvol)
next

'On the second step we can calculate the MSEP for the Historical average.
smpl 1947:03 2010:04

series hm_mean_dif1=zlrvol-hm_mean1
series hm_msep1_sqr=hm_mean_dif1^2
scalar hm_msep1=@mean(hm_msep1_sqr)

scalar r_oos1=100*((sigma1-sigma2)/hm_msep1)
show r_oos1
'COMMENT:.......


smpl @all
' move sample !step obs at a time
for !i = 1  to 154
   ' set sample to estimation period
   smpl 1952q3 1972q2+!i
    series hm_mean2(79+!i)=@mean(zlrvol)
next

'On the second step we can calculate the MSEP for the Historical average.
smpl 1972:03 2010:04

series hm_mean_dif2=zlrvol-hm_mean2
series hm_msep2_sqr=hm_mean_dif2^2
scalar hm_msep2=@mean(hm_msep2_sqr)

scalar r_oos2=100*((sigma3-sigma4)/hm_msep2)
show r_oos2
'COMMENT:.......


'Historical average for the 1947:03-2010:12 time period.

smpl @all
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1962q3 1982q2+!i
    series hm_mean3(79+!i)=@mean(zlrvol)
next

smpl 1982:03 2010:04
'On the second step we can calculate the MSEP for the Historical average.
series hm_mean_dif3=zlrvol-hm_mean3
series hm_msep3_sqr=hm_mean_dif3^2
scalar hm_msep3=@mean(hm_msep3_sqr)

scalar r_oos3=100*((sigma5-sigma6)/hm_msep3)
show r_oos3

'COMMENT: PROBLEMATIC!!!!!


smpl @all
' move sample !step obs at a time
for !i = 1  to 114
   ' set sample to estimation period
   smpl 1952q3 1972q2+!i
   series hm_mean4(79+!i)=@mean(zlrvol)
    smpl 2001q1 @last
     series hm_mean4=NA
next

'On the second step we can calculate the MSEP for the Historical average.
smpl 1972:03 2000:04

series hm_mean_dif4=zlrvol-hm_mean4
series hm_msep4_sqr=hm_mean_dif4^2
scalar hm_msep4=@mean(hm_msep4_sqr)

scalar  r_oos4=100*((sigma7-sigma8)/hm_msep4)
show r_oos4

'COMMENT:.....

''''''''END OF TRIALS''''''''''''



         '''''''''ROBUSTNESS CHECKS-STEP WEIGHTING SCHEME'''''''''


smpl @all
' declare equation for estimation
equation eq1
 
' declare series for final results
series yhat1               ' point estimates
 
' move sample !step obs at a time
for !i = 1  to 254
   ' set sample to estimation period
   smpl 1927q3 1947q1+!i    ''This calculation refers to rolling window estimation scheme.
   ' estimate equation
   eq1.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl1.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 	'Equation MIDAS PDL weighting
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

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
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
   eq2.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl2.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 	'Equation MIDAS PDL weighting
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

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
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
   eq3.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl3.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3)	'Equation MIDAS PDL weighting
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

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
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
   eq4.ls(cov=hac) zlrvol zlrvol(-1) zlrvol(-2) z_dfy(-1) z_dfr(-1)   z_ipvol(-1) z_ppivol(-1) z_tms(-1) z_npy(-1) z_cp(-1) z_exret(-1)
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
  midaspdl4.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m_oos\z_ppivol(-3) ks_m_oos\z_ipvol(-3) ks_m_oos\z_tms(-3) ks_m_oos\z_cp(-3) ks_m_oos\z_dfr(-3) ks_m_oos\z_dfy(-3) ks_m_oos\z_npy(-3) ks_m_oos\z_exret(-3) 	'Equation MIDAS PDL weighting
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

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4
'COMMENT:.......

''''''NEW CW STATS BASED ON REGRESSIONS''''''''              

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-yhat1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-midaspdlfits1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr1=(yhat1-midaspdlfits1)^2

series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

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


series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

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

series CW_stat44=1000*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:  








         '''''FORECAST COMBINATION MONTHLY SERIES OOS ANALYSIS'''''''


'''''COMBINED FORECASTS OF INDIVIDUAL FORECASTS''''''''''''''



smpl @all
rename yhat2 cp1
rename yhat4 cp2
rename yhat6 cp3
rename yhat8 cp4

smpl @all
rename yhat2 dfr1
rename yhat4 dfr2
rename yhat6 dfr3
rename yhat8 dfr4

smpl @all
rename yhat2 dfy1
rename yhat4 dfy2
rename yhat6 dfy3
rename yhat8 dfy4


smpl @all
rename yhat2 exret1
rename yhat4 exret2
rename yhat6 exret3
rename yhat8 exret4




smpl @all
rename yhat2 ip1
rename yhat4 ip2
rename yhat6 ip3
rename yhat8 ip4


smpl @all
rename yhat2 ipvol1
rename yhat4 ipvol2
rename yhat6 ipvol3
rename yhat8 ipvol4


smpl @all
rename yhat2 npy1
rename yhat4 npy2
rename yhat6 npy3
rename yhat8 npy4


smpl @all
rename yhat2 ppivol1
rename yhat4 ppivol2
rename yhat6 ppivol3
rename yhat8 ppivol4


smpl @all
rename yhat2 tms1
rename yhat4 tms2
rename yhat6 tms3
rename yhat8 tms4

smpl @all
rename yhat1 ar6_1
rename yhat3 ar6_2
rename yhat5 ar6_3
rename yhat7 ar6_4


'''''COMBINED FORECASTS-SIMPLE MEAN''''


smpl 1947:03 2010:12
series sigma1_sqr=(zlrvol-ar6_1)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar6_1-zlrvol_f1)^2


series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1






smpl @all
smpl 1972:03 2010:12

series sigma3_sqr=(zlrvol-ar6_2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar6_2-zlrvol_f2)^2

series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......




smpl @all

smpl 1982:03 2010:12
series sigma5_sqr=(zlrvol-ar6_3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar6_3-zlrvol_f3)^2


series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 




smpl @all

smpl 1972:03 2000:12
series sigma7_sqr=(zlrvol-ar6_4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(ar6_4-zlrvol_f4)^2


series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:        



   ''''OOS MONTHLY ANALSIS BASED ON DELTA RSQUARE''''''


smpl 1947:03 2010:12
series sigma1_sqr=(zlrvol-ar6_1)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:12

series sigma3_sqr=(zlrvol-ar6_2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:12
series sigma5_sqr=(zlrvol-ar6_3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:12
series sigma5_sqr=(zlrvol-ar6_4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment:





            ''''''COMBINED FORECASTS-SIMPLE MEDIAN'''''



smpl 1947:03 2010:12
series sigma1_sqr=(zlrvol-ar6_1)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar6_1-zlrvol_f1)^2


series CW_stat11=100*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1






smpl @all
smpl 1972:03 2010:12

series sigma3_sqr=(zlrvol-ar6_2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar6_2-zlrvol_f2)^2

series CW_stat22=100*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......




smpl @all

smpl 1982:03 2010:12
series sigma5_sqr=(zlrvol-ar6_3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar6_3-zlrvol_f3)^2


series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 




smpl @all

smpl 1972:03 2000:12
series sigma7_sqr=(zlrvol-ar6_4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(ar6_4-zlrvol_f4)^2


series CW_stat44=100*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:   

                    


                        ''''OOS MONTHLY ANALSIS BASED ON DELTA RSQUARE''''''


smpl 1947:03 2010:12
series sigma1_sqr=(zlrvol-ar6_1)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 

series gw_stat11=100*(sigma1_sqr-sigma2_sqr)
equation gw_stat1

gw_stat1.ls(cov=hac) gw_stat11 c
show gw_stat1

'COMMENT:.......


smpl 1972:03 2010:12

series sigma3_sqr=(zlrvol-ar6_2)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat22=100*(sigma3_sqr-sigma4_sqr)

equation gw_stat2

gw_stat2.ls(cov=hac) gw_stat22 c
show gw_stat2

'COMMENT:.......


smpl 1982:03 2010:12
series sigma5_sqr=(zlrvol-ar6_3)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3)^2
 

series gw_stat33=100*(sigma5_sqr-sigma6_sqr)
equation gw_stat3

gw_stat3.ls(cov=hac) gw_stat33 c
show gw_stat3

'Comment: 

smpl 1972:03 2000:12
series sigma5_sqr=(zlrvol-ar6_4)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 


series gw_stat44=100*(sigma7_sqr-sigma8_sqr)
equation gw_stat4

gw_stat4.ls(cov=hac) gw_stat44 c
show gw_stat4

'Comment:
