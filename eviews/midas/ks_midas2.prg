



               ''''''MIDAS REGRESSIONS TO COMPARE TO AR(2) TYPE MODELS''''''''


'1) First I will compare the two models by exploiting the rolling windows estimation procedure.

'2) In the second step I will compare the two models by exploiting the recursive estimation procedure.

'3) Afterwards to check the robustness of my main results I will conduct the various weighting schemes: polynomial and step weighting scheme.



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
  midaspdl1.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3) 	'Equation MIDAS PDL weighting
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
  midaspdl2.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3) 	'Equation MIDAS PDL weighting
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
  midaspdl3.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3)	'Equation MIDAS PDL weighting
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
  midaspdl4.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3) 	'Equation MIDAS PDL weighting
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

          ''''''NEW CW STATS BASED ON REGRESSIONS''''''''              

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




      '''''STEP WEIGHTING SCHEME-R.W. ESTIMATION PROCEDURE''''


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
  midaspdl1.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3) 	'Equation MIDAS PDL weighting
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
  midaspdl2.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3) 	'Equation MIDAS PDL weighting
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
  midaspdl3.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3)	'Equation MIDAS PDL weighting
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
  midaspdl4.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3) 	'Equation MIDAS PDL weighting
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

          ''''''NEW CW STATS BASED ON REGRESSIONS''''''''              

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




           ''''RECURSIVE ESTIMATION-POLYNOMIAL WEIGHTING SCHEME''''



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
  midaspdl1.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3) 	'Equation MIDAS PDL weighting
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
  midaspdl2.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3) 	'Equation MIDAS PDL weighting
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
  midaspdl3.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3)	'Equation MIDAS PDL weighting
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
  midaspdl4.MIDAS(fixedlag=9, polynomial=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3) 	'Equation MIDAS PDL weighting
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


          ''''''NEW CW STATS BASED ON REGRESSIONS''''''''              

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



      '''''''''RECURSIVE ESTIMATION-STEP WEIGHTING SCHEME'''''''''


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
  midaspdl1.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3) 	'Equation MIDAS PDL weighting
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
  midaspdl2.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3) 	'Equation MIDAS PDL weighting
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
  midaspdl3.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3)	'Equation MIDAS PDL weighting
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
  midaspdl4.MIDAS(fixedlag=9, midwgt=step, steps=2) zlrvol zlrvol(-1) zlrvol(-2) @ ks_m\z_ppivol(-3) ks_m\z_ipvol(-3) ks_m\z_tms(-3) ks_m\z_cp(-3) ks_m\z_dfr(-3) ks_m\z_dfy(-3) ks_m\z_npy(-3) ks_m\z_exret(-3) 	'Equation MIDAS PDL weighting
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



          ''''''NEW CW STATS BASED ON REGRESSIONS''''''''              

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



                '''''FORECAST COMBINATIONS-R.W2. Mean'''


smpl @all


''Forecast averaging.
cp1 dfr1 dfy1 exret1 ipvol1 npy1 ppivol1 tms1 
cp2 dfr2 dfy2 exret2 ipvol2 npy2 ppivol2 tms2
cp3 dfr3 dfy3 exret3 ipvol3 npy3 ppivol3 tms3
cp4 dfr4 dfy4 exret4 ipvol4 npy4 ppivol4 tms4




smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
show cw_stat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
show cw_stat2




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
show cw_stat3




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4

    
                           ''''''NEW CW STATS BASED ON REGRESSIONS''''''''              


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2

series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2


series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
 

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

'COMMENT: PROBLEMATIC DELTA RSQUARE!!!!!!!!!!!!!!!!









                     '''''FORECAST COMBINATIONS-R.W2. Median'''

smpl @all
''Forecast averaging.
cp1 dfr1 dfy1 exret1 ipvol1 npy1 ppivol1 tms1 
cp2 dfr2 dfy2 exret2 ipvol2 npy2 ppivol2 tms2
cp3 dfr3 dfy3 exret3 ipvol3 npy3 ppivol3 tms3
cp4 dfr4 dfy4 exret4 ipvol4 npy4 ppivol4 tms4




smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
show cw_stat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
show cw_stat2




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
show cw_stat3




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4

    
                           ''''''NEW CW STATS BASED ON REGRESSIONS''''''''              


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2

series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2


series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
 

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

'COMMENT: PROBLEMATIC DELTA RSQUARE!!!!!!!!!!!!!!!!




                '''''FORECAST COMBINATIONS-R.W2. Mean-step weighting scheme'''

'Forecasting variable cp
smpl @all
rename midaspdlfits1 cp1
rename midaspdlfits2 cp2
rename midaspdlfits3 cp3
rename midaspdlfits4 cp4

'Forecasting variable dfr

smpl @all
rename midaspdlfits1 dfr1
rename midaspdlfits2 dfr2
rename midaspdlfits3 dfr3
rename midaspdlfits4 dfr4

'Forecasting variable dfy
smpl @all
rename midaspdlfits1 dfy1
rename midaspdlfits2 dfy2
rename midaspdlfits3 dfy3
rename midaspdlfits4 dfy4

'Forecasting variable exret

smpl @all
rename midaspdlfits1 exret1
rename midaspdlfits2 exret2
rename midaspdlfits3 exret3
rename midaspdlfits4 exret4


'Forecasting variable ipvol

smpl @all
rename midaspdlfits1 ipvol1
rename midaspdlfits2 ipvol2
rename midaspdlfits3 ipvol3
rename midaspdlfits4 ipvol4

'Forecasting variable npy

smpl @all
rename midaspdlfits1 npy1
rename midaspdlfits2 npy2
rename midaspdlfits3 npy3
rename midaspdlfits4 npy4


'Forecasting variable ppivol
smpl @all
rename midaspdlfits1 ppivol1
rename midaspdlfits2 ppivol2
rename midaspdlfits3 ppivol3
rename midaspdlfits4 ppivol4

'Forecasting variable tms
smpl @all
rename midaspdlfits1 tms1
rename midaspdlfits2 tms2
rename midaspdlfits3 tms3
rename midaspdlfits4 tms4



smpl @all

''Forecast averaging.
cp1 dfr1 dfy1 exret1 ipvol1 npy1 ppivol1 tms1 
cp2 dfr2 dfy2 exret2 ipvol2 npy2 ppivol2 tms2
cp3 dfr3 dfy3 exret3 ipvol3 npy3 ppivol3 tms3
cp4 dfr4 dfy4 exret4 ipvol4 npy4 ppivol4 tms4

smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
show cw_stat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
show cw_stat2




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
show cw_stat3




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4

                           ''''''NEW CW STATS BASED ON REGRESSIONS''''''''              


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2

series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2


series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
 

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

'COMMENT: PROBLEMATIC DELTA RSQUARE!!!!!!!!!!!!!!!!        




              '''''FORECAST COMBINATIONS-R.W2. Median-step weighting scheme'''

smpl @all

''Forecast averaging.
cp1 dfr1 dfy1 exret1 ipvol1 npy1 ppivol1 tms1 
cp2 dfr2 dfy2 exret2 ipvol2 npy2 ppivol2 tms2
cp3 dfr3 dfy3 exret3 ipvol3 npy3 ppivol3 tms3
cp4 dfr4 dfy4 exret4 ipvol4 npy4 ppivol4 tms4




smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
show cw_stat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
show cw_stat2




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
show cw_stat3




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4


     ''''''NEW CW STATS BASED ON REGRESSIONS''''''''              


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2

series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2


series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
 

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

'COMMENT: PROBLEMATIC DELTA RSQUARE!!!!!!!!!!!!!!!!        




                    '''''FORECAST COMBINATIONS-R.E. Mean'''

smpl @all


''Forecast averaging.
cp1 dfr1 dfy1 exret1 ipvol1 npy1 ppivol1 tms1 
cp2 dfr2 dfy2 exret2 ipvol2 npy2 ppivol2 tms2
cp3 dfr3 dfy3 exret3 ipvol3 npy3 ppivol3 tms3
cp4 dfr4 dfy4 exret4 ipvol4 npy4 ppivol4 tms4




smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
show cw_stat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
show cw_stat2




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
show cw_stat3




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4


     ''''''NEW CW STATS BASED ON REGRESSIONS''''''''              


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2

series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2


series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
 

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

'COMMENT: PROBLEMATIC DELTA RSQUARE!!!!!!!!!!!!!!!!        




                     '''''FORECAST COMBINATIONS-R.E. Median'''

smpl @all
''Forecast averaging.
cp1 dfr1 dfy1 exret1 ipvol1 npy1 ppivol1 tms1 
cp2 dfr2 dfy2 exret2 ipvol2 npy2 ppivol2 tms2
cp3 dfr3 dfy3 exret3 ipvol3 npy3 ppivol3 tms3
cp4 dfr4 dfy4 exret4 ipvol4 npy4 ppivol4 tms4




smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
show cw_stat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
show cw_stat2




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
show cw_stat3




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4


     ''''''NEW CW STATS BASED ON REGRESSIONS''''''''              


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2

series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2


series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
 

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

'COMMENT: PROBLEMATIC DELTA RSQUARE!!!!!!!!!!!!!!!!        








                '''''FORECAST COMBINATIONS-R.E. Mean-step weighting scheme'''

smpl @all
''Forecast averaging.
cp1 dfr1 dfy1 exret1 ipvol1 npy1 ppivol1 tms1 
cp2 dfr2 dfy2 exret2 ipvol2 npy2 ppivol2 tms2
cp3 dfr3 dfy3 exret3 ipvol3 npy3 ppivol3 tms3
cp4 dfr4 dfy4 exret4 ipvol4 npy4 ppivol4 tms4




smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
show cw_stat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
show cw_stat2




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
show cw_stat3




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4

     ''''''NEW CW STATS BASED ON REGRESSIONS''''''''              


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2

series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2


series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
 

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

'COMMENT: PROBLEMATIC DELTA RSQUARE!!!!!!!!!!!!!!!!               




       
           '''''FORECAST COMBINATIONS-R.E. MEDIAN-step weighting scheme'''''


smpl @all

''Forecast averaging.
cp1 dfr1 dfy1 exret1 ipvol1 npy1 ppivol1 tms1 
cp2 dfr2 dfy2 exret2 ipvol2 npy2 ppivol2 tms2
cp3 dfr3 dfy3 exret3 ipvol3 npy3 ppivol3 tms3
cp4 dfr4 dfy4 exret4 ipvol4 npy4 ppivol4 tms4




smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
show cw_stat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
show cw_stat2




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
show cw_stat3




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4

     ''''''NEW CW STATS BASED ON REGRESSIONS''''''''              


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-ar2_1)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(ar2_1-zlrvol_f1_midas)^2

series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1

'COMMENT:.......


smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-ar2_2)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(ar2_2-zlrvol_f2_midas)^2


series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......


smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-ar2_3)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(ar2_3-zlrvol_f3_midas)^2
 

series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-ar2_4)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(ar2_4-zlrvol_f4_midas)^2
 

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

'COMMENT: PROBLEMATIC DELTA RSQUARE!!!!!!!!!!!!!!!!


