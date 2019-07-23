




                              '''''KITCHEN SINK FORECAST COMBINATION''''''


               '''''COMBINED FORECASTS OF INDIVIDUAL FORECASTS''''''''''''''




'Forecasting variable cp
smpl @all
rename midaspdlfits1 cp1
rename midaspdlfits2 cp2
rename midaspdlfits3 cp3
rename midaspdlfits4 cp4

smpl @all
rename yhat1 aug_cp_1
rename yhat2 aug_cp_2
rename yhat3 aug_cp_3
rename yhat4 aug_cp_4

'Forecasting variable dfr

smpl @all
rename midaspdlfits1 dfr1
rename midaspdlfits2 dfr2
rename midaspdlfits3 dfr3
rename midaspdlfits4 dfr4

smpl @all
rename yhat1 aug_dfr_1
rename yhat2 aug_dfr_2
rename yhat3 aug_dfr_3
rename yhat4 aug_dfr_4

'Forecasting variable dfy
smpl @all
rename midaspdlfits1 dfy1
rename midaspdlfits2 dfy2
rename midaspdlfits3 dfy3
rename midaspdlfits4 dfy4

smpl @all
rename yhat1 aug_dfy_1
rename yhat2 aug_dfy_2
rename yhat3 aug_dfy_3
rename yhat4 aug_dfy_4

'Forecasting variable exret

smpl @all
rename midaspdlfits1 exret1
rename midaspdlfits2 exret2
rename midaspdlfits3 exret3
rename midaspdlfits4 exret4

smpl @all
rename yhat1 aug_exret_1
rename yhat2 aug_exret_2
rename yhat3 aug_exret_3
rename yhat4 aug_exret_4


'Forecasting variable ipvol

smpl @all
rename midaspdlfits1 ipvol1
rename midaspdlfits2 ipvol2
rename midaspdlfits3 ipvol3
rename midaspdlfits4 ipvol4

smpl @all
rename yhat1 aug_ipvol_1
rename yhat2 aug_ipvol_2
rename yhat3 aug_ipvol_3
rename yhat4 aug_ipvol_4

'Forecasting variable npy

smpl @all
rename midaspdlfits1 npy1
rename midaspdlfits2 npy2
rename midaspdlfits3 npy3
rename midaspdlfits4 npy4

smpl @all
rename yhat1 aug_npy_1
rename yhat2 aug_npy_2
rename yhat3 aug_npy_3
rename yhat4 aug_npy_4


'Forecasting variable ppivol
smpl @all
rename midaspdlfits1 ppivol1
rename midaspdlfits2 ppivol2
rename midaspdlfits3 ppivol3
rename midaspdlfits4 ppivol4

smpl @all
rename yhat1 aug_ppivol_1
rename yhat2 aug_ppivol_2
rename yhat3 aug_ppivol_3
rename yhat4 aug_ppivol_4


'Forecasting variable tms
smpl @all
rename midaspdlfits1 tms1
rename midaspdlfits2 tms2
rename midaspdlfits3 tms3
rename midaspdlfits4 tms4

smpl @all
rename yhat1 aug_tms_1
rename yhat2 aug_tms_2
rename yhat3 aug_tms_3
rename yhat4 aug_tms_4

'''''COMBINED FORECASTS-SIMPLE MEAN''''

aug_cp_1 aug_dfr_1 aug_dfy_1 aug_exret_1 aug_ipvol_1 aug_npy_1 aug_ppivol_1 aug_tms_1

aug_cp_2 aug_dfr_2 aug_dfy_2 aug_exret_2 aug_ipvol_2 aug_npy_2 aug_ppivol_2 aug_tms_2

aug_cp_3 aug_dfr_3 aug_dfy_3 aug_exret_3 aug_ipvol_3 aug_npy_3 aug_ppivol_3 aug_tms_3

aug_cp_4 aug_dfr_4 aug_dfy_4 aug_exret_4 aug_ipvol_4 aug_npy_4 aug_ppivol_4 aug_tms_4

cp1 dfr1 dfy1 exret1 ipvol1 npy1 ppivol1 tms1 
cp2 dfr2 dfy2 exret2 ipvol2 npy2 ppivol2 tms2
cp3 dfr3 dfy3 exret3 ipvol3 npy3 ppivol3 tms3
cp4 dfr4 dfy4 exret4 ipvol4 npy4 ppivol4 tms4


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-zlrvol_f1_aug)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(zlrvol_f1_aug-zlrvol_f1_midas)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
show cw_stat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-zlrvol_f2_aug)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(zlrvol_f2_aug-zlrvol_f2_midas)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
show cw_stat2




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-zlrvol_f3_aug)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(zlrvol_f3_aug-zlrvol_f3_midas)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
show cw_stat3




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-zlrvol_f4_aug)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr4=(zlrvol_f4_aug-zlrvol_f4_midas)^2
scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4





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









                   '''''COMBINED FORECASTS-SIMPLE MEDIAN''''

aug_cp_1 aug_dfr_1 aug_dfy_1 aug_exret_1 aug_ipvol_1 aug_npy_1 aug_ppivol_1 aug_tms_1

aug_cp_2 aug_dfr_2 aug_dfy_2 aug_exret_2 aug_ipvol_2 aug_npy_2 aug_ppivol_2 aug_tms_2

aug_cp_3 aug_dfr_3 aug_dfy_3 aug_exret_3 aug_ipvol_3 aug_npy_3 aug_ppivol_3 aug_tms_3

aug_cp_4 aug_dfr_4 aug_dfy_4 aug_exret_4 aug_ipvol_4 aug_npy_4 aug_ppivol_4 aug_tms_4

cp1 dfr1 dfy1 exret1 ipvol1 npy1 ppivol1 tms1 
cp2 dfr2 dfy2 exret2 ipvol2 npy2 ppivol2 tms2
cp3 dfr3 dfy3 exret3 ipvol3 npy3 ppivol3 tms3
cp4 dfr4 dfy4 exret4 ipvol4 npy4 ppivol4 tms4


smpl @all
smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-zlrvol_f1_aug)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(zlrvol_f1_aug-zlrvol_f1_midas)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
show cw_stat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-zlrvol_f2_aug)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(zlrvol_f2_aug-zlrvol_f2_midas)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
show cw_stat2




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-zlrvol_f3_aug)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(zlrvol_f3_aug-zlrvol_f3_midas)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
show cw_stat3




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-zlrvol_f4_aug)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr4=(zlrvol_f4_aug-zlrvol_f4_midas)^2
scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4




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






             '''''COMB FORECASTS-R.W STEP WEIGHTING SCHEME''''''



'''''COMBINED FORECASTS-SIMPLE MEAN''''

aug_cp_1 aug_dfr_1 aug_dfy_1 aug_exret_1 aug_ipvol_1 aug_npy_1 aug_ppivol_1 aug_tms_1

aug_cp_2 aug_dfr_2 aug_dfy_2 aug_exret_2 aug_ipvol_2 aug_npy_2 aug_ppivol_2 aug_tms_2

aug_cp_3 aug_dfr_3 aug_dfy_3 aug_exret_3 aug_ipvol_3 aug_npy_3 aug_ppivol_3 aug_tms_3

aug_cp_4 aug_dfr_4 aug_dfy_4 aug_exret_4 aug_ipvol_4 aug_npy_4 aug_ppivol_4 aug_tms_4

cp1 dfr1 dfy1 exret1 ipvol1 npy1 ppivol1 tms1 
cp2 dfr2 dfy2 exret2 ipvol2 npy2 ppivol2 tms2
cp3 dfr3 dfy3 exret3 ipvol3 npy3 ppivol3 tms3
cp4 dfr4 dfy4 exret4 ipvol4 npy4 ppivol4 tms4


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-zlrvol_f1_aug)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(zlrvol_f1_aug-zlrvol_f1_midas)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
show cw_stat1




smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-zlrvol_f2_aug)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(zlrvol_f2_aug-zlrvol_f2_midas)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
show cw_stat2




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-zlrvol_f3_aug)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(zlrvol_f3_aug-zlrvol_f3_midas)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
show cw_stat3




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-zlrvol_f4_aug)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr4=(zlrvol_f4_aug-zlrvol_f4_midas)^2
scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4





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









                   '''''COMBINED FORECASTS-SIMPLE MEDIAN''''

aug_cp_1 aug_dfr_1 aug_dfy_1 aug_exret_1 aug_ipvol_1 aug_npy_1 aug_ppivol_1 aug_tms_1

aug_cp_2 aug_dfr_2 aug_dfy_2 aug_exret_2 aug_ipvol_2 aug_npy_2 aug_ppivol_2 aug_tms_2

aug_cp_3 aug_dfr_3 aug_dfy_3 aug_exret_3 aug_ipvol_3 aug_npy_3 aug_ppivol_3 aug_tms_3

aug_cp_4 aug_dfr_4 aug_dfy_4 aug_exret_4 aug_ipvol_4 aug_npy_4 aug_ppivol_4 aug_tms_4

cp1 dfr1 dfy1 exret1 ipvol1 npy1 ppivol1 tms1 
cp2 dfr2 dfy2 exret2 ipvol2 npy2 ppivol2 tms2
cp3 dfr3 dfy3 exret3 ipvol3 npy3 ppivol3 tms3
cp4 dfr4 dfy4 exret4 ipvol4 npy4 ppivol4 tms4


smpl @all
smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-zlrvol_f1_aug)^2
scalar sigma1=@mean(sigma1_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2
scalar sigma2=@mean(sigma2_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(zlrvol_f1_aug-zlrvol_f1_midas)^2
 scalar adj_term1=@mean(adj_term_sqr1)

scalar CW_stat1=1000*((sigma1-sigma2)+adj_term1)
show cw_stat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-zlrvol_f2_aug)^2
scalar sigma3=@mean(sigma3_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2
scalar sigma4=@mean(sigma4_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(zlrvol_f2_aug-zlrvol_f2_midas)^2
 scalar adj_term2=@mean(adj_term_sqr2)

scalar CW_stat2=1000*((sigma3-sigma4)+adj_term2)
show cw_stat2




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-zlrvol_f3_aug)^2
scalar sigma5=@mean(sigma5_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2
scalar sigma6=@mean(sigma6_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(zlrvol_f3_aug-zlrvol_f3_midas)^2
 scalar adj_term3=@mean(adj_term_sqr3)

scalar CW_stat3=1000*((sigma5-sigma6)+adj_term3)
show cw_stat3




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-zlrvol_f4_aug)^2
scalar sigma7=@mean(sigma7_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2
scalar sigma8=@mean(sigma8_sqr)
' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series adj_term_sqr4=(zlrvol_f4_aug-zlrvol_f4_midas)^2
scalar adj_term4=@mean(adj_term_sqr4)

scalar CW_stat4=1000*((sigma7-sigma8)+adj_term4)
show cw_stat4




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




     '''''COMBINED FORECASTS-ROLLING WINDOWS MEAN-POLYNOMIAL WEIGHTING SCHEME'''''''

'''''COMBINED FORECASTS-SIMPLE MEAN''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-zlrvol_f1_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(zlrvol_f1_aug-zlrvol_f1_midas)^2


series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-zlrvol_f2_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(zlrvol_f2_aug-zlrvol_f2_midas)^2

series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-zlrvol_f3_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(zlrvol_f3_aug-zlrvol_f3_midas)^2


series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-zlrvol_f4_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(zlrvol_f4_aug-zlrvol_f4_midas)^2


series CW_stat44=1000*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:   

                   
  '''''COMBINED FORECASTS-ROLLING WINDOWS MEAN-POLYNOMIAL WEIGHTING SCHEME'''''''

         '''''COMBINED FORECASTS-SIMPLE MEDIAN''''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-zlrvol_f1_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(zlrvol_f1_aug-zlrvol_f1_midas)^2


series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-zlrvol_f2_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(zlrvol_f2_aug-zlrvol_f2_midas)^2

series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-zlrvol_f3_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(zlrvol_f3_aug-zlrvol_f3_midas)^2


series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-zlrvol_f4_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(zlrvol_f4_aug-zlrvol_f4_midas)^2


series CW_stat44=1000*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:   

                   


     '''''COMBINED FORECASTS-ROLLING WINDOWS MEAN-STEP WEIGHTING SCHEME'''''''

'''''COMBINED FORECASTS-SIMPLE MEAN''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-zlrvol_f1_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(zlrvol_f1_aug-zlrvol_f1_midas)^2


series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-zlrvol_f2_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(zlrvol_f2_aug-zlrvol_f2_midas)^2

series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-zlrvol_f3_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(zlrvol_f3_aug-zlrvol_f3_midas)^2


series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-zlrvol_f4_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(zlrvol_f4_aug-zlrvol_f4_midas)^2


series CW_stat44=1000*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:   

                   
  '''''COMBINED FORECASTS-ROLLING WINDOWS -STEP WEIGHTING SCHEME'''''''

         '''''COMBINED FORECASTS-SIMPLE MEDIAN''''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-zlrvol_f1_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(zlrvol_f1_aug-zlrvol_f1_midas)^2


series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-zlrvol_f2_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(zlrvol_f2_aug-zlrvol_f2_midas)^2

series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-zlrvol_f3_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(zlrvol_f3_aug-zlrvol_f3_midas)^2


series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-zlrvol_f4_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(zlrvol_f4_aug-zlrvol_f4_midas)^2


series CW_stat44=1000*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:   

                          

  

 

        '''''COMBINED FORECASTS-RECURSIVE ESTIMATION-POLYNOMIAL WEIGHTING SCHEME'''''''

                                  '''''COMBINED FORECASTS-SIMPLE MEAN''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-zlrvol_f1_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(zlrvol_f1_aug-zlrvol_f1_midas)^2


series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-zlrvol_f2_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(zlrvol_f2_aug-zlrvol_f2_midas)^2

series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-zlrvol_f3_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(zlrvol_f3_aug-zlrvol_f3_midas)^2


series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-zlrvol_f4_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(zlrvol_f4_aug-zlrvol_f4_midas)^2


series CW_stat44=1000*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:   

                   
  '''''COMBINED FORECASTS-RECURSIVE ESTIMATION-POLYNOMIAL WEIGHTING SCHEME'''''''

         '''''COMBINED FORECASTS-SIMPLE MEDIAN''''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-zlrvol_f1_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(zlrvol_f1_aug-zlrvol_f1_midas)^2


series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-zlrvol_f2_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(zlrvol_f2_aug-zlrvol_f2_midas)^2

series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-zlrvol_f3_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(zlrvol_f3_aug-zlrvol_f3_midas)^2


series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-zlrvol_f4_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(zlrvol_f4_aug-zlrvol_f4_midas)^2


series CW_stat44=1000*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:   

                   


     '''''COMBINED FORECASTS-RECURSIVE ESTIMATION-STEP WEIGHTING SCHEME'''''''

'''''COMBINED FORECASTS-SIMPLE MEAN''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-zlrvol_f1_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(zlrvol_f1_aug-zlrvol_f1_midas)^2


series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-zlrvol_f2_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(zlrvol_f2_aug-zlrvol_f2_midas)^2

series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-zlrvol_f3_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(zlrvol_f3_aug-zlrvol_f3_midas)^2


series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-zlrvol_f4_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(zlrvol_f4_aug-zlrvol_f4_midas)^2


series CW_stat44=1000*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:   

                   
  '''''COMBINED FORECASTS-RECURSIVE ESTIMATION -STEP WEIGHTING SCHEME'''''''

         '''''COMBINED FORECASTS-SIMPLE MEDIAN''''''


smpl 1947:03 2010:04
series sigma1_sqr=(zlrvol-zlrvol_f1_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma2_sqr=(zlrvol-zlrvol_f1_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr1=(zlrvol_f1_aug-zlrvol_f1_midas)^2


series CW_stat11=1000*((sigma1_sqr-sigma2_sqr)+adj_term_sqr1)

equation cwstat1

cwstat1.ls(cov=hac) cw_stat11 c
show cwstat1






smpl @all
smpl 1972:03 2010:04

series sigma3_sqr=(zlrvol-zlrvol_f2_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma4_sqr=(zlrvol-zlrvol_f2_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr2=(zlrvol_f2_aug-zlrvol_f2_midas)^2

series CW_stat22=1000*((sigma3_sqr-sigma4_sqr)+adj_term_sqr2)

equation cwstat2

cwstat2.ls(cov=hac) cw_stat22 c
show cwstat2
'COMMENT:.......




smpl @all

smpl 1982:03 2010:04
series sigma5_sqr=(zlrvol-zlrvol_f3_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma6_sqr=(zlrvol-zlrvol_f3_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr3=(zlrvol_f3_aug-zlrvol_f3_midas)^2


series CW_stat33=100*((sigma5_sqr-sigma6_sqr)+adj_term_sqr3)
equation cwstat3

cwstat3.ls(cov=hac) cw_stat33 c
show cwstat3

'Comment: 




smpl @all

smpl 1972:03 2000:04
series sigma7_sqr=(zlrvol-zlrvol_f4_aug)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
series sigma8_sqr=(zlrvol-zlrvol_f4_midas)^2

' Instead of dividing the MSPE with the corresponding p obs.s I calculated the mean of the whole expression. 
 series adj_term_sqr4=(zlrvol_f4_aug-zlrvol_f4_midas)^2


series CW_stat44=1000*((sigma7_sqr-sigma8_sqr)+adj_term_sqr4)
equation cwstat4

cwstat4.ls(cov=hac) cw_stat44 c
show cwstat4

'Comment:   

                         



          
