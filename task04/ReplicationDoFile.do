

*** Replication do file ***
*** March 2015
*** Journal of Conflict Resolution
*** The Beheading of Criminal Organizations and the Dynamics of Violence in Mexico ***
*** Calder—n: Banco de Mxico, Stanford University (email: gabriela.calderon@banxico.org.mx); 
*** Robles: Stanford University (email: grobles@stanford.edu); 
*** D’az-Cayeros: Stanford University (email: albertod@stanford.edu); 
*** Magaloni: Stanford University (email: magaloni@stanford.edu). 


*** March 2015 ***
clear all
set more off

* log using ".../Log Replication Materials.log"
* use ".../replication.data.beheadings.march.15.dta", clear
cd "/Users/guspeiro/Documents/Poverty Governance Lab/Paper Beheadings/Paper beheadings 2013/regresiones base publica/Replication Materials"
* cd "/afs/ir/users/g/r/grobles/Documents/Paper beheadings 2013/Replication materials"

log using "Log Replication Materials.log", replace
use "replication.data.beheadings.march.15.dta", clear
tsset idunico date
set matsize 11000


***************
*** Table 1 ***
***************
*** We excluded Ciudad Juarez from the sample
*** Summary statistics

**************************
**CONTROL MUNICIPALITIES**
**************************
count if aux_control==1 & year==2006 & month==12
**Mean of Homicides of males 15- 39 yo SINAIS / Mean of Homicides w/o males 15-39 yo SINAIS / Mean of Deaths related to DTOs Government Data )
table aux_control if date>=563 & aux_control==1 & date <= 611, contents(mean d_homi_male_15_39 mean d_homi_not_male_15_39 mean deaths_total )
**SD of Homicides of males 15- 39 yo SINAIS / SD  of Homicides w/o males 15-39 yo SINAIS / SD  of Deaths related to DTOs Government Data )
table aux_control if date>=563& aux_control==1  & date <= 611, contents(sd d_homi_male_15_39 sd d_homi_not_male_15_39 sd deaths_total )
**Total of Homicides of males 15- 39 yo SINAIS / Total of Homicides w/o males 15-39 yo SINAIS/ Total of Deaths related to DTOs Government Data )
table aux_control if date>=563 & aux_control==1 & date <= 611, contents(sum d_homi_male_15_39 sum d_homi_not_male_15_39 sum deaths_total )

**************************
**TREATED MUNICIPALITIES**
**************************
count if aux_treated==1 & year==2006 & month==12 & idunico!=8037
**Mean of Homicides of males 15- 39 yo SINAIS / Mean of Homicides w/o males 15-39 yo SINAIS/ Mean of Deaths related to DTOs Government Data )
table aux_treated if date>=563 &  aux_treated==1 & date <= 611 & idunico!=8037, contents(mean d_homi_male_15_39  mean d_homi_not_male_15_39 mean deaths_total )
**SD of Homicides of males 15- 39 yo SINAIS / SD  of Homicides w/o males 15-39 yo SINAIS / SD  of Deaths related to DTOs Government Data )
table aux_treated if date>=563 &  aux_treated==1 & date <= 611 & idunico!=8037, contents(sd d_homi_male_15_39 sd d_homi_not_male_15_39 sd deaths_total  )
**Total of Homicides of males 15- 39 yo SINAIS / Total of Homicides w/o males 15-39 yo SINAIS / Total of Deaths related to DTOs Government Data )
table aux_treated if date>=563 &  aux_treated==1 & date <= 611 & idunico!=8037, contents(sum d_homi_male_15_39 sum d_homi_not_male_15_39 sum deaths_total  )

******************************
**NEIGHBORING MUNICIPALITIES**
******************************
count if aux_neigh==1 & year==2006 & month==12 & idunico!=8037
**Mean of Homicides of males 15- 39 yo SINAIS / Mean of Homicides w/o males 15-39 yo SINAIS / Mean of Deaths related to DTOs Government Data )
table aux_neigh  if date>=563 &  aux_neigh==1 & date <= 611 & idunico!=8037, contents(mean d_homi_male_15_39  mean d_homi_not_male_15_39 mean deaths_total )
**SD of Homicides of males 15- 39 yo SINAIS / SD  of Homicides w/o males 15-39 yo SINAIS/ SD  of Deaths related to DTOs Government Data )
table aux_neigh  if date>=563 &  aux_neigh==1 & date <= 611 & idunico!=8037, contents(sd d_homi_male_15_39 sd d_homi_not_male_15_39 sd deaths_total  )
**Total of Homicides of males 15- 39 yo SINAIS / Total of Homicides w/o males 15-39 yo SINAIS / Total of Deaths related to DTOs Government Data )
table aux_neigh  if date>=563 &  aux_neigh==1 & date <= 611 & idunico!=8037, contents(sum d_homi_male_15_39 sum d_homi_not_male_15_39 sum deaths_total )

*********
**TOTAL**
*********
gen aux = 1
count if aux ==1 & year==2006 & month==12 & idunico!=8037
**Mean of Homicides of males 15- 39 yo SINAIS / Mean of Homicides w/o males 15-39 yo SINAIS/ Mean of Deaths related to DTOs Government Data )
table aux  if date>=563 & date <= 611 & idunico!=8037, contents(mean d_homi_male_15_39  mean d_homi_not_male_15_39 mean deaths_total )
**SD of Homicides of males 15- 39 yo SINAIS/ SD  of Homicides w/o males 15-39 yo SINAIS / SD  of Deaths related to DTOs Government Data  )
table aux  if date>=563 & date <= 611 & idunico!=8037, contents(sd d_homi_male_15_39 sd d_homi_not_male_15_39 sd deaths_total )
**Total of Homicides of males 15- 39 yo SINAIS/ Total of Homicides w/o males 15-39 yo SINAIS / Total of Deaths related to DTOs Government Data )
table aux  if date>=563 & date <= 611 & idunico!=8037, contents(sum d_homi_male_15_39 sum d_homi_not_male_15_39 sum deaths_total )
drop aux

***************
*** Table 2 ***
***************

*** Upper Table: Effects of Leadership Captures on Homicides Presumably Related to DTOs - Negative Binomial Model ***

*** Column 1: Subsample treatment leader
xi: nbreg d_homi_male_15_39 pob_male_15_39 i.date i.idunico lpub_after06 lpub_after612 lpub_after12 lieupub_after06 lieupub_after612 lieupub_after12 if treat_leader==1 & date>=563 & date<=611 &  idunico!=8037, vce(cluster idunico)			
*** Column 2: Subsample treatment lieutenant
xi: nbreg d_homi_male_15_39 pob_male_15_39 i.date i.idunico lpub_after06 lpub_after612 lpub_after12 lieupub_after06 lieupub_after612 lieupub_after12 if treat_lieu==1 & date>=563 & date<=611 &  idunico!=8037, vce(cluster idunico)			
*** Column 3: Weighted sample treatment leader
xi: nbreg d_homi_male_15_39 pob_male_15_39 i.date i.idunico lpub_after06 lpub_after612 lpub_after12 lieupub_after06 lieupub_after612 lieupub_after12 if date>=563 & date<=611 & idunico!=8037 [iw = weights_treat_lead_dto], vce(cluster idunico)			
*** Column 4: Weighted sample treatment lieutenant
xi: nbreg d_homi_male_15_39 pob_male_15_39 i.date i.idunico lpub_after06 lpub_after612 lpub_after12 lieupub_after06 lieupub_after612 lieupub_after12 if date>=563 & date<=611 & idunico!=8037 [iw = weights_treat_lieu_dto], vce(cluster idunico)			

*** Lower Table: Effects of Leadership Captures on Homicides Among the General Population - Negative Binomial Model ***

*** Column 1: Subsample treatment leader
xi: nbreg d_homi_not_male_15_39 pob_not_male_15_39 i.date i.idunico lpub_after06 lpub_after612 lpub_after12 lieupub_after06 lieupub_after612 lieupub_after12 if treat_leader==1 & date>=563 & date<=611 & idunico!=8037, vce(cluster idunico)			
*** Column 2: Subsample treatment lieutenant
xi: nbreg d_homi_not_male_15_39 pob_not_male_15_39 i.date i.idunico lpub_after06 lpub_after612 lpub_after12 lieupub_after06 lieupub_after612 lieupub_after12 if treat_lieu==1 & date>=563 & date<=611 & idunico!=8037, vce(cluster idunico)			
*** Column 3: Weighted sample treatment leader
xi: nbreg d_homi_not_male_15_39 pob_not_male_15_39 i.date i.idunico lpub_after06 lpub_after612 lpub_after12 lieupub_after06 lieupub_after612 lieupub_after12 if date>=563 & date<=611 & idunico!=8037 [iw = weights_treat_lead_rest], vce(cluster idunico)			
*** Column 4: Weighted sample treatment lieutenant
xi: nbreg d_homi_not_male_15_39 pob_not_male_15_39 i.date i.idunico lpub_after06 lpub_after612 lpub_after12 lieupub_after06 lieupub_after612 lieupub_after12 if date>=563 & date<=611 & idunico!=8037 [iw = weights_treat_lieu_rest], vce(cluster idunico)			

***************
*** Table 3 ***
***************

*** Upper Table: Neighboring Effects of Leadership Captures on Homicides Presumably Related to DTOs - Negative Binomial Model ***

*** Column 1: Subsample treatment leader
xi: nbreg d_homi_male_15_39 pob_male_15_39 i.date i.idunico lcpub_after06 lcpub_after612 lcpub_after12 lieucpub_after06 lieucpub_after612 lieucpub_after12 if treat_leader_cont ==1 & aux_treated==0 & date>=563 & date<=611 & idunico!=8037, vce(cluster idunico)			
*** Column 2: Subsample treatment lieutenant
xi: nbreg d_homi_male_15_39 pob_male_15_39 i.date i.idunico lcpub_after06 lcpub_after612 lcpub_after12 lieucpub_after06 lieucpub_after612 lieucpub_after12 if treat_lieu_cont==1 & aux_treated==0 & date>=563 & date<=611 & idunico!=8037, vce(cluster idunico)			
*** Column 3: Weighted sample treatment leader
xi: nbreg d_homi_male_15_39 pob_male_15_39 i.date i.idunico lcpub_after06 lcpub_after612 lcpub_after12 lieucpub_after06 lieucpub_after612 lieucpub_after12 if aux_treated==0 & date>=563 & date<=611 & idunico!=8037 [iw= weights_neigh_lead_dto], vce(cluster idunico)		
*** Column 4: Weighted sample treatment lieutenant
xi: nbreg d_homi_male_15_39 pob_male_15_39 i.date i.idunico lcpub_after06 lcpub_after612 lcpub_after12 lieucpub_after06 lieucpub_after612 lieucpub_after12 if aux_treated==0 & date>=563 & date<=611 & idunico!=8037 [iw = weights_neigh_lieu_dto], vce(cluster idunico)		

*** Lower Table: Neighboring Effects of Leadership Captures on Homicides Among the General Population - Negative Binomial Model ***

*** Column 1: Subsample treatment leader 
xi: nbreg d_homi_not_male_15_39 pob_not_male_15_39 i.date i.idunico lcpub_after06 lcpub_after612 lcpub_after12 lieucpub_after06 lieucpub_after612 lieucpub_after12 if treat_leader_cont ==1 & aux_treated==0 & date>=563 & date<=611 & idunico!=8037, vce(cluster idunico)			
*** Column 2: Subsample treatment lieutenant
xi: nbreg d_homi_not_male_15_39 pob_not_male_15_39 i.date i.idunico lcpub_after06 lcpub_after612 lcpub_after12 lieucpub_after06 lieucpub_after612 lieucpub_after12 if treat_lieu_cont==1 & aux_treated==0 & date>=563 & date<=611 & idunico!=8037, vce(cluster idunico)			
*** Column 3: Weighted sample treatment leader
xi: nbreg d_homi_not_male_15_39 pob_not_male_15_39 i.date i.idunico lcpub_after06 lcpub_after612 lcpub_after12 lieucpub_after06 lieucpub_after612 lieucpub_after12 if aux_treated==0 & date>=563 & date<=611 & idunico!=8037 [iw = weights_neigh_lead_rest], vce(cluster idunico)			
*** Column 4: Weighted sample treatment lieutenant
xi: nbreg d_homi_not_male_15_39 pob_not_male_15_39 i.date i.idunico lcpub_after06 lcpub_after612 lcpub_after12 lieucpub_after06 lieucpub_after612 lieucpub_after12 if aux_treated==0 & date>=563 & date<=611 & idunico!=8037 [iw = weights_neigh_lieu_rest], vce(cluster idunico)			

***************
*** Table 4 ***
***************

*** Upper Table: Neighboring Effects of Captures of Leaders - Neighboring Municipalities and Strategic Points
keep if aux_treated==0 & date>=563 & date<=611 &  idunico!=8037 

*** Column 1: Homicides of males between 15-39 years old, SINAIS Data 
xi: nbreg d_homi_male_15_39 pob_male_15_39 i.date i.idunico splcpub_after06 splcpub_after612 splcpub_after12 splieucpub_after06 splieucpub_after612 splieucpub_after12 lcpub_after06 lcpub_after612 lcpub_after12 lieucpub_after06 lieucpub_after612 lieucpub_after12 [iw = weights_neigh_lead_dto], vce(cluster idunico)
*** Column 2: Deaths presumably related to DTOs, Government Data
xi: nbreg deaths_total pob_male_15_39 i.date i.idunico splcpub_after06 splcpub_after612 splcpub_after12 splieucpub_after06 splieucpub_after612 splieucpub_after12 lcpub_after06 lcpub_after612 lcpub_after12 lieucpub_after06 lieucpub_after612 lieucpub_after12 [iw = weights_neigh_lead_dto], vce(cluster idunico)
*** Column 3: Homicides excluding males between 15-39 years old, SINAIS Data 
xi: nbreg d_homi_not_male_15_39 pob_not_male_15_39 i.date i.idunico splcpub_after06 splcpub_after612 splcpub_after12 splieucpub_after06 splieucpub_after612 splieucpub_after12 lcpub_after06 lcpub_after612 lcpub_after12 lieucpub_after06 lieucpub_after612 lieucpub_after12 [iw = weights_neigh_lead_rest], vce(cluster idunico)

*** Lower Table: Neighboring Effects of Captures of Lieutenants - Neighboring Municipalities and Strategic Points

*** Column 1: Homicides of males between 15-39 years old, SINAIS Data 
xi: nbreg d_homi_male_15_39 pob_male_15_39 i.date i.idunico splcpub_after06 splcpub_after612 splcpub_after12 splieucpub_after06 splieucpub_after612 splieucpub_after12 lcpub_after06 lcpub_after612 lcpub_after12 lieucpub_after06 lieucpub_after612 lieucpub_after12 [iw = weights_neigh_lieu_dto], vce(cluster idunico)
*** Column 2: Deaths presumably related to DTOs, Government Data
xi: nbreg deaths_total pob_male_15_39 i.date i.idunico splcpub_after06 splcpub_after612 splcpub_after12 splieucpub_after06 splieucpub_after612 splieucpub_after12 lcpub_after06 lcpub_after612 lcpub_after12 lieucpub_after06 lieucpub_after612 lieucpub_after12 [iw = weights_neigh_lieu_dto], vce(cluster idunico)
*** Column 3: Homicides excluding males between 15-39 years old, SINAIS Data 
xi: nbreg d_homi_not_male_15_39 pob_not_male_15_39 i.date i.idunico splcpub_after06 splcpub_after612 splcpub_after12 splieucpub_after06 splieucpub_after612 splieucpub_after12 lcpub_after06 lcpub_after612 lcpub_after12 lieucpub_after06 lieucpub_after612 lieucpub_after12 [iw = weights_neigh_lieu_rest], vce(cluster idunico)
log close


***************
*** Figure 1 **
***************

*** Figure 1: Trends of Homicides Using SINAIS and Government Data
use "replication.data.beheadings.march.15.dta", clear
gen quarter=0
replace quarter=1 if month==1 | month==2 | month==3
replace quarter=2 if month==4 | month==5 | month==6
replace quarter=3 if month==7 | month==8 | month==9
replace quarter=4 if month==10 | month==11 | month==12
gen time=yq(year,quarter)
collapse (sum) deaths_total d_homi_male_15_39 d_homi_not_male_15_39 (last) date year, by(time)
replace deaths_total =. if (year<2007)
drop if year <2000
label variable time "time"

#delimit ;
twoway (connected d_homi_male_15_39 time, sort mcolor(black) msize(small) msymbol(circle) lwidth(medthick)) 
(connected d_homi_not_male_15_39 time, sort mcolor(green) msize(small) msymbol(square) lcolor(green) lwidth(medthick)) 
(connected deaths_total time, sort mcolor(red) msize(small) msymbol(triangle) lcolor(red) lwidth(medthick)), 
xline(187, lwidth(medthick) lcolor(black) extend) xlabel(160(4)202 187, 
labels labsize(small) angle(vertical) format(%tq!Qq-CCYY) valuelabel) 
legend(order(1 "Homicides males 15-39 yo (SINAIS)" 2 "Homicides not males 15-39 yo (SINAIS)" 3 "Drug related deaths (Gov)") size(small)) 
scheme(sj) graphregion(fcolor(none) lcolor(none) ifcolor(none) ilcolor(none)) plotregion(fcolor(none) ifcolor(none))
;
#delimit cr
graph export "Graph historic trends.png",replace
* window manage close graph

***************
*** Figure 3 **
***************

*** Figure 3: Weighted Residuals of Homicides of Males between 15 and 39 Years Old

*** First we save a matrix with of synthetic weigths
use "replication.data.beheadings.march.15.dta", clear
collapse (first) weights_treat_lead_dto weights_treat_lead_rest weights_treat_lieu_dto weights_treat_lieu_rest, by(idunico)
save "weights.dta", replace

*** We create all the relevant variables
* use ".../replication.data.beheadings.march.15.dta", clear
use "replication.data.beheadings.march.15.dta", clear
sort idunico date
drop if idunico==8037
tsset idunico date, monthly

*** We generate variables with the date of first capture by municipality
gen aux_date_treat_leader = 999
replace aux_date_treat_leader = date if leader_pub > 0 
by idunico, sort : egen float date_treat_leader = min(aux_date_treat_leader)
gen aux_date_treat_lieutenant = 999
replace aux_date_treat_lieutenant = date if lieutenant_pub > 0 
by idunico, sort : egen float date_treat_lieutenant = min(aux_date_treat_lieutenant)
gen aux_date_treat_neigh_leader = 999
replace aux_date_treat_neigh_leader = date if leader_cont_pub > 0 
by idunico, sort : egen float date_treat_neigh_leader = min(aux_date_treat_neigh_leader)
gen aux_date_treat_neigh_lieutenant = 999
replace aux_date_treat_neigh_lieutenant = date if lieutenant_cont_pub > 0 
by idunico, sort : egen float date_treat_neigh_lieutenant = min(aux_date_treat_neigh_lieutenant)
drop aux_date_treat_leader aux_date_treat_lieutenant aux_date_treat_neigh_leader aux_date_treat_neigh_lieutenant
save "Base.auxiliar.residuales.dta", replace

*** We estimate the residuals for each series: treatment leader, treatment lieutenant, and control group.

*** Residuals: Treatment Leaders
use "Base.auxiliar.residuales.dta",clear
drop if date < 492
drop if idunico==8037
*** Here we create a relative time variable (counter) for which 0 is the date of the first capture.
gen aux_date_ref = 999 if treat_leader > 0
replace aux_date_ref = leader_pub*date if leader_pub > 0
by idunico, sort : egen float date_ref = min(aux_date_ref)
gen counter_leader = date - date_ref
sort idunico date
drop aux_date_ref date_ref
egen float group_leader = group(idunico) if treat_leader ==1
tab group_leader
** Leaders were captured in 13 municipalities *
** For each municipality, we estimate a time trend using homicides of males between 15-39 yo before the first capture
gen trend_leader = .
forvalues i = 1(1)13 {
reg d_homi_male_15_39 date if counter_leader < 0 & group_leader == `i'
predict aux_predict, xb
replace trend_leader = aux_predict if group_leader == `i'
drop aux_predict
}
drop group_leader
** We regress homicides using time trends and time and municipality fixed effects
regress d_homi_male_15_39 i.date i.idunico trend_leader
predict resid_leader, residuals
** We save the residuals
collapse (mean) resid_leader , by(counter_leader)
rename counter_leader counter
save "residuales.lideres.dta", replace

*** Residuals: Treatment Lieutenants 
use "Base.auxiliar.residuales.dta",clear
drop if date < 480
drop if idunico==8037
*** Here we create a relative time variable (counter) for which 0 is the date of the first capture.
gen aux_date_ref = 999 if treat_lieu >0
replace aux_date_ref = lieutenant_pub*date if lieutenant_pub >0
by idunico, sort : egen float date_ref = min(aux_date_ref)
gen counter_lieutenant = date - date_ref
sort idunico date
drop aux_date_ref date_ref
egen float group_lieutenant = group(idunico) if treat_lieu ==1
tab group_lieutenant
** Lieutenants were captured in 69 municipalities *
** For each municipality, we estimate a time trend using homicides of males between 15-39 yo before the first capture
set more off
gen trend_lieutenant = .
forvalues i = 1(1)69 {
reg d_homi_male_15_39 date if counter_lieutenant < 0 & group_lieutenant == `i'
predict aux_predict, xb
replace trend_lieutenant = aux_predict if group_lieutenant == `i'
drop aux_predict
}
drop group_lieutenant
** We regress homicides using time trends and time and municipality fixed effects
regress d_homi_male_15_39 i.date i.idunico trend_lieutenant
predict resid_lieutenant, residuals
collapse (mean) resid_lieutenant , by(counter_lieutenant)
rename counter_lieutenant counter
save "residuales.lieutenants.dta", replace

*** Residuals: Control Group
use "Base.auxiliar.residuales.dta",clear
drop if idunico==8037
*** Municipality of Tulum did not exist before 2006
drop if idunico==23009  
drop if date < 480
*** Here we create a relative time variable (counter) for which December 2006 is equal to 0.
gen aux_date_ref = 999 if aux_control >0
replace aux_date_ref = date if year ==2006 & month ==12
by idunico, sort : egen float date_ref = min(aux_date_ref)
gen counter_control = date - date_ref
sort idunico date
drop aux_date_ref date_ref
egen float group_control = group(idunico) if aux_control ==1
set more off
tab group_control
** 2048 control municipalities
** For each municipality, we estimate a time trend using homicides of males between 15-39 yo before the first capture
gen trend_control = .
forvalues i = 1(1)2048 {
set more off
reg d_homi_male_15_39 date if counter_control < 0 & group_control == `i'
predict aux_predict, xb
replace trend_control = aux_predict if group_control == `i'
drop aux_predict
display `i'
}
drop group_control

** We weight control municipalities according to the weights we estimated for the cases of the capture of a leader or a lieutenant
merge m:1 idunico using "weights.dta"
drop if idunico==8037
drop if idunico==23009  
drop _merge
gen weights_control = weights_treat_lead_dto + weights_treat_lieu_dto

** We use a weighted regression for homicides using time trends and time and municipality fixed effects
regress d_homi_male_15_39 i.date i.idunico trend_control [iw= weights_control]
predict resid_control_direct_both, residuals
collapse (mean) resid_control_direct_both, by(counter_control)
rename counter_control counter
save "residuales.control.dta", replace

** Figure 3: Weighted Residuals of Homicides of Males between 15 and 39 Years Old **

use "residuales.lideres.dta", clear
merge 1:1 counter using "residuales.lieutenants.dta"
drop _merge
merge 1:1 counter using "residuales.control.dta"
drop _merge
sort counter
drop if counter==.

#delimit ;
twoway (connected resid_control_direct_both counter if counter > -35 & counter < 14, sort mcolor(black) msize(small) msymbol(circle) lwidth(thin) ) 
(connected resid_lieutenant counter if counter > -35 & counter < 14, sort mcolor(blue) msize(small) msymbol(square) lcolor(blue) lwidth(thin) ) 
(connected resid_leader counter if counter > -35 & counter < 14, sort mcolor(red) msize(small) msymbol(triangle) lcolor(red) lwidth(thin) )
, ylabel(-20(10)25) xlabel(-35(10)14) xline(0 6 12, lpattern(dash) lwidth(thin) lcolor(black) extend) 
xtitle("Number of months before and after capture", size(small))
legend(order(1 "Weighted controls" 2 "Lieutenants" 3 "Leaders") 
size(small)) scheme(sj) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) 
plotregion(fcolor(white) ifcolor(white))
;
#delimit cr
graph export "Graph Residuals.png",replace
* window manage close graph

