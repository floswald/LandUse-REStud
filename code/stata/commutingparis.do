**************************************************************************************COMMUTING IN PARIS***************************************************************************

***Mode of transport
import excel "../data/raw/historicalcommute_paris.xlsx", sheet("speed") firstrow clear

***normalize speed in 1835 to see the speed increase
gen Comm_speed_Paris= average/average[1]
rename average average_speed_kmh
rename date year
***save a model's input for figure on commuting speed
keep year average_speed_kmh Comm_speed_Paris
merge 1:1 year using "../data/processed/FRA_model.dta"
sort year
drop _merge
save "../data/processed/FRA_model.dta", replace
