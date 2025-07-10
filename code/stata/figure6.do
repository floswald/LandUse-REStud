**************************************************************************************HOUSING AND LAND VALUE = Figure 6(a) and 6(b)***************************************************************************

***Housing and Agricultural Land Value (from Piketty et al. for France) = Fig. 6(a)
import excel "../data/raw/PikettyZucman.xls", sheet("wealth") firstrow clear
drop if year==.
gen HousingLand_nationalincome100 = (Land_Nationalincome + Housing_Nationalincome)*100
gen Land_Nationalincome100=100*Land_Nationalincome
label var HousingLand_nationalincome100 "Housing + Agricultural Land"
label var Land_Nationalincome100 "Agricultural Land"
***plot starts in 1820
drop if year<1820
graph twoway (line Land_Nationalincome100 year, lwidth(thick) lcolor(green) lpattern(dash) yaxis(1)) (line HousingLand_nationalincome100 year, lwidth(thick) lcolor(red) ), xlabel(1830(20)2010) xscale(r(1830 2015)) ylabel(0(200)600,format(%8.0g))  yti("Total value as a share of national income (in %)") xti("") scheme(s1color)
graph export ../output/data/plots/figure6a.pdf, as(pdf) replace

***compute Agricultural Land over Income and Housing Land over Income for comparison with model (% of total land value)
***use average from OECD since 1979 for land share in housing
gen ShareLandHousing = 0.32
gen AgriLand_inc = Land_Nationalincome
gen HousingLand_inc = ShareLandHousing*Housing_Nationalincome

***save in model's inputs
keep year AgriLand_inc HousingLand_inc
merge 1:1 year using "../data/processed/FRA_model.dta"
sort year
drop _merge
save "../data/processed/FRA_model.dta", replace


****House Prices (data from Knoll et al. for France) = Fig. 6(b)
use "../data/raw/NPLH_FRA.dta", clear
***deflate nominal house price index by CPI
gen hpreal=hpnom/cpi
gen hpreal100 = 100*hpreal
graph twoway line hpreal100 year, xlabel(1870(20)2010) xscale(r(1890 2010)) yti("Real House Price Index in France (1990=100)") xti("") lwidth(thick) lcolor(red) scheme(s1color) 
graph export ../output/data/plots/figure6b.pdf, as(pdf) replace
