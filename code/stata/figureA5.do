
********************************************************************************************************EXPENDITURE SHARES******************************************
import excel "../data/raw/spending_shares.xlsx", sheet("summary") firstrow clear

***express spending share in %
gen Food1= 100*FoodDrinks
gen Housing1 = 100*Housingrelatedexpenses
gen Other1=100*(Other+Clothing)

gen Food= 100*FoodDrinks
gen Housing = 100*Housingrelatedexpenses
replace Other=100*(Other+Clothing)

replace Food1=.  if year>1913 & year<1959
replace Housing1 = . if year>1913 & year<1959
replace Other1=. if year>1913 & year<1959

ipolate Food1 year, generate(Foodipolate)
ipolate Housing1 year, generate(Housingipolate)
ipolate Other1 year, generate(Otheripolate)



************************************************************Appendix Graph on spending shares = Figure A5*************************************************************************************************************
label variable Food "Food (Rural)"
label variable Other "Other (Urban)"
graph twoway (line Food year, lwidth(thick) lcolor(green) cmissing(no)) (line Foodipolate year, lpattern(dash) lcolor(green) cmissing(no)) (line Housing year, lwidth(thick)  lcolor(black) cmissing(no))  (line Housingipolate year, lpattern(dash)  lcolor(black) cmissing(no)) (line Other year, lwidth(thick) lcolor(red) cmissing(no)) (line Otheripolate year, lpattern(dash) lcolor(red) cmissing(no)), xlabel(1900(20)2020) xscale(r(1890 2020)) yti("Expenditure shares (in % of total expenditures)") xti("") scheme(s1color) legend(order(1 3 5))
graph export ../output/data/plots/FigureA5.pdf, as(pdf) replace

****add spending shares to model's input
gen SpendingShare_Rural=Food/100
gen SpendingShare_Urban=Other/100
gen SpendingShare_Housing=Housing/100

keep year SpendingShare_Rural SpendingShare_Urban SpendingShare_Housing
merge 1:1 year using "../data/processed/FRA_model.dta"
sort year
drop _merge
save "../data/processed/FRA_model.dta", replace
