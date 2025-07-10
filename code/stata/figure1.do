
use "../data/raw/FRA_base.dta", clear

*********************************************************************DO INTRO GRAPH***********************************************************************************************************
*******************be careful: the land value for 1790 in Lavoisier is not accurate as it includes a large component of 'prés et herbages' which should not be considered as 'surface agricole utile'. the value of 34000 in the data file is below the Lavoisier measure and largely exclude those (see Appendix A.1.1)
gen Land = 100*Land_Agriculture/Area_France
gen Employment = Share_Emp_Agr100


***graph for Appendix Figure A.2(b) starting 1806
drop if year<1806
graph twoway (line Employment year, lwidth(thick) lcolor(green)) , xlabel(1800(20)2020) xscale(r(1800 2020)) ylabel(0 20 40 60 80) yti("Share of Employment in Agriculture (in %)") xti("") scheme(s1color) 
graph export ../output/data/plots/figureA2b.pdf, as(pdf) replace

***interpolate data on share of agricultural land
ipolate Land year, generate(ipolateLand)
replace Land=ipolateLand
drop ipolateLand

***graph for draft, starting 1840 = Figure 1
drop if year<1840
graph twoway (line Land year, lwidth(thick) lcolor(olive) yaxis(1) yti("Share of Land in Agriculture (in %)")) (line Employment year, lwidth(thick) lcolor(green) lpattern(dash) yaxis(2) yti("Share of Employment in Agriculture (in %)", axis(2))), xlabel(1840(20)2020) xscale(r(1840 2018))  xti("") scheme(s1color)
graph export ../output/data/plots/figure1.pdf, as(pdf) replace

***graph for appendix Figure A.2(a)
graph twoway (line Land year, lwidth(thick) lcolor(olive) yaxis(1) yti("Share of Land in Agriculture (in %)")), xlabel(1840(20)2020) xscale(r(1840 2018))  xti("") scheme(s1color)
graph export ../output/data/plots/figureA2a.pdf, as(pdf) replace

