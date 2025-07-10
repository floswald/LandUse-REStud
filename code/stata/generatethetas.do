
*************************************************************************************BUILD RELATIVE PRICES & PRDUCTIVITY DATA***************************************************************************************************************************
use "../data/raw/FRA_base.dta", clear
***Agricultural Land Use: be cautious with data pre-1840 (not used in the paper)
****be careful: the land value for 1790 in Lavoisier is not accurate as it includes a large component of 'prés et herbages' which should not be considered as 'surface agricole utile'. the value of 34000 in the data file is below the Lavoisier measure of 41000 and largely exclude those (see Appendix A.1.1)
****to compute productivity since 1815 a data point in 1815 is needed: the only point before 1840 by Lavoisier in 1790 is mismeasured; we put an estimated value (35500) consistent with share of land use for agriculture in later periods and close to our estimate when ajusting the measure of Lavoisier in 1790.
****data on productivity in agriculture are thus less reliable prior to 1840 (therefore not used in the paper)

***interpolate data on land use and agricultural employment
ipolate Land_Agriculture  year, generate(Surface_agri) 
ipolate Area_France  year, generate(Surface_totale) 


****use OECD post WW2 and Marchand/Thelot/Herrendorf et al. pre-WW2 for agricultural employment
replace employment_OECD = Empl_total if employment_OECD==.
replace agriculture_OECD = Empl_agriculture if agriculture_OECD==.

ipolate employment_OECD  year, generate(employment) 
ipolate agriculture_OECD  year, generate(employment_agri) 

***drop data pre-1806 due to lack of observations
drop if year<1806

keep year GDP_deflator P_agri P_agri_1949 CPI Volume_Agriculture Volume_INDUSTRY Volume_TOTAL VA_agriculture VA_total Land_Agriculture Area_France employment employment_agri Surface_agri VA_nonagriculture Surface_totale

*********************************************1. Build different measures of relative price of agricultural goods***********************************************
****P index of agricultural good according to Toutain 1993 & comparison with the Sauvy price of agricultural goods*******************************************
***baseline
gen P_agri_Toutain=VA_agriculture/Volume_Agriculture

***renormalize at 100 in 1949
gen P=P_agri_Toutain[144]
replace P_agri_Toutain=100*P_agri_Toutain/P
drop P

*********P index of non agricultural/industrial goods and GDP deflator from Toutain
gen P_nonagri_Toutain=VA_nonagriculture/Volume_INDUSTRY
gen P_GDP_Toutain=VA_total/Volume_TOTAL

***renormalize at 100 in 1949 (obs=144)
gen P=P_nonagri_Toutain[144]
replace P_nonagri_Toutain=100*P_nonagri_Toutain/P
drop P

gen P=P_GDP_Toutain[144]
replace P_GDP_Toutain=100*P_GDP_Toutain/P
drop P

***renormalize GDP deflator in 1960 (obs 155) to continue Toutain GDP deflator
gen P=GDP_deflator[155]/P_GDP_Toutain[155]
replace GDP_deflator=GDP_deflator/P
drop P

***Sanity check: compare CPI and GDP deflator Toutain and relative price of agriuclutral goods*****************************************************
*graph twoway (line GDP_deflator year, lwidth(thick) lcolor(blue)) (line P_GDP_Toutain year, lwidth(thick) lcolor(red)) (line CPI year, lwidth(thick) lpattern(dash)  lcolor(orange)), xlabel(1810(20)2010) xscale(r(1810 2020)) yti("CPI and GDP deflators (1949=100)") xti("") scheme(s1color) yscale(log) ylabel(1 3 10 30 100 300 1000) 

**************since CPI & GDP deflatorare the same, continue the serie of Toutain GDP deflator using the GDP deflator post 1960
replace P_GDP_Toutain=GDP_deflator if year>1959

***********************************************************************************************GRAPH ON RELATIVE PRICES with different measures = Figure A.3**************************************************
gen Toutain=P_agri_Toutain/P_GDP_Toutain  
gen Sauvy= P_agri/P_GDP_Toutain
gen INSEE=P_agri_1949/P_GDP_Toutain

graph twoway (line Toutain year, lwidth(thick) lcolor(dkgreen)) (line Sauvy year, lwidth(thick) lpattern(dash)  lcolor(dkgreen)) (line INSEE year, lwidth(thick) lcolor(black)), xlabel(1810(20)2010) xscale(r(1810 2018)) yti("Relative price of agricultural goods (1949=100)") xti("") scheme(s1color)
graph export ../output/data/plots/figureA3.pdf, as(pdf) replace
drop Toutain Sauvy INSEE 

***********************************************************************************2. Compute the productivity in urban and agriculture (thetas)*************************************
**************************************************Compute value added per worker in real terms in agri and non agri****************************************************************
***deflate using GDP deflator
***all VA (real) using GDP deflator
gen VA_worker=(VA_total)/ (employment)
replace VA_worker= VA_worker/P_GDP_Toutain


***agricultural (real)
*****use INSEE agricultural producer price post 1949 for agri. relative price 
replace P_agri_Toutain=P_agri_1949 if year>1948
***alternative relative price of agri. goods from Sauvy
replace P_agri=P_agri_1949 if year>1948
***nominal value added per worker in agriculture
gen VA_agri_worker=(VA_agriculture)/ (employment_agri)
******real value added per worker in agri (deflated by producer price of agri. goods from Toutain consistently with the production/VA data)
gen VA_agri_worker_real= VA_agri_worker/P_agri_Toutain

drop VA_agri_worker 


***non agricultural (real)
***need first to generate a price for non agricultural goods given the GDP deflator [GDP deflator= Toutain and then INSEE/WB starting 1960]
gen ShNonAgri=(VA_total- VA_agriculture)/VA_total

***Relative price of non-agri goods
gen P_nonagri=1/(1/(P_GDP_Toutain*ShNonAgri)-((1-ShNonAgri)/(P_agri_Toutain*ShNonAgri)))

***nominal value added per worker in non-agri
gen VA_nonagri_worker=(VA_total- VA_agriculture)/ (employment- employment_agri)
***real value added per worker in non-agri (deflated by price of non-agri)
gen VA_nonagri_worker_real= VA_nonagri_worker/P_nonagri

drop VA_nonagri_worker

***renormalize all VA per worker to 1 in initial date in 1815 (=obs 10)
gen x=VA_worker[10]
replace VA_worker=VA_worker/x
drop x

gen x=VA_agri_worker_real[10]
replace VA_agri_worker_real=VA_agri_worker_real/x
drop x

gen x=VA_nonagri_worker_real[10]
replace VA_nonagri_worker_real=VA_nonagri_worker_real/x
drop x

gen Agriculture=VA_agri_worker_real
gen NonAgriculture=VA_nonagri_worker_real

***************************Last step: adjust agricultural productivity for land use**********************************************************
***correct agricultural theta for land use
***compute land per worker in agri
gen Surface_agri_worker = Surface_agri /employment_agri 

***generate the exponent on land = land share
gen exponent_surface=0.25
replace Agriculture=VA_agri_worker_real*Surface_agri_worker^(-exponent_surface)
***normalization in the first period for plot since initial period
gen x=Agriculture[10]
replace Agriculture=Agriculture/x
drop x

*******************************************************************************************SAVE INPUTS OF THETA + Figure 7 = Figure A.4*******************************************************
gen Rural=Agriculture
gen Urban=NonAgriculture
***graph in former appendix since 1815 [not in new version]: be careful data for thetas are uncertain before 1840 due to mismeasurd land use
*graph twoway (line Rural year, lwidth(thick) lcolor(green))  (line Urban year, lwidth(thick) lpattern(dash)  lcolor(red)), xlabel(1810(20)2010) xscale(r(1815 2020)) ylabel(1 2 4 8 16 32) yti("Sectoral Productivity (1815=1)") xti("") scheme(s1color) yscale(log)

gen Employment_rural=employment_agri/ employment
gen Land_rural=Surface_agri/ Surface_totale
****Prepare inputs for Model
drop if year<1815
rename Rural theta_rural
rename Urban theta_urban

***Two different measures of relative price of agri goods (Sauvy and then INSEE post 1949, Toutain and then Insee post 1949)
gen P_rural_Sauvy=P_agri/P_nonagri
gen P_rural_Toutain=P_agri_Toutain/P_nonagri
keep year theta_rural theta_urban Employment_rural Land_rural P_rural_Sauvy P_rural_Toutain employment 
***save inputs for model
save "../data/processed/FRA_model.dta", replace

***only sarting in 1840 for Figures in Paper = Figure 7 in main text and A.4 in Appendix
drop if year<1840

***normalize TFP rural and urban to 1 in 1840 for plot
gen Rural=theta_rural/theta_rural[1]
gen Urban=theta_urban/theta_urban[1]

graph twoway (line Rural year, lwidth(thick) lcolor(green))  (line Urban year, lwidth(thick) lpattern(dash)  lcolor(red)), xlabel(1840(20)2020) xscale(r(1840 2020)) ylabel(1 2 4 8 16 32) yti("Sectoral Productivity (1840=1)") xti("") scheme(s1color) yscale(log)
graph export ../output/data/plots/figure7.pdf, as(pdf) replace
graph export ../output/data/plots/figureA4.pdf, as(pdf) replace

