

*** You have 3 options to run this code:

/* 1. execute the the `run_all.sh` script in code/. On unix/mac, type `./run_all.sh` in that folder [preferred option].
2. In the stata GUI set the working directory to the folder `code/' in this replication package and type : `do stata/replication_aggregate_main.do`
3. manually edit the below line to change the directory to folder `code` and click "run" in the do-file editor */

/* cd  "path/to/ReplicationPackage/code"package */


***  NO EDITS BELOW THIS LINE ARE NEEDED

cap mkdir "../output/data"
cap mkdir "../output/data/plots"


***LAND USE & EMPLOYMENT SHARE IN AGRICULTURE
***Figures on Land Use & Employment share in Agriculture = Figure 1 (+ Figure A.2(a) & Figure A.2(b))
***upload data on aggregate land use, employment, prices described in Appendix A.1.1, A.1.2, A.1.3 in "FRA_base.dta". 
***Data sources are summarized in the dataset and described in details in the Appendix.
quietly do "stata/figure1.do"


***BUILD RELATIVE PRICES, URBAN AND RURAL PRODUCTIVITY
***computes relative agricultural prices and thetas + plot Figure 7 (+ Figure A.3 & Figure A.4) + generate inputs for estimation in "FRA_model.dta"
***upload data on aggregate land use, employment, prices described in Appendix A.1.1, A.1.2, A.1.3 in "FRA_base.dta". 
***Data sources are summarized in the dataset and described in details in the Appendix.
***this do file proceed in two steps: 
***(1) compute relative prices for agricultural goods (Appendix A.1.3) [plotting data on relative prices, Figure A.3]
***(2) compute the aggregate thetas in Urban and Rural (Appendix A.1.4) [plotting the thetas, Figure 7 in main text and Figure A.4]
***Inputs for estimation are saved in "FRA_model.dta"
quietly do "stata/generatethetas.do"

***EXPENDITURE SHARES = Figure A(5) + Housing spending shares as input for the estimation
***upload data on expenditure shares from Pierre Villa over 1896-1939 and from INSEE over 1959-2017 in spending_shares.xls. Data are described in Appendix A.1.5
***plot Figure A.5. Housing spending shares are saved in "FRA_model.dta" as inputs for estimation.
quietly do "stata/figureA5.do"


***HOUSING AND LAND VALUE = Figure 6(a) and 6(b) + Land value over income as inputs for Figure 12
***upload data from  Piketty and Zucman (2014) in Piketty_France.xls and plot Figure 6(a). Land values over income are saved in "FRA_model.dta".
***uplad house price data from Knoll et al. (2017) for France in "NPLH_FRA.dta" and plot Figure Fig. 6(b)
quietly do "stata/figure6.do"


***COMMUTING IN PARIS = compute average speed over the period 1835-2010 for Figure 11(a) + plot Figure A.29(a) and Figure A.30(b)
***upload data on mode use and speed of the different modes in "historicalcommute_paris.xlslx". Short description in the read me of the data and details in Appendix A.6.
***Average commuting speed in Paris is saved in "FRA_model.dta".
quietly do "stata/commutingparis.do"


****export data as csv
clear
use "../data/processed/FRA_model.dta"
format SpendingShare_Rural-Land_rural %17.0g
export delimited using "../data/processed/FRA_model.csv", datafmt replace
