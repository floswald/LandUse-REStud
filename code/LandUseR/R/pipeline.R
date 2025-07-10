# Outline
# 1. get ranking of top n french cities by population in 2015. This defines the study population. The cities are selected such that they are always present (i.e. not Villeurbane near Lyon and not Boulogne-Billancourt near paris) and not overseas territory
# 2. final output is a data.table with five columns: CODGEO, LIBGEO, year, area, population.
# 3. year = c(1876, 1950, seq(1975,2015, by = 5))
# 4. area is a visual measure in 1876 and 1950, automatized from raster data after
# 5. manual measures are in online google sheet

#' Process Raw Data and Make Plots
#' 
#' Data Pipeline of project
#' @export
run_data <- function(){
    tic = Sys.time()
    futile.logger::flog.info("\n\nHi!\nstarting full LandUseR data run now. \n\n")

    # create out dir structure
    dir.create(dataplots(),showWarnings = FALSE, recursive = TRUE)
    dir.create(datatables(),showWarnings = FALSE, recursive = TRUE)

    # get list and crop
    pop_1950_2(overwrite = TRUE)
    cc = cropboxes(overwrite = TRUE)

    # build 100 and 200 city samples
    run_step1(overwrite = TRUE, topn = 100)
    run_step1(overwrite = TRUE, topn = 200)

    futile.logger::flog.info("all data built.")
    futile.logger::flog.info("starting to plot data figures")

    figure3()
    plot_top100_densities(save = TRUE)  # produces a bunch of other plots too
    figure4b()
    figure5()

    futile.logger::flog.info("done plotting main text data figures")

    run_step2()
    toc = Sys.time()

    futile.logger::flog.info("data built and plots done in %s minutes", round(toc - tic,digits = 2))
}

#' LandUse pipeline
#'
#' @export
run_step1 <- function(overwrite = FALSE,topn = 100){
    futile.logger::flog.info("start city measurement with topn=%d",topn)

    cm = measure_cities(topn = topn, overwrite = overwrite)
    mm = combine_measures(overwrite = FALSE)  # rebuilt in previous steps

    # pra output
    pra_stack(topn = topn, overwrite = overwrite)

    export_pop_area(topn = topn)
    futile.logger::flog.info("stop city measurement with topn=%d",topn)
}

run_step2 <- function(with_app_C = FALSE){
    futile.logger::flog.info("producing appendix A data-based figures/tables")
    print_pop_1950()
    print_paris_pop_1950()
    GHSL_plots()
    # plot_top100_densities()  # ran already
    plot_top100_cutoff(save = TRUE)
    plot_sat_vs_manual()
    plot_shlomo()
    if (with_app_C){
        CLC_measure()
        CLC_plots()

    }
    map_pra_all()
    wheat_plots()
    map_pra_prices2000()
    exp_decay_linear()
    # tables A3 needs model solution
    # tableA4()
    # tableA5_A6()
    futile.logger::flog.info("done appendix A data-based figures")
}




#' model vs data
run_step3 <- function(){
    futile.logger::flog.warn("You must have run the structural model before you run this function.")

    table2(save = TRUE)
    tableA3(writedisk = TRUE)
    tableA4(save = TRUE)
    tableA5_A6(writedisk = TRUE)
    
}

#' Read main output table
#'
#' By default selects the version with `cutoff = 30`
#' @export
read_output <- function(cutoff = 30){
    readRDS(file.path(outdatadir(),paste0("france_final_cutoff",cutoff,".Rds")))
}

#' Show Relative population and area
#'
#' output table for top5 cities with relative population and area to Paris.
#' @export
relative_pop_area <- function(cities = top5now(), overwrite = FALSE){
    if (overwrite){
        x = read_output()
        y = x[(LIBGEO %in% cities) & (year == 2015), list(LIBGEO,relative_pop = pop / max(pop), relative_area = area / max(area))][order(relative_pop)]
        fwrite(y, file.path(outdatadir(),"top5poparea.csv"))
        y
    } else {
        fread(file.path(outdatadir(),"top5poparea.csv"))
    }
}

#' Main Output File
#'
#' reads output of manual and satellite measures via \code{\link{read_output}}
#' and amends Paris Population to also include the Seine department as well before WW2.
#'
#' The csv output of this file is used in the model.
#' @export
export_pop_area <- function(topn = 100){

    if (topn == 100) {
        # need to fix population counts for paris: wrong for pre 1975

        x = read_output()
        p0 = data.table(readpop())
        p1 = p0[DEP == 75, list(CODGEO = "75060", LIBGEO = "Paris", population = sum(population)), by = year(date)]
        p = rbind(p0[,list(CODGEO,LIBGEO,population,year)],p1[,list(CODGEO,LIBGEO,population,year)])
        x[year == 1950 , year := 1954] # fix census year
        x[year == 2000 , year := 1999] # fix census year for merge
        p = p[CODGEO %in% x[,unique(CODGEO)]]
        y = merge(x[,list(CODGEO,rank,pop, area, type,year,DEP,REG)], p, all.y = FALSE, by = c("CODGEO","year"))

        # paris census data is incorrect - paris is larger than "paris" even before WW2
        pa = fread(file.path(datadir(),"paris-seine.csv"))
        setnames(pa, "population", "pop2")
        pa[,CODGEO := "75060"]
        pa[year == 1876, pop2 := 1990813]   # manually fixing: in 1876 Paris intra muros IS CORRECT.
        y = merge(y, pa, all.x = TRUE)
        y[ year < 1954 & CODGEO == "75060", population := pop2]
        y[,pop2 := NULL]

        # before 1954, take census measures, after take satellite
        y[ year < 1954, pop := population]
        y[ , rank := .SD[year == 1876, rank], by = CODGEO]
        y <- y[!is.na(pop)]
        y = y[, list(CODGEO = as.character(CODGEO),LIBGEO, DEP,REG,rank,density_data = pop / area, reldensity_data = (pop / area) / max(pop / area), area_data = area,relarea_data = area / max(area),pop_data = pop,relpop_data = pop / max(pop)),by=year]

    } else {
        x = get_flat_list()  # loads top 200 cities only for 1975+
        y = x[, list(CODGEO = as.character(CODGEO),LIBGEO, rank,density_data = pop / area, reldensity_data = (pop / area) / max(pop / area), area_data = area,relarea_data = area / max(area),pop_data = pop,relpop_data = pop / max(pop)),by=year]
    }

    # weights: 1975 population
    p1975 = y[year == 1975, .(CODGEO,pop_1975 = pop_data)]
    y = merge(y, p1975, by = "CODGEO")
    fwrite(y, file.path(outdatadir(),paste0("relpop-",topn,".csv")))
    y

}
