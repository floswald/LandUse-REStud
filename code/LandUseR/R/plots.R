
# Plotting Functions


#' Plot distance from center
#'
#' using GHS grid data, plot average density at a certain distance
#' from the center of a city.
#'
#' This function uses output of \code{\link{dist_from_center}} and
#' estimates an exponential decay model via NLS.
#'
#' @param city_name vector of city names
#'
#' @return list with plots
#'
#' @references https://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/
plot_density_center <- function(city_name = c("Paris","Lyon"),by_year = TRUE, w=9,h=6){
    d0 = dist_from_center()
    l = list()
    lnls = list()
    for (ic in city_name){
        d = d0[LIBGEO == ic & !is.na(distance)]
        L = exp_decay_single(d,by_year = by_year)

        theme_set(theme_bw())

        if (by_year) {
            l[[ic]] = ggplot(data = L$augmented, aes(x=distance,y = density, color = factor(year))) +
                geom_point() +
                geom_line(aes(y = .fitted)) +
                labs(caption = paste("Data: Average density at",d[,max(quantile,na.rm=TRUE)],"points"),
                     subtitle = paste("Avg exponential parameter (over time):",round(mean(L$tab$lambda),5))) +
                ggtitle(paste("Density over time in",ic)) +
                scale_x_continuous(name = "Distance to Center in km")
        } else {
            l[[ic]] = ggplot(data = L$augmented, aes(x=distance,y = density)) +
                geom_point() +
                geom_line(aes(y = .fitted)) +
                labs(caption = paste("Data: Average density at",d[,max(quantile,na.rm=TRUE)],"points"),
                     subtitle = paste("Exponential parameter:",round(L$tab$lambda,5))) +
                ggtitle(paste("Density over time in",ic)) +
                scale_x_continuous(name = "Distance to Center in km")
        }
        ggsave(l[[ic]], filename = file.path(dataplots(),paste0("density-center-",ic,".pdf")), width = w, height = h)
    }
    l
}

#' plot manual vs satellite measurements 2015/2016
#'
#' makes a plot to compare our manual area measures in 2016 to the satellite measure in 2015
plot_sat_vs_manual <- function(w=8.5,h=5){
    f = read_output()
    p = ggplot(dcast.data.table(f[year > 2014,list(type, area, CODGEO)], CODGEO~ type, value.var = "area"), aes(x= manual, y = satellite)) + geom_point() + ggtitle("Manual (2016) vs GHSL (2015) Area Measures", subtitle = "solid line is 45 degrees") + scale_x_log10("km2 manual 2016") + scale_y_log10("km2 satellite 2015") + theme_bw() + geom_segment(x = 0,y = 0,xend=1000,yend=1000)
    ggsave(p, filename = file.path(outdir(),"data","plots","figureA18.pdf"),width = w, height = h)
    p
}

width_paper <- function(){5}
height_paper <- function(){3.5}

#' plot top 100 cities densities over time
#' https://github.com/floswald/LandUse.jl/issues/42
#'
#' Makes several plots of density and in particular, Figure 4a of the paper.
#'
#' @param w plot width in inches
#' @param h plot height in inches
#'
#' The input data is produced by \code{\link{export_pop_area}}.
#' We computed average urban density by weighting cities by their 1975 population counts.
#'
#' @export
plot_top100_densities <- function(save = FALSE,w=9,h=5,wp = width_paper(), hp = height_paper()){

    ff = export_pop_area()
    ff[year == 1876, year := 1870]
    ff[year == 1954, year := 1950]

    p1 = ggplot(ff[complete.cases(ff)], aes(factor(year), y = density_data)) + geom_violin(draw_quantiles = 0.5) + scale_y_log10(labels = scales::comma, name = "population / km2") + ggtitle("Urban Density over time in France") + scale_x_discrete(name = "year") + theme_bw()
    # label min/max city
    ll = ff[complete.cases(ff), .SD[,list(mi = min(density_data),
                        ma = max(density_data),
                        q5 = quantile(density_data,0.05),
                        q25 = quantile(density_data,0.25),
                        q50 = quantile(density_data,0.50),
                        q75 = quantile(density_data,0.75),
                        q95 = quantile(density_data,0.95),
                        milab = LIBGEO[which.min(density_data)],
                        malab = LIBGEO[which.max(density_data)])],by = list(year = factor(year))]
    p2 = p1 + geom_text(data = ll, mapping = aes(x = year, y = mi, label = milab), nudge_y = -.1)
    p3 = p2 + geom_text(data = ll, mapping = aes(x = factor, y = ma, label = malab),nudge_y = .1)

    p = list()
    p$violin <- p1
    p$violin_labelled <- p2 + geom_text(data = ll, mapping = aes(x = year, y = ma, label = malab),
                                        nudge_y = .1)
    # p$violin_quantiles <- p2 + geom_point(data = ll, mapping = aes(x = factor, y = ma, label = malab),
                                        # nudge_y = .1)

    llm = melt.data.table(ll[,.(year,q5,q25,q50,q75,q95)], id.vars = "year", variable.name = "Quantile")
    p$quantiles <- ggplot(data = llm[Quantile %like% "q"],
                          mapping = aes(x = year, y = value, group = Quantile,color = Quantile)) +
        geom_line(linewidth = 1.2) + geom_point(size = 3) + scale_y_log10(name = "Urban Density") +
        theme_bw()


    d4 = ff[,.(density = median(density_data),wdensity = weighted.mean(density_data, w = pop_1975)),by=year]
    d4 = d4[complete.cases(d4)]

    fall = d4[,round(max(density,na.rm = TRUE)/min(density,na.rm = TRUE),0)]
    wfall = d4[,round(max(wdensity,na.rm = TRUE)/min(wdensity,na.rm = TRUE),0)]
    p$ts = ggplot(d4[complete.cases(d4)],aes(x=year, y = density)) +
        geom_point(size = 3) +
        scale_x_continuous(breaks = seq(1870,2010, by = 20)) +
        scale_y_continuous(name = "median density (pop / km2)", breaks = c(3000,8000,15000,25000)) +
        geom_line(size = 1.1) + theme_bw() +
        ggtitle(paste0("Median Urban Density in France fell by Factor ",fall),subtitle = "Top 100 French cities") +
        theme(panel.grid.minor = element_blank())

    p$ts_w = ggplot(d4,aes(x=year, y = wdensity)) +
        geom_point(size = 3) +
        scale_x_continuous(breaks = seq(1870,2010, by = 20)) +
        scale_y_continuous(name = "weighted mean density (pop / km2)", breaks = c(3000,8000,15000,30000)) +
        geom_path(size = 1.1) + theme_bw() +
        ggtitle(paste0("Mean Urban Density in France fell by Factor ",wfall),subtitle = "Top 100 French cities") +
        theme(panel.grid.minor = element_blank()) + labs(caption = "mean weighted by population in 1975")

    p$ts_log = ggplot(d4,aes(x=year, y = density)) +
        geom_point(size = 3) +
        scale_x_continuous(breaks = seq(1870,2010, by = 20)) +
        scale_y_log10(name = "[log scale] median density (pop / km2)", breaks = c(3000,8000,15000,25000)) +
        geom_path(size = 1.1) + theme_bw() +
        ggtitle(paste0("Median Urban Density in France fell by Factor ",fall),subtitle = "Top 100 French cities") +
        theme(panel.grid.minor = element_blank())

    p$ts_log_w = ggplot(d4,aes(x=year, y = wdensity)) +
        geom_point(size = 3) +
        scale_x_continuous(breaks = seq(1870,2010, by = 20)) +
        scale_y_log10(name = "Average Urban Density (log scale)", breaks = c(4000,8000,15000,30000)) +
        geom_path(size = 1.1) + theme_bw() +
        ggtitle(paste0("Mean Urban Density in France fell by Factor ",wfall),subtitle = "Top 100 French cities") +
        theme(panel.grid.minor = element_blank()) + labs(caption = "mean weighted by population in 1975")

    p$ts_log_w_notitle = ggplot(d4,aes(x=year, y = wdensity)) +
        geom_point(size = 3,color = "red") +
        scale_x_continuous(name = "",breaks = seq(1870,2010, by = 20)) +
        scale_y_log10(name = "Average Urban Density (log scale)", breaks = c(4000,8000,15000,30000),limits = c(3900,41000)) +
        geom_path(size = 1.1,color = "red") + theme_bw() +
        theme(panel.grid.minor = element_blank())




    # time series for tops
    dtnow = ff[LIBGEO %in% top5now()]
    p$tstop = ggplot(dtnow[complete.cases(dtnow)],aes(x=year, y = density_data, color = LIBGEO)) +
        geom_line(size = 1.1) +
        scale_y_log10(name = "[log scale] median density (pop / km2)", breaks = c(3000,5000,10000,25000,66000)) +
        labs(color = "City") +
        scale_x_continuous(breaks = seq(1870,2010, by = 20)) +
        ggtitle(paste0("Urban Density by City"),subtitle = "Top 5 cities in 2015") +
        theme_bw() + theme(panel.grid.minor = element_blank())

    dtthen = ff[LIBGEO %in% top5then()]
    p$tstopthen = ggplot(dtthen[complete.cases(dtthen)],aes(x=year, y = density_data, color = LIBGEO)) +
        geom_line(size = 1.1) +
        scale_y_log10(name = "[log scale] median density (pop / km2)", breaks = c(3000,5000,10000,25000,66000)) +
        labs(color = "City") +
        scale_x_continuous(breaks = seq(1870,2010, by = 20)) +
        ggtitle(paste0("Urban Density by City"),subtitle = "Top 5 cities in 1866") +
        theme_bw() + theme(panel.grid.minor = element_blank())

    ff = ff[complete.cases(ff)]

    # cross section
    p$xsect <- list()
    p$xsect[[1]] <- ggplot(ff[complete.cases(ff)][LIBGEO %in% c("Paris", "Lyon")], aes(x = log(pop_data), y = density_data, color = LIBGEO)) + geom_point() + geom_line()
    p$xsect[[2]] <- ggplot(ff[complete.cases(ff)][LIBGEO %in% c("Toulouse", "Lille")], aes(x = pop_data, y = density_data, color = LIBGEO)) + geom_point() + geom_line()
    p$xsect[[3]] <- ggplot(ff[complete.cases(ff)][LIBGEO %in% c("Nantes", "Lyon")], aes(x = pop_data, y = density_data, color = LIBGEO)) + geom_point() + geom_line()
    p$xsect[[4]] <- ggplot(ff[complete.cases(ff)][LIBGEO %in% c("Toulouse", "Saint-Malo")], aes(x = pop_data, y = density_data, color = LIBGEO)) + geom_point() + geom_line()

    # small vs large cities
    issmall = ff[year == min(year), list(small = (pop_data < median(pop_data)), CODGEO)]
    ff = merge(ff, issmall, by = "CODGEO")
    d = ff[, list(pop = median(pop_data), density = median(density_data)) , by = list(year,small)]
    p$xsect[[5]] = ggplot(d, aes(x = pop, y = density, color = small)) + geom_point() + geom_line()
    p$xsect[[6]] = ggplot(d, aes(x = pop, y = density, color = factor(year))) + geom_point() + geom_line()

    if (save) {
        ggsave(p$violin, width = w, height = h,filename = file.path(outdir(),"data","plots","figureA12.pdf"))
        ggsave(p$quantiles, width = w, height = h,filename = file.path(outdir(),"data","plots","densities-quantiles.pdf"))
        ggsave(p$violin_labelled, width = w, height = h,filename = file.path(outdir(),"data","plots","densities-violins-labels.pdf"))
        ggsave(p$ts, width = w, height = h,filename = file.path(outdir(),"data","plots","densities-time.pdf"))
        ggsave(p$ts_log, width = w, height = h,filename = file.path(outdir(),"data","plots","densities-time-log.pdf"))
        ggsave(p$ts_w, width = w, height = h,filename = file.path(outdir(),"data","plots","densities-time-wtd.pdf"))
        ggsave(p$ts_log_w, width = w, height = h,filename = file.path(outdir(),"data","plots","densities-time-log-wtd.pdf"))
        ggsave(p$ts_log_w_notitle, width = wp, height = hp,filename = file.path(outdir(),"data","plots","figure4a.pdf"))
        ggsave(p$tstop, width = w, height = h,filename = file.path(outdir(),"data","plots","densities-time-top5now-city.pdf"))
        ggsave(p$tstopthen, width = w, height = h,filename = file.path(outdir(),"data","plots","figureA13.pdf"))

    }
    p

}


#' Figure 3: Plot urban area and population growth
#'
#' Figure 3: sum of all urban population and sum of all urban land.
#' @export
figure3 <- function(width = 5, height = 3.5){
    y = export_pop_area()
    y[year == 1876, year := 1870]
    y[year == 1954, year := 1950]
    y[year == 1999, year := 2000]

    # x = y[,.(Population = weighted.mean(pop_data, w = pop_data), Area = weighted.mean(area_data, w = pop_data)),by=year]
    x = y[,.(Population = sum(pop_data), Area = sum(area_data)),by=year]
    x = x[complete.cases(x)]
    x = cbind(x[,.(year)],x[, lapply(.SD, function(z) z / z[1]), .SDcols = 2:3])
    mx = melt(x, id.vars = "year")
    pl = ggplot(mx, aes(x = year, y = value, color = variable, linetype = variable)) +
        geom_line(linewidth = 1) + geom_point(size = 3) +
        scale_x_continuous(name = "",breaks = seq(1870,2010, by = 20)) +
        scale_y_log10(name = "1870 = 1") + theme_bw() +
        theme(legend.position = "bottom",legend.margin=margin(t=-20),panel.grid.minor = element_blank()) +
        scale_color_manual(name = NULL,values = c("Population" = "red", "Area" = "orange"))  +
        scale_linetype_manual(name = NULL,values = c("Population" = "solid", "Area" = "dashed"))

    ggsave(plot = pl, filename = file.path(outdir(),"data","plots","figure3.pdf"),
           height = height, width = width)
    pl
}


#' Produce Figure 5
#'
#' Uses 100 city sample and makes 2 scatters for rural price vs urban density
#' and urban population vs urban density, both for year 2000.
#' @export
figure5 <- function(){
    d = pra_stack(topn = 200)
    d = d[year == 2000][!is.na(price)]

    # create 10 bins for price and pop each
    popbreaks = quantile(d$pop_data,seq(0,1,length.out = 11))
    poplabels = zoo::rollmean(popbreaks, 2)
    pricebreaks = quantile(d$price,seq(0,1,length.out = 11),na.rm = TRUE)
    pricelabels = zoo::rollmean(pricebreaks, 2)



    d[, popbin := cut(pop_data,breaks = popbreaks, labels = FALSE,include.lowest = TRUE)]
    d[, pricebin := cut(price,breaks = pricebreaks, labels = FALSE,include.lowest = TRUE)]


    popd = d[, list(density  = mean(density_data)), by = popbin][order(popbin)]
    popd$pop = poplabels
    priced = d[, list(density  = mean(density_data)), by = pricebin][order(pricebin)]
    priced$price = pricelabels

    price = ggplot(priced, aes(x = log(price), y = log(density))) +
        ggplot2::theme_bw() +
        ggplot2::geom_point(color = "red",size = 3) +
        ggplot2::geom_smooth(method = "lm") +
        scale_x_continuous(name = "log Price per ha") + scale_y_continuous(name = "log Urban Density")


    pop = ggplot(popd, aes(x = log(pop), y = log(density))) +
        ggplot2::theme_bw() +
        ggplot2::geom_point(color = "red",size = 3) +
        ggplot2::geom_smooth(method = "lm") +
        scale_x_continuous(name = "log Urban Population") + scale_y_continuous(name = "log Urban Density")


    ggsave(plot = price,
           filename = file.path(outdir(),"data","plots","figure5-a.pdf"),height = height_paper(), width = width_paper())
    ggsave(plot = pop,
           filename = file.path(outdir(),"data","plots","figure5-b.pdf"),height = height_paper(), width = width_paper())

    list(price,pop)

}

#' Figure 4b: Density decline in Paris
#'
#' Comparing central and avergage density in Paris over time.
#' @export
figure4b <- function(width = 5, height = 3.5){

    paris = data.table(paris_central_density())
    paris
    # shift some years to closest round date
    paris[year == 1876, year := 1870]
    paris[year == 1954, year := 1950]
    paris[year == 1999, year := 2000]

    y = export_pop_area()
    y = y[LIBGEO == "Paris", .(Paris = density_data), by = year]
    y[year == 1876, year := 1870]
    y[year == 1954, year := 1950]
    y[year == 1999, year := 2000]


    y = merge(y,paris,by = "year", all.x = TRUE)
    setnames(y,"CentralDensity","Central Paris")
    mx = melt(y, id.vars = "year")
    colors <- c("Paris" = "red","Central Paris" = "orange")
    pl = ggplot(y, aes(x = year, y = Paris, color = "Paris")) +
        geom_line(size = 1, linetype = "solid") + geom_point(size = 3) +
        scale_x_continuous(name = "",breaks = seq(1870,2010, by = 20)) +
        scale_y_log10(name = "Urban Density (log scale)", breaks = c(5000,10000,20000,40000), limits = c(4800,60000)) + theme_bw()

    pl2 = pl + geom_line(data= paris,aes(x = year, y = CentralDensity, color = "Central Paris"), size = 1,linetype = "dashed") +
        geom_point(data= paris,aes(x = year, y = CentralDensity, color = "Central Paris"), size = 3) +
        theme(legend.position = "bottom",legend.margin=margin(t=-20),panel.grid.minor = element_blank()) +
        scale_color_manual(values = colors,name = NULL)

    ggsave(plot = pl2, filename = file.path(outdir(),"data","plots","figure4b.pdf"),
           height = height, width = width)
    pl2
}



#' plot top 100 cities densities cutoff sensitivity
#'
#'
#' @param w plot width in inches
#' @param h plot height in inches
#'
#' @export
plot_top100_cutoff <- function(save = FALSE,w=9,h=5){
    # for all files with this structure
    z = cutoff_sensitivity(overwrite = TRUE)
    y = lapply(names(z), function(x){
        y = z[[x]]
        y[,cutoff := x]
        y
    })
    f = rbindlist(y)
    f[,density := pop / area]
    ff = f[,list(LIBGEO,year,density,rank,pop, cutoff)]
    ff = ff[!is.na(density)]
    ff[year == 1876, year := 1870]  # make years consistent


    p = list()

    # aggregate time series
    d4 = ff[,.(density = median(density),wdensity = weighted.mean(density, w = pop)),by=list(year, cutoff)]
    fall = d4[,round(max(density)/min(density),0), by = cutoff]
    wfall = d4[,round(max(wdensity)/min(wdensity),0), by = cutoff]
    p$ts = ggplot(d4,aes(x=year, y = density, color = cutoff)) +
        geom_point() +
        scale_x_continuous(breaks = d4[,year]) +
        scale_y_continuous(name = "median density (pop / km2)", breaks = c(3000,5000,10000,15000,20000, 25000)) +
        geom_path(size = 1.1) + theme_bw() +
        ggtitle("Median Urban Density Cutoff Sensitivity") +
        theme(panel.grid.minor = element_blank())

    p$ts_w = ggplot(d4,aes(x=year, y = wdensity, color = cutoff)) +
        geom_point() +
        scale_x_continuous(breaks = d4[,year]) +
        scale_y_continuous(name = "weighted mean density (pop / km2)", breaks = c(3000,5000,10000,15000,20000, 25000, 30000,35000)) +
        geom_path(size = 1.1) + theme_bw() +
        ggtitle("Mean Urban Density Cutoff Sensitivity") +
        theme(panel.grid.minor = element_blank()) + labs(caption = "mean weighted by population")

        if (save) {
            ggsave(p$ts, width = w, height = h,filename = file.path(outdir(),"data","plots","figureA15.pdf"))
            ggsave(p$ts_w, width = w, height = h,filename = file.path(outdir(),"data","plots","figureA16.pdf"))
        }
    p
}


#' Plot GHSL Built Grids
#'
#' make plots for top 5 cities
#'
GHSL_plots <- function(){
    MC = measure_cities(topn = 200)
    L = MC$cropped
    ns = names(L[["1975"]])[c(1:5,which(grepl(L[["1975"]],pattern = "Reims")),which(grepl(L[["1975"]],pattern = "Fréjus")))]

    OL = list()
    OC = list()  #cuts
    for (ci in ns) {
        name = L[["1975"]][[ci]]$cityname
        ss = raster::stack(L[["1975"]][[ci]]$built,
                           L[["1990"]][[ci]]$built,
                           L[["2000"]][[ci]]$built,
                           L[["2015"]][[ci]]$built
                           )
        OL[[ci]] = rasterVis::levelplot(ss,
                                  xlab = NULL,
                                  ylab = NULL,
                                  scales=list(draw=FALSE),
                                  names.attr=paste0(name,' ', GHS_years()))
        lattice::trellis.device(pdf, file=file.path(dataplots(),paste0("GHSL-Built-",name,".pdf")),height=7, width=8)
        print(OL[[ci]])
        dev.off()

        # rename marseille and bordeaux
        file.rename(file.path(dataplots(),"GHSL-Built-Marseille.pdf"),file.path(dataplots(),"figureA10.pdf"))
        file.rename(file.path(dataplots(),"GHSL-Built-Bordeaux.pdf"),file.path(dataplots(),"figureA11.pdf"))

        # cut outs
        # if (name %in% c("Paris","Reims")) {
            cus = lapply(GHS_years(), function(x) {L[[paste(x)]][[ci]]$built_cut})
            # cus = lapply(GHS_years(), function(x) {L[[paste(x)]][[ci]]$built[is.na(L[[paste(x)]][[ci]]$inverse_mask), drop = FALSE]})
            # fix extents
            ex15 = raster::extent(L[["2015"]][[ci]]$built)
            for (il in 1:4){
                cus[[il]] <- raster::extend(cus[[il]],ex15)
            }
            futile.logger::flog.info("doing cut outs %s", name)
            ss = raster::stack(cus, quick = TRUE)
            areas = round(unlist(lapply(GHS_years(), function (x) MC$measured[[paste(x)]][[ci]]$area)), 1)
            OC[[ci]] = rasterVis::levelplot(ss,
                                            xlab = NULL,
                                            ylab = NULL,
                                            scales=list(draw=FALSE),
                                            names.attr=paste0(name,' ', GHS_years(), ": ",areas," km2"))
            lattice::trellis.device(pdf, file=file.path(dataplots(),paste0("GHSL-Built-Cut-",name,".pdf")),height=7, width=8)
            print(OC[[ci]])
            dev.off()
        # }



    }
    list(OL,OC)
}


paris_arrondissements_areas <- function(readdisk = TRUE){
    if (readdisk) {
        x = readRDS(file.path(datadir(),"paris-arr-areas.Rds"))
    } else {
        x = sf::st_read(file.path(datadir(), "communes-20150101-100m-shp","communes-20150101-100m.shp")) %>%
            filter(as.numeric(insee) > 75100 & as.numeric(insee) < 75121) %>%
            mutate(area = units::set_units(surf_m2, m^2), CODGEO = insee) %>% 
            mutate(area = units::set_units(area, km^2)) %>% 
            sf::st_set_geometry(NULL) %>% 
            dplyr::select(CODGEO,area)

        saveRDS(x, file.path(datadir(),"paris-arr-areas.Rds"))
    }
    x
}


#' Generate Central Paris Density
#' 
#' We know both area and population for arrondissements 1-6 in Paris
#' 
#' We call those *central paris*. We will use this data to confront with *average Paris*.
paris_central_density <- function(){
    a = paris_arrondissements_areas(readdisk = FALSE)
    readpop() %>%
        filter(DEP == 75) %>%
        left_join(a, by = "CODGEO") %>%
        filter(as.integer(CODGEO) < 75107) %>%
        filter(year %in% c(1876,1954,1975,1990,1999,2015)) %>%
        group_by(year) %>%
        summarise(CentralDensity = as.numeric(sum(population) / sum(area)))
}



plot_sat_densities <- function(){
    z = readRDS(file.path(outdir(),"data","france_final.Rds"))
    x = z[type == "satellite" ,list(year,p50,p90,p10,CODGEO,LIBGEO,rank,area,logarea=log(area),pop,logpop = log(pop))]

    parea = list()
    ppop = list()

    ppop$q10 = ggplot(x[rank < 11],aes(x = logpop, y = p10, color = LIBGEO, label = year, group = LIBGEO)) + geom_point() + geom_path() + geom_text(hjust = 0,nudge_x = 0.05) + ggtitle("10th percentile pop density",subtitle= "how did the lowest density parts of each city develop?")
    ppop$q50 = ggplot(x[rank < 11],aes(x = logpop, y = p50, color = LIBGEO, label = year, group = LIBGEO)) + geom_point() + geom_path() + geom_text(hjust = 0,nudge_x = 0.05) + ggtitle("median pop density",subtitle= "how did the median density parts of each city develop?")
    ppop$q90 = ggplot(x[rank < 11],aes(x = logpop, y = p90, color = LIBGEO, label = year, group = LIBGEO)) + geom_point() + geom_path() + geom_text(hjust = 0,nudge_x = 0.05) + ggtitle("90th percentile pop density",subtitle= "how did the densest parts of each city develop?")
    lapply(ppop, function(x) theme_set(theme_bw()))


    parea$q10 = ggplot(x[rank < 11],aes(x = logarea, y = p10, color = LIBGEO, label = year, group = LIBGEO)) + geom_point() + geom_path() + geom_text(hjust = 0,nudge_x = 0.05) + ggtitle("10th percentile pop density",subtitle= "how did the lowest density parts of each city develop?")
    parea$q50 = ggplot(x[rank < 11],aes(x = logarea, y = p50, color = LIBGEO, label = year, group = LIBGEO)) + geom_point() + geom_path() + geom_text(hjust = 0,nudge_x = 0.05) + ggtitle("median pop density",subtitle= "how did the median density parts of each city develop?")
    parea$q90 = ggplot(x[rank < 11],aes(x = logarea, y = p90, color = LIBGEO, label = year, group = LIBGEO)) + geom_point() + geom_path() + geom_text(hjust = 0,nudge_x = 0.05) + ggtitle("90th percentile pop density",subtitle= "how did the densest parts of each city develop?")
    lapply(parea, function(x) theme_set(theme_bw()))


    setkey(x,CODGEO,year)
    x[ , agrowth_pct := .SD[,100*(area[.N] - area[1])/area[1]],by = CODGEO]
    x[ , agrowth := .SD[,area[.N] - area[1]],by = CODGEO]
    x[ , pgrowth := .SD[,pop[.N] - pop[1]],by = CODGEO]
    x[ , pgrowth_pct := .SD[,100*(pop[.N] - pop[1])/pop[1]],by = CODGEO]

    # summarize: percent growth for all cols
    s = x[ , lapply(.SD,function(y) 100*(y[.N] - y[1])/y[1]), .SDcols = c("p10","p50","p90", "area","pop"),by = .(CODGEO,LIBGEO)]

    # growth plots
    pg = list()

    pg$p10 = ggplot(s,aes(x=area,y=p10)) + geom_point() + scale_x_continuous("% change in area") + scale_y_continuous("% change in density") + geom_smooth(method = "lm") + ggtitle("% change in density at 10-th quantile", subtitle = "Change 1975-2015")
    pg$p50 = ggplot(s,aes(x=area,y=p50)) + geom_point() + scale_x_continuous("% change in area") + scale_y_continuous("% change in density")+ geom_smooth(method = "lm") + ggtitle("% change in density at 50-th quantile", subtitle = "Change 1975-2015")
    pg$p90 = ggplot(s,aes(x=area,y=p90)) + geom_point() + scale_x_continuous("% change in area") + scale_y_continuous("% change in density")+ geom_smooth(method = "lm") + ggtitle("% change in density at 90-th quantile", subtitle = "Change 1975-2015")

    lapply(pg, function(x) theme_set(theme_bw()))
    return(list(growth = pg, area = parea, pop = ppop))

}


plot_paris <- function(d){
    pl = d %>%
        dplyr::filter(DEP == 75) %>%
        dplyr::mutate(Arrond = as.factor(as.numeric(CODGEO)-75100)) %>%
        ggplot2::ggplot(aes(x = year, y = population, group = CODGEO, color = Arrond)) + ggplot2::geom_line(size=1.1) + ggplot2::scale_y_continuous(labels = scales::comma) + ggplot2::labs(title = "Central Paris Population by Arrondissement",subtitle = "Equivalent to `Density` (Area is constant)") + ggplot2::theme_bw()
    ggplot2::ggsave(file.path(outdir(),"plots","paris.pdf"),plot = pl, width = 8, height = 6)
    pl

}


#' Plot Relative Population Growth
#'
plot_rel_popg <- function(){
    x = lapply(c(100,200), function(z) {export_pop_area(topn = z)})
    n = c("100","200")
    pl = list()

    for (i in 1:2){
        setkey(x[[i]], CODGEO, year)
        x[[i]][, rankg := factor(fcase(rank == 1, "1 (Paris)",
                                       rank < 11 & rank > 1, "2-10",
                                       rank < 51 & rank > 10, "11-50",
                                       rank < 101 & rank > 50, "51-100",
                                       rank < 150 & rank > 99, "100-149",
                                       rank > 149, ">= 150" ),
                                 levels= c("1 (Paris)","2-10","11-50","51-100","100-149",">= 150"))]
        if (i == 1){
            brks = c(1870,1954,1975,1990,2000,2015)


            d1 = copy(x[[i]][year > 1950])
            d1 = d1[      , pop_growth := pop_data/ .SD[year == min(year),pop_data],by = CODGEO]
            d2 = copy(x[[i]][  , pop_growth := pop_data/ .SD[year == min(year),pop_data],by = CODGEO])
            dx1 = d1[, .(meangrowth = mean(pop_growth)), by = .(year, ranks = rankg)]
            dx2 = d2[, .(meangrowth = mean(pop_growth)), by = .(year, ranks = rankg)]
            pl[[1]] = ggplot(dx1, aes(x = year, y = meangrowth, color = ranks)) +
                ggplot2::geom_line(linewidth = 1.2) +
                scale_x_continuous(breaks = brks) + geom_point(size = 2) +
                theme_bw() + scale_y_continuous("Relative Population Growth") +
                ggtitle(paste0("Population Growth in the top ",n[i]," French Cities starting in the 1950s"))
            ggplot2::ggsave(file.path(outdir(),"data","plots",paste0("relpop-growth-1950-",n[i],".pdf")),plot = pl[[1]], width = 8, height = 6)
            pl[[2]] = ggplot(dx2, aes(x = year, y = meangrowth, color = ranks)) +
                ggplot2::geom_line(linewidth = 1.2) +
                scale_x_continuous(breaks = brks) + geom_point(size = 2) +
                theme_bw() + scale_y_continuous("Relative Population Growth") +
                ggtitle(paste0("Population Growth in the top ",n[i]," French Cities"))
            ggplot2::ggsave(file.path(outdir(),"data","plots",paste0("relpop-growth-1870-",n[i],".pdf")),plot = pl[[2]], width = 8, height = 6)


        } else {
            brks = c(1975,1990,1999,2015)
            x[[i]][ , pop_growth := pop_data/ .SD[year == min(year),pop_data],by = CODGEO]

            dx = x[[i]][, .(meangrowth = mean(pop_growth)), by = .(year, ranks = rankg)]
            pl[[i+1]] = ggplot(dx, aes(x = year, y = meangrowth, color = ranks)) +
                ggplot2::geom_line(linewidth = 1.2) +
                scale_x_continuous(breaks = brks) + geom_point(size = 2) +
                theme_bw() + scale_y_continuous("Relative Population Growth") +
                ggtitle(paste0("Population Growth in the top ",n[i]," French Cities"))
            ggplot2::ggsave(file.path(outdir(),"data","plots",paste0("relpop-growth-",n[i],".pdf")),plot = pl[[i+1]], width = 8, height = 6)

        }
    }
    list(pl,x)
}



#' Plot Reims
#'
#' make plot for motivation of slides
plot_reims <- function(){
    p = readpop()
    y=p %>% dplyr::filter(CODGEO=="51454", year %in% c(1876,2015))
    y %<>% dplyr::mutate(area = c(50.52,3.29),density = population / area)
    d1 = data.table(variable = c("population","area","density"), increase = unlist(c(y[1,"population"] / y[2,"population"], y[1,"area"] / y[2,"area"], NA)))


    d2 = data.table(variable = factor(c("population","area","density"), levels = c("population","area","density")), increase = unlist(c(y[1,"population"] / y[2,"population"], y[1,"area"] / y[2,"area"], -y[2,"density"] / y[1,"density"])))

    cols <- c("area" = "red", "population" = "green", "density" = "grey")

    p1 = ggplot(d1,aes(x=variable,increase)) + geom_col(aes(fill=variable),alpha=0.6,color = "black") + theme_bw() + scale_fill_manual(values = cols) + scale_y_continuous(name = "Increased by Factor",limits = c(-10,16),breaks = c(-7,0,3,10,15),minor_breaks = NULL) + geom_hline(yintercept = 0,size = 1) + ggtitle("Reims from 1866 to 2015")  + scale_x_discrete(name = "") + theme(legend.position = "none")

    p2 = ggplot(d2,aes(x=variable,increase)) + geom_col(aes(fill=variable),alpha=0.6,color = "black") + theme_bw() + scale_fill_manual(values = cols) + scale_y_continuous(name = "Increased by Factor",limits = c(-10,16),breaks = c(-7,0,3,10,15),minor_breaks = NULL) + geom_hline(yintercept = 0,size = 1) + ggtitle("Reims from 1866 to 2015") + scale_x_discrete(name = "")+ theme(legend.position = "none")

    ggsave(plot = p1, filename=file.path(dataplots(),"reims1.pdf"),width=7,height=5)
    ggsave(plot = p2, filename=file.path(dataplots(),"reims2.pdf"),width=7,height=5)

    return(list(p1,p2))
}


#' Compare Area Measures to Shlomo Angel's Results
#'
#' We obtained data from Shlomo Angel's Atlas of Urban Expansion
#' which contains area estimates fro Paris since 1800. This function
#' makes a plot combining both measures.
plot_shlomo <- function(pw = 8, ph = 4.5){

    x = shcombine()
    pl = ggplot(x[year < 2016], aes(x = year, y = area, color = type)) + geom_line() + geom_point() + theme_bw() + ggtitle("Paris Area Measurements", subtitle = "Comparing With Shlomo Angel's Historical Data") + scale_y_continuous(name = "Urban Area (km2)") + labs(caption = "Our series combines manual and satellite.")
    ggsave(plot = pl, filename = file.path(dataplots(),paste0("figureA19.pdf")),
           width = pw, height = ph)
    pl
}

#' Plot CLC Landuse patterns
#'
CLC_plots <- function(){
    le = CLC_read_legend()
    cuts = CLC_bboxes(FALSE)

    OL = list()
    OL$cut = list()
    OL$notcut = list()
    for (insee in c(1:4,13)) {
        r <- cuts[[insee]]$cut
        v = raster::values(r)
        raster::values(r) <- raster::as.factor(v)
        rat <- raster::levels(r)[[1]]
        rat = merge.data.table(rat, le[,list(mcode,legend,color)],by.x = "VALUE", by.y = "mcode")
        setDT(rat)
        setcolorder(rat, "ID")
        setkey(rat,ID)
        cols = rat[,color]
        rat[, c("VALUE","color") := NULL]

        levels(r) <- rat

        OL$cut[[insee]] <- rasterVis::levelplot(r,col.regions = cols,xlab=NULL, ylab=NULL, scales=list(draw=FALSE))
        lattice::trellis.device(pdf, file=file.path(notsharedplots(),paste0("CLC-",cuts[[insee]]$cityname,".pdf")),height=4.5, width=8)
        print(OL$cut[[insee]])
        dev.off()


        # now inside city
        rn <- cuts[[insee]]$notcut
        v = raster::values(rn)
        raster::values(rn) <- raster::as.factor(v)
        levels(rn) <- rat

        OL$notcut[[insee]] <- rasterVis::levelplot(rn,col.regions = cols,xlab=NULL, ylab=NULL, scales=list(draw=FALSE))
        lattice::trellis.device(pdf, file=file.path(notsharedplots(),paste0("CLC-inside-",cuts[[insee]]$cityname,".pdf")),height=4.5, width=8)
        print(OL$notcut[[insee]])
        dev.off()

    }
    OL
}



#' TWFE plots of density dataset
#'
#' Two way fixed effect on density data
twfe <- function(){
    # shapefile
    sh = sf::st_read(file.path(datadir(), "departements-shp","departements-20140306-100m.shp")) %>%
        dplyr::filter(code_insee %in% sprintf("%02d", (1:95)))

    # density fixed effects:
    f = readRDS(file.path(outdir(),"data","france_final.Rds"))
    f[,density := pop / area]
    ff = f[,list(LIBGEO,CODGEO,year,density,pop)]
    ff = ff[complete.cases(ff)]
    fx = fixest::feols(log(density) ~ 1 | CODGEO + year, data = ff)
    fe = fixest::fixef(fx)
    dfe = data.table(cityfe = fe$CODGEO, CODGEO = stringr::str_pad(names(fe$CODGEO), width = 5,side = "left",pad = 0))
    setkey(dfe, CODGEO)

    r = fread(file.path(datadir(),"cog_ensemble_2020_csv","communes2020.csv"),key = "CODGEO", select = c("CODGEO","LIBGEO","REG","DEP"))
    dfer = r[dfe]
    dfer[is.na(LIBGEO), c("LIBGEO","REG","DEP") := list("Paris",11,"75")]
    dfer <- rbind(dfer, data.table(LIBGEO = "Paris",REG = 11, DEP = "77",CODGEO = "75060", cityfe = dfer[DEP == "75",cityfe]))
    dfer <- rbind(dfer, data.table(LIBGEO = "Paris",REG = 11, DEP = "78",CODGEO = "75060", cityfe = dfer[DEP == "75",cityfe]))
    dfer <- rbind(dfer, data.table(LIBGEO = "Paris",REG = 11, DEP = "91",CODGEO = "75060", cityfe = dfer[DEP == "75",cityfe]))
    # dfer <- rbind(dfer, data.table(LIBGEO = "Paris",REG = 11, DEP = "92",CODGEO = "75060", cityfe = dfer[DEP == "75",cityfe]))
    # dfer <- rbind(dfer, data.table(LIBGEO = "Paris",REG = 11, DEP = "93",CODGEO = "75060", cityfe = dfer[DEP == "75",cityfe]))
    # dfer <- rbind(dfer, data.table(LIBGEO = "Paris",REG = 11, DEP = "94",CODGEO = "75060", cityfe = dfer[DEP == "75",cityfe]))
    dfer <- rbind(dfer, data.table(LIBGEO = "Paris",REG = 11, DEP = "95",CODGEO = "75060", cityfe = dfer[DEP == "75",cityfe]))

    # merge to departement shapefile
    # average for multiple cities in a dep
    dfer <- dfer[ , list(cityfe = mean(cityfe)), by = DEP ]
    shr = dplyr::inner_join(sh,dfer, by = c("code_insee" = "DEP"))
    # plot(sh[,"cityfe"])


}





