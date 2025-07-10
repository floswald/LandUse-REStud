
# Appellations Geographiques
# https://www.inao.gouv.fr/Publications/Donnees-et-cartes/Informations-geographiques

AOC_browse <- function(){browseURL("https://www.data.gouv.fr/fr/datasets/delimitation-des-aires-geographiques-des-siqo/")}

AOC_read <- function(simplify = TRUE){
    x = readRDS(file.path(AOCdir(),"shapefile","2021-12-22_delim_aire_geographique_shp.Rds"))
    if (simplify){
        sf::st_simplify(x, dTolerance = 2000) # 2000m
    }
}



AOC_count <- function(){
    a = AOC_read()
    sp = map_pra_prices2000_data() %>% sf::st_transform(sf::st_crs(a))
    AOC_count_imp(a,sp)
}

AOC_mv <-function(a,prod){
    mapview::mapview(AOC_prods(a,prod))
}

AOC_prods <- function(a,prod) {
    stopifnot(!(prod %in% table(a$type_prod)))
    a %>%
        dplyr::filter(type_ig == "AOC" & type_denom == "appellation" & grepl(prod,type_prod))
}

# https://gis.stackexchange.com/questions/323038/dissolve-only-overlapping-polygons-in-r-using-sf
AOC_count_imp <-function(a,sp){

    l = list()
    # l[[1]] = ggplot() + geom_sf(data = sp, aes(fill = price_cat), lwd = 0.1) + scale_fill_brewer(name = "Francs per hectare",palette = "YlOrRd") + theme_bw() + labs(title = "Prices")
    # xlims = ggplot_build(l[[1]])$layout$panel_scales_x[[1]]$range$range
    # ylims = ggplot_build(l[[1]])$layout$panel_scales_y[[1]]$range$range

    ds = sp %>% dplyr::select(price_cat) %>% mutate(type = "All")
    da = sp %>% dplyr::select(price_cat) %>% mutate(type = "All")

    prods <- c("Viandes","Fromages","Vins")
    for (ipr in 1:length(prods)) {
        pr = prods[ipr]
        logger::log_info("doing {pr}")

        xx = AOC_prods(a,pr)
        xxx = sf::st_cast(sf::st_union(xx),"POLYGON")
        i = sf::st_intersects(sp,xxx)
        isection = sf::st_intersection(sp,xxx)
        ds = rbind(ds, isection %>% dplyr::select(price_cat) %>% mutate(type = pr))
        da = rbind(da, sp[unlist(lapply(i,function(z){length(z)>0})), ] %>% dplyr::select(price_cat) %>% mutate(type = pr))
        # l[[ipr]] = ggplot() + geom_sf(data = isection, aes(fill = price_cat), lwd = 0.1) + scale_fill_brewer(name = "Francs",palette = "YlOrRd") + theme_bw() + scale_x_continuous(limits = xlims) + scale_y_continuous(limits = ylims) + labs(title = paste0(pr))

        sp[[pr]] <- unlist(lapply(i,length))
    }
    pl = ggplot(ds) + geom_sf(aes(fill = price_cat), lwd = 0.1) + scale_fill_brewer(name = "Francs",palette = "YlOrRd") + theme_bw() + facet_wrap(~type)

    pl2 = ggplot(da) + geom_sf(aes(fill = price_cat), lwd = 0.1) + scale_fill_brewer(name = "Francs",palette = "YlOrRd") + theme_bw() + facet_wrap(~type)

    ggsave(plot = pl, filename = file.path(AOCdir(),"output","AOCmaps.pdf"),w = 10,h=9)
    ggsave(plot = pl2, filename = file.path(AOCdir(),"output","AOCmaps2.pdf"),w = 10,h=9)

    # ap = cowplot::plot_grid(plotlist = l, nrow = 2, ncol = 2)
    #
    #
    list(sp,pl)
}

AOC_corrs <- function(spa){

        p = spa[,.SD, .SDcols = price:fromages]
        p[,price_cat := NULL]
        p = p[complete.cases(p)]
        p[ , logprice := log(price)]
        p[ , price := NULL]
        setcolorder(p, "logprice")
        pcorr = cor(p)
        pdf(file.path(AOCdir(),"output","corrs.pdf"), h = 10, w = 16)
        pairs(p, main = "AOC correlations")
        dev.off()

        pdf(file.path(AOCdir(),"output","corrplot.pdf"), h = 10, w = 16)

        corrplot::corrplot.mixed(pcorr,
                                 lower = "ellipse",
                                 upper = "number",
                                 tl.pos = "lt",
                                 diag = "l",
                                 mar=c(0,0,2,0),
                                 tl.col = "black",title = "AOC correlations")
        dev.off()


        # pdf(file.path(GAEZdir(),"output",iy,paste0(var,"-corrplot.pdf")), h = 10, w = 10)
        # corrplot::corrplot.mixed(pcorr,
        #                          lower = "ellipse",
        #                          upper = "number",
        #                          tl.pos = "lt",
        #                          diag = "l",
        #                          mar=c(0,0,2,0),
        #                          tl.col = "black",title = paste(gaez_varname(var),iy))
        # dev.off()
}

AOC_merge <- function(){
    x = pra_stack()
    a = AOC_count()
    da = data.table(a[[1]])
    da[,c("PRA_Lib","geometry","n","surf_m2","DEP","price","price_cat") := NULL]
    xa = merge(x, da, by = "PRA_Code")
    data.table::fwrite(xa, file = file.path(PRAdir(),"area-pop-prices-AOC.csv"))
    xa

}


