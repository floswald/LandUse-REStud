shape_france_depts <- function(overwrite = FALSE){
    if (overwrite){
        sh = sf::st_read(file.path(datadir(), "departements-shp","departements-20140306-100m.shp")) %>%
            dplyr::filter(code_insee %in% sprintf("%02d", (1:95)))

        f = sh %>% dplyr::summarise(n = n(), do_union = TRUE)
        saveRDS(sh, file.path(outdatadir(), "shape-departements.Rds"))
        saveRDS(f, file.path(outdatadir(), "shape-France.Rds"))
    } else{
        sh = readRDS(file.path(outdatadir(), "shape-departements.Rds"))
        f = readRDS(file.path(outdatadir(), "shape-France.Rds"))
    }
    list(sh,f)
}

wheat_plots <- function(yr = 2000,ncat = 4,wi = 6, he = 5){
    # x = data.table::fread(file.path(yielddir(),
                                # "2021-001_Schauberger-et-al_Data_FILTERED",
                                # "wheat_total_data_1900-2018_FILTERED.txt"))
    # get yields data computed by nicolas - is x but with dep column
    x = data.table(haven::read_dta(file.path(IVdir(),"IV_yields.dta")))
    x[, dep := stringr::str_pad(as.numeric(dep), pad = "0",width = 2, side = "left")]
    x = x[year == yr]
    # get share of SAU in cereales
    share = data.table(haven::read_dta(file.path(IVdir(),"AgriLandUse.dta")))
    share[, dep := stringr::str_pad(as.numeric(dep), pad = "0",width = 2, side = "left")]
    share[,wheat_share_2000 := ble2000 / sau2000]
    share[Département == "Lozère", wheat_share_2000 := 0.001] # almost zero
    xs = merge(x, share[,.(dep,wheat_share_2000)])

    # make categories for both outcomes
    brks = c(0,0.05,0.1,0.2,1)
    labs = c("[0%,5%]", "(5%,10%]","(10%,20%]","(20%,100%]")
    xs[, c("ycat","scat") := .(ggplot2::cut_number(yield_wheat, n = ncat),
           cut(wheat_share_2000,brks,include.lowest = TRUE, labels = FALSE))]
    xs[, sfac := factor(scat,labels = labs)]
           # cut(wheat_share_2000,c(0,round(quantile(wheat_share_2000,na.rm = TRUE), 2)[-c(1,5)],1),include.lowest = TRUE, dig.lab = 1))]
    # xs[, c("ycat","scat") := .(ggplot2::cut_number(yield, n = ncat),
                               # ggplot2::cut_number(wheat_share_2000, n = ncat, dig.lab = 2))]


    sh = shape_france_depts(overwrite = TRUE)
    sx = merge(sh[[1]], xs, by.x = "code_insee", by.y = "dep")
    pls = sx %>% dplyr::filter( !is.na(wheat_share_2000))
    ply = sx %>% dplyr::filter( !is.na(yield_wheat) )

    # 5 colors
    mapyield = ggplot(ply) + geom_sf(aes(fill = ycat), lwd = 0.1) +
        scale_fill_manual(name = NULL,values = sf::sf.colors(ncat)) +
        notheme() +
        theme(legend.position = c(0.15,0.35))
    #+ ggtitle(paste("Wheat Yields")) +theme(plot.title = element_text(hjust = 0))

    mapshare = ggplot(pls) + geom_sf(aes(fill = sfac), lwd = 0.1) +
        theme(legend.position = c(0.15,0.35)) +
        scale_fill_manual(name = NULL , values = sf::sf.colors(ncat)) + notheme()

    ggsave(plot = mapyield, file.path(dataplots(),paste0("wheat-yield-",yr,".pdf")), width = wi, height = he)
    ggsave(plot = mapshare, file.path(dataplots(),paste0("wheat-share-",yr,".pdf")), width = wi, height = he)

    if (yr == 2000){
        file.rename(file.path(dataplots(),paste0("wheat-yield-",yr,".pdf")),file.path(dataplots(),"figureA21a.pdf"))
        file.rename(file.path(dataplots(),paste0("wheat-share-",yr,".pdf")),file.path(dataplots(),"figureA21b.pdf"))
    }


    return(list(mapshare,mapyield, xs))

    # time series
    # grouping

    # gx = x[!is.na(yield_bin), list(yield = mean(yield,na.rm = TRUE),
    #                                area = mean(area, na.rm = TRUE)),
    #        by = list(year, yield_bin = factor(yield_bin))]
    #
    # ax = x[!is.na(area_bin), list(area = mean(area, na.rm = TRUE)),
    #        by = list(year, area_bin = factor(area_bin))]
    # tsyield = ggplot(gx, aes(x = year, y = yield, color = yield_bin)) + geom_line(size = 1) + theme_bw() +
    #     labs(color = "Quintile", title = paste0("Wheat Yield Across Departements")) + scale_y_continuous("Wheat Yield") +
    #     scale_color_manual(values = sf::sf.colors(5)) + theme(legend.position = "none")
    #
    # tsarea = ggplot(ax, aes(x = year, y = area, color = area_bin)) + geom_line(size = 1) + theme_bw() +
    #     labs(color = "Quintile", title = paste0("Wheat Area Across Departements ")) + scale_y_continuous("Wheat Area") +
    #     scale_color_manual(values = sf::sf.colors(5)) + theme(legend.position = "none")
    #
    # yield = cowplot::plot_grid(tsyield, mapyield, rel_widths = c(1,1.5))
    # area = cowplot::plot_grid(tsarea, maparea, rel_widths = c(1,1.5))
    # cowplot::save_plot(filename = file.path(dataplots(),paste0("wheat-yield-",yr,".pdf")),
    #                    plot = yield,ncol = 2,base_width = 4)
    # cowplot::save_plot(filename = file.path(dataplots(),paste0("wheat-area-",yr,".pdf")),
    #                    plot = area,ncol = 2,base_width = 4)
    #
    #
    # list(yield, area)
}




