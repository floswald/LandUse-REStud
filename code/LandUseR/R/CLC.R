

# corine land cover https://land.copernicus.eu/pan-european/corine-land-cover

#' Crop CLC 2018 raster to 2015 French Cities
#'
#' load CLC raster and cut out our bounding boxes of French cities.
#' Boxes are obtained via \code{\link{measure_cities}}
CLC_bboxes <- function(overwrite = FALSE){
    if (overwrite) {
        ms = measure_cities()$cropped$`2015`
        if (!file.exists(file.path(notshareddata(),"france_CLC.tif"))) {
            futile.logger::flog.info("reading CLC raster")
            CLC = raster::raster(file.path(notshareddata(),"u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif"))
            # france
            CLCF = raster::crop(CLC,raster::extent(3100000, 4300000, 2100000, 3200000))
            # CLCF <- raster::projectRaster(CLCF, crs = raster::crs(ms[[1]]$inverse_mask), method = "ngb")  # does not work! it interpolates data values and fucks up everything
            raster::writeRaster(CLCF, file.path(notshareddata(),"france_CLC.tif"))
        } else {
            CLCF = raster::raster( file.path(notshareddata(),"france_CLC.tif"))
        }

        OL = list()
        citynames = c()
        allnames = names(ms)
        futile.logger::flog.info("reading CLC raster")

        for (ix in 1:length(ms)){
            # bounding box in CLC crs
            if (!is.null(ms[[ix]]$inverse_mask)){
                bb = sf::st_bbox(ms[[ix]]$inverse_mask)  # sf to the rescue
                bb_ll = sf::st_bbox(
                sf::st_transform(
                    sf::st_as_sfc(bb), crs = raster::projection(CLCF))
                )
                e = raster::extent(c(bb_ll["xmin"],bb_ll["xmax"],bb_ll["ymin"],bb_ll["ymax"]))
                OL[[ix]] <- list()
                OL[[ix]]$full <- raster::crop(CLCF, e)
                citynames = c(citynames,allnames[ix])

            
            

                # mask in CLC crs
                bb = sf::st_bbox(ms[[ix]]$city_mask)  # sf to the rescue
                bb_ll = sf::st_bbox(
                    sf::st_transform(
                        sf::st_as_sfc(bb), crs = raster::projection(CLCF))
                )
                ec = raster::extent(c(bb_ll["xmin"],bb_ll["xmax"],bb_ll["ymin"],bb_ll["ymax"]))
                OL[[ix]]$fullcity <- raster::crop(CLCF, ec)


                # inverse_mask in CLC crs
                ivm = raster::projectRaster(ms[[ix]]$inverse_mask, to = OL[[ix]]$full, method = "ngb")
                im = raster::projectRaster(ms[[ix]]$city_mask, to = OL[[ix]]$fullcity, method = "ngb")
                OL[[ix]]$cut  <- OL[[ix]]$full[ivm, drop = FALSE]
                OL[[ix]]$notcut  <- OL[[ix]]$fullcity[im, drop = FALSE]
                OL[[ix]]$cityname  <- ms[[ix]]$cityname
            }
        }
        names(OL) <- citynames
        saveRDS(OL,file.path(notshareddata(),"CLC-France-cropped.Rds"))
        OL
    } else {
        readRDS(file.path(notshareddata(),"CLC-France-cropped.Rds"))
    }
}

#' Read CLC classification
#'
CLC_read_legend <- function(){
    x = fread(file.path(notshareddata(),"u2018_clc2018_v2020_20u1_raster100m/Legend/CLC2018_CLC2018_V2018_20_QGIS.txt"))
    setnames(x, c("code","red","green","blue","alpha","label"))
    x[,color := rgb(red,green,blue, maxColorValue = alpha)]
    x[,mcode := factor(c(1:44,48))]  # get out of u2018_clc2018_v2020_20u1_raster100m/Legend/clc_legend_qgis_raster.qml
    x[mcode == 21, label := "Agr. Land w/ sign. areas of natural vegetation"]
    x[,legend := factor(code,labels = label)]
    # add aggregation of labels
    x[,agg_labels := NA_character_]
    x[,agg_code := NA_integer_]
    x[,agg_color := NA_character_]

    agri_green = "#00A600"
    urban_red = "#de0202"
    forest_brown = "#a85603"
    moors_black = "#000000"

    x[label %like% "urban fabric", c("agg_labels","agg_code","agg_color") := list("Discontinous Urban", 1, urban_red) ]
    x[label %like% "Fruit|Vineyards|Olive", c("agg_labels","agg_code","agg_color") := list("Fruits, Vineyards and Olives", 2, agri_green) ]
    x[label %like% "arable land|Rice|Permanently irrigated|Annual crops", c("agg_labels","agg_code","agg_color") := list("Arable Land", 3, agri_green) ]
    x[label %like% "Pastures|grassland", c("agg_labels","agg_code","agg_color") := list("Pasture and Grazing Fields", 4, agri_green) ]
    x[label %like% "Complex" | code == 243, c("agg_labels","agg_code","agg_color") := list("Mixed Farm Use", 5, agri_green) ]
    x[label %like% "Sport|Airports|Road|Green urban|Port", c("agg_labels","agg_code","agg_color") := list("Leisure and Transport Facilities", 6, urban_red) ]
    x[label %like% "Industrial|Mineral|Dump|Construction", c("agg_labels","agg_code","agg_color") := list("Industrial/Commercial Sites", 7, urban_red) ]
    x[label %like% "forest", c("agg_labels","agg_code","agg_color") := list("Forest", 8, forest_brown) ]
    x[label %like% "Sclerophyllous|Transitional|Moors|Sparsely", c("agg_labels","agg_code","agg_color") := list("Moors and Sparse Vegetation", 9, forest_brown) ]
    x[label %like% "Sea|Water|Coastal|Estuaries|Glaciers", c("agg_labels","agg_code","agg_color") := list("Water Bodies", 10, "#85fffb") ]
    x[label %like% "Intertidal|Inland marshes|Salt marshes|Beaches dunes|Salines|Bare rocks|Burnt areas|Peat bogs", c("agg_labels","agg_code","agg_color") := list("Marshes, Rock and Sand", 11, moors_black) ]

    x
}



#' Measure Landuse Around Cities with CLC
#'
#' takes cropped CLC rasters from \code{\link{CLC_bboxes}} for top 100 cities and counts the proportion of land
#' outside city falling into each category. We plot the top 15 categories of the CLC classification and provide our own aggregation of labels into 11 exhaustive classes of land use.
#'
CLC_measure <- function(overwrite = TRUE){
    le = CLC_read_legend()
    cuts = CLC_bboxes(overwrite = overwrite)

    r = lapply(cuts, function(z){ r <- raster::values(z$cut);
                                  r <- r[!is.na(r)];
                                  fr = factor(r, levels = levels(le[,mcode]));
                                  prop.table(table(fr))
        })

    # inside cities
    ir = lapply(cuts, function(z){ r <- raster::values(z$notcut);
    r <- r[!is.na(r)];
    fr = factor(r, levels = levels(le[,mcode]));
    prop.table(table(fr))
    })


    df = data.table(mcode = names(r[[1]]),
                    avg_prop = rowMeans(matrix(unlist(r),ncol = length(r), byrow = F)),
                    avg_prop_inside = rowMeans(matrix(unlist(ir),ncol = length(ir), byrow = F)))
    df2 = merge(df,le[,list(mcode,legend,color,agg_code,agg_labels,agg_color)])
    df2 = df2[order(avg_prop,decreasing = TRUE)]

    df_agg = df2[,list(avg_prop = sum(avg_prop), avg_prop_inside = sum(avg_prop_inside)),by = list(agg_labels,agg_color)]
    df_agg = df_agg[complete.cases(df_agg)]
    df_agg[, agg_labels := factor(agg_labels)]

    colnames = df2[, as.character(color)]
    names(colnames) <- df2[, as.character(legend)]
    gg = ggplot(df2[1:15], aes(x = reorder(legend,avg_prop) ,y = avg_prop)) + geom_bar(stat = "identity", aes(fill = legend)) + coord_flip() + ggtitle("Average Land Use Outside top 100 Cities")  + scale_fill_manual(values = colnames) + theme_bw()  + theme(legend.position = "none") + scale_x_discrete(name = "") + scale_y_continuous("Proportion")

    colnames = df_agg[, agg_color]
    names(colnames) <- df_agg[, agg_labels]
    gg_agg = ggplot(df_agg, aes(x = reorder(agg_labels,avg_prop) ,y = avg_prop)) + geom_bar(stat = "identity", aes(fill = agg_labels)) + coord_flip() + ggtitle("Average Land Use Outside top 100 Cities")  + scale_fill_manual(values = colnames) + theme_bw()  + theme(legend.position = "none") + scale_x_discrete(name = "") + scale_y_continuous("Proportion")

    fwrite(df2, file = file.path(notsharedoutput(),"CLC-landuse.csv"))
    fwrite(df_agg, file = file.path(notsharedoutput(),"CLC-landuse-agg.csv"))
    ggsave(gg, file = file.path(notsharedplots(),"CLC-landuse-top100.pdf"), width = 8 , height = 4.5)
    ggsave(gg_agg, file = file.path(notsharedplots(),"CLC-landuse-top100-agg.pdf"), width = 8 , height = 4.5)
    file.copy(file.path(notsharedplots(),"CLC-landuse-top100-agg.pdf"), file.path(notsharedplots(),"figureC2.pdf"))
    list(gg,gg_agg,df2)
}



