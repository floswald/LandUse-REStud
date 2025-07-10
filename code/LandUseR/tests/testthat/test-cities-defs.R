
context("city definitions")

x = merge_centers(overwrite = FALSE)
test_that("city center of Niort", {
    s = sf::st_as_sf(x[LIBGEO %in% c("Niort","Limoges","Rouen"), list(LIBGEO, CODGEO, center_x, center_y)], coords = c("center_x","center_y"), crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") %>%
        sf::st_transform(4326)

    mairies = data.frame(LIBGEO = c("Niort","Limoges","Rouen"), x = c(-0.465020, 1.260870, 1.099697), y = c(46.323658, 45.826664, 49.443339) )

    centers = s %>% sf::st_coordinates()
    d = s %>% left_join(mairies) %>% cbind(centers) %>% sf::st_set_geometry(NULL)
    expect_equal(d$x, d$X, tolerance = 0.01)
    expect_equal(d$y, d$Y, tolerance = 0.01)

})
