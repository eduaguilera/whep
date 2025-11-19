whep_polities <- whep_read_file("whep_polities", type = "geojson")

usethis::use_data(whep_polities, overwrite = TRUE)
