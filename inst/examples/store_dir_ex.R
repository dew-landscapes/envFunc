# a list of settings
settings <- list(extent = list(polygons = "sa_ibrasub_xn"
                               , filt_col = NULL
                               , filt_level = NULL
                               , buffer = 0
                               , temp_ext = "P50Y"
                               )
                 , grain = list(x = 90
                                , y = 90
                                , z = "P50Y"
                                , taxonomic = "species"
                                )
                 , aoi = list(polygons = "lsa"
                              , filt_col = "LSA"
                              , filt_level = "GA"
                              , buffer = 0
                              )
                 )

# generate the full path to a store
out_dir <- store_dir(settings)

out_dir
