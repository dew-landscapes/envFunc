
settings <- list(extent = list(polygons = "sa_ibrasub_xn"
                               , filt_col = NULL
                               , filt_level = NULL
                               , bbox = TRUE
                               , buffer = 0
                               , temp_ext = "P50Y"
                               )
                 , grain = list(x = 90
                                , y = 90
                                , z = "P50Y"
                                , taxonomic = "species"
                                )
                 , reliability = list(rel_geo = 10000
                                      , rel_temp = "P10Y"
                                      )
                 )

settings$out_dir <- name_env_out(set_list = settings#[1:3]
                                 , base_dir = here::here("inst", "examples")
                                 )$path

settings$out_dir

fs::dir_create(settings$out_dir)

# create some junk files
purrr::map(1:10
           , \(x) tempfile(tmpdir = settings$out_dir)
           ) %>%
  fs::file_create()

files <- name_env_out(set_list = settings[1:3]
                      , base_dir = here::here("inst", "examples")
                      , all_files = TRUE
                      ) %>%
  tidyr::unnest(cols = c(files)
                , keep_empty = TRUE
                )

files

