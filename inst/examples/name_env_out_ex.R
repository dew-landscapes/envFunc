
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
                 , reliability = list(rel_geo = 10000
                                      , rel_temp = "P10Y"
                                      )
                 , aoi = list(polygons = "lsa"
                              , filt_col = "LSA"
                              , filt_level = "GA"
                              , buffer = 0
                              )
                 )

# generate an 'out directory' based on those settings
settings$out_dir <- name_env_out(set_list = settings#[1:4]
                                 , base_dir = here::here("inst", "examples")
                                 , dir_with_context = TRUE
                                 )$path

settings$out_dir

# create the 'out directory'
fs::dir_create(settings$out_dir)

# put some junk files in the 'out directory'
purrr::map(1:10
           , \(x) tempfile(tmpdir = settings$out_dir)
           ) %>%
  fs::file_create()

# use name_env_out to return the list of files, based on the settings
files <- name_env_out(set_list = settings[1:4]
                      , base_dir = here::here("inst", "examples")
                      , dir_with_context = TRUE
                      , all_files = TRUE
                      # dots
                      , recurse = TRUE
                      ) %>%
  tidyr::unnest(cols = c(files)
                , keep_empty = TRUE
                )

files

# clean up
# fs::dir_delete(settings$out_dir) # only run this if you're sure it is safe
rm(settings, files)


