#' Get taxa list from the Biological Database of South Australia (BDBSA)
#'
#' @param source Character. Where to source the BDBSA taxonomy. Either 'web' (default) or 'oracle' (for internal DEW use only).
#' @param url If `source` = "web", a character vector of url paths to the relevant taxonomy xlsx links on https://data.environment.sa.gov.au.
#' @param out_dir Output directory for downloaded taxonomy xlsx files if `source` == "web".
#' @param standardise_taxonomy Logical. Standardise the taxa to the ALA taxonomic backbone via `envClean::make_taxonomy`?
#' @param taxonomy If !`standardise_taxonomy`, an optional alternative existing taxonomy list containing
#' `lutaxa` and `taxonomy` elements, as produced by `envClean::make_taxonomy`.
#' @param taxonomy_file If `standardise_taxonomy`, file path to save taxonomy results from `envClean::make_taxonomy`.
#' @param needed_ranks If `standardise_taxonomy`, character vector of taxonomic ranks required in the
#' returned list, as per `envClean::make_taxonomy`. Can be "all" or any combination of ranks from
#' envClean::lurank greater than or equal to subspecies. Each rank receives its own taxonomic list with
#' taxa binned up to that level, e.g. if "genus" then all species and subspecies are binned to genus,
#' but also returns all other taxa at levels higher than genus, i.e. family through to kingdom.
#' @param overrides Dataframe of taxa to override, as per `envClean::make_taxonomy`.
#' @param taxonomic_groups Optional character vector of broad taxonomic groups to filter the results, e.g. if only want fauna,
#' use taxonomic_groups = "fauna". Options are 'fauna', 'vascular-plants' & 'other' (for non-fauna and non-vascular plants).
#' @param filt_ranks If `standardise_taxonomy`, a character vector of taxonomic ranks to keep.
#' Note, these may be different to `needed_ranks`, which set the lowest taxonomic level to bin to, e.g.
#' with `needed_ranks` = "species" but without any `filt_ranks` this would return all taxa >= "species", but
#' with `filt_ranks` = "species" would only return species level taxa.
#' @param return_scientific Logcial. Return the scientific_name as per the ALA taxonomic backbone
#' (e.g. as opposed to species name in ALA which can be different)?
#' @param current Logical. Return only current BDBSA taxonomy? Applicable to `source` = "oracle" only.
#'
#' @returns Dataframe with BDBSA original names, common names, NSX codes, indigenous status ('ind' field), and
#' standardised taxa names and returned ranks from ALA (if `standardise_taxonomy` or `taxonomy` is used).
#'
#' @export
#'
get_bdbsa_taxa <- function(source = "web"
                           , url = c("https://data.environment.sa.gov.au/Content/Publications/fauna-bdbsa-taxonomy.xlsx"
                                     , "https://data.environment.sa.gov.au/Content/Publications/vascular-plants-bdbsa-taxonomy.xlsx"
                                     , "https://data.environment.sa.gov.au/Content/Publications/other-taxonomic-groups-bdbsa-taxonomy.xlsx"
                           )
                           , out_dir = tempdir()
                           , standardise_taxonomy = FALSE
                           , taxonomy = NULL
                           , taxonomy_file = tempfile()
                           , needed_ranks = c("species", "subspecies")
                           , overrides = NULL
                           , taxonomic_groups = c("fauna", "vascular-plants", "other")
                           , filt_ranks = NULL
                           , return_scientific = FALSE
                           , current = TRUE
) {

  if(source == "web") {

    # url ----

    url <- url |>
      tibble::as_tibble_col() |>
      dplyr::filter(grepl(paste(taxonomic_groups, collapse = "|"), value)) |>
      dplyr::pull(value)

    # download & read xlsx ----

    bdbsa_taxa <- purrr::map(url, \(x) {

      out_file <- fs::path(out_dir, basename(x))

      download.file(url = x
                    , destfile = out_file
                    , mode = "wb"
      )

      readxl::read_xlsx(out_file,
                        sheet = 2,
                        col_names = TRUE,
                        skip = if(grepl("fauna", x)) 2 else 0
      ) |>
        dplyr::select(tidyr::any_of(c("original_name" = "SCIENTIFIC NAME"
                                      , "original_name" = "SCIENTIFICNAME"
                                      , "common" = "COMMON NAME"
                                      , "common" = "COMMONNAME"
        )
        )
        )

    }
    ) |>
      dplyr::bind_rows()

  } else {

    # oracle ------
    ## bdbsa oracle connection ----
    con <- DBI::dbConnect(odbc::odbc()
                          , if(Sys.info()['sysname'] == "Windows") "BDBSA Production" else "BDBSA_Production"
                          , database = if(Sys.info()['sysname'] == "Windows") "BDBSA Production" else "EARX_PRD"
                          , uid = Sys.getenv("BDBSA_PRD_user")
                          , pwd = Sys.getenv("BDBSA_PRD_pwd")
    )

    ## table prefixes ----
    if(all(c("fauna" %in% taxonomic_groups, any(c("vascular-plants", "other") %in% taxonomic_groups)))) {

      tbl_prefixes <- c("VS", "FL")

    } else if(taxonomic_groups == "fauna") {

      tbl_prefixes <- "VS"

    } else {

      tbl_prefixes <- "FL"

    }

    ## extract data ----
    bdbsa_taxa <- tbl_prefixes |>
      purrr::map(\(x) {

        dplyr::tbl(con, paste0(x, "VNONSYNNOTREN")) %>%
          dplyr::left_join(dplyr::tbl(con, paste0(x, "SP")) %>%
                             dplyr::select(SPECIESNR
                                           , NSXCODE
                                           , ISINSA
                                           , ISINDIGENOUS
                             )
                           , by = c("SPECIESNR"
                                    , "NSXCODE"
                           )
          ) %>%
          dplyr::filter(ISINSA != "N") %>%
          dplyr::collect() %>%
          {if(current) dplyr::filter(., ISCURRENT == "Y") else .} %>%
          dplyr::select(original_name = SPECIES
                        , common = COMNAME1
                        , nsxcode = NSXCODE
                        , ind = ISINDIGENOUS
          ) %>%
          dplyr::mutate(original_name = iconv(original_name, to = "UTF-8", sub = "byte")) # Needed to overcome: error in `gsub()`: ! input string 5602 is invalid UTF-8 caused by "Dentimitrella semiconvexa\xc3\xe1"

      }
      ) |>
      dplyr::bind_rows()

    DBI::dbDisconnect(con)

  }

  # standardise taxonomy ----

  if(standardise_taxonomy) {

    library(galah)

    taxonomy <- bdbsa_taxa |>
      dplyr::distinct(original_name) |>
      envClean::clean_quotes() |>
      envClean::make_taxonomy(taxonomy_file = taxonomy_file
                              , needed_ranks = needed_ranks
                              , overrides = overrides
      )

  }

  # taxonomy ----

  if(!is.null(taxonomy)) {

    res <- needed_ranks |>
      purrr::map(\(x) bdbsa_taxa |>
                   dplyr::left_join(taxonomy[[x]]$lutaxa) |>
                   dplyr::filter(!is.na(taxa)) |>
                   dplyr::left_join(taxonomy[[x]]$taxonomy) %>%
                   {if(!is.null(filt_ranks)) dplyr::filter(., returned_rank %in% filt_ranks) else .} %>%
                   {if(return_scientific) dplyr::distinct(., taxa, original_name, common, returned_rank) |>
                       dplyr::left_join(taxonomy$raw |>
                                          dplyr::filter(rank_adj == x) |>
                                          dplyr::select(original_name, x, scientific_name)
                                        , by = "original_name") %>%
                       dplyr::distinct(taxa, scientific_name, original_name, common, returned_rank) |>
                       dplyr::filter(!is.na(scientific_name)) else dplyr::distinct(., taxa, original_name, common, returned_rank)}
      ) |>
      dplyr::bind_rows() |>
      dplyr::distinct()

  } else {

    res <- bdbsa_taxa |>
      dplyr::distinct()

  }

  return(res)

}
