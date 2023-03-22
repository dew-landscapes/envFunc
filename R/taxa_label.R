
#' Create a label for a species
#'
#' Default is 'Common (\emph{Scientific}). Possibly including `n = `
#' if `records` is supplied. Formatted for either ggplot (default
#' `what_for =plot`) or markdown.
#'
#' @param common Character. Common name for species
#' @param taxa Character. Scientific name for species
#' @param records Numeric. Optional number of records.
#' @param what_for "plot" or something else. If plot will be formatted for
#' plotting otherwise formatted for markdown.
#'
#' @return
#' @export
#'
#' @examples
#' cat(taxa_label("Black-chinned Honeyeater", "Melithreptus gularis"))
#' cat(taxa_label(taxa = "Melithreptus gularis"))
#' cat(taxa_label("Black-chinned Honeyeater", records = 3))
#' cat(taxa_label("Black-chinned Honeyeater", "Melithreptus gularis", what_for = "md"))
#' cat(taxa_label("Black-chinned Honeyeater", "Melithreptus gularis", what_for = "md", records = 3))
taxa_label <- function(common = NULL
                       , taxa = NULL
                       , records = NULL
                       , what_for = "plot"
                       ) {

  if(!is.null(common)) common <- as.character(common)
  if(!is.null(taxa)) taxa <- as.character(taxa)
  if(!is.null(records)) records <- as.numeric(records)

  if(is.null(common) & is.null(taxa)) stop("need 'common' and/or 'taxa'")

  if(!is.null(records)) {

    records <- paste0(", n = "
                      , records
                      )

    if(any(is.null(common), common == "", is.na(common))) {

      # Taxa only
      lab <- bquote(~italic.(taxa)*.(records))

    } else if(any(is.null(taxa), taxa == "", is.na(taxa))) {

      # Common only
      lab <- bquote(.(common)*.(records))

    } else {

      # both
      lab <- bquote(.(common)~italic(.(taxa))*.(records))

    }

  } else {

    if(any(is.null(common), common == "", is.na(common))) {

      # Taxa only
      lab <- bquote(~italic(.(taxa)))

    } else if(any(is.null(taxa), taxa == "", is.na(taxa))) {

      # Common only
      lab <- bquote(.(common))

    } else {

      # both
      lab <- bquote(.(common)~italic(.(taxa)))

    }

  }

  if(what_for != "plot") {

    has_common <- !any(is.null(common), is.na(common), common == "")
    has_taxa <- !any(is.null(taxa), is.na(taxa), taxa == "")

    lab <- paste0(if(has_common) paste0(common
                                        , if(has_taxa) " ("
                                        )
                  , if(what_for == "md") "_"
                  , if(has_taxa) taxa
                  , if(what_for == "md") "_"
                  , if(has_common) ")"
                  , if(!is.null(records)) records
                  )

  } else lab <- deparse(lab)

  return(lab)

}

