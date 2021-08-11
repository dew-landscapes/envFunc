
#' Apply clustering algorithms
#'
#' @param df Dataframe. Needs the 'Cols' outlined below.
#' @param methodsDf Dataframe.
#' @param sppCol
#' @param siteCol
#' @param numCol
#' @param groups
#' @param minTaxaCount
#'
#' @return
#' @export
#'
#' @examples
  make_clusters <- function(df
                            , methodsDf = clustMethod
                            , sppCol = "Taxa"
                            , siteCol = "list"
                            , numCol = "p"
                            , groups = 2:100
                            , minTaxaCount = 1
                            ) {

    if(!numCol %in% names(df)) df$p = 1

    datWide <- df %>%
      dplyr::add_count(!!ensym(sppCol)) %>%
      dplyr::filter(n > minTaxaCount) %>%
      dplyr::group_by(!!ensym(sppCol),!!ensym(siteCol)) %>%
      dplyr::summarise(value = sum(!!ensym(numCol),na.rm = TRUE)) %>%
      dplyr::filter(!is.na(value)) %>%
      tidyr::pivot_wider(names_from = all_of(sppCol), values_fill = 0) %>%
      dplyr::arrange(!!ensym(siteCol)) %>%
      tibble::column_to_rownames(siteCol) %>%
      as.matrix()

    siteNames <- rownames(datWide)

    dist <- parallelDist::parDist(datWide
                                  , method = "bray"
                                  , threads = if(exists("useCores")) useCores else 1
    )

    assign("sqDist",as.matrix(dist^2),pos = .GlobalEnv)

    dend <- methodsDf %>%
      dplyr::mutate(dend = map(method
                               ,~fastcluster::hclust(dist, .)
      )
      )

    clust <- dend %>%
      dplyr::mutate(clusters = map(dend
                                   , cutree
                                   , groups
      )
      , clusters = map(clusters
                       , as_tibble
      )
      ) %>%
      dplyr::select(-dend) %>%
      tidyr::unnest(clusters) %>%
      tidyr::pivot_longer(2:ncol(.),names_to = "groups",values_to ="clust") %>%
      dplyr::mutate(groups = as.integer(groups)) %>%
      tidyr::nest(clusters = c(clust))

  }

#' Basic summary of clusters
#'
#' @param clustDf dataframe. Contains a list column with output from cuttree in column groupName
#' @param groupName character. Name of column containing output from cuttree
#'
#' @return tibble
#' @export
#'
#' @examples
  clustering_summarise <- function(clustDf,groupName="clust") {

    clust <- clustDf %>%
      dplyr::select(tidyselect::all_of(groupName))

    tab <- table(clust)

    tibble::tibble(nSites = nrow(clust)
                   , nClusters = length(tab)
                   , minClustSize = min(tab)
                   , avClustSize = mean(tab)
                   , maxClustSize = max(tab)
                   )

  }


  # Make a cluster data frame (now prefer to use make_clusters)
  make_cluster_df <- function(rawClusters,siteIDsDf) {

    siteIDsDf %>%
      dplyr::mutate(id = row_number()) %>%
      dplyr::bind_cols(rawClusters %>%
                         dplyr::mutate(cluster = numbers2words(clust)
                                       , cluster = fct_reorder(cluster,clust)
                         )
      )

  }




make_sil <- function(clustDf, distObj = datDist, clustID = "clust"){

  clustCol <- if(is.character(clustID)) which(names(clustDf)==clustID) else siteID

  silhouette(dplyr::pull(clustDf,clustCol),distObj)

}

# Turn an object of class silhouette into a data frame with one row per site
make_sil_df <- function(clustDf,silObj) {

  clustDf %>%
    dplyr::bind_cols(tibble(neighbour = silObj[,2],sil_width = silObj[,3]))

}

# Calculate the within clusters sum of squares using only the floristic distances (datDist)
calc_SS <- function(clustDf,clustDfMin,dist = datDist) {

  if(!exists("sqDist")) sqDist <- as.matrix(dist^2)

  clustDf %>%
    dplyr::mutate(id = row_number()) %>%
    dplyr::inner_join(clustDfMin) %>%
    dplyr::group_by(cluster) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    #dplyr::sample_n(2) %>% # TESTING
    dplyr::mutate(wss = map_dbl(data
                                , ~sum(sqDist[.$id,.$id])/(2*nrow(.))
    )
    ) %>%
    dplyr::select(-data)

}


#-------Clustering - Indval--------

cluster_indval <- function(clustDf,dfWithNames = datWide){

  # clustDf is usually siteID, clust, cluster
  # datWide is usually siteID * spp

  datForInd <- clustDf %>%
    dplyr::inner_join(dfWithNames) %>%
    .[,colSums(. != 0) > 0]

  labdsv::indval(datForInd[,names(datForInd) %in% names(dfWithNames[,-1])]
                 ,datForInd$clust
  )

}


# Indval result
cluster_indval_df <- function(clustInd,clustDf){

  tibble(Taxa = names(clustInd$maxcls)
         , clust = clustInd$maxcls
         , indval = clustInd$indcls
         , pval = clustInd$pval
  ) %>%
    dplyr::inner_join(clustInd$relabu %>%
                        as_tibble(rownames = "Taxa") %>%
                        tidyr::gather(clust,abu,names(.)[names(.) %in% unique(clustDf$clust)]) %>%
                        dplyr::mutate(clust = as.numeric(clust)) %>%
                        dplyr::filter(abu > 0)
    ) %>%
    dplyr::inner_join(clustInd$relfrq %>%
                        as_tibble(rownames = "Taxa") %>%
                        tidyr::gather(clust,frq,names(.)[names(.) %in% unique(clustDf$clust)]) %>%
                        dplyr::mutate(clust = as.numeric(clust)) %>%
                        dplyr::filter(frq > 0)
    ) %>%
    dplyr::mutate(clust = as.numeric(clust)
                  , cluster = numbers2words(as.numeric(clust))
                  , cluster = fct_reorder(cluster,clust)
    )

}

#------Clustering - Diagnostics----------


diagnostic_plot <- function(diagnosticDf
                            ,labelDiagnostic = "diagnostic"
                            ,displayAll = FALSE
) {

  df <- diagnosticDf %>%
    {if(displayAll) (.) else (.) %>% dplyr::filter(weight)} %>%
    dplyr::mutate(across(where(is.factor),factor)) %>%
    dplyr::filter(!is.na(value))

  ggplot(df
         ,aes(groups
              , scale
              , colour = combo
              , alpha = if(displayAll) weight else NULL
              , label = groups
              , size = top
         )
  ) +
    geom_point() +
    ggrepel::geom_text_repel(data = df %>%
                               dplyr::filter(best)
                             , size = 2
                             , show.legend = FALSE
                             , box.padding = 1
                             , min.segment.length = 0
                             , colour = "black"
    ) +
    facet_grid(as.formula(paste0(labelDiagnostic,"~method"))
               , scales="free_y"
               ,  labeller = label_wrap_gen(20,multi_line = TRUE)
    ) +
    labs(colour = "Combination"
         , alpha = "Diagnostic used" #paste0("Top ",unique(diagnosticDf$topThresh)*100,"%")
         , title = paste0("Labels indicate top ",numbers2words(unique(df$bestThresh))," results")
         , size = paste0("Best ",(unique(df$topThresh))*100,"%")
    ) +
    scale_colour_viridis_c() +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    theme(strip.text.y = element_text(angle = 0)
          , strip.text.x = element_text(angle = 90)
    )

}


diagnostic_df <- function(df
                          , useWeights = "weightClusters"
                          , diagnosticMinGroups = min(possibleGroups)
                          , summariseMethod = median
                          , topThresh = 0.25
                          , bestThresh = 5
                          , diagnosticDf = diagnostics
) {

  df %>%
    dplyr::filter(groups > diagnosticMinGroups) %>%
    dplyr::select(method,groups,any_of(diagnostics$diagnostic)) %>%
    #select_if(~ !all(is.na(.))) %>%
    dplyr::group_by(method,groups) %>%
    dplyr::summarise(across(any_of(diagnostics$diagnostic),summariseMethod)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(any_of(diagnostics$diagnostic),names_to = "diagnostic", values_to = "value") %>%
    dplyr::left_join(diagnostics) %>%
    dplyr::group_by(diagnostic,diagDefinition) %>%
    dplyr::mutate(scale = if_else(highGood
                                  ,scales::rescale(value,to=c(0,1))
                                  ,scales::rescale(desc(value),to=c(0,1))
    )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(comboInit = scale*!!ensym(useWeights)) %>%
    dplyr::group_by(method,groups) %>%
    dplyr::mutate(combo = mean(comboInit)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(combo = scales::rescale(combo,to=c(0,1))) %>%
    dplyr::mutate(topThresh = topThresh
                  , bestThresh = bestThresh
                  , top = combo >= quantile(combo,probs = 1-topThresh,na.rm = TRUE)
                  , top = if_else(is.na(top),FALSE,top)
                  , best = combo >= sort(unique(.$combo),TRUE)[bestThresh]
                  , best = if_else(is.na(best),FALSE,best)
                  , diagnostic = factor(diagnostic, levels = levels(diagnostics$diagnostic))
                  , weight = !!ensym(useWeights)
    )

}
