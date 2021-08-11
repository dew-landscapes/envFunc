


#' Make an object if it is not available from provided file
#'
#'
#' @param saveFile Character. Path to file. This will be loaded to object if it
#' exists
#' @param fn Function. Function to make the object if it the file does not
#' exist
#' @param ... Arguments to function.
#'
#' @return Dataframe. Also, file if it did not previously exist.
#' @export
#'
#' @examples
  get_or_make <- function(saveFile = outFile, forceNew = FALSE, fn, ...) {

    make_and_save <- function(.saveFile = saveFile, .fn = fn) {

      temp <- do.call(.fn,list(...))

      feather::write_feather(temp,.saveFile)

    }

    if(forceNew) {

      make_and_save()

    } else if(!file.exists(saveFile)) {

      make_and_save()

    } else rio::import(saveFile)


  }


#--------GIS----------

  # Calculate area in hectares of levels within a raster or polygon within another polygon boundary

  calc_poly_areas <- function(boundary,toSummarise) {

    boundary <- st_make_valid(boundary) %>%
      st_transform(crs = 8059)

    if(grepl("raster",tolower(class(toSummarise)))) {

      res <- raster::extract(toSummarise
                             ,boundary
                             ,df = TRUE
      ) %>%
        as_tibble() %>%
        dplyr::select(ID = 2) %>%
        dplyr::count(ID, name = "cells") %>%
        dplyr::mutate(hectares = res(toSummarise)[[1]]*res(toSummarise)[[2]]*cells/10000
                      , prop = hectares/(as.numeric(st_area(boundary))/10000)
                      , per = 100*prop
        ) %>%
        dplyr::left_join(ecosystemsDesc, by = c("ID" = "rclNum"))

    }

    if("sf" %in% class(toSummarise)) {

      toSummarise <- st_make_valid(toSummarise) %>%
        st_transform(crs = 8059)

      subPolys <- st_intersection(toSummarise,boundary)

      res <- subPolys %>%
        dplyr::mutate(hectares = as.numeric(st_area(geometry))/10000) %>%
        st_set_geometry(NULL) %>%
        dplyr::filter(!is.na(SA_VEG_ID)) %>%
        dplyr::group_by(SA_VEG_ID,VEG_ID,VG_GEN_STR,VG_STR_FOR,BROAD_DESC,DOMSP_GENS,DETSP_DOM,ALLIANCE,DOMSP_LAY
                        , SA_VEG_DES
        ) %>%
        dplyr::summarise(hectares = sum(hectares)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(prop = hectares/(as.numeric(st_area(boundary))/10000))

    }

    return(res)

  }

  # add coordinate columns to a sf point object
  # https://github.com/r-spatial/sf/issues/231
  sfc_as_cols <- function(x, names = c("x","y")) {
    stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
    ret <- sf::st_coordinates(x)
    ret <- tibble::as_tibble(ret)
    stopifnot(length(names) == ncol(ret))
    x <- x[ , !names(x) %in% names]
    ret <- setNames(ret,names)
    dplyr::bind_cols(x,ret)
  }


# Convert deg°min'sec to decimal degrees
  deg_min_sec_to_dec_deg <- function(df,col,sep = "\u00B0|'", into = c("lat","long")) {

    dfNames <- names(df)

    df %>%
      tidyr::separate(!!ensym(col), into = c("deg","min","sec"), sep = paste0(sep)) %>%
      dplyr::mutate(sec = parse_number(sec)) %>%
      dplyr::mutate(across(all_of(c("deg","min","sec")),as.numeric)) %>%
      dplyr::mutate(!!ensym(into) := deg + min/60 + sec/60^2) %>%
      dplyr::select(all_of(dfNames),!!ensym(into))

  }

#-------Bibliographies----------

# get next 'RN' number to use

  next_bib_no <- function(bibPath = fs::path("..","template","toCommon","refs.bib")) {

    as_tibble(read_lines(bibPath)) %>%
      dplyr::filter(grepl("^@",value)) %>%
      dplyr::mutate(value = parse_number(value)) %>%
      dplyr::pull(value) %>%
      max() %>%
      `+` (1)

  }

# get a bib entry from a DOI
  get_bib <- function(DOI,outFile = NULL){

    # https://stackoverflow.com/questions/57340204/r-convert-list-of-dois-to-bibtex

    h <- curl::new_handle()
    curl::handle_setheaders(h, "accept" = "application/x-bibtex")

    get_the_bib <- function(doi) {

      try(
        curl::curl(doi,handle=h) %>%
        readLines(warn = FALSE) %>%
        {if(is.character(outFile)) write(.,file=outFile,append = TRUE) else (.)}
      )

    }

    DOI %>%
      gsub("https://doi.org/","",.,fixed = TRUE) %>%
      paste0("https://doi.org/",.) %>%
      map(get_the_bib)

  }

# make package bibliography, including tweaks for known package issues.
  fix_bib <- function(bibFile, makeKey = FALSE, isPackageBib = FALSE) {

    inRefs <- bib2df::bib2df(bibFile)

    namesInRefs <- colnames(inRefs) %>%
      grep("\\.\\d+$",.,value = TRUE,invert = TRUE) %>%
      `[`(1:28) %>%
      c(.,"COPYRIGHT")

    refs <- inRefs %>%
      {if(isPackageBib) (.) %>% dplyr::mutate(package = gsub("R-|\\d{4}","",BIBTEXKEY)) else (.)} %>%
      tidytext::unnest_tokens("titleWords"
                              ,TITLE
                              ,token = "regex"
                              ,pattern = " "
                              ,to_lower = FALSE
                              #,strip_punct = FALSE
                              ,collapse = FALSE
                              ) %>%
      dplyr::mutate(titleWords = gsub("\\{|\\}","",titleWords)
                    , isCap = grepl(paste0(LETTERS,collapse="|"),titleWords)
                    , titleWords = if_else(isCap,paste0("{",titleWords,"}"),titleWords)
                    ) %>%
      tidyr::nest(data = c(titleWords,isCap)) %>%
      dplyr::mutate(TITLE = map_chr(data,. %>% dplyr::pull(titleWords) %>% paste0(collapse = " "))
                    , AUTHOR = map(AUTHOR,~gsub("Microsoft Corporation","{Microsoft Corporation}",.))
                    , AUTHOR = map(AUTHOR,~gsub("Fortran original by |R port by ","",.))
                    , AUTHOR = map(AUTHOR, ~gsub("with contributions by","and",.))
                    , AUTHOR = map(AUTHOR, ~gsub("Â "," ",.))
                    , YEAR = substr(YEAR,1,4)
                    ) %>%
      {if(makeKey) (.) %>%
          dplyr::mutate(BIBTEXKEY = map2_chr(AUTHOR
                                       ,YEAR
                                       ,~paste0(toupper(gsub("[[:punct:]]|\\s","",.x[[1]]))
                                                , .y
                                                )
                                       )
                        ) else (.)} %>%
      {if(isPackageBib) (.) %>% dplyr::mutate(TITLE = map2_chr(package,TITLE,~gsub(.x,paste0("{",.x,"}"),.y))) else (.)} %>%
      dplyr::select(any_of(namesInRefs)) %>%
      dplyr::filter(!grepl("MEDIA SCREEN AND",CATEGORY))

    bib2df::df2bib(refs,bibFile)

    return(refs)

  }


#------Git----------

  # Git add.
  gitadd <- function(dir = getwd()){
    cmd_list <- list(
      cmd1 = tolower(substr(dir,1,2)),
      cmd2 = paste("cd",dir),
      cmd3 = "git add --all"
    )
    cmd <- paste(unlist(cmd_list),collapse = " & ")
    shell(cmd)
  }

  # Git commit.
  gitcommit <- function(msg = "commit from Rstudio", dir = getwd()){
    cmd = sprintf("git commit -m\"%s\"",msg)
    shell(cmd)
  }

  # Git push.
  gitpush <- function(dir = getwd()){
    cmd_list <- list(
      cmd1 = tolower(substr(dir,1,2)),
      cmd2 = paste("cd",dir),
      cmd3 = "git push"
    )
    cmd <- paste(unlist(cmd_list),collapse = " & ")
    shell(cmd)
  }


# Site a package in rmarkdown
  # assumes these have been run and that 'packages' contains all packages to cite
    # knnitr::write_bib(packages,"packageCitations.bib")
    # refs <- bib2df::bib2df("packageCitations.bib")

  cite_package <- function(package,brack = TRUE,startText = "", endText = "") {

    thisRef <- refs %>%
      dplyr::filter(grepl(paste0("-",package),BIBTEXKEY) | grepl(paste0("^",package),BIBTEXKEY)) %>%
      dplyr::pull(BIBTEXKEY)

    starts <- if(brack) paste0("[",startText,"@") else paste0(startText,"@")
    ends <- if(brack) paste0(endText,"]") else endText

    if(length(thisRef) > 1) {

      paste0(starts,paste0(thisRef,collapse = "; @"),ends)

    } else {

      paste0(starts,"R-",package,ends)

    }

  }




# Are the values within a column unique
  col_is_unique <- function(df,col = "SiteID") {

    notUnique <- df %>%
      dplyr::select(grep("^n$",names(.),value = TRUE,invert = TRUE)) %>%
      dplyr::count(!!ensym(col)) %>%
      dplyr::filter(n > 1)

    print(paste0("there are ",nrow(notUnique)," ",col,"(s) that are not unique: ",vec_to_sentence(notUnique[,1])))

  }


# Unscale scaled data
unscale_data <- function(scaledData) {

  scaledData*attr(scaledData,"scaled:scale")+attr(scaledData,"scaled:center")

}




# From https://gist.github.com/danlwarren/271288d5bab45d2da549

# Function to rarefy point data in any number of dimensions.  The goal here is to
# take a large data set and reduce it in size in such a way as to approximately maximize the
# difference between points.  For instance, if you have 2000 points but suspect a lot of
# spatial autocorrelation between them, you can pass in your data frame, the names (or indices)
# of the lat/lon columns, and the number 200, and you get back 200 points from your original data
# set that are chosen to be as different from each other as possible given a randomly chosen
# starting point

# Input is:
#
# x, a data frame containing the columns to be used to calculate distances along with whatever other data you need
# cols, a vector of column names or indices to use for calculating distances
# npoints, the number of rarefied points to spit out
#
# e.g., thin.max(my.data, c("latitude", "longitude"), 200)


  thin_max <- function(x, cols, npoints){
    #Create empty vector for output
    inds <- vector(mode="numeric")

    #Create distance matrix
    this.dist <- as.matrix(dist(x[,cols], upper=TRUE))

    #Draw first index at random
    inds <- c(inds, as.integer(runif(1, 1, length(this.dist[,1]))))

    #Get second index from maximally distant point from first one
    #Necessary because apply needs at least two columns or it'll barf
    #in the next bit
    inds <- c(inds, which.max(this.dist[,inds]))

    while(length(inds) < npoints){
      #For each point, find its distance to the closest point that's already been selected
      min.dists <- apply(this.dist[,inds], 1, min)

      #Select the point that is furthest from everything we've already selected
      this.ind <- which.max(min.dists)

      #Get rid of ties, if they exist
      if(length(this.ind) > 1){
        print("Breaking tie...")
        this.ind <- this.ind[1]
      }
      inds <- c(inds, this.ind)
    }

    return(x[inds,])
  }


#--------Random Forests----------

  # Iteratively add trees to a random forest with tibble output
  add_row_rf_simple <- function(resDf,rowGrow = 499) {

    prevRf <- resDf$rf[nrow(resDf)][[1]]

    nextRf <- rf_simple(rowGrow)

    newRf <- combine(prevRf,nextRf)

    resDf %>%
      dplyr::bind_rows(tibble(start = Sys.time()
                              , run = max(resDf$run) + 1
                              ) %>%
                         dplyr::mutate(trees = max(resDf$trees) + rowGrow
                                       , rf = list(newRf)
                                       #, rfProbCell = map(rf,rf_prob_cell)
                                       #, meanVotesCell = map_dbl(rfProbCell,~mean(.$votes))
                                       #, rfProbClass = map(rfProbCell,rf_prob_class)
                                       #, meanVotesClass = map_dbl(rfProbClass,~mean(.$votes))
                                       , deltaPrev = map_dbl(rf
                                                             , ~tibble(last = .$predicted
                                                                       , prev = prevRf$predicted
                                                                       ) %>%
                                                               dplyr::mutate(rows = nrow(.)
                                                                             , same = last == prev
                                                                             ) %>%
                                                               dplyr::summarise(same = sum(same)/mean(rows)) %>%
                                                               dplyr::pull(same)
                                                             )
                                       , kappaPrevRf = map_dbl(rf
                                                                ,~caret::confusionMatrix(.$predicted
                                                                                         ,prevRf$predicted
                                                                                         )$overall[["Kappa"]]
                                                                )
                                       , end = Sys.time()
                                       )
                       ) %>%
      dplyr::mutate(seconds = lag(seconds, default = 0) + as.numeric(difftime(end,start, units = "secs")))

  }

  rf_simple <- function(trees = 99, cores = length(cl), df = envData) {

    # Parallel computation depends upon a parallel backend that must be registered before running foreach %dopar%
    # cores here is just used to help split up the task in foreach, it does not create the a parallel cluster

    x <- envData[,which(names(envData) %in% envNames)]
    y <- envData %>% dplyr::pull(cluster)

    # Assumes envData exists and is ready to go
    foreach(ntree=rep(ceiling(trees/cores), cores)
            , .combine=combine
            , .packages = c("randomForest")
            , .export = c("useMtry")
            ) %dopar%
      randomForest::randomForest(x = x
                                 , y = y
                                 , ntree = ntree
                                 , importance = TRUE
                                 , mtry = useMtry
                                 )

    }

  # Get some rf results
  rf_prob_cell <- function(rf
                     , envDf = predSample
                     , classes = levels(envData$cluster)
                     , targetVotes = 0.5
                     ) {

    predict(rf,envDf,type = "prob") %>%
      as_tibble() %>%
      dplyr::mutate(id = row_number()
                    , across(where(is.matrix),as.numeric)
                    ) %>%
      tidyr::pivot_longer(any_of(classes), names_to = "predCluster", values_to = "votes") %>%
      dplyr::group_by(id) %>%
      dplyr::slice(which.max(votes))

  }

  rf_prob_class <- function(rfProbCells) {

    rfProbCells %>%
      dplyr::group_by(predCluster) %>%
      dplyr::summarise(votes = mean(votes)) %>%
      dplyr::ungroup()

  }

# A function to run random forest over a df with first column 'cluster' and other columns explanatory

  rf_mod_fold <- function(envClust
                          , clustCol = "cluster"
                          , envCols = names(patchesEnvSelect)[-1]
                          , idCol = "cell"
                          , doFolds = folds
                          , outFile
                          , saveModel = FALSE
                          , saveImp = FALSE
                          , ...
                          ){

    idCol <- if(is.numeric(idCol)) names(envClust)[idCol] else idCol

    clustCol <- if(is.numeric(clustCol)) names(envClust)[clustCol] else clustCol

    envCols <- if(is.numeric(envCols)) names(envClust)[envCols] else envCols

    envClust <- envClust %>%
      dplyr::mutate(fold = sample(1:doFolds,nrow(.),replace=TRUE,rep(1/doFolds,doFolds)))

    folds <- 1:doFolds

    fold_rf_mod <- function(fold) {

      outFile <- gsub("_conf",paste0("_fold",fold,"_conf"),outFile)

      if(!file.exists(outFile)) {

        if(doFolds > 1) {

          train <- envClust[envClust$fold != fold,which(names(envClust) %in% c(clustCol,envCols))] %>%
            dplyr::mutate(cluster = factor(cluster))

          test <- envClust[envClust$fold == fold,which(names(envClust) %in% c(idCol,clustCol,envCols))] %>%
            dplyr::mutate(cluster = factor(cluster, levels = levels(train$cluster)))

          rfMod <- randomForest(x = train[,envCols]
                                , y = train[,clustCol] %>% dplyr::pull(!!ensym(clustCol))
                                , ntree = 500
                                , importance = saveImp
                                )

          rfPred <- test %>%
            dplyr::select(!!ensym(idCol),!!ensym(clustCol)) %>%
            dplyr::bind_cols(predict(rfMod
                                     , newdata = test[,envCols]
                                     ) %>%
                               tibble::enframe(name = NULL, value = "predCluster")
                             ) %>%
            dplyr::bind_cols(predict(rfMod
                                     , newdata = test[,envCols]
                                     , type = "prob"
                                     ) %>%
                               as_tibble()
                             )

        } else {

          rfMod <- randomForest::randomForest(x = envClust[,which(names(envClust) %in% c(envCols))]
                                              , y = envClust %>% dplyr::pull(!!ensym(clustCol))
                                              , ntree = 500
                                              , importance = saveImp
                                              )

          rfPred <- envClust[,c(idCol,clustCol)] %>%
            dplyr::bind_cols(predict(rfMod) %>%
                               tibble::enframe(name = NULL, value = "predCluster")
                             ) %>%
            dplyr::bind_cols(predict(rfMod
                                     , type = "prob"
                                     ) %>%
                               as_tibble()
                             )

        }

        feather::write_feather(rfPred,outFile)

        if(saveImp) {feather::write_feather(as_tibble(rfMod$importance, rownames = "att"),gsub("_rfPred","_rfImp",outFile))}

        if(saveModel) {feather::write_feather(rfMod,gsub("_rfPred","",outFile))}

      }

    }

    map(folds,fold_rf_mod)

    }

  rf_mod <- function(envClust
                          , clustCol
                          , envCols
                          , idCol
                          , outFile
                          , saveModel = FALSE
                          , saveImp = FALSE
                          , ...
                     ){

    idCol <- if(is.numeric(idCol)) names(envClust)[idCol] else idCol

    clustCol <- if(is.numeric(clustCol)) names(envClust)[clustCol] else clustCol

    envCols <- if(is.numeric(envCols)) names(envClust)[envCols] else envCols

    rfMod <- randomForest::randomForest(x = envClust[,which(names(envClust) %in% c(envCols))]
                                        , y = envClust %>% dplyr::pull(!!ensym(clustCol))
                                        , ntree = 500
                                        , importance = saveImp
                                        )

    rfPred <- envClust[,c(idCol,clustCol)] %>%
      dplyr::bind_cols(predict(rfMod) %>%
                         tibble::enframe(name = NULL, value = "predCluster")
                       ) %>%
      dplyr::bind_cols(predict(rfMod
                               , type = "prob"
                               ) %>%
                         as_tibble()
                       )

    feather::write_feather(rfPred,outFile)

    if(saveImp) {feather::write_feather(as_tibble(rfMod$importance, rownames = "att"),gsub("_rfPred","_rfImp",outFile))}

    if(saveModel) {feather::write_feather(rfMod,gsub("_rfPred","",outFile))}

  }


# A function to run random forest using tidymodels dialogue

  run_rf <- function(datTrain,modRecipe) {

    randomForest::randomForest(as.formula(modRecipe)
                               , data = datTrain
                               , ntree = 500
                               )

  }

  # Generate predictions with oob/cv probabilities

  caret_pred <- function(trainObj,data,idCol = "cell", groupCol = "cluster") {

    rfType <- class(trainObj)

    if("randomForest" %in% rfType)  mod <- trainObj
    if("randomForest" %in% class(trainObj$finalModel)) mod <- trainObj$finalModel

    stopifnot(exists("mod"))

    tibble(cell = data %>% dplyr::pull(!!ensym(idCol))
           , cluster = data %>% dplyr::pull(!!ensym(groupCol))
           , predCluster = mod$predicted
           ) %>%
      dplyr::bind_cols(as_tibble(mod$votes) %>%
                         dplyr::mutate(across(.fns = as.numeric))
                       ) %>%
      dplyr::mutate(across(where(is.matrix),as.numeric)) %>%
      dplyr::mutate(predCluster = factor(predCluster, levels = levels(data %>% dplyr::pull(!!ensym(groupCol)))))

  }



#-------Miscellaneous--------



  dist_to_df <- function(inDist,patchNames) {

    # https://stackoverflow.com/questions/23474729/convert-object-of-class-dist-into-data-frame-in-r/23475065

    if (class(inDist) != "dist") stop("wrong input type")
    A <- attr(inDist, "Size")
    B <- patchNames
    if (isTRUE(attr(inDist, "Diag"))) attr(inDist, "Diag") <- FALSE
    if (isTRUE(attr(inDist, "Upper"))) attr(inDist, "Upper") <- FALSE
    data.frame(
      row = B[unlist(lapply(sequence(A)[-1], function(x) x:A))],
      col = rep(B[-length(B)], (length(B)-1):1),
      value = as.vector(inDist))

  }

  # https://stackoverflow.com/questions/64519640/error-in-summary-connectionconnection-invalid-connection
  unregister_dopar <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
  }

# function to read in previously saved rds

  read_rds_file <- function(fileName) if(file.exists(fileName)) read_rds(fileName) else NULL





# Create a colour palette for n groups

  col_pal <-  function(n) {
    if (n <= 8) {
      RColorBrewer::brewer.pal(n, "Set2")
    } else {
      hcl(h=seq(0,(n-1)/(n),length=n)*360,c=100,l=65,fixup=TRUE)
    }
  }



#' Vector to sentence
#'
#' Turn a vector into a comma separated list of values with a penultimate 'and'
#' or other separator.
#'
#' @param x Character. Vector to collapse to a sentence.
#' @param sep Character. Separator between all except last.
#' @param end Character. Last separator.
#'
#' @return
#' @export
#'
#' @examples
#' x <- c("apples", "bannanas", "pears", "grapes")
#' vec_to_sentence(x)
#' vec_to_setence(x,end = "&")
  vec_to_sentence <- function(x,sep=",",end="and") {

    x[!is.na(x)] %>%
      paste(collapse = "JOINSRUS") %>%
      (function(x) if(sep == ";") {

        stringi::stri_replace_last_regex(x,"JOINSRUS", paste0(sep," and ")) %>%
          str_replace_all("JOINSRUS",paste0(sep," "))

      } else {

        stringi::stri_replace_last_regex(x,"JOINSRUS",paste0(" ",end," ")) %>%
          str_replace_all("JOINSRUS",paste0(sep," "))

      }
      )

  }

# https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r

  numbers2words <- function(x){
    ## Function by John Fox found here:
    ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
    ## Tweaks by AJH to add commas and "and"
    helper <- function(x){

      digits <- rev(strsplit(as.character(x), "")[[1]])
      nDigits <- length(digits)
      if (nDigits == 1) as.vector(ones[digits])
      else if (nDigits == 2)
        if (x <= 19) as.vector(teens[digits[1]])
      else trim(paste(tens[digits[2]],
                      Recall(as.numeric(digits[1]))))
      else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
                                        Recall(makeNumber(digits[2:1]))))
      else {
        nSuffix <- ((nDigits + 2) %/% 3) - 1
        if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
        trim(paste(Recall(makeNumber(digits[
          nDigits:(3*nSuffix + 1)])),
          suffixes[nSuffix],"," ,
          Recall(makeNumber(digits[(3*nSuffix):1]))))
      }
    }
    trim <- function(text){
      #Tidy leading/trailing whitespace, space before comma
      text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
      #Clear any trailing " and"
      text=gsub(" and$","",text)
      #Clear any trailing comma
      gsub("\ *,$","",text)
    }
    makeNumber <- function(...) as.numeric(paste(..., collapse=""))
    #Disable scientific notation
    opts <- options(scipen=100)
    on.exit(options(opts))
    ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
              "eight", "nine")
    names(ones) <- 0:9
    teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
               "sixteen", "seventeen", "eighteen", "nineteen")
    names(teens) <- 0:9
    tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
              "ninety")
    names(tens) <- 2:9
    x <- round(x)
    suffixes <- c("thousand", "million", "billion", "trillion")
    if (length(x) > 1) return(trim(sapply(x, helper)))
    res <- helper(x)
    #res <- gsub(" ","",res)
    return(res)
  }


#---------Taxonomy----------

# a function to retrieve taxonomy to accepted names and retrieve taxonomic hierarchy for a df with a column of taxonomic names

  gbif_tax <- function(df
                       , sppCol=1
                       , outFile="data/luGBIF.feather"
                       , kingType="Plantae"
                       , getCommon = FALSE
                       , targetRank = "Species"
                       ){

    tmpFile <- paste0(gsub(".feather","",outFile),"_temp.feather")

    luRank <- tribble(
      ~Rank, ~sort
      , "Kingdom", 1
      , "Phylum", 2
      , "Class", 3
      , "Order", 4
      , "Family", 5
      , "Genus", 6
      , "Species", 7
      , "Subspecies", 8
      , "Variety", 9
      , "Form", 10
    )

    assign("luRank",luRank,envir = .GlobalEnv)

    targetSort <- luRank %>%
      dplyr::filter(Rank == targetRank) %>%
      dplyr::pull(sort)

    alreadyDone01 <- if(file.exists(outFile)) read_feather(outFile) %>%
      dplyr::distinct(originalName) %>%
      dplyr::pull()

    alreadyDone02 <- if(file.exists(tmpFile)) read_feather(tmpFile) %>%
      dplyr::distinct(originalName) %>%
      dplyr::pull()

    alreadyDone <- c(get0("alreadyDone01"),get0("alreadyDone02"))

    toCheck <- df %>%
      dplyr::select(all_of(sppCol)) %>%
      dplyr::distinct() %>%
      dplyr::pull()

    taxa <- tibble(originalName = setdiff(toCheck,alreadyDone)) %>%
      dplyr::filter(!grepl("BOLD:.*\\d{4}",originalName)
                    , !is.na(originalName)
                    ) %>%
      dplyr::mutate(searchedName = gsub("\\s*\\(.*\\).*|\\'|\\?| spp\\.| sp\\.| ssp\\.| var\\.| ex| [A-Z].*|#|\\s^"
                                ,""
                                ,originalName
                                )
                    , searchedName = gsub(" x .*$| X .*$","",searchedName)
                    , searchedName = gsub("\\s{2,}"," ",searchedName)
                    , searchedName = str_squish(searchedName)
                    )

    taxas <- taxa %>%
      dplyr::distinct(searchedName) %>%
      dplyr::arrange(searchedName)

    if(length(taxas$searchedName)>0){

      for (i in taxas$searchedName){

        print(i)

        taxGBIF <- name_backbone(i, kingdom = kingType) %>%
          dplyr::mutate(searchedName = i)

        taxGBIF <- if(sum(grepl("acceptedUsageKey",names(taxGBIF)))>0) {

          name_usage(taxGBIF$acceptedUsageKey,return="data")$data %>%
            dplyr::mutate(matchType = "Synonym") %>%
            dplyr::rename(usageKey = key
                          , status = taxonomicStatus
                          ) %>%
            dplyr::mutate(searchedName = i)

        } else {

          taxGBIF

        }

        if(getCommon) taxGBIF$Common <- get_gbif_common(taxGBIF$usageKey)

        taxGBIF$Taxa <- taxGBIF %>%
          tidyr::pivot_longer(where(is.numeric),names_to = "key") %>%
          dplyr::mutate(key = map_chr(key,~gsub("Key","",.))
                        , key = str_to_sentence(key)
                        ) %>%
          dplyr::filter(key %in% luRank$Rank) %>%
          dplyr::left_join(luRank, by = c("key" = "Rank")) %>%
          dplyr::filter(sort <= targetSort) %>%
          dplyr::filter(sort == max(sort)) %>%
          dplyr::select(tolower(luRank$Rank[luRank$sort == .$sort])) %>%
          dplyr::pull()

        taxGBIF$Stamp <- Sys.time()

        taxGBIF <- taxa %>%
          dplyr::inner_join(taxGBIF)

        if(file.exists(tmpFile)) {

          write_feather(taxGBIF %>%
                          dplyr::bind_rows(read_feather(tmpFile)) %>%
                          dplyr::select(1,2,Taxa,everything())
                        , paste0(gsub(".feather","",outFile),"_temp.feather")
                        )

        } else {

          write_feather(taxGBIF %>%
                          dplyr::select(1,2,Taxa,everything())
                        , paste0(gsub(".feather","",outFile),"_temp.feather")
                        )

          }

      }

      # Clean up results
      read_feather(tmpFile) %>%
        {if(!file.exists(outFile)) (.) else (.) %>% dplyr::bind_rows(read_feather(outFile))} %>%
        dplyr::group_by(originalName) %>%
        dplyr::filter(Stamp == max(Stamp)) %>%
        dplyr::ungroup() %>%
        write_feather(outFile)

      file.remove(tmpFile)

    } else {

      {warning( "No taxa supplied" ) }

    }

  }

  # Find common name from GBIF (key is the gbif 'useagekey')
  get_gbif_common <- function(key) {

    print(key)

    commonNames <- name_usage(key)$data %>%
      dplyr::select(contains("Key")) %>%
      dplyr::select(where(is.numeric)) %>%
      tidyr::pivot_longer(1:ncol(.),names_to = "key") %>%
      dplyr::mutate(key = map_chr(key,~gsub("Key","",.))
                    , key = str_to_sentence(key)
                    ) %>%
      dplyr::filter(key %in% luRank$Rank) %>%
      dplyr::left_join(luRank, by = c("key" = "Rank")) %>%
      dplyr::filter(sort == max(sort)) %>%
      dplyr::pull(value) %>%
      name_usage(data="vernacularNames")

    df <- commonNames$data %>%
      dplyr::select(any_of(c("vernacularName","language","preferred")))

    hasAny <- nrow(df) > 0

    hasPreferred <- if("preferred" %in% names(df)) sum(df$preferred, na.rm = TRUE) > 0 else FALSE

    hasLanguage <- if("language" %in% names(df)) sum(df$preferred, na.rm = TRUE) > 0 else FALSE

    hasPreferredEng <- if(hasPreferred) df %>%
      dplyr::filter(preferred) %>%
      dplyr::filter(language == "eng") %>%
      nrow() %>%
      `>` (0) else FALSE

    hasEng <- if(hasLanguage) df %>%
      dplyr::filter(language == "eng") %>%
      nrow() %>%
      `>` (0) else FALSE

    if(hasPreferredEng) {

      df %>%
        dplyr::filter(preferred
                      , language == "eng"
                      ) %>%
        dplyr::pull(vernacularName) %>%
        unique() %>%
        sort() %>%
        paste0(collapse = ", ")

    } else if(hasEng) {

      df %>%
        dplyr::filter(language == "eng") %>%
        tidytext::unnest_tokens("common",vernacularName,token = "regex", pattern = ",|and",collapse = FALSE) %>%
        dplyr::mutate(common = gsub("^\\s|\\s$|etc","",common)) %>%
        dplyr::distinct(common) %>%
        dplyr::pull(common) %>%
        unique() %>%
        sort() %>%
        paste0(collapse = ", ")

    } else if(hasAny) {

      df %>%
        dplyr::count(language,vernacularName) %>%
        dplyr::arrange(desc(n)
                       , language
        ) %>%
        dplyr::slice(1) %>%
        dplyr::pull(vernacularName) %>%
        `[` (1)

    } else ""

  }

  # Add common name to existing taxonomic data frame
  add_gbif_common <- function(path = "data/luGBIF.feather") {

    gbifTaxDf <- read_feather(path) %>%
      #(if(testing) {. %>% dplyr::sample_n(5)} else {.}) %>%
      dplyr::mutate(Common = future_map_chr(key,get_gbif_common))

    write_feather(gbifTaxDf,path)

  }



