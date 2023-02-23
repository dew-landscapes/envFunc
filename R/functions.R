
#' Make an object if it is not available from provided file
#'
#'
#' @param save_file Character. Path to file. This will be loaded to object if it
#' exists
#' @param force_new Logical. If TRUE, run function irrespective of existence of
#' saveFile. Otherwise, only run fn if saveFile does not exist.
#' @param fn Function. Function to make the object if it saveFile does not
#' exist
#' @param ... Arguments to function.
#'
#' @return Dataframe. Also, file if it did not previously exist.
#' @export
#'
  get_or_make <- function(save_file, force_new = FALSE, fn, ...) {

    make_and_save <- function(.save_file = save_file, .fn = fn) {

      temp <- do.call(.fn,list(...))

      rio::export(temp,.save_file)

    }

    if(force_new) {

      make_and_save()

    } else if(!file.exists(save_file)) {

      make_and_save()

    } else rio::import(save_file)


  }


#' Vector to phrase
#'
#' Turn a vector into a phrase with a final 'and' (or other separator). If `sep`
#' is ";" the final is `sep` plus `end`.
#'
#' @param x Character. Vector to collapse to phrase
#' @param sep Character. Separator between all except last
#' @param end Character. Last separator
#' @param na_rm Logical. Remove `NA`?
#'
#' @return Character.
#' @export
#'
#' @examples
#' x <- c("apples", "bannanas", "pears", NA, "grapes")
#' vec_to_sentence(x)
#' vec_to_sentence(x, end = "&")
#' vec_to_sentence(x, ";")
#' vec_to_sentence(x, na_rm = F)
  vec_to_sentence <- function(x
                              , sep = ","
                              , end = "and"
                              , na_rm = TRUE
                              ) {

    sep <- paste0(sep, " ")
    end <- paste0(end, " ")

    if(na_rm) x <- x[!is.na(x)]

    x %>%
      paste(collapse = sep) %>%
      stringi::stri_replace_last_regex(sep
                                       , if(sep == "; ") {

                                         paste0(sep
                                                , end
                                                )

                                       } else {

                                         paste0(" "
                                                , end
                                                )

                                       }
                                       )
  }


#' Convert a numeric to its corresponding english character.
#'
#' Edited from the [Github](https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r)
#' file by [ateucher](https://github.com/ateucher).
#'
#' @param x Numeric
#'
#' @return Character. 'spelled out' (in english) x.
#' @export
#'
#' @examples
#' x <- 10
#' numbers2words(10)
#'
#' x <- c(x, 0, 20)
#' numbers2words(x)
#'
#' x <- c(x, sample(1:10000,1))
#' numbers2words(x)
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

    res <- trim(sapply(x, helper))

    res[x == 0] <- "zero"

    return(res)

  }



