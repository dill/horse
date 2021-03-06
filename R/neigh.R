#' Generate a string from a horse object
#'
#' This function generates strings from horse objects. Once the transition
#' matrix has been found for your corpus (e.g. using \code{\link{get_tweets}}
#' you can use the resulting horse object (generated by
#' \code{\link{make_horse}}) to generate new strings.
#'
#' @param horse.obj a horse object containing the transition matrix for the
#'  Markov chain.
#' @param length maximum length for the resulting string (default 140).
#'
#' @return a string of length \code{length}.
#'
#' @author David L. Miller
#' @importFrom stats rmultinom
#' @export
neigh <- function(horse.obj,length=140){

  tm <- horse.obj$transition.matrix
  lookup <- horse.obj$lookup
  strings <- horse.obj$strings


  # id of TKTKTKSTART
  start.id <- unique(strings$i[strings$str=="TKTKTKSTART"])

  # find a starting point
  first <- which(rmultinom(1,1,tm[start.id,])==1, arr.ind=TRUE)[1]

  # string holder
  this.str <- ""

  # keep going until we get to 140 characters
  while (length(strsplit(this.str,"")[[1]]) < length){

    first <- which(rmultinom(1,1,tm[first,])==1, arr.ind=TRUE)[1]
    new.str <- as.character(unique(lookup$word[lookup$i == first]))

    # if we get to the end then stop
    if(new.str == "TKTKTKEND"){
      break
    }

    # remove any links or usernames
    if(!(grepl("@",new.str) | grepl("http:",new.str) |
         grepl("\n",new.str)| grepl("https:",new.str))){
      this.str <- paste(this.str,new.str,sep=" ")
    }
  }

  # if we returned an empty string re-run
  if(this.str==" "){
    this.str <- neigh(horse.obj,length)
  }

  # remove starting whitespace
  this.str <- sub("^ ", "", this.str)

  return(this.str)
}
