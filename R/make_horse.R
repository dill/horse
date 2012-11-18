#' Build a horse object
#'
#' This function takes a set of tweets and builds a transition matrix from
#' them. This then gives a Markov chain that can be used to generate new
#' strings from.
#'
#' @param tweets a character vector.
#'
#' @return a horse object ready to generate tweets from.
#'
#' @author David L. Miller
#' @export
make_horse <- function(tweets){

  # append start and stop strings to each tweet
  fl <- function(x){c("TKTKTKSTART", unlist(strsplit(x," ")),"TKTKTKEND")}
  strings <- lapply(tweets,fl)

  # unlist everything
  name.strings <- as.factor(unlist(strings))
  ustrings <- as.numeric(as.factor(unlist(strings)))
  # find the unique strings and create storage
  nustrings <- length(unique(ustrings))

  # pairs
  string.pairs <- cbind(ustrings[-length(ustrings)],
                        ustrings[2:length(ustrings)])

  pp <- apply(string.pairs,1,paste,collapse="-")
  pp.tab <- table(pp)

  ijs <- strsplit(names(pp.tab),"-")

  i <- unlist(lapply(ijs,function(x){as.numeric(x[1])}))
  j <- unlist(lapply(ijs,function(x){as.numeric(x[2])}))

  # a transition matrix!
  cc <- spMatrix(nrow=length(i), ncol=length(j),
                 i=i,
                 j=j,
                 x=pp.tab)


  # need a lookup table for the words
  lookup <- data.frame(i=ustrings,
                       word=as.character(unlist(strings)))

  # build the return object
  horse.obj <- list(lookup=lookup,
                    transition.matrix=cc,
                    strings=data.frame(i=ustrings,str=name.strings))

  return(horse.obj)
}
