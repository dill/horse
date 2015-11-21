#' Grab tweets for a particular user
#'
#' Here is where some blurb goes.
#'
#' @param user the name of a Twitter user
#' @param n.tweets number of tweets to pull from the user
#'
#' @return character vector of tweets.
#'
#' @section Details:
#' First need to use \code{\link{twitter_setup}} to get the correct credentials
#' to use the Twitter API. This only needs to be done once for then all called 
#' to \code{\link{get_tweets}} can be used in one session.
#'
#'
#' @author David L. Miller
#' @export
get_tweets <- function(user, n.tweets=300){

  # grab the timeline for the user and strip out the useless stuff
  timeline <- userTimeline(user, n=n.tweets)
  timeline <- unlist(lapply(timeline,function(x){x$text}))

  return(timeline)
}
