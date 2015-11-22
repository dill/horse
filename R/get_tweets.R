#' Grab tweets for a particular user
#'
#' Here is where some blurb goes.
#'
#' @param user the name of a Twitter user
#' @param n.tweets number of tweets to pull from the user
#' @param sleep_time seconds to sleep inbetween requests
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
#' @importFrom twitteR userTimeline
get_tweets <- function(user, n.tweets=300, sleep_time=10){

  last_ID <- NULL
  tweets <- c()
  n <- n.tweets

  while(length(tweets) < n.tweets){
    # grab the timeline for the user and strip out the useless stuff
    timeline <- userTimeline(user, n=n, excludeReplies=TRUE)

    # get the last ID:
    if(length(timeline)==0) break()
    last_ID <- timeline[[length(timeline)]]$id
    tweets <- c(tweets, unlist(lapply(timeline, function(x){x$text})))
    n <- n.tweets-length(tweets)
    Sys.sleep(sleep_time)
  }


  return(tweets)
}
