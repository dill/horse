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
#' First need to use \code{\link{twitter_setup}} to get the correct credentials to use the Twitter API. This only needs to be done once for then all called to \code{\link{get_tweets}} can be used in one session.
#'
#' Retweets are not included in the returned tweets.
#'
#' @author David L. Miller
#' @export
#' @importFrom twitteR userTimeline
get_tweets <- function(user, n.tweets=300, sleep_time=3){

  last_ID <- NULL
  tweets <- c()
  n <- min(n.tweets, 3000)

  last_ID <- NULL

  while(length(tweets) < n.tweets){
    # grab the timeline for the user and strip out the useless stuff
    timeline <- userTimeline(user, n=2*n, sinceID=last_ID)

    # get the last ID:
    if(length(timeline)==0) break()
    last_ID <- timeline[[length(timeline)]]$id
    tweets <- unique(c(tweets, unlist(lapply(timeline, function(x){x$text}))))
    n <- n.tweets-length(tweets)
    Sys.sleep(sleep_time)
  }


  return(tweets[1:min(n.tweets, length(tweets))])
}
