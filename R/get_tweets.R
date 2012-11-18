#' Grab tweets for a particular user
#'
#' Here is where some blurb goes.
#'
#' @param user the name of a Twitter user
#' @param consumer.key your Twitter "consumer" key, see Details.
#' @param consumer.secret your Twitter "consumer" secret, see Details.
#' @param access.token access token
#' @param access.token.secret access token secret
#' @param n.tweets number of tweets to pull from the user
#'
#' @section Details
#'
#' First need to set up a "new app" get key and secret...
#' https://dev.twitter.com/apps/new
#'
#' @author David L. Miller
#' @export
get_tweets <- function(user, consumer.key, consumer.secret, access.token,
                       access.token.secret, n.tweets=300){

  # these shouldn't change
  requestURL <- "https://api.twitter.com/oauth/request_token"
  accessURL  <- "https://api.twitter.com/oauth/authorize"
  authURL    <- "https://api.twitter.com/oauth/access_token"

  # build the OAuth
  cred <- OAuthFactory$new(consumerKey=consumer.key,
                           consumerSecret=consumer.secret,
                           requestURL=requestURL,
                           accessURL=accessURL,
                           authURL=authURL)

  # this doesn't work at the moment
  #cred$handshake()

  # workaround
  cred$handshakeComplete <- TRUE
  cred$oauthKey <- access.token
  cred$oauthSecret <- access.token.secret
  cred$signMethod <- "HMAC"

  # need to do this either way
  registerTwitterOAuth(cred)

  # grab the timeline for the user and strip out the useless stuff
  timeline <- userTimeline(user, n=n.tweets)
  timeline <- unlist(lapply(timeline,function(x){x$text}))

  return(timeline)
}
