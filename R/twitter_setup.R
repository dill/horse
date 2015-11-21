#' Setup a connection to the Twitter API
#'
#' Now just calls \code{\link{setup_twitter_oauth}}
#'
#' @param consumer.key your Twitter "consumer" key, see Details.
#' @param consumer.secret your Twitter "consumer" secret, see Details.
#' @param access.token access token
#' @param access.token.secret access token secret
#'
#' @section Details:
#' First need to set up a "new app" get key and secret...
#' https://dev.twitter.com/apps/new
#'
#' @author David L. Miller
#' @export
#' @importFrom twitteR setup_twitter_oauth
twitter_setup <- function(consumer.key, consumer.secret, access.token,
                       access.token.secret){

  setup_twitter_oauth(consumer.key, consumer.secret,
                      access.token, access.token.secret)
  invisible()
}
