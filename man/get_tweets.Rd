\name{get_tweets}
\alias{get_tweets}
\title{Grab tweets for a particular user}
\usage{
  get_tweets(user, n.tweets = 300)
}
\arguments{
  \item{user}{the name of a Twitter user}

  \item{n.tweets}{number of tweets to pull from the user}
}
\value{
  character vector of tweets.
}
\description{
  Here is where some blurb goes.
}
\section{Details

First need to use \code{\link{twitter_setup}} to get the correct credentials
to use the Twitter API. This only needs to be done once for then all called
to \code{\link{get_tweets}} can be used in one session.}{

}
\author{
  David L. Miller
}

