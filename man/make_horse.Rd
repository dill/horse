\name{make_horse}
\alias{make_horse}
\title{Build a horse object}
\usage{
  make_horse(tweets)
}
\arguments{
  \item{tweets}{a character vector.}
}
\value{
  a horse object ready to generate tweets from.
}
\description{
  This function takes a set of tweets and builds a
  transition matrix from them. This then gives a Markov
  chain that can be used to generate new strings from.
}
\author{
  David L. Miller
}

