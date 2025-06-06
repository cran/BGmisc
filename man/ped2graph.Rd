% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/segmentPedigree.R
\name{ped2graph}
\alias{ped2graph}
\title{Turn a pedigree into a graph}
\usage{
ped2graph(
  ped,
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  directed = TRUE,
  adjacent = c("parents", "mothers", "fathers"),
  ...
)
}
\arguments{
\item{ped}{a pedigree dataset.  Needs ID, momID, and dadID columns}

\item{personID}{character.  Name of the column in ped for the person ID variable}

\item{momID}{character.  Name of the column in ped for the mother ID variable}

\item{dadID}{character.  Name of the column in ped for the father ID variable}

\item{directed}{Logical scalar. Default is TRUE. Indicates whether or not to create a directed graph.}

\item{adjacent}{Character.  Relationship that defines adjacency in the graph: parents, mothers, or fathers}

\item{...}{additional arguments to be passed to \code{\link{ped2com}}}
}
\value{
A graph
}
\description{
Turn a pedigree into a graph
}
\details{
The general idea of this function is to represent a pedigree as a graph using the igraph package.

Once in graph form, several common pedigree tasks become much simpler.

The \code{adjacent} argument allows for different kinds of graph structures.
When using \code{parents} for adjacency, the graph shows all parent-child relationships.
When using \code{mother} for adjacency, the graph only shows mother-child relationships.
Similarly when using \code{father} for adjacency, only father-child relationships appear in the graph.
Construct extended families from the parent graph, maternal lines from the mothers graph,
and paternal lines from the fathers graph.
}
