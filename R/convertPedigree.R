#' Take a pedigree and turn it into a relatedness matrix
#' @param ped a pedigree dataset.  Needs ID, momID, and dadID columns
#' @param component character.  Which component of the pedigree to return.  See Details.
#' @param max.gen the maximum number of generations to compute
#'  (e.g., only up to 4th degree relatives).  The default is 25. However it can be set to infinity.
#'   `Inf` uses as many generations as there are in the data.
#' @param sparse logical.  If TRUE, use and return sparse matrices from Matrix package
#' @param verbose logical.  If TRUE, print progress through stages of algorithm
#' @param gc logical. If TRUE, do frequent garbage collection via \code{\link{gc}} to save memory
#' @param flatten.diag logical. If TRUE, overwrite the diagonal of the final relatedness matrix with ones
#' @param standardize.colnames logical. If TRUE, standardize the column names of the pedigree dataset
#' @param tcross.alt.crossprod logical. If TRUE, use alternative method of using Crossprod function for computing the transpose
#' @param tcross.alt.star logical. If TRUE, use alternative method of using \%\*\% for computing the transpose
#' @param ... additional arguments to be passed to \code{\link{ped2com}}
#' @details The algorithms and methodologies used in this function are further discussed and exemplified in the vignette titled "examplePedigreeFunctions". For more advanced scenarios and detailed explanations, consult this vignette.
#' @export
#'
ped2com <- function(ped, component,
                    max.gen = 25,
                    sparse = FALSE,
                    verbose = FALSE,
                    gc = FALSE,
                    flatten.diag = FALSE,
                    standardize.colnames = TRUE,
                    tcross.alt.crossprod = FALSE,
                    tcross.alt.star = FALSE,
                    ...) {
  # Validate the 'component' argument and match it against predefined choices
  component <- match.arg(tolower(component),
    choices = c(
      "generation",
      "additive",
      "common nuclear",
      "mitochondrial"
    )
  )
  # standardize colnames
  if (standardize.colnames) {
    ped <- standardizeColnames(ped)
  }
  # Get the number of rows in the pedigree dataset, representing the size of the family
  nr <- nrow(ped)

  # Print the family size if verbose is TRUE
  if (verbose) {
    cat(paste0("Family Size = ", nr, "\n"))
  }
  # Initialize variables
  parList <- list()
  lens <- integer(nr)

  # Loop through each individual in the pedigree build the adjacency matrix for parent-child relationships
  # Is person in column j the parent of the person in row i? .5 for yes, 0 for no.
  for (i in 1:nr) {
    x <- ped[i, , drop = FALSE]

    # Handle parentage according to the 'component' specified
    if (component %in% c("generation", "additive")) {
      # Code for 'generation' and 'additive' components
      # Checks if is mom of ID or is dad of ID
      sMom <- (as.numeric(x["ID"]) == as.numeric(ped$momID))
      sDad <- (as.numeric(x["ID"]) == as.numeric(ped$dadID))
      val <- sMom | sDad
      val[is.na(val)] <- FALSE
    } else if (component %in% c("common nuclear")) {
      # Code for 'common nuclear' component
      # IDs have the Same mom and Same dad
      sMom <- (as.numeric(x["momID"]) == as.numeric(ped$momID))
      sMom[is.na(sMom)] <- FALSE
      sDad <- (as.numeric(x["dadID"]) == as.numeric(ped$dadID))
      sDad[is.na(sDad)] <- FALSE
      val <- sMom & sDad
    } else if (component %in% c("mitochondrial")) {
      # Code for 'mitochondrial' component
      sMom <- (as.numeric(x["ID"]) == as.numeric(ped$momID))
      sDad <- TRUE
      val <- sMom & sDad
      val[is.na(val)] <- FALSE
    } else {
      stop("Unknown relatedness component requested")
    }
    # Storing the indices of the parent-child relationships
    # keep track of indices only, and then initialize a single sparse matrix
    wv <- which(val)
    parList[[i]] <- wv
    lens[i] <- length(wv)
    # Print progress if verbose is TRUE
    if (verbose && !(i %% 100)) {
      cat(paste0("Done with ", i, " of ", nr, "\n"))
    }
  }
  # Construct sparse matrix
  jss <- rep(1L:nr, times = lens)
  iss <- unlist(parList)

  # Garbage collection if gc is TRUE
  if (gc) {
    rm(parList, lens)
    gc()
  }

  # Set parent values depending on the component type
  if (component %in% c("generation", "additive")) {
    parVal <- .5
  } else if (component %in% c("common nuclear", "mitochondrial")) {
    parVal <- 1
  } else {
    stop("Don't know how to set parental value")
  }

  # Initialize adjacency matrix for parent-child relationships
  isPar <- Matrix::sparseMatrix(
    i = iss,
    j = jss,
    x = parVal,
    dims = c(nr, nr),
    dimnames = list(ped$ID, ped$ID)
  )

  if (verbose) {
    cat("Completed first degree relatives (adjacency)\n")
  }
  # isPar is the adjacency matrix.  'A' matrix from RAM
  if (component %in% c("common nuclear")) {
    Matrix::diag(isPar) <- 1
    if (!sparse) {
      isPar <- as.matrix(isPar)
    }
    return(isPar)
  }
  isChild <- apply(ped[, c("momID", "dadID")], 1, function(x) {
    2^(-!all(is.na(x)))
  })
  # isChild is the 'S' matrix from RAM
  r <- Matrix::Diagonal(x = 1, n = nr)
  gen <- rep(1, nr)
  mtSum <- sum(r, na.rm = TRUE)
  newIsPar <- isPar
  count <- 0
  maxCount <- max.gen + 1
  if (verbose) {
    cat("About to do RAM path tracing\n")
  }
  # r is I + A + A^2 + ... = (I-A)^-1 from RAM
  while (mtSum != 0 & count < maxCount) {
    r <- r + newIsPar
    gen <- gen + (Matrix::rowSums(newIsPar) > 0)
    newIsPar <- newIsPar %*% isPar
    mtSum <- sum(newIsPar)
    count <- count + 1
    if (verbose) {
      cat(paste0("Completed ", count - 1, " degree relatives\n"))
    }
  }
  # compute rsq <- r %*% sqrt(diag(isChild))
  # compute rel <- tcrossprod(rsq)
  if (gc) {
    rm(isPar, newIsPar)
    gc()
  }
  if (verbose) {
    cat("Doing I-A inverse times diagonal multiplication\n")
  }
  r2 <- r %*% Matrix::Diagonal(x = sqrt(isChild), n = nr)
  if (gc) {
    rm(r, isChild)
    gc()
  }
  if (verbose) {
    cat("Doing tcrossprod\n")
  }
  if (tcross.alt.crossprod) {
    if (verbose) {
      cat("Doing alt tcrossprod crossprod t \n")
    }
    r <- crossprod(t(as.matrix(r2)))
  } else if (tcross.alt.star) {
    if (verbose) {
      cat("Doing alt tcrossprod %*% t \n")
    }
    r <- r2 %*% t(as.matrix(r2))
  } else {
    r <- Matrix::tcrossprod(r2)
  }
  if (component == "generation") {
    return(gen)
  } else {
    if (component == "mitochondrial") {
      r@x <- rep(1, length(r@x))
      # Assign 1 to all nonzero elements for mitochondrial component
    }
    if (!sparse) {
      r <- as.matrix(r)
    }
    if (flatten.diag) { # flattens diagonal if you don't want to deal with inbreeding
      diag(r) <- 1
    }
    return(r)
  }
}

#' Take a pedigree and turn it into an additive genetics relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#'
ped2add <- function(ped, max.gen = 25, sparse = FALSE, verbose = FALSE, gc = FALSE, flatten.diag = FALSE, standardize.colnames = TRUE,
                    tcross.alt.crossprod = FALSE, tcross.alt.star = FALSE) {
  ped2com(
    ped = ped,
    max.gen = max.gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "additive",
    flatten.diag = flatten.diag,
    standardize.colnames = standardize.colnames,
    tcross.alt.crossprod = tcross.alt.crossprod,
    tcross.alt.star = tcross.alt.star
  )
}

#' Take a pedigree and turn it into a mitochondrial relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#' @aliases ped2mt
#'
ped2mit <- ped2mt <- function(ped, max.gen = 25, sparse = FALSE, verbose = FALSE, gc = FALSE, flatten.diag = FALSE, standardize.colnames = TRUE, tcross.alt.crossprod = FALSE, tcross.alt.star = FALSE) {
  ped2com(
    ped = ped,
    max.gen = max.gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "mitochondrial",
    flatten.diag = flatten.diag,
    standardize.colnames = standardize.colnames,
    tcross.alt.crossprod = tcross.alt.crossprod,
    tcross.alt.star = tcross.alt.star
  )
}

#' Take a pedigree and turn it into a common nuclear environmental relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#'
ped2cn <- function(ped, max.gen = 25, sparse = FALSE, verbose = FALSE, gc = FALSE, flatten.diag = FALSE, standardize.colnames = TRUE, tcross.alt.crossprod = FALSE, tcross.alt.star = FALSE) {
  ped2com(
    ped = ped,
    max.gen = max.gen,
    sparse = sparse,
    verbose = verbose,
    gc = gc,
    component = "common nuclear",
    flatten.diag = flatten.diag,
    standardize.colnames = standardize.colnames,
    tcross.alt.crossprod = tcross.alt.crossprod,
    tcross.alt.star = tcross.alt.star
  )
}

#' Take a pedigree and turn it into an extended environmental relatedness matrix
#' @inheritParams ped2com
#' @inherit ped2com details
#' @export
#'
ped2ce <- function(ped) {
  matrix(1, nrow = nrow(ped), ncol = nrow(ped), dimnames = list(ped$ID, ped$ID))
}
