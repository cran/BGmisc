test_that("hazard data loads", {
  expect_silent(data(hazard))
})

test_that("inbreeding data loads", {
  expect_silent(data(inbreeding))
})

test_that("ped2fam gets the right families for hazard data", {
  data(hazard)
  ds <- ped2fam(hazard, famID = "newFamID")
  tab <- table(ds$FamID, ds$newFamID)
  expect_equal(ds$FamID, ds$newFamID)
})

test_that("ped2fam gets the right families for inbreeding data", {
  data(inbreeding)
  ds <- ped2fam(inbreeding, famID = "newFamID")
  tab <- table(ds$FamID, ds$newFamID)
  expect_equal(ds$FamID, ds$newFamID)
})

test_that("ped2graph produces a graph for hazard data", {
  expect_silent(data(hazard))
  g <- ped2graph(hazard)
  expect_true(inherits(g, "igraph"))
})

test_that("ped2graph produces a graph for inbreeding data", {
  expect_silent(data(inbreeding))
  g <- ped2graph(inbreeding)
  expect_true(inherits(g, "igraph"))
})



test_that("ped2add produces correct matrix dims, values, and dimnames for hazard", {
  tolerance <- 1e-10
  data(hazard)
  add <- ped2add(hazard)
  # Check dimension
  expect_equal(dim(add), c(nrow(hazard), nrow(hazard)))
  # Check several values
  #expect_true(all(diag(add) == 1))
  expect_true(sum((diag(add) - 1)^2) < tolerance)
  expect_equal(add, t(add))
  expect_equal(add[2, 1], 0)
  expect_equal(add[10, 1], .25)
  expect_equal(add[9, 1], 0)
  expect_equal(add["5", "6"], .5)
  # Check that dimnames are correct
  dn <- dimnames(add)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(hazard$ID))
})

test_that("ped2add produces correct matrix dims, values, and dimnames for alternative transpose", {
  tolerance <- 1e-10
  data(hazard)
  add <- ped2add(hazard, tcross.alt.crossprod = TRUE)
  # Check dimension
  expect_equal(dim(add), c(nrow(hazard), nrow(hazard)),tolerance = tolerance)
  # Check several values
  #expect_true(all(diag(add) == 1))
  expect_true(sum((diag(add) - 1)^2) < tolerance)
  expect_equal(add, t(add),tolerance = tolerance)
  expect_equal(add[2, 1], 0,tolerance = tolerance)
  expect_equal(add[10, 1], .25,tolerance = tolerance)
  expect_equal(add[9, 1], 0,tolerance = tolerance)
  expect_equal(add["5", "6"], .5)
  # Check that dimnames are correct
  dn <- dimnames(add)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(hazard$ID))
})
# to do, combine the sets that are equalivant. shouldn't need to run 1000 expect equals

test_that("ped2add produces correct matrix dims, values, and dimnames for inbreeding data", {
  tolerance <- 1e-10
  data(inbreeding)
  add <- ped2add(inbreeding)
  # Check dimension
  expect_equal(dim(add), c(nrow(inbreeding), nrow(inbreeding)),tolerance = tolerance)
  # Check several values
  expect_true(all(diag(add) >= 1-tolerance))
  expect_equal(add, t(add),tolerance = tolerance)
  expect_equal(add[2, 1], 0,tolerance = tolerance)
  expect_equal(add[6, 1], .5,tolerance = tolerance)
  expect_equal(add[113, 113], 1.1250,tolerance = tolerance)
  expect_equal(add["113", "112"], 0.62500)
  # Check that dimnames are correct
  dn <- dimnames(add)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})


test_that("ped2add produces correct matrix dims, values, and dimnames for inbreeding data with alternative transpose", {
  tolerance <- 1e-10
  data(inbreeding)
  add <- ped2add(inbreeding, tcross.alt.star = TRUE)
  # Check dimension
  expect_equal(dim(add), c(nrow(inbreeding), nrow(inbreeding)))
  # Check several values
  expect_true(all(diag(add) >= 1))
  expect_equal(add, t(add), tolerance = tolerance)
  expect_equal(add[2, 1], 0, tolerance = tolerance)
  expect_equal(add[6, 1], .5, tolerance = tolerance)
  expect_equal(add[113, 113], 1.1250, tolerance = tolerance)
  expect_equal(add["113", "112"], 0.62500)
  # Check that dimnames are correct
  dn <- dimnames(add)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})
test_that("ped2add flattens diagonal for inbreeding data", {
  tolerance <- 1e-10
  data(inbreeding)
  add <- ped2add(inbreeding, flatten.diag = TRUE)
  # Check dimension
  expect_equal(dim(add), c(nrow(inbreeding), nrow(inbreeding)), tolerance = tolerance)
  # Check several values
 # expect_true(all(diag(add) == 1))
  expect_true(sum((diag(add) - 1)^2) < tolerance)
  expect_equal(add, t(add), tolerance = tolerance)
  expect_equal(add[2, 1], 0, tolerance = tolerance)
  expect_equal(add[6, 1], .5, tolerance = tolerance)
  expect_equal(add[113, 113], 1, tolerance = tolerance)
  expect_equal(add["113", "112"], 0.62500)
  # Check that dimnames are correct
  dn <- dimnames(add)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})
test_that("ped2mit produces correct matrix dims, values, and dimnames for inbreeding", {
  tolerance <- 1e-10
  # Check dimension
  data(inbreeding)
  mit <- ped2mit(inbreeding)
  # Check dimension
  expect_equal(dim(mit), c(nrow(inbreeding), nrow(inbreeding)))
  # Check several values
 # expect_true(all(diag(mit) == 1))
  expect_true(sum((diag(mit) - 1)^2) < tolerance)
  expect_equal(mit, t(mit), tolerance = tolerance)
  expect_equal(mit[2, 1], 0, tolerance = tolerance)
  expect_equal(mit[6, 1], 1, tolerance = tolerance)
  expect_equal(mit[113, 113], 1, tolerance = tolerance)
  expect_equal(mit["113", "112"], 1, tolerance = tolerance)
  # Check that dimnames are correct
  dn <- dimnames(mit)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})

test_that("ped2mit produces correct matrix dims, values, and dimnames for inbreeding", {
  tolerance <- 1e-10
  # Check dimension
  data(inbreeding)
  mit <- ped2mit(inbreeding)
  # Check dimension
  expect_equal(dim(mit), c(nrow(inbreeding), nrow(inbreeding)), tolerance = tolerance)
  # Check several values
  # expect_true(all(diag(mit) == 1))
  expect_true(sum((diag(mit) - 1)^2) < tolerance)
  expect_equal(mit, t(mit), tolerance = tolerance)
  expect_equal(mit[2, 1], 0, tolerance = tolerance)
  expect_equal(mit[6, 1], 1, tolerance = tolerance)
  expect_equal(mit[113, 113], 1, tolerance = tolerance)
  expect_equal(mit["113", "112"], 1, tolerance = tolerance)
  # Check that dimnames are correct
  dn <- dimnames(mit)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})

test_that("ped2cn produces correct matrix dims, values, and dimnames", {
  tolerance <- 1e-10

  # Check dimension
  data(inbreeding)
  cn <- ped2cn(inbreeding)
  expect_equal(dim(cn), c(nrow(inbreeding), nrow(inbreeding)), tolerance = tolerance)
  # Check several values
 # expect_true(all(diag(cn) == 1))
  expect_true(sum((diag(cn) - 1)^2) < tolerance)
  expect_equal(cn, t(cn), tolerance = tolerance)
  expect_equal(cn[2, 1], 0, tolerance = tolerance)
  expect_equal(cn[6, 1], 0, tolerance = tolerance)
  expect_equal(cn[113, 113], 1, tolerance = tolerance)
  expect_equal(cn["113", "112"], 1, tolerance = tolerance)
  # Check that dimnames are correct
  dn <- dimnames(cn)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
  #  expect_silent(data(inbreeding))
})

test_that("ped2ce produces correct matrix dims, values, and dimnames", {
  tolerance <- 1e-10
  data(inbreeding)
  ce <- ped2ce(inbreeding)
  expect_equal(dim(ce), c(nrow(inbreeding), nrow(inbreeding)), tolerance = tolerance)
  # Check several values
 # expect_true(all(diag(ce) == 1))
  expect_true(sum((diag(ce) - 1)^2) < tolerance)
  expect_equal(ce, t(ce), tolerance = tolerance)
  expect_equal(ce[2, 1], 1, tolerance = tolerance)
  expect_equal(ce[6, 1], 1, tolerance = tolerance)
  expect_equal(ce[113, 113], 1, tolerance = tolerance)
  expect_equal(ce["113", "112"], 1, tolerance = tolerance)
  # Check that dimnames are correct
  dn <- dimnames(ce)
  expect_equal(dn[[1]], dn[[2]])
  expect_equal(dn[[1]], as.character(inbreeding$ID))
})

test_that("ped2add verbose prints updates", {
  data(hazard)
  expect_output(ped2add(hazard, verbose = TRUE), regexp = "Family Size =")
})



test_that("ped2maternal/paternal produces correct matrix dims", {
  data(hazard)
  tolerance <- 1e-10
  mat <- ped2maternal(hazard)
  expect_equal(dim(mat), c(nrow(hazard), ncol(hazard) + 1))
  data(hazard)
  pat <- ped2paternal(hazard)
  expect_equal(dim(pat), c(nrow(hazard), ncol(hazard) + 1))

  expect_lt(cor(pat$patID, mat$matID), 1)
})
