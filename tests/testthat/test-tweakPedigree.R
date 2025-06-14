# Test for makeTwins function
test_that("makeTwins - Twins specified by IDs", {
  set.seed(1234)
  ped <- data.frame(
    famID = c(1, 1, 2, 2),
    ID = c(1, 2, 3, 4),
    gen = c(1, 1, 2, 2),
    dadID = c(NA, NA, 1, 1),
    momID = c(NA, NA, 2, 2),
    spID = c(NA, NA, NA, NA),
    sex = c("M", "F", "M", "F")
  )
  expected_result <- data.frame(
    famID = c(1, 1, 2, 2),
    ID = c(1, 2, 3, 4),
    gen = c(1, 1, 2, 2),
    dadID = c(NA, NA, 1, 1),
    momID = c(NA, NA, 2, 2),
    spID = c(NA, NA, NA, NA),
    sex = c("M", "F", "M", "F"),
    twinID = c(2, 1, NA, NA),
    zygosity = c("MZ", "MZ", NA, NA)
  )
  result <- makeTwins(ped, ID_twin1 = 1, ID_twin2 = 2)

  expect_equal(result, expected_result)

  # does it handle weird variable names? "fam" = "famID"

  names(ped)[1] <- "fam"

  result_badfam <- makeTwins(ped, ID_twin1 = 1, ID_twin2 = 2, verbose = TRUE)

  expect_equal(result_badfam, expected_result)


  result2 <- makeTwins(ped,
    ID_twin1 = 1,
    ID_twin2 = 2,
    verbose = TRUE, zygosity = "DZ"
  )

  expected_result$zygosity[expected_result$zygosity == "MZ"] <- "DZ"
  expect_equal(result2, expected_result)
  # hp <- makeTwins(potter, ID_twin1 = 12, ID_twin2 = 13, verbose = TRUE)
  result3 <- makeTwins(ped,
    ID_twin1 = 1,
    ID_twin2 = 2,
    verbose = TRUE, zygosity = "SS"
  )
  expected_result$zygosity[expected_result$zygosity == "DZ"] <- "SS"
  expect_equal(result3, expected_result)
})

test_that("makeTwins - mz Twins specified by generation", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  gen_twin <- 2
  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  #
  result <- makeTwins(ped, gen_twin = gen_twin)
  expect_equal(names(result), c("famID", "ID", "gen", "dadID", "momID", "spID", "sex", "twinID", "zygosity"))
  # do we have the same people?
  expect_equal(result$ID, ped$ID)
  # did it make one pair of twins?
  expect_equal(sum(!is.na(result$twinID)), 2)
  # did it make the pair in the correct generation?
  expect_equal(mean(result$gen[!is.na(result$twinID)]), gen_twin)
  # are they the same sex?
  expect_equal(length(unique(result$sex[!is.na(result$twinID)])), 1)
  # are they from the same family?
  expect_equal(length(unique(result$fam[!is.na(result$twinID)])), 1)
  # do they have the same mom?
  expect_equal(length(unique(result$momID[!is.na(result$twinID)])), 1)
  # do they have the same dad?
  expect_equal(length(unique(result$dadID[!is.na(result$twinID)])), 1)
})

test_that("makeTwins - dz Twins specified by generation", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  gen_twin <- 2
  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  resultdz <- makeTwins(ped, gen_twin = gen_twin, zygosity = "DZ")

  expect_equal(names(resultdz), c("famID", "ID", "gen", "dadID", "momID", "spID", "sex", "twinID", "zygosity"))
  # do we have the same people?
  expect_equal(resultdz$ID, ped$ID)
  # did it make one pair of twins?
  expect_equal(sum(!is.na(resultdz$twinID)), 2)
  # did it make the pair in the correct generation?
  expect_equal(mean(resultdz$gen[!is.na(resultdz$twinID)]), gen_twin)
  # are they the same sex?
  expect_equal(length(unique(resultdz$sex[!is.na(resultdz$twinID)])), 1)
  # are they from the same family?
  expect_equal(length(unique(resultdz$fam[!is.na(resultdz$twinID)])), 1)
  # do they have the same mom?
  expect_equal(length(unique(resultdz$momID[!is.na(resultdz$twinID)])), 1)
  # do they have the same dad?
  expect_equal(length(unique(resultdz$dadID[!is.na(resultdz$twinID)])), 1)
})
test_that("makeTwins - os Twins specified by generation", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  gen_twin <- 2
  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  resultss <- makeTwins(ped, gen_twin = gen_twin, zygosity = "SS")
  expect_equal(names(resultss), c("famID", "ID", "gen", "dadID", "momID", "spID", "sex", "twinID", "zygosity"))
  # do we have the same people?
  expect_equal(resultss$ID, ped$ID)
  # did it make one pair of twins?
  expect_equal(sum(!is.na(resultss$twinID)), 2)
  # did it make the pair in the correct generation?
  expect_equal(mean(resultss$gen[!is.na(resultss$twinID)]), gen_twin)
  # are they the same sex?
  expect_equal(length(unique(resultss$sex[!is.na(resultss$twinID)])), 1)
  # are they from the same family?
  expect_equal(length(unique(resultss$fam[!is.na(resultss$twinID)])), 1)
  # do they have the same mom?
  expect_equal(length(unique(resultss$momID[!is.na(resultss$twinID)])), 1)
  # do they have the same dad?
  expect_equal(length(unique(resultss$dadID[!is.na(resultss$twinID)])), 1)
})

# Test for makeInbreeding function
test_that("makeInbreeding - Inbred mates specified by IDs", {
  ped <- data.frame(
    famID = c(1, 1, 2, 2),
    ID = c(1, 2, 3, 4),
    gen = c(1, 1, 2, 2),
    dadID = c(NA, NA, 1, 1),
    momID = c(NA, NA, 2, 2),
    spID = c(NA, NA, NA, NA),
    sex = c("M", "F", "M", "F")
  )
  expected_result <- data.frame(
    famID = c(1, 1, 2, 2),
    ID = c(1, 2, 3, 4),
    gen = c(1, 1, 2, 2),
    dadID = c(NA, NA, 1, 1),
    momID = c(NA, NA, 2, 2),
    spID = c(2, 1, NA, NA),
    sex = c("M", "F", "M", "F")
  )
  result <- makeInbreeding(ped, ID_mate1 = 1, ID_mate2 = 2)
  expect_equal(result, expected_result)
})

test_that("makeInbreeding - Inbred mates specified by generation and sibling", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  gen_inbred <- 2
  type_inbred <- "sibling"

  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  #
  result <- makeInbreeding(ped, gen_inbred = gen_inbred, type_inbred = type_inbred)
  expect_equal(names(result), c("famID", "ID", "gen", "dadID", "momID", "spID", "sex"))

  # do we have the same people?
  expect_equal(result$ID, ped$ID)

  # did we get more spID values than we started with?
  expect_gt(sum(!is.na(result$spID)), sum(!is.na(ped$spID)))
})

test_that("makeInbreeding - Inbred mates specified by generation and cousin", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  gen_inbred <- 2
  type_inbred <- "cousin"
  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)

  expect_error(makeInbreeding(ped,
    gen_inbred = gen_inbred,
    type_inbred = type_inbred, verbose = TRUE
  ), regexp = "Cousin inbreedin")
})

test_that("makeInbreeding - Inbred mates specified by generation and non inplemented relations", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  gen_inbred <- 2
  type_inbred <- "not_implemented"
  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)

  expect_error(makeInbreeding(ped,
    gen_inbred = gen_inbred,
    type_inbred = type_inbred, verbose = TRUE
  ), regexp = "The type of inbreeding should be either 'sib' or 'cousin'")
})

test_that("dropLink - Drop specified by ID", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  ID_drop <- 10021

  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  result <- dropLink(ped, ID_drop = ID_drop)


  # are the dataframes the same in both the undropped and dropepd relationships for all but the dropped ID?
  expect_equal(result[result$ID != ID_drop, ], ped[ped$ID != ID_drop, ])

  # are the families of the dropped ID in the original?
  expect_true(!is.na(ped$dadID[ped$ID == ID_drop]) & !is.na(ped$momID[ped$ID == ID_drop]))

  # are the families dropped from the dropped
  expect_true(is.na(result$dadID[result$ID == ID_drop]) & is.na(result$momID[result$ID == ID_drop]))
})


test_that("dropLink - Drop specified by ID", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  ID_drop <- 10021

  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  result <- dropLink(ped, ID_drop = ID_drop)

  # are the dataframes the same in both the undropped and dropped relationships for all but the dropped ID?
  expect_equal(result[result$ID != ID_drop, ], ped[ped$ID != ID_drop, ])

  # are the families of the dropped ID in the original?
  expect_true(!is.na(ped$dadID[ped$ID == ID_drop]) & !is.na(ped$momID[ped$ID == ID_drop]))

  # are the families dropped from the dropped
  expect_true(is.na(result$dadID[result$ID == ID_drop]) & is.na(result$momID[result$ID == ID_drop]))
})

test_that("dropLink - Drop specified by generation", {
  set.seed(15)
  Ngen <- 4
  kpc <- 4
  sexR <- .50
  marR <- .7
  gen_drop <- 2

  ped <- simulatePedigree(kpc = kpc, Ngen = Ngen, sexR = sexR, marR = marR)
  result <- dropLink(ped, gen_drop = gen_drop)

  # are the dataframes the same in both the undropped and dropped relationships for all but the dropped gen?
  expect_equal(result[result$gen != gen_drop, ], ped[ped$gen != gen_drop, ])

  # are there more missing dads?
  expect_lt(sum(is.na(ped$dadID[ped$gen == gen_drop])), sum(is.na(result$dadID[result$gen == gen_drop])))
  # are there more missing moms?
  expect_lt(sum(is.na(ped$momID[ped$gen == gen_drop])), sum(is.na(result$momID[result$gen == gen_drop])))
})
