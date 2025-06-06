test_that("readGedcom reads and parses a GEDCOM file correctly", {
  # Create a temporary GEDCOM file for testing
  gedcom_content <- c(
    "0 HEAD",
    "1 GEDC",
    "2 VERS 5.5",
    "2 FORM LINEAGE-LINKED",
    "1 CHAR UTF-8",
    "1 LANG English",
    "0 @I1@ INDI",
    "1 NAME John /Doe/",
    "1 SEX M",
    "1 BIRT",
    "2 DATE 1 JAN 1900",
    "2 PLAC Someplace",
    "0 @I2@ INDI",
    "1 NAME Jane /Smith/",
    "1 SEX F",
    "1 BIRT",
    "2 DATE 2 FEB 1910",
    "2 PLAC Anotherplace",
    "1 NCHI 2"
  )
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  # Call readGedcom
  df <- readGedcom(temp_file, verbose = TRUE, skinny = FALSE)
  # note to self, the code is not reading in the 2nd person. and is also not reading in the birth date and place
  # Check that the data frame has the expected structure
  expect_true("personID" %in% colnames(df))
  expect_true("name_given" %in% colnames(df))
  expect_true("name_surn" %in% colnames(df))
  expect_true("sex" %in% colnames(df))
  expect_true("birth_date" %in% colnames(df))
  expect_true("birth_place" %in% colnames(df))

  # Check the contents of the data frame
  expect_equal(nrow(df), 2)
  expect_equal(df$name_given[1], "John")
  expect_equal(df$name_surn[1], "Doe")
  expect_equal(df$sex[1], "M")
  expect_equal(df$birth_date[1], "1 JAN 1900")
  expect_equal(df$birth_place[1], "Someplace")
  expect_equal(df$name_given[2], "Jane")
  expect_equal(df$name_surn[2], "Smith")
  expect_equal(df$sex[2], "F")
  expect_equal(df$birth_date[2], "2 FEB 1910")
  expect_equal(df$birth_place[2], "Anotherplace")

  # Clean up temporary file
  unlink(temp_file)
})

test_that("readGedcom combines duplicate columns correctly", {
  # Create a temporary GEDCOM file for testing
  gedcom_content <- c(
    "0 @I1@ INDI",
    "1 NAME John /Doe/",
    "1 GIVN John",
    "1 SEX M",
    "0 @I2@ INDI",
    "1 NAME Jane /Smith/",
    "1 GIVN Jane",
    "1 SEX F"
  )
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  # Call readGedcom with combine_cols = TRUE
  df <- readGedcom(temp_file, verbose = TRUE, combine_cols = TRUE)

  # Check that the data frame has the expected structure
  expect_true("name_given" %in% colnames(df))
  expect_false("name_given_pieces" %in% colnames(df))

  # Check the contents of the data frame
  expect_equal(nrow(df), 2)
  expect_equal(df$name_given[1], "John")
  expect_equal(df$name_given[2], "Jane")

  # Clean up temporary file
  unlink(temp_file)
})

test_that("readGedcom removes empty columns correctly", {
  # Create a temporary GEDCOM file for testing
  gedcom_content <- c(
    "0 @I1@ INDI",
    "1 NAME John /Doe/",
    "1 SEX M"
  )
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  # Call readGedcom with remove_empty_cols = TRUE
  df <- readGedcom(temp_file, verbose = TRUE, remove_empty_cols = TRUE)

  # Check that empty columns are removed
  expect_false("birth_date" %in% colnames(df))
  expect_false("birth_place" %in% colnames(df))

  # Clean up temporary file
  unlink(temp_file)
})

test_that("readGedcom handles skinny option correctly", {
  # Create a temporary GEDCOM file for testing
  gedcom_content <- c(
    "0 @I1@ INDI",
    "1 NAME John /Doe/",
    "1 SEX M",
    "1 FAMC @F1@",
    "1 FAMS @F2@"
  )
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  # Call readGedcom with skinny = TRUE
  df <- readGedcom(temp_file, verbose = TRUE, skinny = TRUE)

  # Check that FAMC and FAMS columns are removed
  expect_false("FAMC" %in% colnames(df))
  expect_false("FAMS" %in% colnames(df))

  # Clean up temporary file
  unlink(temp_file)
})

test_that("processParents adds momID and dadID correctly", {
  # Create a data frame for testing
  df_temp <- data.frame(
    personID = c("I1", "I2", "I3"),
    sex = c("M", "F", "M"),
    FAMS = c("@F1@", "@F1@", NA),
    FAMC = c(NA, NA, "@F1@"),
    stringsAsFactors = FALSE
  )

  # Call processParents
  df_temp <- processParents(df_temp, datasource = "gedcom")

  # Check the structure of the data frame
  expect_true("momID" %in% colnames(df_temp))
  expect_true("dadID" %in% colnames(df_temp))

  # Check the contents of the data frame
  expect_equal(df_temp$momID[1], NA_character_)
  expect_equal(df_temp$dadID[1], NA_character_)
  expect_equal(df_temp$momID[2], NA_character_)
  expect_equal(df_temp$dadID[2], NA_character_)
  expect_equal(df_temp$momID[3], "I2")
  expect_equal(df_temp$dadID[3], "I1")

  # Create a more complex data frame for testing
  df_temp <- data.frame(
    personID = c("I1", "I2", "I3", "I4", "I5"),
    sex = c("M", "F", "M", "F", "M"),
    FAMS = c("@F1@", "@F1@", "@F2@", "@F2@", "@F3@"),
    FAMC = c(NA, NA, "@F1@", "@F1@", "@F2@"),
    stringsAsFactors = FALSE
  )

  # Call processParents
  df_temp <- processParents(df_temp, datasource = "gedcom")

  # Check the contents of the data frame
  expect_equal(df_temp$momID[3], "I2")
  expect_equal(df_temp$dadID[3], "I1")
  expect_equal(df_temp$momID[4], "I2")
  expect_equal(df_temp$dadID[4], "I1")
  expect_equal(df_temp$momID[5], "I4")
  expect_equal(df_temp$dadID[5], "I3")
})

test_that("if file does not exist, readGedcom throws an error", {
  # Call readGedcom with a non-existent file
  expect_error(readGedcom("nonexistent.ged"))
})



test_that("readGedcom parses death event correctly", {
  # Test that a GEDCOM file with a death event is parsed correctly.
  gedcom_content <- c(
    "0 @I1@ INDI",
    "1 NAME John /Doe/",
    "1 SEX M",
    "1 DEAT",
    "2 DATE 31 DEC 2000",
    "2 PLAC Lastplace",
    "2 CAUS Old age",
    "2 LATI 12.3456",
    "2 LONG -65.4321"
  )
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  df <- readGedcom(temp_file, verbose = TRUE)

  expect_true("death_date" %in% colnames(df))
  expect_true("death_place" %in% colnames(df))
  expect_true("death_caus" %in% colnames(df))
  expect_true("death_lat" %in% colnames(df))
  expect_true("death_long" %in% colnames(df))

  expect_equal(df$death_date[1], "31 DEC 2000")
  expect_equal(df$death_place[1], "Lastplace")
  expect_equal(df$death_caus[1], "Old age")
  expect_equal(df$death_lat[1], "12.3456")
  expect_equal(df$death_long[1], "-65.4321")
  df_leg <- .readGedcom.legacy(temp_file, verbose = TRUE)

  expect_true("death_date" %in% colnames(df_leg))
  expect_true("death_place" %in% colnames(df_leg))
  expect_true("death_caus" %in% colnames(df_leg))
  expect_true("death_lat" %in% colnames(df_leg))
  expect_true("death_long" %in% colnames(df_leg))

  expect_equal(df_leg$death_date[1], "31 DEC 2000")
  expect_equal(df_leg$death_place[1], "Lastplace")
  expect_equal(df_leg$death_caus[1], "Old age")
  expect_equal(df_leg$death_lat[1], "12.3456")
  expect_equal(df_leg$death_long[1], "-65.4321")

  row.names(df) <- NULL
  row.names(df_leg) <- NULL
  df_leg <- dplyr::rename(df_leg, personID = id)
  expect_equal(df_leg, df)

  unlink(temp_file)
})

test_that("readGedcom handles incomplete individual records gracefully", {
  # Test that an individual record missing a NAME line is handled without error.
  gedcom_content <- c(
    "0 @I1@ INDI",
    "1 SEX M"
    # No NAME or BIRT information.
  )
  temp_file <- tempfile(fileext = ".ged")
  writeLines(gedcom_content, temp_file)

  df <- readGedcom(temp_file, verbose = TRUE)

  # Expect one record with missing name fields.
  expect_equal(nrow(df), 1)
  expect_true(is.null(df$name[1]))

  unlink(temp_file)
})
