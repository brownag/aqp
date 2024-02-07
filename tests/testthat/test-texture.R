context("fragvol_to_texmod")


# fragvol_to_texmod ----
test_that("increasing volume of one size class", {
  expect_equal(c("by", "byv", "byx", NA), fragvol_to_texmod(data.frame(boulders = c(25, 48, 75, 95)))$texmod)
  expect_equal(c(NA, NA, NA, "by"), fragvol_to_texmod(data.frame(boulders = c(25, 48, 75, 95)))$lieutex)
})


test_that("2:1 rules for dominant/largest size class", {
  expect_equal("cbx", fragvol_to_texmod(data.frame(gravel = 20, cobbles = 40))$texmod)
  expect_equal("grx", fragvol_to_texmod(data.frame(gravel = 40, cobbles = 20))$texmod)
})


test_that("no fragments > 15%", {
  expect_equal(NA_character_, fragvol_to_texmod(data.frame(gravel = 1, cobbles = 2, flagstones = 10))$texmod)
})
  

test_that("sum of fragments plus parafragments >15%, but nopf<15 and pf<15", {
  expect_equal("pgr", fragvol_to_texmod(data.frame(gravel = 14, paragravel = 2))$texmod)
})


## texture_to_texmod ----

test_that("very cobbly loam (works)", {
  expect_equal(texture_to_texmod("CBV-L"), "cbv")
})

test_that("ashy cobbly sandy loam (works)", {
  expect_equal(texture_to_texmod("ASHY-CB-SL"), "cb")
})

test_that("ashy boulders (works)", {
  expect_equal(texture_to_texmod("ASHY-BY"), NA_character_)
})


## texcl_to_classlimit ----

test_that("single class limits match expected output", {
  x <- texcl_to_classlimit(c("l", "sicl", "cl"))

  expect_equal(x$texcl, c("l", "sicl", "cl"))
  expect_equal(x$clay_l, c(7, 27, 27))
  expect_equal(x$clay_m, c(17, 33.5, 33.5))
  expect_equal(x$clay_h, c(27, 40, 40))
  expect_equal(x$sand_l, c(23, 0, 20))
  expect_equal(x$sand_m, c(37.5, 10, 32.5))
  expect_equal(x$sand_h, c(52, 20, 45))
  expect_equal(x$silt_l, c(28, 40, 15))
  expect_equal(x$silt_m, c(39, 56.5, 34))
  expect_equal(x$silt_h, c(50, 73, 53))
})

test_that("grouped class limits match expected envelope", {
  x <- texcl_to_classlimit(list(c("l", "sicl", "cl")))

  expect_equal(x$texcl, "l,sicl,cl")
  expect_equal(x$clay_l, 7)
  expect_equal(x$clay_m, 23.5)
  expect_equal(x$clay_h, 40)
  expect_equal(x$sand_l, 0)
  expect_equal(x$sand_m, 26)
  expect_equal(x$sand_h, 52)
  expect_equal(x$silt_l, 15)
  expect_equal(x$silt_m, 44)
  expect_equal(x$silt_h, 73)
})

test_that("unknown class codes fail with clear error", {
  expect_error(texcl_to_classlimit(c("l", "bogus")), "unknown texture class codes")
})


## texcl_to_ssc output modes ----

test_that("texcl_to_ssc default behavior unchanged", {
  x <- texcl_to_ssc(c("l", "sicl", "cl"))
  expect_equal(names(x), c("sand", "silt", "clay"))
})

test_that("texcl_to_ssc supports limits and both output modes", {
  lim <- texcl_to_ssc(c("l", "sicl", "cl"), what = "limits")
  both <- texcl_to_ssc(c("l", "sicl", "cl"), what = "both")

  expect_true(all(c("clay_l", "clay_m", "clay_h", "sand_l", "sand_m", "sand_h", "silt_l", "silt_m", "silt_h") %in% names(lim)))
  expect_true(all(c("sand", "silt", "clay", "clay_l", "clay_m", "clay_h") %in% names(both)))
  expect_equal(lim$clay_m, both$clay_m)
})


## ssc_range_to_texcl ----

test_that("range classifier returns possible classes", {
  x <- ssc_range_to_texcl(sand_l = 20, sand_h = 45, clay_l = 27, clay_h = 40)

  expect_true(x$valid_range)
  expect_true(x$n_possible >= 2)
  expect_true(grepl("cl", x$possible_texcl))
  expect_true(grepl("sicl", x$possible_texcl))
})

test_that("invalid ranges are flagged", {
  x <- ssc_range_to_texcl(sand_l = 80, sand_h = 90, clay_l = 40, clay_h = 50, silt_l = 20, silt_h = 30)
  expect_false(x$valid_range)
  expect_equal(x$n_possible, 0)
  expect_true(is.na(x$possible_texcl))
})

test_that("marginal overlap without closure feasibility is excluded", {
  x <- ssc_range_to_texcl(sand_l = 50, sand_h = 52, clay_l = 25, clay_h = 27, silt_l = 20, silt_h = 30)

  expect_true(x$valid_range)
  expect_true(grepl("scl", x$possible_texcl))
  expect_false(grepl("(^|,)l(,|$)", x$possible_texcl))
})
