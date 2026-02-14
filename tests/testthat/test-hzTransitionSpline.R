
test_that("hzTransitionSpline works as expected", {
  
  # Create a clean mock SPC for testing
  spc <- data.frame(
    id = "P1",
    top = c(0, 20, 50),
    bottom = c(20, 50, 100),
    prop = c(10, 20, 15),
    bound_distinct = c('A', 'C', 'G'),
    stringsAsFactors = FALSE
  )
  depths(spc) <- id ~ top + bottom
  
  # basic functionality
  res <- hzTransitionSpline(spc, "prop")
  expect_s4_class(res, "SoilProfileCollection")
  expect_true("prop_spline" %in% horizonNames(res))
  
  # mass preservation (quadratic)
  res1 <- hzTransitionSpline(spc, "prop", method = 'est_1cm')
  orig_mass <- sum(spc$prop * (spc$bottom - spc$top))
  spl_mass <- sum(res1$prop_spline)
  expect_equal(orig_mass, spl_mass, tolerance = 1e-5)
  
  # mass preservation (linear)
  res_lin <- hzTransitionSpline(spc, "prop", method = 'est_1cm', type = 'linear')
  spl_mass_lin <- sum(res_lin$prop_spline)
  expect_equal(orig_mass, spl_mass_lin, tolerance = 1e-5)
  
  # distinctness classes
  res_dist <- hzTransitionSpline(spc, "prop", hzbr = "bound_distinct")
  expect_equal(sum(res_dist$prop_spline), orig_mass, tolerance = 1e-5)
  
  # multi-variable
  spc$val2 <- c(1, 2, 3)
  res_multi <- hzTransitionSpline(spc, c("prop", "val2"))
  expect_true(all(c("prop_spline", "val2_spline") %in% horizonNames(res_multi)))
  
  # est_dcm
  d_intervals <- c(0, 5, 15, 30, 60, 100)
  res_dcm <- hzTransitionSpline(spc, "prop", method = "est_dcm", d = d_intervals)
  expect_equal(nrow(res_dcm), length(d_intervals) - 1)
  
  # total mass in dcm should also be preserved
  expect_equal(sum(res_dcm$prop_spline * (res_dcm$bottom - res_dcm$top)), orig_mass, tolerance = 1e-5)
})

test_that("hzTransitionSpline handles NA in hzbr", {
  # Profile with NA in bound_distinct
  spc <- data.frame(
    id = "P1",
    top = c(0, 20, 50),
    bottom = c(20, 50, 100),
    prop = c(10, 20, 15),
    bound_distinct = c('A', NA, 'G'),
    stringsAsFactors = FALSE
  )
  depths(spc) <- id ~ top + bottom
  
  res <- hzTransitionSpline(spc, "prop", hzbr = "bound_distinct")
  expect_false(any(is.na(res$prop_spline)))
  expect_equal(sum(res$prop_spline), sum(spc$prop * (spc$bottom - spc$top)), tolerance = 1e-5)
})

test_that("hzTransitionSpline handles vlow default", {
  # A case that might produce negative values if not capped
  # High contrast, thin horizon
  spc <- data.frame(
    id = "P1",
    top = c(0, 10, 12),
    bottom = c(10, 12, 100),
    prop = c(0, 50, 0),
    stringsAsFactors = FALSE
  )
  depths(spc) <- id ~ top + bottom
  
  # With vlow = 0 (default)
  res <- hzTransitionSpline(spc, "prop", type = "linear")
  expect_true(all(res$prop_spline >= 0))
  
  # With vlow = -Inf
  res_neg <- hzTransitionSpline(spc, "prop", type = "linear", vlow = -Inf)
  expect_true(any(res_neg$prop_spline < 0))
})

test_that("hzTransitionSpline handles transition probability matrix", {
  data(sp4)
  depths(sp4) <- id ~ top + bottom
  hzdesgnname(sp4) <- "name"
  sp4 <- generalizeHz(sp4, c("A", "B"), c("^A", "^B"))
  tp <- hzTransitionProbabilities(sp4, "genhz")
  res <- hzTransitionSpline(sp4, "clay", hzbr = tp)
  expect_false(any(is.na(res$clay_spline)))
})

test_that("hzTransitionSpline handles n=2 profiles", {
  spc <- data.frame(
    id = "P1",
    top = c(0, 20),
    bottom = c(20, 50),
    prop = c(10, 20),
    stringsAsFactors = FALSE
  )
  depths(spc) <- id ~ top + bottom
  res <- hzTransitionSpline(spc, "prop")
  expect_false(any(is.na(res$prop_spline)))
  expect_equal(sum(res$prop_spline), sum(spc$prop * (spc$bottom - spc$top)), tolerance = 1e-5)
})
