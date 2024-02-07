#' Convert Texture Class to Class Limits
#'
#' @param x _character_ or _list_. A vector of texture class codes (e.g. `"l"`
#'   for loam, `"sicl"` for silty clay loam) without texture class modifiers.
#'   If the input is a list, multiple texture classes within each list element
#'   are aggregated to create combined class limits.
#' @details Logic derived from NASIS validation calculation Textural Class
#'   versus Particle Size Separates (Cathy Seybold, last updated 4/07/14).
#'
#' @return A _data.frame_ with column names `"texcl"`, `"clay_l"`, `"clay_m"`,
#'   `"clay_h"`, `"sand_l"`, `"sand_m"`, `"sand_h"`, `"silt_l"`, `"silt_m"`,
#'   `"silt_h"`.
#'
#' @export
#'
#' @examples
#' texcl_to_classlimit(c("l", "sicl", "cl"))
#' texcl_to_classlimit(list(c("l", "sicl", "cl")))
texcl_to_classlimit <- function(x) {
  lookup <- .texcl_classlimit_lookup()
  groups <- .normalize_texcl_groups(x)

  unknown <- setdiff(unique(unlist(groups, use.names = FALSE)), lookup$texcl)
  if (length(unknown) > 0) {
    stop(
      sprintf(
        "unknown texture class codes: %s",
        paste(sort(unknown), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  res <- lapply(groups, function(g) {
    sub <- lookup[match(unique(g), lookup$texcl), ]

    clay_l <- min(sub$clay_l)
    clay_h <- max(sub$clay_h)
    sand_l <- min(sub$sand_l)
    sand_h <- max(sub$sand_h)
    silt_l <- min(sub$silt_l)
    silt_h <- max(sub$silt_h)

    data.frame(
      texcl = paste(g, collapse = ","),
      clay_l = clay_l,
      clay_m = mean(c(clay_l, clay_h)),
      clay_h = clay_h,
      sand_l = sand_l,
      sand_m = mean(c(sand_l, sand_h)),
      sand_h = sand_h,
      silt_l = silt_l,
      silt_m = mean(c(silt_l, silt_h)),
      silt_h = silt_h,
      stringsAsFactors = FALSE
    )
  })

  res <- do.call(rbind, res)
  rownames(res) <- NULL
  res
}


#' Classify Sand-Silt-Clay Ranges to Possible Texture Classes
#'
#' @param sand_l,sand_h _numeric_. Lower and upper bounds of sand (%).
#' @param clay_l,clay_h _numeric_. Lower and upper bounds of clay (%).
#' @param silt_l,silt_h _numeric_. Optional lower and upper bounds of silt (%).
#'   If omitted, silt ranges are computed by closure from sand and clay ranges.
#'   If only one of \code{silt_l} or \code{silt_h} is supplied, both must be provided; partial silt bounds are not accepted.
#' @param simplify _logical_. Return only 12 simplified classes (`TRUE`) or all
#'   21 classes (`FALSE`, default).
#'
#' @return A _data.frame_ with columns `possible_texcl`, `n_possible`,
#'   `ambiguous`, and `valid_range`.
#'
#' @export
#'
#' @examples
#' ssc_range_to_texcl(sand_l = 20, sand_h = 45, clay_l = 27, clay_h = 40)
ssc_range_to_texcl <- function(
  sand_l,
  sand_h,
  clay_l,
  clay_h,
  silt_l = NULL,
  silt_h = NULL,
  simplify = FALSE
) {
  args <- list(
    sand_l = sand_l,
    sand_h = sand_h,
    clay_l = clay_l,
    clay_h = clay_h,
    silt_l = silt_l,
    silt_h = silt_h
  )
  lens <- vapply(args[!vapply(args, is.null, logical(1))], length, integer(1))
  if (!all(lens == max(lens))) {
    stop("length of inputs do not match", call. = FALSE)
  }

  n <- max(lens)
  recycle_to_n <- function(v) {
    if (is.null(v)) return(rep(NA_real_, n))
    if (length(v) == 1L && n > 1L) return(rep(v, n))
    as.numeric(v)
  }

  x <- data.frame(
    sand_l = recycle_to_n(sand_l),
    sand_h = recycle_to_n(sand_h),
    clay_l = recycle_to_n(clay_l),
    clay_h = recycle_to_n(clay_h),
    silt_l = recycle_to_n(silt_l),
    silt_h = recycle_to_n(silt_h),
    stringsAsFactors = FALSE
  )

  idx_silt_missing <- is.na(x$silt_l) & is.na(x$silt_h)
  x$silt_l[idx_silt_missing] <- 100 - x$sand_h[idx_silt_missing] - x$clay_h[idx_silt_missing]
  x$silt_h[idx_silt_missing] <- 100 - x$sand_l[idx_silt_missing] - x$clay_l[idx_silt_missing]

  lookup <- texcl_to_classlimit(as.character(SoilTextureLevels(which = "codes", simplify = FALSE)))

  if (simplify) {
    keep <- unique(as.character(SoilTextureLevels(which = "codes", simplify = TRUE)))
    lookup <- lookup[lookup$texcl %in% keep, ]
  }

  is_feasible_intersection <- function(r, c) {
    # Intersect row ranges with class limits, then require closure-feasible
    # compositions (sand + clay + silt = 100) within those intersections.
    sand_l_i <- max(r$sand_l, c$sand_l)
    sand_h_i <- min(r$sand_h, c$sand_h)
    clay_l_i <- max(r$clay_l, c$clay_l)
    clay_h_i <- min(r$clay_h, c$clay_h)
    silt_l_i <- max(r$silt_l, c$silt_l)
    silt_h_i <- min(r$silt_h, c$silt_h)

    non_empty <- sand_l_i <= sand_h_i && clay_l_i <= clay_h_i && silt_l_i <= silt_h_i
    if (!non_empty) {
      return(FALSE)
    }

    (sand_l_i + clay_l_i + silt_l_i) <= 100 &&
      (sand_h_i + clay_h_i + silt_h_i) >= 100
  }

  classify_row <- function(i) {
    r <- x[i, ]
    valid <- all(
      !is.na(unlist(r)),
      r$sand_l <= r$sand_h,
      r$clay_l <= r$clay_h,
      r$silt_l <= r$silt_h,
      r$sand_l >= 0,
      r$clay_l >= 0,
      r$silt_l >= 0,
      r$sand_h <= 100,
      r$clay_h <= 100,
      r$silt_h <= 100,
      (r$sand_l + r$clay_l + r$silt_l) <= 100,
      (r$sand_h + r$clay_h + r$silt_h) >= 100,
      !xor(is.na(r$silt_l), is.na(r$silt_h))
    )

    if (!valid) {
      return(data.frame(
        possible_texcl = NA_character_,
        n_possible = 0L,
        ambiguous = NA,
        valid_range = FALSE,
        stringsAsFactors = FALSE
      ))
    }

    idx <- vapply(seq_len(nrow(lookup)), function(j) {
      is_feasible_intersection(r, lookup[j, ])
    }, logical(1))

    classes <- lookup$texcl[idx]
    data.frame(
      possible_texcl = if (length(classes)) paste(classes, collapse = ",") else NA_character_,
      n_possible = as.integer(length(classes)),
      ambiguous = length(classes) > 1,
      valid_range = TRUE,
      stringsAsFactors = FALSE
    )
  }

  res <- do.call(rbind, lapply(seq_len(nrow(x)), classify_row))
  rownames(res) <- NULL
  res
}


.normalize_texcl_groups <- function(x) {
  if (is.null(x)) {
    stop("x cannot be NULL", call. = FALSE)
  }

  if (is.character(x) || is.factor(x)) {
    x <- as.list(as.character(x))
  } else if (!is.list(x)) {
    stop("x must be a character vector, factor, or list", call. = FALSE)
  }

  groups <- lapply(x, function(y) {
    if (length(y) == 0) {
      stop("x cannot contain empty elements", call. = FALSE)
    }
    y <- trimws(tolower(as.character(y)))
    y <- y[!is.na(y) & nzchar(y)]
    if (length(y) == 0) {
      stop("x cannot contain only NA/empty values", call. = FALSE)
    }
    y
  })

  groups
}


.texcl_classlimit_lookup <- function() {
  texcl <- as.character(SoilTextureLevels(which = "codes", simplify = FALSE))
  idx <- seq_along(texcl)

  clay_h <- ifelse(idx == 21, 100,
                   ifelse(idx == 20, 60,
                          ifelse(idx == 19, 55,
                                 ifelse(idx %in% c(17, 18), 40,
                                        ifelse(idx == 16, 35,
                                               ifelse(idx %in% c(13, 14), 27,
                                                      ifelse(idx %in% 9:12, 20,
                                                             ifelse(idx %in% 5:8, 15,
                                                                    ifelse(idx == 15, 12, 10)))))))))

  clay_l <- ifelse(idx %in% c(13, 16:21),
                   ifelse(idx == 13, 7,
                          ifelse(idx == 16, 20,
                                 ifelse(idx %in% c(17, 18), 27,
                                        ifelse(idx == 19, 35, 40)))),
                   0)

  silt_h <- ifelse(idx == 15, 100,
                   ifelse(idx == 14, 88,
                          ifelse(idx == 18, 73,
                                 ifelse(idx == 20, 60,
                                        ifelse(idx == 17, 53,
                                               ifelse(idx %in% 9:13, 50,
                                                      ifelse(idx == 21, 40,
                                                             ifelse(idx %in% 5:8, 30,
                                                                    ifelse(idx == 16, 28,
                                                                           ifelse(idx == 19, 20, 15))))))))))

  silt_l <- ifelse(idx %in% c(13, 14, 15, 17, 18, 20),
                   ifelse(idx == 17, 15,
                          ifelse(idx == 13, 28,
                                 ifelse(idx %in% c(18, 20), 40,
                                        ifelse(idx == 14, 50, 80)))),
                   0)

  sand_h <- ifelse(idx %in% 1:4, 100,
                   ifelse(idx %in% 5:8, 90,
                          ifelse(idx %in% 9:12, 85,
                                 ifelse(idx == 16, 80,
                                        ifelse(idx == 19, 65,
                                               ifelse(idx == 13, 52,
                                                      ifelse(idx == 14, 50,
                                                             ifelse(idx %in% c(17, 21), 45, 20))))))))

  sand_l <- ifelse(idx %in% c(17, 13, 9:12, 16, 19, 5:8, 1:4),
                   ifelse(idx == 17, 20,
                          ifelse(idx == 13, 23,
                                 ifelse(idx %in% 9:12, 43,
                                        ifelse(idx %in% c(16, 19), 45,
                                               ifelse(idx %in% 5:8, 70, 85))))),
                   0)

  data.frame(
    texcl = texcl,
    clay_l = clay_l,
    clay_m = (clay_l + clay_h) / 2,
    clay_h = clay_h,
    sand_l = sand_l,
    sand_m = (sand_l + sand_h) / 2,
    sand_h = sand_h,
    silt_l = silt_l,
    silt_m = (silt_l + silt_h) / 2,
    silt_h = silt_h,
    stringsAsFactors = FALSE
  )
}
