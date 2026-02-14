#' @export
setGeneric("hzTransitionSpline", function(object,
                                         var,
                                         hzbr = NULL,
                                         hzbr_nm = NULL,
                                         d = c(0, 5, 15, 30, 60, 100, 200),
                                         method = c('est_1cm', 'est_icm', 'est_dcm'),
                                         type = c('quadratic', 'linear'),
                                         vlow = 0,
                                         vhigh = Inf, 
                                         min_thick = 2,
                                         max_thick = 15,
                                         FUN = hzTransitionSplineSolve,
                                         entropyFUN = shannonEntropy, ...)
  standardGeneric("hzTransitionSpline"))

#' @title Mass-preserving Spline with Transition Zones
#' 
#' @description 
#' This function implements a mass-preserving spline that specifically targets 
#' transition zones between horizons, while keeping the central parts of 
#' horizons at a constant value. The transition zone thickness can be 
#' parameterized using distinctness classes or specified directly.
#' 
#' @param object A SoilProfileCollection
#' 
#' @param var The name of the horizon-level variable(s) to spline.
#' 
#' @param hzbr Column name containing horizon boundary 
#' distinctness codes (e.g., 'V', 'A', 'C', 'G', 'D') or numeric thickness in cm. 
#' Alternatively, a transition probability (TP) matrix (e.g. from `hzTransitionProbabilities()`).
#' Default `NULL` uses a default thickness of 1cm for all boundaries.
#' 
#' @param hzbr_nm Column name used for lookup when `hzbr` is a transition 
#' probability matrix. Defaults to `GHL(object)`.
#' 
#' @param d Output depth intervals (cm). Default `c(0, 5, 15, 30, 60, 100, 200)`.
#' 
#' @param method Return method:
#' 
#'   * 'est_1cm' (default) returns 1cm slices
#'   * 'est_icm' returns original input horizon boundaries with splined values
#'   * 'est_dcm' returns averages over specified depth intervals `d`
#' 
#' @param type Type of transition:
#' 
#'   * 'quadratic' (smooth, \eqn{ C^1 } continuous at boundaries and horizon centers)
#'   * 'linear' (linear gradient in transition zone)
#' 
#' @param vlow Minimum allowable value. Default: `0`.
#' 
#' @param vhigh Maximum allowable value. Default: `Inf`.
#' 
#' @param min_thick Minimum transition thickness (cm) when using a TP matrix. Default: `2`.
#' 
#' @param max_thick Maximum transition thickness (cm) when using a TP matrix. Default: `15`.
#' 
#' @param FUN A solver function. Default: `hzTransitionSplineSolve`.
#' 
#' @param entropyFUN An entropy function for TP matrix mapping. Default: `shannonEntropy`.
#' 
#' @param ... Additional arguments (not used)
#' 
#' @return A SoilProfileCollection
#' 
#' @details 
#' The tridiagonal system is solved to find constant values \eqn{V_i} for each 
#' horizon `i` such that the total mass is preserved, accounting for the 
#' transitions between horizons. 
#' 
#' For 'quadratic' transitions, the function is constant in the middle of 
#' the horizon and joins the neighbor horizon with a quadratic function 
#' in the transition zone of half-width delta.
#' 
#' For 'linear' transitions, the function is constant in the middle and 
#' has a linear gradient in the transition zone \eqn{[z_b - \delta, z_b + \delta]}.
#' 
#' When `hzbr` is a transition probability matrix, transition widths are assigned 
#' based on the probability of transition between adjacent horizons and the 
#' Shannon Entropy of the source horizon. Shorter (sharper) widths are assigned 
#' to high-probability transitions from low-entropy states. Larger (more diffuse) 
#' widths are assigned to low-probability transitions or transitions from 
#' high-entropy states (indicating confusion).
#' 
#' Width \eqn{2 \cdot \delta} is calculated as:
#' \eqn{w = min\_thick + (max\_thick - min\_thick) \cdot (1 - P_{ij} \cdot (1 - H_{norm}))}
#' where \eqn{P_{ij}} is the transition probability and \eqn{H_{norm}} is the 
#' normalized Shannon Entropy of the source horizon's transitions.
#' 
#' @author Andrew G. Brown
#' 
#' @export
#' @aliases hzTransitionSpline
#' @rdname hzTransitionSpline
#' @seealso [hzTransitionProbabilities()] [shannonEntropy()]
#' @examples
#' data(sp1)
#' depths(sp1) <- id ~ top + bottom
#' 
#' # basic usage
#' res <- hzTransitionSpline(sp1, "prop")
#' 
#' # plot result
#' plotSPC(
#'   res[1:5, ],
#'   color = "prop_spline",
#'   col.palette = rev(hcl.colors(10)),
#'   divide.hz = FALSE
#' )
#' 
#' # use distinctness classes (abrupt, clear, gradual) in horizons(sp1)$bound_distinct
#' res2 <- hzTransitionSpline(sp1, "prop", hzbr = "bound_distinct")
#' plotSPC(
#'   res2[1:5, ],
#'   color = "prop_spline",
#'   col.palette = rev(hcl.colors(10)),
#'   divide.hz = FALSE
#' )
#' 
#' # linear transitions
#' res3 <- hzTransitionSpline(sp1, "prop", type = "linear")
#' plotSPC(
#'   res3[1:5, ],
#'   color = "prop_spline",
#'   col.palette = rev(hcl.colors(10)),
#'   divide.hz = FALSE
#' )
#' 
#' # use transition probability matrix
#' data(sp4)
#' depths(sp4) <- id ~ top + bottom
#' hzdesgnname(sp4) <- "name"
#' sp4 <- generalizeHz(sp4, pattern = c("^A","^B"), new = c("A", "Bt"))
#' tp <- hzTransitionProbabilities(sp4, "genhz")
#' res4 <- hzTransitionSpline(sp4, "clay", hzbr = tp, hzbr_nm = "genhz")
#' plotSPC(res4, color = "clay_spline", divide.hz = FALSE)
#'
setMethod("hzTransitionSpline", signature(object = "SoilProfileCollection"),
          function(object, 
                   var, 
                   hzbr = NULL, 
                   hzbr_nm = NULL,
                   d = c(0, 5, 15, 30, 60, 100, 200), 
                   method = c('est_1cm', 'est_icm', 'est_dcm'),
                   type = c('quadratic', 'linear'),
                   vlow = 0, 
                   vhigh = Inf, 
                   min_thick = 2,
                   max_thick = 15,
                   FUN = hzTransitionSplineSolve,
                   entropyFUN = shannonEntropy, ...) {
  
  .id <- .hzID <- .top <- .bot <- .delta <- .dt <- .db <- .L <- .scale <- .V <- .v_spline <- .d_idx <- .SD <- .I <- .N <- row_entropy_norm <- NULL
  
  method <- match.arg(method)
  type <- match.arg(type)
  
  if (is.null(var) || !all(var %in% horizonNames(object)))
    stop("all `var` must specify horizon-level variables", call. = FALSE)
  
  idn_orig <- idname(object)
  hzidn_orig <- hzidname(object)
  td_orig <- horizonDepths(object)
  
  res_spc <- NULL
  h_all_dt <- data.table::as.data.table(horizons(object))
  pids <- profile_id(object)
  
  is_tp_matrix <- is.matrix(hzbr)
  if (is_tp_matrix) {
    if (is.null(hzbr_nm)) hzbr_nm <- GHL(object, required = TRUE)
    # Calculate row entropy for the TP matrix (normalized 0-1 if shannonEntropy used)
    if (identical(entropyFUN, shannonEntropy)) {
      row_entropy_norm <- apply(hzbr, 1, entropyFUN, b = ncol(hzbr))
    } else {
      row_entropy_norm <- apply(hzbr, 1, entropyFUN)
    }
  }
  
  for (v in var) {
    cols_to_keep <- c(idn_orig, hzidn_orig, td_orig, v, if (!is_tp_matrix) hzbr else hzbr_nm)
    h_v <- h_all_dt[!is.na(get(v)), cols_to_keep, with = FALSE]
    if (nrow(h_v) == 0) next
    
    data.table::setnames(h_v, c(idn_orig, hzidn_orig, td_orig[1], td_orig[2]), c(".id", ".hzID", ".top", ".bot"))
    
    if (is.null(hzbr)) {
      h_v[[".delta"]] <- 1
    } else if (is_tp_matrix) {
      h_v[[".delta"]] <- h_v[, {
        .ghl <- .SD[[hzbr_nm]]
        .next_ghl <- data.table::shift(.ghl, type = "lead")
        
        # Vectorized lookup of Transition Probabilities
        .tp_val <- rep(0, length(.ghl))
        # Valid transitions: both current and next GHL exist and are in matrix
        valid_idx <- which(!is.na(.ghl) & !is.na(.next_ghl) & 
                             .ghl %in% rownames(hzbr) & .next_ghl %in% colnames(hzbr))
        
        if (length(valid_idx) > 0) {
          # Create index matrix for direct lookup
          idx_mat <- cbind(.ghl[valid_idx], .next_ghl[valid_idx])
          .tp_val[valid_idx] <- hzbr[idx_mat]
        }
        
        # Vectorized lookup of Normalized Entropy
        .h_norm <- rep(0, length(.ghl))
        valid_h_idx <- which(!is.na(.ghl) & .ghl %in% names(row_entropy_norm))
        if (length(valid_h_idx) > 0) {
          .h_norm[valid_h_idx] <- row_entropy_norm[.ghl[valid_h_idx]]
        }
        
        # Apply formula to all rows (last row naturally handled as valid_idx excludes it via NA next_ghl)
        list((min_thick + (max_thick - min_thick) * (1 - .tp_val * (1 - .h_norm))) / 2)
      }, by = .id]$V1
    } else {
      .val <- h_v[[hzbr]]
      h_v[[".delta"]] <- if (is.numeric(.val)) .val / 2 else hzDistinctnessCodeToOffset(.val)
      h_v[is.na(.delta) | is.na(.val), ".delta"] <- 1
    }    
    
    h_v[[".db"]] <- h_v[[".delta"]]
    h_v[[".dt"]] <- h_v[, data.table::shift(.delta, fill = 0), by = .id]$V1
    h_v[h_v[, .I[.N], by = .id]$V1, ".db"] <- 0
    
    h_v[[".L"]] <- h_v[[".bot"]] - h_v[[".top"]]
    h_v[[".scale"]] <- pmin(1, (0.9 * h_v[[".L"]]) / pmax(h_v[[".dt"]] + h_v[[".db"]], 1e-6))
    h_v[[".dt"]] <- h_v[[".dt"]] * h_v[[".scale"]]
    h_v[[".db"]] <- h_v[[".db"]] * h_v[[".scale"]]
    
    k_factor <- if (type == 'quadratic') 1/3 else 1/4
    v_col <- v
    
    param_dt <- h_v[, {
      solve_res <- FUN(.L, .dt, .db, .SD[[v_col]], k_factor, vlow, vhigh)
      list(
        .hzID = .hzID,
        .V = solve_res$V,
        .Y_top = solve_res$Y_all[1:(length(solve_res$Y_all) - 1)],
        .Y_bot = solve_res$Y_all[2:length(solve_res$Y_all)],
        .dt = .dt,
        .db = .db,
        .hz_top = .top,
        .hz_bot = .bot
      )
    }, by = .id, .SDcols = c(v_col)]
    
    if (method %in% c('est_1cm', 'est_dcm')) {
      pids_suc <- unique(param_dt[[".id"]])
      
      # Explode into 1cm slices
      h_dice <- param_dt[, {
        tops <- seq(.hz_top, .hz_bot - 1)
        list(
          top = tops,
          bottom = tops + 1,
          .V = .V,
          .Y_top = .Y_top,
          .Y_bot = .Y_bot,
          .dt = .dt,
          .db = .db,
          .hz_top = .hz_top,
          .hz_bot = .hz_bot
        )
      }, by = list(.id, .hzID)]
      
      rel_z <- h_dice[["top"]] + 0.5
      is_top_trans <- !is.na(h_dice$.V) & rel_z < (h_dice$.hz_top + h_dice$.dt)
      is_bot_trans <- !is.na(h_dice$.V) & rel_z > (h_dice$.hz_bot - h_dice$.db)
      
      if (type == 'quadratic') {
        B_i <- ifelse(h_dice$.dt > 0, (h_dice$.Y_top - h_dice$.V) / pmax(h_dice$.dt^2, 1e-6), 0)
        A_i <- ifelse(h_dice$.db > 0, (h_dice$.Y_bot - h_dice$.V) / pmax(h_dice$.db^2, 1e-6), 0)
        val_vec <- data.table::fcase(is.na(h_dice$.V), NA_real_,
                                     is_top_trans, h_dice$.V + B_i * (rel_z - (h_dice$.hz_top + h_dice$.dt))^2,
                                     is_bot_trans, h_dice$.V + A_i * (rel_z - (h_dice$.hz_bot - h_dice$.db))^2,
                                     default = h_dice$.V)
      } else {
        val_vec <- data.table::fcase(is.na(h_dice$.V), NA_real_,
                                     is_top_trans, h_dice$.V + (h_dice$.Y_top - h_dice$.V) * (1 - (rel_z - h_dice$.hz_top) / pmax(h_dice$.dt, 1e-6)),
                                     is_bot_trans, h_dice$.V + (h_dice$.Y_bot - h_dice$.V) * ((rel_z - (h_dice$.hz_bot - h_dice$.db)) / pmax(h_dice$.db, 1e-6)),
                                     default = h_dice$.V)
      }
      val_vec <- pmin(pmax(val_vec, vlow), vhigh)
      
      if (method == 'est_1cm') {
        # Construct sliced SPC manually
        spc.spl <- object[match(pids_suc, pids), ]
        h_sliced <- h_dice[, list(.id, .hzID, top, bottom)]
        data.table::setnames(h_sliced, c(".id", ".hzID", "top", "bottom"), c(idn_orig, hzidn_orig, td_orig[1], td_orig[2]))
        h_sliced[[paste0(v, "_spline")]] <- val_vec
        replaceHorizons(spc.spl) <- as.data.frame(h_sliced)
      } else {
        h_dice[[".v_spline"]] <- val_vec
        h_dice[[".d_idx"]] <- findInterval(h_dice[["top"]], d, all.inside = TRUE)
        agg_dt <- h_dice[, list(val = mean(.v_spline, na.rm = TRUE)), by = list(.id, .d_idx)]
        
        spc.res_dcm <- data.frame(id = rep(pids_suc, each = length(d) - 1),
                                  top = rep(d[-length(d)], length(pids_suc)),
                                  bottom = rep(d[-1], length(pids_suc)))
        names(spc.res_dcm) <- c(idn_orig, td_orig)
        depths(spc.res_dcm) <- names(spc.res_dcm)
        
        final_agg <- merge(data.table::data.table(.id = rep(pids_suc, each = length(d) - 1),
                                                  .d_idx = rep(seq_along(d[-1]), length(pids_suc))),
                           agg_dt, by = c(".id", ".d_idx"), all.x = TRUE)
        spc.res_dcm[[paste0(v, "_spline")]] <- final_agg[order(match(.id, pids_suc), .d_idx)]$val
        spc.spl <- spc.res_dcm
      }
      res_spc <- if (is.null(res_spc)) spc.spl else { res_spc[[paste0(v, "_spline")]] <- spc.spl[[paste0(v, "_spline")]]; res_spc }
    } else {
      val_vec <- param_dt$.V[match(h_all_dt[[hzidn_orig]], param_dt$.hzID)]
      res_spc <- if (is.null(res_spc)) object else res_spc
      res_spc[[paste0(v, "_spline")]] <- val_vec
    }
  }
  res_spc
})

#' @title Mass-preserving Spline Solver
#' @description Internal solver for mass-preserving spline with transition zones.
#' @param L Horizon thicknesses (cm)
#' @param dt Top transition half-widths (cm)
#' @param db Bottom transition half-widths (cm)
#' @param vals Horizon values
#' @param k factor (1/3 for quadratic, 1/4 for linear)
#' @param vlow Minimum allowable value
#' @param vhigh Maximum allowable value
#' @return A list containing `V` (constant values for each horizon) and `Y_all` (values at each horizon boundary)
#' @export
hzTransitionSplineSolve <- function(L, dt, db, vals, k, vlow, vhigh) {
  n <- length(L)
  if (n == 1) return(list(V = vals, Y_all = c(vals, vals)))
  rhs <- vals * L
  a <- rep(0, n); b_diag <- c_diag <- rep(0, n-1)
  for (i in 1:n) {
    dt_i <- dt[i]; db_i <- db[i]
    db_prev <- if (i > 0) db[i-1] else 0
    dt_next <- if (i < n) dt[i+1] else 0
    denom_t <- db_prev + dt_i
    denom_b <- db_i + dt_next
    w_t <- if (i > 1 && denom_t > 0) dt_i / denom_t else 0
    w_b <- if (i < n && denom_b > 0) db_i / denom_b else 0
    a[i] <- (L[i] - k*dt_i - k*db_i) + k*dt_i*w_t + k*db_i*w_b
    if (i > 1) b_diag[i-1] <- if (denom_t > 0) k * dt_i * db_prev / denom_t else 0
    if (i < n) c_diag[i] <- if (denom_b > 0) k * db_i * dt_next / denom_b else 0
  }
  cp <- rep(0, n-1); dp <- rep(0, n)
  cp[1] <- c_diag[1] / a[1]; dp[1] <- rhs[1] / a[1]
  if (n > 2) {
    for (i in 2:(n-1)) {
      m <- a[i] - b_diag[i-1] * cp[i-1]
      cp[i] <- c_diag[i] / m
      dp[i] <- (rhs[i] - b_diag[i-1] * dp[i-1]) / m
    }
  }
  m_n <- a[n] - b_diag[n-1] * cp[n-1]
  dp[n] <- (rhs[n] - b_diag[n-1] * dp[n-1]) / m_n
  V <- rep(0, n); V[n] <- dp[n]
  for (i in (n-1):1) V[i] <- dp[i] - cp[i] * V[i+1]
  V <- pmin(pmax(V, vlow), vhigh)
  denom <- db[-n] + dt[-1]
  Y <- ifelse(denom > 0, (V[-n] * db[-n] + V[-1] * dt[-1]) / denom, (V[-n] + V[-1]) / 2)
  return(list(V = V, Y_all = c(V[1], Y, V[n])))
}
