setGeneric("glom", function(p, z1, z2 = NA,
                            ids = FALSE, df = FALSE,
                            truncate = FALSE, invert = FALSE,
                            modality = "all")
  standardGeneric("glom"))

#' Subset soil horizon data using a depth or depth interval
#' 
#' @param p A single-profile SoilProfileCollection; usually \code{glom} is called via \code{profileApply()} e.g. via convenience method \code{glomApply}
#' @param z1 Top depth (required) - depth to intersect horizon; if 'z2' specified, top depth of intersect interval.
#' @param z2 OPTIONAL: Bottom depth - bottom depth of intersection interval
#' @param ids Return just horizon IDs in interval? default: FALSE
#' @param df Return a data.frame, by intersection with \code{horizons(p)}? default: FALSE
#' @param truncate Truncate horizon top and bottom depths to \code{z1} and \code{z2}? default: FALSE
#' @param invert Get the horizons/depth ranges of the profile outside the interval z1/z2? default: FALSE
#' @param modality Return all data (default: \code{"all"}) or first, thickest (\code{modality = "thickest"}) horizon in interval. This can be a way of flattening a many:1 relationship over a depth interval applied to a set of profiles.)
#'
#' @description \code{glom()} returns a "clod" of horizons from a single profile SoilProfileCollection that have depth (range) in common. 
#'
#' All horizons included within the specified interval are returned in their entirety (not just the portion within the interval), unless the \code{truncate} argument is specified. Horizon intersection is based on unique ID \code{hzidname(spc)} and attribute of interest.
#'
#' If intersection at the specified boundaries \code{['z1', 'z2']} results in no horizon data, 'NULL' is returned with a warning containing the offending pedon ID.
#' 
#' If inverting results with \code{invert}, it is possible that thick horizons (that span more than the entire glom interval) will be split into two horizons. This may make the results from \code{ids = TRUE} different from what you expect, as they will be based on a profile with an "extra" horizon.
#'
#' If the upper or lower bound is less than or greater than the shallowest top depth or deepest bottom depth, respectively, a warning is issued, but the horizons within the interval are returned as usual. Users can handle the possibility of incomplete results using \code{evalMissingData} or similar approach. While these warnings can make for messy standard output, it is felt by the author that users need to consciously "choose" to ignore (e.g. via \code{suppressWarnings}) this output. Most commonly these warning occurs when calling \code{glom} on a SPC that is impractical to inspect via \code{glomApply}, so it may be the only warning of a potential problem in a downstream analysis.
#'
#' @details The verb/function that creates a clod is "glom". "To glom" is "to steal" or to "become stuck or attached to". The word is related to the compound "glomalin", which is a glycoprotein produced by mycorrhizal fungi in soil.
#' 
#' @seealso \code{\link{glomApply}} \code{\link{trunc}}
#' 
#' @author Andrew G. Brown
#'
#' @return A SoilProfileCollection, data.frame, or a vector of horizon IDs. \code{NULL} if no result.
#'
#' @export glom
#' @aliases glom
#'
#' @examples
#' data(sp1, package = 'aqp')
#' depths(sp1) <- id ~ top + bottom
#' site(sp1) <- ~ group
#'
#' p <- sp1[1]
#'
#' foo <- glom(p, 25, 100)
#'
#' # there are 4 horizons in the clod glommed from depths 25 to 100 on profile 1 in sp1
#' nrow(foo)
setMethod(f = 'glom', signature(p = 'SoilProfileCollection'),
          function(p, z1, z2 = NA,
                 ids = FALSE, df = FALSE,
                 truncate = FALSE, invert = FALSE,
                 modality = "all") {
            
  if (!inherits(p, 'SoilProfileCollection') | length(p) > 1) { 
    stop("`p` must be a SoilProfileCollection containing one profile", call.=FALSE)
  }

  depthn <- horizonDepths(p)
  
  if (!invert) {
    idx <- .glom(p, z1, z2, modality)
  } else {
    idx <- c(.glom(p, min(p[[depthn[1]]], na.rm = TRUE), z1, modality),
             .glom(p, z2, max(p[[depthn[2]]], na.rm = TRUE), modality))
  }

  # truncate ragged edges to PSCS
  # if we have an interval [z1, z2], option to truncate to it
  if (!invert  & truncate & !is.null(z2)) {
    .top <- p[[depthn[1]]]
    .bottom <- p[[depthn[2]]]

    .top[.top < z1] <- z1
    .bottom[.bottom < z1] <- z1

    .top[.top > z2] <- z2
    .bottom[.bottom > z2] <- z2

    p[[depthn[1]]] <- .top
    p[[depthn[2]]] <- .bottom

  } else if (invert & truncate & !is.null(z2)) {
    .top <- p[[depthn[1]]]
    .bottom <- p[[depthn[2]]]

    # special case: single horizon spans whole invert interval
    #               and is SPLIT by invert = TRUE
    if (all(c(z1,z2) < max(.bottom, na.rm = TRUE)) & 
        all(c(z1,z2) > min(.top, na.rm = TRUE))) {
      
      # make two profiles from one profile (upper and lower)
      p1 <- suppressWarnings(glom(p, 0, z1, truncate = TRUE))
      p2 <- suppressWarnings(glom(p, z2, max(p[[depthn[2]]]) + 1, truncate = TRUE))
      
      # combine horizon data
      newhz <- rbind(horizons(p1), horizons(p2))
      
      # replace horizon data
      newhz$hzID <- as.character(1:nrow(newhz))
      replaceHorizons(p) <- newhz
      hzidname(p) <- "hzID"
      
      # update index to reflect new horizons and modality
      if (modality == "all") {
        
        # all horizons
        idx <- newhz$hzID 
        
      } else if (modality == "thickest") {
        # take thickest horizon, shallowest
        thk <- newhz[[depthn[2]]] - newhz[[depthn[1]]]
        idx <- newhz$hzID[which(thk == max(thk)[1])]
      }
      
      # this should not happen -- this is a single profile SPC!
      if (!spc_in_sync(p)$valid)
        warning(sprintf("inverting glom inteval for profile %s failed; be sure to check SPC validity", profile_id(p)), call. = FALSE)

    } else {
      .top[.top > z1 & .top < z2] <- z2
      .bottom[.bottom > z1 & .bottom < z2] <- z1
      p[[depthn[1]]] <- .top
      p[[depthn[2]]] <- .bottom
    }
  }
  # short circuit to get hzIDs of result
  if (ids & !invert) {
    hids <- hzID(p)
    return(hids[match(idx, hzID(p))])
  } else if (ids & invert) {
    warning("invert = TRUE option may be unstable with ids = TRUE due to (possible) splitting of horizons!", call. = FALSE)
  }
  
  if (!all(is.na(idx))) {
    if (!df) {
      return(p[, which(hzID(p) %in% idx)])
    } else {
      return(horizons(p)[hzID(p) %in% idx,])
    }
  } else {
    return(NA)
  }
})

.glom <- function (p, z1, z2 = NA, modality = "all", as.list = FALSE) {
  # access SPC slots to get important info about p
  hz <- horizons(p)
  dz <- horizonDepths(p)
  id <- idname(p)
  pid <- profile_id(p)
  hzid <- hzidname(p)

  # get top and bottom horizon depth vectors
  tdep <- hz[[dz[1]]]
  bdep <- hz[[dz[2]]]

  # short circuit test of hz logic
  depthlogic <- hzDepthTests(tdep, bdep)
  logic_tests <- c('depthLogic','missingDepth','overlapOrGap')
  logic_fail <- as.logical(depthlogic[logic_tests])

  if(any(logic_fail)) {
    warning(paste0('Horizon logic error(s) ',
                   paste0(logic_tests[logic_fail], collapse=","),
                   ' found. (',id,': ',
                   pid,")"), call.=FALSE)
  }

  # determine top depths greater/equal to z1
  gt1 <- tdep >= z1

  # include horizons whose bottom portion are below z1
  gt1 <- gt1 | (bdep > z1)

  # get the index of the first horizon
  idx.top <- which(gt1)

  if (!length(idx.top)) {
    warning(paste0('Invalid upper bound `z1` (',z1,'), returning `NULL` (',
                   id,': ', pid,')'), call.=FALSE)
    return(NULL)
  }

  # always returns the top horizon
  idx.top <- idx.top[1]

  if(z1 < tdep[idx.top]) {
    warning(paste0('Upper boundary `z1` (',z1,') shallower than top depth (',
                   tdep[idx.top],') of shallowest horizon in subset. (',
                   id,': ', pid, ')'), call.=FALSE)
  }

  # if a bottom depth of the clod interval is specified
  if (!is.na(z2)) {

    # determine bottom depths less than z2
    lt2 <- bdep < z2

    # include bottom depths equal to z2
    lt2[which(bdep == z2)] <- TRUE

    # include horizons whose top portion are above z2
    lt2 <- lt2 | (tdep < z2)

    # get index of last horizon
    idx.bot <- rev(which(lt2))

    if (!length(idx.bot)) {
      warning('Invalid bounds (',z1,"-",z2,') returning `NULL` (',
              id,':', pid,')', call.=FALSE)
      return(NULL)
    }

    idx.bot <- idx.bot[1]

    if(z2 > bdep[idx.bot]) {
      warning(paste0('Lower boundary `z2` (',z2,
                     ') is deeper than bottom depth of deepest horizon (',
                     bdep[idx.bot],') in subset. (',id,': ',
                     pid,")"), call.=FALSE)
    }

    # not really sure how this could happen ...
    # maybe with wrong depth units for z?
    if(!(all(idx.top:idx.bot %in% 1:nrow(p)))) {
      warning('Invalid bounds (',z1,"-",z2,') returning `NULL` (',
              id,':', pid,')', call.=FALSE)
      return(NULL)
    }

    # warn if incomplete result
    target.thickness <- z2 - z1
    actual.thickness <- sum(bdep[idx.top:idx.bot] - tdep[idx.top:idx.bot])

    if(actual.thickness < target.thickness) {
      warning(paste0('Missing data in glom interval (actual/target: ',
                     actual.thickness,'/',target.thickness,' ',
                     depth_units(p),' (',id,': ', pid, ')'), call.=FALSE)

      if(z1 < tdep[idx.top]) {
        warning(paste0('glom boundary `z1` (',z1,') shallower than top depth (',tdep[idx.top],
                       ') of shallowest horizon. (',id,': ', pid, ')'), call.=FALSE)
      }

      if(z2 > bdep[idx.bot]) {
        warning(paste0('`z2` (',z2,') deeper than bottom depth of deepest horizon (', bdep[idx.bot],
                       '). (',id,': ', pid,")"), call.=FALSE)
      }
    }

    if(modality == "thickest") {
      sub.thk <- bdep[idx.top:idx.bot] - tdep[idx.top:idx.bot]
      max.sub.thk <- max(sub.thk)
      first.thickest.idx <- which(sub.thk == max.sub.thk)[1]
      idx.top <- idx.bot <- idx.top - 1 + first.thickest.idx
    }

    # get the ID values out of horizon table
    idval <- .data.frame.j(hz[idx.top:idx.bot,], hzid,
                           aqp_df_class(p))[[hzidname(p)]]

    # just the horizon IDs
    if (!as.list)
      return(idval)

    # list result.
    return(list(hzid = hzid, hz.idx = idx.top:idx.bot, value = idval))
  }

  if(modality == "thickest") {
    first.thickest.idx <- which(bdep - tdep == max(bdep - tdep))[1]
    idx.top <- first.thickest.idx
  }

  idval <- .data.frame.j(hz[idx.top,], hzid, aqp_df_class(p))[[hzidname(p)]]

  if (!as.list) {
    return(idval)
  }

  return(list(hz.idx = idx.top, value = idval))
}
