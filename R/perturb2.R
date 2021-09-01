# id <- do.call('c', lapply(1:10, rep, 6))
# randomdepths <- function(i) {
#   round(pmax(c(0,5,15,30,100,150,200) + runif(7, -1, 1), 0))
# }
# deps <- lapply(1:10, randomdepths)
# tdep <- do.call('c', lapply(deps, head)) #rep(c(0,5,15,30,100,150,200), max(id))
# bdep <- do.call('c', lapply(deps, tail))#rep(c(5,15,30,100,150,200), max(id))
# sds <- runif(60) + runif(60) #rep(runif(6), max(id))

#' perturb2
#' 
#' It's vectorized
#'
#' @param x a SoilProfileCollection
#' @param n number of realizations
#' @param sds vector of standard deviations, or a column name in the horizon slot of x 
#'
#' @return a SoilProfileCollection with `length(x) * n` profiles
#'
#' @examples
#' data(sp5)
#' horizons(sp5)$sdvar <-  1
#' 
#' # old perturb
#' system.time(res1 <- combine(profileApply(sp5[1:5,], function(x) {
#'   res <- aqp::perturb(x, boundary.attr = "sdvar")
#'   profile_id(res) <- paste0(profile_id(x),'_',profile_id(res))
#'   res
#' })))
#' 
#' # new perturb
#' system.time(res2 <- perturb2(sp5[1:5,]))
perturb2 <- function(x, n = 100, sds = rep(1, nrow(x))) {
 
  id <- aqp::idname(x)
  hzd <- aqp::horizonDepths(x)
  
  # prepare horizon data
  d <- horizons(x)
  d$hzID <- NULL
  d$.internalSD <- sds
  
  # duplicate rows, create profile IDs
  d <- d[rep(1:nrow(x), n),]
  d[[id]] <- paste0(d[[id]], "_", do.call('c', lapply(1:n, rep, nrow(x))))
  
  # tabulate unique standard deviations to 6 decimal places
  sdt <- table(d$.internalSD)
  pos <- match(round(d$.internalSD, 6), round(as.numeric(names(sdt)), 6))
  
  # order by increasing standard deviation
  d <- d[order(pos),]
  
  # calculate bottom offset; run rnorm once for each unique standard deviation
  d$boffset <- round(do.call('c', lapply(seq_along(sdt), function(i) {
      rnorm(n = sdt[i], mean = 0, sd = as.numeric(names(sdt)[i]))
  })))
  
  # id+topdepth sort
  d <- d[order(d[[idname(x)]], d[[hzd[1]]]),]
  
  # # new bottom depths are cumulative sum of thicknesses + offset
  # d[[hzd[2]]] <- round(cumsum((d[[hzd[2]]] - d[[hzd[1]]]) + d$boffset))
  # 
  # # keep first top depth, and all subsequent bottom depths
  # d[[hzd[1]]] <- c(d[[hzd[1]]][1], d[[hzd[2]]][1:(nrow(d)-1)])
  
  # calculate top offset from adjacent bottom
  d$toffset <- c(0, d$boffset[1:(nrow(d) - 1)])
  d[[hzd[1]]] <- d[[hzd[1]]] + d$toffset
  d[[hzd[2]]] <- d[[hzd[2]]] + d$boffset
  
  aqp::depths(d) <- c(id, hzd)      
  # stopifnot(all(aqp::checkHzDepthLogic(d)$valid))
  d
}
# 
# library(aqp)
# data(sp5)
# horizons(sp5)$sdvar <-  1
# system.time(res1 <- combine(profileApply(sp5[1:5,], function(x) {
#   res <- aqp::perturb(x, boundary.attr = "sdvar")
#   profile_id(res) <- paste0(profile_id(x),'_',profile_id(ries))
#   res
# })))
# system.time(res2 <- perturb2(sp5[1:5,]))


# bench::mark(a = length(perturb2(sp5[1:100, ])),
#             b = length(combine(profileApply(sp5[1:100, ], function(x) {
#               res <- aqp::perturb(x, boundary.attr = "sdvar")
#               profile_id(res) <- paste0(profile_id(x), '_', profile_id(res))
#               res
#             }))))
# # # A tibble: 2 × 13
# # expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result    memory time           gc              
# # <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list>    <list> <list>         <list>          
# #   1 a          621.27ms 621.27ms     1.61         NA     3.22     1     2   621.27ms <int [1]> <NULL> <bench_tm [1]> <tibble [1 × 3]>
# #   2 b             3.85s    3.85s     0.260        NA     4.68     1    18      3.85s <int [1]> <NULL> <bench_tm [1]> <tibble [1 × 3]>
# 


