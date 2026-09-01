library(data.table)

add_sg_rollups_long <- function(rounds_long_dt) {
  dt <- as.data.table(rounds_long_dt)
  stopifnot(all(c("player_id","round_date") %in% names(dt)))
  
  # Ensure numeric
  for (cc in intersect(names(dt), c("sg_ott","sg_app","sg_arg","sg_putt","sg_total"))) {
    dt[, (cc) := suppressWarnings(as.numeric(get(cc)))]
  }
  
  # Build sg_total if missing / all NA
  if (!("sg_total" %in% names(dt)) || all(is.na(dt$sg_total))) {
    need <- intersect(names(dt), c("sg_ott","sg_app","sg_arg","sg_putt"))
    if (length(need) < 2) stop("Need sg_total or enough SG components to form sg_total.")
    dt[, sg_total := rowSums(.SD, na.rm = TRUE), .SDcols = need]
  }
  
  setorder(dt, player_id, round_date)
  
  # Rolling means
  dt[, sg12  := frollmean(sg_total, 12, align="right"), by=player_id]
  dt[, sg24  := frollmean(sg_total, 24, align="right"), by=player_id]
  dt[, sg50  := frollmean(sg_total, 50, align="right"), by=player_id]
  dt[, sg100 := frollmean(sg_total, 100, align="right"), by=player_id]
  
  # Rolling SD(24) without frollapply:
  # sd = sqrt(E[x^2] - (E[x])^2), require >=2 finite obs in window
  dt[, `:=`(
    sg24_mean  = frollmean(sg_total, 24, align="right"),
    sg24_mean2 = frollmean(sg_total^2, 24, align="right"),
    sg24_n     = frollsum(is.finite(sg_total), 24, align="right")
  ), by = player_id]
  
  dt[, sg_sd24 := sqrt(pmax(sg24_mean2 - sg24_mean^2, 0))]
  dt[sg24_n < 2, sg_sd24 := NA_real_]
  
  dt[, c("sg24_mean","sg24_mean2","sg24_n") := NULL]
  
  # Component rollups
  base_comps <- intersect(names(dt), c("sg_ott","sg_app","sg_arg","sg_putt"))
  for (v in base_comps) {
    dt[, paste0(v,"_24")  := frollmean(get(v), 24, align="right"), by=player_id]
    dt[, paste0(v,"_100") := frollmean(get(v), 100, align="right"), by=player_id]
    dt[, sg24_n  := frollsum(is.finite(sg_total), 24, align="right"), by=player_id]
    dt[, sg100_n := frollsum(is.finite(sg_total), 100, align="right"), by=player_id]
  }
  
  dt[]
}