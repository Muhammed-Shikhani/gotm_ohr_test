wide2long_vec <- function(vmat, zmat) {
  data.frame(
    date = rep(vmat[,1], each = ncol(vmat) - 1),
    depth = as.numeric(t(as.matrix(zmat[,-1]))),
    value = as.numeric(t(as.matrix(vmat[,-1])))
  )
}



vertmean_vec <- function(VAR_MAT, Z, init_level = 525, upr = 11, lwr = 16,
                         bathy_elevation, bathy_area,h, wl) {
  
  dt <- VAR_MAT[, 1]
  vmat <- as.matrix(VAR_MAT[, -1, drop = FALSE])        
  Zm   <- as.matrix(Z[, -1, drop = FALSE])
  Zref <- Z[, 2]                                        
  
  dpths_mat <- sweep(Zm, 1, (wl-(h*0.5)), FUN = "-")
  
  bathy_depth <- bathy_elevation - init_level
  
  f_area <- approxfun(bathy_depth, bathy_area, rule = 2)  # rule=2 to clamp ends
  vols_mat <- f_area(dpths_mat)*h
  
  
  sub_top <- (dpths_mat >=  -upr)
  sub_mid <- (dpths_mat < -upr) & (dpths_mat >  -lwr)
  sub_bot <- (dpths_mat <= -lwr)
  
  w_top <- vols_mat * sub_top
  w_mid <- vols_mat * sub_mid
  w_bot <- vols_mat * sub_bot
  
  num_top <- rowSums(vmat * w_top, na.rm = TRUE)
  den_top <- rowSums(w_top,       na.rm = TRUE)
  num_mid <- rowSums(vmat * w_mid, na.rm = TRUE)
  den_mid <- rowSums(w_mid,        na.rm = TRUE)
  num_bot <- rowSums(vmat * w_bot, na.rm = TRUE)
  den_bot <- rowSums(w_bot,        na.rm = TRUE)
  
  safe_div <- function(num, den) { out <- num / den; out[den == 0] <- NA_real_; out }
  
  out_top <- safe_div(num_top, den_top)
  out_mid <- safe_div(num_mid, den_mid)
  out_bot <- safe_div(num_bot, den_bot)
  
  data.frame(date = dt, top = out_top, mid = out_mid, bot = out_bot, row.names = NULL)
}





vertmean_vec_2layers <- function(VAR_MAT, Z, init_level = 525, threshold = 20,
                                 bathy_elevation, bathy_area,h, wl) {
  
  dt <- VAR_MAT[, 1]
  vmat <- as.matrix(VAR_MAT[, -1, drop = FALSE])        
  Zm   <- as.matrix(Z[, -1, drop = FALSE])
  Zref <- Z[, 2]                                        
  
  dpths_mat <- sweep(Zm, 1, (wl-(h*0.5)), FUN = "-")
  
  bathy_depth <- bathy_elevation - init_level
  
  f_area <- approxfun(bathy_depth, bathy_area, rule = 2)  # rule=2 to clamp ends
  vols_mat <- f_area(dpths_mat)*h
  
  
  sub_top <- (dpths_mat >=  -threshold)
  sub_bot <- (dpths_mat < -threshold)
  
  w_top <- vols_mat * sub_top
  w_bot <- vols_mat * sub_bot
  
  num_top <- rowSums(vmat * w_top, na.rm = TRUE)
  den_top <- rowSums(w_top,       na.rm = TRUE)
  num_bot <- rowSums(vmat * w_bot, na.rm = TRUE)
  den_bot <- rowSums(w_bot,        na.rm = TRUE)
  
  safe_div <- function(num, den) { out <- num / den; out[den == 0] <- NA_real_; out }
  
  out_top <- safe_div(num_top, den_top)
  out_bot <- safe_div(num_bot, den_bot)
  
  data.frame(date = dt, top = out_top,  bot = out_bot, row.names = NULL)
}


vertmean_vec_wholelake <- function(VAR_MAT, Z, init_level = 525,
                                   bathy_elevation, bathy_area, h, wl) {
  
  dt   <- VAR_MAT[, 1]
  vmat <- as.matrix(VAR_MAT[, -1, drop = FALSE])
  Zm   <- as.matrix(Z[, -1, drop = FALSE])
  
  # Depth of each layer midpoint relative to bathymetry reference
  dpths_mat <- sweep(Zm, 1, (wl - (h * 0.5)), FUN = "-")
  
  # Bathymetry: elevation -> depth, area -> volume per cell
  bathy_depth <- bathy_elevation - init_level
  f_area      <- approxfun(bathy_depth, bathy_area, rule = 2)
  vols_mat <- matrix(f_area(dpths_mat) * h, nrow = nrow(dpths_mat), ncol = ncol(dpths_mat))  
  # Volume-weighted mean across all layers
  num <- rowSums(vmat * vols_mat, na.rm = TRUE)
  den <- rowSums(vols_mat,        na.rm = TRUE)
  
  out <- num / den
  out[den == 0] <- NA_real_
  
  data.frame(date = dt, wholelake = out, row.names = NULL)
}



compute_layer_means <- function(the_var) {
  var_mat <- get_vari(ncdf = out_file, var = the_var)
  var_mat <- var_mat[year(var_mat[, 1]) %in% the_years, ]
  
  va_mod_w <- vertmean_vec(
    VAR_MAT = var_mat, Z = z, init_level = 525,
    upr = 11, lwr = 16,
    bathy_elevation = bathy$elevation,
    bathy_area = bathy$area,
    wl = zeta$var1,
    h = h[, 2]
  )
  
  v_mod <- va_mod_w %>%
    tidyr::pivot_longer(cols = c(top, mid, bot), names_to = "layer", values_to = "mean_value")
  v_mod$data_type <- "mod"
  v_mod$layer <- factor(v_mod$layer, levels = c("top","mid","bot"))
  levels(v_mod$layer) <- c("0-11 m", "11-16 m", "16 m to bottom")
  v_mod
}



compute_layer_means_0_20 <- function(the_var) {
  var_mat <- get_vari(ncdf = out_file, var = the_var)
  var_mat <- var_mat[year(var_mat[, 1]) %in% the_years, ]
  
  va_mod_w <- vertmean_vec(
    VAR_MAT = var_mat, Z = z, init_level = 525,
    upr = 11, lwr = 16,
    bathy_elevation = bathy$elevation,
    bathy_area = bathy$area,
    wl = zeta$var1,
    h = h[, 2]
  )
  
  v_mod <- va_mod_w %>%
    tidyr::pivot_longer(cols = c(top, mid, bot), names_to = "layer", values_to = "mean_value")
  v_mod$data_type <- "mod"
  v_mod$layer <- factor(v_mod$layer, levels = c("top","mid","bot"))
  levels(v_mod$layer) <- c("0-11 m", "11-16 m", "16 m to bottom")
  v_mod
}