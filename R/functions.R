library(tidyverse)
#' Creates a dataset for plotting OYP and EY plots
#'
#' This function creates a dataframe that can be used by plot_profile() and plot_ey()
#'
#' @param post_dat An mcmc object with nodes lnalpha, beta, phi, and sigma.
#' @param multiplier The Shiny app uses a multiplier to scale beta. Input that here. Defaults to 1.
#' @param OYP_pct Either 70 or 80 corresponding to a 70% or 80% OYP, respectively. Defaults to NA. The 90% OYP is included regardless.
#'
#' @return A data.frame
#'
#' @examples
#' get_profile(post, 1e-5)
#'
#' @export

get_profile <- function(post_dat, multiplier = 1, OYP_pct = NA){
  if (!is.na(OYP_pct) & !(OYP_pct %in% c(70, 80))) {
    stop("Error: 'OYP_pct' must be either 70 or 80 coresponding to a 70% or 80% OYP. The 90% OYP is included by default")
  }

  samples <- dim(post_dat)[1]

  temp <-
    data.frame(beta = post_dat$beta * multiplier,
               lnalpha = post_dat$lnalpha,
               phi = ifelse(is.na(post_dat$phi), 0, post_dat$phi),
               sigma = post_dat$sigma) %>%
    as.data.frame() %>%
    dplyr::mutate(S.msy = lnalpha / beta * (0.5 - 0.07 * lnalpha),
                  R.msy = S.msy * exp(lnalpha - beta * S.msy),
                  MSY = R.msy - S.msy) %>%
    tibble::as_tibble() %>%
    tibble::rownames_to_column(var = "id_var")

  s <- seq(0, median(temp$S.msy) * 6, by = median(temp$S.msy) * 6 / 1000)

  temp2 <-
    dplyr::inner_join(temp,
                      data.frame(id_var = as.character(rep(1:samples, each = length(s))),
                                 s = rep(s, samples), stringsAsFactors = FALSE),
                      by = "id_var") %>%
    dplyr::mutate(Rs = s  * exp(lnalpha  - beta * s),
                  SY = Rs - s,
                  OYP_custom = (SY - OYP_pct / 100 * MSY) > 0,
                  OYP90 = (SY - 0.9 * MSY) > 0
    ) %>%
    dplyr::select(s, dplyr::starts_with("O"), SY) %>%
    dplyr::group_by(s) %>%
    dplyr::summarise(OYP90 = mean(OYP90, na.rm = TRUE),
                     !!paste0("OYP", OYP_pct) := mean(OYP_custom, na.rm = TRUE),
                     SY = median(SY, na.rm = TRUE)) %>%
    dplyr::mutate(S.msy = median(temp$S.msy)) %>%
    dplyr::ungroup()

  if(is.na(OYP_pct)){temp2[, c("s", "OYP90", "SY", "S.msy")]} else(temp2)
}

#' OYP Plot
#'
#' Produces a faceted plot of OYP an overlay of the goal range.
#'
#' @param profile_dat Output of the get_profile function
#' @param goal_dat A dataframe containing calendar year (yr), the escapement goal lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include years where the goal changed. ub = NA for lower bound SEGs.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#' @param limit Upper bound of spawners for plot. Default (NULL) will use 2.25 times S.msy.
#'
#' @return A figure
#'
#' @examples
#' plot_profile(get_profile(post, 1e-5), "Igushik River Sockeye Salmon", c(150000, 400000))
#' plot_profile(get_profile(post, 1e-5, 80), "Igushik River Sockeye Salmon", c(150000, 400000))
#'
#' @export

plot_profile <- function(profile_dat,
                         goal_dat,
                         title,
                         limit = NULL){
  S.msy50 <- median(profile_dat$S.msy)
  n_OYP <- sum(grepl("OYP\\d+", names(profile_dat)))

  if(is.null(limit)){
    xmax <- S.msy50 * 2.25
  }
  else xmax <- limit

  goal_range <- as.numeric(goal_dat[dim(goal_dat)[1], c(2, 3)])

  cap_width = 85
  OYP2 <- sum(!(names(profile_dat) %in% c( "s", "OYP90", "SY", "S.msy")))
  cap <-
    case_when(
      is.na(goal_range) & OYP2 == 1 ~ str_wrap("Note: Optimal Yield Profiles (OYP) show the probability (under average productivity) of achieving X% of maximum sustained yield (MSY) relative to the number of salmon escaped. Achieving 90% of MSY is the standard criteria used to identify an escapement goal range.", width = cap_width),
      is.na(goal_range) & OYP2 != 1 ~ str_wrap("Note: Optimal Yield Profiles (OYP) show the probability (under average productivity) of achieving 90% of maximum sustained yield (MSY) relative to the number of salmon escaped. Achieving 90% of MSY is the standard criteria used to identify an escapement goal range.", width = cap_width),
      !is.na(goal_range) & OYP2 == 1 ~ str_wrap("Note: Optimal Yield Profiles (OYP) show the probability (under average productivity) of achieving 90% of maximum sustained yield (MSY) relative to the number of salmon escaped. Probabilities associated with the intersection of the OYP curve and the escapement goal bounds are useful to describe the utility of the goal range with respect to MSY. Achieving 90% of MSY is the standard criteria used to describe an escapement goal range.", width = cap_width),
      !is.na(goal_range) & OYP2 != 1 ~ str_wrap("Note: Optimal Yield Profiles (OYP) show the probability (under average productivity) of achieving X% of maximum sustained yield (MSY) relative to the number of salmon escaped. Probabilities associated with the intersection of the OYP curve and the escapement goal bounds are useful to describe the utility of the goal range with respect to MSY. Achieving 90% of MSY is the standard criteria used to describe an escapement goal range.", width = cap_width)
    )

  plot <- profile_dat %>%
    dplyr::group_by(s) %>%
    dplyr::filter(s <= xmax) %>%
    tidyr::gather("key", "prob", -s, -S.msy, -SY, factor_key = TRUE) %>%
    dplyr::mutate(max_pct = gsub("[A-Z]+([0-9]+)", "\\1", key)) %>%
    ggplot2::ggplot(ggplot2::aes(x = s, y = prob, linetype = max_pct)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(limits = c(0, xmax), labels = scales::comma) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    ggplot2::scale_linetype_manual(name = "Percent of MSY",
                                   values = if(n_OYP == 2){c("dashed", "solid")}else("solid"))+
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::ggtitle(title) +
    labs(
      title = title,
      x = "Escapement",
      y = "Probability",
      caption = cap) +
    theme(text = element_text(family = "sans"),
          plot.caption = element_text(
            hjust = 0,
            size = 10),
          plot.caption.position = "plot")

  if(!anyNA(goal_range)) {
    dat <- data.frame(xmin = unname(goal_range[1]), xmax = unname(goal_range[2]), ymin = -Inf, ymax = Inf)

    plot2 <-
      plot +
      ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                         data = dat,
                         inherit.aes = FALSE, fill = "grey", alpha = 0.2) +
      ggplot2::annotate("segment",
                        x = -Inf, xend = goal_range[[1]],
                        y = profile_dat[["OYP90"]][which.min(abs(profile_dat$s - goal_range[[1]]))],
                        linewidth = 0.25) +
      ggplot2::annotate("segment",
                        x = -Inf, xend = goal_range[[2]],
                        y = profile_dat[["OYP90"]][which.min(abs(profile_dat$s - goal_range[[2]]))],
                        linewidth = 0.25)

    if(OYP2 == 1){
      col <- names(profile_dat)[!(names(profile_dat) %in% c( "s", "OYP90", "SY", "S.msy"))]
      plot2 +
        ggplot2::annotate("segment",
                          x = -Inf, xend = goal_range[[1]],
                          y = profile_dat[[col]][which.min(abs(profile_dat$s - goal_range[[1]]))],
                          linewidth = 0.25,
                          linetype = 2) +
        ggplot2::annotate("segment",
                          x = -Inf, xend = goal_range[[2]],
                          y = profile_dat[[col]][which.min(abs(profile_dat$s - goal_range[[2]]))],
                          linewidth = 0.25,
                          linetype = 2)
    }
    else plot2
  }
  else plot
}

make.age <- function(agedata,min.age,max.age,combine=TRUE){
  eage <- names(agedata)[substr(names(agedata),1,1) =='a']
  rage <- names(agedata)[substr(names(agedata),1,1) =='A']
  if(length(eage)>0){
    ac <- data.frame(t(agedata[,eage]))
    # Create European Age  fw.sw
    ac$eage <-as.numeric(substr(rownames(ac),2,5))
    # Convert European to Actual Age: freshwater age + seawater age + 1
    ac$age <- round(with(ac, floor(eage)+ 10*(eage-floor(eage)))+1)
  } else if(length(rage)>0){
    ac <- data.frame(t(agedata[rage]))
    ac$age <- round(as.numeric(substr(rownames(ac),2,3)))
  }
  # Combine of eliminate age
  if(isTRUE(combine)){
    ac$age <- with(ac, ifelse(age<min.age,min.age,ifelse(age >max.age,max.age,age)))
  } else {
    ac <- ac[which(ac$age>=min.age & ac$age<=max.age),]
  }
  # change NA to 0
  ac[is.na(ac)] <- 0
  # combine age
  t.ac <- aggregate(.~age,sum,data=ac[,names(ac) != 'eage'])
  age <- t.ac$age
  t.ac <-data.frame(t(t.ac[,names(t.ac) != 'age']))
  names(t.ac) <- paste0('A',age)
  t.ac <- data.frame(proportions(as.matrix(t.ac),margin=1))
  return(t.ac)
}

make.brood <- function(data,p){
  # Extract name of age data (A2,A3,etc)
  A.age <- names(data)[substr(names(data),1,1) =='A']
  # Convert the name to to numeric age
  N.age <- as.numeric(substr(A.age,2,2))
  # fage is the first age
  fage <- min(N.age)
  # nages is the number of return ages
  nages <- length(N.age)
  # lage is the last reutn ages
  lage <- fage+nages-1
  # Calculate maximum brood year range:
  byr <- seq(min(data$yr)-lage,max(data$yr))
  # Set up brood year matrix
  brood <- matrix(0,ncol=nages+2,nrow = length(byr))
  # First column is year
  brood[,1] <- byr
  # Second column is Escapement by year
  brood[,2] <- c(rep(NA,lage),data$S)
  # 3rd to the last columns are brood year return by age
  # Age comp data
  if(isTRUE(p)) {data[,A.age] <- data$N*data[,A.age]}
  # Case: only 1 age (Pink Salmon)
  if(nages ==1){
    brood[,3] <- c(rep(NA,lage-fage),data[,3],rep(NA,fage))
  } else {
    for(i in 1:nages){
      brood[,i+2] <- c(rep(NA,lage-fage+1-i),data[,i+3],rep(NA,fage+i-1))
    }
  }
  # Change to data.frame
  brood <- data.frame(brood)
  # Name all columns
  names(brood) <- c('byr','S',paste0('b.Age',seq(fage,lage)))
  # Recruit is sum of brood year return by age
  if(nages==1){
    brood$R <- brood[,-c(1:2)]
  } else {
    brood$R <- rowSums(brood[,-c(1:2)])
  }
  # Create SR data
  SR <- brood[complete.cases(brood),c('byr','S','R')]
  out <- list(brood=brood,SR=SR)
  # Output data is a list data
  SR #return(out)
}

#' Spawner-Recruit Plot
#'
#' Produces a SR plot with an overlay of Smsy and the goal range.
#'
#' @param post_dat A dataframe containing lnalpha, beta, phi, and sigma. Can handle point estimates (input as a single row) or mcmc samples (input as multiple rows)
#' @param SR_dat A dataframe containing brood year (byr), Recruits (R), and Spawners (S) to be included in the plot.
#' @param goal_dat A dataframe containing calendar year (yr), the escapement goal lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include years where the goal changed. ub = NA for lower bound SEGs.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#' @param multiplier The Shiny app uses a multiplier to scale beta. Input that here. Defaults to 1.
#'
#' @return A figure
#'
#' @examples
#' plot_SR(post, SR, "Igushik River Sockeye Salmon", c(150000, 400000), 2006, 1e-5)
#'
#' @export

plot_SR <- function(post_dat,
                    SR_dat,
                    goal_dat,
                    title,
                    multiplier){
  param_50 <-
    data.frame(beta = post_dat[["beta"]] * multiplier,
               lnalpha = post_dat[["lnalpha"]],
               phi = post_dat[["phi"]],
               sigma = post_dat[["sigma"]]) %>%
    summarise(beta = median(beta),
              lnalpha = median(lnalpha),
              phi = median(phi),
              sigma = median(sigma),
              Smsy = lnalpha / beta * (0.5 - 0.07 * lnalpha))

  goal_range <- as.numeric(goal_dat[dim(goal_dat)[1], c(2, 3)])

  # Identify brood years added since the last goal change.
  # Likely fragile. Need Hamachan to output some of this stuff.
  yr_max <- max(SR_dat$byr) + as.numeric(rownames(SR_dat)[1])
  yr_modified <- goal_dat$yr[if(max(goal_dat$yr) > yr_max){dim(goal_dat)[1] - 1}else(dim(goal_dat)[1])]
  SR_dat <-
    SR_dat %>%
    mutate(update = ifelse(byr >= min(yr_max, yr_modified) - as.numeric(rownames(SR_dat)[1]),
                           "updated",
                           "existing"))

  upper_x = max(SR_dat$S) * 1.05
  upper_y = max(SR_dat$R) * 1.05

  cap_width = 85
  cap <-
    case_when(
      sum(SR_dat$update == "updated") == 0 ~ str_wrap("Note: The dashed line represents the 1:1 line; points above this line represent spawning events that produced a harvestable surplus. The vertical line shows Smsy. The current escapement goal range is shaded grey.", width = cap_width),
      sum(SR_dat$update == "updated") > 0 ~ str_wrap("Note: Filled circles indicated observations added to the dataset since the escapement goal last changed. The dashed line represents the 1:1 line; points above this line represent spawning events that produced a harvestable surplus. The vertical line shows Smsy. The current escapement goal range is shaded grey.", width = cap_width)
    )

  plot <-
    ggplot2::ggplot(SR_dat, ggplot2::aes(x = S, y = R)) +
    ggplot2::geom_point(aes(shape = update), size = 2) +
    ggplot2::stat_function(fun=function(x){x * exp(param_50$lnalpha - param_50$beta * x)},
                           linewidth = 1.5,
                           xlim = c(0, upper_x)) +
    ggplot2::scale_x_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::coord_cartesian(xlim = c(0, upper_x), ylim = c(0, upper_y)) +
    ggplot2::geom_abline(slope = 1, linewidth = 1, linetype = 2) +
    ggplot2::geom_vline(xintercept = param_50$Smsy) +
    ggplot2::theme_bw(base_size = 16) +
    scale_shape_manual(values = c("updated" = 16, "existing" = 1)) +
    labs(
      title = title,
      subtitle = paste0("Brood Years:", min(SR_dat$byr), " - ", max(SR_dat$byr)),
      x = "Escapement",
      y = "Recruitment",
      caption = cap) +
    theme(text = element_text(family = "sans"),
          plot.caption = element_text(
            hjust = 0,
            size = 10),
          plot.subtitle = element_text(size = 10),
          plot.caption.position = "plot",
          legend.position = "none")

  if(!anyNA(goal_range)) {
    dat <- data.frame(xmin = unname(goal_range[1]), xmax = unname(goal_range[2]), ymin = -Inf, ymax = Inf)
    plot +
      ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                         data = dat,
                         inherit.aes = FALSE, fill = "grey", alpha = 0.2)
  }
  else plot
}


#' Expected Yield Plot
#'
#' Produces a SR plot with an overlay of Smsy and the goal range.
#'
#' @param profile_dat Output of the get_profile function.
#' @param SR_dat A dataframe containing brood year (byr), Recruits (R), and Spawners (S) to be included in the plot.
#' @param goal_dat A dataframe containing calendar year (yr), the escapement goal lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include years where the goal changed. ub = NA for lower bound SEGs.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#'
#' @return A figure
#'
#' @examples
#' plot_ey(post, SR, "Igushik River Sockeye Salmon", c(150000, 400000), 2006)
#'
#' @export

plot_ey <- function(profile_dat,
                    SR_dat,
                    goal_dat,
                    title){
  plot_dat <-
    profile_dat %>%
    dplyr::select(s, dplyr::starts_with("SY")) %>%
    dplyr::group_by(s) %>%
    dplyr::summarise(median.SY = median(SY, na.rm = TRUE),
                     p25.SY = quantile(SY, probs = 0.25, na.rm = TRUE),
                     p75.SY = quantile(SY, probs = 0.75, na.rm = TRUE)
    ) %>%
    tidyr::gather(Productivity, SY, median.SY)

  goal_range <- as.numeric(goal_dat[dim(goal_dat)[1], c(2, 3)])

  # Identify brood years added since the last goal change.
  # Likely fragile. Need Hamachan to output some of this stuff.
  yr_max <- max(SR_dat$byr) + as.numeric(rownames(SR_dat)[1])
  yr_modified <- goal_dat$yr[if(max(goal_dat$yr) > yr_max){dim(goal_dat)[1] - 1}else(dim(goal_dat)[1])]
  SR_dat <-
    SR_dat %>%
    mutate(Y = R - S,
           update = ifelse(byr >= min(yr_max, yr_modified) - as.numeric(rownames(SR_dat)[1]),
                           "updated",
                           "existing"))

  ymax <- max(SR_dat$Y) * 1.05
  ymin <- if(min(SR_dat$Y) < 0){min(SR_dat$Y) * 1.05} else{0}
  xmax <- max(SR_dat$S) * 1.05

  cap_width = 85
  cap <-
    case_when(
      sum(SR_dat$update == "updated") == 0 ~ str_wrap("Note: The current escapement goal range is shaded grey.", width = cap_width),
      sum(SR_dat$update == "updated") > 0 ~ str_wrap("Note: Filled circles indicated observations added to the dataset since the escapement goal was last modified. The current escapement goal range is shaded grey.", width = cap_width)
    )

  plot <-
    ggplot2::ggplot(SR_dat, aes(x = S, y = Y)) +
    geom_point(aes(shape = update), size = 2) +
    ggplot2::geom_line(ggplot2::aes(x = s, y = SY, color = Productivity), data = plot_dat) +
    ggplot2::geom_ribbon(ggplot2::aes(x = s, ymin = p25.SY, ymax = p75.SY), data = plot_dat, inherit.aes = FALSE, alpha = 0.1) +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::coord_cartesian(xlim = c(0, xmax), ylim = c(ymin, ymax)) +
    ggplot2::scale_color_manual(guide = "none", values = "black") +
    scale_shape_manual(values = c("updated" = 16, "existing" = 1)) +
    ggplot2::theme_bw(base_size = 16) +
    labs(
      title = title,
      subtitle = paste0("Brood Years:", min(SR_dat$byr), " - ", max(SR_dat$byr)),
      x = "Escapement",
      y = "Yield",
      caption = cap) +
    theme(text = element_text(family = "sans"),
          plot.caption = element_text(
            hjust = 0,
            size = 10),
          plot.subtitle = element_text(size = 10),
          plot.caption.position = "plot",
          legend.position = "none")

  if(!anyNA(goal_range)) {
    dat <- data.frame(xmin = unname(goal_range[1]), xmax = unname(goal_range[2]), ymin = -Inf, ymax = Inf)
    plot + ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                              data = dat,
                              inherit.aes = FALSE, fill = "grey", alpha = 0.2)
  }
  else plot
}

#' Historical Escapement Plot
#'
#' Produces a plot of histocial escapements with an overlay of the goal range.
#'
#' @param S_dat A dataframe containing calendar year (yr) and escapement(S).
#' @param goal_dat A dataframe containing calendar year (yr), the escapement goal lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include years where the goal changed. ub = NA for lower bound SEGs.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#'
#' @return A figure
#'
#' @examples
#' plot_S(df, goal_df, "Igushik River Sockeye Salmon")
#'
#' @export
plot_S <- function(S_dat,
                   goal_dat,
                   title){
  goal <-
    goal_dat %>%
    mutate(across(c(lb, ub), function(x){ifelse(is.na(x), -99, x)})) %>%
    complete(yr = full_seq(c(yr, max(S_dat$yr)), 1)) %>%
    fill(lb, ub, .direction = "down") %>%
    mutate(across(c(lb, ub), function(x){ifelse(x == -99, NA, x)})) %>%
    pivot_longer(cols = c(lb, ub), names_to = "bound", values_to = "S_bound")

  cap <- str_wrap("Note: Escapement goal lower and upper bounds are shown as solid and dashed lines, respectively. Escapements below the lower bound of the contemporaneous escapement goal are indicated with black fill.",
                  width = 85)

  S_dat %>%
    select(yr, S) %>%
    left_join(goal[goal$bound == "lb", ], by = "yr") %>%
    mutate(miss = ifelse(S >= S_bound | is.na(S_bound), FALSE, TRUE)) %>%
    ggplot(aes(x = yr)) +
    geom_bar(aes(y = S, fill = miss), stat = "identity") +
    geom_line(aes(y = S_bound, linetype = bound), data = goal) +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("grey75", "black")) +
    theme_bw(base_size = 16) +
    labs(
      title = title,
      x = "Year",
      y = "Escapement",
      caption = cap) +
    theme(text = element_text(family = "sans"),
          plot.caption = element_text(
            hjust = 0,
            size = 10),
          plot.caption.position = "plot",
          legend.position = "none")
}

