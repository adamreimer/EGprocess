library(tidyverse)
#' Creates a dataset for plotting OYP and EY plots
#'
#' This function creates a dataframe that can be used by plot_profile() and plot_ey()
#'
#' @param post_dat An mcmc object with nodes lnalpha, beta, phi, and sigma.
#' @param multiplier The Shiny app uses a multiplier to scale beta. Input that here. Defaults to 1.
#' @param MSY_pct Either 70 or 80 corresponding to a 70% or 80% OYP, respectively. Defaults to NA. The 90% OYP is included regardless.
#'
#' @return A data.frame
#'
#' @examples
#' get_profile(post_Igushik_byr63-15, 1e-5)
#'
#' @export

get_profile <- function(post_dat, multiplier = 1, MSY_pct = NA){
  if (!is.na(MSY_pct) & !(MSY_pct %in% c(70, 80))) {
    stop("Error: 'MSY_pct' must be either 70 or 80 coresponding to a 70% or 80% OYP. The 90% OYP is included by default")
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
                  OYP_custom = (SY - MSY_pct / 100 * MSY) > 0,
                  OYP90 = (SY - 0.9 * MSY) > 0
    ) %>%
    dplyr::select(s, dplyr::starts_with("O"), SY) %>%
    dplyr::group_by(s) %>%
    dplyr::summarise(OYP90 = mean(OYP90, na.rm = TRUE),
                     !!paste0("OYP", MSY_pct) := mean(OYP_custom, na.rm = TRUE),
                     SY = median(SY, na.rm = TRUE)) %>%
    dplyr::mutate(S.msy = median(temp$S.msy)) %>%
    dplyr::ungroup()

  if(is.na(MSY_pct)){temp2[, c("s", "OYP90", "SY", "S.msy")]} else(temp2)
}

#' OYP Plot
#'
#' Produces a OYP with an overlay of the goal range.
#'
#' @param profile_dat Output of the get_profile function
#' @param goal_dat  A dataframe containing calendar year (yr), the escapement goal lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include years where the goal changed. If the updated analysis resulted in a new escapement goal finding the new finding should be included as the last row with the year labeled as "new". Use ub = NA for lower bound SEGs.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#' @param limit Upper bound of spawners for plot. Default (NULL) will use 2.25 times S.msy.
#'
#' @return A figure
#'
#' @examples
#'
#' post_list <-
#'   list(
#'     'Brood Years: 1963-2005' = post_Igushik_byr63_05,
#'     'Brood Years: 1963-2017' = post_Igushik_byr63-15
#'   )
#' profile_list <- lapply(post_list, get_profile, multiplier = 1e-5)
#'
#' plot_profile2(profile_list, goal_Igushik, "Igushik River Sockeye Salmon")
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
      OYP2 == 1 ~ str_wrap("Note: Optimal Yield Profiles (OYP) show the probability (under average productivity) of achieving 90% of maximum sustained yield (MSY) relative to the number of salmon escaped. Probabilities associated with the intersection of the OYP curve and the escapement goal bounds are useful to describe the utility of the goal range with respect to MSY. Achieving 90% of MSY is the standard criteria used to describe an escapement goal range.", width = cap_width),
      OYP2 != 1 ~ str_wrap("Note: Optimal Yield Profiles (OYP) show the probability (under average productivity) of achieving X% of maximum sustained yield (MSY) relative to the number of salmon escaped. Probabilities associated with the intersection of the OYP curve and the escapement goal bounds are useful to describe the utility of the goal range with respect to MSY. Achieving 90% of MSY is the standard criteria used to describe an escapement goal range.", width = cap_width)
    )

  plot <- profile_dat %>%
    dplyr::group_by(s) %>%
    dplyr::filter(s <= xmax) %>%
    tidyr::gather("key", "prob", -s, -S.msy, -SY, factor_key = TRUE) %>%
    dplyr::mutate(max_pct = gsub("[A-Z]+([0-9]+)", "\\1", key)) %>%
    ggplot2::ggplot(ggplot2::aes(x = s, y = prob, linetype = max_pct)) +
    ggplot2::geom_line() +
    ggplot2::geom_rect(ggplot2::aes(xmin = lb, xmax = ub, ymin = -Inf, ymax = Inf),
                       data = goal_dat[dim(goal_dat)[1], ],
                       inherit.aes = FALSE, fill = "grey", alpha = 0.2) +
    ggplot2::annotate("segment",
                      x = -Inf, xend = goal_range[[1]],
                      y = profile_dat[["OYP90"]][which.min(abs(profile_dat$s - goal_range[[1]]))],
                      linewidth = 0.25) +
    ggplot2::annotate("segment",
                      x = -Inf, xend = goal_range[[2]],
                      y = profile_dat[["OYP90"]][which.min(abs(profile_dat$s - goal_range[[2]]))],
                      linewidth = 0.25) +
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

    if(OYP2 == 1){
      col <- names(profile_dat)[!(names(profile_dat) %in% c( "s", "OYP90", "SY", "S.msy"))]
      plot +
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
    else plot
}

#' Comparative OYP Plot
#'
#' Produces faceted OYP plots representing the prior and updated OYPs for the stock, both overlain with escapement goal ranges. If a new escapement goal finding was made as a result of the updated analysis the new escapement goal will be overlay the updated OYP while the prior escapement goal range will overlay the prior OYP.
#'
#' @param profile_dat a list of 2 OYPs that you wish to plot. The name each list object will be used as the facet title. Recommend "Brood Year: xxxx - YYYY" for each item. The function assumes the first item in the list is the old OYP and the second item in the list is the updated OYP.
#' @param goal_dat A dataframe containing calendar year (yr), the escapement goal lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include years where the goal changed. If the updated analysis resulted in a new escapement goal finding the new finding should be included as the last row with the year labeled as "new". Use ub = NA for lower bound SEGs.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#' @param limit Upper bound of spawners for plot. Default (NULL) will use 2.25 times S.msy.
#'
#' @return A figure
#'
#' @examples
#'
#' post_list <-
#'   list(
#'     'Brood Years: 1963-2005' = post_Igushik_byr63_05,
#'     'Brood Years: 1963-2017' = post_Igushik_byr63-15
#'   )
#' profile_list <- lapply(post_list, get_profile, multiplier = 1e-5)
#'
#' plot_profile2(profile_list, goal_Igushik, "Igushik River Sockeye Salmon")
#'
#' @export

plot_profile2 <- function(profile_dat,
                          goal_dat,
                          title,
                          limit = NULL){
  S.msy50 <- max(sapply(profile_dat, function(x) median(x[["S.msy"]])))
  n_OYP <- sapply(profile_dat, function(x) sum(grepl("OYP\\d+", names(x))))
  name_OYP <- names(profile_dat[[2]])[!(names(profile_dat[[2]]) %in% c("s", "OYP90", "SY", "S.msy"))]

  if(is.null(limit)){
    xmax <- S.msy50 * 2.25
  }
  else xmax <- limit

  goal_dat <-
    if(goal_dat$yr[dim(goal_dat)[1]] == "new")
    {goal_dat[(dim(goal_dat)[1] - 1):dim(goal_dat)[1], ]}else{
      goal_dat[rep(dim(goal_dat)[1], 2), ]}
  goal_dat$profile <- names(profile_dat)

  cap_width = 85
  cap <-
    case_when(
      max(n_OYP) == 1 ~ str_wrap("Note: Optimal Yield Profiles (OYP) show the probability (under average productivity) of achieving 90% of maximum sustained yield (MSY) relative to the number of salmon escaped. Probabilities associated with the intersection of the OYP curve and the escapement goal bounds are useful to describe the utility of the goal range with respect to MSY. Achieving 90% of MSY is the standard criteria used to describe an escapement goal range.", width = cap_width),
      max(n_OYP) == 2 ~ str_wrap("Note: Optimal Yield Profiles (OYP) show the probability (under average productivity) of achieving X% of maximum sustained yield (MSY) relative to the number of salmon escaped. Probabilities associated with the intersection of the OYP curve and the escapement goal bounds are useful to describe the utility of the goal range with respect to MSY. Achieving 90% of MSY is the standard criteria used to describe an escapement goal range.", width = cap_width)
    )

  ref_lines0 <-
    data.frame(profile = names(profile_dat), lb = goal_dat$lb, ub = goal_dat$ub) %>%
    pivot_longer(cols = lb:ub, values_to = "xend") %>%
    rowwise() %>%
    mutate(y90 = profile_dat[[profile]][["OYP90"]][which.min(abs(profile_dat[[profile]]$s - xend))],
           x = -Inf)

  if(length(name_OYP) == 0){}
  else(ref_lines0 <-
         ref_lines0 <-
         ref_lines0 %>%
         mutate(y00 = profile_dat[[profile]][[name_OYP]][which.min(abs(profile_dat[[profile]]$s - xend))])
  )

  ref_lines <-
    ref_lines0 %>%
    pivot_longer(dplyr::starts_with("y"),
                 values_to = "y",
                 names_to = "max_pct",
                 names_prefix = "y") %>%
    mutate(max_pct = ifelse(max_pct == "00", gsub("OYP(\\d+)", "\\1", name_OYP), max_pct))

  lapply(1:length(profile_dat), function(x) mutate(profile_dat[[x]], profile = names(profile_dat)[x])) %>%
    do.call(rbind, .) %>%
    dplyr::group_by(s) %>%
    dplyr::filter(s <= xmax) %>%
    tidyr::gather("key", "prob", -s, -S.msy, -SY, - profile, factor_key = TRUE) %>%
    dplyr::mutate(max_pct = gsub("[A-Z]+([0-9]+)", "\\1", key)) %>%
    ggplot2::ggplot(ggplot2::aes(x = s, y = prob, linetype = max_pct)) +
    ggplot2::geom_line() +
    ggplot2::geom_segment(aes(x = x, xend = xend, y = y), data = ref_lines, linewidth = 0.25) +
    ggplot2::geom_rect(ggplot2::aes(xmin = lb, xmax = ub, ymin = -Inf, ymax = Inf),
                       data = goal_dat,
                       inherit.aes = FALSE, fill = "grey", alpha = 0.2) +
    ggplot2::facet_grid(. ~ profile) +
    ggplot2::scale_x_continuous(limits = c(0, xmax), labels = scales::comma) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
    ggplot2::scale_linetype_manual(name = "Percent of MSY",
                                   values = if(max(n_OYP) == 2){c("dashed", "solid")}else("solid"))+
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
  names(brood) <- c('yr','S',paste0('b.Age',seq(fage,lage)))
  # Recruit is sum of brood year return by age
  if(nages==1){
    brood$R <- brood[,-c(1:2)]
  } else {
    brood$R <- rowSums(brood[,-c(1:2)])
  }
  # Create SR data
  # SR <- brood[complete.cases(brood),c('byr','S','R')]
  # out <- list(brood=brood,SR=SR)
  # Output data is a list data
  brood #return(out)
}

#' Spawner-Recruit Plot
#'
#' Produces a SR plot with an overlay of Smsy and the goal range.
#'
#' @param post_dat A dataframe containing lnalpha, beta, phi, and sigma. Can handle point estimates (input as a single row) or mcmc samples (input as multiple rows)
#' @param brood_dat A dataframe containing year (yr), Spawners (S), and Recruits (R) to be included in the plot. The data frame should include years without empirical observations of S and R.
#' @param goal_dat  A dataframe containing calendar year (yr), the escapement goal lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include years where the goal changed. If the updated analysis resulted in a new escapement goal finding the new finding should be included as the last row with the year labeled as "new". Use ub = NA for lower bound SEGs.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#' @param multiplier The Shiny app uses a multiplier to scale beta. Input that here. Defaults to 1.
#'
#' @return A figure
#'
#' @examples
#'
#' p_Igushik <- make.age(data_Igushik, 3, 8)
#' brood_Igushik <- make.brood(data_Igushik, p_Igushik)
#'
#' plot_SR(post_Igushik_byr63-15, brood_Igushik, goal_Igushik, "Igushik River Sockeye Salmon", 1e-5)
#'
#' @export

plot_SR <- function(post_dat,
                    brood_dat,
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

  # Identify brood years added since the last goal change.
  # Likely fragile. Need Hamachan to output some of this stuff.
  max_age <- length(brood_dat$yr[is.na(brood_dat$R) & !is.na(brood_dat$S)])
  byr_modified <- suppressWarnings(max(as.numeric(goal_dat$yr[goal_dat$yr != "new"])) - max_age)
  brood_dat <-
    brood_dat %>%
    mutate(update = ifelse(yr > byr_modified, "updated", "existing")) %>%
    select(yr, S, R, update) %>%
    filter(complete.cases(.))

  upper_x = max(brood_dat$S) * 1.05
  upper_y = max(brood_dat$R) * 1.05

  cap_width = 85
  cap <-
    case_when(
      sum(brood_dat$update == "existing") == 0 ~ str_wrap("Note: The dashed line represents the 1:1 line; points above this line represent spawning events that produced a harvestable surplus. The vertical line shows Smsy. The current escapement goal range is shaded grey.", width = cap_width),
      sum(brood_dat$update == "updated") > 0 ~ str_wrap("Note: Filled circles indicated observations added to the dataset since the escapement goal last changed. The dashed line represents the 1:1 line; points above this line represent spawning events that produced a harvestable surplus. The vertical line shows Smsy. The current escapement goal range is shaded grey.", width = cap_width)
    )

  ggplot2::ggplot(brood_dat, ggplot2::aes(x = S, y = R)) +
    ggplot2::geom_point(aes(shape = update), size = 2) +
    ggplot2::stat_function(fun=function(x){x * exp(param_50$lnalpha - param_50$beta * x)},
                           linewidth = 1.5,
                           xlim = c(0, upper_x)) +
    ggplot2::scale_x_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::scale_y_continuous(minor_breaks = NULL, labels = scales::comma) +
    ggplot2::coord_cartesian(xlim = c(0, upper_x), ylim = c(0, upper_y)) +
    ggplot2::geom_abline(slope = 1, linewidth = 1, linetype = 2) +
    ggplot2::geom_vline(xintercept = param_50$Smsy) +
    ggplot2::geom_rect(ggplot2::aes(xmin = lb, xmax = ub, ymin = -Inf, ymax = Inf),
                       data = goal_dat[dim(goal_dat)[1], ],
                       inherit.aes = FALSE, fill = "grey", alpha = 0.2) +
    ggplot2::theme_bw(base_size = 16) +
    scale_shape_manual(values = c("updated" = 16, "existing" = 1)) +
    labs(
      title = title,
      subtitle = paste0("Brood Years:", min(brood_dat$yr), " - ", max(brood_dat$yr)),
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
}


#' Expected Yield Plot
#'
#' Produces a SR plot with an overlay of Smsy and the goal range.
#'
#' @param profile_dat Output of the get_profile function.
#' @param brood_dat A dataframe containing year (yr), Spawners (S), and Recruits (R) to be included in the plot. The data frame should include years without empirical observations of S and R.
#' @param goal_dat  A dataframe containing calendar year (yr), the escapement goal lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include years where the goal changed. If the updated analysis resulted in a new escapement goal finding the new finding should be included as the last row with the year labeled as "new". Use ub = NA for lower bound SEGs.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#'
#' @return A figure
#'
#' @examples
#'
#' p_Igushik <- make.age(data_Igushik, 3, 8)
#' brood_Igushik <- make.brood(data_Igushik, p_Igushik)
#'
#' profile_Igushik <- get_profile(post_Igushik_byr63-15, multiplier = 1e-5)
#'
#' plot_ey(profile_Igushik, brood_Igushik, goal_Igushik, "Igushik River Sockeye Salmon")
#'
#' @export

plot_ey <- function(profile_dat,
                    brood_dat,
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
  max_age <- length(brood_dat$yr[is.na(brood_dat$R) & !is.na(brood_dat$S)])
  byr_modified <- suppressWarnings(max(as.numeric(goal_dat$yr[goal_dat$yr != "new"])) - max_age)
  brood_dat <-
    brood_dat %>%
    mutate(update = ifelse(yr > byr_modified, "updated", "existing"),
           Y = R - S) %>%
    select(yr, S, Y, update) %>%
    filter(complete.cases(.))

  ymax <- max(brood_dat$Y) * 1.05
  ymin <- if(min(brood_dat$Y) < 0){min(brood_dat$Y) * 1.05} else{0}
  xmax <- max(brood_dat$S) * 1.05

  cap_width = 85
  cap <-
    case_when(
      sum(brood_dat$update == "existing") == 0 ~ str_wrap("Note: The current escapement goal range is shaded grey.", width = cap_width),
      sum(brood_dat$update == "updated") > 0 ~ str_wrap("Note: Filled circles indicated observations added to the dataset since the escapement goal was last modified. The current escapement goal range is shaded grey.", width = cap_width)
    )

  ggplot2::ggplot(brood_dat, aes(x = S, y = Y)) +
    geom_point(aes(shape = update), size = 2) +
    ggplot2::geom_line(ggplot2::aes(x = s, y = SY, color = Productivity), data = plot_dat) +
    ggplot2::geom_ribbon(ggplot2::aes(x = s, ymin = p25.SY, ymax = p75.SY), data = plot_dat, inherit.aes = FALSE, alpha = 0.1) +
    ggplot2::geom_rect(ggplot2::aes(xmin = lb, xmax = ub, ymin = -Inf, ymax = Inf),
                       data = goal_dat[dim(goal_dat)[1], ],
                       inherit.aes = FALSE, fill = "grey", alpha = 0.2) +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::coord_cartesian(xlim = c(0, xmax), ylim = c(ymin, ymax)) +
    ggplot2::scale_color_manual(guide = "none", values = "black") +
    scale_shape_manual(values = c("updated" = 16, "existing" = 1)) +
    ggplot2::theme_bw(base_size = 16) +
    labs(
      title = title,
      subtitle = paste0("Brood Years:", min(brood_dat$yr), " - ", max(brood_dat$yr)),
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
}

#' Historical Escapement Plot
#'
#' Produces a plot of historical escapements with an overlay of the goal range.
#'
#' @param brood_dat A dataframe containing calendar year (yr) and escapement(S).
#' @param goal_dat  A dataframe containing calendar year (yr), the escapement goal lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include years where the goal changed. If the updated analysis resulted in a new escapement goal finding the new finding should be included as the last row with the year labeled as "new". Use ub = NA for lower bound SEGs.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#'
#' @return A figure
#'
#' @examples
#'
#' p_Igushik <- make.age(data_Igushik, 3, 8)
#' brood_Igushik <- make.brood(data_Igushik, p_Igushik)
#'
#' plot_S(brood_Igushik, goal_Igushik, "Igushik River Sockeye Salmon")
#'
#' @export
plot_S <- function(brood_dat,
                   goal_dat,
                   title){
  brood_dat <- brood_dat %>% filter(!is.na(S))

  yr_max <- if(sum(goal_dat$yr == "new") == 0){max(brood_dat$yr)}else{max(brood_dat$yr) + 2}
  goal <-
    goal_dat %>%
    mutate(across(c(lb, ub), function(x){ifelse(is.na(x), -99, x)}),
           yr = as.numeric(ifelse(yr == "new", max(brood_dat$yr) + 1, yr))) %>%
    complete(yr = full_seq(c(yr, yr_max), 1)) %>%
    fill(lb, ub, .direction = "down") %>%
    mutate(across(c(lb, ub), function(x){ifelse(x == -99, NA, x)})) %>%
    pivot_longer(cols = c(lb, ub), names_to = "bound", values_to = "S_bound")

  cap <- str_wrap("Note: Escapement goal lower and upper bounds are shown as solid and dashed lines, respectively. Escapements below the lower bound of the contemporaneous escapement goal are indicated with black fill.",
                  width = 85)

  brood_dat %>%
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


#' Escapement goal process output wrapper
#'
#' Produces a list containing standardized escapement goal process tables and figures.
#'
#' @param post_dat An mcmc object with nodes lnalpha, beta, phi, and sigma.
#' @param brood_dat A dataframe containing year (yr), Spawners (S), and Recruits (R) to be included in the plot. The data frame should include years without empirical observations of S and R.
#' @param goal_dat  A dataframe containing calendar year (yr), the escapement goal lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include years where the goal changed. If the updated analysis resulted in a new escapement goal finding the new finding should be included as the last row with the year labeled as "new". Use ub = NA for lower bound SEGs.
#' @param MSY_pct Either 70 or 80 corresponding to a 70% or 80% OYP, respectively. Defaults to NA. The 90% OYP is included regardless.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#'
#' @param multiplier The Shiny app uses a multiplier to scale beta. Input that here. Defaults to 1.
#'
#' @return A figure
#'
#' @examples
#'
#' p_Igushik <- make.age(data_Igushik, 3, 8)
#' brood_Igushik <- make.brood(data_Igushik, p_Igushik)
#'
#' post_list <-
#'   list(
#'     'Brood Years: 1963-2005' = post_Igushik_byr63_05,
#'     'Brood Years: 1963-2017' = post_Igushik_byr63-15
#'   )
#' profile_list <- lapply(post_list, get_profile, multiplier = 1e-5)
#'
#' EGoutput(post_Igushik, brood_Igushik, goal_Igushik, "Igushik River Sockeye Salmon", multiplier = 1e-5)
#'
#' @export
EGoutput <- function(post_dat, brood_dat, goal_dat, title, MSY_pct = NA, multiplier = 1){
  if(length(post_dat) == 2){
    profile_dat <- lapply(post_dat, get_profile, MSY_pct = MSY_pct, multiplier = multiplier)

    out <- list(
      plot_S(brood_dat, goal_dat, title),
      plot_SR(post_dat[[2]], brood_dat, goal_dat, title, multiplier = multiplier),
      plot_ey(profile_dat[[2]], brood_dat, goal_dat, title),
      plot_profile2(profile_dat, goal_dat, title)
    )
  }
  else{
    profile_dat <- get_profile(post_dat, MSY_pct = MSY_pct, multiplier = multiplier)

    out <-   list(
      plot_S(brood_dat, goal_dat, title),
      plot_SR(post_dat, brood_dat, goal_dat, title, multiplier = multiplier),
      plot_ey(profile_dat, brood_dat, goal_dat, title),
      plot_profile(profile_dat, goal_dat, title)
    )
  }

  out
}
