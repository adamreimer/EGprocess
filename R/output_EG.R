#' @title Escapement goal process output wrapper
#'
#' @description Produces a list containing standardized escapement goal process tables and figures.
#'
#' @param posterior_data An mcmc object with nodes lnalpha, beta, phi, and sigma.
#' @param brood_data A dataframe containing year (yr), Spawners (S), and Recruits (R)
#' to be included in the plot. The data frame should include years without empirical
#' observations of S and R.
#' @param goal_data  A dataframe containing calendar year (yr), the escapement goal
#' lower bound (lb) and, the escapement goal upper bound (ub). Only needs to include
#' years where the goal changed. If the updated analysis resulted in a new escapement
#' goal finding the new finding should be included as the last row with the year
#' labeled as "new". Use ub = NA for lower bound SEGs.
#' @param MSY_pct Either 70 or 80 corresponding to a 70% or 80% OYP, respectively.
#' Defaults to NA. The 90% OYP is included regardless.
#' @param title A character vector with the plot title. Suggest "X River, Y Salmon".
#' @param multiplier The Shiny app uses a multiplier to scale beta. Input that here. Defaults to 1.
#'
#' @return A figure
#'
#' @import dplyr
#' @import tibble
#'
#' @examples
#'
#' p_Igushik <- make_age(data_Igushik, 3, 8)
#' brood_Igushik <- make_brood(data_Igushik, p_Igushik)
#'
#' post_list <-
#'   list(
#'     'Brood Years: 1963-2005' = post_Igushik_byr63_05,
#'     'Brood Years: 1963-2017' = post_Igushik_byr63-15
#'   )
#' profile_list <- lapply(post_list, get_profile, multiplier = 1e-5)
#'
#' EGoutput(posterior_data = post_Igushik, brood_data = brood_Igushik,
#' goal_data = goal_Igushik, title = "Igushik River Sockeye Salmon", multiplier = 1e-5)
#'
#' @export
EGoutput <- function(posterior_data, brood_data, goal_data, title, MSY_pct = NA, multiplier = 1){
  if(length(posterior_data) == 2){
    profile_dat <- lapply(posterior_data, get_profile, MSY_pct = MSY_pct, multiplier = multiplier)

    out <- list(
      plot_escapement(brood_data, goal_data, title),
      plot_SR(posterior_data[[2]], brood_data, goal_data, title, multiplier = multiplier),
      plot_ey(profile_dat[[2]], brood_data, goal_data, title),
      plot_profile_facet(profile_dat, goal_data, title)
    )
  }
  else{
    profile_dat <- get_profile(posterior_data, MSY_pct = MSY_pct, multiplier = multiplier)

    out <- list(
      plot_escapement(brood_data, goal_data, title),
      plot_SR(posterior_data, brood_data, goal_data, title, multiplier = multiplier),
      plot_ey(profile_dat, brood_data, goal_data, title),
      plot_profile(profile_dat, goal_data, title)
    )
  }

  out
}
