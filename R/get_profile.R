#' @title SR Profile Data Creation
#' @description Creates a dataset for plotting OYP and EY plots.
#' This function creates a dataframe that can be used by plot_profile() and plot_ey()
#'
#' @param post_dat An mcmc object with nodes lnalpha, beta, phi, and sigma.
#' @param multiplier The Shiny app uses a multiplier to scale beta. Input that here. Defaults to 1.
#' @param MSY_pct Either 70 or 80 corresponding to a 70% or 80% OYP, respectively. Defaults to NA. The 90% OYP is included regardless.
#'
#' @return A data.frame
#'
#' @import tidyverse
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
    #JTP: can this be deleted??
    #as.data.frame() %>%
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
