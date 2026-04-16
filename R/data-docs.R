#' Escapement Goal Bounds for the Igushik River
#'
#' A data frame describing year-specific escapement goal bounds used for
#' Igushik River stock assessments. The lower bound (`lb`) and upper bound (`ub`)
#' represent the recommended escapement goal range for each management period.
#'
#' @format A data frame with one row per management period and 3 variables:
#' \describe{
#'   \item{yr}{Calendar year (integer) marking the beginning of the management period.}
#'   \item{lb}{Lower bound of the escapement goal (numeric, number of fish).}
#'   \item{ub}{Upper bound of the escapement goal (numeric, number of fish).}
#' }
#'
#' @details
#' These values are used by downstream functions to produce Optimal Yield Profiles
#' (OYP), reference points such as SMSY, and historical brood year productivity.
#'
#' @source ADF&G and Alaska Board of Fisheries set goals.
#'
#' @examples
#' head(goal_Igushik)
"goal_Igushik"



#' Igushik River Escapement and Age Composition Data
#'
#' A data frame containing annual escapement (`S`), total run (`N`), and age
#' composition columns (`A3`–`A8`) for the Igushik River. These observations are
#' typically used to generate brood tables, estimate recruitment, and prepare
#' inputs for stock–recruit analyses.
#'
#' @format A data frame with one row per year and the following variables:
#' \describe{
#'   \item{yr}{Calendar year of observation.}
#'   \item{S}{Spawning escapement (numeric).}
#'   \item{N}{Total return (numeric).}
#'   \item{A3}{Count or proportion of age‑3 returning fish.}
#'   \item{A4}{Count or proportion of age‑4 returning fish.}
#'   \item{A5}{Count or proportion of age‑5 returning fish.}
#'   \item{A6}{Count or proportion of age‑6 returning fish.}
#'   \item{A7}{Count or proportion of age‑7 returning fish.}
#'   \item{A8}{Count or proportion of age‑8 returning fish.}
#' }
#'
#' @details
#' Age composition fields may represent either expanded counts or proportions,
#' depending on how the dataset was processed prior to being archived.
#' These values are used by functions such as `make_age()` and `make_brood()`.
#' The dataset originates from Igushik River escapement goal analyses.
#'
#' @source Internal ADF&G escapement and age composition data,
#' collected at the Igushik River.
#'
#' @examples
#' head(data_Igushik)
"data_Igushik"



#' Igushik River MCMC Output (Brood Years 1963–2005)
#'
#' A matrix or data frame of posterior samples from a stock–recruitment model
#' fit to Igushik River brood years 1963–2005. Each row represents one MCMC
#' sample and contains primary parameter nodes used by downstream functions such
#' as `get_profile()`, `plot_SR()`, and Optimal Yield Profile calculations.
#'
#' @format A data frame with one row per MCMC draw and columns:
#' \describe{
#'   \item{beta}{Density dependence parameter.}
#'   \item{deviance}{Deviance measure used for MCMC diagnostics.}
#'   \item{e0}{Adam will fill this in}
#'   \item{lnalpha}{Log‑scale productivity parameter.}
#'   \item{phi}{Temporal autocorrelation parameter.}
#'   \item{sigma}{Process or observation error standard deviation.}
#' }
#'
#' @details
#' Posterior samples are used to generate stock–recruit curves, calculate SMSY,
#' and derive Optimal Yield Profiles (OYP).
#'
#' @source Igushik River stock–recruit model (brood years 1963–2005).
#'
#' @examples
#' head(post_Igushik_byr63_05)
"post_Igushik_byr63_05"



#' Igushik River MCMC Output (Brood Years 1963–2017)
#'
#' Similar to `post_Igushik_byr63_05` but using an extended brood‑year series
#' through 2017. These posterior samples allow evaluation of escapement goals
#' using the full contemporary brood table.
#'
#' @format A data frame with one row per MCMC draw and columns:
#' \describe{
#'   \item{beta}{Density dependence parameter.}
#'   \item{deviance}{Deviance measure used for MCMC diagnostics.}
#'   \item{e0}{Adam will fill this in}
#'   \item{lnalpha}{Log-scale productivity parameter.}
#'   \item{phi}{Temporal autocorrelation parameter.}
#'   \item{sigma}{Process or observation error standard deviation.}
#' }
#'
#' @details
#' Posterior samples are used to generate stock–recruit curves, calculate SMSY,
#' and derive Optimal Yield Profiles (OYP).
#' This dataset is nearly identical to `post_Igushik_byr63_05` but based on
#' an expanded brood year range. Used to demonstrate updating a dataset to
#' evaluate historic vs. modern productivity.
#'
#' @source Igushik River stock–recruit model (brood years 1963–2015).
#'
#' @examples
#' head(post_Igushik_byr63_15)
"post_Igushik_byr63_15"
