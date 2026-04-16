#' @title Create Brood Data
#'
#' @param data Brood data
#' @param p Adam will explain
#'
#' @returns data frame
#'
#' @examples
#' make_brood(data = data_Igushik, p = p_Igushik)
#'
#' #' @export
make_brood <- function(data, p){
  # Extract name of age data (A2, A3,etc)
  A.age <- names(data)[substr(names(data), 1, 1) == 'A']

  # Convert the name to to numeric age
  N.age <- as.numeric(substr(A.age, 2, 2))

  # fage is the first age
  fage <- min(N.age)

  # nages is the number of return ages
  nages <- length(N.age)

  # lage is the last return ages
  lage <- fage+nages-1

  # Calculate maximum brood year range:
  byr <- seq(min(data$yr)-lage,max(data$yr))

  # Set up brood year matrix
  brood <- matrix(0,ncol=nages+2,nrow = length(byr))

  # First column is year
  brood[, 1] <- byr

  # Second column is Escapement by year
  brood[, 2] <- c(rep(NA, lage), data$S)

  # 3rd to the last columns are brood year return by age
  # Age comp data
  if(isTRUE(p)) {data[, A.age] <- data$N*data[, A.age]}
  # Case: only 1 age (Pink Salmon)
  if(nages ==1){
    brood[, 3] <- c(rep(NA, lage-fage), data[, 3], rep(NA, fage))
  } else {
    for(i in 1:nages){
      brood[, i+2] <- c(rep(NA, lage-fage+1-i), data[,i+3], rep(NA, fage+i-1))
    }
  }
  brood <- data.frame(brood)
  # Name all columns
  names(brood) <- c('yr', 'S', paste0('b.Age', seq(fage,lage)))
  # Recruit is sum of brood year return by age
  if(nages == 1){
    brood$R <- brood[, -c(1:2)]
  } else {
    brood$R <- rowSums(brood[, -c(1:2)])
  }
  # Create SR data
  # SR <- brood[complete.cases(brood),c('byr','S','R')]
  # out <- list(brood=brood,SR=SR)
  # Output data is a list data
  brood #return(out)
}

# Backwards compatibility
make.brood <- make_brood

