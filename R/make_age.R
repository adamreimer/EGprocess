#' @title Create Age Dataset
#'
#' @param agedata The age data
#' @param min_age Minimum age to assess
#' @param max_age Maximum age to assess
#' @param combine Determine whether to combine age data. Defaults to TRUE.
#'
#' @returns data frame
#'
#' @examples
#' make_age(agedata = data_Igushik, min.age = 3, max.age = 8)
#'
#' #' @export
make_age <- function(agedata, min_age, max_age, combine=TRUE){

  eage <- names(agedata)[substr(names(agedata), 1, 1) =='a']
  rage <- names(agedata)[substr(names(agedata), 1, 1) =='A']

  if(length(eage)>0){
    ac <- data.frame(t(agedata[,eage]))

    # Create European Age  fw.sw
    ac$eage <-as.numeric(substr(rownames(ac), 2, 5))

    # Convert European to Actual Age: freshwater age + seawater age + 1
    ac$age <- round(with(ac, floor(eage)+ 10*(eage-floor(eage)))+1)

  } else if(length(rage)>0){
    ac <- data.frame(t(agedata[rage]))
    ac$age <- round(as.numeric(substr(rownames(ac), 2, 3)))
  }
  # Combine of eliminate age
  if(isTRUE(combine)){
    ac$age <- with(ac, ifelse(age<min_age, min_age,ifelse(age >max_age, max_age, age)))
  } else {
    ac <- ac[which(ac$age>=min_age & ac$age<=max_age),]
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

make.age <- make_age
