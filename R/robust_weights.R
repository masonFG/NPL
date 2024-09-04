#' add the random effects and observation weights to the dataframe
#'
#' @param model_tested an object of class lmerMod or rlmerMod to define the model to test
#'
#' @return the dataframe with robust weights
#' @export
#'
robust_weights <- function(model_tested){
  if(!is(model_tested,"rlmerMod")){
    model_tested=rlmer(formula(model_tested), data = model_tested@frame)
  }
  randoms_length=seq_along(model_tested@blocks)
  bdd=model.frame(model_tested)
  bdd$Index <- as.numeric(rownames(bdd))
  cnames<-NULL
  random<-list()
  for(i in randoms_length){
    randomlab<-names(ranef(model_tested))[i]
    random_i<-cbind(levels(model_tested@frame[,randomlab]),as.data.frame(getME(model_tested,"w_b")[i]))
    cnames_i<-paste0("W_",randomlab)
    colnames(random_i)<-c(randomlab,paste0("W_",randomlab))
    cnames<-c(cnames,cnames_i)
    bdd=merge(bdd,random_i, by=randomlab)
  }

  frame_utilise <- model.frame(model_tested)

  W_e <- getME(model_tested, "w_e")

  frame_utilise$Index <- as.numeric(rownames(frame_utilise))

  frame_utilise$W_e <- W_e

  data_weights <- merge(bdd, frame_utilise[, c("Index", "W_e")], by = "Index", all.x = TRUE)

    return(data_weights)
}
