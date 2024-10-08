#' A function to create automatically output to report stats results in publications
#'
#' This function returns a data frame, in which, for each effect, a text output containing the results to export in the publications are produced.
#'
#' @param model_data For "mainef_anova", "chisq", "mainef_Anova" and "summary" methods, put the statistical model. For the "emmeans" method, create an object as: your_model$pairwise..., and in a second step, convert this model in data frame using the as.data.frame() function and use this transformed model as argument.
#' @param method Statistical method employed: either "mainef_anova", in this case an anova(model_data) will be performed, "chisq", then the results of the model will be used, "mainef_Anova" the Anova(model_data) from the car package will be performed. The "summary" method uses the summary(model_data).
#' Some methods are implemented only for certain models. *mainef_anova* is implemented only for lm() and lmer() models. *chisq* only supports models from the chisq.test() function. *emmeans* supports only emmeans() models after transformation as described above. *mainef_Anova* supports only glmer() models. *summary* supports lm(), lmer(), and glmer() models.
#' @export
#'

report_results <- function(model_data, method){
  #require(lme4)

  # Method anova (model) -> supports lm and lmer
  if(method == "mainef_anova"){
    anova_table <- anova(model_data)
    if(typeof(model_data) == "S4"){
      if(isGLMM(model_data) == TRUE){
        return("Only lm and lmer models are implemented in this function for the mainef_anova method. Try to use mainef_Anova as method.")
      } else {
      effects <- paste("F(", round(anova_table$NumDF,2),",",round(anova_table$DenDF,2), ") = ", round(anova_table$`F value`,2), "; p ", sep = "")
      names_eff <- rownames(anova_table)
      output <- data.frame(names_eff, effects)
      output$p <- round(anova_table$`Pr(>F)`,3)
      output$p <- ifelse(output$p < 0.001, "<0.001", paste("= ",output$p, sep = ""))
      output$effects <- paste(output$effects, output$p, sep = "")
      output$p <- NULL
      return(output)
      }
    } else {
      effects <- paste("F(", round(anova_table$Df,2),") = ", round(anova_table$`F value`,2), "; p ", sep = "")
      names_eff <- rownames(anova_table)
      output <- data.frame(names_eff, effects)
      output$p <- round(anova_table$`Pr(>F)`,3)
      output$p <- ifelse(output$p == 0.000, "<0.001",  paste("= ",output$p, sep = ""))
      output$effects <- paste(output$effects, output$p, sep = "")
      output$p <- NULL
      return(output)
    }

    # Method "chisq" supports chisq.test() models
  }else if(method == "chisq"){
    if(model_data$p.value < 0.001){
      return(paste("X2 = ",round(model_data$statistic,2), "; p<0.001", sep = ""))
    } else {
      return(paste( "X2 = ",round(model_data$statistic,2), "; p = ", round(model_data$p.value,3), sep = ""))
    }

    # Method "emmeans" supports as.data.frame(emmeans_model$pairwise...)
  }else if(method == "emmeans"){
    post_hoc <- model_data
    contrasts_index <- grep("estimate", colnames(post_hoc))-1
    output <- data.frame(post_hoc[,1:contrasts_index])
    if(sum(grepl("t.ratio", colnames(post_hoc))) == 1){
      output$report <- paste("t(", round(post_hoc$df,2),") = ", round(post_hoc$t.ratio,2), "; p", sep = "")
      output$p <- post_hoc$p.value
      output$p <- ifelse(post_hoc$p.value < 0.001, "<0.001", paste("= ",round(post_hoc$p.value,3)))
      output$report <- paste(output$report, output$p)
      output$p <- NULL
      return(output)
    } else if(sum(grepl("z.ratio", colnames(post_hoc))) == 1){
      output$report <- paste("z = ", round(post_hoc$z.ratio,2), "; p", sep = "")
      output$p <- post_hoc$p.value
      output$p <- ifelse(post_hoc$p.value < 0.001, "<0.001", paste("= ", round(post_hoc$p.value,3), sep = ""))
      output$report <- paste(output$report, output$p)
      output$p <- NULL
      return(output)
    } else{
      print("Unknown format, only results from z and t distributions are available")
    }

    # Method "mainef_Anova" supports only glmer() models
  }else if(method == "mainef_Anova"){
    if(typeof(model_data) == "S4"){
      if(isLMM(model_data) == TRUE){
        return("Only glmer models are implemented in this function for the mainef_Anova method. Try to use mainef_anova as method.")
      } else {
    require(car)
    Anova_model <- as.data.frame(Anova(model_data))
    effects <- rownames(Anova_model)
    output <- data.frame(effects)
    output$report <- paste("X2 = ", round(Anova_model$Chisq,2), "; p", sep = "")
    output$p <- ifelse(round(Anova_model$`Pr(>Chisq)`,3) < 0.001, "<0.001", paste("= ", round(Anova_model$`Pr(>Chisq)`,3), sep = ""))
    output$report <- paste(output$report, output$p)
    output$p <- NULL
    return(output)
      }
    } else {
      return("Only glmer models are implemented in this function for the mainef_Anova method. Try to use mainef_anova as method.")
    }

    # Method "summary" supports lm(), lmer() and glmer() models
  } else if(method == "summary"){
    if(typeof(model_data) == "S4"){
      summary_model <- summary(model_data)
      coefs <- as.data.frame(summary_model$coefficients)
      effects <- rownames(coefs)
      output <- data.frame(effects)
      if(isLMM(model_data) == TRUE){
      output$report <- paste("t(", round(coefs$df,2), ") = ", round(coefs$`t value`,2), "; p ", sep = "")
      output$p <- ifelse(round(coefs$`Pr(>|t|)`,3) < 0.001, "<0.001", paste("= ", round(coefs$`Pr(>|t|)`,3), sep = ""))
      output$report <- paste(output$report, output$p, sep = "")
      output$p <- NULL
      return(output)
      } else {
      output$report <- paste("z = ", round(coefs$`z value`,2), "; p ", sep = "")
      output$p <- ifelse(round(coefs$`Pr(>|z|)`, 3) < 0.001, "<0.001", paste("= ", round(coefs$`Pr(>|z|)`,3), sep = ""))
      output$report <- paste(output$report, output$p, sep = "")
      output$p <- NULL
      return(output)
      }
    } else {
    summary_model <- summary(model_data)
    coefs <- as.data.frame(summary_model$coefficients)
    effects <- rownames(coefs)
    output <- data.frame(effects)
    output$report <- paste("t = ", round(coefs$`t value`,2), "; p ", sep = "")
    output$p <- ifelse(round(coefs$`Pr(>|t|)`,3) < 0.001, "<0.001", paste("= ", round(coefs$`Pr(>|t|)`,3), sep = ""))
    output$report <- paste(output$report, output$p, sep = "")
    output$p <- NULL
    return(output)
    }
    # If the method is not one of these:
  } else{
    return("Non-supported method. Try either mainef_anova, mainef_Anova, summary, emmeans, or chisq")

  }
}
