#' Title
#'
#' @param dataset the dataset frame containing robust weights
#' @param model an object of class rlmerMod
#'
#' @return exploratory graphs, DV as a function of IV and weights
#' @export
plot_weights<-function(dataset, model){
  random.labels=names(ranef(model))
  cnames<-NULL

  for(i in 1:length(random.labels)){
    randomlab<-random.labels[i]
    random_i<-cbind(levels(model@frame[,randomlab]),as.data.frame(getME(model,"w_b")[i]))
    cnames_i<-paste0("W_",randomlab)
    colnames(random_i)<-c(randomlab,paste0("W_",randomlab))
    cnames<-c(cnames,cnames_i)
  }

  col_selected1 <- setdiff(colnames(model@frame),random.labels)
  col_selected2 <- setdiff(col_selected1,as.character(formula(model)[[2]]))
  IV_supp <- data.frame(model@frame[,c(col_selected2)])
  DV =  model@frame[,as.character(formula(model)[[2]])]
  cat_i = data.frame(model@frame[,random.labels])
  weighting_var = data.frame(dataset[,c(cnames,"W_e")])

  if(ncol(IV_supp)>0){
    for(i in 1: dim(IV_supp)[2]){
      if(is.factor(IV_supp[,i])){
        for(j in 1:dim(weighting_var)[2]){
          for(k in 1:dim(cat_i)[2]){

            dataplot<-data.frame(
              X =cat_i[,k],
              W = weighting_var[,j],
              panel_var = IV_supp[,i])

            W_names = c(cnames,"W_e")
            titleplot = paste0(as.character(formula(model)[[2]])," by ",col_selected2[i],", ",random.labels[k]," and ",W_names[j])

            gg<-ggplot(dataplot,aes(x=X,y=DV,color=W)) +
              geom_jitter()+
              geom_violin(trim=F)+
              geom_boxplot(outlier.shape=NA,width=0.5)+
              facet_grid(.~panel_var)+
              scale_color_continuous(limits = c(0,1), guide = guide_legend(reverse = T))+
              ylab(as.character(formula(model)[[2]]))+
              xlab(random.labels[k])+
              ggtitle(titleplot)+
              theme(legend.position = "bottom",
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.text.x =element_text(size = 12),
                    axis.text.y.left = element_text(size = 12),
                    axis.title.x = element_text(size=12),
                    plot.title = element_text(size=12),
                    strip.text.y = element_text(size = 12),
                    panel.background = element_rect(fill = "white",
                                                    colour = "grey26",
                                                    size = 0.5, linetype = "solid"))

            print(gg)
            readline(prompt = "Press [Enter] to continue :")
          }
        }
      }else{
        for(j in 1:dim(weighting_var)[2]){
          for(k in 1:dim(cat_i)[2]){
            dataplot<-data.frame(
              panel_var =cat_i[,k],
              W = weighting_var[,j],
              X = IV_supp[,i])

            W_names = c(cnames,"W_e")
            titleplot = paste0(as.character(formula(model)[[2]])," by ",col_selected2[i],", ",random.labels[k]," and ",W_names[j])


            gg<-ggplot(dataplot,aes(x=X,y=DV,color=W, group =panel_var)) +
              geom_line()+
              geom_point()+
              scale_color_continuous(limits = c(0,1), guide = guide_legend(reverse = T))+
              ylab(as.character(formula(model)[[2]]))+
              xlab(col_selected2[i])+
              ggtitle(titleplot)+
              theme(legend.position = "bottom",
                    axis.text.x =element_text(size=12),
                    axis.text.y.left = element_text(size = 12),
                    axis.title.x =element_text(size=12),
                    plot.title = element_text(size=12),
                    strip.text.y = element_text(size = 12),
                    panel.background = element_rect(fill = "white",
                                                    colour = "grey26",
                                                    size = 0.5, linetype = "solid"))

            print(gg)
            readline(prompt = "Press [Enter] to continue :")
          }
        }
      }
    }
  }else{
    for(j in 1:dim(weighting_var)[2]){
      for(k in 1:dim(cat_i)[2]){
        dataplot<-data.frame(
          X =cat_i[,k],
          W = weighting_var[,j])

        W_names = c(cnames,"W_e")
        titleplot = paste0(as.character(formula(model)[[2]])," by ",random.labels[k]," and ",W_names[j])


        gg<-ggplot(dataplot,aes(x=X,y=DV,color=W)) +
          geom_jitter()+
          geom_violin(trim=F)+
          geom_boxplot(outlier.shape=NA,width=0.5)+
          scale_color_continuous(limits = c(0,1), guide = guide_legend(reverse = T))+
          ggtitle(titleplot)+
          ylab(as.character(formula(model)[[2]]))+
          xlab(random.labels[k])+
          theme(legend.position = "bottom",
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text.x =element_text(size = 12),
                axis.text.y.left = element_text(size = 12),
                axis.title.x = element_text(size=12),
                plot.title = element_text(size=12),
                strip.text.y = element_text(size = 12),
                panel.background = element_rect(fill = "white",
                                                colour = "grey26",
                                                size = 0.5, linetype = "solid"))

        print(gg)
        readline(prompt = "Press [Enter] to continue :")
      }
    }
  }
}
