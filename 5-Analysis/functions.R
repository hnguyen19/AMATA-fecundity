## The codes are ordered chronologically. 

library(patchwork)
library(emmeans)

###~~~~~~~~~~ File name: Population-sex-biom-dens.Rmd ~~~~~~~~###
### Effects of crop ID, corn weed management, population aboveground mass, population density, and their interaction (when applicable) on population sex ratio. 

### Figure 2 was made with a customized function based on emmeans::plot() 
arrow_all <- function(data){
  plot(data, comparisons = TRUE) + 
    theme_bw() + 
    theme(text=element_text(size=10))+
    coord_flip() +
    ylab("Crop ID") # xlab to be filled at each plot
}


### Figure 3 was made with a customized function based on emmeans::plot() 
arrow_split <- function(data){
  plot(data, comparisons = TRUE, by = "Corn_weed_management") +  #facet by corn_weed_management
    theme_bw() + 
    theme(text=element_text(size=10),
          strip.text.x = element_text(size = 5))+
    coord_flip() +
    geom_vline(xintercept=0.5, size=0.5, linetype="dashed") + 
    xlab("Female proportion") +
    ylab("Crop ID") + 
    facet_wrap(~ Corn_weed_management, 
               labeller = custom_labels, strip.position = "right", ncol = 1) #remove "_" in "corn_weed_management"
}


### Type 3 ANOVA  ###
#Figure 4, panel A: to test the effects of crop identity and corn weed management on sex ratio using 24 imputed data sets 
mod_jt = function(dat) {
  mod = glm(formula = cbind(Female, Male) ~ Block +
              Crop_ID*Corn_weed_management,
            data=dat, family=quasibinomial(link = "logit"))
  # emm = emmeans(mod, c("Crop_ID", "Corn_weed_management", "Block"), data = dat, type = "response")
  joint_tests(mod, test = "F")
}

# Figure 4, panel B: to test the effects of crop identity and corn weed management on sex ratio using 24 imputed data sets 
#A customized function from emmeans::emmeans()
mod_emm_crop_ID = function(dat) {
  mod = glm(formula = cbind(Female, Male) ~ Block + 
              Crop_ID*Corn_weed_management,
            data=dat, family=quasibinomial(link = "logit"))
  emmeans(mod, "Crop_ID" ,  #mean sex ratio at each Crop_ID, averaged over corn_weed_management.
          data = dat,
          type = "response",
          infer = c(TRUE,TRUE))
}


###~~~~~~~~~~ File name: sex-29-imputation-jan29-22.Rmd  ~~~~~~~~~~###
## Appendix B: 2019 sex ratio imputation
## Extract ANOVA tables for all imputed data sets by applying `mod_jt` on all the sets. This function was used to identify which imputed data sets allow for estimation of corn weed management effect on sex ratio (full sets, denoted "n" in Appendix B). ANOVA tables from the full sets have 5 columns (source of variation, df1, df2, F-value, and p-value), while ANOVA tables from the not full sets have an additional column called Note. 

## Table 11: Numeric diagnosis for pooled analysis of imputed data sets, with no  covariate 

mod_sum = function(dat) {
  mod = glm(formula = cbind(Female, Male) ~ Block +
              Crop_ID*Corn_weed_management,
            data=dat, family=quasibinomial(link = "logit"))
  # emm = emmeans(mod, c("Crop_ID", "Corn_weed_management", "Block"),data = dat, type = "response")
  data.frame(Deviance  = c(summary(mod)$deviance),
             Dispersion = c(summary(mod)$dispersion))
}

## Table 11: Numeric diagnosis for pooled analysis of imputed data sets, with population density covariate 

sexr19_dens_sum  = function(dat) {
  mod = glm(cbind(Female, Male) ~ log(plant_m_sq + 0.025)  + Block + Crop_ID + Corn_weed_management +
              Crop_ID:Corn_weed_management + 
              log(plant_m_sq + 0.025):Crop_ID +
              log(plant_m_sq + 0.025):Corn_weed_management +
              log(plant_m_sq + 0.025):Crop_ID:Corn_weed_management ,
            data=dat, family=quasibinomial(link = "logit"))
  data.frame(Deviance  = c(summary(mod)$deviance), 
             Dispersion = c(summary(mod)$dispersion))
}  

## for pooled analysis of imputed data sets, with population biomass covariate  

## Table 11 - Appendix B: Numeric diagnosis for pooled analysis of imputed data sets, with population aboveground mass covariate 
## non-zero adjustment of half the smallest non-zero values in g_m_sq column

sexr19_biom_sum  = function(dat) {
  mod =  glm(cbind(Female, Male) ~ log(g_m_sq + 0.002)  + Block +
               Crop_ID + Corn_weed_management +
               Crop_ID:Corn_weed_management + 
               log(g_m_sq + 0.002):Crop_ID +
               log(g_m_sq + 0.002):Corn_weed_management +
               log(g_m_sq + 0.002):Crop_ID:Corn_weed_management, 
             data=dat,family=quasibinomial(link = "logit"))
  data.frame(Deviance  = c(summary(mod)$deviance), 
             Dispersion = c(summary(mod)$dispersion))
}


###~~~~~~~~~~ Appendix C: Diagnosis plots in ggplot2 style. ~~~~~~~~~~###
## Appendix C is not included in the manuscript, but presented here for model diagnosis reference 

### gls data here need to be augmented first 
# predicted vs residual 
#https://www.datanovia.com/en/blog/ggplot-title-subtitle-and-caption/

## Diagnose a gls model to assess the effects of crop identity and corn weed management on individual fecundity (number of seeds)
diag_seed <- function(data, tag){
  aug_data <- augment(data)
  fit_resid <-  ggplot(aug_data, aes(x =  .fitted, y = .resid)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm",  size = 0.5, se = FALSE) +
    xlab("Predicted values") +
    ylab("Residuals") +
    ggtitle(label = tag) + 
    theme_bw()+
    theme(text=element_text(size=20))  +
    theme(plot.title = element_text(face = "bold")) 
    #labs(tag = tag, face = "bold")
  
  obs_resid <-  ggplot(aug_data, aes(x = log(Seed + 1) , y = .resid)) +
    geom_point(size = 3) +
    geom_smooth( method = "lm", size = 0.5, se = F) +
    xlab("Observation Number") + 
    ylab("Residuals") +
    ggtitle(label = "", subtitle ="Index plot") +
    theme_bw()+
    theme(text=element_text(size=20))  +
    theme(plot.title = element_text(face = "bold")) 
  
  resid_h <-  ggplot(aug_data, aes( x = .resid)) +
    geom_histogram( aes(y = ..density..), binwidth = 0.4, color = "black", fill = "grey") +
    # geom_density() + 
    xlab("Residuals") + 
    ylab("Density") + 
    stat_function(fun = dnorm, color = "blue", size = 0.5) + 
  #  ggtitle(label = "", subtitle ="Histogram") +
    theme_bw()+
    theme(text=element_text(size=20))  +
    theme(plot.title = element_text(face = "bold")) 
  
  qq <-  ggplot(aug_data, aes(sample = log(Seed+1))) +
    stat_qq(size = 3) + 
    stat_qq_line(color = "blue", size = 0.5) + 
    xlab("Theoretical Quantiles") + 
    ylab("Sample Quantiles") + 
  #  ggtitle(label = "", subtitle ="Q-Q plot") +
    theme_bw()+
    theme(text=element_text(size=20))  +
    theme(plot.title = element_text(face = "bold")) 
  
 return((fit_resid / qq / obs_resid / resid_h)) 
 
}

## Diagnose a gls model used to assess the effects of crop identity and corn weed management on individual biomass 
diag_biom <- function(data, tag){
  aug_data <- augment(data)
  fit_resid <-  ggplot(aug_data, aes(x =  .fitted, y = .resid)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm",  size = 0.5, se = F) +
    xlab("Predicted values") +
    ylab("Residuals") +
     ggtitle(label = tag) + 
    theme_bw()+
    theme(text=element_text(size=20))  +
    theme(plot.title = element_text(face = "bold")) 
   # labs(tag = tag)
  
  obs_resid <-  ggplot(aug_data, aes(x = log(Biomass + 0.005) , y = .resid)) +
    geom_point(size = 3) +
    geom_smooth( method = "lm", size = 0.5, se = F) +
    xlab("Observation Number") + 
    ylab("Residuals") +
    ggtitle(label = "", subtitle ="Index plot") +
    theme_bw()+
    theme(text=element_text(size=20))  +
    theme(plot.title = element_text(face = "bold")) 
  
  resid_h <-  ggplot(aug_data, aes( x = .resid)) +
    geom_histogram( aes(y = ..density..), binwidth = 0.4, color = "black", fill = "grey") +
    # geom_density() + 
    xlab("Residuals") + 
    ylab("Density") + 
    stat_function(fun = dnorm, color = "blue", size = 0.5) + 
    #  ggtitle(label = "", subtitle ="Histogram") +
    theme_bw()+
    theme(text=element_text(size=20))  +
    theme(plot.title = element_text(face = "bold")) 
  
  qq <-  ggplot(aug_data, aes(sample = log(Seed+1))) +
    stat_qq(size = 3) + 
    stat_qq_line(color = "blue", size = 0.5) + 
    xlab("Theoretical Quantiles") + 
    ylab("Sample Quantiles") + 
    #  ggtitle(label = "", subtitle ="Q-Q plot") +
    theme_bw()+
    theme(text=element_text(size=20))  +
    theme(plot.title = element_text(face = "bold")) 
  return((fit_resid / qq / obs_resid / resid_h)) 
}

