## The codes are ordered chronologically. 

library(patchwork)


###~~~~~~~~~~ Population Biomass, Density, Sex ratio, and their relationship ~~~~~~~~###

## comparison arrows for ANOVA
arrow_p <- function(data){
  plot(data, comparisons = T) + 
    theme_bw() + 
    theme(text=element_text(size=20))+
    coord_flip() +
    #  xlab("Population biomass (g/m2)") +
    ylab("Crop ID")
}

arrow_p <- function(data){
plot(data, comparisons = TRUE, by = "Corn_weed_management") + 
  theme_bw() + 
  theme(text=element_text(size=10),
        strip.text.x = element_text(size = 5))+
  coord_flip() +
  geom_vline(xintercept=0.5, size=0.5, linetype="dashed") + 
  #ggtitle(expression((A)~1~plant/m^2))  +
  xlab("Female proportion") +
  ylab("Crop ID") 
}
## ## comparison arrows for ANOVA with density covar

arrow_p_dens <- function(data){
  plot(data, comparisons = TRUE, by = "Corn_weed_management") + 
    theme_bw() + 
    theme(text=element_text(size=10),
          strip.text.x = element_text(size = 5))+
    coord_flip() +
    geom_vline(xintercept=0.5, size=0.5, linetype="dashed") + 
    #ggtitle(expression((A)~1~plant/m^2))  +
    xlab("Female proportion") +
    ylab("Crop ID") 
}

#print p-values as is  


## scatter not working yet
scatter_p <- function(data, x, y, z, t){
  ggplot(prop_biom18, aes(x, y)) + 
    geom_point(aes(col = z, size = 3))+
    geom_smooth(aes(fill=z, col = z)) +
    facet_wrap(t, scales = "free_x") +
    theme_bw() +
    theme(text=element_text(size=15))+
    scale_size(guide=FALSE)
}

#add points from the testing set with another geom_point

# visual evaluation  https://rpubs.com/therimalaya/43190



## for pooled analysis of imputed data sets, without covariate    
# Chi-squared is not appropriate for quasibinomial, F-test is
mod_type1 = function(dat) {
  mod = glm(formula = cbind(Female, Male) ~ Block + Crop_ID + Corn_weed_management + Crop_ID:Corn_weed_management,
            data=dat, family=quasibinomial(link = "logit"))
  anova(mod,  test = "F")[-1,]
}

#Type 3 ANOVA 
mod_jt = function(dat) {
  mod = glm(formula = cbind(Female, Male) ~ Block + Crop_ID + Corn_weed_management + Crop_ID:Corn_weed_management,
            data=dat, family=quasibinomial(link = "logit"))
  # emm = emmeans(mod, c("Crop_ID", "Corn_weed_management", "Block"),data = dat, type = "response")
  joint_tests(mod, test = "F")
}

# summary for deviance and overdispersion inspection  

mod_sum = function(dat) {
  mod = glm(formula = cbind(Female, Male) ~ Block + Crop_ID + Corn_weed_management + Crop_ID:Corn_weed_management,
            data=dat, family=quasibinomial(link = "logit"))
  # emm = emmeans(mod, c("Crop_ID", "Corn_weed_management", "Block"),data = dat, type = "response")
  data.frame(Deviance  = c(summary(mod)$deviance),
             Dispersion = c(summary(mod)$dispersion))
}

# <https://stackoverflow.com/questions/57373775/how-to-save-the-output-summarymodel-of-generalized-linear-model-as-a-csv-fi> 


### follow-up emm

mod_emm_x = function(dat) {
  mod = glm(formula = cbind(Female, Male) ~ Block + Crop_ID + Corn_weed_management + Crop_ID:Corn_weed_management,
            data=dat, family=quasibinomial(link = "logit"))
  print(emmeans(mod, c("Crop_ID", "Corn_weed_management"), data = dat, type = "response", infer = c(T,T)), export = T)
}

# emm averaged over corn herbicide regimes
mod_emm_h = function(dat) {
  mod = glm(formula = cbind(Female, Male) ~ Block + Crop_ID + Corn_weed_management + Crop_ID:Corn_weed_management,
            data=dat, family=quasibinomial(link = "logit"))
  emmeans(mod, "Crop_ID" ,data = dat, type = "response", infer = c(T,T))
}



### grid  
mod_grid = function(dat) {
  mod = glm(formula = cbind(Female, Male) ~ Block + Crop_ID + Corn_weed_management + Crop_ID:Corn_weed_management,
            data=dat, family=binomial)
  m_grid = ref_grid(mod)
  grid_crop = add_grouping(m_grid, "Crop", "Crop_ID", Crop)
  emmeans(grid_crop, c("Crop", "Corn_weed_management"), type = "response", infer = c(T,T))
}

mod_pwpp = function(dat) {
  mod = glm(formula = cbind(Female, Male) ~ Block + Crop_ID + Corn_weed_management + Crop_ID:Corn_weed_management,
            data=dat, family=binomial)
  m_grid = ref_grid(mod)
  grid_crop = add_grouping(m_grid, "Crop", "Crop_ID", Crop)
  grid_crop_emm = emmeans(grid_crop, c("Crop", "Corn_weed_management"), type = "response")
  pwpp(grid_crop_emm, type ="response", by = "Corn_weed_management" , comparisons = TRUE)
}




###~~~~~~~~~~ Individual biomass, fecundity and their relationship ~~~~~~~~###





#### Appendix C: Diagnosis plots in ggplot2 style. 

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

## Diagnose a gls model to assess the effects of crop identity and corn weed management on individual biomass 
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

### Extract ANOVA tables for all imputed data sets. This function was used to identify which imputed data sets allow for estimation of corn weed management effect on sex ratio (full sets, denoted "n" in Appendix B). ANOVA tables from the full sets have 5 columns (source of variation, df1, df2, F-value, and p-value), while ANOVA tables from the not full sets have an additional column called Note. 
sexr19_dens_glm3  = function(dat) {
  mod = glm(cbind(Female, Male) ~ log(plant_m_sq + 0.025)  + Block +
              Crop_ID + Corn_weed_management +
              Crop_ID:Corn_weed_management + 
              log(plant_m_sq + 0.025):Crop_ID +
              log(plant_m_sq + 0.025):Corn_weed_management +
              log(plant_m_sq + 0.025):Crop_ID:Corn_weed_management ,
            data=dat, family=quasibinomial(link = "logit"))
  joint_tests(mod, test = "F")
}

## Table 11 - Appendix B: Numeric diagnosis for pooled analysis of imputed data sets, with population density covariate 

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

### Extract ANOVA tables for all imputed data sets. This function was used to identify which imputed data sets allow for estimation of corn weed management effect on sex ratio (full sets, denoted "n" in Appendix B). ANOVA tables from the full sets have 5 columns (source of variation, df1, df2, F-value, and p-value), while ANOVA tables from the not full sets have an additional column called Note.

#### As of the current data size, community density is a better covariate than community cumulative aboveground mass in estimating sex ratio at different treatments

## non-zero adjustment of half the smallest non-zero values in plant_m_sq column

sexr19_biom_glm3 = function(dat){
  mod =  glm(cbind(Female, Male) ~ log(g_m_sq + 0.0025) + Block +
               Crop_ID + Corn_weed_management +
               Crop_ID:Corn_weed_management + 
               log(g_m_sq + 0.002):Crop_ID +
               log(g_m_sq + 0.002):Corn_weed_management +
               log(g_m_sq + 0.002):Crop_ID:Corn_weed_management,
             data=dat, family=quasibinomial(link = "logit"))
  joint_tests(mod, test = "F")
}


