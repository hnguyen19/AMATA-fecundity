## The codes are ordered by applicability: for the whole manuscript first, then each section. The sectional codes are organized by the sectional order in the manuscript

#################################### Whole manuscript ####################################

## Code for tables customization
#need to replace : with x
index_tab <- function(dat, cap){
  print(dat, export = T) %>%
    kable(booktabs = T,
          linesep = "",
        #  digits = 4,
          caption = cap)
} #add "latex" inside kable if needed, but latex is not supported for latex now

## Diagnosis plots 
### for lm
fit_resid1 <- function(data){
  ggplot(,aes(x=fitted(data), y=residuals(data))) +
    geom_point() +  
    geom_smooth(method = "lm") +
    xlab("Predicted value") +
    ylab("Residuals")}



### for gls data here need to be augmented first 
# predicted vs residual 
#https://www.datanovia.com/en/blog/ggplot-title-subtitle-and-caption/

#replace title with tag and bold it?
require(patchwork)
diag_seed <- function(data, tag){
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



################################# Sectional  #####################################

###~~~~~~~~~~ Individual Biomass, Fecundity and their relationship ~~~~~~~~###

## custom plot function for biomass vs fecundity relationship 
train_p <- function(data){
  ggplot(data, aes(y = log_Seed, x = log_Biomass)) + 
  geom_point(size = 3) +   # points from the training set
  geom_smooth(method = "lm") + 
    theme_bw()+
    xlab("Biomass index") +   
    ylab("Fecundity index")
  #facet_wrap(~ group, ncol = 2) +
  # facet_grid(Group ~ Tercile.f) +
}

lm_eqn <- function(data){
  m <- lm(y ~ x, data);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
} #from <https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph>

train_p2 <- function(data){
  ggplot(data, aes(y = log_Seed, x = log_Biomass)) + 
    geom_point() +   # points from the training set
    geom_smooth(method = "lm") + 
    theme_bw()+
    geom_text(label = lm_eqn(data), parse = TRUE) +
    xlab("Biomass index") +   
    ylab("Fecundity index")
  #facet_wrap(~ group, ncol = 2) +
  # facet_grid(Group ~ Tercile.f) +
}

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

## ## comparison arrows for ANOVA with density covar

arrow_p_dens <- function(data){
  plot(data, comparisons = T, by = "Corn_weed_management") + 
    geom_vline(xintercept = 0.5, linetype = "dashed", size=1.5) + 
    theme_bw() + 
    theme(text=element_text(size=25))+
    coord_flip() +
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

## for pooled analysis of imputed data sets, with population biomass covariate  

## Summary, to extract Deviance and Dispersion information
sexr19_biom_sum  = function(dat) {
  mod =  glm(cbind(Female, Male) ~ sqrt(g_m_sq)  + Block + Crop_ID + Corn_weed_management +
               Crop_ID:Corn_weed_management + 
               sqrt(g_m_sq):Crop_ID +
               sqrt(g_m_sq):Corn_weed_management +
               sqrt(g_m_sq):Crop_ID:Corn_weed_management, 
  data=dat,family=quasibinomial(link = "logit"))
data.frame(Deviance  = c(summary(mod)$deviance), 
           Dispersion = c(summary(mod)$dispersion))
}

## Type III ANOVA

sexr19_biom_glm3 = function(dat){
 mod =  glm(cbind(Female, Male) ~ sqrt(g_m_sq)  + Block + Crop_ID + Corn_weed_management +
              Crop_ID:Corn_weed_management + 
              sqrt(g_m_sq):Crop_ID +
              sqrt(g_m_sq):Corn_weed_management +
              sqrt(g_m_sq):Crop_ID:Corn_weed_management,
      data=dat, family=quasibinomial(link = "logit"))
 joint_tests(mod, test = "F")
}


## for pooled analysis of imputed data sets, with population density covariate   

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


sexr19_dens_glm3  = function(dat) {
  mod = glm(cbind(Female, Male) ~ log(plant_m_sq + 0.025)  + Block + Crop_ID + Corn_weed_management +
              Crop_ID:Corn_weed_management + 
              log(plant_m_sq + 0.025):Crop_ID +
              log(plant_m_sq + 0.025):Corn_weed_management +
              log(plant_m_sq + 0.025):Crop_ID:Corn_weed_management ,
            data=dat, family=quasibinomial(link = "logit"))
  joint_tests(mod, test = "F")
}
