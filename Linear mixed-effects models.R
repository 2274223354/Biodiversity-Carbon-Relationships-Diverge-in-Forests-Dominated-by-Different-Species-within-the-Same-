##===============================================================================
## PD_Cunninghamia lanceolata:LMM ---------
##===============================================================================
readDir=c("E:/")
setwd(readDir)
rm(list=ls())
library(ggplot2)
library(dplyr)
library(forcats)
library(grid)  
library(scales)
library(tidyr)
library(lmerTest)
library(piecewiseSEM)
library(data.table)
library(pROC)
library(plyr)
library(doParallel)
library(foreign)
library(car)
library(MuMIn)
####Cunninghamia lanceolata PD：LMM_All####
rm(list=ls())

data_sem2=read.csv("Plot Cunninghamia lanceolata.csv" )
colnames( data_sem2 )




data_sem2[, c("AverageDBH"   , "Density", "AGC","DBH_CV")] <- lapply(
  data_sem2[, c("AverageDBH"  , "Density", "AGC","DBH_CV")],
  function(x) log(as.numeric(x))
)


data_sem2[, c( 4:20)] <- lapply(
  data_sem2[, c( 4:20)],
  function(x) as.numeric(scale(x))
)


data_sem2 <- as.data.frame(data_sem2)



colSums(is.na(data_sem2))
modall <- lmer(
  AGC ~ Age+AverageDBH + Density + DBH_CV +
    Shannon + PD + shan_Broadleaf +
    MAT + MAP +
    SoilThickness + HumusLayerThickness + 
    
    
    SoilThickness:Shannon +
    
    (1 | RegionName),
  
  data = data_sem2,
  REML = FALSE
)

summary(modall)
r.squaredGLMM(modall)
round(vif(modall), 2)

vif(modall) 

#####VIF#####

new_order <- c("AverageDBH", "DBH_CV", "Density", "Age", 
               "PD", "shan_Broadleaf", "Shannon", "MAT", "MAP",
               "SoilThickness", 
               "HumusLayerThickness",
               "Shannon:SoilThickness")


all(new_order %in% names(vif(modall)))  


vif_values <- vif(modall)[new_order]
vif_values


vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values,
  row.names = NULL  
)


vif_table


print(vif_table, digits = 2)

write.csv(vif_table, "VIF_All_310.csv")

####Extract model data####
library(broom)

model_avg <- summary(modall) 


coef_full_avg <- model_avg$coefficients


coef_table_full <- as.data.frame(coef_full_avg)


colnames(coef_table_full) <- c("Estimate", "Std.Error", "df"  , "t.value","Pr(>|t|)")
coef_table_full


write.csv(coef_table_full, "PD_Fixed_All_310.csv")

#################################################################################
rm(list=ls())
####Cunninghamia lanceolata PD：LMM_High####
rm(list=ls())

data_sem2=read.csv("Plot Cunninghamia lanceolata.csv" ) 
colnames(data_sem2)
data_sem2 = data_sem2%>% filter(Group %in% c("2"))



data_sem2[, c("AverageDBH"   , "Density", "AGC","DBH_CV")] <- lapply(
  data_sem2[, c("AverageDBH"  , "Density", "AGC","DBH_CV")],
  function(x) log(as.numeric(x))
)


data_sem2[, c( 4:20)] <- lapply(
  data_sem2[, c( 4:20)],
  function(x) as.numeric(scale(x))
)


data_sem2 <- as.data.frame(data_sem2)



colSums(is.na(data_sem2))
modall <- lmer(
  AGC ~ AverageDBH + Density + DBH_CV +
    Shannon + PD + shan_Broadleaf +
    MAT + MAP +
    SoilThickness + HumusLayerThickness + 
    
    
    SoilThickness:Shannon +
    
    (1 | RegionName),
  
  data = data_sem2,
  REML = FALSE
)

summary(modall)
r.squaredGLMM(modall)
round(vif(modall), 2)

vif(modall) 

#####VIF#####

new_order <- c("AverageDBH", "DBH_CV", "Density", "Age", 
               "PD", "shan_Broadleaf", "Shannon", "MAT", "MAP",
               "SoilThickness", 
               "HumusLayerThickness",
               "Shannon:SoilThickness")


all(new_order %in% names(vif(modall)))  


vif_values <- vif(modall)[new_order]
vif_values


vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values,
  row.names = NULL  
)


vif_table


print(vif_table, digits = 2)

write.csv(vif_table, "VIF_All_310.csv")

####Extract model data####
library(broom)

model_avg <- summary(modall) 


coef_full_avg <- model_avg$coefficients


coef_table_full <- as.data.frame(coef_full_avg)


colnames(coef_table_full) <- c("Estimate", "Std.Error", "df"  , "t.value","Pr(>|t|)")
coef_table_full



write.csv(coef_table_full, "PD_Fixed_High_310.csv")

#################################################################################
####Cunninghamia lanceolata PD：LMM_Low####
rm(list=ls())

data_sem2=read.csv("Plot Cunninghamia lanceolata.csv" )
colnames(data_sem2)
data_sem2 = data_sem2%>% filter(Group %in% c("1"))



data_sem2[, c("AverageDBH"   , "Density", "AGC","DBH_CV")] <- lapply(
  data_sem2[, c("AverageDBH"  , "Density", "AGC","DBH_CV")],
  function(x) log(as.numeric(x))
)


data_sem2[, c( 4:20)] <- lapply(
  data_sem2[, c( 4:20)],
  function(x) as.numeric(scale(x))
)


data_sem2 <- as.data.frame(data_sem2)


colSums(is.na(data_sem2))
modall <- lmer(
  AGC ~ AverageDBH + Density + DBH_CV +
    Shannon + PD + shan_Broadleaf +
    MAT + MAP +
    SoilThickness + HumusLayerThickness + 
    
    
    SoilThickness:Shannon +
    
    (1 | RegionName),
  
  data = data_sem2,
  REML = FALSE
)

summary(modall)
r.squaredGLMM(modall)
round(vif(modall), 2)

#####VIF#####

new_order <- c("AverageDBH", "DBH_CV", "Density", "Age", 
               "PD", "shan_Broadleaf", "Shannon", "MAT", "MAP",
               "SoilThickness", 
               "HumusLayerThickness",
               "Shannon:SoilThickness")


all(new_order %in% names(vif(modall)))  


vif_values <- vif(modall)[new_order]
vif_values


vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values,
  row.names = NULL  
)


vif_table


print(vif_table, digits = 2)

write.csv(vif_table, "VIF_All_310.csv")

####Extract model data####
library(broom)

model_avg <- summary(modall) 


coef_full_avg <- model_avg$coefficients


coef_table_full <- as.data.frame(coef_full_avg)


colnames(coef_table_full) <- c("Estimate", "Std.Error", "df"  , "t.value","Pr(>|t|)")
coef_table_full


write.csv(coef_table_full, "PD_Fixed_Low_310.csv")

##===============================================================================
## PD_Pinus massoniana:LMM ---------
##===============================================================================
####Pinus massoniana PD：LMM_All####
data_sem2=read.csv("Plot Pinus massoniana.csv" )


data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV")] <- lapply(
  data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV")],
  function(x) log(as.numeric(x))
)

colnames(data_sem2  )


data_sem2[, c(4:20)] <- lapply(
  data_sem2[, c(4:20)],
  function(x) as.numeric(scale(x))
)



modall <- lmer(
  AGC ~ Age+AverageDBH + 
    Density + DBH_CV +
    Shannon + PD + shan_Broadleaf +
    MAT + MAP +
    SoilThickness + HumusLayerThickness + 
    
    
    SoilThickness:Shannon +
    
    (1 | RegionName),
  
  data = data_sem2,
  REML = FALSE
)

summary(modall)
r.squaredGLMM(modall)
round(vif(modall), 2)

max(vif(modall) )
#####VIF#####

new_order <- c("AverageDBH", "DBH_CV", "Density", "Age", 
               "PD", "shan_Broadleaf", "Shannon", "MAT", "MAP",
               "SoilThickness", 
               "HumusLayerThickness",
               "Shannon:SoilThickness")


all(new_order %in% names(vif(modall)))  


vif_values <- vif(modall)[new_order]
vif_values


vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values,
  row.names = NULL  
)


vif_table


print(vif_table, digits = 2)

write.csv(vif_table, "VIF_All_310.csv")

####Extract model data####
library(broom)

model_avg <- summary(modall) 


coef_full_avg <- model_avg$coefficients


coef_table_full <- as.data.frame(coef_full_avg)


colnames(coef_table_full) <- c("Estimate", "Std.Error", "df"  , "t.value","Pr(>|t|)")
coef_table_full


write.csv(coef_table_full, "PD_Fixed_All_220.csv")

#################################################################################
####Pinus massoniana PD：LMM_High####
rm(list=ls())

data_sem2=read.csv("Plot Pinus massoniana.csv" )

data_sem2 = data_sem2%>% filter(Group %in% c("2"))


data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV" )] <- lapply(
  data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV" )],
  function(x) log(as.numeric(x))
)

colnames(data_sem2  )


data_sem2[, c(4:20)] <- lapply(
  data_sem2[, c(4:20)],
  function(x) as.numeric(scale(x))
)

modall <- lmer(
  AGC ~ AverageDBH + 
    Density + 
    DBH_CV +
    Shannon + PD + shan_Broadleaf +
    MAT + MAP +
    SoilThickness + HumusLayerThickness +
    SoilThickness:Shannon +           
    
    (1 | RegionName),
  
  data = data_sem2,
  REML = FALSE
)

summary(modall)
r.squaredGLMM(modall)
round(vif(modall), 2)

#####VIF#####

new_order <- c("AverageDBH", "DBH_CV", "Density", "Age", 
               "PD", "shan_Broadleaf", "Shannon", "MAT", "MAP",
               "SoilThickness", 
               "HumusLayerThickness",
               "Shannon:SoilThickness")


all(new_order %in% names(vif(modall)))  


vif_values <- vif(modall)[new_order]
vif_values


vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values,
  row.names = NULL  
)


vif_table


print(vif_table, digits = 2)

write.csv(vif_table, "VIF_All_310.csv")

####Extract model data####
library(broom)

model_avg <- summary(modall) 


coef_full_avg <- model_avg$coefficients


coef_table_full <- as.data.frame(coef_full_avg)


colnames(coef_table_full) <- c("Estimate", "Std.Error", "df"  , "t.value","Pr(>|t|)")
coef_table_full


write.csv(coef_table_full, "PD_Fixed_High_220.csv")

#################################################################################
####Pinus massoniana PD：LMM_Low####
rm(list=ls())
library(dplyr)
data_sem2=read.csv("Plot Pinus massoniana.csv" )
colnames(data_sem2)
data_sem2 = data_sem2%>% filter(Group %in% c("1"))


data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV")] <- lapply(
  data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV" )],
  function(x) log(as.numeric(x))
)


colnames(data_sem2  )


data_sem2[, c(4:20)] <- lapply(
  data_sem2[, c(4:20)],
  function(x) as.numeric(scale(x))
)

modall <- lmer(
  AGC ~ AverageDBH +
    Density + 
    DBH_CV +
    Shannon + PD + shan_Broadleaf +
    MAT + MAP +
    SoilThickness + HumusLayerThickness + 
    
    
    # SoilThickness:PD +
    SoilThickness:Shannon+
    
    
    (1 | RegionName),
  
  data = data_sem2,
  REML = FALSE
)


summary(modall)
r.squaredGLMM(modall)
round(vif(modall), 2)

#####VIF#####

new_order <- c("AverageDBH", "DBH_CV", "Density", "Age", 
               "PD", "shan_Broadleaf", "Shannon", "MAT", "MAP",
               "SoilThickness", 
               "HumusLayerThickness",
               "Shannon:SoilThickness")


all(new_order %in% names(vif(modall)))  


vif_values <- vif(modall)[new_order]
vif_values


vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values,
  row.names = NULL  
)


vif_table


print(vif_table, digits = 2)

write.csv(vif_table, "VIF_All_310.csv")

####Extract model data####
library(broom)

model_avg <- summary(modall) 


coef_full_avg <- model_avg$coefficients


coef_table_full <- as.data.frame(coef_full_avg)
colnames(coef_table_full) <- c("Estimate", "Std.Error", "df"  , "t.value","Pr(>|t|)")
coef_table_full

write.csv(coef_table_full, "PD_Fixed_Low_220.csv")

###################################################################################
##===============================================================================
## PD:LMMPlot ---------
##===============================================================================
#######Cunninghamia lanceolata：plot_all######
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(ggh4x)
library(dplyr)

df_All<-read.csv("PD_Fixed_All_310.csv",check.names = F)
df_High<-read.csv("PD_Fixed_High_310.csv",check.names = F)
df_Low<-read.csv("PD_Fixed_Low_310.csv",check.names = F)

df_All$Grid<-"All"
df_High$Grid<-"High"
df_Low$Grid<-"Low"

df=rbind( df_All,df_High,df_Low )
#df=df[,-7]

colnames(df) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)" , "Grid"   )

unique(df$var)
head(df)


grid_levels <- unique(df$Grid)

library(dplyr)

df <- df %>%
  arrange(Grid, var)

df <- df %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"Shannon" ,"shan_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","Shannon:SoilThickness", 
               "Shannon:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))



cols<-c("#ADD9E6" , "#d2de61" ,"#dd7500")


####To calculate the percentage for each category########
variable_order <- c(
  "AverageDBH", "DBH_CV", "Density", "Age", 
  "PD", "shan_Broadleaf", "Shannon", "MAT", "MAP",
  "SoilThickness", 
  "HumusLayerThickness",
  "Shannon:SoilThickness"
)

B <- df %>% 
  group_by(Grid, var) %>%
  dplyr::mutate(var = factor(var, levels = variable_order)) %>% 
  group_by(Grid, var) %>% 
  dplyr::summarise(sum_value = sum(abs(Estimate), na.rm = TRUE), .groups = 'drop') %>% 
  group_by(Grid) %>% 
  dplyr::mutate(
    new_col = sum_value / sum(sum_value, na.rm = TRUE),
    new_col = round(new_col, 4)
  ) %>% 
  arrange(Grid, var)


print(B, n = 20)

write.csv(B, "Percentage for each category.csv", row.names = FALSE)


####Plot1########

df <- df %>%
  dplyr::mutate(
    sign = if_else(
      Grid == "Low", Estimate +2*abs(`Std. Error`) ,
      if_else(Grid == "High", Estimate + 2*abs( `Std. Error`),
              if_else(Grid == "All", Estimate + 2*abs( `Std. Error`), NA_real_))
    )
  )
custom_order <- c( "AverageDBH" ,"DBH_CV"  ,"Density",
                   "PD" ,"shan_Broadleaf","Shannon" ,"Age",
                   
                   "PD:HumusLayerThickness","Shannon:HumusLayerThickness" , 
                   "PD:SoilThickness", "Shannon:SoilThickness",
                   "HumusLayerThickness", "SoilThickness" ,
                   "MAT", "MAP" 
)
df <- df %>%
  mutate(Grid = fct_recode(Grid,
                           "All" = "All",
                           "HCS" = "High",
                           "LCS" = "Low"
  ))

unique(df$var)
p_all310= df %>%
  dplyr::mutate(Grid = fct_relevel(Grid, c("All", "LCS","HCS")))  %>% 
  dplyr::mutate(group1=fct_relevel(var,custom_order))  %>% 
  group_by(group1, Grid) %>% 
  arrange(Estimate) %>% 
  dplyr::mutate(var = fct_relevel(var, var)) %>% 
  dplyr::mutate(signi = case_when(
    `Pr(>|t|)` < 0.1 & `Pr(>|t|)` >= 0.05 ~ '',
    `Pr(>|t|)` > 0.1 ~ '',
    `Pr(>|t|)` > 0.05 ~ '',
    `Pr(>|t|)` < 0.05 & `Pr(>|t|)` >= 0.01 ~ '*',
    `Pr(>|t|)` < 0.01 & `Pr(>|t|)` >= 0.001 ~ '**',
    `Pr(>|t|)` < 0.001 ~ '***'
  )) %>% 
  
  arrange(group) %>%  
  ggplot(aes(x = Estimate, y = fct_reorder(var, desc(group1)), color = Grid, group = Grid)) +
  geom_vline(xintercept=c(0), linetype="longdash", color ="#cccccc")+
  
  geom_linerange(
    aes(xmin = Estimate - `Std. Error`- `Std. Error`, xmax = Estimate + `Std. Error`+ `Std. Error`, 
        color = Grid, group = Grid),
    size =1, 
    #color = "#cccccc",
    alpha =1, position = position_dodge(width = 0.8)
  ) +
  geom_point(
    aes(color = Grid, group = Grid),
    show.legend = T,
    size = 1, alpha = 1, 
    #shape=21,
    color = "black",
    #fill = "black",
    position = position_dodge(width = 0.8)
  )+
  scale_shape_manual(values = c(21,21,21,21,21)) +
  
  #scale_color_manual(values = c("All"="#a1a1a4", "Low"="#B7D982","High"="#1572CD" )) +
  #scale_fill_manual(values =  c("All"="#a1a1a4", "Low"="#B7D982","High"="#1572CD" )) +
  
  
  geom_text(aes(x = sign , y = var,label = signi),
            # size =3, family = "serif",  color ="black",fontface = "bold", vjust = -0.3,position = position_dodge(width = 0.6), hjust = 0.5) +
            size =3.5, family = "serif",  color ="black",
            fontface = "bold", position = position_dodge(width = 0.8), 
            # vjust = 0.5,   
            hjust = 0) +
  
  geom_hline(yintercept = seq(0.5, length(custom_order) - 0.5), linetype = "solid", color = "#f3f4f7") +  
  
  theme_bw( )+
  
  labs(title = expression((a)~italic(Cunninghamia~lanceolata)))+
  labs(x="Parameter Estimate",y=NULL)+
  
  #labs(x=NULL,y=NULL)+
  
  #theme(axis.title.x = element_text(size = 12, family =  "serif"))+
  scale_color_manual(values = cols)+
  scale_x_continuous(limits = c(-0.6,1.1),
                     breaks = c(-0.5,0,0.5,1))+
  
  
  scale_y_discrete(
    position = "right",
    labels = c(
      expression(Average~DBH),             
      expression(DBH~variation),           
      expression(Stand~density),         
      expression(Phylogenetic~diversity),                       
      expression(Species~diversity[Broad]),  
      expression(Species~diversity),            
      expression(Age),            
      expression(atop(Soil~thickness %*% Species~diversity, phantom())),
      
      # 
      expression(Humus~layer~thickness),            
      expression(Soil~thickness),        
      
      expression(MAT),  
      expression(MAP)    
    ),
    limits = rev
  )+
  
  theme(
    axis.ticks = element_line(),
    axis.text.x = element_text(size = 14,color="black",family="serif"),
    axis.text.y = element_text(size = 14,color="black",face="bold",family="serif"),
    #axis.text.y =  element_blank(),
    axis.ticks.y = element_line(),
    axis.title = element_text(size = 14, family =  "serif",color = "black"),
    plot.title = element_text(size=14,family="serif"),
    
    
    
    panel.grid = element_blank(),
    
    axis.line = element_line(color = "black", size = 0.5),  
    
    
    axis.text = element_text(color = "black"),  
    axis.line.x = element_line(color = "black", size = 0.5),  
    axis.line.y = element_line(color = "black", size = 0.5) , 
    #axis.line.y = element_blank() , 
    legend.position = c(0.82,0.88),
    #legend.position = "bottom",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 12, family = "serif"#,face = "italic"
    ), 
    legend.title = element_blank(),  
    
    legend.direction = "vertical",  
    legend.box = "vertical",
    # legend.margin = margin(0, 0, 0, 0, unit = "pt"),
    
  )+ guides(
    color = guide_legend(reverse = TRUE)  )


p_all310

#################################################################################
#######Pinus massoniana：plot_all######

library(tidyverse)
library(ggplot2)
library(ggh4x)
library(dplyr)

df_All<-read.csv("PD_Fixed_All_220.csv",check.names = F)
df_High<-read.csv("PD_Fixed_High_220.csv",check.names = F)
df_Low<-read.csv("PD_Fixed_Low_220.csv",check.names = F)

df_All$Grid<-"All"
df_High$Grid<-"High"
df_Low$Grid<-"Low"

df=rbind( df_All,df_High,df_Low )
#df=df[,-7]
colnames(df) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)" , "Grid"   )

unique(df$var)
head(df)


grid_levels <- unique(df$Grid)

library(dplyr)


df <- df %>%
  arrange(Grid, var)

df <- df %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"Shannon" ,"shan_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","Shannon:SoilThickness", 
               "Shannon:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))



cols<-c("#ADD9E6" , "#d2de61" ,"#dd7500")


####To calculate the percentage for each category########
variable_order <- c(
  "AverageDBH", "DBH_CV", "Density", "Age", 
  "PD", "shan_Broadleaf", "Shannon", "MAT", "MAP",
  "SoilThickness", 
  "HumusLayerThickness",
  "Shannon:SoilThickness"
)

B <- df %>% 
  group_by(Grid, var) %>%
  dplyr::mutate(var = factor(var, levels = variable_order)) %>% 
  group_by(Grid, var) %>% 
  dplyr::summarise(sum_value = sum(abs(Estimate), na.rm = TRUE), .groups = 'drop') %>% 
  group_by(Grid) %>% 
  dplyr::mutate(
    new_col = sum_value / sum(sum_value, na.rm = TRUE),
    new_col = round(new_col, 4)
  ) %>% 
  arrange(Grid, var)


print(B, n = 20)

write.csv(B, "Percentage for each category.csv", row.names = FALSE)


####Plot1########

df <- df %>%
  dplyr::mutate(
    sign = if_else(
      Grid == "Low", Estimate +2*abs(`Std. Error`) ,
      if_else(Grid == "High", Estimate + 2*abs( `Std. Error`),
              if_else(Grid == "All", Estimate + 2*abs( `Std. Error`), NA_real_))
    )
  )
custom_order <- c( "AverageDBH" ,"DBH_CV"  ,"Density",
                   "PD" ,"shan_Broadleaf","Shannon" ,"Age",
                   
                   "PD:HumusLayerThickness","Shannon:HumusLayerThickness" , 
                   "PD:SoilThickness", "Shannon:SoilThickness",
                   "HumusLayerThickness", "SoilThickness" ,
                   "MAT", "MAP" 
)

unique(df$var)

library(forcats)

df <- df %>%
  mutate(Grid = fct_recode(Grid,
                           "All" = "All",
                           "HCS" = "High",
                           "LCS" = "Low"
  ))


p_all220= df %>%
  dplyr::mutate(Grid = fct_relevel(Grid, c("All", "LCS","HCS")))  %>% 
  dplyr::mutate(group1=fct_relevel(var,custom_order))  %>% 
  group_by(group1, Grid) %>% 
  arrange(Estimate) %>% 
  dplyr::mutate(var = fct_relevel(var, var)) %>% 
  dplyr::mutate(signi = case_when(
    `Pr(>|t|)` < 0.1 & `Pr(>|t|)` >= 0.05 ~ '',
    `Pr(>|t|)` > 0.1 ~ '',
    `Pr(>|t|)` > 0.05 ~ '',
    `Pr(>|t|)` < 0.05 & `Pr(>|t|)` >= 0.01 ~ '*',
    `Pr(>|t|)` < 0.01 & `Pr(>|t|)` >= 0.001 ~ '**',
    `Pr(>|t|)` < 0.001 ~ '***'
  )) %>% 
  
  arrange(group) %>%  
  ggplot(aes(x = Estimate, y = fct_reorder(var, desc(group1)), color = Grid, group = Grid)) +
  geom_vline(xintercept=c(0), linetype="longdash", color ="#cccccc")+
  
  geom_linerange(
    aes(xmin = Estimate - `Std. Error`- `Std. Error`, xmax = Estimate + `Std. Error`+ `Std. Error`, 
        color = Grid, group = Grid),
    size =1, 
    #color = "#cccccc",
    alpha =1, position = position_dodge(width = 0.8)
  ) +
  geom_point(
    aes(color = Grid, group = Grid),
    show.legend = T,
    size = 1, alpha = 1, 
    #shape=21,
    color = "black",
    #fill = "black",
    position = position_dodge(width = 0.8)
  )+
  scale_shape_manual(values = c(21,21,21,21,21)) +
  
 
  geom_text(aes(x = sign , y = var,label = signi),
            # size =3, family = "serif",  color ="black",fontface = "bold", vjust = -0.3,position = position_dodge(width = 0.6), hjust = 0.5) +
            size =3.5, family = "serif",  color ="black",
            fontface = "bold", position = position_dodge(width = 0.8), 
            # vjust = 0.5,   
            hjust = 0) +
  
  geom_hline(yintercept = seq(0.5, length(custom_order) - 0.5), linetype = "solid", color = "#f3f4f7") +  
  
  theme_bw( )+
  
  labs(title = expression((b)~italic(Pinus~massoniana)))+
  labs(x="Parameter Estimate",y=NULL)+
  
  #labs(x=NULL,y=NULL)+
  
  #theme(axis.title.x = element_text(size = 12, family =  "serif"))+
  scale_color_manual(values = cols)+
  scale_x_continuous(limits = c(-0.6,1.2),
                     breaks = c(-0.5,0,0.5,1))+
  
  
  scale_y_discrete(
    position = "right",
    labels = c(
      expression(Average~DBH),             
      expression(DBH~variation),           
      expression(Stand~density),         
      expression(Phylogenetic~diversity),                       
      expression(Species~diversity[Broad]),  
      expression(Species~diversity),            
      expression(Age),            
      expression(atop(Soil~thickness %*% Species~diversity, phantom())),
      
      # 
      expression(Humus~layer~thickness),            
      expression(Soil~thickness),         
      
      expression(MAT),  
      expression(MAP)    
    ),
    limits = rev
  )+
  
  theme(
    axis.ticks = element_line(),
    axis.text.x = element_text(size = 14,color="black",family="serif"),
    axis.text.y = element_text(size = 14,color="black",face="bold",family="serif"),
    #axis.text.y =  element_blank(),
    axis.ticks.y = element_line(),
    axis.title = element_text(size = 14, family =  "serif",color = "black"),
    plot.title = element_text(size=14,family="serif"),
    
    
    
    panel.grid = element_blank(),
    
    axis.line = element_line(color = "black", size = 0.5),  
    
    
    axis.text = element_text(color = "black"),  
    axis.line.x = element_line(color = "black", size = 0.5),  
    axis.line.y = element_line(color = "black", size = 0.5) , 
    #axis.line.y = element_blank() , 
    legend.position = c(0.82,0.88),
    #legend.position = "bottom",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 12, family = "serif"#,face = "italic"
    ), 
    legend.title = element_blank(),  
    
    legend.direction = "vertical",  
    legend.box = "vertical",
    #legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    
  )+ guides(
    color = guide_legend(reverse = TRUE)  )


p_all220

####ggarrange####
library(patchwork)
library(ggpubr)
Plot_Liear1=ggarrange(p_all310,p_all220,
                      ncol = 1,nrow =2,
                      heights = c(1,1),
                      widths = c(1,1),common.legend = FALSE, align = c("hv"))

Plot_Liear1
ggsave("Plot_Liear_PD.png", path = "E:/",
       width =5, height =8,dpi=600, plot=Plot_Liear1)


#################################################################################
####Cunninghamia lanceolata：Plot2：100%Plot############
df<-read.csv("PD_Fixed_All_310.csv",check.names = F)
df$Estimate <- round(df$Estimate, 2)
colnames(df) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)"   )

unique(df$var)

df <- df %>%
  dplyr::mutate(across(where(is.numeric), round,1))

df %>% 
  dplyr::mutate(group=fct_relevel(var,
                                  rev(c()))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value))



df <- df %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"Shannon" ,"shan_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","Shannon:SoilThickness", 
               "Shannon:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))

cols1=c(
  "#463D09" ,
  "#FFF1CC",
  "#9AB560" , "#E7E5E6"
  
)
df1=df

p_all2_G=df1 %>% 
  dplyr::mutate(group=fct_relevel(group,
                                  rev(c("Stand structural", "Diversity",
                                        
                                        "Environment","Interaction"
                                  )))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(
    new_col=sum_value/sum(sum_value)*100) %>% 
  ggplot(aes(x = 1, y = new_col, label = group)) +
  geom_col(aes(fill = group, color = group), alpha =1, show.legend = FALSE) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values =cols1    ) +
  coord_flip()+
  
  scale_fill_manual(values = cols1)+
  scale_y_continuous(expand = c(0,0), limits = c(0,100), breaks = c(0,20,40,60,80,100))+
  
  
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 12, family = "serif"),
    axis.title.y = element_blank(),  
    axis.line.y = element_blank(),   
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    axis.text.y = element_blank(),
    plot.title = element_blank(),
    axis.title.x = element_text(size = 14, 
                                family =  "serif")
  ) +
  
  
  
  labs(y="Relative effect of estimates (%)")+
  
  labs(subtitle  = expression(Adj.~ italic(R)^2==73~ '%' ))+
  labs(title = expression(All~plots))+
  theme(plot.title = element_text(hjust = 0, face = "bold"))+
  
  theme(plot.subtitle = element_text(size = 14, family = "serif") 
  )+
  theme(plot.title = element_text(size = 14, 
                                  family =  "serif")
        
        
  )
p_all2_G
#################
df2<-read.csv("PD_Fixed_Low_310.csv",check.names = F)
df2$Estimate <- round(df2$Estimate, 1)
colnames(df2) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)"   )

unique(df2$var)


df2<- df2 %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"Shannon" ,"shan_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","Shannon:SoilThickness", 
               "Shannon:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))


cols1=c("#463D09" ,"#FFF1CC", "#9AB560" , "#E7E5E6")


df2$Estimate <- round(df2$Estimate, 2)

p_all2_L=df2 %>% 
  dplyr::mutate(group=fct_relevel(group,
                                  rev(c("Stand structural", "Diversity",
                                        
                                        "Environment","Interaction"
                                  )))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value)*100) %>% 
  ggplot(aes(x = 1, y = new_col, label = group)) +
  geom_col(aes(fill = group, color = group), alpha =1, show.legend = FALSE) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values =cols1  ) + 
  coord_flip()+
  
  scale_fill_manual(values = cols1)+
  scale_y_continuous(expand = c(0,0), limits = c(0,100), breaks = c(0,20,40,60,80,100))+
  
  
  theme_minimal()+
  theme(
    axis.text.x = element_blank(),  
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),  
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank( ), 
    axis.text.y = element_blank(),  
    plot.title = element_blank()
  )+
  
  
  labs(y="Relative effect of estimates (%)")+
  
  labs(subtitle  = expression(Adj.~ italic(R)^2==83~ '%' ))+
  labs(title = expression(Low-carbon-sequestration))+
  theme(plot.title = element_text(hjust = 0, face = "bold"))+
  
  theme(plot.subtitle = element_text(size = 14, family = "serif") 
  )+
  theme(plot.title = element_text(size = 14, 
                                  family =  "serif"))
p_all2_L
#################
df3<-read.csv("PD_Fixed_High_310.csv",check.names = F)
df3$Estimate <- round(df3$Estimate, 3)
colnames(df3) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)"   )

unique(df3$var)

df3 %>% 
  dplyr::mutate(group=fct_relevel(var,
                                  rev(c()))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value))
df3


df3 <- df3 %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"Shannon" ,"shan_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","Shannon:SoilThickness", 
               "Shannon:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))


cols1=c("#463D09" ,"#FFF1CC", "#9AB560" , "#E7E5E6")
p_all2_H=df3 %>% 
  dplyr::mutate(group=fct_relevel(group,
                                  rev(c("Stand structural", "Diversity",
                                        
                                        "Environment","Interaction")))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value)*100) %>% 
  ggplot(aes(x = 1, y = new_col, label = group)) +
  geom_col(aes(fill = group, color = group), alpha =1, show.legend = FALSE) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1    ) + 
  coord_flip()+
  
  scale_fill_manual(values = cols1)+
  scale_y_continuous(expand = c(0,0), limits = c(0,100), breaks = c(0,20,40,60,80,100))+
  
  
  theme_minimal()+
  theme(
    axis.text.x = element_blank(),  
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),  
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank( ), 
    axis.text.y = element_blank(),  
    plot.title = element_blank()
  )+
  
  labs(y="Relative effect of estimates (%)")+
  
  labs(subtitle  = expression(Adj.~ italic(R)^2==72~ '%' ))+
  labs(title = expression(High-carbon-sequestration))+
  
  theme(plot.title = element_text(hjust = 0, face = "bold"))+
  
  theme(plot.subtitle = element_text(size = 14, family = "serif") 
  )+
  theme(plot.title = element_text(size = 14, 
                                  family =  "serif"))

p_all2_H

##########################
library(patchwork)
library(ggpubr)
pG1=ggarrange(p_all2_H,p_all2_L, p_all2_G ,
              ncol =1,nrow = 3,
              heights = c(1,1),
              widths = c(1,1),common.legend = FALSE, align = c("hv"))
pG1
##################################################################################
####Pinus massoniana：Plot2：100%Plot############
dfP<-read.csv("PD_Fixed_All_220.csv",check.names = F)

colnames(dfP) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)"   )

unique(dfP$var)

df <- df %>%
  dplyr::mutate(across(where(is.numeric), round,1))



dfP <- dfP %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"Shannon" ,"shan_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","Shannon:SoilThickness", 
               "Shannon:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))


cols1=c("#463D09" ,"#FFF1CC", "#9AB560" , "#E7E5E6")

p_all2_PG=dfP %>% 
  dplyr::mutate(group=fct_relevel(group,
                                  rev(c("Stand structural", "Diversity",
                                        
                                        "Environment","Interaction")))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value)*100) %>% 
  ggplot(aes(x = 1, y = new_col, label = group)) +
  geom_col(aes(fill = group, color = group), alpha =1, show.legend = FALSE) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1    ) +
  coord_flip()+
  
  scale_fill_manual(values = cols1)+
  scale_y_continuous(expand = c(0,0), limits = c(0,100), breaks = c(0,20,40,60,80,100))+
  
  
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 12, family = "serif"),
    axis.title.y = element_blank(),  
    axis.line.y = element_blank(),   
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    axis.text.y = element_blank(),
    plot.title = element_blank(),
    axis.title.x = element_text(size = 14, 
                                family =  "serif")
  ) +
  
  
  
  labs(y="Relative effect of estimates (%)")+
  
  labs(subtitle  = expression(Adj.~ italic(R)^2==83~ '%' ))+
  labs(title = expression(All~plots))+
  theme(plot.title = element_text(hjust = 0, face = "bold")
        
        
  )+
  
  theme(plot.subtitle = element_text(size = 14, family = "serif") 
  )+
  theme(plot.title = element_text(size = 14, 
                                  family =  "serif"))
p_all2_PG
#################
dfP2<-read.csv("PD_Fixed_Low_220.csv",check.names = F)

colnames(dfP2) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)"   )

unique(dfP2$var)
dfP2 <- dfP2 %>%
  dplyr::mutate(across(where(is.numeric), round,2))

dfP2 %>% 
  dplyr::mutate(group=fct_relevel(var,
                                  rev(c()))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value))

dfP2 <- dfP2 %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"Shannon" ,"shan_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","Shannon:SoilThickness", 
               "Shannon:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))


cols1=c("#463D09" ,"#FFF1CC", "#9AB560" , "#E7E5E6")
p_all2_PL=dfP2 %>% 
  dplyr::mutate(group=fct_relevel(group,
                                  rev(c("Stand structural", "Diversity",
                                        "Environment","Interaction")))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value)*100) %>% 
  ggplot(aes(x = 1, y = new_col, label = group)) +
  geom_col(aes(fill = group, color = group), alpha =1, show.legend = FALSE) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1 ) +
  coord_flip()+
  
  scale_fill_manual(values = cols1)+
  scale_y_continuous(expand = c(0,0), limits = c(0,100), breaks = c(0,20,40,60,80,100))+
  
  
  theme_minimal()+
  theme(
    axis.text.x = element_blank(),  
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),  
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank( ), 
    axis.text.y = element_blank(),  
    plot.title = element_blank()
  )+
  
  labs(y="Relative effect of estimates (%)")+
  
  labs(subtitle  = expression(Adj.~ italic(R)^2==90~ '%' ))+
  labs(title = expression(Low-carbon-sequestration))+
  
  theme(plot.title = element_text(hjust = 0, face = "bold"))+
  
  theme(plot.subtitle = element_text(size = 14, family = "serif") 
  )+
  theme(plot.title = element_text(size = 14, 
                                  family =  "serif"))
p_all2_PL
#################
dfP3<-read.csv("PD_Fixed_High_220.csv",check.names = F)



colnames(dfP3) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)"   )

unique(dfP3$var)

dfP3 <- dfP3 %>%
  dplyr::mutate(across(where(is.numeric), round,2))

dfP3 %>% 
  dplyr::mutate(group=fct_relevel(var,
                                  rev(c()))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value))


dfP3 <- dfP3 %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"Shannon" ,"shan_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","Shannon:SoilThickness", 
               "Shannon:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))


cols1=c("#463D09" ,"#FFF1CC", "#9AB560" , "#E7E5E6")


p_all2_PH=dfP3 %>% 
  dplyr::mutate(group=fct_relevel(group,
                                  rev(c("Stand structural", "Diversity",
                                        
                                        "Environment","Interaction"
                                  )))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value)*100) %>% 
  ggplot(aes(x = 1, y = new_col, label = group)) +
  geom_col(aes(fill = group, color = group), alpha =1, show.legend = FALSE) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  coord_flip()+
  
  scale_fill_manual(values = cols1)+
  scale_y_continuous(expand = c(0,0), limits = c(0,100), breaks = c(0,20,40,60,80,100))+
  
  
  theme_minimal()+
  theme(
    axis.text.x = element_blank(),  
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),  
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank( ), 
    axis.text.y = element_blank(),  
    plot.title = element_blank()
  )+
  
  labs(y="Relative effect of estimates (%)")+
  
  labs(subtitle  = expression(Adj.~ italic(R)^2==73~ '%' ))+
  labs(title = expression(High-carbon-sequestration))+

  theme(plot.title = element_text(hjust = 0, face = "bold"))+
  
  theme(plot.subtitle = element_text(size = 14, family = "serif") 
  )+
  theme(plot.title = element_text(size = 14, 
                                  family =  "serif"))

p_all2_PH

##########################
library(patchwork)
library(patchwork)
pGP1=ggarrange(p_all2_PH,p_all2_PL,p_all2_PG ,
               ncol =1,nrow = 3,
               heights = c(1,1),
               widths = c(1,1),common.legend = FALSE, align = c("hv"))
pGP1


######ggarrange##########
pG=ggarrange(
  pG1,
  pGP1,
  ncol =2,nrow =1,
  heights = c(1,1),
  widths = c(1,1),
  common.legend = FALSE, align = c("hv"))
pG

ggsave("Plot_Liear2_PD.png", path = "E:/",width =6,
       height =4.2,dpi=1200, plot=pG)

###################################################################################
###################################################################################
##===============================================================================
## FD_Cunninghamia lanceolata:LMM ---------
##===============================================================================
####Cunninghamia lanceolata FD：LMM_All####
rm(list=ls())

data_sem2=read.csv("Plot Cunninghamia lanceolata.csv")
colnames(data_sem2)


data_sem2[, c("AverageDBH"   , "Density", "AGC","DBH_CV")] <- lapply(
  data_sem2[, c("AverageDBH"  , "Density", "AGC","DBH_CV")],
  function(x) log(as.numeric(x))
)


data_sem2[, c( 4:20)] <- lapply(
  data_sem2[, c( 4:20)],
  function(x) as.numeric(scale(x))
)


data_sem2 <- as.data.frame(data_sem2)



colSums(is.na(data_sem2))
modall <- lmer(
  AGC ~ Age+AverageDBH + Density + DBH_CV +
    FDis + PD + FD_Broadleaf +
    MAT + MAP +
    SoilThickness + HumusLayerThickness + 
    
    SoilThickness: FDis+
   
    
    (1 | RegionName),
  
  data = data_sem2,
  REML = FALSE
)

summary(modall)
r.squaredGLMM(modall)
round(vif(modall), 2)

vif(modall) 

#####VIF#####

new_order <- c("AverageDBH", "DBH_CV", "Density", "Age", 
               "PD", "FD_Broadleaf", "FDis", "MAT", "MAP",
               "SoilThickness", 
               "HumusLayerThickness",
               "FDis:SoilThickness")


all(new_order %in% names(vif(modall)))  


vif_values <- vif(modall)[new_order]
vif_values


vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values,
  row.names = NULL  
)


vif_table


print(vif_table, digits = 2)

write.csv(vif_table, "VIF_All_310.csv")

####Extract model data####
library(broom)

model_avg <- summary(modall) 


coef_full_avg <- model_avg$coefficients


coef_table_full <- as.data.frame(coef_full_avg)


colnames(coef_table_full) <- c("Estimate", "Std.Error", "df"  , "t.value","Pr(>|t|)")
coef_table_full


write.csv(coef_table_full, "FD_Fixed_All_310.csv")

#################################################################################
rm(list=ls())
####Cunninghamia lanceolata FD：LMM_High####
rm(list=ls())

data_sem2=read.csv("Plot Cunninghamia lanceolata.csv" ) 
colnames(data_sem2)
data_sem2 = data_sem2%>% filter(Group %in% c("2"))



data_sem2[, c("AverageDBH"   , "Density", "AGC","DBH_CV")] <- lapply(
  data_sem2[, c("AverageDBH"  , "Density", "AGC","DBH_CV")],
  function(x) log(as.numeric(x))
)


data_sem2[, c( 4:20)] <- lapply(
  data_sem2[, c( 4:20)],
  function(x) as.numeric(scale(x))
)


data_sem2 <- as.data.frame(data_sem2)



colSums(is.na(data_sem2))
modall <- lmer(
  AGC ~ AverageDBH + Density + DBH_CV +
    FDis + PD + FD_Broadleaf +
    MAT + MAP +
    SoilThickness + HumusLayerThickness + 
    
    SoilThickness:FDis+
    
    
    (1 | RegionName),
  
  data = data_sem2,
  REML = FALSE
)

summary(modall)
r.squaredGLMM(modall)
round(vif(modall), 2)

vif(modall) 

#####VIF#####

new_order <- c("AverageDBH", "DBH_CV", "Density", "Age", 
               "PD", "FD_Broadleaf", "FDis", "MAT", "MAP",
               "SoilThickness", 
               "HumusLayerThickness",
               "FDis:SoilThickness")


all(new_order %in% names(vif(modall)))  


vif_values <- vif(modall)[new_order]
vif_values


vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values,
  row.names = NULL  
)


vif_table


print(vif_table, digits = 2)

write.csv(vif_table, "VIF_All_310.csv")

####Extract model data####
library(broom)

model_avg <- summary(modall) 


coef_full_avg <- model_avg$coefficients


coef_table_full <- as.data.frame(coef_full_avg)


colnames(coef_table_full) <- c("Estimate", "Std.Error", "df"  , "t.value","Pr(>|t|)")
coef_table_full



write.csv(coef_table_full, "FD_Fixed_High_310.csv")

#################################################################################
####Cunninghamia lanceolata FD：LMM_Low####
rm(list=ls())

data_sem2=read.csv("Plot Cunninghamia lanceolata.csv" )
colnames(data_sem2)
data_sem2 = data_sem2%>% filter(Group %in% c("1"))



data_sem2[, c("AverageDBH"   , "Density", "AGC","DBH_CV")] <- lapply(
  data_sem2[, c("AverageDBH"  , "Density", "AGC","DBH_CV")],
  function(x) log(as.numeric(x))
)


data_sem2[, c( 4:20)] <- lapply(
  data_sem2[, c( 4:20)],
  function(x) as.numeric(scale(x))
)


data_sem2 <- as.data.frame(data_sem2)


colSums(is.na(data_sem2))
modall <- lmer(
  AGC ~ AverageDBH + Density + DBH_CV +
    FDis + PD + FD_Broadleaf +
    MAT + MAP +
    SoilThickness + HumusLayerThickness + 
    
    SoilThickness:FDis+
    
    (1 | RegionName),
  
  data = data_sem2,
  REML = FALSE
)

summary(modall)
r.squaredGLMM(modall)
round(vif(modall), 2)

#####VIF#####

new_order <- c("AverageDBH", "DBH_CV", "Density", "Age", 
               "PD", "FD_Broadleaf", "FDis", "MAT", "MAP",
               "SoilThickness", 
               "HumusLayerThickness",
               "FDis:SoilThickness")


all(new_order %in% names(vif(modall)))  


vif_values <- vif(modall)[new_order]
vif_values


vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values,
  row.names = NULL  
)


vif_table


print(vif_table, digits = 2)

write.csv(vif_table, "VIF_All_310.csv")

####Extract model data####
library(broom)

model_avg <- summary(modall) 


coef_full_avg <- model_avg$coefficients


coef_table_full <- as.data.frame(coef_full_avg)


colnames(coef_table_full) <- c("Estimate", "Std.Error", "df"  , "t.value","Pr(>|t|)")
coef_table_full



write.csv(coef_table_full, "FD_Fixed_Low_310.csv")

##===============================================================================
## FD_Pinus massoniana:LMM ---------
##===============================================================================
####Pinus massoniana FD：LMM_All####
data_sem2=read.csv("Plot Pinus massoniana.csv" )

data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV")] <- lapply(
  data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV")],
  function(x) log(as.numeric(x))
)

colnames(data_sem2  )


data_sem2[, c(4:20)] <- lapply(
  data_sem2[, c(4:20)],
  function(x) as.numeric(scale(x))
)



modall <- lmer(
  AGC ~ Age+AverageDBH + 
    Density + DBH_CV +
    FDis + PD + FD_Broadleaf +
    MAT + MAP +
    SoilThickness + HumusLayerThickness + 
    
    SoilThickness:FDis+
    
    (1 | RegionName),
  
  data = data_sem2,
  REML = FALSE
)

summary(modall)
r.squaredGLMM(modall)
round(vif(modall), 2)

max(vif(modall) )
#####VIF#####

new_order <- c("AverageDBH", "DBH_CV", "Density", "Age", 
               "PD", "FD_Broadleaf", "FDis", "MAT", "MAP",
               "SoilThickness", 
               "HumusLayerThickness",
               "FDis:SoilThickness")


all(new_order %in% names(vif(modall)))  


vif_values <- vif(modall)[new_order]
vif_values


vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values,
  row.names = NULL  
)


vif_table


print(vif_table, digits = 2)

write.csv(vif_table, "VIF_All_310.csv")

####Extract model data####
library(broom)

model_avg <- summary(modall) 


coef_full_avg <- model_avg$coefficients


coef_table_full <- as.data.frame(coef_full_avg)


colnames(coef_table_full) <- c("Estimate", "Std.Error", "df"  , "t.value","Pr(>|t|)")
coef_table_full



write.csv(coef_table_full, "FD_Fixed_All_220.csv")

#################################################################################
####Pinus massoniana FD：LMM_High####
rm(list=ls())

data_sem2=read.csv("Plot Pinus massoniana.csv" )
colnames(data_sem2)



data_sem2 = data_sem2%>% filter(Group %in% c("2"))


data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV" )] <- lapply(
  data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV" )],
  function(x) log(as.numeric(x))
)

colnames(data_sem2  )


data_sem2[, c(4:20)] <- lapply(
  data_sem2[, c(4:20)],
  function(x) as.numeric(scale(x))
)

modall <- lmer(
  AGC ~ AverageDBH + 
    Density + 
    DBH_CV +
    FDis + PD + FD_Broadleaf +
    MAT + MAP +
    SoilThickness + HumusLayerThickness + 
    
    
    SoilThickness:FDis+
    
    
    (1 | RegionName),
  
  data = data_sem2,
  REML = FALSE
)

summary(modall)
r.squaredGLMM(modall)
round(vif(modall), 2)

#####VIF#####

new_order <- c("AverageDBH", "DBH_CV", "Density", "Age", 
               "PD", "FD_Broadleaf", "FDis", "MAT", "MAP",
               "SoilThickness", 
               "HumusLayerThickness",
               "FDis:SoilThickness")


all(new_order %in% names(vif(modall)))  


vif_values <- vif(modall)[new_order]
vif_values


vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values,
  row.names = NULL  
)


vif_table


print(vif_table, digits = 2)

write.csv(vif_table, "VIF_All_310.csv")

####Extract model data####
library(broom)

model_avg <- summary(modall) 


coef_full_avg <- model_avg$coefficients


coef_table_full <- as.data.frame(coef_full_avg)


colnames(coef_table_full) <- c("Estimate", "Std.Error", "df"  , "t.value","Pr(>|t|)")
coef_table_full


write.csv(coef_table_full, "FD_Fixed_High_220.csv")

#################################################################################
####Pinus massoniana FD：LMM_Low####
rm(list=ls())

data_sem2=read.csv("Plot Pinus massoniana.csv" )
colnames(data_sem2)

data_sem2 = data_sem2%>% filter(Group %in% c("1"))


data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV")] <- lapply(
  data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV" )],
  function(x) log(as.numeric(x))
)


colnames(data_sem2  )


data_sem2[, c(4:20)] <- lapply(
  data_sem2[, c(4:20)],
  function(x) as.numeric(scale(x))
)

modall <- lmer(
  AGC ~ AverageDBH +
    Density + 
    DBH_CV +
    FDis + PD + FD_Broadleaf +
    MAT + MAP +
    SoilThickness + HumusLayerThickness + 
    
    
    
    SoilThickness:FDis+
    
    
    (1 | RegionName),
  
  data = data_sem2,
  REML = FALSE
)


summary(modall)
r.squaredGLMM(modall)
round(vif(modall), 2)

#####VIF#####

new_order <- c("AverageDBH", "DBH_CV", "Density", "Age", 
               "PD", "FD_Broadleaf", "FDis", "MAT", "MAP",
               "SoilThickness", 
               "HumusLayerThickness",
               "FDis:SoilThickness")


all(new_order %in% names(vif(modall)))  


vif_values <- vif(modall)[new_order]
vif_values


vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values,
  row.names = NULL  
)


vif_table


print(vif_table, digits = 2)

write.csv(vif_table, "VIF_All_310.csv")

####Extract model data####
library(broom)

model_avg <- summary(modall) 


coef_full_avg <- model_avg$coefficients


coef_table_full <- as.data.frame(coef_full_avg)


colnames(coef_table_full) <- c("Estimate", "Std.Error", "df"  , "t.value","Pr(>|t|)")
coef_table_full



write.csv(coef_table_full, "FD_Fixed_Low_220.csv")

###################################################################################
###################################################################################
###################################################################################
##===============================================================================
## FD:LMMPlot ---------
##===============================================================================
#######Cunninghamia lanceolata：plot_all######
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(ggh4x)
library(dplyr)

df_All<-read.csv("FD_Fixed_All_310.csv",check.names = F)
df_High<-read.csv("FD_Fixed_High_310.csv",check.names = F)
df_Low<-read.csv("FD_Fixed_Low_310.csv",check.names = F)

df_All$Grid<-"All"
df_High$Grid<-"High"
df_Low$Grid<-"Low"

df=rbind( df_All,df_High,df_Low )

colnames(df) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)" , "Grid"   )

unique(df$var)
head(df)


grid_levels <- unique(df$Grid)

library(dplyr)

df <- df %>%
  arrange(Grid, var)

df <- df %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"FDis" ,"FD_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","FDis:SoilThickness", 
               "FDis:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))



cols<-c("#ADD9E6" , "#d2de61" ,"#dd7500")


####To calculate the percentage for each category########

variable_order <- c(
  "AverageDBH", "DBH_CV", "Density", "Age", 
  "PD", "FD_Broadleaf", "FDis", "MAT", "MAP",
  "SoilThickness", 
  "HumusLayerThickness",
  "FDis:SoilThickness"
)

B <- df %>% 
  group_by(Grid, var) %>%
  dplyr::mutate(var = factor(var, levels = variable_order)) %>% 
  group_by(Grid, var) %>% 
  dplyr::summarise(sum_value = sum(abs(Estimate), na.rm = TRUE), .groups = 'drop') %>% 
  group_by(Grid) %>% 
  dplyr::mutate(
    new_col = sum_value / sum(sum_value, na.rm = TRUE),
    new_col = round(new_col, 4)
  ) %>% 
  arrange(Grid, var)


print(B, n = 20)


write.csv(B, "Percentage for each category.csv", row.names = FALSE)


####Plot1########

df <- df %>%
  dplyr::mutate(
    sign = if_else(
      Grid == "Low", Estimate +2*abs(`Std. Error`) ,
      if_else(Grid == "High", Estimate + 2*abs( `Std. Error`),
              if_else(Grid == "All", Estimate + 2*abs( `Std. Error`), NA_real_))
    )
  )
custom_order <- c( "AverageDBH" ,"DBH_CV"  ,"Density",
                   "PD" ,"FD_Broadleaf","FDis" ,"Age",
                   
                   "FDis:SoilThickness",
                   "HumusLayerThickness", "SoilThickness" ,
                   "MAT", "MAP" 
)
df <- df %>%
  mutate(Grid = fct_recode(Grid,
                           "All" = "All",
                           "HCS" = "High",
                           "LCS" = "Low"
  ))

unique(df$var)
p_all310= df %>%
  dplyr::mutate(Grid = fct_relevel(Grid, c("All", "LCS","HCS")))  %>% 
  dplyr::mutate(group1=fct_relevel(var,custom_order))  %>% 
  group_by(group1, Grid) %>% 
  arrange(Estimate) %>% 
  dplyr::mutate(var = fct_relevel(var, var)) %>% 
  dplyr::mutate(signi = case_when(
    `Pr(>|t|)` < 0.1 & `Pr(>|t|)` >= 0.05 ~ '',
    `Pr(>|t|)` > 0.1 ~ '',
    `Pr(>|t|)` > 0.05 ~ '',
    `Pr(>|t|)` < 0.05 & `Pr(>|t|)` >= 0.01 ~ '*',
    `Pr(>|t|)` < 0.01 & `Pr(>|t|)` >= 0.001 ~ '**',
    `Pr(>|t|)` < 0.001 ~ '***'
  )) %>% 
  
  arrange(group) %>%  
  ggplot(aes(x = Estimate, y = fct_reorder(var, desc(group1)), color = Grid, group = Grid)) +
  geom_vline(xintercept=c(0), linetype="longdash", color ="#cccccc")+
  
  geom_linerange(
    aes(xmin = Estimate - `Std. Error`- `Std. Error`, xmax = Estimate + `Std. Error`+ `Std. Error`, 
        color = Grid, group = Grid),
    size =1, 
    #color = "#cccccc",
    alpha =1, position = position_dodge(width = 0.8)
  ) +
  geom_point(
    aes(color = Grid, group = Grid),
    show.legend = T,
    size = 1, alpha = 1, 
    color = "black",
    position = position_dodge(width = 0.8)
  )+
  scale_shape_manual(values = c(21,21,21,21,21)) +
  

  geom_text(aes(x = sign , y = var,label = signi),
            # size =3, family = "serif",  color ="black",fontface = "bold", vjust = -0.3,position = position_dodge(width = 0.6), hjust = 0.5) +
            size =3.5, family = "serif",  color ="black",
            fontface = "bold", position = position_dodge(width = 0.8), 
            # vjust = 0.5,   
            hjust = 0) +
  
  geom_hline(yintercept = seq(0.5, length(custom_order) - 0.5), linetype = "solid", color = "#f3f4f7") +  
  
  theme_bw( )+
  
  labs(title = expression((a)~italic(Cunninghamia~lanceolata)))+
  labs(x="Parameter Estimate",y=NULL)+
  scale_color_manual(values = cols)+
  scale_x_continuous(limits = c(-0.6,1.1),
                     breaks = c(-0.5,0,0.5,1))+
  
  
  scale_y_discrete( position = "right",
                    labels = c(
                      expression(Average~DBH),             
                      expression(DBH~variation),           
                      expression(Stand~density),         
                      expression(Phylogenetic~diversity),                       
                      expression(Functional~diversity[Broad]),   
                      expression(Functional~diversity),            
                      expression(Age),            
                      
                      expression(atop(Soil~thickness %*% Functional~diversity, phantom())),     
                      
                      
                      expression(Humus~layer~thickness),            
                      expression(Soil~thickness),              
                      
                      expression(MAT),  
                      expression(MAP)    
                    ),
                    limits = rev
  )+
  
  theme(
    axis.ticks = element_line(),
    axis.text.x = element_text(size = 14,color="black",family="serif"),
    axis.text.y = element_text(size = 14,color="black",face="bold",family="serif"),
    #axis.text.y =  element_blank(),
    axis.ticks.y = element_line(),
    axis.title = element_text(size = 14, family =  "serif",color = "black"),
    plot.title = element_text(size=14,family="serif"),
    
    
    
    panel.grid = element_blank(),
    
    axis.line = element_line(color = "black", size = 0.5),  
    
    
    axis.text = element_text(color = "black"),  
    axis.line.x = element_line(color = "black", size = 0.5),  
    axis.line.y = element_line(color = "black", size = 0.5) , 
    #axis.line.y = element_blank() , 
    legend.position = c(0.82,0.88),
    #legend.position = "bottom",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 12, family = "serif"#,face = "italic"
    ), 
    legend.title = element_blank(),  
    
    legend.direction = "vertical",  
    legend.box = "vertical",
    #legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    
  )+ guides(
    color = guide_legend(reverse = TRUE)  )


p_all310


#################################################################################
#######Pinus massoniana：plot_all######

library(tidyverse)
library(ggplot2)
library(ggh4x)
library(dplyr)

df_All<-read.csv("FD_Fixed_All_220.csv",check.names = F)
df_High<-read.csv("FD_Fixed_High_220.csv",check.names = F)
df_Low<-read.csv("FD_Fixed_Low_220.csv",check.names = F)

df_All$Grid<-"All"
df_High$Grid<-"High"
df_Low$Grid<-"Low"

df=rbind( df_All,df_High,df_Low )


colnames(df) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)" , "Grid"   )

unique(df$var)
head(df)


grid_levels <- unique(df$Grid)

library(dplyr)


df <- df %>%
  arrange(Grid, var)

df <- df %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"FDis" ,"FD_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","FDis:SoilThickness", 
               "FDis:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))



cols<-c("#ADD9E6" , "#d2de61" ,"#dd7500")


####To calculate the percentage for each category########

variable_order <- c(
  "AverageDBH", "DBH_CV", "Density", "Age", 
  "PD", "FD_Broadleaf", "FDis", "MAT", "MAP",
  "SoilThickness", 
  "HumusLayerThickness",
  "FDis:SoilThickness"
)

B <- df %>% 
  group_by(Grid, var) %>%
  
  dplyr::mutate(var = factor(var, levels = variable_order)) %>% 
  group_by(Grid, var) %>% 
  dplyr::summarise(sum_value = sum(abs(Estimate), na.rm = TRUE), .groups = 'drop') %>% 
  
  group_by(Grid) %>% 
  dplyr::mutate(
    new_col = sum_value / sum(sum_value, na.rm = TRUE),
    new_col = round(new_col, 4)
  ) %>% 
 
  arrange(Grid, var)


print(B, n = 20)


write.csv(B, "Percentage for each category.csv", row.names = FALSE)


####Plot1########

df <- df %>%
  dplyr::mutate(
    sign = if_else(
      Grid == "Low", Estimate +2*abs(`Std. Error`) ,
      if_else(Grid == "High", Estimate + 2*abs( `Std. Error`),
              if_else(Grid == "All", Estimate + 2*abs( `Std. Error`), NA_real_))
    )
  )
custom_order <- c( "AverageDBH" ,"DBH_CV"  ,"Density",
                   "PD" ,"FD_Broadleaf","FDis" ,"Age",
                   
                   "FDis:SoilThickness",
                   "HumusLayerThickness", "SoilThickness" ,
                   "MAT", "MAP" 
)

unique(df$var)

library(forcats)

df <- df %>%
  mutate(Grid = fct_recode(Grid,
                           "All" = "All",
                           "HCS" = "High",
                           "LCS" = "Low"
  ))


p_all220= df %>%
  dplyr::mutate(Grid = fct_relevel(Grid, c("All", "LCS","HCS")))  %>% 
  dplyr::mutate(group1=fct_relevel(var,custom_order))  %>% 
  group_by(group1, Grid) %>% 
  arrange(Estimate) %>% 
  dplyr::mutate(var = fct_relevel(var, var)) %>% 
  dplyr::mutate(signi = case_when(
    `Pr(>|t|)` < 0.1 & `Pr(>|t|)` >= 0.05 ~ '',
    `Pr(>|t|)` > 0.1 ~ '',
    `Pr(>|t|)` > 0.05 ~ '',
    `Pr(>|t|)` < 0.05 & `Pr(>|t|)` >= 0.01 ~ '*',
    `Pr(>|t|)` < 0.01 & `Pr(>|t|)` >= 0.001 ~ '**',
    `Pr(>|t|)` < 0.001 ~ '***'
  )) %>% 
  
  arrange(group) %>%  
  ggplot(aes(x = Estimate, y = fct_reorder(var, desc(group1)), color = Grid, group = Grid)) +
  geom_vline(xintercept=c(0), linetype="longdash", color ="#cccccc")+
  
  geom_linerange(
    aes(xmin = Estimate - `Std. Error`- `Std. Error`, xmax = Estimate + `Std. Error`+ `Std. Error`, 
        color = Grid, group = Grid),
    size =1, 
    #color = "#cccccc",
    alpha =1, position = position_dodge(width = 0.8)
  ) +
  geom_point(
    aes(color = Grid, group = Grid),
    show.legend = T,
    size = 1, alpha = 1, 
    #shape=21,
    color = "black",
    #fill = "black",
    position = position_dodge(width = 0.8)
  )+
  scale_shape_manual(values = c(21,21,21,21,21)) +
  

  geom_text(aes(x = sign , y = var,label = signi),
            # size =3, family = "serif",  color ="black",fontface = "bold", vjust = -0.3,position = position_dodge(width = 0.6), hjust = 0.5) +
            size =3.5, family = "serif",  color ="black",
            fontface = "bold", position = position_dodge(width = 0.8), 
            # vjust = 0.5,   
            hjust = 0) +
  
  geom_hline(yintercept = seq(0.5, length(custom_order) - 0.5), linetype = "solid", color = "#f3f4f7") +  
  
  theme_bw( )+
  
  labs(title = expression((b)~italic(Pinus~massoniana)))+
  labs(x="Parameter Estimate",y=NULL)+
  

  scale_color_manual(values = cols)+
  scale_x_continuous(limits = c(-0.6,1.1),
                     breaks = c(-0.5,0,0.5,1))+
  
  
  scale_y_discrete( position = "right",
                    labels = c(
                      expression(Average~DBH),             
                      expression(DBH~variation),           
                      expression(Stand~density),         
                      expression(Phylogenetic~diversity),                       
                      expression(Functional~diversity[Broad]),   
                      expression(Functional~diversity),            
                      expression(Age),            
 
                      
                      expression(atop(Soil~thickness %*% Functional~diversity, phantom())),        
                      
                      expression(Humus~layer~thickness),            
                      expression(Soil~thickness),         
                      
                      expression(MAT),  
                      expression(MAP)    
                    ),
                    limits = rev
  )+
  
  theme(
    axis.ticks = element_line(),
    axis.text.x = element_text(size = 14,color="black",family="serif"),
    axis.text.y = element_text(size = 14,color="black",face="bold",family="serif"),
    #axis.text.y =  element_blank(),
    axis.ticks.y = element_line(),
    axis.title = element_text(size = 14, family =  "serif",color = "black"),
    plot.title = element_text(size=14,family="serif"),
    
    
    
    panel.grid = element_blank(),

    
    axis.line = element_line(color = "black", size = 0.5),  
    
    
    axis.text = element_text(color = "black"),  
    axis.line.x = element_line(color = "black", size = 0.5),  
    axis.line.y = element_line(color = "black", size = 0.5) , 
    #axis.line.y = element_blank() , 
    legend.position = c(0.82,0.88),
    #legend.position = "bottom",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 12, family = "serif"#,face = "italic"
    ), 
    legend.title = element_blank(),  
    
    legend.direction = "vertical",  
    legend.box = "vertical",
    #legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    
  )+ guides(
    color = guide_legend(reverse = TRUE)  )


p_all220


####ggarrange####
library(patchwork)
library(ggpubr)
Plot_Liear1=ggarrange(p_all310,p_all220,
                      ncol = 1,nrow =2,
                      heights = c(1,1),
                      widths = c(1,1),common.legend = FALSE, align = c("hv"))

Plot_Liear1
ggsave("Plot_Liear_FD.png", path = "E:/",
       width =5, height =8,dpi=600, plot=Plot_Liear1)


#################################################################################
####Cunninghamia lanceolata：Plot2：100%Plot############
df<-read.csv("FD_Fixed_All_310.csv",check.names = F)
df$Estimate <- round(df$Estimate, 3)
colnames(df) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)"   )

unique(df$var)

df <- df %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"FDis" ,"FD_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","FDis:SoilThickness", 
               "FDis:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))

cols1=c(
  "#463D09" ,
 
  "#FFF1CC",
  "#9AB560" , "#E7E5E6"
  
)
df1=df

p_all2_G=df1 %>% 
  dplyr::mutate(group=fct_relevel(group,
                                  rev(c("Stand structural", "Diversity",
                                        
                                        "Environment","Interaction"
                                  )))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(
    new_col=sum_value/sum(sum_value)*100) %>% 
  ggplot(aes(x = 1, y = new_col, label = group)) +
  geom_col(aes(fill = group, color = group), alpha =1, show.legend = FALSE) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values =cols1    ) +
  coord_flip()+
  
  scale_fill_manual(values = cols1)+
  scale_y_continuous(expand = c(0,0), limits = c(0,100), breaks = c(0,20,40,60,80,100))+
  
  
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 12, family = "serif"),
    axis.title.y = element_blank(),  
    axis.line.y = element_blank(),   
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    axis.text.y = element_blank(),
    plot.title = element_blank(),
    axis.title.x = element_text(size = 14, 
                                family =  "serif")
  ) +
  
  
  
  labs(y="Relative effect of estimates (%)")+
  
  labs(subtitle  = expression(Adj.~ italic(R)^2==73~ '%' ))+
  labs(title = expression(All~plots))+
 
  theme(plot.title = element_text(hjust = 0, face = "bold"))+
  
  theme(plot.subtitle = element_text(size = 14, family = "serif") 
  )+
  theme(plot.title = element_text(size = 14, 
                                  family =  "serif")
        
        
  )
p_all2_G
#################
df2<-read.csv("FD_Fixed_Low_310.csv",check.names = F)
colnames(df2) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)"   )

unique(df2$var)

df2<- df2 %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"FDis" ,"FD_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","FDis:SoilThickness", 
               "FDis:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))


cols1=c("#463D09" ,"#FFF1CC", "#9AB560" , "#E7E5E6")


df2$Estimate <- round(df2$Estimate, 2)

p_all2_L=df2 %>% 
  dplyr::mutate(group=fct_relevel(group,
                                  rev(c("Stand structural", "Diversity",
                                        
                                        "Environment","Interaction"
                                  )))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value)*100) %>% 
  ggplot(aes(x = 1, y = new_col, label = group)) +
  geom_col(aes(fill = group, color = group), alpha =1, show.legend = FALSE) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values =cols1  ) + 
  coord_flip()+
  
  scale_fill_manual(values = cols1)+
  scale_y_continuous(expand = c(0,0), limits = c(0,100), breaks = c(0,20,40,60,80,100))+
  
  
  theme_minimal()+
  theme(
    axis.text.x = element_blank(),  
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),  
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank( ), 
    axis.text.y = element_blank(),  
    plot.title = element_blank()
  )+
  
  
  labs(y="Relative effect of estimates (%)")+
  
  labs(subtitle  = expression(Adj.~ italic(R)^2==83~ '%' ))+
  labs(title = expression(Low-carbon-sequestration))+
  
  theme(plot.title = element_text(hjust = 0, face = "bold"))+
  
  theme(plot.subtitle = element_text(size = 14, family = "serif") 
  )+
  theme(plot.title = element_text(size = 14, 
                                  family =  "serif"))
p_all2_L
#################
df3<-read.csv("FD_Fixed_High_310.csv",check.names = F)
df3$Estimate <- round(df3$Estimate, 2)
colnames(df3) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)"   )

unique(df3$var)


df3 %>% 
  dplyr::mutate(group=fct_relevel(var,
                                  rev(c()))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value))


df3 <- df3 %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"FDis" ,"FD_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","FDis:SoilThickness", 
               "FDis:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))

df3 %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value))

cols1=c("#463D09" ,"#FFF1CC", "#9AB560" , "#E7E5E6")
p_all2_H=df3 %>% 
  dplyr::mutate(group=fct_relevel(group,
                                  rev(c("Stand structural", "Diversity",
                                        
                                        "Environment","Interaction")))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value)*100) %>% 
  ggplot(aes(x = 1, y = new_col, label = group)) +
  geom_col(aes(fill = group, color = group), alpha =1, show.legend = FALSE) +
  scale_fill_manual(values = cols1) +
  #scale_color_manual(values = c("#1572CD"  ,"#1572CD" ,"#1572CD" ,"#1572CD"      )    ) + 
  scale_color_manual(values = cols1    ) + 
  coord_flip()+
  
  scale_fill_manual(values = cols1)+
  scale_y_continuous(expand = c(0,0), limits = c(0,100), breaks = c(0,20,40,60,80,100))+
  
  
  theme_minimal()+
  theme(
    axis.text.x = element_blank(),  
    #axis.text.x = element_text(size = 10, family =  "serif"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),  
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank( ), 
    axis.text.y = element_blank(),  
    plot.title = element_blank()
  )+
  
  labs(y="Relative effect of estimates (%)")+
  
  labs(subtitle  = expression(Adj.~ italic(R)^2==74~ '%' ))+
  #labs(x="Pinus massoniana")+
  labs(title = expression(High-carbon-sequestration))+
  
  #ggtitle(" ") +
  
  theme(plot.title = element_text(hjust = 0, face = "bold"))+
  
  theme(plot.subtitle = element_text(size = 14, family = "serif") 
  )+
  theme(plot.title = element_text(size = 14, 
                                  family =  "serif"))

p_all2_H

##########################
library(patchwork)
library(ggpubr)
pG1=ggarrange(p_all2_H,p_all2_L, p_all2_G ,
              ncol =1,nrow = 3,
              heights = c(1,1),
              widths = c(1,1),common.legend = FALSE, align = c("hv"))
pG1
##################################################################################
####Pinus massoniana：Plot2：100%Plot############
dfP<-read.csv("FD_Fixed_All_220.csv",check.names = F)

colnames(dfP) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)"   )

unique(dfP$var)

dfP <- dfP %>%
  dplyr::mutate(across(where(is.numeric), round,2))



dfP <- dfP %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"FDis" ,"FD_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","FDis:SoilThickness", 
               "FDis:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))


cols1=c("#463D09" ,"#FFF1CC", "#9AB560" , "#E7E5E6")

p_all2_PG=dfP %>% 
  dplyr::mutate(group=fct_relevel(group,
                                  rev(c("Stand structural", "Diversity",
                                        
                                        "Environment","Interaction")))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value)*100) %>% 
  ggplot(aes(x = 1, y = new_col, label = group)) +
  geom_col(aes(fill = group, color = group), alpha =1, show.legend = FALSE) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1    ) +
  coord_flip()+
  
  scale_fill_manual(values = cols1)+
  scale_y_continuous(expand = c(0,0), limits = c(0,100), breaks = c(0,20,40,60,80,100))+
  
  
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 12, family = "serif"),
    axis.title.y = element_blank(),  
    axis.line.y = element_blank(),   
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    axis.text.y = element_blank(),
    plot.title = element_blank(),
    axis.title.x = element_text(size = 14, 
                                family =  "serif")
  ) +
  
  
  
  labs(y="Relative effect of estimates (%)")+
  
  labs(subtitle  = expression(Adj.~ italic(R)^2==83~ '%' ))+
  labs(title = expression(All~plots))+
  
  theme(plot.title = element_text(hjust = 0, face = "bold")
        
        
  )+
  
  theme(plot.subtitle = element_text(size = 14, family = "serif") 
  )+
  theme(plot.title = element_text(size = 14, 
                                  family =  "serif"))
p_all2_PG
#################
dfP2<-read.csv("FD_Fixed_Low_220.csv",check.names = F)

colnames(dfP2) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)"   )

unique(dfP2$var)
dfP2 <- dfP2 %>%
   dplyr::mutate(across(where(is.numeric), round,2))


dfP2 <- dfP2 %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"FDis" ,"FD_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","FDis:SoilThickness", 
               "FDis:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))


cols1=c("#463D09" ,"#FFF1CC", "#9AB560" , "#E7E5E6")
p_all2_PL=dfP2 %>% 
  dplyr::mutate(group=fct_relevel(group,
                                  rev(c("Stand structural", "Diversity",
                                        "Environment","Interaction")))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value)*100) %>% 
  ggplot(aes(x = 1, y = new_col, label = group)) +
  geom_col(aes(fill = group, color = group), alpha =1, show.legend = FALSE) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1 ) +
  coord_flip()+
  
  scale_fill_manual(values = cols1)+
  scale_y_continuous(expand = c(0,0), limits = c(0,100), breaks = c(0,20,40,60,80,100))+
  
  
  theme_minimal()+
  theme(
    axis.text.x = element_blank(),  
    #axis.text.x = element_text(size = 10, family =  "serif"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),  
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank( ), 
    axis.text.y = element_blank(),  
    plot.title = element_blank()
  )+
  
  labs(y="Relative effect of estimates (%)")+
  
  labs(subtitle  = expression(Adj.~ italic(R)^2==89~ '%' ))+
  labs(title = expression(Low-carbon-sequestration))+
  
  theme(plot.title = element_text(hjust = 0, face = "bold"))+
  
  theme(plot.subtitle = element_text(size = 14, family = "serif") 
  )+
  theme(plot.title = element_text(size = 14, 
                                  family =  "serif"))
p_all2_PL
#################
dfP3<-read.csv("FD_Fixed_High_220.csv",check.names = F)

colnames(dfP3) <- c("var" ,"Estimate", "Std. Error", "df" , "t.value",   "Pr(>|t|)"   )

unique(dfP3$var)
dfP3 <- dfP3 %>%
  dplyr::mutate(across(where(is.numeric), round,2))



dfP3 <- dfP3 %>%
  filter(!( var %in% c("(Intercept)") )  ) %>%
  dplyr::mutate(group = case_when(
    var %in% c("Age","AverageDBH" ,"DBH_CV"  ,"Density") ~ "Stand structural",
    var %in% c( "PD" ,"FDis" ,"FD_Broadleaf") ~ "Diversity",
    var %in% c("MAT", "MAP" , "SoilThickness" ,"HumusLayerThickness") ~ "Environment",
    var %in% c("PD:SoilThickness","FDis:SoilThickness", 
               "FDis:HumusLayerThickness" ,"PD:HumusLayerThickness"  ) ~ "Interaction", 
    
    TRUE ~ NA_character_  # In case there are any terms not listed above
  ))


cols1=c("#463D09" ,"#FFF1CC", "#9AB560" , "#E7E5E6")


p_all2_PH=dfP3 %>% 
  dplyr::mutate(group=fct_relevel(group,
                                  rev(c("Stand structural", "Diversity",
                                        
                                        "Environment","Interaction"
                                  )))) %>% 
  group_by(group) %>% 
  dplyr::summarise(sum_value=sum(abs(Estimate))) %>% 
  dplyr::mutate(new_col=sum_value/sum(sum_value)*100) %>% 
  ggplot(aes(x = 1, y = new_col, label = group)) +
  geom_col(aes(fill = group, color = group), alpha =1, show.legend = FALSE) +
  scale_fill_manual(values = cols1) +
  scale_color_manual(values = cols1) +
  coord_flip()+
  
  scale_fill_manual(values = cols1)+
  scale_y_continuous(expand = c(0,0), limits = c(0,100), breaks = c(0,20,40,60,80,100))+
  
  
  theme_minimal()+
  theme(
    axis.text.x = element_blank(),  
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),  
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank( ), 
    axis.text.y = element_blank(),  
    plot.title = element_blank()
  )+
  
  labs(y="Relative effect of estimates (%)")+
  
  labs(subtitle  = expression(Adj.~ italic(R)^2==70~ '%' ))+
  labs(title = expression(High-carbon-sequestration))+

  
  theme(plot.title = element_text(hjust = 0, face = "bold"))+
  
  theme(plot.subtitle = element_text(size = 14, family = "serif") 
  )+
  theme(plot.title = element_text(size = 14, 
                                  family =  "serif"))

p_all2_PH

##########################
library(patchwork)
library(patchwork)
pGP1=ggarrange(p_all2_PH,p_all2_PL,p_all2_PG ,
               ncol =1,nrow = 3,
               heights = c(1,1),
               widths = c(1,1),common.legend = FALSE, align = c("hv"))
pGP1

######ggarrange##########
pG=ggarrange(
  pG1,
  pGP1,
  ncol =2,nrow =1,
  heights = c(1,1),
  widths = c(1,1),
  common.legend = FALSE, align = c("hv"))
pG

ggsave("Plot_Liear2_FD.png", path = "E:/",width =6,
       height =4.2,dpi=1200, plot=pG)
