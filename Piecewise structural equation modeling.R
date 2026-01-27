library(lmerTest)
library(piecewiseSEM)
library(data.table)
library(pROC)
library(plyr)
library(dplyr)
library(doParallel)
library(foreign)
##===============================================================================
## PD:PSEM ---------
##===============================================================================
####Cunninghamia lanceolata####
####PSEM_All:PSEM####
rm(list=ls())

data_sem2=read.csv("Plot Cunninghamia lanceolata.csv" )

data_sem2=data_sem2[,-c(21)]


data_sem2[, c("AverageDBH"   , "Density", "AGC","DBH_CV")] <- lapply(
  data_sem2[, c("AverageDBH"  , "Density", "AGC","DBH_CV")],
  function(x) log(as.numeric(x))
)
colnames( data_sem2 )

data_sem2[, c( 4:20)] <- lapply(
  data_sem2[, c( 4:20)],
  function(x) as.numeric(scale(x))
)


data_sem2 <- as.data.frame(data_sem2)


mod_220_All<-psem(
  lmer(AverageDBH~ Density+
         Age+DBH_CV +
         (1|RegionName)
       ,data=data_sem2),
  
  lmer(DBH_CV~ 
         Age+
         Shannon +
         (1|RegionName)
       ,data=data_sem2),
  
  lmer(Density~ 
         Age+
         Shannon +
         
         PD     + HumusLayerThickness+(1|RegionName)
       ,data=data_sem2),
  
  
  
  lmer(PD~ MAT+
        
         MAP+
         Shannon+shan_Broadleaf+
        
         (1|RegionName)
       , data=data_sem2),
  
  lmer(Shannon~
         Age + (1|RegionName)
       , data=data_sem2),
  
  lmer( AGC~ Age+AverageDBH+
          Density+ 
          DBH_CV +
         
          PD +    
        
          MAT+
          MAP+
        
          SoilThickness  + (1|RegionName)
        ,data=data_sem2),
  Shannon%~~%shan_Broadleaf
  
)

summary(mod_220_All, .progressBar = FALSE) 

AIC(mod_220_All, AIC.type = "dsep",
    aicc = T )

fn<-paste("C_psem_mode_All.R")
save(mod_220_All,file=fn)

mod_bor=mod_220_All

# 
pz_bor <- lapply(mod_bor[1:(length(mod_bor) - 1)], function(x) { which(summary(x)$co[, grep('P', colnames(summary(x)$co))] > 0.05) })
newmod_bor <- lapply(1:length(pz_bor), function(i) {
  dropcoeff <- vector()
  if (length(pz_bor[[i]][!names(pz_bor[[i]]) %in% '(Intercept)']) > 0) {
    dropcoeff <- names(pz_bor[[i]])[!names(pz_bor[[i]]) %in% '(Intercept)']
    update(mod_bor[[i]], as.formula(paste('. ~ . -', paste(dropcoeff, collapse = '-'))), data = mod_bor[[i]]@frame)
  } else {
    mod_bor[[i]]
  }
})
newmod_bor
fn_new_bor <- "C_psem_Newmode_All.R"
save(newmod_bor, file = fn_new_bor)

####PSEM_High:PSEM #########################
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



mod_220_High<-psem(
  lmer(AverageDBH~ 
         
         SoilThickness +
         (1|RegionName)
       ,data=data_sem2),
  

  
  lmer( AGC~ AverageDBH+
         
          Shannon +shan_Broadleaf + MAP++(1|RegionName) ,data=data_sem2)

)


summary(mod_220_High, .progressBar = FALSE) 

AIC(mod_220_High, AIC.type = "dsep",
    aicc = T )

fn<-paste("C_psem_mode_High.R")
save(mod_220_High,file=fn)

mod_bor=mod_220_High

# 
pz_bor <- lapply(mod_bor[1:(length(mod_bor) - 1)], function(x) { which(summary(x)$co[, grep('P', colnames(summary(x)$co))] > 0.05) })
newmod_bor <- lapply(1:length(pz_bor), function(i) {
  dropcoeff <- vector()
  if (length(pz_bor[[i]][!names(pz_bor[[i]]) %in% '(Intercept)']) > 0) {
    dropcoeff <- names(pz_bor[[i]])[!names(pz_bor[[i]]) %in% '(Intercept)']
    update(mod_bor[[i]], as.formula(paste('. ~ . -', paste(dropcoeff, collapse = '-'))), data = mod_bor[[i]]@frame)
  } else {
    mod_bor[[i]]
  }
})
newmod_bor
fn_new_bor <- "C_psem_Newmode_High.R"
save(newmod_bor, file = fn_new_bor)


####PSEM_Low:PSEM #########################
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



mod_220_Low<-psem(
  lmer(AverageDBH~ Density+
         DBH_CV +(1|RegionName) 
       
       ,data=data_sem2),
  
  lmer(DBH_CV~ 
         PD+
       
         (1|RegionName) 
       ,data=data_sem2),
  
  lmer(PD~ 
         Shannon+
         (1|RegionName)
       , data=data_sem2),
  
 
  
  lmer( AGC~ AverageDBH+
          Density+ 
          
          HumusLayerThickness+(1|RegionName)
        ,data=data_sem2)
  
  
)

summary(mod_220_Low, .progressBar = FALSE) 

AIC(mod_220_Low, AIC.type = "dsep",
    aicc = T )



fn<-paste("C_psem_mode_Low.R")
save(mod_220_Low,file=fn)

mod_bor=mod_220_Low

# 
pz_bor <- lapply(mod_bor[1:(length(mod_bor) - 1)], function(x) { which(summary(x)$co[, grep('P', colnames(summary(x)$co))] > 0.05) })
newmod_bor <- lapply(1:length(pz_bor), function(i) {
  dropcoeff <- vector()
  if (length(pz_bor[[i]][!names(pz_bor[[i]]) %in% '(Intercept)']) > 0) {
    dropcoeff <- names(pz_bor[[i]])[!names(pz_bor[[i]]) %in% '(Intercept)']
    update(mod_bor[[i]], as.formula(paste('. ~ . -', paste(dropcoeff, collapse = '-'))), data = mod_bor[[i]]@frame)
  } else {
    mod_bor[[i]]
  }
})
newmod_bor
fn_new_bor <- "C_psem_Newmode_Low.R"
save(newmod_bor, file = fn_new_bor)

#################################################################################
####Pinus massoniana####
####PSEM_All:PSEM####
rm(list=ls())

data_sem2=read.csv("Plot Pinus massoniana.csv" )
data_sem2=data_sem2[,-c(21)]

data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV")] <- lapply(
  data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV")],
  function(x) log(as.numeric(x))
)

colnames(data_sem2  )


data_sem2[, c(4:20)] <- lapply(
  data_sem2[, c(4:20)],
  function(x) as.numeric(scale(x))
)

mod_220_All<-psem(
  lmer(AverageDBH~ Density+
         Age+
         DBH_CV +
        
         (1|RegionName)
       ,data=data_sem2),
  
  lmer(DBH_CV~ 
         Age+ Shannon + (1|RegionName),data=data_sem2),

  lmer(Density~  PD+(1|RegionName) ,data=data_sem2),
        

  lmer(PD~ 
         Age+Shannon+shan_Broadleaf+ SoilThickness +(1|RegionName)
       , data=data_sem2),
  
  lmer(Shannon~
         
         Age+
         
         SoilThickness + (1|RegionName)
       , data=data_sem2),
  lmer( AGC~ Age+AverageDBH+
          Density+ 
          
          shan_Broadleaf+ 
          (1|RegionName)
        ,data=data_sem2),
  
  Shannon%~~%shan_Broadleaf
)

summary(mod_220_All, .progressBar = FALSE) 

AIC(mod_220_All, AIC.type = "dsep",
    aicc = T )
fn<-paste("P_psem_mode_All.R")
save(mod_220_All,file=fn)

mod_bor=mod_220_All

# 
pz_bor <- lapply(mod_bor[1:(length(mod_bor) - 1)], function(x) { which(summary(x)$co[, grep('P', colnames(summary(x)$co))] > 0.05) })
newmod_bor <- lapply(1:length(pz_bor), function(i) {
  dropcoeff <- vector()
  if (length(pz_bor[[i]][!names(pz_bor[[i]]) %in% '(Intercept)']) > 0) {
    dropcoeff <- names(pz_bor[[i]])[!names(pz_bor[[i]]) %in% '(Intercept)']
    update(mod_bor[[i]], as.formula(paste('. ~ . -', paste(dropcoeff, collapse = '-'))), data = mod_bor[[i]]@frame)
  } else {
    mod_bor[[i]]
  }
})
newmod_bor
fn_new_bor <- "P_psem_Newmode_All.R"
save(newmod_bor, file = fn_new_bor)

####PSEM_High:PSEM #########################
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


mod_220_High <-psem(
  lmer(AverageDBH~ Density+
         DBH_CV +
        
         (1|RegionName)
       
       ,data=data_sem2),
  
  lmer(DBH_CV~ 
         Shannon   +(1|RegionName)
       
       ,data=data_sem2),
  
  
  lmer(PD~ Shannon  +(1|RegionName)
       , data=data_sem2),
  
  
  lmer( AGC~ AverageDBH+
         
          DBH_CV +
          Shannon +  
         
          PD   +

          (1|RegionName)
        ,data=data_sem2)
  
  
)

summary(mod_220_High, .progressBar = FALSE) 

AIC(mod_220_High, AIC.type = "dsep",
    aicc = T )

fn<-paste("P_psem_mode_High.R")
save(mod_220_High,file=fn)

mod_bor=mod_220_High

# 
pz_bor <- lapply(mod_bor[1:(length(mod_bor) - 1)], function(x) { which(summary(x)$co[, grep('P', colnames(summary(x)$co))] > 0.05) })
newmod_bor <- lapply(1:length(pz_bor), function(i) {
  dropcoeff <- vector()
  if (length(pz_bor[[i]][!names(pz_bor[[i]]) %in% '(Intercept)']) > 0) {
    dropcoeff <- names(pz_bor[[i]])[!names(pz_bor[[i]]) %in% '(Intercept)']
    update(mod_bor[[i]], as.formula(paste('. ~ . -', paste(dropcoeff, collapse = '-'))), data = mod_bor[[i]]@frame)
  } else {
    mod_bor[[i]]
  }
})
newmod_bor
fn_new_bor <- "P_psem_Newmode_High.R"
save(newmod_bor, file = fn_new_bor)

####PSEM_Low:PSEM #########################
rm(list=ls())

data_sem2=read.csv("Plot Pinus massoniana.csv" )
colnames(data_sem2)

data_sem2 = data_sem2%>% filter(Group %in% c("1"))


data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV" )] <- lapply(
  data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV" )],
  function(x) log(as.numeric(x))
)

colnames(data_sem2  )


data_sem2[, c(4:20)] <- lapply(
  data_sem2[, c(4:20)],
  function(x) as.numeric(scale(x))
)



mod_220_Low <-psem(
  lmer(AverageDBH~ Density+DBH_CV + (1|RegionName)  ,data=data_sem2),

  lmer( AGC~ AverageDBH+ Density+ Shannon +   (1|RegionName),data=data_sem2)

)

summary(mod_220_Low, .progressBar = FALSE) 

AIC(mod_220_Low, AIC.type = "dsep",
    aicc = T )

ofn<-paste("P_psem_mode_Low.R")
save(mod_220_Low,file=fn)

mod_bor=mod_220_Low

# 
pz_bor <- lapply(mod_bor[1:(length(mod_bor) - 1)], function(x) { which(summary(x)$co[, grep('P', colnames(summary(x)$co))] > 0.05) })
newmod_bor <- lapply(1:length(pz_bor), function(i) {
  dropcoeff <- vector()
  if (length(pz_bor[[i]][!names(pz_bor[[i]]) %in% '(Intercept)']) > 0) {
    dropcoeff <- names(pz_bor[[i]])[!names(pz_bor[[i]]) %in% '(Intercept)']
    update(mod_bor[[i]], as.formula(paste('. ~ . -', paste(dropcoeff, collapse = '-'))), data = mod_bor[[i]]@frame)
  } else {
    mod_bor[[i]]
  }
})
newmod_bor
fn_new_bor <- "P_psem_Newmode_Low.R"
save(newmod_bor, file = fn_new_bor)

#################################################################################
#################################################################################
##===============================================================================
## FD:PSEM ---------
##===============================================================================
####Cunninghamia lanceolata####
####PSEM_All:PSEM####
rm(list=ls())

data_sem2=read.csv("Plot Cunninghamia lanceolata.csv" )
colnames(data_sem2)
data_sem2=data_sem2[,-c(21)]


data_sem2[, c("AverageDBH"   , "Density", "AGC","DBH_CV")] <- lapply(
  data_sem2[, c("AverageDBH"  , "Density", "AGC","DBH_CV")],
  function(x) log(as.numeric(x))
)


data_sem2[, c( 4:20)] <- lapply(
  data_sem2[, c( 4:20)],
  function(x) as.numeric(scale(x))
)


data_sem2 <- as.data.frame(data_sem2)


mod_220_All<-psem(
  lmer(AverageDBH~ Density+
         Age+DBH_CV +
         
         (1|RegionName)
       
       
       ,data=data_sem2),
  
  lmer(DBH_CV~ 
         Age+Density+ 
         PD+ MAT
       
       +(1|RegionName)
       
       ,data=data_sem2),
  
  lmer(Density~ 
         Age+
         FDis +
         
         PD     + HumusLayerThickness+(1|RegionName)
       
       ,data=data_sem2),
  
  
  
  lmer(PD~ MAT+
         Age+
         MAP+
         
         (1|RegionName)
       , data=data_sem2),
  
  lmer(FDis~
         SoilThickness+ Age + (1|RegionName), data=data_sem2),

  lmer( AGC~ Age+AverageDBH+
          Density+ 
          DBH_CV +
          FDis +  
         
          
          MAT+
          MAP+
    
          SoilThickness  + (1|RegionName)
        ,data=data_sem2)#,
# FDis%~~%PD
  
)

summary(mod_220_All, .progressBar = FALSE) 

AIC(mod_220_All, AIC.type = "dsep",
    aicc = T )

fn<-paste("C_psem_mode_All_FD.R")
save(mod_220_All,file=fn)

mod_bor=mod_220_All

# 
pz_bor <- lapply(mod_bor[1:(length(mod_bor) - 1)], function(x) { which(summary(x)$co[, grep('P', colnames(summary(x)$co))] > 0.05) })
newmod_bor <- lapply(1:length(pz_bor), function(i) {
  dropcoeff <- vector()
  if (length(pz_bor[[i]][!names(pz_bor[[i]]) %in% '(Intercept)']) > 0) {
    dropcoeff <- names(pz_bor[[i]])[!names(pz_bor[[i]]) %in% '(Intercept)']
    update(mod_bor[[i]], as.formula(paste('. ~ . -', paste(dropcoeff, collapse = '-'))), data = mod_bor[[i]]@frame)
  } else {
    mod_bor[[i]]
  }
})
newmod_bor
fn_new_bor <- "C_psem_Newmode_All_FD.R"
save(newmod_bor, file = fn_new_bor)

####PSEM_High:PSEM #########################
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



mod_220_High<-psem(
  lmer(AverageDBH~ 
         SoilThickness +
         (1|RegionName)
       ,data=data_sem2),
  
  lmer( AGC~ AverageDBH+ FDis +FD_Broadleaf +(1|RegionName) ,data=data_sem2)
          
)


summary(mod_220_High, .progressBar = FALSE) 

AIC(mod_220_High, AIC.type = "dsep",
    aicc = T )

fn<-paste("C_psem_mode_High_FD.R")
save(mod_220_High,file=fn)

mod_bor=mod_220_High

# 
pz_bor <- lapply(mod_bor[1:(length(mod_bor) - 1)], function(x) { which(summary(x)$co[, grep('P', colnames(summary(x)$co))] > 0.05) })
newmod_bor <- lapply(1:length(pz_bor), function(i) {
  dropcoeff <- vector()
  if (length(pz_bor[[i]][!names(pz_bor[[i]]) %in% '(Intercept)']) > 0) {
    dropcoeff <- names(pz_bor[[i]])[!names(pz_bor[[i]]) %in% '(Intercept)']
    update(mod_bor[[i]], as.formula(paste('. ~ . -', paste(dropcoeff, collapse = '-'))), data = mod_bor[[i]]@frame)
  } else {
    mod_bor[[i]]
  }
})
newmod_bor
fn_new_bor <- "C_psem_Newmode_High_FD.R"
save(newmod_bor, file = fn_new_bor)


####PSEM_Low:PSEM #########################
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



mod_220_Low<-psem(
  lmer(AverageDBH~ Density+
         DBH_CV +(1|RegionName) 
       
       ,data=data_sem2),
  
  lmer(DBH_CV~ 
        
         PD+
         
         (1|RegionName) 
       ,data=data_sem2),
  
  
  
  lmer( AGC~ AverageDBH+
          Density+ 
         
          FDis+
         
          HumusLayerThickness+
          (1|RegionName)
        ,data=data_sem2)
)

summary(mod_220_Low, .progressBar = FALSE) 

AIC(mod_220_Low, AIC.type = "dsep",
    aicc = T )
########


fn<-paste("C_psem_mode_Low_FD.R")
save(mod_220_Low,file=fn)

mod_bor=mod_220_Low

# 
pz_bor <- lapply(mod_bor[1:(length(mod_bor) - 1)], function(x) { which(summary(x)$co[, grep('P', colnames(summary(x)$co))] > 0.05) })
newmod_bor <- lapply(1:length(pz_bor), function(i) {
  dropcoeff <- vector()
  if (length(pz_bor[[i]][!names(pz_bor[[i]]) %in% '(Intercept)']) > 0) {
    dropcoeff <- names(pz_bor[[i]])[!names(pz_bor[[i]]) %in% '(Intercept)']
    update(mod_bor[[i]], as.formula(paste('. ~ . -', paste(dropcoeff, collapse = '-'))), data = mod_bor[[i]]@frame)
  } else {
    mod_bor[[i]]
  }
})
newmod_bor
fn_new_bor <- "C_psem_Newmode_Low_FD.R"
save(newmod_bor, file = fn_new_bor)

#################################################################################
####Pinus massoniana####
####PSEM_All:PSEM####
rm(list=ls())

data_sem2=read.csv("Plot Pinus massoniana.csv" )


data_sem2=data_sem2[,-21]

data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV")] <- lapply(
  data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV")],
  function(x) log(as.numeric(x))
)

colnames(data_sem2  )


data_sem2[, c(4:20)] <- lapply(
  data_sem2[, c(4:20)],
  function(x) as.numeric(scale(x))
)

mod_220_All<-psem(
  lmer(AverageDBH~ Density+Age+ DBH_CV + MAP+(1|RegionName),data=data_sem2),

  lmer(DBH_CV~ 
         Age+
         
         PD+
         
         FDis +
         
         (1|RegionName)
       ,data=data_sem2),
  
  lmer(Density~ 
         PD+
       
         (1|RegionName)  
       
       
       ,data=data_sem2),
  
  
  lmer(PD~ 
         Age+ SoilThickness
       +(1|RegionName)
       , data=data_sem2),
  
  lmer(FDis~MAT+
       
         Age+
         
         SoilThickness + (1|RegionName)
       , data=data_sem2),
 
   lmer( AGC~ Age+AverageDBH+
          Density+ PD + MAT+ (1|RegionName)
        ,data=data_sem2)#,
  
  #FDis%~~% PD
)

summary(mod_220_All, .progressBar = FALSE) 

AIC(mod_220_All, AIC.type = "dsep",
    aicc = T )
fn<-paste("P_psem_mode_All_FD.R")
save(mod_220_All,file=fn)

mod_bor=mod_220_All

# 
pz_bor <- lapply(mod_bor[1:(length(mod_bor) - 1)], function(x) { which(summary(x)$co[, grep('P', colnames(summary(x)$co))] > 0.05) })
newmod_bor <- lapply(1:length(pz_bor), function(i) {
  dropcoeff <- vector()
  if (length(pz_bor[[i]][!names(pz_bor[[i]]) %in% '(Intercept)']) > 0) {
    dropcoeff <- names(pz_bor[[i]])[!names(pz_bor[[i]]) %in% '(Intercept)']
    update(mod_bor[[i]], as.formula(paste('. ~ . -', paste(dropcoeff, collapse = '-'))), data = mod_bor[[i]]@frame)
  } else {
    mod_bor[[i]]
  }
})
newmod_bor
fn_new_bor <- "P_psem_Newmode_All_FD.R"
save(newmod_bor, file = fn_new_bor)

####PSEM_High:PSEM #########################
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


mod_220_High <-psem(
  lmer(AverageDBH~ Density+
         DBH_CV +
         
         (1|RegionName)
       
       ,data=data_sem2),
  
  lmer(DBH_CV~ PD+
        
         (1|RegionName)
       
       ,data=data_sem2),
  
  
  
  
  lmer( AGC~ AverageDBH+
          
          DBH_CV +
       
          
          (1|RegionName)
        ,data=data_sem2)
  
  
)

summary(mod_220_High, .progressBar = FALSE) 


AIC(mod_220_High, AIC.type = "dsep",
    aicc = T )

fn<-paste("P_psem_mode_High_FD.R")
save(mod_220_High,file=fn)

mod_bor=mod_220_High

# 
pz_bor <- lapply(mod_bor[1:(length(mod_bor) - 1)], function(x) { which(summary(x)$co[, grep('P', colnames(summary(x)$co))] > 0.05) })
newmod_bor <- lapply(1:length(pz_bor), function(i) {
  dropcoeff <- vector()
  if (length(pz_bor[[i]][!names(pz_bor[[i]]) %in% '(Intercept)']) > 0) {
    dropcoeff <- names(pz_bor[[i]])[!names(pz_bor[[i]]) %in% '(Intercept)']
    update(mod_bor[[i]], as.formula(paste('. ~ . -', paste(dropcoeff, collapse = '-'))), data = mod_bor[[i]]@frame)
  } else {
    mod_bor[[i]]
  }
})
newmod_bor
fn_new_bor <- "P_psem_Newmode_High_FD.R"
save(newmod_bor, file = fn_new_bor)

####PSEM_Low:PSEM #########################
rm(list=ls())

data_sem2=read.csv("Plot Pinus massoniana.csv" )
colnames(data_sem2)

data_sem2 = data_sem2%>% filter(Group %in% c("1"))

data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV" )] <- lapply(
  data_sem2[, c("AverageDBH", "Density", "AGC","DBH_CV" )],
  function(x) log(as.numeric(x))
)

colnames(data_sem2  )


data_sem2[, c(4:20)] <- lapply(
  data_sem2[, c(4:20)],
  function(x) as.numeric(scale(x))
)



mod_220_Low <-psem(
  lmer(AverageDBH~ Density+
         
         DBH_CV +
         
         (1|RegionName)  
       
       
       ,data=data_sem2),
  
 
  lmer( AGC~ AverageDBH+
          Density+
          
          
          (1|RegionName)
        ,data=data_sem2)
  
  
)

summary(mod_220_Low, .progressBar = FALSE) 

AIC(mod_220_Low, AIC.type = "dsep",
    aicc = T )

fn<-paste("P_psem_mode_Low_FD.R")
save(mod_220_Low,file=fn)

mod_bor=mod_220_Low

# 
pz_bor <- lapply(mod_bor[1:(length(mod_bor) - 1)], function(x) { which(summary(x)$co[, grep('P', colnames(summary(x)$co))] > 0.05) })
newmod_bor <- lapply(1:length(pz_bor), function(i) {
  dropcoeff <- vector()
  if (length(pz_bor[[i]][!names(pz_bor[[i]]) %in% '(Intercept)']) > 0) {
    dropcoeff <- names(pz_bor[[i]])[!names(pz_bor[[i]]) %in% '(Intercept)']
    update(mod_bor[[i]], as.formula(paste('. ~ . -', paste(dropcoeff, collapse = '-'))), data = mod_bor[[i]]@frame)
  } else {
    mod_bor[[i]]
  }
})
newmod_bor
fn_new_bor <- "P_psem_Newmode_Low_FD.R"
save(newmod_bor, file = fn_new_bor)

