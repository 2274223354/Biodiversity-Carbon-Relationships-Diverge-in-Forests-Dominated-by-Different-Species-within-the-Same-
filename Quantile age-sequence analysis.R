##====================================================================================================================================
## Biodiversity–Carbon Relationships Diverge in Forests Dominated by Different Species within the Same Plant Functional Type ---------
##====================================================================================================================================
library(ggplot2)
library(ggthemes)
library(ggplot2)
library(ggsignif)

#####Figure2.A Carbon among age groups#### 
c.data=read.csv("Plot Cunninghamia lanceolata.csv" )
colnames(c.data)
c.data=c.data[,c("Plot", "AGC","Age")]
c.data$species="310"

p.data =read.csv("Plot Pinus massoniana.csv" )
p.data=p.data[,c("Plot", "AGC","Age")]
p.data$species="220"


combined_data <- rbind(p.data, c.data)

combined_data <- combined_data %>%
  dplyr::mutate(
    Age = as.integer(Age), 
    AgeGroup1 = case_when(
      Age >= 1 & Age <= 10 ~ "A",  
      Age >= 11 & Age <= 20 ~ "B", 
      Age >= 21 & Age <= 30 ~ "C", 
      Age >= 31 & Age <= 40 ~ "D", 
      Age >= 41 & Age <= 50 ~ "E", 
      
      TRUE ~ NA_character_ 
    )
  ) 


agb_summary <- combined_data %>%
  group_by(AgeGroup1, species) %>% 
  dplyr::summarise(
    Mean_AGB = mean(AGC, na.rm = TRUE)
    
  )

agb_summary


test_results <- combined_data  %>%
  group_by(AgeGroup1) %>%
  filter(n_distinct(species) == 2) %>% 
  
  do(tidy(t.test(AGC ~ species, data = ., var.equal = FALSE))) %>%
  select(AgeGroup1, estimate, p.value) %>%
  mutate(
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  )

test_results




combined_data$AgeGroup1 <- factor(combined_data$AgeGroup1,
                                  levels = c("A", "B", "C", "D", "E"), 
                                  labels = c("1-10", "11-20", "21-30", "31-40", "41-50") ) 


####Plot####
combined_data$species[which(combined_data$species == 220)] = c("P. massoniana")
combined_data$species[which(combined_data$species == 310)] = c("C. lanceolata")

# Insert a dummy row to enhance plot aesthetics and alignment
combined_data <- combined_data %>%
  add_row(
    AgeGroup1 = "41-50",
    species = "C. lanceolata",
    Plot=800,
    AGC=0,
    
    Age=0,
    .before = 1 
  )

plotAgeGroupN <- ggplot(data = combined_data, aes(x = AgeGroup1
                                                  , y = AGC, fill = species)) +
  geom_jitter(aes(x = AgeGroup1, y = AGC, fill = species), alpha = 1, size =1, 
              color = "grey",
              position = position_jitterdodge(dodge.width = 0.8, 
                                              jitter.width = 0.1)) +
  geom_boxplot(aes(x = AgeGroup1, y = AGC, fill = species), outlier.shape = NA, 
               alpha = 1, width = 0.4, lwd = 0.20, 
               position = position_dodge(width = 0.8)) +
  stat_boxplot(geom = "errorbar", width = 0.1, lwd = 0.20, 
               color = "#000000", position = position_dodge(width = 0.8)) +
  
  scale_fill_manual(values = c( "#74D2E7","#C1D82F" )) +  
  scale_color_manual(values =c(  "#74D2E7","#C1D82F"  )) + 
  
  geom_signif(y_position=c(100), xmin=c(3.8), xmax=c(4.2),  
              annotation=c("**"), tip_length=0.02, size=0.5, textsize = 5, 
              vjust = 0.3) +
  
  theme_bw()+
  labs(x = "Age group")+
  ylab(expression("AGC "*"("*"Mg ha"^{-1}*")"))+
  ylim(0,150)+
  ggtitle("(a)")+
  theme_test()+
  theme(axis.text.x = element_text(size = 14,color="black",family="serif"),
        axis.text.y = element_text(size = 14,color="black",family="serif"))+
  theme(axis.title.y = element_text(size = 14, color="black",family="serif") )+
  theme(axis.title.x = element_text(size = 14, color="black",family="serif") )+
  theme(plot.title = element_text(size=18, color="black",family="serif"))+
  theme(panel.border = element_rect(fill=NA,color="black", size=0.5))+
  theme(
    legend.position = c(0.02, 0.98),  
    legend.justification = c(0, 1),   
    legend.margin = margin(l = 0, t = 0, unit = "cm")  # Adjust the margin
  )+
  
  theme(legend.text = element_text(size = 10,face = "italic" ,family = "serif"),
        legend.key.size = unit(4, "mm"),  
        legend.title = element_blank())  

plotAgeGroupN



#####Figure.2B carbon vs age#####
library(ggplot2)

c.data=read.csv("Plot Cunninghamia lanceolata.csv" )
colnames(c.data)
c.data=c.data[,c("Plot", "AGC","Age","Group"   )]
c.data$species="310"

p.data =read.csv("Plot Pinus massoniana.csv" )
p.data=p.data[,c("Plot", "AGC","Age","Group"   )]
p.data$species="220"

x <- c.data$Age
y <- c.data$AGC
x1 <- p.data$Age
y1 <- p.data$AGC


c.data90= c.data[c.data$Group== 2, ]
c.data10 = c.data[c.data$Group== 1, ]
x2 <- c.data90$Age
y2 <- c.data90$AGC
x3 <- c.data10$Age
y3 <- c.data10$AGC


p.data90= p.data[p.data$Group== 2, ]
p.data10= p.data[p.data$Group== 1, ]

x4 <- p.data90$Age
y4 <- p.data90$AGC
x5 <- p.data10$Age
y5 <- p.data10$AGC

#####Slope##############
rq(y ~ x, data = c.data, tau = 0.10) 
rq(y ~ x, data = c.data, tau = 0.50)
rq(y ~ x, data = c.data, tau = 0.90) 

rq(y1 ~ x1, data = p.data, tau = 0.10)
rq(y1 ~ x1, data = p.data, tau = 0.50)
rq(y1 ~ x1, data = p.data, tau = 0.90)
###################

fig_quantileN <-ggplot() +
  
  geom_point(data =c.data10,aes(x=x3, y=y3), colour =  "#74D2E7",size=2,shape=21,alpha=1,position = position_nudge(y = -0.5),show.legend = TRUE )+
  geom_point(data =c.data90,aes(x=x2, y=y2),colour =  "#74D2E7",size=2,shape=16,alpha=1, position = position_nudge(y = -0.5),show.legend = TRUE )+
  geom_point(data =p.data90,aes(x=x4, y=y4 ),colour = "#C1D82F",size=2,shape=17,alpha=1, position = position_nudge(y = -0.5),show.legend = TRUE )+
  geom_point(data =p.data10,aes(x=x5, y=y5 ), colour = "#C1D82F" ,size=2,shape=2,alpha=1, position = position_nudge(y = -0.5),show.legend = TRUE )+
  geom_quantile(data = c.data, aes(x = x, y = y), formula = y ~ x, quantiles = c(0.90),colour =  "#00a9c0",  size = 0.4,linetype = "solid", show.legend = TRUE ) +
  geom_quantile(data = c.data, aes(x = x, y = y), formula = y ~ x, quantiles = c(0.10), colour =  "#00a9c0"  , size = 0.4, linetype = "dotted", show.legend = TRUE) +
  
  
  geom_quantile(data = p.data, aes(x = x1,y = y1), formula = y ~ x, quantiles = c(0.90),colour =  "#6FAC6F" , size = 0.4,linetype = "solid", show.legend = TRUE)+
  geom_quantile(data = p.data, aes(x = x1,y = y1), formula = y ~ x, quantiles = c(0.10),colour = "#6FAC6F" , size = 0.4, linetype ="dotted", show.legend = TRUE)+
  
  
  
  
  geom_quantile(data = c.data, aes(x = x,y = y), formula = y ~ x, quantiles = c(0.50),
                colour = "#00a9c0",size = 0.4,linetype = "dashed", show.legend = TRUE)+
  geom_quantile(data = p.data, aes(x = x1,y = y1), formula = y ~ x, quantiles = c(0.50),
                colour ="#6FAC6F", size = 0.4, linetype ="dashed" , show.legend = TRUE)+
  
  
  scale_color_manual(
    values = c( "#00a9c0","#00a9c0",
                "#00a9c0",
                
                "#6FAC6F",
                "#6FAC6F",
                
                "#6FAC6F"
    ),
    
    labels = c(
      expression(italic("Cunninghamia lanceolata")~10^"th" ~ quantile),
      expression(italic("Cunninghamia lanceolata")~ 50^"th" ~ quantile),
      expression(italic("Cunninghamia lanceolata")~ 90^"th" ~ quantile),
      
      
      expression(italic("Pinus massoniana")~10^"th" ~ quantile),
      expression(italic("Pinus massoniana")~50^"th" ~ quantile),
      expression(italic("Pinus massoniana")~90^"th" ~ quantile)
    ),)+
  
  
  theme_bw()+
  scale_x_continuous(name="Age (years)") +
  
  ylab(expression("AGC "*"("*"Mg ha"^{-1}*")"))+
  # scale_y_continuous(breaks = seq(0,120,20), limits = c(-1,130))+ 
  ylim(0,150)+
  guides(color = guide_legend(title = ""))+
  
  ggtitle("(b)")+
  
  theme(
    plot.title = element_text(size = 18, family = "serif"),
    axis.text.x = element_text(size = 14, color = "black", family = "serif"),
    axis.text.y = element_text(size = 14, color = "black", family = "serif"),
    axis.title.x = element_text(size = 14, color = "black", family = "serif"),
    axis.title.y = element_text(size = 14, color = "black", family = "serif"),
    panel.border = element_rect(fill = NA, color = "black", size = 0.3, linetype = "solid"),
    legend.position = "bottom", 
    legend.justification = c(0.1, 0.4),
    legend.text = element_text(size = 10, face = "italic", family = "serif"),
    legend.title = element_blank(),
    panel.grid = element_blank()
  )


fig_quantileN
####
####marginal plot: density####

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(grDevices)
data_table1 <- data.frame(Age = x, Carbon  = y)
data_table1$species=310
data_table2 <- data.frame(Age = x1, Carbon = y1)
data_table2$species=220
combined_table <- rbind(data_table1, data_table2)
combined_table$species = factor(combined_table$species,levels = c('220','310'))
xdens=axis_canvas(fig_quantileN, axis = "x" ) +
  geom_density(data=combined_table,aes(x= Age,group=species,fill=species), alpha=0.7,size=0.1)+
  scale_fill_manual(values = c( "#C1D82F", "#74D2E7")) +
  theme_classic() 

ydens=axis_canvas(fig_quantileN,axis="y",coord_flip=T) +
  geom_density(data=combined_table,aes(x=Carbon,group=species,fill=species),alpha=0.7,size=0.1) +
  scale_fill_manual(values = c(  "#C1D82F", "#74D2E7")) +
  #fill_palette("jco")+
  coord_flip()
p1=insert_xaxis_grob(fig_quantileN, xdens, grid::unit(.2, "null"), position = "top")
p2=insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p2)

#######################
library(stats)
library(ggpubr)
fig_ageN= ggarrange(plotAgeGroupN,p2,    
                    ncol = 2,nrow = 1,
                 
                    common.legend = FALSE, align = c("hv"))

fig_ageN

ggsave("fig1.png", path = "E:/",
       width =8, height =4,
       dpi=600, plot=fig_ageN)

