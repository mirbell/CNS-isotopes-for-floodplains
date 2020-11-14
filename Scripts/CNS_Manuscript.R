
# Libraries ---------------------------------------------------------------


library(tidyverse)
library(dplyr)
library(scales)
library(cowplot)
library(lubridate)
library(MASS)
library(lme4)
library(here)
library(openxlsx) # read in excel files xlsx
library(pals) #color scales
library(lemon)#facet axis 
library(sjstats)



# Preparing various data sets for different analysis ----------------------


data<-read.csv("C:/Users/mirbe/Box Sync/Isodope/Isodope/YoloBypass/Data/RData/UPDATEDallyears99_17.csv")
ldadata<-read.csv("C:/Users/mirbe/Box Sync/Isodope/Isodope/YoloBypass/Data/RData/LDA_allyears.csv")

table(data$Year)

data1<-data

data1<- data1 %>% 
  filter(! is.na(d34S) ) %>% 
  filter(! is.na(d13C))

data1<-data1[data1$Site !="San Joaquin",]
data1<-data1[data1$subsite !="Hatchery",]
data1<-data1[data1$Site !="Sutter",]
data1<-data1[data1$Tissue !="F",]
data1<-data1[data1$Site !="Hatchery",]

loc<- factor(data1$Site, levels=c("Yolo Bypass", "Sacramento", "River Caged"))
levels(data1$Site) <- gsub(" ", "\n", levels(data1$Site))



data2<-data
data2<-data2[data2$Tissue !="M",]
data2<-data2[data2$Tissue !="F",]
data2<-data2[data2$Site !="San Joaquin",]
data2<-data2[data2$Site !="Hatchery",]


data2$Site <- factor(data2$Site, levels=c("Yolo Bypass", "Sacramento"))


data3<-data
data3<-data3[data3$Tissue !="S",]
data3<-data3[data3$Tissue !="F",]
data3<-data3[data3$Site !="San Joaquin",]
data3<-data3[data3$Site !="Hatchery",]

data4<-data
data4<- data4[data4$Year !="2014",]
data4<- data4[data4$Year !="2015",]
data4<- data4[data4$Year !="2016",]
data4<-data4[data4$Site !="San Joaquin",]
data4<-data4[data4$Site !="Hatchery",]
data4<-data4[data4$Site !="Sacramento",]
data4<- data4[data4$Tissue != "F",]

data5<- data
data5<-data5[data5$Year !="1999",]
data5<-data5[data5$Year !="2012",]
data5<-data5[data5$Year !="2017",]
data5<-data5[data5$Site !="San Joaquin",]
data5<-data5[data5$Site != "Sacramento",]
data5<- data5[data5$Tissue != "F",]

data6<- data
data6<-data6[data6$Year !="1999",]
data6<-data6[data6$Year !="2012",]
data6<-data6[data6$Year !="2017",]
data6<-data6[data6$Year !="2014",]
data6<-data6[data6$Year !="2015",]
data6<-data6[data6$Site !="San Joaquin",]
data6<-data6[data6$Site != "Sacramento",]




# LDA model looking at d34S and d13C stomach contents values for c --------

ldadata<-ldadata %>% 
  filter(! is.na(d34S) ) %>% 
  filter(! is.na(d13C))


ldadata<-ldadata[ldadata$Tissue !="M",]
ldadata<-ldadata[ldadata$Tissue !="F",]

##check data for normality

qqnorm(ldadata$d13C, pch = 1, frame = FALSE)
qqline(ldadata$d13C, col = "steelblue", lwd = 2)

qqnorm(ldadata$d34S, pch = 1, frame = FALSE)
qqline(ldadata$d34S, col = "steelblue", lwd = 2)



###plot LDA data 
###This data includes both wild caught and caged reared juvenile Chinook Salmon in all years sample collection was possible. stomach contents must have both a d13C and d34S value. 

##Figure 4
ldaplot<- ggplot(data = ldadata, aes(x =d13C, y=d34S, color = Site, shape = Exp_Wild)) +
  geom_point(size = 2.5, alpha = 0.8) +
  stat_ellipse(aes(group= Site)) +
  geom_hline(yintercept = 0, linetype = "dashed", color ="black") +
  scale_color_manual(name = "Site",labels = c("Sacramento River", "Floodplain"), values = c( "blue2", "green3")) +
  scale_shape_manual(labels = c("Experimental", "Wild"), values = c(19,21)) +
  coord_cartesian(xlim = c(-40, -20), ylim = c(-12,15)) + labs( x=expression(paste(delta^"13", "C"["Inverts "],"(\211 VPDB)")), 
                                                                y = expression(paste(delta^"34", "S"["Inverts "],"(\211 VCDT)")))
ldaplot + theme_bw() +
  theme(legend.position=c(0.175,0.89), legend.background =  element_rect(fill="white", size=0.5, 
                                                                         linetype="solid", colour ="black")) + guides(shape = FALSE) 

###LDA test

fit <- lda(Site ~  d34S + d13C, data =ldadata, CV=TRUE)
ct <- table(ldadata$Site, fit$class)
diag(prop.table(ct, 1))
summary(fit)
sum(diag(prop.table(ct)))

fit <- lda(Site ~  d34S, data =ldadata, CV=TRUE)
ct <- table(ldadata$Site, fit$class)
diag(prop.table(ct, 1))
summary(fit)
sum(diag(prop.table(ct)))

plot(fit)


# Figure 4 ----------------------------------------------------------------

boxplot<- ggplot(data=data1, aes(x=loc, y = d34S, color = Tissue)) +
  geom_boxplot() +  
  labs(x = "", y =  expression(paste(delta^"34", "S","(\211 VCDT)")))  +
  geom_hline(yintercept = 0, linetype="dashed", color ="black") +
  scale_color_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3")) + 
  scale_x_discrete(labels=c("Yolo Bypass" = "Yolo \n Bypass", "Sacramento" = "Sacramento \n  River", "River Caged" = "Caged \n in Sac. River"))
boxplot + theme_bw() + facet_grid(WDN ~ .) + theme(legend.position = "bottom", legend.title= element_blank())

boxplotC<- ggplot(data=data1, aes(x=loc, y = d13C, color = Tissue)) +
  geom_boxplot() +  
  labs(x = "", y =  expression(paste(delta^"13", "C","(\211 VPBD)")))  +
  scale_color_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))
boxplotC + theme_bw() + facet_grid(WDN ~ .) + theme(legend.position = "bottom", legend.title= element_blank()) +
  scale_x_discrete(labels=c("Yolo Bypass" = "Yolo \n Bypass", "Sacramento" = "Sacramento \n River", "River Caged" = "Caged \nin Sac. River"))

csboxplot<- plot_grid((boxplotC + theme_bw() + facet_grid(WDN ~ .) +  scale_x_discrete(labels=c("Yolo Bypass" = "Yolo Bypass", "Sacramento" = "Sacramento  River", "River Caged" = "Caged in Sac. River")) + theme(legend.position= "none")) , (boxplot + theme_bw() + facet_grid(WDN ~ .)+ theme(legend.position= "none") ), align = 'h', labels =c("A", "B"), nrow = 1 )
csboxplot ##final figure


# Figure 5 ----------------------------------------------------------------

sulfurallyears<- ggplot (data5,aes(x= Day, y = d34S, color = Tissue)) + 
  geom_smooth (span = 0.75)  + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs( y = expression(paste(delta^"34", "S","(\211 VCDT)"))) +
  scale_color_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))


sulfurallyears +
  facet_grid( Year ~ .) + 
  theme_bw() +  
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black"))

carbonallyears<- ggplot (data5,aes(x= Day, y = d13C, color = Tissue)) + 
  geom_smooth (span = 0.75)  + geom_point() +
  labs( y = expression(paste(delta^"13", "C","(\211 VPBD)"))) +
  scale_color_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))


carbonallyears +
  facet_grid( Year ~ .) + 
  theme_bw() +  
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black"))

csdep<- plot_grid((carbonallyears + theme_bw() + facet_grid(Year ~ .) + theme(legend.position= "none")) , (sulfurallyears + theme_bw() + facet_grid(Year ~ .)+ theme(legend.position= "none") ), align = 'h', labels =c("A", "B"), nrow = 1 )
csdep ##final figure


# lmer model --------------------------------------------------------------

data7<-data
data7<-data7[data7$Tissue !="M",]
data7<-data7[data7$Tissue !="F",]
data7<-data7[data7$Site !="San Joaquin",]
data7<-data7[data7$Site !="Hatchery",]
data7<-data7[data7$Year !="2012",]
data7<-data7[data7$Year !="2013",]
data7<-data7[data7$Year !="1999",]

###for sulfur
model1<- lmer(d34S ~ R_FP + (1|subsite) + (1|WDN), data = data7, REML = FALSE)
summary(model1) ## best fit model based on multiple variations in r markdown
p_value(model1)
r2(model1)


model.null<-lmer(d34S ~ R_FP +  (1|WDN), data = data7, REML = FALSE)
summary(model.null)
anova(model1, model.null) ##biggest effect
coef(model1)

model.null3<-lmer(d34S ~ R_FP  + (1|subsite) , data = data7, REML = FALSE)
anova(model1, model.null3) ##small effect

###mixed effect models with tissues ###remove cages from all models (cage was not included as a random effect due to sample constrains)

###model for muscle tissue

data6<-data
data6<-data6[data6$Tissue !="S",]
data6<-data6[data6$Tissue !="F",]
data6<-data6[data6$Site !="San Joaquin",]
data6<-data6[data6$Site !="Hatchery",]
data6<-data6[data6$Year !="2012",]
data6<-data6[data6$Year !="2013",]
data6<-data6[data6$Year !="1999",]

m1<- lmer(d34S ~ R_FP + (1|subsite) + (1|WDN), data = data6, REML = FALSE)



summary(m1) ## best fit model with the cages being an interaction of site
p_value(m1)

m.null<-lmer(d34S ~ R_FP +  (1|WDN), data = data6, REML = FALSE)
anova(m1, m.null) ##biggest effect with subsite removed
coef(model1)

m.null2<- lmer(d34S ~ R_FP + (1|subsite) + (1|WDN), data = data6, REML = FALSE)
summary(m.null2)
anova(m1, m.null2) ##little to no effect without cages as an interaction

m.null3<-lmer(d34S ~ R_FP  + (1|subsite) , data = data6, REML = FALSE)
anova(m1, m.null3) ##small effect without water year type. Larger effect on muscle tissues compared to stomach contents

###for carbon stomach contents
modelC<- lmer(d13C ~ R_FP + (1|subsite) + (1|WDN), data = data7, REML = FALSE)
summary(modelC) ## best fit model based on multiple variations in r markdown
p_value(modelC)
r2(modelC)


model.nullC<-lmer(d13C ~ R_FP +  (1|WDN), data = data7, REML = FALSE)
summary(model.nullC)
anova(modelC, model.nullC) ##biggest effect
coef(modelC)

model.nullC2<-lmer(d13C ~ R_FP  + (1|subsite) , data = data7, REML = FALSE)
anova(model1, model.null3) ##small effect

###mixed effect models with tissues ###remove cages from all models (cage was not included as a random effect due to sample constrains)

##For d13C muscle tissues

mC<- lmer(d13C ~ R_FP + (1|subsite) + (1|WDN), data = data6, REML = FALSE)



summary(mC) 
p_value(m1)

mC.null<-lmer(d13C ~ R_FP +  (1|WDN), data = data6, REML = FALSE)
anova(mc, mC.null) ##biggest effect with subsite removed
coef(mC)

mC.null2<- lmer(d13C ~ R_FP + (1|subsite) + (1|WDN), data = data6, REML = FALSE)
summary(mC.null2)
anova(m1, m.null2) 

mC.null3<-lmer(d13C ~ R_FP  + (1|subsite) , data = data6, REML = FALSE)
anova(mC, mC.null3) 

# T-test for d15N data ----------------------------------------------------

t.test(d15N ~ R_FP, data2, mu=0,alt = "two.sided", conf = 0.95, var.eq = F, paired = F)


# Otolith analysis using on d34S ------------------------------------------
#Clear the workspace while developing the script
rm(list = ls())

#Custom function
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

#read in data
salmon_readin <-read.xlsx(here('salmon_s_data.xlsx'), sheet="Data")
tissue_readin <-read.xlsx(here('salmon_s_data.xlsx'), sheet="tissue")




#clean data
salmon_data <- salmon_readin %>%
  data.frame()%>%
  dplyr::select(Fish_ID, Sample, Spot_number, age, distance, Spot_designation, d34Scor_vcdt, wStdErr_95T_permil)%>%
  group_by(Fish_ID, Spot_designation)%>%
  mutate(region_average=mean(d34Scor_vcdt, na.rm=TRUE),
         region_sd=sd(d34Scor_vcdt, na.rm=TRUE))%>%
  ungroup()

tissue_data <-tissue_readin

##Figure 6

#Plot 2 Salmon S profiles
p_salmon2 <- ggplot(data= salmon_data%>%filter(Fish_ID=="NP163500"|Fish_ID=="NP163668"))+
  annotate("rect",ymin = -0.29, ymax = 4.71, xmin = -Inf, xmax = Inf,  fill = "steelblue3", alpha=.1)+
  annotate("rect",ymin = -5.74, ymax = -1.2, xmin = -Inf, xmax = Inf, fill = 'forestgreen', alpha=.1)+
  geom_vline(aes(xintercept=changedis), linetype="dashed", color="black")+
  geom_smooth(aes(x=distance, y=d34Scor_vcdt, group=Fish_ID), span=0.2, color="grey95", fill="grey95", alpha=0.5)+
  geom_pointrange(aes(x=distance, y=d34Scor_vcdt, ymax = d34Scor_vcdt +wStdErr_95T_permil, ymin = d34Scor_vcdt-wStdErr_95T_permil, 
                      fill=Spot_designation), shape=21, color="black")+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), 
         legend.position = "bottom",
         legend.title=element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_blank())+
  scale_x_continuous("Distance (µm)")+
  scale_y_continuous(name= expression(paste(delta^"34", "S"["Otolith"]," [‰ VCDT]")),  breaks = scales::pretty_breaks(n = 5))+
  scale_fill_manual(values=c("firebrick","palegreen3","orange","steelblue", "grey"))+
  facet_rep_wrap(~Fish_ID, ncol=1, repeat.tick.labels = 'all')
p_salmon2
ggsave(plot=p_salmon2, "salmon2_s_profiles.png", width = 6, height = 6)



#Tissue comparison (stomach, muscle, and otolith) 
salmon_tissue <- salmon_data %>%
  filter(Spot_designation=="River"| Spot_designation=="Floodplain")%>%
  group_by(Fish_ID)%>%
  mutate (d34_oto=case_when (distance==max(distance, na.rm=T)~ d34Scor_vcdt))%>%
  ungroup()%>%
  distinct(Fish_ID, d34_oto, .keep_all=T) %>%
  dplyr::select(Fish_ID,Sample, d34_oto)%>%
  drop_na()%>%
  left_join(tissue_data, by="Sample")%>%
  gather(Tissue,d34, 3:5)

###Figure 7
p_tissue <- ggplot(data= salmon_tissue)+
  geom_point(aes(x=Fish_ID, y=d34, shape=Tissue, fill=Tissue))+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), 
         legend.position = "top",
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_blank())+
  scale_x_discrete("Fish ID")+
  scale_y_continuous(name= expression(paste(delta^"34", "S"["Tissue"]," [VCDT ‰]")),  breaks = scales::pretty_breaks(n = 10))+
  scale_fill_manual(values = c("steelblue", "grey50", "palegreen3"))+
  scale_shape_manual(values=c(21,22,23))
p_tissue #final figure
ggsave(plot=p_tissue , "tissue_comparison.png", width = 4, height = 4)


# Appendix plots and tables -----------------------------------------------

# Flow data from the Central Valley 2014-2017 -----------------------------
##Data here was pulled from CDEC then cleaned into a data set 


flow<-read.csv("C:/Users/mirbe/Box Sync/Isodope/Isodope/YoloBypass/Data/RData/fremontweirflow2014_17.csv")




###Sulfur Table Appendix C 

sdata <- ddply(data1, c("Year", "Site", "Tissue"), summarise,
               N    = length(d34S),
               mean = mean(d34S),
               sd   = sd(d34S),
               se   = sd / sqrt(N)
)

sdata

###Carbon Table Appendix D

cdata <- ddply(data1, c("Year", "Site", "Tissue"), summarise,
                N    = length(d13C),
                mean = mean(d13C),
                sd   = sd(d13C),
                se   = sd / sqrt(N)
)
cdata

###Nitrogen Table Appendix E

ndata <- ddply(data1, c("Year", "Site", "Tissue"), summarise,
                N    = length(d15N),
                mean = mean(d15N),
                sd   = sd(d15N),
                se   = sd / sqrt(N)
                
)
ndata

###Appendix F

boxplotN<- ggplot(data=data1, aes(x=Site, y = d15N, color = Tissue)) +
  geom_boxplot() +  
  labs(x = "", y =  expression(paste(delta^"15", "N","(\211 Air)")))  +
  scale_color_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))
boxplotN + theme_bw() + facet_grid(WDN ~ .) + theme(legend.position = "bottom", legend.title= element_blank()) +
  scale_x_discrete(labels=c("Yolo Bypass" = "Yolo Bypass", "Sacramento" = "Sacramento River", "River Caged" = "Caged in Sac. River"))

###Appendix G

nitroallyears<- ggplot (data5,aes(x= Day, y = d15N, color = Tissue)) + 
  geom_smooth (span = 0.75)  + geom_point() +
  labs( y = expression(paste(delta^"15", "N","(\211 Air)"))) +
  scale_color_manual(labels = c("Muscle Tissue", "Stomach Contents"), values = c( "black", "green3"))


nitroallyears +
  facet_grid( Year ~ .) + 
  theme_bw() +  
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.background =  element_rect(fill="white", size=0.5, linetype="solid", colour ="black"))

###Appendic H

#Plot all Salmon S profiles
p_salmon <- ggplot(data= salmon_data)+
  annotate("rect",ymin = -0.29, ymax = 4.71, xmin = -Inf, xmax = Inf,  fill = "steelblue3", alpha=.1)+
  annotate("rect",ymin = -5.74, ymax = -1.2, xmin = -Inf, xmax = Inf, fill = 'forestgreen', alpha=.1)+
  geom_vline(aes(xintercept=changedis), linetype="dashed", color="black")+
  geom_smooth(aes(x=distance, y=d34Scor_vcdt, group=Fish_ID), span=0.2, color="grey95", fill="grey95", alpha=0.5)+
  geom_pointrange(aes(x=distance, y=d34Scor_vcdt, ymax = d34Scor_vcdt +wStdErr_95T_permil, ymin = d34Scor_vcdt-wStdErr_95T_permil, 
                      fill=Spot_designation), shape=21, color="black")+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), 
         legend.position = "bottom",
         legend.title=element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_blank())+
  scale_x_continuous("Distance (µm)")+
  scale_y_continuous(name= expression(paste(delta^"34", "S"["Otolith"]," [‰ VCDT]")),  breaks = scales::pretty_breaks(n = 5))+
  scale_fill_manual(values=c("firebrick","palegreen3","orange","steelblue", "grey"))+
  facet_rep_wrap(~Fish_ID, ncol=1, repeat.tick.labels = 'all')
p_salmon
ggsave(plot=p_salmon, "salmon_s_profiles.png", width = 6, height = 12)
















