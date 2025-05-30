#FIGURE 2 PLOT - STANDARD SPECTRA SHOWING REGIONS OF INTEREST, PLUS FEASIBILITY MODEL RESULTS

# load packages

library(tidyverse)
library(prospectr)
library(openxlsx)
library(patchwork)
library(ggpmisc)


## STANDARD SPECTRA REGIONS OF INTEREST

# load data and add labels for constituent and type of constituent

data <- read.xlsx("../data/raw/laboratoryatline_ringcup_standardSpectra.xlsx") %>% 
  mutate(type = ifelse(constituent == "Glucose", "Reactant", 
                       ifelse(constituent == "Xylose", "Reactant", "Product"))) %>% 
  mutate(type = factor(type, levels = c("Reactant", "Product"))) %>% 
  mutate(constituent = factor(constituent, levels = c("Glucose", "Xylose","BDO" , "Acetoin","Glycerol")))

# save spectral columns as vector object for easy processing and graphing selection

cols <- colnames(data)[5:2139]

# perform signal processing on spectra

sg <- data %>% select(all_of(cols)) %>% standardNormalVariate() %>% savitzkyGolay(m=1, p = 2, w= 11) %>% as.data.frame()

# plot each constituent signal, zoomed in to combinational region

aa <- sg %>%
  cbind(data %>% select(!any_of(cols))) %>% 
  pivot_longer(cols = any_of(cols), names_to = "wn", values_to = "abs") %>% 
  mutate(wn = as.numeric(wn)) %>% 
  ggplot(aes(x=wn,y = abs, group = constituent, col = constituent))+
  geom_line(size = 1, alpha = .8)+
  theme_bw()+
  labs(x=bquote('Wavenumber '(cm^-1)), 
       y = "", 
       title ="Combinational Region",
       tag = "(B)")+
  scale_x_reverse()+
  xlim(4800, 4000)+
  ylim(-0.06,0.03)+
  facet_wrap(~constituent, nrow = 5)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(hjust = .5,size = 8,face = "bold"),
        plot.tag = element_text(face = "bold"))+
  scale_color_manual(name = "Constituent", 
                     values = c("#feb24c","#f03b20","#7570b3","#99d594","#3288bd") )+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.width = unit(0.5,"mm"),
        strip.text.y = element_blank(),
        plot.margin = unit(c(1,1,1,0), "pt"),
        strip.text.x= element_blank(),
        strip.background = element_blank(),
        legend.key.height = unit(2,"mm"),
        panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))
aa

# plot each constituent signal, zoomed in to 1st overtone region 

bb<- sg %>%
  cbind(data %>% select(!any_of(cols))) %>% 
  pivot_longer(cols = any_of(cols), names_to = "wn", values_to = "abs") %>% 
  mutate(wn = as.numeric(wn)) %>% 
  ggplot(aes(x=wn,y = abs, group = constituent, col = constituent))+geom_line(size = 1, alpha = .8)+theme_bw()+
  labs(x=bquote('Wavenumber '(cm^-1)), 
       y = "Transformed Transflectance",  title ="1st Overtone Region", tag = "(A)")+scale_x_reverse()+xlim(6100, 5600)+
  ylim(-0.006,0.003)+
  facet_wrap(~constituent, nrow = 5)+
theme(legend.position = "none",
      text = element_text(size = 8),
      plot.title = element_text(hjust = .5,size = 8,face = "bold"),
      plot.tag = element_text(face = "bold"))+
  scale_color_manual(name = "Constituent", values = c("#feb24c","#f03b20","#7570b3","#99d594","#3288bd") )+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.width = unit(0.5,"mm"), 
        strip.text.y = element_blank(),
        plot.margin = unit(c(1,1,1,0), "pt"),
        strip.text.x= element_blank(),
        strip.background = element_blank(),
        legend.key.height = unit(2,"mm"),
        panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))

bb+aa


# MOCK MODEL RESULTS

#load mock constituent model result data 

mockdata <- readRDS("../data/processed/laboratoryatline_mocksample_modelresults.RDS")


# turn results into a long dataframe for easy plotting of results with faceting

longdata <- data.frame(
  Constituent = "BDO",
  CVPredicted = mockdata$bdo_cv,
  Measured = mockdata$totalBDO_gperL
) %>% 
  rbind(
    data.frame(
      Constituent = "Glucose",
      CVPredicted = mockdata$glucose_cv,
      Measured = mockdata$glucose_gperL
    ) 
  )%>% rbind(
    data.frame(
      Constituent = "Xylose",
      CVPredicted = mockdata$xylose_cv,
      Measured = mockdata$xylose_gperL
    ) 
  )%>% rbind(
    data.frame(
      Constituent = "Acetoin",
      CVPredicted = mockdata$acetoin_cv,
      Measured = mockdata$acetoin_gperL
    ) 
  )%>% rbind(
    data.frame(
      Constituent = "Glycerol",
      CVPredicted = mockdata$glycerol_cv,
      Measured = mockdata$glycerol_gperL
    ) 
  ) 

## CALCULATE AVERAGE CV ERROR

rmsecv <- longdata %>% 
  mutate(Constituent = factor(Constituent, levels = c("Glucose","Xylose","BDO", "Acetoin","Glycerol"))) %>% 
  group_by(Constituent) %>% 
  summarize(RMSECV =str_trunc(as.character(
    round(caret::RMSE(CVPredicted, Measured),4))
    , width =4, side = "right", ellipsis = ""))


# join average error calculations to results to label with plots 

longdata <- longdata %>%
  left_join(rmsecv, by = c("Constituent"= "Constituent")) %>% 
  mutate(label = paste0("RMSECV = \n",RMSECV))


# plot CV results across each constituent

c <-  longdata %>% 
  mutate(Constituent = factor(Constituent, levels = c("Glucose","Xylose","BDO", "Acetoin","Glycerol"))) %>% 
  ggplot(aes(x=CVPredicted, y=Measured, col = Constituent))+geom_point()+
  geom_abline(slope = 1)+
  theme_bw()+
  scale_color_manual(name = "Constituent", values = c("#feb24c","#f03b20","#7570b3","#99d594","#3288bd") )+
  labs(x="Predicted (g/L)",y="Measured (g/L)", title = "Feasibility Model Results",
       tag = "(C)")+
  facet_wrap(~Constituent, nrow = 5)+
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5),
        plot.tag = element_text(face = "bold"))+
  geom_text( x= 17, y = 50,
           aes(label = label),size = 3, col = "black")+
  theme(plot.title = element_text(size = 8,face = "bold"),
        legend.key.width = unit(0.5,"mm"), 
        text = element_text(size = 8),
        strip.text.y = element_blank(),
        strip.text.x= element_blank(),
        strip.background = element_blank(),
        legend.key.height = unit(2,"mm"),
        panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))

# plot signal and CV results together 

d <- (bb+aa)+c

# add labels and formatting for the entire plot

labels <- data.frame(x = 1, y= 1,
  Constituent = factor(c("Glucose","Xylose","2,3-BDO", "Acetoin","Glycerol"),
                         levels = c("Glucose","Xylose","2,3-BDO", "Acetoin","Glycerol")))%>%
  ggplot(aes(x=x, y = y , label = Constituent))+geom_label(size = 4, label.size = 0)+facet_wrap(~Constituent, nrow =5)+
  theme_bw()+
  theme(strip.text = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())
    
labels +d
e <- d+plot_layout(axis_titles = "collect")

labels+e+plot_layout(widths = c(.1, 1))


ggsave("../figures/figure2_stds_mock.tiff", width  = 200,height = 200 ,units = "mm", compression = "lzw")

