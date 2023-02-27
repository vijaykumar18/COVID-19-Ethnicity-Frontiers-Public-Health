
# Add the necessary libraries
  library("readxl")
  library("GGally")
  library("mice")
  library("corrgram");
  library("Hmisc");
  library("MASS");
  library("dplyr")
  library("ggpubr")
  library("car")
  library("pwr")
  library("corrplot")
  library("ggplot2")
  library("qqplotr")
  library("nortest")
  library("mctest")
  library("plot3D")
  library("rstatix")
  library("olsrr")
  library("readr")

  NHW_Final <- read_csv("20210706_NHW_Final.csv")
  View(NHW_Final)
  NHB_Final <- read_csv("20210706_NHB_Final.csv")
  View(NHB_Final)
  Hispanic_Final <- read_csv("20210706_Hispanic_Final.csv")
  View(Hispanic_Final)
  
  
#Correlation 
#Change file name to get particular correlation matrix
  mydata <- Hispanic_Final %>%
    dplyr::select(`Hispanic % known Cases`,`Hispanic % known Death`,Poverty,Over60,Diabetes,Obesity,High_BP,CVD)
  head(mydata, 3)
  cor.mat <- mydata %>% cor_mat()
  cor.mat
  
  #For p-values
  cor.mat %>%
    cor_reorder() %>%
    pull_lower_triangle() %>%
    cor_plot(label = TRUE)
  cor.mat_long<-cor.mat %>% cor_gather()
  cor.mat_long$p<-round(cor.mat_long$p,digit=3)
  cor.mat_long%>%filter(var1=="Poverty")
  

#Regression
#Vif and MLR White Deaths
model <- lm(`NHW % known Death`~Obesity+Diabetes+Poverty+Over60+High_BP+CVD, data=NHW_Final)
summary(model)
confint(model, level=0.95)
car::vif(model)
model <- lm(`NHW % known Death`~ Obesity+Diabetes+Poverty, data=NHW_Final)
summary(model)
confint(model, level=0.95)
car::vif(model)



#Vif and MLR NHB Cases
model <- lm(`NHB % known Cases`~Obesity+Poverty+Diabetes+Over60+High_BP+CVD, data=NHB_Final)
summary(model)
confint(model, level=0.95)
car::vif(model)
model <- lm(`NHB % known Cases`~Obesity+Poverty, data=NHB_Final)
summary(model)
confint(model, level=0.95)
car::vif(model)

model <- lm(`NHB % known Death`~Obesity+Poverty+Diabetes+Over60+High_BP+CVD, data=NHB_Final)
summary(model)
confint(model, level=0.95)
car::vif(model)
model <- lm(`NHB % known Death`~Obesity+Poverty, data=NHB_Final)
summary(model)
confint(model, level=0.95)
car::vif(model)


model <- lm(`Hispanic % known Cases`~Diabetes+Poverty+Obesity+Over60+High_BP+CVD, data=Hispanic_Final)
summary(model)
confint(model, level=0.95)
car::vif(model)
model <- lm(`Hispanic % known Cases`~Diabetes+Poverty, data=Hispanic_Final)
summary(model)
confint(model, level=0.95)
car::vif(model)



model <- lm(`Hispanic % known Death`~Over60+Poverty+Diabetes+Obesity+High_BP+CVD, data=Hispanic_Final)
summary(model)
confint(model, level=0.95)
car::vif(model)
model <- lm(`Hispanic % known Death`~Over60+Poverty, data=Hispanic_Final)
summary(model)
confint(model, level=0.95)
car::vif(model)




#Residual plots Whit
# Define my theme for the ggplots
my_theme=theme(panel.background = element_rect(fill = "white"),
               panel.border = element_rect(color="black", fill="NA", size=1),
               panel.grid.major = element_line(size = 1, linetype = 'solid', color = "lightgray"),
               panel.grid.minor = element_line(size = 0.5, linetype = 'solid', color = "lightgray"),
               axis.text=element_text(size=30),
               axis.title=element_text(size=30),
               legend.title=element_text(size=20),
               legend.key.height= unit(0.27, "npc")) # height of thr colorbar if it exists


#For case and Death change the model and run following code
NHW_Cor<-NHW_Final%>%dplyr::select(`NHW % known Cases`,`NHW % known Death`,Diabetes,Poverty,Obesity)
NHW_Cor<-NHW_Cor%>%drop_na()

x_pts<-1:nrow(NHW_Cor);
model <- lm(`NHW % known Cases`~Diabetes+Poverty+Obesity, data=NHW_Final)
model <- lm(`NHW % known Death`~Diabetes+Poverty+Obesity, data=NHW_Final)
res<-residuals(model) # residuals
standard_res <- stdres(model) # standardized residuals
studentized_res <- studres(model) ; # studentized residuals
df_res<-data.frame(x_pts, res, standard_res, studentized_res); # dataset of residuals
df_res$res<-round(df_res$res,digits = 2)
# *** Plotting of Residuals with normal curve
lim<-1.2*ceiling(max(abs(min(df_res$res)), abs(max(df_res$res))));
P<-ggplot(df_res, aes(x=res)) + 
  my_theme+
  labs(x="Residuals", y="Density")+
  geom_histogram(aes(y =..density..), 
                 #breaks = seq(-3, 3, by = 0.5),
                 color="black", 
                 fill="lightblue", 
                 binwidth=1, 
                 size=1)+
  stat_function(fun = dnorm, args = list(mean = mean(df_res$res), sd = sd(df_res$res)), size=1)+
  xlim(-30,20);
P
P + scale_y_continuous(
  labels = scales::number_format(accuracy = 0.02),n.breaks = 5,limits = c(0,0.105))
# QQ plots 
ggplot(data = df_res, mapping = aes(sample = res))+
  stat_qq_band(conf = 0.95, size=0.5, fill="lightblue")  +
  stat_qq_line() +
  stat_qq_point(shape=21, size=2, stroke=1, fill="blue") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  my_theme+
  ylim(-20,20)+
  xlim(-20,20)





AM_Cor<-NHB_Final%>%dplyr::select(`NHB % known Cases`,`NHB % known Death`,Poverty,Obesity)
AM_Cor<-AM_Cor%>%drop_na()
x_pts<-1:nrow(AM_Cor);
model <- lm(`NHB % known Cases`~Poverty+Obesity, data=NHB_Final)
model <- lm(`NHB % known Death`~Poverty+Obesity, data=NHB_Final)



res<-residuals(model) # residuals
standard_res <- stdres(model) # standardized residuals
studentized_res <- studres(model) ; # studentized residuals
df_res<-data.frame(x_pts, res, standard_res, studentized_res); # dataset of residuals
df_res$res<-round(df_res$res,digits = 2)
# *** Plotting of Residuals with normal curve
lim<-1.2*ceiling(max(abs(min(df_res$res)), abs(max(df_res$res))));
P<-ggplot(df_res, aes(x=res)) + 
  my_theme+
  labs(x="Residuals", y="Density")+
  geom_histogram(aes(y =..density..), 
                 #breaks = seq(-3, 3, by = 0.5),
                 color="black", 
                 fill="lightblue", 
                 binwidth=1, 
                 size=1)+
  stat_function(fun = dnorm, args = list(mean = mean(df_res$res), sd = sd(df_res$res)), size=1)+
  xlim(-20,20)+ylim(0,0.2);

# QQ plots 
ggplot(data = df_res, mapping = aes(sample = res))+
  stat_qq_band(conf = 0.95, size=0.5, fill="lightblue")  +
  stat_qq_line() +
  stat_qq_point(shape=21, size=2, stroke=1, fill="blue") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  my_theme+
  ylim(-20,20)+
  xlim(-10,10)



Hispanic_Cor<-Hispanic_Final%>%dplyr::select(`Hispanic % known Cases`,`Hispanic % known Death`,Diabetes,Poverty)
Hispanic_Cor<-Hispanic_Cor%>%drop_na()
x_pts<-1:nrow(Hispanic_Cor);
model <- lm(`Hispanic % known Cases`~Poverty+Diabetes, data=Hispanic_Final)
Hispanic_Cor<-Hispanic_Final%>%dplyr::select(`Hispanic % known Death`,Poverty,Over60)
Hispanic_Cor<-Hispanic_Cor%>%drop_na()
x_pts<-1:nrow(Hispanic_Cor);
model <- lm(`Hispanic % known Death`~Over60+Poverty, data=Hispanic_Final)
res<-residuals(model) # residuals
standard_res <- stdres(model) # standardized residuals
studentized_res <- studres(model) ; # studentized residuals
df_res<-data.frame(x_pts, res, standard_res, studentized_res); # dataset of residuals
df_res$res<-round(df_res$res,digits = 2)
# *** Plotting of Residuals with normal curve
lim<-1.2*ceiling(max(abs(min(df_res$res)), abs(max(df_res$res))));
P<-ggplot(df_res, aes(x=res)) + 
  my_theme+
  labs(x="Residuals", y="Density")+
  geom_histogram(aes(y =..density..), 
                 #breaks = seq(-3, 3, by = 0.5),
                 color="black", 
                 fill="lightblue", 
                 binwidth=1, 
                 size=1)+
  stat_function(fun = dnorm, args = list(mean = mean(df_res$res), sd = sd(df_res$res)), size=1)+
  xlim(-30,20)+ylim(0,0.2);

# QQ plots 
ggplot(data = df_res, mapping = aes(sample = res))+
  stat_qq_band(conf = 0.95, size=0.5, fill="lightblue")  +
  stat_qq_line() +
  stat_qq_point(shape=21, size=2, stroke=1, fill="blue") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  my_theme+
  ylim(-20,20)+
  xlim(-20,20)



#For model normality test

res<-residuals(model) # residuals

shapiro_res<-shapiro.test(res);               shapiro_res$p.value


anderson_res<-ad.test(res);                   anderson_res$p.value


lillie_res<-lillie.test(res);                 lillie_res$p.value




#
