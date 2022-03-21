#Final data analysis
Sys.setlocale("LC_ALL","English")
setwd("C:/Users/beuk_/OneDrive/IJsland/Data/Cod/Ethovision")
options(ggrepel.max.overlaps = Inf)
library(readxl)
library(rptR)
library(dplyr)
library(ggplot2)
library(lme4)
library(ggpubr)
library(data.table)
library(car)
library(performance)#check model
library(corrplot)
library(rstatix)
library(Hmisc)
library(lmerTest) #add pvalue to lmer summary
library(stringr)

#change


# Load OFT data -----------------------------------------------------------
data2=as.data.frame(read_excel("Results_moving_010_150_125.xlsx"))
data3=as.data.frame(read_excel("Results_moving_novlastones_010_150_125.xlsx"))
data1=rbind(data2, data3)
names(data1)[1:6]<- c("A", "B", "C", "Treatment", "Vid", "Trial" )

#Split the vid_String in usuable components
data1$Vid<- str_replace(data1$Vid, "-converted.mp4" , "")
data1$Vid<- str_replace(data1$Vid, ".avi" , "")
data1$Vid<- str_replace(data1$Vid, "_plastic info in video wrong" , "")

data1$Vid[nchar(data1$Vid) >90]<- str_sub(data1$Vid[nchar(data1$Vid) >90], start= 103) # #November strings
#Other strings: in this order!
data1$Vid[nchar(data1$Vid) %in% c(30:35)]<- str_sub(data1$Vid[nchar(data1$Vid) %in% c(30:35)],  start= 16)
data1$Vid[nchar(data1$Vid) >25]<- str_sub(data1$Vid[nchar(data1$Vid) >25],  start= 40)

split=str_split(data1$Vid, "_")
data1$Cod= str_sub(lapply(split,'[[',3))
data1$date= as.Date(str_sub(lapply(split,'[[',1), start=-8), format= "%Y%m%d")
data1$Month= months(data1$date)
#data1$Time= str_sub(lapply(split,'[[',2))

#Only OFT and November
df=data1[which(data1$Treatment == "Activity" & data1$Month == "November"),]
df$Trial[df$date < as.Date("2019-11-10")] <- 1
df$Trial[df$date > as.Date("2019-11-20")] <- 3
df$Trial[df$Trial =="1B"]<-2

df=df %>% 
  select("Cod","Trial","Movement Moving / Center-point Cumulative Duration s",
         "Distance moved Center-point Total cm" ) 
names(df)= c("Cod", "Trial", "OFT_mov", "OFT_dist")

df$OFT_mov= df$OFT_mov/300 #Make it a proportion (5 minutes original)


# Load home tank data ----------------------------------------------------

ant1= read.csv("C:/Users/beuk_/OneDrive/IJsland/Data/Cod/Hometank_data/Hometank_november.csv")
#Add the trial (add automatically the number of first, second and third occurence)
ant1 <- ant1 %>% 
  group_by(Cod) %>% 
  mutate(Trial=1:n())

ant1$HT= ant1$actscore/100
ant1= ant1 %>% 
  arrange(Cod, Date)



#merge ant1 (home tank) with df (OFT) 
all= merge(df, ant1[,c("Cod", "HT", "Trial")], by= c("Cod", "Trial"))
rm(ant1, df, data1, data2, data3, split)

summary(all)


# Read in yearly data -----------------------------------------------------
year= read_excel("C:/Users/beuk_/OneDrive/IJsland/Data/Cod/Data_yearly.xlsx", na=c("DEAD", "NA"), .name_repair = "minimal")
#Add Allele to all data
all=merge(all, year[,c("Cod", "Allele")], by="Cod")


# Add a column wheter they left the shelter voluntarily ( <300) o --------

shelt= read_excel("C:/Users/beuk_/OneDrive/IJsland/Data/Cod/Data_shelter.xlsx", na=c("DEAD", "NA"), .name_repair = "minimal")
shelt= shelt %>% 
  filter(Exp %in% c("1A", "1B", "1C")) %>% 
  select(Cod, Exp, Latency)
shelt$Exp[shelt$Exp== "1A"]<-1
shelt$Exp[shelt$Exp== "1B"]<-2
shelt$Exp[shelt$Exp== "1C"]<-3
shelt$Latency[shelt$Latency < 300] <- 1
shelt$Latency[shelt$Latency == 300] <- 0

names(shelt)<- c("Cod", "Trial", "Shelt_lat_yn")
all=merge(all, shelt, by=c("Cod", "Trial"))
# Add weight length data --------------------------------------------------

we1= read_excel("C:/Users/beuk_/OneDrive/IJsland/Data/Cod/Data_wl.xlsx", na=c("DEAD", "NA"), .name_repair = "minimal")
we1$Date= as.Date(we1$Date)
we1$Month= months(we1$Date)
we= we1[we1$Month %in% c("October", "November"),]
rm(we1)

###Calculate SGR oct-nov

#Transform to long
weo= we[we$Month == "October",]
wen= we[we$Month == "November",]
wef= merge(weo, wen, by= "Cod")
rm(weo)
rm(wen)
wef$SGR= (log(wef$Weight.y)-log(wef$Weight.x))*100/ as.numeric((wef$Date.y-wef$Date.x))
hist(wef$SGR)
all= merge(all, wef[c("Cod","SGR")], by="Cod")

#Calculate condition factor
we$cond= we$Weight/(we$STL^3)*100
all=merge(all, we[we$Month %in% c("November"),c("Cod", "STL", "Weight", "cond")], by= c("Cod"))


# Throw out data described the selecting procedures -----------------------

#Only keep the fish that have all trials
comp=as.data.frame(table(all$Cod))
comp2=comp$Var1[comp$Freq == 3]
all=all[all$Cod %in% comp2,] ## 76 cod left
rm(comp)

#Fish that died in the two weeks after, but not by human error
seqd=seq(as.Date("2019-10-28"), as.Date("2019-12-06"), by = "days") 
#67 died on December 7. When was he test last?: nov 26. So 11 days after exp
died=year$Cod[as.Date(year$Dead) %in% seqd & year$Reason_Death != "human error"]
all=all[!(all$Cod %in% died),] ## 59 left l

###########Fish that lost weight (SGR oct-nov)
all=all[all$SGR > 0,] ##55 left
hist(all$SGR)

###Remove too low condition factor
all=all[all$cond>0.8,]

#Remove fish with missing length and missing Allele
#Describing experimental fish
all= all[!is.na(all$STL) & !is.na(all$Allele),] #53 cod left

##I know that 21, 1B has a too low frame rate, so remove 21 here
all=all[!(all$Cod == "21"),]

IDall= unique(all$Cod)


# Area covered ------------------------------------------------------------
#Merge data frame with area covered
#0 digits is the roughest and there is still a very good correlation
ar=read.csv("Un_xy_0digits.csv")
##Area=80 cm, so in total80X40=3200 unique combinations... (with 0 digits) So it is correct
ar= ar[ar$Trial %in% c("1A", "1B", "1C"),]
ar$Trial[ar$Trial== "1A"]<-1
ar$Trial[ar$Trial== "1B"]<-2
ar$Trial[ar$Trial== "1C"]<-3
names(ar)<- c("ID", "Trial", "Un_xy")
hist(log(ar$Un_xy))
arall= merge(all, ar, by.x= c("Cod", "Trial"), by.y= c("ID","Trial"))

#For all three trials a very high correlation between % movement and total area covered
ggplot(arall, aes(OFT_mov,Un_xy))+
  geom_point()+
  ylab("Total area covered [unique x/y coordinates]")+
  xlab("Movement in open field test [%]")+
  facet_grid(cols= vars(Trial))

#Distance traveled?
ggplot(arall, aes(OFT_dist/100,Un_xy))+
  geom_point()+
  ylab("Total area covered [unique x/y coordinates]")+
  xlab("Distance traveled in OFT [m]")+
  facet_grid(cols= vars(Trial))

#hist(log(arall$OFT_dist))
#hist(arall$OFT_mov)
#hist(asin(sqrt(arall$OFT_mov/100)))
#hist(asin(sqrt(arall$Un_xy)))
#cor.test(log(arall$Un_xy),asin(sqrt(arall$OFT_mov/100)))
hist(log(arall$Un_xy))
hist(log(arall$OFT_mov))
cor.test(log(arall$Un_xy),log(arall$OFT_mov))
hist(log(arall$OFT_dist))
cor.test(log(arall$OFT_dist),log(arall$OFT_mov))

mod11a=lm(log(Un_xy) ~ log(OFT_mov), data=arall[arall$Trial == 1,])
summary(mod11a)

mod22a=lm(log(Un_xy) ~ log(OFT_dist), data=arall[arall$Trial == 1,])
summary(mod22a)

mod11b=lm(log(Un_xy) ~ log(OFT_mov), data=arall[arall$Trial == 2,])
summary(mod11b)
mod22b=lm(log(Un_xy) ~ log(OFT_dist), data=arall[arall$Trial == 2,])
summary(mod22b)

mod11c=lm(log(Un_xy) ~ log(OFT_mov), data=arall[arall$Trial == 3,])
summary(mod11c)
mod22c=lm(log(Un_xy) ~ log(OFT_dist), data=arall[arall$Trial == 3,])
summary(mod22c)

#Distance traveled and area covered (unique x/y coordinates), correlated highly (R2 > 0.94)
#With % moved, so this was taken as the best measure to compare with the home tank data
#+ real exploration measurement 0 digits= cm grids
check_model(mod11a)
check_model(mod11b)
check_model(mod11c)
all$OFT_dist = NULL #Remove OFT_Dist
rm(ar, arall, mod11a, mod11b, mod11c, mod22a, mod22b, mod22c, wef)


# Collect the weight and condition data -----------------------------------


#### 1 column for each (nov and oct)
#Add the condition factor to the long data frame
wea= we[we$Cod %in% IDall,]
wea<-  wea %>%
  group_by(Cod, Month) %>%
  mutate(cond = Weight/STL^3 *100)

ggplot(wea, aes(Month, cond, group=Cod, col=Cod))+
  geom_point()+
  geom_path()+
  scale_x_discrete(limits = c("October", "November"))
#Decreasing condition, but not decreasing weight

ggplot(wea, aes(Month, Weight, group=Cod, col=Cod))+
  geom_point()+
  geom_path()+
  scale_x_discrete(limits = c("October", "November"))


names(all)[names(all) == 'cond'] <- 'con_nov'

alluni= unique(all[,c("SGR", "Weight", "con_nov")])

###Collect the data of the weight and condition increase: in article
mean(wea$Weight[wea$Month == "October"],)
sd(wea$Weight[wea$Month == "October"],)
mean(wea$Weight[wea$Month == "November"],)
sd(wea$Weight[wea$Month == "November"],)

mean(wea$cond[wea$Month == "October"],)
sd(wea$cond[wea$Month == "October"],)
mean(wea$cond[wea$Month == "November"],)
sd(wea$cond[wea$Month == "November"],)

mean(all$SGR)
sd(all$SGR)
table(all$Allele)

#See if the condition is the same between october and november
#rtest=rpt(cond ~ + (1|Cod), grname = "Cod", data = boxy, datatype = "Gaussian", nboot = 1000, npermut = 1000)


###Repeatabilities
#r1=rpt(asin(sqrt(OFT_mov)) ~ + (1|Cod), grname = "Cod", data = all, datatype = "Gaussian", nboot = 1000, npermut = 1000)

#r1a=rpt(asin(sqrt(OFT_mov)) ~ + (1|Cod), grname = "Cod", data = all[all$Trial %in% c("1","2"),], datatype = "Gaussian", nboot = 1000, npermut = 1000)
#r1b=rpt(asin(sqrt(OFT_mov)) ~ + (1|Cod), grname = "Cod", data = all[all$Trial %in% c("1","3"),], datatype = "Gaussian", nboot = 1000, npermut = 1000)
#r1c=rpt(asin(sqrt(OFT_mov)) ~ + (1|Cod), grname = "Cod", data = all[all$Trial %in% c("2","3"),], datatype = "Gaussian", nboot = 1000, npermut = 1000)


# all$OFT_mov_c= asin(sqrt(all$OFT_mov/100))
# all$HT_c= asin(sqrt(all$HT/100))
# summary(all)

# p1= ggplot(all, aes(Trial, asin(sqrt(OFT_mov/100)), group= Cod, col=Cod))+
#   geom_path(size=1.5)+
#   theme(legend.position = "none")+
#   xlab("Trial")+
#   ylab("Transformed locomotion in unknown environment")+
#   ylim(c(0,1.5)) #+
#   #theme(legend.position = "none",
#         # axis.text=element_markdown(face="bold", size= 17),
#         # axis.title=element_markdown(face="bold", size= 17),
#         # panel.background = element_blank(),
#         # axis.line = element_line(colour = "black"))

#r2=rpt(asin(sqrt(HT)) ~ (1 | Cod), grname = "Cod", data = all, datatype = "Gaussian", nboot = 1000, npermut = 1000)
#r2a=rpt(asin(sqrt(HT)) ~ (1 | Cod), grname = "Cod", data = all[all$Trial %in% c("1","2"),], datatype = "Gaussian", nboot = 1000, npermut = 1000)
#r2b=rpt(asin(sqrt(HT)) ~ (1 | Cod), grname = "Cod", data = all[all$Trial %in% c("1","3"),], datatype = "Gaussian", nboot = 1000, npermut = 1000)
#r2c=rpt(asin(sqrt(HT)) ~ (1 | Cod), grname = "Cod", data = all[all$Trial %in% c("2","3"),], datatype = "Gaussian", nboot = 1000, npermut = 1000)


# p2=ggplot(all, aes(as.character(Trial), asin(sqrt(HT/100)), group= Cod, col=Cod))+
#   geom_path(size=1.5)+
#   theme(legend.position = "none")+
#   xlab("Trial")+
#   ylab("Tranformed locomotion in unknown environment") #+
#   # theme(legend.position = "none",
#   #       axis.text=element_markdown(face="bold", size= 17),
#   #       axis.title=element_markdown(face="bold", size= 17),
#   #       panel.background = element_blank(),
#   #       axis.line = element_line(colour = "black"))
#
# ggarrange(p2, p1,
#           labels = c("A", "B"),
#           ncol = 2,
#           #font.label=list(size = 30, color = "black"),
#           hjust = 0.01)



###Correlation

#Use weight, cond, SGR or STL?
##Take unique fish!!
all2a= unique(all[,c("Cod", "STL", "Weight", "SGR", "con_nov")])
all2b= all2a[,2:5]
names(all2b)<- c("STL", "W", "SGR", "K")

cor_5 <- rcorr(as.matrix(all2b))

#Figure S1:#Cross means not signifcant
corrplot(cor_5$r, type = "upper", order = "hclust", 
         p.mat = cor_5$P, sig.level = 0.01, addCoef.col = "red", pch.col = "red",
         pch.cex = 6, tl.col="black")

par(mfcol = c(1, 1))
hist(all$STL)
hist(all$Weight)
hist(all$SGR)
#hist(all$con_oct)
hist(all$con_nov)



##Check if fish that were pushed out the shelter moved less during the OFT
table(all$Shelt_lat_yn, all$Trial)
modtry=lmer(asin(sqrt(OFT_mov)) ~ as.character(Shelt_lat_yn) + (1|Cod) + (1|Trial), data=all)
summary(modtry)
Anova(modtry, type=2, test.statistic = "F")

ggplot(all, aes(Shelt_lat_yn, asin(sqrt(OFT_mov))))+
  geom_point()+
  geom_smooth(method="lm")
#No, there is no difference. So, remove Shelt_lat_yn
all$Shelt_lat_yn= NULL


# Final analysis ----------------------------------------------------------
setwd("C:/Users/beuk_/OneDrive/IJsland/Writing/Articles/Article II")
#write.csv(all, "Final_datatable.csv", row.names=F)
############Differences between the two tests

library(data.table)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggplot2)
library(car)
library(lmerTest) #add pvalue to lmer summary


setwd("C:/Users/beuk_/OneDrive/IJsland/Writing/Articles/Article II")

ku1= read.csv("Final_datatable.csv")
ku1$Cod= as.character(ku1$Cod)
ku1$Trial= as.character(ku1$Trial)

#ku1=all

summary(ku1)
#From wide to long
kl= melt(setDT(ku1), id.vars = c("Cod","Trial","Allele", "Weight", "con_nov", "SGR", "STL"), variable.name = "Environment")
ku= na.omit(kl[, c("Cod","Trial","Allele", "SGR", "con_nov", "value", "Environment")])
ku$Labels= as.character(ku$Environment)
ku$Labels[ku$Labels == "HT"]<- "A"
ku$Labels[ku$Labels == "OFT_mov"]<- "B"

ggsave("C:/Users/beuk_/OneDrive/IJsland/Writing/Articles/Article II/Fig2.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')
ggplot(ku, aes(as.character(Trial), asin(sqrt(value)), group= Cod, col=Cod))+
  geom_path()+
  xlab("Trial")+
  ylab("Transformed locomotion")+
  facet_grid(cols= vars(Labels))+
  theme(legend.position = "none", 
        strip.text.x = element_text(size = 10, colour = "black", angle = 0, face="bold"),
        #panel.grid.major.x = element_blank(),
        panel.grid.major = element_line( size=.1, color="grey80"),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line( size=.1, color="black"),
        panel.border = element_rect(colour = "black", fill=NA)
  )


dev.off()

##Mean and SD from the data (repeated measurements doesn't matter)
range(ku$value[ku$Environment == "OFT_mov"])
median(ku$value[ku$Environment == "OFT_mov"])
range(ku$value[ku$Environment == "HT"])
median(ku$value[ku$Environment == "HT"])

#Full model
modfull=lmer(asin(sqrt(value)) ~ Environment * SGR + Environment * con_nov +
               Environment * Allele +  (1|Environment:Cod)+ (1|Environment:Trial), data=ku)
summary(modfull)
Anova(modfull, type=3, test.statistic = "F")


#Dropping interactions
moddrop=lmer(asin(sqrt(value)) ~  Environment + SGR + con_nov + Allele +
               (1|Environment:Cod)+ (1|Environment:Trial), data=ku)
summary(moddrop)
Anova(moddrop, type=2, test.statistic = "F")



###Make beautiful table
#https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html?fbclid=IwAR1Z_pU6_Ui7P0q2_0-YUw06K27QTAv03W0pEoCx_3xvWrtgdpUacf9JL0Q

#df and mean

Anova(modfull, type=3, test.statistic = "F")
Anova(moddrop, type=2, test.statistic = "F")
#T values are the statistic: how to go to F

tab_model(modfull, moddrop, show.se = TRUE, show.df= TRUE, show.stat= TRUE, show.ci = FALSE,
          dv.labels = c("Full model - Type 3", "Full model - Type 2"), #Add labels
          string.se = "se",
          df.method = "kenward",
          p.val= "kenward",
          #string.stat = "X^2",
          title = "Table S2. Outputs of the full and reduced linear mixed models testing for the locomotion differences in a known and unkown (Intercept) environment and the interactions with specific growth rate (SGR), Fulton's conditions factor (K) and PanI allele."
          #pred.labels = c("(Intercept)", "Env (known)", "SGR", "K", "Allele (Migratory)", 
                         # "Allele (Resident)", "Env (known) * SGR", "Env (known) * K", 
                          #"Env (known) * Allele (Migratory)", "Env (known) * Allele (Resident)")
          #file= "Stat_table.html"
          #col.order = c("est", "se", "stat", "df", "p")
)

##I adjusted this table by hand for the final report, the Esitmates mentioned are  t-values
#F values don't show up (package problem)

ku$Environment = as.character(ku$Environment)
ku$Environment[ku$Environment == "OFT_mov"]<- "Unknown"
ku$Environment[ku$Environment == "HT"]<- "Known"


#bl= cbind(ku, resid(modfull)) #full model for article
bl= cbind(ku, resid(moddrop)) #with dropped interactions for sup
#From long to wide
finr=reshape(bl[, c("Cod", "Trial", "Environment", "V2")], idvar = c("Cod", "Trial"), timevar = "Environment", direction = "wide")
#summary(ku)
#table(ku$Allele)

hist(finr$V2.Unknown)
hist(finr$V2.Known)
cor.test(finr$V2.Known, finr$V2.Unknown)
var.test(finr$V2.Known, finr$V2.Unknown) # Differences in variances


ggsave("C:/Users/beuk_/OneDrive/IJsland/Writing/Articles/Article II/Fig 3 .tiff", units="in", width=4, height=4, dpi=300, compression = 'lzw')
library(ggthemes)
ggplot(finr, aes(finr$V2.Known, finr$V2.Unknown))+
  geom_point(size=1)+
  #geom_smooth(method= 'lm')+
  ylab("Residuals unknown environment")+
  xlab("Residuals known environment")+
  theme_few()+
  xlim(-0.5,0.8)+
  ylim(-0.5,0.8)

dev.off()
plot(finr$V2.Known, finr$V2.Unknown)

# Show that home tank and OFT anylsis overlap -----------------------------

#Select 16 random videos (ID+Trial)
#set.seed=1
#sample(1:159, 16)
#Random sample of 12 videos with unique fish
library(stringr)
all=ku1
dem=c(27,  18,   1,  7,  148, 130,  24,  57, 143,  92,  75,  79, 122, 133, 117,  45)
all$Cod[dem]
#Videos and trials to analyze
ana= all[dem,]
table(ana$Trial)

check=NULL

setwd("C:/Users/beuk_/OneDrive/IJsland/Data/Cod/Analysis_HT_OFT_compare")
files.in= list.files()
library(readr)
sumtab= NULL

for (i in 1:length(files.in)){
  #i=1
  data= read.csv(files.in[i])
  data= as.data.frame(data)
  split=str_split(files.in[i], "_")
  
  ID1<- str_sub(lapply(split,'[[',3))%>% str_extract( ".*[^.csv]")
  ID= parse_number(ID1)
  dt= as.Date(str_sub(lapply(split,'[[',1)), format= "%Y%m%d")

  if(data$Time[1] != "0,00") {break}
  #Select 
  dat2= data[0:1500,]
  n_obs= nrow(dat2[!is.na(dat2$Default),])
  actscore=  nrow(dat2[dat2$Default == "Moving",]) / n_obs
  
  ##Yawn, acceleration, int_start, int_stop 
  sumtab= rbind(sumtab, data.frame("Cod"= ID, 
                                   "date"=dt,
                                   "n_obs"= n_obs, 
                                   "actscore"=actscore)) 
  
}


htoft= merge(sumtab, ana, by="Cod")

ggplot(htoft, aes(OFT_mov, actscore))+
  geom_point()+
  geom_smooth(method='lm')

hist(htoft$OFT_mov)
hist(htoft$actscore)
#summary(lm(actscore ~ OFT_mov, data= htoft)) #This I want signifcant
cor.test(htoft$actscore,htoft$OFT_mov)

t.test(htoft$actscore, htoft$OFT_mov, paired=T) #This I want not
mean(htoft$actscore)
mean(htoft$OFT_mov)

#wilcox.test(htoft$actscore, htoft$OFT_mov, paired = TRUE, alternative = "two.sided")

mean(htoft$actscore)
mean(htoft$OFT_mov)
hist(htoft$actscore)
hist(htoft$OFT_mov)
summary(htoft$actscore)
summary(htoft$OFT_mov)

htoft %>% 
  #select(Cod, OFT_mov) %>% 
  arrange(OFT_mov) %>% 
  pull(Cod)

htoft %>% 
  #select(Cod, actscore) %>% 
  arrange(actscore) %>% 
  pull(Cod)

#Change the settings of Ethovision to match the home tank better?
#New data with 0.1 smoothing and 150 start, 125 stop
setwd("C:/Users/beuk_/OneDrive/IJsland/Data/Cod/Ethovision")

#Those give a much better movement match, so I will change this above
ext1=read_excel("Results_moving_010_150_125.xlsx")
ext2=read_excel("Results_moving_novlastones_010_150_125.xlsx")
#ext1=read_excel("Results_moving_015_150_125.xlsx") 
#ext2=read_excel("Results_moving_novlastones_015_150_125.xlsx")
ext3=rbind(ext1, ext2)
ext=ext3 %>% 
  select(4,5,6,66) 

names(ext)= c("Exp","Cod","Trial", "OFT_mov_010")

ext=ext %>%
  filter(Trial %in% c("1A", "1B", "1C") & Exp == "Activity")
ext$Trial[ext$Trial == "1A"] <- "1"
ext$Trial[ext$Trial == "1B"] <- "2"
ext$Trial[ext$Trial == "1C"] <- "3"
ext$Cod<- str_replace(ext$Cod, "-converted.mp4" , "")
ext$Cod<- str_replace(ext$Cod, ".avi" , "")
ext$Cod<- str_replace(ext$Cod, "_plastic info in video wrong" , "")
ext$Cod= str_sub(ext$Cod,  start= 117)



ext010= merge(sumtab, ext, by= c("Cod", "Trial"))

ggplot(ext010, aes(OFT_mov_010/300, actscore))+
  geom_point()+
  geom_smooth(method=lm)

t.test(ext010$actscore, ext010$OFT_mov_010/300, paired=T) #this is kind of okay
a=ext010$actscore
b= ext010$OFT_mov_010/300
summary(lm(a ~ b))
