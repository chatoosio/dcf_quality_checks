# Script to explore and plot error patterns in MEDITS from the output summary tables from Postgres quality checks
require(RODBC) # odbcConnectAccess
require(doBy)  # summaryBy
library(ggplot2); library(reshape2); library(stringr)
library(reshape)


#setwd("C:/qulitychecks_JRC")
setwd("S:/MEDITS_scripts2/qulitychecks_JRC")
# Import da
# connection and data upload
# queries
#qry1 <- paste("SELECT * FROM M10_MED_TA")

#UPDATED MEDITS DB
#ch <- odbcConnectAccess("C:/Stecf Ancona 2012/Survey_DB/G-L MEDITS survey data 20121207.mdb")# not working
#ch <- odbcConnectAccess("C:/EWG 12-10/EWG 12-10/G-O survey/G-L MEDITS survey data 20120717_areas_15_16.mdb")


#TA <- sqlQuery(ch, qry1)
#odbcClose(ch)

# to access the lates 2012 data 
#TA <- read.csv("C:/qulitychecks_JRC/data/M10_MED_TA.csv", quote="")
TA <- read.csv("S:/STECF/EWG_mediterranean/2014/ewg 14_19/data/medits_taFINAL.csv")
names(TA) <- toupper(names(TA))
# extract the number of hauls by gsa and year
ta<-summaryBy(HAUL_NUMBER~YEAR+AREA+COUNTRY, data=TA, FUN=length)
names(ta)<-c("year", "area", "country", "n_hauls")
ta$code <- paste(ta$area, ta$country, sep="_")

a <- ggplot(ta, aes(year, n_hauls, color=code))+geom_line()+geom_point()+facet_grid(country~.)
ggsave(a, file=paste("medits_hauls",Sys.Date(),".png"), width=12, height=8, dpi=300)



medits2012 <- read.csv("S:/MEDITS_scripts2/qulitychecks_JRC/data/all_medits_checks-4.csv", header=T)
medits7_2013 <- read.csv("S:/MEDITS_scripts2/qulitychecks_JRC/data/all_medits_checks20130717.csv", header=F)
#meditsN <- read.csv("S:/MEDITS_scripts2/qulitychecks_JRC/data/all_medits_checks24012014.csv", header=T)
medits2014 <- read.csv("S:/MEDITS_scripts2/qulitychecks_JRC/data/all_medits_checks_3_2014.txt", header=T)
medits2015 <- read.csv("S:/MEDITS_scripts2/qulitychecks_JRC/data/all_medits_checks_29_01_2015.csv")
medits2015_2 <- read.csv("S:/MEDITS_scripts2/qulitychecks_JRC/data/all_medits_checks_26_08_2015.csv")

medadj<- function(x) {  
if (dim(x)[2]<24) {
names(x)<-c("check_name","comment","country", "area","X1994", "X1995", "X1996","X1997","X1998","X1999","X2000","X2001","X2002","X2003","X2004","X2005", "X2006","X2007","X2008","X2009", "X2010","X2011","X2012" )
}  else  {
names(x)<-c("check_name","comment","country", "area","X1994", "X1995", "X1996","X1997","X1998","X1999","X2000","X2001","X2002","X2003","X2004","X2005", "X2006","X2007","X2008","X2009", "X2010","X2011","X2012", "X2013", "X2014"  )
} 

if (dim(x)[2]<24) { 
xx<-melt.data.frame(x, id.vars=c("check_name","comment","country", "area"), measure.vars=c("X1994", "X1995", "X1996","X1997","X1998","X1999","X2000","X2001","X2002","X2003","X2004","X2005", "X2006","X2007","X2008","X2009", "X2010","X2011","X2012" )) 
}  else  {
xx<-melt.data.frame(x, id.vars=c("check_name","comment","country", "area"), measure.vars=c("X1994", "X1995", "X1996","X1997","X1998","X1999","X2000","X2001","X2002","X2003","X2004","X2005", "X2006","X2007","X2008","X2009", "X2010","X2011","X2012","X2013", "X2014" )) 
}
    xx$year<-as.numeric(str_sub(xx$variable, 2, -1))
    xx<-subset(xx, select=-(variable))
    xx$area<-as.factor(xx$area)
return(xx)
}

medits2012<-medadj(medits2012)
medits7_2013<-medadj(medits7_2013)
#meditsN<-medadj(meditsN)
medits2014<-medadj(medits2014)
medits2015<-medadj(medits2015)
medits2015_2<-medadj(medits2015_2)

#combine old medits with new ones
  #create id variable
medits2012$DC<-rep("12_2012", length(medits2012$year))
medits7_2013$DC<-rep("7_2013", length(medits7_2013$year))
#meditsN$DC<-rep("12_2013", length(meditsN$year))
medits2014$DC<-rep("1_2014", length(medits2014$year))
medits2015$DC<-rep("1_2015", length(medits2015$year))
medits2015_2$DC<-rep("8_2015", length(medits2015_2$year))

#temp <- medits2015_2
temp<- rbind(medits2012,medits7_2013, medits2014, medits2015, medits2015_2) # meditsN,
  
#plotting all error function in a loop without % of errors
checks<-c(unique(temp$check_name))
for(i in temp$check_name){
	
	
a<-	ggplot(temp[temp$check_name==i,], aes(year, value,  color=DC, linetype=DC , linewidth=DC))+geom_line(position=position_jitter(w=0.03, h=0))+facet_wrap(country~area)+
		ggtitle(i)+xlab("Year")

ggsave(assign(paste("check",i,sep=""), a), file=paste("plots/",i,Sys.Date(),".png", sep=""), width=12, height=8)

}

# 
# plot number of MEDITS hauls by area
summaryBy(NOMINAL_EFFORT~COUNTRY+AREA+time,id~time2, FUN=c(sum,mean), data=subset(effort,  GEAR=="OTB" ))

# output sum of errors by country
write.csv(summaryBy(value ~ check_name+country+comment, FUN=sum, data= medits2015 ), file= "MEDITSquality_checks_2014.csv")

# output sum of errors by area
write.csv(summaryBy(value ~ check_name+area+country+comment, FUN=sum, data= medits2015 ), file= "MEDITSquality_checks_2014.csv")


# merge tables and calculate proprotion of erroneous records

#temp<-merge(ta, meditst, by.x=c("area","country", "year"), by.y=c("area","country","year"), all=TRUE)

#temp[is.na(temp$value)==TRUE,]<-0

#temp$properror<-(1-(temp$HAUL_NUMBER.length-temp$value)/temp$HAUL_NUMBER.length)*100
#temp$properror<-(temp$value/temp$n_hauls)*100
#temp<-na.omit(temp)

#ggplot(temp[temp$check_name=="check_zero_warp_diam_valid_ta_o",], aes(year, properror, color=area))+geom_line()+facet_grid(country~area)+opts( axis.text.x  = theme_text(angle=90, size=8))

##############################################################################
#Start depth and end depth of each haul should be in the same stratum
temp<-meditst

a<-ggplot(temp[temp$check_name=="rome_shooting_depth_hauling_depth_same_stratum_ta_o",], aes(year, value,  color=DC, linetype=DC , linewidth=DC))+geom_line(position=position_jitter(w=0.03, h=0))+facet_wrap(country~area)+
#theme(axis.text.x = element_text(size = 8, angle = 75))+
#+ 	opts( axis.text.x  = theme_text(angle=75, size=8))+
  ggtitle("Start depth and end depth of each haul should be in the same stratum")+xlab("Year")

ggsave(a, file="plots/rome_shooting_depth_hauling_depth_same_stratum_ta_o.png", width=12, height=8)



#Check between duration of the haul and distance (tolerance of 15%)
b<-ggplot(temp[temp$check_name=="rome_duration_distance_consistency_ta_o",], aes(year, value,  color=DC, linetype=DC , linewidth=DC))+geom_line(position=position_jitter(w=0.03, h=0))+facet_wrap(country~area)+
    #opts( axis.text.x  = theme_text(angle=75, size=8))+
  ggtitle("Check between duration of the haul and distance (tolerance of 15%)")+xlab("Year")
ggsave(b, file="plots/rome_shooting_depth_hauling_depth_same_stratum_ta_o.png", width=12, height=8)


#Check consistency among duration start time and end time of the haul in TA
b1<-ggplot(temp[temp$check_name=="rome_duration_consistency_ta_o",], aes(year, value,  color=DC, linetype=DC , linewidth=DC))+geom_line(position=position_jitter(w=0.03, h=0))+facet_wrap(country~area)+ggtitle("Check consistency among duration start time and end time of the haul in TA")+xlab("Year")

ggsave(b1, file="plots/rome_shooting_depth_hauling_depth_same_stratum_ta_o.png", width=12, height=8)



#Check if the value of bridles length is consistent according to the mean depth
b2<-ggplot(temp[temp$check_name=="rome_bridles_len_mean_depth_ta_o",], aes(year, value,  color=DC, linetype=DC , linewidth=DC))+geom_line(position=position_jitter(w=0.03, h=0))+facet_grid(country~area)+
  #opts( axis.text.x  = theme_text(angle=75, size=8))+
  ggtitle("Value of bridles length is consistent according to the mean depth")+xlab("Year")

ggsave(b2, file="plots/Value of bridles length is consistent according to the mean depth.png", width=12, height=8)


#Check in case of valid records if wing_opening is zero

c<-ggplot(temp[temp$check_name=="check_zero_wing_ope_valid_ta_o",], aes(year, value,  color=DC, linetype=DC , linewidth=DC))+geom_line(position=position_jitter(w=0.03, h=0))+facet_grid(country~area)+
#+opts( axis.text.x  = theme_text(angle=75, size=8))+
  ggtitle("Check in case of valid records if wing_opening is zero")+xlab("Year")

ggsave(c, file="plots/Check in case of valid records if wing_opening is zero.png", width=12, height=8)

#check_zero_vert_open_valid_ta_o
d<-ggplot(temp[temp$check_name=="check_zero_vert_open_valid_ta_o",], aes(year, value,  color=DC, linetype=DC , linewidth=DC))+geom_line(position=position_jitter(w=0.03, h=0))+facet_grid(country~area)+
  #opts( axis.text.x  = theme_text(angle=75, size=8))+
  ggtitle("Check in case of valid records if vertical_opening is zero")+xlab("Year")

ggsave(d, file="plots/Check in case of valid records if vertical_opening is zero.png", width=12, height=8)

#check_zero_vert_open_valid_ta_o
e<-ggplot(temp[temp$check_name=="rome_check_haul_coord_distance_ta_o",], aes(year, value,   color=DC, linetype=DC , linewidth=DC))+geom_line(position=position_jitter(w=0.03, h=0))+facet_grid(country~area)+
  #opts( axis.text.x  = theme_text(angle=75, size=8))+
  ggtitle("Consistency of the hauls coordinates with the distance (adjusted to 100% difference)")+xlab("Year")
ggsave(e, file="plots/Consistency of the hauls coordinates with the distance (adjusted to 100% difference).png", width=12, height=8)


############################################################################
#Check consistency between not null weight and not null total number
f<-ggplot(temp[temp$check_name=="rome_consistency_zero_weight_tot_number_tb_o",], aes(year, value,   color=DC, linetype=DC , linewidth=DC))+geom_line(position=position_jitter(w=0.03, h=0))+
  facet_grid(country~area)+
  ggtitle("consistency between not null weight and not null total number")+xlab("Year")
ggsave(f, file="plots/consistency between not null weight and not null total number.png", width=12, height=8)


# Check if in case of sub-sampling in TC the number per sex in TB is raised correctly
g<-ggplot(temp[temp$check_name=="rome_num_sex_raising_tb_o",], aes(year, value,  color=DC, linetype=DC , linewidth=DC))+geom_line(position=position_jitter(w=0.03, h=0))+facet_grid(country~area)+
  opts( axis.text.x  = theme_text(angle=75, size=8))+
  ggtitle("rome_num_sex_raising_tb_o")+xlab("Year")
ggsave(g, file="plots/rome_num_sex_raising_tb_o.png", width=12, height=8)

#Small raising percentages (>10% threshold)
h<-ggplot(temp[temp$check_name=="small_raising_perc_o",], aes(year, value,   color=DC, linetype=DC , linewidth=DC))+geom_line(position=position_jitter(w=0.03, h=0))+facet_grid(country~area)+
  #opts( axis.text.x  = theme_text(angle=75, size=8))+
  ggtitle("Subsampling below 10%")+xlab("Year")
ggsave(h, file="plots/Subsampling below 10.png", width=12, height=8)

#Check if in TB  NB_TOTAL equals NB_F+NB_M+NB_I
i<-ggplot(temp[temp$check_name=="rome_tot_num_num_per_sex_tb_o",], aes(year, value,  color=DC, linetype=DC , linewidth=DC))+geom_line(position=position_jitter(w=0.03, h=0))+facet_grid(country~area)+opts( axis.text.x  = theme_text(angle=75, size=8))+ggtitle("Check if in TB  NB_TOTAL equals NB_F+NB_M+NB_I")+xlab("Year")

ggsave(i, file="plots/Check if in TB  NB_TOTAL equals NB_F+NB_M+NB_I.png", width=12, height=8)


#Internal check  in TC (the number per sex must be equal to the sum of nb per length per sex) 
l<-ggplot(temp[temp$check_name=="rome_num_sex_eq_sum_nb_length_sex_tc_o",], aes(year, value,  color=DC, linetype=DC , linewidth=DC))+geom_line(position=position_jitter(w=0.03, h=0))+
  facet_grid(country~area)+
  opts( axis.text.x  = theme_text(angle=75, size=8))+
  ggtitle("TC, number per sex must be equal to the sum of nb per length per sex")+xlab("Year")

ggsave(l, file="plots/TC, number per sex must be equal to the sum of nb per length per sex.png", width=12, height=8)

#Check consistency between WEIGHT_OF_THE_FRACTION in TC and TOTAL_WEIGHT_IN_HAUL in TB
m<-ggplot(temp[temp$check_name=="rome_consistency_wght_frac_tc_tot_wght_tb_o",], aes(year, value,  color=DC, linetype=DC , linewidth=DC))+geom_line(position=position_jitter(w=0.03, h=0))+facet_grid(country~area)+
  opts( axis.text.x  = theme_text(angle=75, size=8))+
  ggtitle("Consistency TC WEIGHT_OF_THE_FRACTION and TB TOTAL_WEIGHT_IN_HAUL")+xlab("Year")

ggsave(m, file="plots/Consistency TC WEIGHT_OF_THE_FRACTION and TB TOTAL_WEIGHT_IN_HAUL.png", width=12, height=8)

#Check if in case of sub-sampling in TC the number per sex in TB is raised correctly
n<-ggplot(temp[temp$check_name=="rome_num_sex_raising_tb_o",], aes(year, value,   color=DC, linetype=DC , linewidth=DC))+
  geom_line(position=position_jitter(w=0.03, h=0))+
  facet_grid(country~area)+opts( axis.text.x  = theme_text(angle=75, size=8))+ggtitle("Check if in case of sub-sampling in TC the number per sex in TB is raised correctly")+xlab("Year")

ggsave(n, file="plots/Check if in case of sub-sampling in TC the number per sex in TB is raised correctly.png", width=12, height=8)

###############################################################################
