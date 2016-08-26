# R code to evaluate data quality and coverage from DCF Med and BS data
# Authors: Giacomo Chato Osio and Alessandro Mannini
# August 2016


####################################################################### 
# Functions to plot the time series of Catch, Landings and Effort 
# Each table is plotted in two ways: 
#                       1. one more disaggregated is by country+ area + gear + VL + species 
#                       2. one aggregated at country+ area + gear + species
# Quarters and Years are plotted with different symbols 
#######################################################################


library(plyr); library(dplyr); library(reshape2)
library(ggplot2); library(zoo);library(gridExtra)

setwd("S:/data call 2016/quality_checks_data_call_2016")




#########################
# Catch @ Age Data      #
#########################

catch <- read.csv("input_data/catches_new.csv")
names(catch) <-toupper(names(catch))


# create a time stamp year-quarter for plotting
# if Quarter = -1 label it in col
catch$col <- NA
catch$col <- ifelse(catch$QUARTER == -1, "Yr", "Qtr")

# if Quarter is = -1, convert to a mid year quarter => 2
catch$QUARTER <- ifelse(catch$QUARTER == -1, 2 , catch$QUARTER)

catch$date <- paste(catch$YEAR, catch$QUARTER, "01", sep ="-" )

# convert date to yearmon class
catch <- transform(catch, date = as.yearqtr(date))


##############
# Plot Data
catch$gear2 <- paste(catch$VESSEL_LENGTH,  catch$GEAR, catch$FISHERY, sep = "-")


p = ggplot(catch[catch$SPECIES=="HKE",], aes(x=date, y = LANDINGS, col = gear2, shape = factor(col))) +
  geom_line()+ geom_point()+
  facet_wrap(AREA ~ COUNTRY+SPECIES , scales = "free_y") +
  scale_x_yearqtr(format = "%YQ%q")+
  theme( axis.text.x = element_text(angle = 60, hjust = 1))+
  xlab("Year + Quarter")+ylab("Landings (t)")+
  guides(col = guide_legend(nrow =  40, bcol = TRUE), guide_legend(title = "Gear Fishery VL"),
  legend.text = element_text(size = 6))
#p 

plots = dlply(catch , "SPECIES", `%+%`, e1 = p)
ml = do.call(marrangeGrob, list(grobs=plots, nrow = 1, ncol = 1))
ggsave("DCFCatch_Area_Country_Species_Gear_VL.pdf", ml , width=18, height=12, dpi=300)


# Explore Trends by YEAR and SPECIES

ct1 <- ddply(catch[catch$LANDINGS>0,], .( YEAR, COUNTRY, AREA, SPECIES, col), summarize,
             Catch = sum(LANDINGS, na.rm = TRUE))

p2 =  ggplot(ct1, aes(x=YEAR, y = Catch, col = AREA, shape = factor(col))) +
      geom_line()+ geom_point()+
      facet_wrap(COUNTRY ~SPECIES, scales = "free_y")+ 
      scale_x_yearqtr(format = "%YQ%q")+
      theme( axis.text.x = element_text(angle = 60, hjust = 1))+
      xlab("Year")+ylab("Landings (t)")+
  scale_fill_brewer(palette="Spectral")

#p2 

plots2 = dlply(ct1 , "SPECIES", `%+%`, e1 = p2)
ml2 = do.call(marrangeGrob, list(grobs = plots2, nrow=1, ncol=1))
ggsave("DCFCatch_Area_Country_Species.pdf", ml2 , width=18, height=12, dpi=300)



#=====================================================================================================


##########################################
# Explore Landings @ Lenght Data        # # 
##########################################

landings <- read.csv("input_data/landings_new.csv")
names(landings) <- toupper(names(landings))

# create a time stamp year-quarter for plotting
# if Quarter = -1 label it in col
landings$col <- NA
landings$col <- ifelse(landings$QUARTER == -1, "Yr", "Qtr")

# if Quarter is = -1, convert to a mid year quarter => 2
landings$QUARTER <- ifelse(landings$QUARTER == -1, 2 , landings$QUARTER)



landings$date <- paste(landings$YEAR, landings$QUARTER, "01", sep ="-" )

# convert date to yearmon class
landings <- transform(landings, date = as.yearqtr(date))


##############
# Plot Data
landings$gear2 <- paste(landings$VESSEL_LENGTH, landings$GEAR, landings$FISHERY, sep = "-")


pl = ggplot(landings[landings$SPECIES=="HKE",], aes(x=date, y = LANDINGS, col = gear2, shape = factor(col))) +
  #geom_line(aes(y=LANDINGS), col = "blue") + 
  geom_line()+ geom_point()+
  facet_wrap(AREA ~ COUNTRY+SPECIES , scales = "free_y") +
  scale_x_yearqtr(format = "%YQ%q")+
  theme( axis.text.x = element_text(angle = 60, hjust = 1))+
  xlab("Year + Quarter")+ylab("Landings (t)")+
guides(col = guide_legend(nrow =  40, bcol = TRUE))
  #guides(col = guide_legend(ncol = 3), guide_legend(title = "Gear Fishery VL"),
  #       legend.text = element_text(size = 6))

#pl 

plotsL = dlply(landings , "SPECIES", `%+%`, e1 = pl)
mlL = do.call(marrangeGrob, list(grobs = plotsL, nrow=1, ncol=1))
ggsave("DCFLandings_Species_Area_Gear_VL_Fishery.pdf", mlL , width=18, height=12, dpi=300)

# Explore Trends in Landings @ Length by YEAR and SPECIES

lnd1 <- ddply(landings[landings$LANDINGS>0,], .( YEAR, COUNTRY, AREA, SPECIES, col), summarize,
              Catch = sum(LANDINGS, na.rm = TRUE))

plnd =  ggplot(lnd1, aes(x = YEAR, y = Catch, col = AREA, shape = factor(col))) +
  geom_line()+ geom_point()+
  facet_wrap(COUNTRY ~SPECIES, scales = "free_y")+ 
  scale_x_yearqtr(format = "%YQ%q")+
  theme( axis.text.x = element_text(angle = 60, hjust = 1))+
  xlab("Year")+ylab("Landings (t)")+
  scale_fill_brewer(palette="Spectral")

#plnd 

plotslnd = dlply(lnd1 , "SPECIES", `%+%`, e1 = plnd)
mlnd = do.call(marrangeGrob, list(grobs = plotslnd, nrow=1, ncol=1))
ggsave("DCFLandings_Area_Country_Species.pdf", mlnd , width=18, height=12, dpi=300)




#=================================================================================
# Explore Fishing Effort Data
effort <- read.csv("input_data/effort.csv")
names(effort) <- toupper(names(effort))

# create a time stamp year-quarter for plotting
# if Quarter = -1 label it in col
effort$col <- NA
effort$col <- ifelse(effort$QUARTER == -1, 1, 0)

# if Quarter is = -1, convert to a mid year quarter => 2
effort$QUARTER <- ifelse(effort$QUARTER == -1, 2 , effort$QUARTER)
#effort$QUARTER <- ifelse(effort$QUARTER <0, effort$QUARTER*-1,effort$QUARTER)




effort$date <- paste(effort$YEAR, effort$QUARTER, "01", sep ="-" )

# convert date to yearmon class
effort <- transform(effort, date = as.yearqtr(date))

##############
# Plot Data
effort$gear2 <- paste(effort$VESSEL_LENGTH, effort$GEAR, effort$FISHERY, sep = "-")


pE = ggplot(effort[effort$GEAR=="OTB",], aes(x=date, y = NOMINAL_EFFORT, col = gear2, shape = factor(col))) +
   geom_line()+ geom_point()+
  facet_wrap(AREA ~ COUNTRY+GEAR , scales = "free_y") +
  scale_x_yearqtr(format = "%YQ%q")+
  theme( axis.text.x = element_text(angle = 60, hjust = 1))+
  xlab("Year + Quarter")+ylab("Effort (Kw/Days)")+
  guides(col = guide_legend(nrow =  40, bcol = TRUE))

#pE 

plotsE = dlply(effort , "GEAR", `%+%`, e1 = pE)
mlE = do.call(marrangeGrob, list(grobs = plotsE, nrow=1, ncol=1))
ggsave("DCFeffort_Area_Country_Gear_VL_splitQUARTER.pdf", mlE , width=18, height=12, dpi=300)


# Aggregated effort
E1 <- ddply(effort[effort$NOMINAL_EFFORT>0,], .( YEAR, COUNTRY, AREA, GEAR, col), summarize,
             sum_effort = sum(NOMINAL_EFFORT, na.rm = TRUE))

E2 =  ggplot(E1, aes(x=YEAR, y = sum_effort, col = AREA, shape = factor(col))) +
  geom_line()+ geom_point()+
  facet_wrap(COUNTRY ~  AREA + GEAR, scales = "free_y") +
#scale_x_yearqtr(format = "%YQ%q")+
  theme( axis.text.x = element_text(angle = 60, hjust = 1))+
  xlab("Year + Quarter")+ylab("Effort (Kw/Days)")

E2 

plots3 = dlply(E1 , "GEAR", `%+%`, e1 = E2)
ml3 = do.call(marrangeGrob, list(grobs = plots3, nrow=1, ncol=1))
ggsave("DCFeffort_Country_Area_Gear_splitQuarter2.pdf", ml3 , width=18, height=12, dpi=300)

E3 =  ggplot(E1, aes(x=YEAR, y = sum_effort, col = AREA, shape = factor(col))) +
  geom_line()+ geom_point()+
  facet_wrap(COUNTRY ~  AREA + GEAR) +
#scale_x_yearqtr(format = "%YQ%q")+
  theme( axis.text.x = element_text(angle = 60, hjust = 1))+
  xlab("Year + Quarter")+ylab("Effort (Kw/Days)")

E3 

plots4 = dlply(E1 , "GEAR", `%+%`, e1 = E3)
ml4 = do.call(marrangeGrob, list(grobs = plots4, nrow=1, ncol=1))
ggsave("DCFeffort_Country_Area_Gear_splitQuarter2_fixscales.pdf", ml4 , width=18, height=12, dpi=300)



# Compare Kw Days and Days at Sea
# Check numbers of Fishing Days 
# Etc
#===========================================================================================




#################################################################################################
# Check Data Coverage 
#################################################################################################



library(plyr); library(dplyr); library(reshape2)
library(ggplot2)

#setwd("S:/data coverage report 2016/dcf/dcf")
#catch <- read.csv("catches_length.csv", sep=";")
#names(catch) <- toupper(names(catch))


# Get number of species reported by, however this might overestimate since a species is there
# even when only landings are reported with no age structure
c1 <- ddply(catch, .( YEAR, GEAR, MESH_SIZE_RANGE, AREA), summarize, N.species = length(unique(SPECIES)))
c2 <- ddply(catch, .( YEAR, GEAR, AREA), summarize, N.species = length(unique(SPECIES)))
c3 <- ddply(catch, .( YEAR, AREA), summarize, N.species_Catch = length(unique(SPECIES)))

# Get real numbers reported at age
nAGE<-ddply(catch[
  catch$AGE0_NO_LANDED>0 | catch$AGE1_NO_LANDED>0 | catch$AGE2_NO_LANDED>0 | catch$AGE3_NO_LANDED>0 |  catch$AGE4_NO_LANDED>0| 
  catch$AGE5_NO_LANDED>0 | catch$AGE6_NO_LANDED>0| catch$AGE7_NO_LANDED>0| catch$AGE8_NO_LANDED>0| catch$AGE9_NO_LANDED>0| catch$AGE10_NO_LANDED>0| 
    catch$AGE11_NO_LANDED>0| catch$AGE12_NO_LANDED>0| catch$AGE13_NO_LANDED>0| catch$AGE14_NO_LANDED>0| catch$AGE15_NO_LANDED>0| catch$AGE16_NO_LANDED>0| 
    catch$AGE17_NO_LANDED>0| catch$AGE18_NO_LANDED>0| 
     catch$AGE19_NO_LANDED>0 | catch$AGE20_NO_LANDED>0 ,], .(COUNTRY, AREA, YEAR), summarize,  n.species_age=length(unique(as.character(SPECIES))), .drop = TRUE)


nAGER<-reshape(nAGE, idvar=c("COUNTRY", "AREA"), timevar="YEAR", direction="wide")
write.csv(nAGER, file="NumberAtAGETableACountSpecies.csv")



#landings <- read.csv("C:/data call 2015/landingsITA.csv")
#landings <- read.csv("landings_length.csv")
#names(landings) <- toupper(names(landings))

landings <- landings[landings$COUNTRY=="ITA",]

l1 <- ddply(landings, .( YEAR, GEAR, MESH_SIZE_RANGE, AREA), summarize, N.species = length(unique(SPECIES)))
l2 <- ddply(landings, .( YEAR, GEAR, AREA), summarize, N.species = length(unique(SPECIES)))
l3 <- ddply(landings, .( YEAR, AREA), summarize, N.species_Landings = length(unique(SPECIES)))

ddply(landings, .( YEAR, AREA), summarize, spe = identity(SPECIES))

# Landings @ length, this is more tricky since there are many size classes 1-100+, so need a robust and defensible rule for selecting the relevant ones

# FIX THIS TABLE TO INCLUDE ONLY >0 and ALL lenght columns, now some are missing

landingslength<-ddply(
  landings[
      landings[,16]>=0 | landings[,17]>=0 | landings[,18]>=0 | landings[,19]>=0 |  landings[,20]>=0 |  landings[,21]>=0 | landings[,22]>=0 | landings[,23]>=0 | landings[,24]>=0 | landings[,25]>=0 | landings[,26]>=0 | landings[,27]>=0 | landings[,28]>=0 | landings[,29]>=0 | landings[,30]>=0 | landings[,31]>=0 | landings[,32]>=0 | landings[,33]>=0 | landings[,34]>=0 | landings[,35]>=0 | landings[,36]>=0 | landings[,37]>=0 | landings[,38]>=0 | landings[,39]>=0 | landings[,40]>=0 | landings[,41]>=0 | landings[,42]>=0 | landings[,43]>=0 | landings[,44]>=0 | landings[,45]>=0 | landings[,46]>=0 | landings[,47]>=0 | landings[,48]>=0 | landings[,49]>=0 | 
      landings[,50]>=0 | landings[,51]>=0 | landings[,52]>=0 | landings[,53]>=0  | landings[,54]>=0 | landings[,55]>=0 | landings[,56]>=0 | landings[,57]>=0 | landings[,58]>=0 | landings[,59]>=0 |
      landings[,60]>=0 | landings[,61]>=0 | landings[,62]>=0 | landings[,63]>=0  | landings[,64]>=0 | landings[,65]>=0 | landings[,66]>=0 | landings[,67]>=0 | landings[,68]>=0 | landings[,69]>=0 |
      landings[,70]>=0 | landings[,71]>=0 | landings[,72]>=0 | landings[,73]>=0  | landings[,74]>=0 | landings[,75]>=0 | landings[,76]>=0 | landings[,77]>=0 | landings[,78]>=0 | landings[,79]>=0 |
      landings[,80]>=0 | landings[,81]>=0 | landings[,82]>=0 | landings[,83]>=0  | landings[,84]>=0 | landings[,85]>=0 | landings[,86]>=0 | landings[,87]>=0 | landings[,88]>=0 | landings[,89]>=0 |
      landings[,90]>=0 | landings[,91]>=0 | landings[,92]>=0 | landings[,93]>=0  | landings[,94]>=0 | landings[,95]>=0 | landings[,96]>=0 | landings[,97]>=0 | landings[,98]>=0 | landings[,99]>=0 | landings[,104]>=0 | landings[,105]>=0 | landings[,110]>=0
    
    ,] ,
  .(COUNTRY, AREA, YEAR), summarize,  n.species_lenght=length(unique(as.character(SPECIES))), .drop = TRUE)

landingslengthR<-reshape(landingslength, idvar=c("COUNTRY", "AREA"), timevar="YEAR", direction="wide")
write.csv(landingslengthR, file="NumberofSpeciesLandingsAtLenght2.csv")

# plot time series of species with landings @ lenght
land_spe <- ggplot(landingslength, aes(YEAR, n.species_lenght, col = AREA))+geom_point()+geom_line()+facet_grid(COUNTRY~.)
ggsave(land_spe, file=paste("DCF_NumbSpe_Landed",Sys.Date(),".png"), width=10, height=10, dpi=300)



# Combine table where at least one individual was reported either by lenght or by Age for a given speices
landsAge_Lengh <- merge(nAGE , landingslength, all=TRUE)
write.csv(landsAge_Lengh, file="NumberofSpeciesLandingsAtLenghtandAGE.csv")



# look at the species specifically
write.csv(table(landings$AREA,  landings$SPECIES), file="speciesFrequencyLandingsLength.csv")
write.csv(table(catch$AREA,  catch$SPECIES), file="speciesFrequencyCatchAge.csv")


# Check number of records with numbers of age reading in catch, landings and discards
names(catch)
write.csv(
ddply(catch[catch$NO_AGE_MEASUREMENTS_LANDINGS>0 | catch$NO_AGE_MEASUREMENTS_DISCARDS>0 | 
              catch$NO_AGE_MEASUREMENTS_CATCH>0,], .(COUNTRY, YEAR, AREA), summarize,
      NO_AGE_MEASUREMENTS_LANDINGS = length(NO_AGE_MEASUREMENTS_LANDINGS), 
      NO_AGE_MEASUREMENTS_DISCARDS = length(NO_AGE_MEASUREMENTS_DISCARDS), 
      NO_AGE_MEASUREMENTS_CATCH    = length(NO_AGE_MEASUREMENTS_CATCH)),
file = "num_age_measurmentITA.csv")


#-------------------------------------------------------------------------------------------
# Look at coverage in Fishing Effort Tables

effort <- read.csv("effort.csv", sep=";")
names(effort) <- tolower(effort)

# Effort by gear, but very long table
e1 <- ddply(effort, .( year, gear, country, area), summarize, 
            effortKW = length(nominal_effort) 
            #effortDaysSea = length(days_at_sea),
            #effortFishingdays = length(fishing_days)
            )
effortR <- reshape(e1, idvar=c("country","area", "gear"), timevar="year", direction="wide")
write.csv(effortR, file="effortDCF.csv")

# Effort by country/area only, but very long table
e2 <- ddply(effort, .( year,  country, area), summarize, effort = length(nominal_effort))
effort2R<-reshape(e2, idvar=c("country","area"), timevar="year", direction="wide")
write.csv(effort2R, file="effort2DCF.csv")

#-------------------------------------------------------------------------------------------
# Look at coverage in Discards Tables

discards <- read.csv("input_data/discards_new.csv", sep=",")


discardslength<-ddply(
  discards[discards[,15]>=0 | 
          discards[,16]>=0 |  discards[,17]>=0 | discards[,18]>=0 | discards[,19]>=0 |  discards[,20]>=0 |  discards[,21]>=0 | discards[,22]>=0 | discards[,23]>=0 | discards[,24]>=0 | discards[,25]>=0 | 
          discards[,26]>=0 |  discards[,27]>=0 | discards[,28]>=0 | discards[,29]>=0 | discards[,30]>=0 | discards[,31]>=0 | discards[,32]>=0 | discards[,33]>=0 | discards[,34]>=0 | discards[,35]>=0 | 
          discards[,36]>=0 |  discards[,37]>=0 | discards[,38]>=0 | discards[,39]>=0 | discards[,40]>=0 | discards[,41]>=0 | discards[,42]>=0 | discards[,43]>=0 | discards[,44]>=0 | discards[,45]>=0 | 
          discards[,46]>=0 |  discards[,47]>=0 | discards[,48]>=0 | discards[,49]>=0 | 
          discards[,50]>=0 |  discards[,51]>=0 | discards[,52]>=0 | discards[,53]>=0  | discards[,54]>=0 | discards[,55]>=0 | discards[,56]>=0 | discards[,57]>=0 | discards[,58]>=0 | discards[,59]>=0 |
          discards[,60]>=0 |  discards[,61]>=0 | discards[,62]>=0 | discards[,63]>=0  | discards[,64]>=0 | discards[,65]>=0 | discards[,66]>=0 | discards[,67]>=0 | discards[,68]>=0 | discards[,69]>=0 |
          discards[,70]>=0 |  discards[,71]>=0 | discards[,72]>=0 | discards[,73]>=0  | discards[,74]>=0 | discards[,75]>=0 | discards[,76]>=0 | discards[,77]>=0 | discards[,78]>=0 | discards[,79]>=0 |
          discards[,80]>=0 |  discards[,81]>=0 | discards[,82]>=0 | discards[,83]>=0  | discards[,84]>=0 | discards[,85]>=0 | discards[,86]>=0 | discards[,87]>=0 | discards[,88]>=0 | discards[,89]>=0 |
          discards[,90]>=0 |  discards[,91]>=0 | discards[,92]>=0 | discards[,93]>=0  | discards[,94]>=0 | discards[,95]>=0 | discards[,96]>=0 | discards[,97]>=0 | discards[,98]>=0 | discards[,99]>=0 | 
          discards[,100]>=0 | discards[,101]>=0 | discards[,102]>=0 | discards[,103]>=0 | discards[,104]>=0 | discards[,105]>=0 | discards[,110]>=0 ,],
     #     discards[,111]>=0 | discards[,112]>=0 | discards[,113]>=0 | discards[,114]>=0  | discards[,115]>=0 ,] ,
.(country, area,  year), summarize,  n.species_lenght=length(unique(as.character(species))), .drop = TRUE)

discardslengthR<-reshape(discardslength, idvar=c("country","area"), timevar="year", direction="wide")
write.csv(discardslengthR, file="discardslengthDCF.csv")

discardslength<-ddply(
  discards[discards[,15]>0 | 
          discards[,16]>0 |  discards[,17]>0 | discards[,18]>0 | discards[,19]>0 |  discards[,20]>0 |  discards[,21]>0 | discards[,22]>0 | discards[,23]>0 | discards[,24]>0 | discards[,25]>0 | 
          discards[,26]>0 |  discards[,27]>0 | discards[,28]>0 | discards[,29]>0 | discards[,30]>0 | discards[,31]>0 | discards[,32]>0 | discards[,33]>0 | discards[,34]>0 | discards[,35]>0 | 
          discards[,36]>0 |  discards[,37]>0 | discards[,38]>0 | discards[,39]>0 | discards[,40]>0 | discards[,41]>0 | discards[,42]>0 | discards[,43]>0 | discards[,44]>0 | discards[,45]>0 | 
          discards[,46]>0 |  discards[,47]>0 | discards[,48]>0 | discards[,49]>0 | 
          discards[,50]>0 |  discards[,51]>0 | discards[,52]>0 | discards[,53]>0  | discards[,54]>0 | discards[,55]>0 | discards[,56]>0 | discards[,57]>0 | discards[,58]>0 | discards[,59]>0 |
          discards[,60]>0 |  discards[,61]>0 | discards[,62]>0 | discards[,63]>0  | discards[,64]>0 | discards[,65]>0 | discards[,66]>0 | discards[,67]>0 | discards[,68]>0 | discards[,69]>0 |
          discards[,70]>0 |  discards[,71]>0 | discards[,72]>0 | discards[,73]>0  | discards[,74]>0 | discards[,75]>0 | discards[,76]>0 | discards[,77]>0 | discards[,78]>0 | discards[,79]>0 |
          discards[,80]>0 |  discards[,81]>0 | discards[,82]>0 | discards[,83]>0  | discards[,84]>0 | discards[,85]>0 | discards[,86]>0 | discards[,87]>0 | discards[,88]>0 | discards[,89]>0 |
          discards[,90]>0 |  discards[,91]>0 | discards[,92]>0 | discards[,93]>0  | discards[,94]>0 | discards[,95]>0 | discards[,96]>0 | discards[,97]>0 | discards[,98]>0 | discards[,99]>0 | 
          discards[,100]>0 | discards[,101]>0 | discards[,102]>0 | discards[,103]>0 | discards[,104]>0 | discards[,105]>0 | discards[,110]>0 ,],
     #     discards[,111]>0 | discards[,112]>0 | discards[,113]>0 | discards[,114]>0  | discards[,115]>0 ,] ,
.(country, area,  year), summarize,  n.species_lenght=length(unique(as.character(species))), .drop = TRUE)

discardslengthR<-reshape(discardslength, idvar=c("country","area"), timevar="year", direction="wide")
write.csv(discardslengthR, file="discardslengthDCF2.csv")








##############################################################################################################
# Checks on Acoustic survey data
##############################################################################################################


# Load files
abund_biom <- read.csv("input_data/abund_biom.csv", sep=",")

# fix area miscoding in some areas  from 2014 call
#abund_biom$area <- as.character (abund_biom$area)
#abund_biom$area <- ifelse(abund_biom$area == "SA16", "SA 16", abund_biom$area)
#abund_biom$area <- ifelse(abund_biom$area == "SA17", "SA 17", abund_biom$area)
#abund_biom$area <- ifelse(abund_biom$area == "SA18", "SA 18", abund_biom$area)

# remove ROM and BUL since not affected by this call
#abund_biom <- abund_biom[!(abund_biom$country=="ROM" | abund_biom$country=="BUL"),]

# Plot the survey timing

timing <- ggplot(abund_biom, aes(year, end_month))+geom_point(aes(color = "blue", shape = "b"))+
  geom_point(aes(year, start_month, color = "red"))+facet_wrap(area~country)+ylab("Month of the Year")+
  theme(legend.position="none")

 ggsave(timing, file=paste("AcousticSurveyTiming",Sys.Date(),".png"), width=12, height=8, dpi=300)


# Aggregate by species over sex

abund_biom <- abund_biom[2:24] # To remove empty columns with ages > 8


# aggregate over age classes

abund_biomL <- (melt(abund_biom, id.vars=c("country", "area", "species", "sex", "year", "name_survey")))

# split abund from biomass
abund_biomL$var <- grepl("abund", abund_biomL$variable)
abund_biomL$vars <- ifelse(abund_biomL$var == TRUE, "abund", "biomass")


##########################################################################################################
# In SA 1 and SA 6 it is evident that the in 2012 and 2013 the numbers at age have been provided in actual numbers rather than thousands as 
# specified in the Data Call, thus scale it /1000

abund_biomL$value <- ifelse(abund_biomL$area == "SA 6" & (abund_biomL$year == 2012 | abund_biomL$year == 2013) &
                              abund_biomL$vars == "abund", abund_biomL$value / 1000 , abund_biomL$value)
abund_biomL$value <- ifelse( abund_biomL$area == "SA 1" & abund_biomL$year == 2012  & abund_biomL$vars == "abund" , 
                             abund_biomL$value / 1000 , abund_biomL$value)

# Fix data from SA 16, e.g. data pre 2012 were not reported in thousands, while the data since 2012 is in thousands
abund_biomL$value <- ifelse(abund_biomL$area == "SA 16" & abund_biomL$year <2012 & abund_biomL$vars == "abund" ,
                            abund_biomL$value / 1000 , abund_biomL$value)

# Need the same as in 16 in SA 15?

###########################################################################################################

temp <- ddply(abund_biomL, .(country, area, year, vars, species), summarize, value2 = sum(value, na.rm= T))

# Plot Abundance
ab <- ggplot(temp[temp$vars=="abund",], aes(year, value2, color = species))+geom_point()+
  facet_wrap(country~area, scales = "free_y")+geom_line()+ylab("Abundance, in Thousands")+
  xlim(2000,2016)
      ggsave(ab, file=paste("CORRECTEDAcousticAbundanceDCF",Sys.Date(),".png"), width=12, height=8, dpi=300)


# Plot Biomass
bio <- ggplot(temp[temp$vars=="biomass",], aes(year, value2, color = species))+geom_point()+
  facet_wrap(country~area, scales="free_y")+geom_line()+ylab("Biomass, in Tons")+
  xlim(2000,2016)
      ggsave(bio, file=paste("CORRECTEDAcousticBiomassDCF",Sys.Date(),".png"), width=12, height=8, dpi=300)
      

      
      
##########################################################################      
# EXplore Biomass Tables
#########################################################################
      
biomass <- read.csv("input_data/biomass.csv") 

biomass$biom <- rowSums(biomass[,13:113], na.rm = TRUE)

# aggregate over Sex 
biomass2 <- ddply(biomass, .(country, area, year, species), summarize, biom = sum(biom, na.rm= T))

bio2 <- ggplot(biomass2, aes(year, biom, color = species)) + geom_point()+
  facet_wrap(country~area, scales="free_y")+geom_line()+ylab("Biomass, in Tons")+
  xlim(2000,2016)

ggsave(bio2, file=paste("Abundance_ABUNDANCEatLenght",Sys.Date(),".png"), width=12, height=8, dpi=300)


# look at coverage in biomass
biomass2 <- ddply(biomass, .(country, area, year), summarize, N_spec = length(unique(as.character(species))))



###################
# Explore covarage and trends from ABUNDANCE@length  table

abundance <- read.csv("input_data/abundance.csv")
abundance$abund <- rowSums(abundance[,13:113], na.rm = TRUE)

# aggregate over Sex 
abundance2 <- ddply(abundance, .(country, area, year, species), summarize, abund = sum(abund, na.rm= T))

abu2 <- ggplot(abundance2, aes(year, abund, color = species)) + geom_point()+
  facet_wrap(country~area, scales="free_y")+geom_line()+ylab("abundance, in Thousands")+
  xlim(2000,2016)

ggsave(abu2, file=paste("abundance_from_ABUNDANCEatLenght",Sys.Date(),".png"), width=12, height=8, dpi=300)


# look at coverage in biomass
biomass2 <- ddply(biomass, .(country, area, year), summarize, N_spec = length(unique(as.character(species))))



###################################################################################################################
# EXPLORE Numbers @ Age by survey
###################################################################################################################

# Sum over SEX
temp_age <- ddply(abund_biomL, .(country, area, year, vars, variable, species), summarize, value2 = sum(value, na.rm= T))
   
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

# Plot Abundance
abund_age <- ggplot(temp_age[temp_age$vars=="abund" &  temp_age$value2 >0 & temp_age$country == "GRC",], aes(year, value2))+
  geom_point(aes(color = variable))+
  facet_wrap(species ~ area+country, scales= "free_y")+geom_line(aes(color = variable)) + 
   scale_colour_manual(values=cbbPalette) +ylab("Abundance, in Thousands")
      
ggsave(abund_age, file=paste("UNCORRECTEDAcousticAbundanceDCF@Age",Sys.Date(),".png"), width=10, height=10, dpi=300)
      
      
      
      

biomass_age <- ggplot(temp_age[temp_age$vars=="biomass"&  temp_age$value2 >0 & temp_age$country == "GRC",], aes(year, value2))+
  geom_point(aes(color = variable))+
  facet_wrap(species ~ area+country, scales= "free_y")+geom_line(aes(color = variable)) + 
   scale_colour_manual(values=cbbPalette) +ylab("Biomass, Tons")
      
ggsave(biomass_age, file=paste("UNCORRECTEDAcousticBiomassDCF@Age",Sys.Date(),".png"), width=10, height=10, dpi=300)









#######################################################################################################################
# Look at biological parameters Tables
#################################################################################################################
# GP
gp <- read.csv("gp.csv", sep=";")
gp1 <- ddply(gp[gp$vb_linf > 0 ,], .( end_year, country, area), summarize, VBPar = length(vb_linf))
gpR<-reshape(gp1, idvar=c("country","area"), timevar="end_year", direction="wide")
write.csv(gpR, file="gpDCF.csv")

# MA
ma <- read.csv("ma.csv", sep=";")
ma1 <- ddply(ma[ma$prm >= 0 ,], .( end_year, country, area), summarize, 
             Num_Spec_prm = length(unique(as.character(species))))
maR<-reshape(gp1, idvar=c("country","area"), timevar="end_year", direction="wide")
write.csv(maR, file="maDCF.csv")

# ML
ml <- read.csv("ml.csv", sep=";")
ml1 <- ddply(ml[ml$prm >= 0 ,], .( end_year, country, area), summarize, 
             Num_Spec_prm = length (unique(as.character(species))))
mlR<-reshape(ml1, idvar=c("country","area"), timevar="end_year", direction="wide")
write.csv(mlR, file="mlDCF.csv")

# SRL
srl <- read.csv("srl.csv", sep=";")
srl1 <- ddply(srl[srl$sexratio >= 0 ,], .( end_year, country, area), summarize, 
             Num_Spec_sexrat = length (unique(as.character(species))))
srlR<-reshape(srl1, idvar=c("country","area"), timevar="end_year", direction="wide")
write.csv(srlR, file="srlDCF.csv")

# SRA
sra <- read.csv("sra.csv", sep=";")
sra1 <- ddply(sra[sra$sexratio >= 0 ,], .( end_year, country, area), summarize, 
             Num_Spec_sexra = length (unique(as.character(species))))
sraR<-reshape(sra1, idvar=c("country","area"), timevar="end_year", direction="wide")
write.csv(sraR, file="sraDCF.csv")







############################################################################################################
# MEDITS CHECKS
############################################################################################################

library(ggplot2); library(plyr)

setwd("E:/quality_checks_data_call_2016")
TA <- read.csv("input_data/medits_ta.csv", sep=",")
TB <- read.csv("input_data/medits_tb.csv", sep=",")
TC <- read.csv("input_data/medits_tc.csv", sep=",")


names(TA) <- toupper(names(TA))
names(TB) <- toupper(names(TB))
names(TC) <- toupper(names(TC))

# plot number of hauls performed per month
ggplot(ddply(TA, .(YEAR, COUNTRY, AREA, MONTH), summarize, num = length(HAUL_NUMBER)), 
       aes(x=YEAR, y=AREA, col=factor(AREA ))) + geom_point() + geom_line()+ 
        facet_wrap( MONTH ~ COUNTRY)+ ylab("Month")

ggplot(ddply(TA, .(YEAR, COUNTRY, AREA, MONTH), summarize, num = length(HAUL_NUMBER)), 
       aes(x=YEAR, y=num, col=factor(MONTH ))) + geom_point() +# geom_line()+ 
  facet_wrap( COUNTRY~ AREA)+ ylab("Month")


ggplot(ddply(TA[TA$COUNTRY=="ITA" | TA$COUNTRY=="GRC" , ], .(YEAR, COUNTRY, AREA, MONTH), summarize, num = length(HAUL_NUMBER)), aes(x=YEAR, y=AREA, col=factor(AREA )))+
  geom_point()+facet_grid(MONTH ~ COUNTRY + AREA) + ylab("Month")

# All countries
ggplot(ddply(TA, .(YEAR, COUNTRY, AREA, MONTH), summarize, num = length(HAUL_NUMBER)), 
       aes(x=YEAR, y=MONTH))+geom_path()+geom_point()+stat_smooth( se = FALSE)+
  facet_wrap(AREA~COUNTRY)+
  scale_y_continuous(breaks=c(1,2,3, 4, 5, 6,7,8, 9, 10, 11,12))+
  geom_hline(yintercept=6, colour="red", linetype = "longdash", size =1)+  ylim(4,12)

ggsave(last_plot(), file="Meditstiming2.png", width=12, height=8)

ggplot(ddply(TA[TA$COUNTRY == "ITA",], .(YEAR, COUNTRY, AREA, MONTH), summarize, num = length(HAUL_NUMBER)), 
       aes(x=YEAR, y=MONTH))+geom_path()+stat_smooth( se = FALSE)+
  facet_grid(~AREA, labeller = "label_both")+
  scale_y_continuous(breaks=c(1,2,3, 4, 5, 6,7,8, 9, 10, 11,12))+
  geom_hline(yintercept=6, colour="red", linetype = "longdash", size =1)




# Number of tows per year in TA

towta <- ddply(TA, .(YEAR, COUNTRY, AREA), summarize, num = length(unique(HAUL_NUMBER)))
towtb <- ddply(TB, .(YEAR, COUNTRY, AREA), summarize, num = length(unique(HAUL_NUMBER)))
towtc <- ddply(TC, .(YEAR, COUNTRY, AREA), summarize, num = length(unique(HAUL_NUMBER)))

tow <-merge(towta, towtb, by=c("YEAR", "COUNTRY", "AREA"), all.x = TRUE)
tow <-merge(tow, towtc, by=c("YEAR", "COUNTRY", "AREA"), all.x = TRUE)
write.csv(tow, file = "number_hauls_medits_ta_tb_tc.csv")

ggplot(ddply(TA, .(YEAR, COUNTRY, AREA), summarize, num = length(HAUL_NUMBER)), 
       aes(x=YEAR, y=num))+geom_path()+ #stat_smooth(method="lm", se = FALSE)+
  facet_wrap(COUNTRY ~ AREA)+ ylab("Number of Tows")
ggsave(last_plot(), file="MeditsNumTows.png", width=12, height=8)

# Number of SPECIES per year in TB AND TC
 
SPtb <- ddply(TB, .(YEAR, COUNTRY, AREA), summarize, num = length(unique(SPECIES)))
SPtc <- ddply(TC, .(YEAR, COUNTRY, AREA), summarize, num = length(unique(SPECIES)))

SP <-merge(SPtb, SPtc, by=c("YEAR", "COUNTRY", "AREA"), all = TRUE)
names(SP)[4:5] <-c("Num_species_TB", "Num_species_TC")
write.csv(SP, file = "number_SPECIES_medits_tb_tc.csv")

ggplot(SP, aes(x=YEAR, y=Num_species_TB))+geom_path()+ #stat_smooth(method="lm", se = FALSE)+
  facet_wrap(COUNTRY ~ AREA)+ ylab("Number of Tows")

ggplot(SP)+geom_path(aes(x=YEAR, y=Num_species_TB, col = "blue"))+geom_path(aes(x=YEAR, y=Num_species_TC, col = "red"))+ #stat_smooth(method="lm", se = FALSE)+
  facet_wrap(COUNTRY ~ AREA)+ ylab("Number of Species")


