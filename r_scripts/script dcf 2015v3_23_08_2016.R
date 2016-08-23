library(plyr); library(dplyr); library(reshape2)
library(ggplot2); library(zoo);library(gridExtra)

setwd("S:/data call 2016/quality_checks_data_call_2016")

# Catch @ Age Data

catch <- read.csv("input_data/catches_new.csv")
names(catch) <-toupper(names(catch))

# fix areas
#catch <- catch[catch$YEAR >1900 & !catch$AREA==-1,]
#catch$AREA <- as.character(catch$AREA)
#catch$AREA <- ifelse(catch$AREA == "16", "SA 16", catch$AREA)
#catch$AREA <- ifelse(catch$AREA == "10", "SA 10", catch$AREA)

# create a time stamp year-quarter for plotting
# if Quarter is = -1, convert to a mid year quarter => 2
catch$QUARTER <- ifelse(catch$QUARTER == -1, 2,catch$QUARTER)
catch$QUARTER <- ifelse(catch$QUARTER <0, catch$QUARTER*-1,catch$QUARTER)

catch$date <- paste(catch$YEAR, catch$QUARTER, "01", sep ="-" )

# convert date to yearmon class
catch <- transform(catch, date = as.yearqtr(date))


##############
# Plot Data
catch$gear2 <- paste(catch$VESSEL_LENGTH,  catch$GEAR, catch$FISHERY, sep = "-")


p = ggplot(catch[catch$SPECIES=="HKE",], aes(x=date, y = LANDINGS, col = gear2)) +
  geom_line()+ geom_point()+
  facet_wrap(AREA ~ COUNTRY+SPECIES , scales = "free_y") +
  scale_x_yearqtr(format = "%YQ%q")+
  theme( axis.text.x = element_text(angle = 60, hjust = 1))+
  xlab("Year + Quarter")+ylab("Landings (t)")+
  guides(col = guide_legend(nrow =  40, bcol = TRUE), guide_legend(title = "Gear Fishery VL"),
  legend.text = element_text(size = 6))
#p 

plots = dlply(catch , "SPECIES", `%+%`, e1 = p)
ml = do.call(marrangeGrob, list(grobs=plots, nrow = 1, ncol = 2))
ggsave("DCFCatch_Area_Country_Species_Gear_VL.pdf", ml , width=18, height=12, dpi=300)


# Explore Trends by YEAR and SPECIES

ct1 <- ddply(catch[catch$LANDINGS>0,], .( date, COUNTRY, AREA, SPECIES), summarize,
             Catch = sum(LANDINGS, na.rm = TRUE))

p2 =  ggplot(ct1, aes(x=date, y = Catch, col = AREA)) +
      geom_line()+ geom_point()+
      facet_wrap(COUNTRY ~SPECIES, scales = "free_y")+ 
      scale_x_yearqtr(format = "%YQ%q")+
      theme( axis.text.x = element_text(angle = 60, hjust = 1))+
      xlab("Year")+ylab("Landings (t)")+
  scale_fill_brewer(palette="Spectral")

p2 

plots2 = dlply(ct1 , "SPECIES", `%+%`, e1 = p2)
ml2 = do.call(marrangeGrob, list(grobs = plots2, nrow=1, ncol=1))
ggsave("DCFCatch_Area_Country_Species.pdf", ml2 , width=18, height=12, dpi=300)



#=====================================================================================================
# Explore Landings @ Lenght Data

landings <- read.csv("input_data/landings_new.csv")
names(landings) <- toupper(names(landings))

# create a time stamp year-quarter for plotting
# if Quarter is = -1, convert to a mid year quarter => 2
landings$QUARTER <- ifelse(landings$QUARTER == -1, 2,landings$QUARTER)
landings$QUARTER <- ifelse(landings$QUARTER <0, landings$QUARTER*-1,landings$QUARTER)

landings$date <- paste(landings$YEAR, landings$QUARTER, "01", sep ="-" )

# convert date to yearmon class
landings <- transform(landings, date = as.yearqtr(date))


##############
# Plot Data
landings$gear2 <- paste(landings$VESSEL_LENGTH, landings$GEAR, landings$FISHERY, sep = "-")


pl = ggplot(landings[landings$SPECIES=="HKE",], aes(x=date, y = LANDINGS, col = gear2)) +
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

lnd1 <- ddply(landings[landings$LANDINGS>0,], .( date, COUNTRY, AREA, SPECIES), summarize,
              Catch = sum(LANDINGS, na.rm = TRUE))

plnd =  ggplot(lnd1, aes(x=date, y = Catch, col = AREA)) +
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




# Compare Kw Days and Days at Sea

