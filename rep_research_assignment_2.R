#loading data
    file.url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
    if (!file.exists('stormdata.bz2')) {download.file(url=file.url, destfile = 'stormdata.bz2')}
#reading data
    storm.data <- read.csv('stormdata.bz2')
#subsetting data
    storm.data<- storm.data[c("EVTYPE","FATALITIES","INJURIES","PROPDMG", "PROPDMGEXP","CROPDMG","CROPDMGEXP")]
#processing
    unique(storm.data$EVTYPE)
    storm.data$EVTYPE<- toupper(storm.data$EVTYPE)
    storm.data$EVENT.TYPE<-
      ifelse(grepl('TORN|GUST|FUNNEL|ROTA', storm.data$EVTYPE), 'TORNADO',
      ifelse(grepl('WINT|ICE|COOL| LOW|COLD|FROST|AVAL|BLIZ|SNOW|FREEZ|FROZ|LOW TEMP|HYPOTH|ICY', storm.data$EVTYPE),'EXTREME WINTER',
      ifelse(grepl('STOR|WIND|HURR|TYPH|TURB|WND', storm.data$EVTYPE), 'STORM',
      ifelse(grepl('DOWNBURST|WATER|FLOO|WET|RAIN|PRECIP|COASTAL|DROWN|TSU|DAM|SHOW|SPOUT|FLD',storm.data$EVTYPE),'FLOOD',
      ifelse(grepl('HAIL',storm.data$EVTYPE),'HAIL',
      ifelse(grepl('HEAT|WARM|HIGH TEMP|HOT|HYPER|TROP',storm.data$EVTYPE),'EXCESS HEAT',
      ifelse(grepl('FIRE|SMOK',storm.data$EVTYPE),'FIRE',
      ifelse(grepl('DRY|DRI|DROUG', storm.data$EVTYPE),'DROUGHT',
      ifelse(grepl('SEA|ROUGH|WAV|MARI|BEACH|RIP|TIDE|CURR|HIGH SEA|SURF|DROW',storm.data$EVTYPE),'SEA AND OCEAN',
      ifelse(grepl('LIGH',storm.data$EVTYPE), 'LIGHTNING',
      ifelse(grepl('VOLC',storm.data$EVTYPE), 'VOLCANO',
      ifelse(grepl('DUS|FOG|VOG',storm.data$EVTYPE ),'FOG&DUST',
      ifelse(grepl('SLI|SWE|LAND', storm.data$EVTYPE),'LAND SLIDE',
      'OTHER')))))))))))))
unique(storm.data$EVENT.TYPE)
#Fatalities 
    fatalities <- aggregate(FATALITIES~ EVENT.TYPE, storm.data, sum)
    par(mar=c(8, 6, 3, 1))
    barplot(height = fatalities$FATALITIES[order(fatalities$FATALITIES, decreasing = T)], 
            names.arg = fatalities$EVENT.TYPE[order(fatalities$FATALITIES, decreasing = T)], 
            las = 2,cex.names = .8,
            ylab = 'Number of fatalities (Count)\n',
            main = 'Fatalities of Extreme weather \n Conditions in the US from 1950 until 2011')
#Injuries 
    injuries <- aggregate(INJURIES~ EVENT.TYPE, storm.data, sum)
    par(mar=c(8, 6, 3, 1))
    barplot(height = injuries$INJURIES[order(injuries$INJURIES, decreasing = T)], 
            names.arg = injuries$EVENT.TYPE[order(injuries$INJURIES, decreasing = T)], 
            las = 2,cex.names = .8,
            ylab = 'Number of injuries (Count)\n',
            main = 'Injuries of Extreme weather \n Conditions in the US from 1950 until 2011')
#Economy 
    
    #fixing the multiplier in PROPDMGEXP and CROPDMGEXP
    unique(storm.data$PROPDMGEXP)
    multip.lookup.table <- data.frame(
      symb=unique(as.character(storm.data$PROPDMGEXP)),
      expon=c(3,6,0,9,6,0,0,5,6,0,4,2,3,2,7,2,0,1,8))
    multip.lookup.table
    storm.data$PROPDMGVAL<-storm.data$PROPDMG*10^multip.lookup.table$expon[match(storm.data$PROPDMGEXP, multip.lookup.table$symb)]
    storm.data$CROPDMGVAL<-storm.data$CROPDMG*10^multip.lookup.table$expon[match(storm.data$CROPDMGEXP, multip.lookup.table$symb)]
    storm.data$TOTALDMGVAL <- storm.data$PROPDMGVAL+storm.data$CROPDMGVAL
    
    tot.dmg<- aggregate(TOTALDMGVAL~ EVENT.TYPE, storm.data, sum)
    par(mar=c(8, 6, 3, 4))
    barplot(height = tot.dmg$TOTALDMGVAL[order(tot.dmg$TOTALDMGVAL, decreasing = T)]/10^6, 
            names.arg = tot.dmg$EVENT.TYPE[order(tot.dmg$TOTALDMGVAL, decreasing = T)], 
            las = 2,cex.names = .8,
            ylab = 'Damage cost (Milion Dollars)\n',
            main = 'Economical damage caused by Extreme Weather \n Conditions in the US from 1950 until 2011')
