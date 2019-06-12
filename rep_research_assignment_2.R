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
storm.data$EVENT.TYPE<-ifelse(grepl('TORN|GUST|FUNNEL|ROTA', storm.data$EVTYPE), 'TORNADO',
            ifelse(grepl('WINT|ICE|COOL| LOW|COLD|FROST|AVAL|BLIZ|SNOW|FREEZ|FROZ|LOW TEMP|HYPOTH|ICY', storm.data$EVTYPE),
                   'EXTREME WINTER',
                   ifelse(grepl('STOR|WIND|HURR|TYPH|TURB|WND', storm.data$EVTYPE), 
                          'STORM',
                          ifelse(grepl('DOWNBURST|WATER|FLOO|WET|RAIN|PRECIP|COASTAL|DROWN|TSU|DAM|SHOW|SPOUT|FLD',storm.data$EVTYPE),
                                 'FLOOD',
                                 ifelse(grepl('HAIL',storm.data$EVTYPE),
                                        'HAIL',
                                        ifelse(grepl('HEAT|WARM|HIGH TEMP|HOT|HYPER|TROP',storm.data$EVTYPE),
                                               'EXCESS HEAT',
                                               ifelse(grepl('FIRE|SMOK',storm.data$EVTYPE),
                                                      'FIRE',
                                                      ifelse(grepl('DRY|DRI|DROUG', storm.data$EVTYPE),
                                                             'DROUGHT',
                                                             ifelse(grepl('SEA|ROUGH|WAV|MARI|BEACH|RIP|TIDE|CURR|HIGH SEA|SURF|DROW',storm.data$EVTYPE),
                                                                    'SEA AND OCEAN',
                                                                    ifelse(grepl('LIGH',storm.data$EVTYPE), 'LIGHTNING',
                                                                           ifelse(grepl('VOLC',storm.data$EVTYPE), 'VOLCANO',
                                                                                  ifelse(grepl('DUS|FOG|VOG',storm.data$EVTYPE ),'FOG&DUST',
                                                                                         ifelse(grepl('SLI|SWE|LAND', storm.data$EVTYPE),
                                                                                                'LAND SLIDE',
                                                                                                'OTHER')))))))))))))
unique(storm.data$EVENT.TYPE)

#PROPDMGEXP
unique(storm.data$PROPDMGEXP)
multip.lookup.table <- data.frame(
  symb=unique(as.character(storm.data$PROPDMGEXP)),
  expon=c(3,6,0,9,6,0,0,5,6,0,4,2,3,2,7,2,0,1,8))
multip.lookup.table
storm.data$PROPDMGVAL<-storm.data$PROPDMG*10^multip.lookup.table$expon[match(storm.data$PROPDMGEXP, multip.lookup.table$symb)]
storm.data$CROPDMGVAL<-storm.data$CROPDMG*10^multip.lookup.table$expon[match(storm.data$CROPDMGEXP, multip.lookup.table$symb)]
storm.data$TOTALDMGVAL <- storm.data$PROPDMGVAL+storm.data$CROPDMGVAL
