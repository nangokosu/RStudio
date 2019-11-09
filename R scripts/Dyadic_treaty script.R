library(tidyverse)
library(readxl)
file_path<-file.path("C:","Users","S410U","Documents","GitHub","RStudio","Country Summary",sep="/")
#For reproducibility
Country_Summary<-list.files(file_path)
Country_Summary<-paste(rep(file_path,length(Country_Summary)),Country_Summary,sep="")
cs<-lapply(Country_Summary,read_csv)
cs<-cs[-1]
cs_df<-as.tibble(bind_rows(cs))
cs1<-cs_df%>%filter(Indicator%in%c( "Exports (in US$ Mil)","Imports (in US$ Mil)"  ),Partner=="World")%>%gather(key="Year",value="Value",-c("Reporter","Partner","Product categories","Indicator Type","Indicator"))
cs1$Year<-as.numeric(str_remove(cs1$Year,"X"))
cs1$Indicator[cs1$Indicator=="Exports (in US$ Mil)"]<-"Export to world (in US$ Mil)"
cs1$Indicator[cs1$Indicator=="Imports (in US$ Mil)"]<-"Import to world (in US$ Mil)"
cs1<-select(cs1,Reporter,Indicator,Year,Value)

library(XML)
casey<-xmlToList("http://wits.worldbank.org/API/V1/wits/datasource/trn/country/ALL")
casey1<-casey[[1]]
casey1[["country"]][["name"]]
Reporter<-unlist(lapply(casey1,function(x){x[["name"]]}))
ISO_3<-unlist(lapply(casey1,function(x){x[["iso3Code"]]}))
ISO3Code<-tibble(Reporter,ISO_3)
ISO3Code$ISO_3_fixed<-ISO3Code$ISO_3
ISO3Code$ISO_3_fixed[ISO3Code$ISO_3_fixed=="SUD"]<-"SDN"
ISO3Code$ISO_3_fixed[ISO3Code$ISO_3_fixed=="ROM"]<-"ROU"
ISO3Code$ISO_3_fixed[ISO3Code$ISO_3_fixed=="SER"]<-"SRB"
library(countrycode)
ISO3Code$ISO_3n<-countrycode(ISO3Code$ISO_3_fixed,"iso3c","iso3n",nomatch=NULL)
ISO3Code<-filter(ISO3Code,!is.na(ISO_3n))%>%select(Reporter,ISO_3,ISO_3n)
#Saving for future reference
saveRDS(ISO3Code,"C:/Users/S410U/Documents/RStudio/ISO3Code.rds")

country_iso_code<-inner_join(ISO3Code,cs1)
#Note: Netherlands Antilles, European Union, Montenegro, European Union, East Timor do not have conversion from ISO_3 to ISO_3n
by_country_iso_global<-spread(country_iso_code,key=Indicator,value=Value)
by_country_iso_global$Year<-as.numeric(by_country_iso_global$Year)
rawCountrySummary<-gather(cs_df,key=Year,value=Value,'2015':'1988')
rawCountrySummary$Year<-as.numeric(rawCountrySummary$Year)
CS1<-inner_join(rawCountrySummary,by_country_iso_global,by=c('Year'='Year','Reporter'='Reporter'))
CS1<-select(CS1,-'Indicator Type')
CS1<-inner_join(ISO3Code,CS1,by=c("Reporter"="Partner"))
Indicators<-c("Trade (US$ Mil)-Top 5 Export Partner","Trade (US$ Mil)-Top 5 Import Partner")
CS2<-filter(CS1,`Product categories`=="All Products",Indicator%in%Indicators)%>%spread(key=Indicator,value =Value)
CS2$perc_export<-round(CS2$`Trade (US$ Mil)-Top 5 Export Partner`/CS2$`Export to world (in US$ Mil)`,digits=3)
CS2$perc_import<-round(CS2$`Trade (US$ Mil)-Top 5 Import Partner`/CS2$`Import to world (in US$ Mil)`,digits=3)


dyadic_treaty<-read_excel("C:/Users/S410U/Documents/GitHub/RStudio/Dyadic/List of dyadic treaties.xlsx")
dyadic_treat2<-read_excel("C:/Users/S410U/Documents/GitHub/RStudio/Dyadic/List of dyadic treaties.xlsx")

dyadic_treat2$country1<-dyadic_treaty$country2
dyadic_treat2$country2<-dyadic_treaty$country1
dyadic_treat2$iso1<-dyadic_treaty$iso2
dyadic_treat2$iso2<-dyadic_treaty$iso1
dyad_treaty<-bind_rows(dyadic_treaty,dyadic_treat2)%>%select(-country1,-country2,-year)
final<-inner_join(CS2,dyad_treaty,by=c("ISO_3n.x"="iso1","ISO_3n.y"="iso2"))
final<-select(final,"Reporter",Reporter_ISO3c="ISO_3.x",Reporter_ISO3n="ISO_3n.x",Partner="Reporter.y",Partner_ISO3c="ISO_3.y",Partner_ISO3n="ISO_3n.y","Year","Export (US$ Mil)_partner"="Trade (US$ Mil)-Top 5 Export Partner","Import (US$ Mil)_partner"="Trade (US$ Mil)-Top 5 Import Partner","Export to world (in US$ Mil)","Import to world (in US$ Mil)","Percentage of partner export to world export"="perc_export","Percentage of partner import to world import"="perc_import","number","base_treaty","name","entry_type","consolidated","coded","entryforceyear","typememb","regioncon","wtolisted","wto_name") 
final<-arrange(final,Reporter,Partner,desc(Year))
saveRDS(final,"C:/Users/S410U/Documents/RStudio/country_dyadicvsworld.rds")