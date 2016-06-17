library(downloader)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

#Download Raw files
download("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv",destfile="./Data/Raw/gdp.csv")
download("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv",destfile="./Data/Raw/edstats_country.csv")

## GDP
#load csv to variable
rawGdp <- read.csv("./Data/Raw/gdp.csv")#, sep = ",", skip = 5, header = FALSE, )

#Remove empty raws at the top and the bottom, and pull columns with data
gdp <- rawGdp[5:235,c(1,4,5)]
#assign column names
names(gdp) <- c("CountryCode","country","gdp")

#remove empty rows by counting number of characters in CountryCode
gdp <- subset(gdp, nchar(as.character(CountryCode)) > 0)

#Check if duplicate country codes
table(gdp$CountryCode)[table(gdp$CountryCode)>1]
table(gdp$country)[table(gdp$country)>1]

##EDSTATS_Country
#load csv to variable
rawEdstats <- read.csv("./Data/Raw/edstats_country.csv")#, sep = ",", skip = 5, header = FALSE, )

#Remove row for world
edstats <- subset(rawEdstats[,c(1:28)], CountryCode != 'WLD')

#Check if duplicate country codes
table(edstats$CountryCode)[table(edstats$CountryCode)>1]
table(edstats$country)[table(edstats$country)>1]

#merge gdp and edstats into one dataframe 
#We get 223 matching records
gdpEdstats <- merge(gdp, edstats, by = "CountryCode")
matchingRecords <- dim(gdpEdstats)[1]


#Do we have duplicate rows
gdpEdstats[duplicated(gdpEdstats[,1:30]),]

#Recode columns
gdpEdstats$Latest.population.census[gdpEdstats$Latest.population.census == "2006 (rolling)"] <- "2006"

#Count number on NAs
sum(is.na(gdpEdstats$CountryCode))
sum(is.na(gdpEdstats$country))
sum(is.na(gdpEdstats$gdp))
sum(is.na(gdpEdstats$Long.Name))
sum(is.na(gdpEdstats$Income.Group))
sum(is.na(gdpEdstats$Region))
sum(is.na(gdpEdstats$Lending.category))
sum(is.na(gdpEdstats$Other.groups))
sum(is.na(gdpEdstats$Currency.Unit))
sum(is.na(gdpEdstats$Latest.population.census))
sum(is.na(gdpEdstats$Latest.household.survey))
sum(is.na(gdpEdstats$Special.Notes))
sum(is.na(gdpEdstats$National.accounts.base.year))
sum(is.na(gdpEdstats$National.accounts.reference.year))
sum(is.na(gdpEdstats$System.of.National.Accounts))
sum(is.na(gdpEdstats$SNA.price.valuation))
sum(is.na(gdpEdstats$Alternative.conversion.factor))
sum(is.na(gdpEdstats$PPP.survey.year))
sum(is.na(gdpEdstats$Balance.of.Payments.Manual.in.use))
sum(is.na(gdpEdstats$External.debt.Reporting.status))
sum(is.na(gdpEdstats$System.of.trade))
sum(is.na(gdpEdstats$Government.Accounting.concept))
sum(is.na(gdpEdstats$IMF.data.dissemination.standard))
sum(is.na(gdpEdstats$Source.of.most.recent.Income.and.expenditure.data))
sum(is.na(gdpEdstats$Vital.registration.complete))
sum(is.na(gdpEdstats$Latest.agricultural.census))
sum(is.na(gdpEdstats$Latest.industrial.data))
sum(is.na(gdpEdstats$Latest.trade.data))
sum(is.na(gdpEdstats$Latest.water.withdrawal.data))
sum(is.na(gdpEdstats$X2.alpha.code))

#Make numbers look like numbers
gdpEdstats$gdp <- str_replace_all(gdpEdstats$gdp, "[[:punct:]]", "")

#change long name data type
gdpEdstats$Long.Name <- as.character(gdpEdstats$Long.Name)

#Remove NAs
gdpEdstats$CountryCode[is.na(gdpEdstats$CountryCode)] <- ""
gdpEdstats$country[is.na(gdpEdstats$country)] <- ""
gdpEdstats$gdp[is.na(gdpEdstats$gdp)] <- ""
gdpEdstats$Long.Name[is.na(gdpEdstats$Long.Name)] <- ""
gdpEdstats$Income.Group[is.na(gdpEdstats$Income.Group)] <- ""
gdpEdstats$Region[is.na(gdpEdstats$Region)] <- ""
gdpEdstats$Lending.category[is.na(gdpEdstats$Lending.category)] <- ""
gdpEdstats$Other.groups[is.na(gdpEdstats$Other.groups)] <- ""
gdpEdstats$Currency.Unit[is.na(gdpEdstats$Currency.Unit)] <- ""
gdpEdstats$Latest.population.census[is.na(gdpEdstats$Latest.population.census)] <- ""
gdpEdstats$Latest.household.survey[is.na(gdpEdstats$Latest.household.survey)] <- ""
gdpEdstats$Special.Notes[is.na(gdpEdstats$Special.Notes)] <- ""
gdpEdstats$National.accounts.base.year[is.na(gdpEdstats$National.accounts.base.year)] <- ""
gdpEdstats$National.accounts.reference.year[is.na(gdpEdstats$National.accounts.reference.year)] <- ""
gdpEdstats$System.of.National.Accounts[is.na(gdpEdstats$System.of.National.Accounts)] <- ""
gdpEdstats$SNA.price.valuation[is.na(gdpEdstats$SNA.price.valuation)] <- ""
gdpEdstats$Alternative.conversion.factor[is.na(gdpEdstats$Alternative.conversion.factor)] <- ""
gdpEdstats$PPP.survey.year[is.na(gdpEdstats$PPP.survey.year)] <- ""
gdpEdstats$Balance.of.Payments.Manual.in.use[is.na(gdpEdstats$Balance.of.Payments.Manual.in.use)] <- ""
gdpEdstats$External.debt.Reporting.status[is.na(gdpEdstats$External.debt.Reporting.status)] <- ""
gdpEdstats$System.of.trade[is.na(gdpEdstats$System.of.trade)] <- ""
gdpEdstats$Government.Accounting.concept[is.na(gdpEdstats$Government.Accounting.concept)] <- ""
gdpEdstats$IMF.data.dissemination.standard[is.na(gdpEdstats$IMF.data.dissemination.standard)] <- ""
gdpEdstats$Source.of.most.recent.Income.and.expenditure.data[is.na(gdpEdstats$Source.of.most.recent.Income.and.expenditure.data)] <- ""
gdpEdstats$Vital.registration.complete[is.na(gdpEdstats$Vital.registration.complete)] <- ""
gdpEdstats$Latest.agricultural.census[is.na(gdpEdstats$Latest.agricultural.census)] <- ""
gdpEdstats$Latest.industrial.data[is.na(gdpEdstats$Latest.industrial.data)] <- ""
gdpEdstats$Latest.trade.data[is.na(gdpEdstats$Latest.trade.data)] <- ""
gdpEdstats$Latest.water.withdrawal.data[is.na(gdpEdstats$Latest.water.withdrawal.data)] <- ""
gdpEdstats$X2.alpha.code[is.na(gdpEdstats$X2.alpha.code)] <- ""

#change type of column
gdpEdstats$gdp <- as.numeric(gdpEdstats$gdp)
gdpEdstats$gdp[is.na(gdpEdstats$gdp)] <- 0 #The NA values show up again as blank is not a number
gdpEdstats$CountryCode <- as.character(gdpEdstats$CountryCode)
gdpEdstats$country <- as.character(gdpEdstats$country)
gdpEdstats$Long.Name <- as.character(gdpEdstats$Long.Name)

# Deal with Aggregate regions, create a csv file of the aggregate regions and remove from working data frame
write.csv(gdpEdstats[grep(".aggregate.", gdpEdstats$Special.Notes)[-5],], file ="./Data/AggregateRegions.csv")
gdpEdstatsCl <- gdpEdstats[-grep(".aggregate.", gdpEdstats$Special.Notes)[-5],]
#Remove countries with no GDP
gdpEdstatsCl <- gdpEdstatsCl[gdpEdstatsCl$gdp > 0,]

#change column names to lower case
names(gdpEdstatsCl)[1] <- "country.code"
names(gdpEdstatsCl)[4] <- "long.name"
names(gdpEdstatsCl)[5] <- "income.group"
names(gdpEdstatsCl)[6] <- "region"
names(gdpEdstatsCl)[7] <- "lending.category"
names(gdpEdstatsCl)[8] <- "other.groups"
names(gdpEdstatsCl)[9] <- "currency.unit"
names(gdpEdstatsCl)[10] <- "latest.population.census"
names(gdpEdstatsCl)[11] <- "latest.household.survey"
names(gdpEdstatsCl)[12] <- "special.notes"
names(gdpEdstatsCl)[13] <- "national.accounts.base.year"
names(gdpEdstatsCl)[14] <- "national.accounts.reference.year"
names(gdpEdstatsCl)[15] <- "system.of.national.accounts"
names(gdpEdstatsCl)[16] <- "sna.price.valuation"
names(gdpEdstatsCl)[17] <- "alternative.conversion.factor"
names(gdpEdstatsCl)[18] <- "ppp.survey.year"
names(gdpEdstatsCl)[19] <- "balance.of.payments.manual.in.use"
names(gdpEdstatsCl)[20] <- "external.debt.reporting.status"
names(gdpEdstatsCl)[21] <- "system.of.trade"
names(gdpEdstatsCl)[22] <- "government.accounting.concept"
names(gdpEdstatsCl)[23] <- "imf.data.dissemination.standard"
names(gdpEdstatsCl)[24] <- "source.of.most.recent.income.and.expenditure.data"
names(gdpEdstatsCl)[25] <- "vital.registration.complete"
names(gdpEdstatsCl)[26] <- "latest.agricultural.census"
names(gdpEdstatsCl)[27] <- "latest.industrial.data"
names(gdpEdstatsCl)[28] <- "latest.trade.data"
names(gdpEdstatsCl)[29] <- "latest.water.withdrawal.data"
names(gdpEdstatsCl)[30] <- "country.2.code"

# Save intermediate csv after cleaning
write.csv(gdpEdstatsCl, file ="./Data/Raw/Merged01.csv")

#Count each column or show an extract for the columns
summary(gdpEdstatsCl$country.code)
summary(gdpEdstatsCl$country)
summary(gdpEdstatsCl$gdp)
summary(gdpEdstatsCl$long.name)
summary(gdpEdstatsCl$income.group)
summary(gdpEdstatsCl$region)
summary(gdpEdstatsCl$lending.category)
summary(gdpEdstatsCl$other.groups)
summary(gdpEdstatsCl$currency.unit)
summary(gdpEdstatsCl$latest.population.census)
summary(gdpEdstatsCl$latest.household.survey)
str(gdpEdstatsCl$national.accounts.base.year)
summary(gdpEdstatsCl$national.accounts.reference.year)
summary(gdpEdstatsCl$system.of.national.accounts)
summary(gdpEdstatsCl$sna.price.valuation)
summary(gdpEdstatsCl$alternative.conversion.factor)
summary(gdpEdstatsCl$ppp.survey.year)
summary(gdpEdstatsCl$balance.of.payments.manual.in.use)
summary(gdpEdstatsCl$external.debt.reporting.status)
summary(gdpEdstatsCl$system.of.trade)
summary(gdpEdstatsCl$government.accounting.concept)
summary(gdpEdstatsCl$imf.data.dissemination.standard)
summary(gdpEdstatsCl$source.of.most.recent.income.and.expenditure.data)
summary(gdpEdstatsCl$vital.registration.complete)
summary(gdpEdstatsCl$latest.agricultural.census)
summary(gdpEdstatsCl$latest.industrial.data)
summary(gdpEdstatsCl$latest.trade.data)
summary(gdpEdstatsCl$latest.water.withdrawal.data)
head(gdpEdstatsCl$country.2.code, 20)

#Questions 1
#Match the data based on the country shortcode. How many of the IDs match
matchingRecords

#Questions 2
#Sort data frame in ascending order by GDP and Country.
gdpEdstatsClSorted <- gdpEdstatsCl[order(gdpEdstatsCl$gdp, gdpEdstatsCl$country),]
gdpEdstatsClSorted[13,2]

#St. Kitts and Nevis

#Question 3.a  High income: OECD
hiOECD <- subset(gdpEdstatsClSorted$gdp, gdpEdstatsClSorted$income.group == "High income: OECD")
sum(hiOECD == 0)
#0 values are zero and will be eliminated as they were NA and not 0 and will change the mean if included
mean(hiOECD)

#Question 3.b High income: nonOECD
hiNoOECD <- subset(gdpEdstatsClSorted$gdp, gdpEdstatsClSorted$income.group == "High income: nonOECD")
sum(hiNoOECD == 0)
#14 values are zero and will be eliminated as they were NA and not 0 and will change the mean if included
mean(hiNoOECD)

#Question 4 Plot 
#Boxplot with raw gdp
bp <- ggplot(gdpEdstatsCl, aes(x=income.group, y=gdp, fill=income.group))
bp + geom_boxplot() 
#Boxplot with gdp transformed to log scale
bp <- ggplot(gdpEdstatsCl, aes(x=income.group, y=log(gdp), fill=income.group))
bp + geom_boxplot() 

#Question 5
#Quantiles by gdp 
#Quantile breaks
quantiles <- quantile(gdpEdstatsCl$gdp, probs = seq(0, 1, 0.2) )
#Tag rows according to breaks 
gdpEdstatsCl$quantile <- cut(gdpEdstatsCl$gdp, breaks=c(quantiles), labels=c("0-20","20-40","40-60","60-80","80-100"), include.lowest=TRUE)
table(gdpEdstatsCl$income.group,gdpEdstatsCl$quantile)


