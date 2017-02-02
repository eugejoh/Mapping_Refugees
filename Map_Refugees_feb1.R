# Refugee Data Visualization
library(ggplot2)
library(reshape2)
library(grid)
library(wordcloud)
library(extrafont)
library(scales)

# https://data.unhcr.org/dataviz/ THE GOAL IS TO REPLICATE THIS IN SHINY

# METADAT on dataset
# http://popstats.unhcr.org/en/overview
cat("\014") # clears Console (in RStudio)
sessionInfo() # gives session info, ver of R, packages
rm(list=ls()) #removes work space environment

getwd() #home directory /Users/eugenejoh
setwd("~/Documents/Post-Grad/UNHCR Data/") # ~ acts as base for home directory

list.files() # list files or folders in current working directory 
list.files(full.names=TRUE) # gives full path for files or folders - ./ shows that its a folder
# list files in the Canada directory
files.all <- list.files(path="All_Data/") #assigns name to file names in the All_Data folder
length(files.all) #checks how many objects there are in the /All_Data folder
files.all

ref.d <- read.csv(paste0("All_Data/",files.all), #insert filepath and name into 1st argument
	header=T, #select the headers in 3rd row
	skip=2, #skips the first rows (metadata in .csv file)
	#na.string=c("","-","*") #convert all blanks, "i","*" cells into missing type NA
	)
names(ref.d)

#I don't like names, so we'll specify them
new.names <- c("Year", "Country", "Country_Origin", "Refugees", "Asylum_Seekers", "Returned_Refugees", "IDPs", "Returned_IDPs", "Stateless_People", "Others_of_Concern","Total") 
names(ref.d) <- new.names

head(ref.d) #some of the values are NA but other blank ones are not?

summary(ref.d) #some have values of numbers, but also there are "*" - redacted information #http://popstats.unhcr.org/en/time_series
length(which(ref.d$Asylum_Seekers=="*")) #the counts of redacted information
length(which(ref.d$Asylum_Seekers=="")) #the counts of missing information
length(which(ref.d$Asylum_Seekers==0))
ref0<-sub("^./*$","redacted",ref.d$Refugees) #replace all the "*" with redact, but coerces all values into string
str(ref0)

# Simplify by converting all blank, missing or * labled to NA (none explicitly stated as 0) by changing na.strings() argument in read.csv()

ref.d <- read.csv(paste0("All_Data/",files.all), #insert filepath and name into 1st argument
	header=T, #select the headers in 3rd row
	skip=2, #skips the first rows (metadata in .csv file)
	na.string=c("","-","*"), #convert all blanks, "i","*" cells into missing type NA
	col.names=new.names #since we already made new names
	)

ref0<-ifelse(ref.d$Refugees=="",0,ref.d$Refugees)

ref.new<-as.data.frame(sapply(1:length(ref.d),function(y,x) ifelse(ref.d[,x]=="",0,ref.d[,x]), y=ref.d)) #sapply() and apply functions generally are good with all the values in the dataframe are of the same class/data type. Since we had strings as factors, numeric and integers - this is probably not the best method

dim(ref.d) #dimensions of dataset
names(ref.d)

names(ref.d) <- new.names #abbreviated headers for columns
head(ref.d$Asylum_Seekers)
names(ref.d) #check the names again

##########################
# Investigate Year Range #
##########################

summary(ref.d$Year) #
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   # 1951    2000    2006    2004    2010    2014 

table(ref.d$Year) #we see there is alot of missing data
# 1951 1952 1953 1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 
  # 22   18   18   21   17   17   17   19   19   20   26   27   30   42   49   56 
# 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 
  # 54   71   87  106  104  121   90  105  109  104  127  151  162  208  218  262 
# 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 
 # 266  282  316  293  289  394  470  589  663  802 1518 1776 1924 2209 2561 3013 
# 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 
# 3058 4100 4323 4542 4783 4908 5051 5233 5500 5558 5579 5902 5987 6160 6508 6692

ref.na<-apply(ref.d,2, function(x) sum(is.na(x))) #counts the number of NAs in each column
ref.na #counts of the NAs, row total is 103746

# https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Classes
str(ref.d) #structure of the columns

# Renaming the names in of certain countries for brevity
# http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/
# library(plyr) #load the plyr package
# levels(ref.d$Country)<-revalue(ref.d$Country, 
	# c("Bolivia (Plurinational State of)"="Bolivia",
	# "China, Hong Kong SAR"="Hong Kong",
	# "China, Macao SAR"="Macao",
	# "Iran (Islamic Rep. of)"="Iran",
	# "Micronesia (Federated States of)"="Micronesia",
	# "Serbia and Kosovo (S/RES/1244 (1999))"="Serbia & Kosovo",
	# "Venezuela (Bolivarian Republic of)"="Venezuela",
	# "Various/Unknown"="Unknown")
	# )

# levels(ref.d$Country_Origin)<-revalue(ref.d$Country_Origin,
		# c("Bolivia (Plurinational State of)"="Bolivia",
	# "China, Hong Kong SAR"="Hong Kong",
	# "China, Macao SAR"="Macao",
	# "Iran (Islamic Rep. of)"="Iran",
	# "Micronesia (Federated States of)"="Micronesia",
	# "Serbia and Kosovo (S/RES/1244 (1999))"="Serbia & Kosovo",
	# "Venezuela (Bolivarian Republic of)"="Venezuela",
	# "Various/Unknown"="Unknown")
	# )
# levels(ref.d$Country)
# levels(ref.d$Country_Origin)
# str(ref.d)

# two factors nad the remainder are integers
for(i in 2:length(names(ref.d))){ # "2"-ignores the first column (we want to keep Year as an integer)
	if (class(ref.d[,i])=="factor"){
		ref.d[,i] <- as.character(ref.d[,i]) #converts all columns with factor type to string
	}
	if (class(ref.d[,i])=="integer"){
		ref.d[,i] <- as.numeric(ref.d[,i]) #converts all columns with integer to numeric
	}}

# list of the country names I want to change
old.countries <- c("Bolivia (Plurinational State of)",
	"China, Hong Kong SAR",
	"China, Macao SAR",
	"Iran (Islamic Rep. of)",
	"Micronesia (Federated States of)",
	"Serbia and Kosovo (S/RES/1244 (1999))",
	"Venezuela (Bolivarian Republic of)",
	"Various/Unknown")

# replacement names
new.countries <- c("Bolivia","Hong Kong","Macao","Iran","Micronesia","Serbia & Kosovo","Venezuela","Unknown")

for (k in 1:length(old.countries)){ 
	ref.d$Country_Origin[ref.d$Country_Origin==old.countries[k]]<-new.countries[k]
	ref.d$Country[ref.d$Country==old.countries[k]]<-new.countries[k]
}

table(ref.d$Country)
table(ref.d$Country_Origin)

# > table(ref.d$Country)

               # Australia                  Austria                  Belgium 
                     # 102                     4046                     3989 
                 # Burundi                   Canada   Dem. Rep. of the Congo 
                     # 164                     4469                      952 
                 # Denmark                   France                  Germany 
                    # 5710                     6500                     3924 
                  # Greece                Hong Kong                    Italy 
                    # 5175                      255                     5411 
              # Luxembourg                  Morocco              Netherlands 
                    # 6955                     8303                     7052 
                  # Norway                    Spain                   Sweden 
                    # 4882                     2170                     5172 
             # Switzerland                  Tunisia                   Turkey 
                    # 2656                     1123                     6932 
          # United Kingdom United States of America                  Unknown 
                    # 5574                     8538                     3692 

table(ref.d$Country) #check the above changes
table(ref.d$Country_Origin) #check the above changes

which(ref.d$Country=="Bolivia (Plurinational State of)") #just to check again
which(ref.d$Country=="Bolivia") #just to check again


# Creating a list of the countries in Country
clist<-sort(unique(ref.d$Country)) #alphabetical
clist

# Creating a list of the countries in Country_Origin
or.clist<-sort(unique(ref.d$Country_Origin)) #alphabetical
or.clist

# We want to if there are any differences between these two vectors of country names
# http://stackoverflow.com/questions/9162134/comparing-character-vectors-in-r-to-find-unique-and-or-missing-values
clist[!clist %in% or.clist] #which countries from clist are not in or.clist
 # this means that these countries either don't have data for refugees or there are no refugees in these countries
 # this means that these countries have not produced refugees or there is no data on refugees from these countries
setdiff(clist,or.clist) #also accomplishes the same as line 81

or.clist[!or.clist %in% clist] #which countries from or.clist are not in clist
 # this means that these are ONLY countries of origin and either there is no data that refugees seltted or there are no refugees in these countries
 # this means that these countries (ie. NK) have only produced refugees (not taken in any refugees) or there is no data on this.
setdiff(or.clist,clist) #also accomplishes the same as line 86

ref.d[which(ref.d$Country=="Bonaire"),] #retrieves rows that match the criteria: country Bonaire
NK<-ref.d[which(ref.d$Country_Origin=="Dem. People's Rep. of Korea"),] #retrieves the rows that match the criteria: country of origin as NK

apply(ref.d[1:10,1:10],2,function(x) paste0(x," new words")) #adding asdf text to the selection rows

# Make a wordcloud to qualitatively visualize... which countries have the most reports, that have taken in refugees
library(wordcloud) #https://www.r-bloggers.com/word-cloud-in-r/
countries<-sort(unique(ref.d$Country),decreasing=F)
countries

country.counts <- as.data.frame(table(ref.d$Country))
pal2 <- brewer.pal(10,"Accent") #palette
wordcloud(countries,
	country.counts[,2],
	scale=c(2.5,.2),
	min.freq=10,
	vfont=c("serif","bold"),
	random.order=FALSE,
	colors=pal2) # selection of font #ref: https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/Hershey.html
#http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html

# Make a worldcloud to ... refugee country of origin
or.countries<-sort(unique(ref.d$Country_Origin),decreasing=F)
or.countries

or.country.counts <- as.data.frame(table(ref.d$Country_Origin))
wordcloud(or.countries,
	or.country.counts[,2],
	scale=c(2.5,.2),
	min.freq=10,
	vfont=c("serif","bold"),
	random.order=FALSE,
	colors=pal2)

# Sum the Totals by Year, Country, Country of Origin
names(ref.d) #http://stackoverflow.com/questions/15933968/r-programming-sum-elements-of-rows-with-common-values?rq=1
?aggregate

# YEAR
year.tot <- aggregate(cbind(Total)~Year,data=ref.d,FUN=sum)
year.tot

# COUNTRY
count.tot <- aggregate(cbind(Total)~Country,data=ref.d,FUN=sum)
count.tot

# COUNTRY OF ORIGIN
or.count.tot <- aggregate(cbind(Total)~Country_Origin,data=ref.d,FUN=sum)
or.count.tot

# NK
NK.tot<- aggregate(cbind(Total)~Country,data=NK,FUN=sum)
NK.tot

NK.tot[order(-NK.tot[,2]),][1:10,]

head(sort(NK.tot[,1],decreasing=TRUE), n = 50)
sort(NK.count.tot)

aggregate(cbind(Refugees)~Year+Country,data=ref.d,FUN=sum) # sums of "Refugee" by country and year

# REFUGEE vs. IDP: the difference between them is that “a refugee has crossed an international border and has sought refuge in a country other than his own, whereas an IDP is trying to find safety and refuge within his country,

##############
# WORDCLOUDS #
##############

library(extrafont)
loadfonts()

# Set the Palette
pal3 <- c("#274c56", #http://tools.medialab.sciences-po.fr/iwanthue/
"#664c47",
"#4e5c48",
"#595668",
"#395e68",
"#516964",
"#6b6454",
"#58737f",
"#846b6b",
"#807288",
"#758997",
"#7e9283",
"#a79486",
"#aa95a2",
"#8ba7b4")

# YEAR
plot(year.tot) #basic R plot, it's ugly
wordcloud(year.tot[,1], #list of words
	year.tot[,2], #frequencies for words
	scale=c(5,.5), #scale of size range 
	min.freq=100, #minimum frequency
	family="Garamond", font=2, #text edit (The font "face" (1=plain, 2=bold, 3=italic, 4=bold-italic))
	# https://www.stat.auckland.ac.nz/~paul/R/fonts.html
	random.order=F, #F-plotted in decreasing frequency 
	colors=rev(pal3)) #colours from least to most frequent

# COUNTRY OF ORIGIN
png("wordcloud_count_or.png",width=600,height=850,res=200)
wordcloud(or.count.tot[,1], #list of words
	or.count.tot[,2], #frequencies for words
	scale=c(3,.5), #scale of size range 
	min.freq=100, #minimum frequency
	max.words=100, #maximum number of words show (others dropped)
	family="Garamond", font=2, #text edit (The font "face" (1=plain, 2=bold, 3=italic, 4=bold-italic))
	# https://www.stat.auckland.ac.nz/~paul/R/fonts.html
	random.order=F, #F-plotted in decreasing frequency 
	colors=rev(pal3)) #colours from least to most frequent
dev.off()

# question, Afghanistan looks to be the biggest
sum(ref.d[which(ref.d$Country=="Afghanistan"),11],na.rm=T)

# COUNTRY
wordcloud(count.tot[,1], #list of words
	count.tot[,2], #frequencies for words
	scale=c(3,.5), #scale of size range 
	min.freq=100, #minimum frequency
	max.words=100, #maximum number of words show (others dropped)
	family="Garamond", font=2, #text edit (The font "face" (1=plain, 2=bold, 3=italic, 4=bold-italic))
	# https://www.stat.auckland.ac.nz/~paul/R/fonts.html
	random.order=F, #F-plotted in decreasing frequency 
	colors=rev(pal3)) #colours from least to most frequent

# PLOTS
# http://stackoverflow.com/questions/21236229/stacked-bar-chart

# 1st prepare data in LONG form to use in ggplot2 http://stackoverflow.com/questions/21236229/stacked-bar-chart
#install.packages("reshape2")

names(ref.d)
# https://www.r-bloggers.com/reshape-and-aggregate-data-with-the-r-package-reshape2/
# going to use melt() to assign long form for ggplot2 so that the fill() in aes() can be set to the levels in variable
no.count<-(ref.d[c(-2,-3,-11)]) #removes Country, Country of Origin and Total
lno.count<-melt(no.count,id=c("Year"))
head(lno.count)


lref.d<-melt(ref.d, id.vars=new.names[c(-1,-2,-3,-11)]) #removal of variables that aren't the people groups
head(lref.d)
names(lref.d)
table(lref.d$variable)
table(lref.d$value)

cow <- melt(ref.d,id=c("Year"));head(cow);dim(cow);tail(cow);table(cow$variable)

# 1- Year, 2- Country, 3- Country of Origin, 11-Total
names(ref.d)

lref.d<-melt(ref.d,id.vars=c("Country","Country_Origin","Year")) # variable set to Country
head(lref.d)
table(lref.d$variable)
table(lref.d$value)

l.count<-melt(ref.d[,-c(3,11)],id.vars=c("Country","Year")) #only looking at countries
head(l.count)
table(l.count$variable)

l.or.count<-melt(ref.d[,-c(2,11)],id.vars=c("Country_Origin","Year")) #
head(l.count)
head(lref.d)
head(lref.d)
tail(lref.d)

table(lref.d$variable)
head(l.count)

# Making the Stacked Bar Plot
help.search("geom_", package = "ggplot2")

# choosing a theme
blank_t <- theme_minimal()+
  theme(
  panel.border = element_blank(), #removes border
  panel.background = element_rect(fill = "#d0d1cf",colour=NA),
  panel.grid = element_line(colour = "#ffffff"),
  plot.title=element_text(size=20, face="bold",hjust=0,vjust=2), #set the plot title
  plot.subtitle=element_text(size=15,face=c("bold","italic"),hjust=0.01), #set subtitle
  legend.title=element_text(size=10,face="bold",vjust=0.5), #specify legend title
  axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,vjust=1,margin=margin(r=10)),
  axis.text.y = element_text(size=10,margin = margin(r=5)),
  legend.background = element_rect(fill="gray90", size=.5),
  legend.position=c(0.15,0.65),
  plot.margin=unit(c(t=1,r=1.2,b=1.2,l=1),"cm")
  )
unique(lno.count$variable)
clpal <- c("#bf4d39", #Refugees 7 TOTAL
"#ad8744", #Asylum Seekers
"#70b145", #Returned Refugees
"#549071", #IDPs
"#7077b2", #Returned IDPs
"#9349c1", #Stateless People
"#b05079") #Others of Concern

pal4 <- c("#AA4B39", #http://paletton.com/#uid=c063r133U0koArBf3BZk9ums2m+vuic
	"#916702",
	"#F2855A",
	"#256C8B",
	"#0D4D4D",
	"#2D882D",
	"#F2C55A"
	)

pal5 <- c("#B53C26", #Refugees 7 TOTAL
"#79542D", #Asylum Seekers
"#DA634D", #Returned Refugees
"#0B4659", #IDPs
"#4B8699", #Returned IDPs
"#D38F47", #Stateless People
"#09692A") #Others of Concern

# stacked barplot of refugee types by year
gg1 <-ggplot(lno.count,aes(Year,value)) + 
	geom_bar(aes(fill=variable),stat="identity") +
	labs(title="UNHCR Population Statistics Database",
	subtitle="(1951 - 2014)",
	x="Year",y="Number of People (Millions)") + blank_t +
	scale_fill_manual(guide_legend(title="Populations of Concern"),labels=gsub("*_"," ",names(ref.d)[c(-1,-2,-3,-11)]),values=pal5)

mil.func <- function(x) {x/1000000 }


require(scales)
gg2<-gg1+
	scale_y_continuous(limits=c(0,6e7), 
	breaks=pretty(0:6e7,n=5), 
	labels=mil.func, 
	expand=c(0.025,0)) +
	
	scale_x_continuous(limits=c(1950,2015), 
	breaks=seq(1950,2015,by=5), 
	expand=c(0.01,0))
gg2

ggsave(plot=gg2,filename="UNHCR_Totals_Yr.png", width=9.5, height=5.5, dpi=200)
dev.off()

ggplot(ref.d,aes(Year,Total)) + geom_bar(aes(fill=Country),stat="identity") #not helpful, too many layers - maybe specify by regions?


# POPULATION DATA FROM UN
# https://esa.un.org/unpd/wpp/Download/Standard/ASCII/
files.pop <- list.files(path="Population/",pattern="\\.csv$")
files.pop
file.size(paste0("Population/",files.pop))/1000000 #size of the file in MB

WPP <- read.csv(paste0("Population/",files.pop)[1], #first file chosen with [1]
	header=T, #first line is a header
	)
head(WPP);tail(WPP)
levels(WPP$Location) #list of countries, regions, areas, and other categorizations
sum(is.na(WPP)) # missing values

sum(WPP$PopTotal[WPP$Time==1951]) #164147341e3
summary(WPP$PopTotal)
summary(world.pop$PopTotal)
world.pop<-WPP[WPP$Location=="World",]
names(world.pop)
str(world.pop)
world.pop<-world.pop[,c(2,5,9,10,11)] #keep the following vars: Location, Time, PopTotal, GrowthRate, PopDensity
world.pop[,3]<-world.pop[,3]*1000
sum(WPP$PopTotal[WPP$Time==1951])
head(world.pop)

sum(world.pop$PopTotal[world.pop$Time==1951])

summary(world.pop$Time)
summary(world.pop$MidPeriod)

wp.year <- aggregate(cbind(Value)~Time,data=world.pop,FUN=sum) #not needed for aggregation due to "world" level in Location variable
names(wp.year)
# I want to get the range from 1951 to 2014, to match the UNHCR Data
which(year.pop$Time==1951)
which(year.pop$Time==2014)

UNHCR.pop <- year.pop[c(which(year.pop$Time==1951):which(year.pop$Time==2014)),]

# USE API TO DOWNLOAD DATA
# http://data.unhcr.org/wiki/index.php/Information_Sharing_Portal.html#Codes_management


#### http://www.unhcr.org/en-us/news/latest/2016/6/5763b65a4/global-forced-displacement-hits-record-high.html

# Comment on how the levels of refugges are now at a all time high