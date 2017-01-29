# Refugee Data Visualization
# 
# https://data.unhcr.org/dataviz/ THE GOAL IS TO REPLICATE THIS IN SHINY

# METADAT on dataset
# http://popstats.unhcr.org/en/overview
rm(list=ls())

getwd() #home directory /Users/eugenejoh
setwd("~/Documents/Post-Grad/UNHCR Data/") # ~ acts as base for home directory

list.files() # list files or folders in current working directory 
list.files(full.names=TRUE) # gives full path for files or folders - ./ shows that its a folder
# list files in the Canada directory
files.all <- list.files(path="All_Data/") #assigns name to file names in the Canada folder
length(files.all) #checks how many objects there are in the /Canada folder
files.all

ref.d <- read.csv(paste0("Canada/",files.all), #insert filepath and name into 1st argument
	header=T, #select the headers in 3rd row
	skip=2, #skips the first rows (metadata in .csv file)
	na.string=c("","-","*")) #convert all blanks, "i","*" cells into missing type NA

dim(ref.d) #dimensions of dataset
names(ref.d) #I don't like names, so we'll specify them

names(ref.d) <- c("Year", "Country", "Country_Origin", "Refugees", "Asylum_Seekers", "Returned_Refugees", "IDPs", "Returned_IDPs", "Stateless_People", "Others_of_Concern","Total") #abbreviated headers for columns

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
library(plyr) #load the plyr package
levels(ref.d$Country)<-revalue(ref.d$Country, 
	c("Bolivia (Plurinational State of)"="Bolivia",
	"China, Hong Kong SAR"="Hong Kong",
	"China, Macao SAR"="Macao",
	"Iran (Islamic Rep. of)"="Iran",
	"Micronesia (Federated States of)"="Micronesia",
	"Serbia and Kosovo (S/RES/1244 (1999))"="Serbia & Kosovo",
	"Venezuela (Bolivarian Republic of)"="Venezuela",
	"Various/Unknown"="Unknown")
	)

levels(ref.d$Country_Origin)<-revalue(ref.d$Country_Origin,
		c("Bolivia (Plurinational State of)"="Bolivia",
	"China, Hong Kong SAR"="Hong Kong",
	"China, Macao SAR"="Macao",
	"Iran (Islamic Rep. of)"="Iran",
	"Micronesia (Federated States of)"="Micronesia",
	"Serbia and Kosovo (S/RES/1244 (1999))"="Serbia & Kosovo",
	"Venezuela (Bolivarian Republic of)"="Venezuela",
	"Various/Unknown"="Unknown")
	)
levels(ref.d$Country)
levels(ref.d$Country_Origin)
str(ref.d)

## ^ above replaces what was done before but erorrs occur when I convert from factor to string type
# ref.d$Country_Origin[ref.d$Country_Origin=="Various/Unknown"] <- "Unknown" #change the "Various/Unknown" to a new string label "UNKNOWN" in the country of origin column
# ref.d$Country[ref.d$Country=="Various/Unknown"] <- "Unknown" #do the same for the countries

# two factors nad the remainder are integers
summary(ref.d$Year);class(ref.d$Year);range(ref.d$Year)
ref.d$Country <- as.character(ref.d$Country) #convert from factor to string
ref.d$Country_Origin <-as.character(ref.d$Country_Origin) #convert from factor to string

for(i in 2:length(names(ref.d))){ # "2"-ignores the first column (we want to keep Year as an integer)
	if (class(ref.d[,i])=="factor"){
		ref.d[,i] <- as.character(ref.d[,i])	 #converts all columns with factor type to string
	}
	if (class(ref.d[,i])=="integer"){
		ref.d[,i] <- as.numeric(ref.d[,i]) #converts all columns with integer to numeric
	}
}

ref.d$Country_Origin[ref.d$Country_Origin=="Various/Unknown"] <- "Unknown" #change the "Various/Unknown" to a new string label "UNKNOWN" in the country of origin column
ref.d$Country[ref.d$Country=="Various/Unknown"] <- "Unknown" #do the same for the countries


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

clist<-sort(unique(ref.d$Country)) #alphabetical list of countries in Country
which(ref.d$Country=="Bolivia (Plurinational State of)")

ref.d[ref.d$Country=="Bolivia (Plurinational State of)",]

or.clist<-sort(unique(ref.d$Country_Origin)) #alphabetical list of countries in Country_Origin
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
head(ref.d[which(ref.d$Country_Origin=="Dem. People's Rep. of Korea"),]) #retrievs the rows that match the criteria: country of origin as NK

apply(ref.d[1:10,1:10],2,function(x) paste0(x,"asdf")) #adding asdf text to the selection rows

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

?aggregate
# Sum the Totals by Year, Country, Country of Origin
names(ref.d) #http://stackoverflow.com/questions/15933968/r-programming-sum-elements-of-rows-with-common-values?rq=1
year.tot<-aggregate(cbind(Total)~Year,data=ref.d,FUN=sum)
or.country.tot<-aggregate(cbind(Total)~Country_Origin,data=ref.d,FUN=sum)

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

# Install more Fonts
install.packages("extrafont")
library(extrafont)
loadfonts()
wordcloud(or.country.tot[,1],
	or.country.tot[,2],
	scale=c(3,.5),
	min.freq=100,
	family="Garamond", font=2,
	random.order=F,
	colors=rev(pal3))