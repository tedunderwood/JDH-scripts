# TrendMATCHER.R
# Identifies words that correlate with a known trend.
# We assume the trend to be matched is stored in TrendVector
# before the script begins. In practice, this vector was drawn from
# RatioPLOTTER. 

library(RMySQL)

GenreList <- c('bio', 'dra', 'fic', 'juv', 'let', 'mis', 'non', 'ora', 'poe')
GenreNames <- c('biography', 'drama', 'fiction', 'juvenile', 'letters', 'miscellanies', 'nonfiction prose', 'sermons and orations', 'poetry', 'all works')

# A list of words; its main function here is to exclude very rare words from the comparison.

WordFile <- read.table('/Users/tunderwood/Wordlists/ReMergedEtymologies.txt', stringsAsFactors=FALSE, sep = '\t', fill = TRUE, nrows = 10500, quote = '"')

Wordset <- WordFile$V1
WordsLen <- length(Wordset)

Genre <- readline('Genre? ')
StartYear <- as.integer(readline('Start year? '))
EndYear <- as.integer(readline('End year? '))
Timespan = (EndYear - StartYear) + 1

WordFreqs <- array(data = 0, dim = c(WordsLen, Timespan))

m <- dbDriver("MySQL")
con <- dbConnect(m,user="root",password="tunderwood1",dbname="joint")

	for (Y in StartYear: EndYear) {
		Year <- as.character(Y)
		YearIdx = (Y - StartYear) + 1
		querystring <- paste("select word, occur from yeargenjoint where genre=\"",Genre,"\" and year=\"", Year, "\"" ,sep="")
		sres <- dbSendQuery(con,querystring)
		seedframe <- fetch(sres,n=30000)
		dbClearResult(sres)
		Hits <- seedframe$word
		Counts <- seedframe$occur
		YearTotal <- sum(Counts)
		names(Counts) <- Hits
		Counts <- Counts[names(Counts) %in% Wordset]
		Hits <- Hits[Hits %in% Wordset]
	
		Unpacked <- integer(WordsLen)
		names(Unpacked) <- Wordset
		Unpacked[Hits] <- Counts[Hits] / YearTotal
		WordFreqs[ , YearIdx] <- Unpacked
		}

dbDisconnect(con)

# Now we identify words whose frequency in Genre correlates with the TrendVector.

Correlations <- numeric(WordsLen)

for (i in 1: WordsLen) {
	Correlations[i] <- cor(WordFreqs[i, ], TrendVector[(StartYear-1699) : (EndYear-1699)], method="pearson", use = "complete.obs")
	}
	
names(Correlations) <- Wordset
Presort <- Correlations
Correlations <- sort(Correlations)
tail(Correlations)
head(Correlations)	
	
		
	