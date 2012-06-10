# The Genre Grapher
#
# Accepts a set of words and graphs their frequency in different genres over time.
# I didn't use this to generate visualizations in the JDH article, but it did
# mine data that was then fed into RatioPLOTTER.

library(RMySQL)

GenreList <- c('bio', 'dra', 'fic', 'juv', 'let', 'mis', 'non', 'ora', 'poe')
GenreNames <- c('biography', 'drama', 'fiction', 'juvenile', 'letters', 'miscellanies', 'nonfiction prose', 'sermons and orations', 'poetry', 'all works')
Timespan <- 200

DocData <- read.table("/Users/tunderwood/METADATA/DocMetadata.txt", stringsAsFactors=FALSE, sep = '\t', fill = TRUE, nrows = 6000, quote = '"')
Years <- as.integer(DocData$V4)
Genres <- DocData$V5
Tokens <- as.integer(DocData$V7)

WordFile <- read.table('/Users/tunderwood/Wordlists/ReMergedEtymologies.txt', stringsAsFactors=FALSE, sep = '\t', fill = TRUE, nrows = 10500, quote = '"')

Words <- WordFile$V1
Dates <- WordFile$V2

remove(WordFile)

Pre1150 <- Words[Dates < 1150]
Post100 <- Words[Dates > 100]
Pre1150 <- Words[Words %in% Pre1150 & Words %in% Post100]
Stopwords <- Words[Dates == 1]
TailEnd <- Pre1150[500: length(Pre1150)]

Post1150 <- Words[Dates > 1149 & Dates < 1700]
cat(length(Pre1150),length(Post1150), sep = " ")

Pre1150 <- tolower(Pre1150)
Post1150 <- tolower(Post1150)

m <- dbDriver("MySQL")
con <- dbConnect(m,user="root",password="tunderwood1",dbname="joint")

PreCt <- array(data = 0, dim = c(10, Timespan))
PostCt <- array(data = 0, dim = c(10, Timespan))
AllCt <- array(data = 0, dim = c(10, Timespan))

for (GenreCt in 1: 9) {
	Gen <- GenreList[GenreCt]
	cat(Gen, '\n')
	for (i in 1: Timespan) {
		querystring <- paste("select word, occur from yeargenjoint where year =", 1699 + i, " and genre = \"", Gen, "\"",sep="")
		sres <- dbSendQuery(con,querystring)
		seedframe <- fetch(sres,n=30000)
		dbClearResult(sres)
		WordRes <- seedframe$word
		Counts <- as.integer(seedframe$occur)
		names(Counts) <- WordRes
		PreOcc <- Counts[names(Counts) %in% Pre1150]
		PostOcc <- Counts[names(Counts) %in% Post1150]
		PreCt[GenreCt, i] <- sum(PreOcc)
		PostCt[GenreCt, i] <- sum(PostOcc)
		IndexVector <- which(Genres == Gen & Years == (1699 + i))
		AllCt[GenreCt, i] <- sum(Tokens[IndexVector])
		}
	}

dbDisconnect(con)

# The tenth row holds the sum of all genres.

for (i in 1:Timespan) {
	PreCt[10, i] <- sum(PreCt[ , i])
	PostCt[10, i] <- sum(PostCt[ , i])
	AllCt[10, i] <- sum(AllCt[ , i])
	}

GenreList <- c(GenreList, 'all')

# That allows you to compare all by saying all.

while (TRUE == TRUE) {
	PrePost <- readline(prompt = "pre or post? ")
	commastring <- readline(prompt = "Genres to compare, separated by commas: ")
	seedvector <- unlist(strsplit(commastring,split=","))
	howmany <- length(seedvector)
	ToSum <- integer(howmany)
	for (i in 1: howmany) {
		ToSum[i] <- which(GenreList == seedvector[i])
	}
	
	Hits <- integer(Timespan)
	Totals <- rep(.01, Timespan)
	
	for (i in 1: howmany){
		Totals <- Totals + AllCt[ToSum[i], ]
		if (PrePost == 'pre') Hits <- Hits + PreCt[ToSum[i], ]
		else Hits <- Hits + PostCt[ToSum[i], ]
		}
	
	if (PrePost == "pre") Legend = "Pre-1150 words in: "
	else Legend = "Post-1149 words in: "
	
	for (i in 1: howmany){
		Legend <- paste(Legend, GenreNames[ToSum[i]], sep = "")
		if (i < howmany) Legend <- paste(Legend, ", ", sep = "")
		}
		
	PlotFreq <- Hits / Totals

	PlotYears <- which(PlotFreq > .0001)
	PlotYears <- PlotYears + 1699
	PlotVals <- PlotFreq[PlotFreq > .0001] * 1000

	scatter.smooth(PlotYears,PlotVals, span = 0.2, col='brown3',xlab= Legend ,ylab="aggregate freq per thousand")
	
	User <- readline(prompt="Continue? ")
	if (User == 'n') break
}


#for (j in 1:150){
#	stpoint = j-5
#	if (stpoint<1) stpoint=1
#	endpoint = j+5
#	if (endpoint>150) endpoint=150
#	FicSmooth[j] = mean(FicFreq[stpoint:endpoint])
#	NonSmooth[j] = mean(NonFreq[stpoint:endpoint])
#	}
#
#Year <- c(1:150, 1:150)
#Genre <- c(rep('fiction', 150), rep('nonfiction', 150))
#Freq <- c(FicSmooth, NonSmooth)
#
#PlotFrame <- data.frame(Year = Year, Freq = Freq, Genre = Genre)
#chromatic <- c("plum3", "olivedrab", "darkorchid3", "olivedrab3", "rosybrown4", "darkgoldenrod", "lightsteelblue4","sienna3")
#
#p <- ggplot(PlotFrame, aes(x = Year, y = Freq, group = Genre, colour = Genre, size = Genre))
#
#p + geom_line() + scale_colour_manual(values = chromatic) + scale_size_manual(values = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8)) + scale_x_continuous(" ") + scale_y_continuous(" ")