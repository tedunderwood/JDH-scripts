# RatioPLOTTER

# This script borrows arrays PreCt and PostCt that are
# produced by GenreGRAPHER, then converts them into 
# ratios and organizes the ratio data into a data
# frame that can be sent to ggplot2 for visualization.

library(ggplot2)

GenreList <- c('bio', 'dra', 'fic', 'juv', 'let', 'mis', 'non', 'ora', 'poe')
Ratios <- array(data = 0, dim = c(5,200))

Ratios[1, ] <- PreCt[10, ] / PostCt[10, ]
# Contains all results

nonpre <- numeric(200)
nonpost <- numeric(200)
nonfiction <- c(1, 5, 7, 8)
for (i in 1:4) {
	genre = nonfiction[i]
	nonpre = nonpre + PreCt[genre, ]
	nonpost = nonpost + PostCt[genre, ]
	}
# nonfiction
Ratios[5, ] <- nonpre / nonpost
# poetry
Ratios[2, ] <- PreCt[9, ] / PostCt[9, ]
#drama
Ratios[3, ] <- PreCt[2, ] / PostCt[2, ]
#fiction
Ratios[4, ] <- PreCt[3, ] / PostCt[3, ]

PlotArray <- array(data = 0, dim = c(3,200))
PlotArray[1, ] <- Ratios[2, ]
#PlotArray[2, ] <- Ratios[3, ]
PlotArray[2, ] <- Ratios[4, ]
PlotArray[3, ] <- Ratios[5, ]

LabelVector <- c('Poetry', 'Prose fiction', 'Nonfiction')

Dimens <- dim(PlotArray)
Threads = Dimens[1]
Vals = Dimens[2]

Year <- integer(0)
Frequency <- numeric(0)
Ngram = character(0)

for (i in 1: Threads){
	Genre <- LabelVector[i]
	for (j in 1:Vals){
		if (is.nan(PlotArray[i, j])) next
		Year <- c(Year, j + 1699)
		Frequency <- c(Frequency, PlotArray[i, j])
		Ngram = c(Ngram, Genre)
		}
}


PlotFrame <- data.frame(Year = Year, Freq = Frequency, Genre = Ngram)
PlotFrame$Genre = factor(PlotFrame$Genre, levels = LabelVector)

chromatic <- c("maroon4", "gray10", "olivedrab4", "dodgerblue3")

p <- ggplot(PlotFrame, aes(x = Year, y = Freq, group = Genre, shape = Genre, colour = Genre, size = Genre))

p + geom_line() + scale_colour_manual(values = chromatic) + scale_size_manual(values = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8)) + scale_x_continuous(" ") + scale_y_continuous(" ")
