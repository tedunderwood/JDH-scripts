ReadMe.txt

A set of three R scripts that I used to produce most of the visualizations in "The Emergence of Literary Diction."

GenreGRAPHER.R uses RMySQL to extract yearly word frequencies from an underlying MySQL database divided by year and genre. It divides words into pre- and post-1150 sets, and graphs aggregate frequencies.

RatioPLOTTER.R then uses the arrays PreCt and PostCt produced by GenreGRAPHER to generate a pre/post-1150 ratio and prepare it for visualization.

Finally, TrendMATCHER.R can find individual words that correlate closely with a given vector of ratio values from RatioPLOTTER.