install.packages("XML")
require(XML)
theURL <- "http://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool/"
bowlPool <- readHTMLTable(theURL, which = 1, header = FALSE, stringsAsFactors = FALSE)
bowlPool

#### Q1 ####

#Answer: bowlpool is a dataframe (data.frame)

#### Q2 ####

theURL <- "http://www.w3schools.com/html/html_tables.asp"
hvalues <- readHTMLTable(theURL)

# Answer: hvalues is returned as a list in this case.

#### Q3 ####
require(dplyr)
tablesOnly <- Filter(Negate(is.null), hvalues)
numberTables <- length(tablesOnly)

#Answer:Code above yields 2

#### Q4 ####
theURL <- "http://www.w3schools.com/html/html_tables.asp"
table <- readHTMLTable(theURL, which = 1, header = TRUE, stringsAsFactors = FALSE)

#### Q5 ####
table <- table[c("Last Name", "Points")]

#### Q6 ####

# Answer:  http://www.danglefactory.com/projections/skaters

#### Q7 ####
theURL <- "http://www.danglefactory.com/projections/skaters"
hvalues <- readHTMLTable(theURL)
tablesOnly <- Filter(Negate(is.null), hvalues)
numberTables <- length(tablesOnly)

# Answer: 1 table.

#### Q8 ####
# Answer: My primary browser is Google Chrome.  To view the page source, I generally press F12 to open up the dev tools and then click on the elements tab.  
# Alternatively, right clicking on an html element on the main page and selecting "inspect element" will take you to the object in the page source.  
