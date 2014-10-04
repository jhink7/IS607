install_github("hadley/rvest")

library(rvest)

#### Primary Question ####
feynmann_d_wiki <- html("http://en.wikipedia.org/wiki/Feynman_diagram")

# Gets all images on the page (specifically their src attribute)
allimages <- feynmann_d_wiki %>%
  html_nodes("img") %>%
  html_attr("src")

# Gets the first example of a Feynmann diagram on the page.  The classic example.
exampleimage <- allimages[1]
exampleimage

#### Optional Exercise ####
# list all available demos
demo(package="rvest") 
# lists code for tripadvisor demo; follow instructions 
# in your RStudio console window.
demo("tripadvisor", "rvest") 

#### Advanced Optional Exercise ####
#I've used the older webpage scraping techniques in R before to scrape NHL data.  To be honest, they're nowhere near as robust as those offered on other
# platforms (Python - Beautiful Soup, .Net - HtmlAgility).  Though this is the first time I've used rvest, it did prove convenient and the syntax
# makes use intuitive.  I will consider it going forward, especially if I need to do subsequent analysis in R.