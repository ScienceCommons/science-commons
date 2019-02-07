require(scholar)
## Define the id for Richard Feynman
lorne.campell.gs.id <- 'E80DLosAAAAJ'
julia.rohrer.gs.id <- 'Bg2qoJEAAAAJ'
daniel.lakens.gs.id <- 'ZbqYyrsAAAAJ'

## Get his publications (a large data frame)
lorne.campbell.publications <- get_publications(lorne.campell.gs.id)
write.csv(lorne.campbell.publications, fileEncoding="UTF-8", file="C:/Users/Etienne LeBel/Google Drive/Curate Science/website/science-commons-alpha/author-article-list-meta-data/lorne.campbell.GS.publications.csv")

julia.rohrer.publications <- get_publications(julia.rohrer.gs.id)
write.csv(julia.rohrer.publications, fileEncoding="UTF-8", file="C:/Users/Etienne LeBel/Google Drive/Curate Science/website/science-commons-alpha/author-article-list-meta-data/julia.rohrer.GS.publications.csv")


daniel.lakens.publications <- get_publications(daniel.lakens.gs.id)
write.csv(daniel.lakens.publications, fileEncoding="UTF-8", file="C:/Users/Etienne LeBel/Google Drive/Curate Science/website/science-commons-alpha/author-article-list-meta-data/daniel.lakens.GS.publications.csv")









## Get his profile
l <- get_profile(id)

## Print his name and affliation
l$name



## Get his citation history, i.e. citations to his work in a given year
ct <- get_citation_history(id)

## Plot citation trend
library(ggplot2)
ggplot(ct, aes(year, cites)) + geom_line() + geom_point()

## Get article citation history
ach <- get_article_cite_history(id, p$pubid[1])

## Plot citation trend
ggplot(ach, aes(year, cites)) +
  geom_segment(aes(xend = year, yend = 0), size=1, color='darkgrey') +
  geom_point(size=3, color='firebrick')

orcid_auth()
AlnZg8
browse(as.orcid("0000-0002-1642-628X"))
out <- works(orcid_id("0000-0001-7377-008X"))

install.packages(googlesheets)
library(googlesheets)
gs_auth(new_user = TRUE)
gs_ls() 
gs_new(title = "mtcars", ws_title = "first_sheet", input = p)


dumfxn <- function() {
  return("test")
}
stringddd <- ""
for (i in 1:3) {
  stringddd = paste0(stringddd , "<div>fucktard</div>", as.character(i), dumfxn())
}

DOI222 <- "1234"



f_denom(c(12345, 12563, 919, 4), relative=1, pad.char = '' ) #mix.denom=TRUE, then the decimal goes away
f_denom(1002, relative=1, pad.char = '') #mix.denom=TRUE, then the decimal goes away




f_denom(c(1234365, 122123563, 12913919), prefix = '$')
f_denom(c(12343676215, 122126763563, 1291673919), prefix = '$')
f_denom(c(NA, 2, 12343676215, 122126763563, 1291673919), prefix = '$')
f_denom(c(NA, 2, 123436, 122126763, 1291673919), prefix = '$', mix.denom = TRUE)
f_denom(c(NA, 2, 12343676215, 122126763563, 1291673919), prefix = '$', pad.char = '')
f_denom(c(NA, 2, 12343676215, 122126763563, 1291673919), relative = 1, prefix = '$')
f_denom(c(NA, 2, 12343676215, 122126763563, 1291673919), relative = 9, prefix = '$')
f_denom(c(NA, 2, 12343676215, 122126763563, 1291673919), less.than.replace = TRUE)




article.title = article.table[i,"article.title"],
journal.name = article.table[i,"journal.name"],
DOI = article.table[i,"DOI"],
article.PDF.URL = article.table[i,"article.PDF.URL"],
article.preprint.URL = article.table[i,"article.preprint.URL"],
article.HTML.URL = article.table[i,"article.HTML.URL"],
open.mat.URL = article.table[i,"open.mat.URL"],
prereg.URL = article.table[i,"prereg.URL"],
open.data.URL = article.table[i,"open.data.URL"],
open.code.URL = article.table[i,"open.code.URL"],
code.ocean.URL = article.table[i,"code.ocean.URL"],
reporting.type = article.table[i,"reporting.type"],
disclosure.URL = article.table[i,"disclosure.URL"],
disclosure.date = article.table[i,"disclosure.date"],
discl.exclusions = article.table[i,"discl.exclusions"],
discl.conditions = article.table[i,"discl.conditions"],
discl.DVs = article.table[i,"discl.DVs"],
discl.sample.size = article.table[i,"discl.sample.size"],
discl.analyses = article.table[i,"discl.analyses"],
discl.other.studies = article.table[i,"discl.other.studies"],
discl.others = article.table[i,"discl.others"],
rs = article.table[i,"rs"],
om = article.table[i,"om"],
prereg = article.table[i,"prereg"],
od = article.table[i,"od"],
rc = article.table[i,"rc"],
article.type = article.table[i,"article.type"],
rep.num = article.table[i,"rep.num"],
addition.date = article.table[i,"addition.date"],
target.effects = article.table[i,"target.effects"],
EC.URL = article.table[i,"EC.URL"],
added.by = NA.to.blank(article.table[i,"added.by"]),
article.URL = article.table[i,"article.URL"],
commentaries.URLs = article.table[i,"commentaries.URLs"]