require(htmltools)
require(shiny) #required functions to source
require(numform) #required for f_denom fucntion (convert 1333 into 1.3K)


#STEP 1: pre-code string constant
#(customizations settings in here, but implemented later)
curateArticleList <- function(articleList, yearHeadings = FALSE, cardWidth = 550, includeDOILinks = TRUE, 
                              articleWidgetBorder=FALSE, cardHoverEmphasis=TRUE, output.file.path) {

    incrementalString = ""
    #headerString <- HTML(paste0("<div class='widget-container' style='margin-bottom:50px;'>
    #    						    <div class='article-list-heading' style='margin-top:5px;margin-bottom:25px;'>Articles 
    #      						    <a href='http://curatescience.org' title='Powered by CurateScience.org'><img src='../logos/author-logos/CS-logo-powered-by-logo.png'
    #                        style='max-width:90px;padding-bottom:2px;'></a>
    #      						  </div>", "\n"))
    
    #incrementalString = HTML(paste0(incrementalString , headerString))
    
    #STEP 2: sort article list
    #will sort/categorize peer-reviewed vs. working paper later
    #sort latest to oldest (later)
    
    #STEP 3: Genereat HTML for each articles (& concatenate incrementally)
    articleListString = ""
    for (i in 1:nrow(articleList)) {
    #for (i in 1:12) {
      #calling by header names so still works if column order changes in CSV file
      articleListString = HTML(paste0(articleListString , create.article.card.HTML(authors = articleList[i,"authors"],
                                                                                   pub.year = articleList[i,"pub.year"],
                                                                                   article.title = articleList[i,"article.title"],
                                                                                   journal.name = articleList[i,"journal.name"],
                                                                                   DOI = articleList[i,"DOI"],
                                                                                   article.PDF.URL = articleList[i,"article.PDF.URL"],
                                                                                   PDF.views = articleList[i,"PDF.views"],
                                                                                   PDF.downloads = articleList[i,"PDF.downloads"],
                                                                                   PDF.citations = articleList[i,"PDF.citations"],
                                                                                   article.HTML.URL = articleList[i,"article.HTML.URL"],
                                                                                   HTML.views = articleList[i,"HTML.views"],
                                                                                   article.preprint.URL = articleList[i,"article.preprint.URL"],
                                                                                   preprint.views = articleList[i,"preprint.views"],
                                                                                   preprint.downloads = articleList[i,"preprint.downloads"],
                                                                                   metrics.date = articleList[i,"metrics.date"],
                                                                                   prereg.type = articleList[i,"prereg.type"],
                                                                                   prereg.URL = articleList[i,"prereg.URL"],
                                                                                   open.mat.URL = articleList[i,"open.mat.URL"],
                                                                                   open.data.URL = articleList[i,"open.data.URL"],
                                                                                   open.code.URL = articleList[i,"open.code.URL"],
                                                                                   rs.type = articleList[i,"rs.type"],
                                                                                   disclosure.date = articleList[i,"disclosure.date"],
                                                                                   article.type = articleList[i,"article.type"],
                                                                                   rep.num = articleList[i,"rep.num"],
                                                                                   original.study = articleList[i,"original.study"],
                                                                                   target.effects = articleList[i,"target.effects"],
                                                                                   commentaries.URLs = articleList[i,"commentaries.URLs"],
                                                                                   abstract.text = articleList[i,"abstract.text"],
                                                                                   keywords = articleList[i,"keywords"],
                                                                                   author.contributions = articleList[i,"author.contributions"],
                                                                                   competing.interests = articleList[i,"competing.interests"],
                                                                                   funding.sources = articleList[i,"funding.sources"],
                                                                                   peer.review.editor = articleList[i,"peer.review.editor"],
                                                                                   peer.reviewers = articleList[i,"peer.reviewers"],
                                                                                   peer.reviews.URL = articleList[i,"peer.reviews.URL"],
                                                                                   addition.date = articleList[i,"addition.date"])))
    }
    incrementalString = HTML(paste0(incrementalString , articleListString))
    
    
    #STEP 4: Closing tags and final concatenation
    #closingTags = HTML("</div>")
    #incrementalString = HTML(paste0(incrementalString , closingTags))
    
    if (output.file.path != "") {
      cat(iconv(incrementalString, to="UTF-8"), file=paste0(output.file.path, "/article-list-widget.html"))
    }
    else {
      return (incrementalString)}
}



create.article.card.HTML <- function(authors,	pub.year, article.title, journal.name, DOI, article.PDF.URL, PDF.views, PDF.downloads, 
                                     PDF.citations,	article.HTML.URL,	HTML.views,	article.preprint.URL,	preprint.views,	preprint.downloads,
                                     metrics.date, prereg.type,	prereg.URL,	open.mat.URL,	open.data.URL, open.code.URL,	rs.type,
                                     disclosure.date,	article.type,	rep.num, original.study,	target.effects,	commentaries.URLs,
                                     abstract.text,	keywords,	author.contributions,	competing.interests,	funding.sources,	peer.review.editor,
                                     peer.reviewers, peer.reviews.URL, addition.date) {
  cardString = HTML(paste0("<tr><td><div class='demo-card-wide mdl-card mdl-shadow--2dp'> <a class='anchor' id='", getArticleUniqueID(authors, DOI), "'></a>
                      <div class='mdl-card__supporting-text'>",
                          fullTextComponents(article.PDF.URL, PDF.views, PDF.downloads,	PDF.citations, article.HTML.URL, HTML.views, 
                                             article.preprint.URL, preprint.views, preprint.downloads, metrics.date),
      										"<div class='article-type-line'>", "\n\t\t",
                              articleTypeLabels(article.type, prereg.type, prereg.URL, rep.num, original.study, target.effects), "\n\t\t",
    													"<div class='sharing-links'>", "\n\t\t\t",
    													    "<a href='#", getArticleUniqueID(authors, DOI), "'><i class='fas fa-link link-button'></i></a> 
                               </div>
                           </div>
                            <div class='article-title'>", article.title, "</div>
                            <div class='author-text'>", authors, " (", pub.year, ")</div>
									          <div class='journal-text'>", getJournalName(journal.name), "</div>", DOIcomponent(DOI), 
      											    transparencyComponent(prereg.type, prereg.URL, open.mat.URL, open.data.URL, open.code.URL, rs.type,	
      											                          disclosure.date, article.type),
      											    commentaries.label(commentaries.URLs),
      											    moreButton(getArticleUniqueID(authors, DOI)),
      											"<div id='abstract-section-", getArticleUniqueID(authors, DOI), "' class='abstract-section' style='display:none;'>
      											    <div class='abstractText minimize'>", NA.to.blank(abstract.text), 
      											    "</div>",
      											    keywordsHTML(keywords),
      											"<div class='secondary-section'>
        											  <div class='secondary-section-on-hover'>
          											  <div class='author-contributions minimizeAC'>", NA.to.blank(author.contributions), 
          											  "</div>
        											    <div class='competing-interests-info'>", competingInterestsHTML(competing.interests), "</div>
          											  <div class='funding-sources-info'>", fundingSourcesHTML(funding.sources), "</div>
          											  <div class='peer-review-info'>", editorHTML(peer.review.editor), reviewersHTML(peer.reviewers), 
openPeerReviewHTML(peer.reviews.URL), "</div>
          											</div>
        										</div>
      									</div> <div class='addition-date-text'>Added: ", addition.date, "</div>
</div></div>
</td>", tags$td(rsFilterText(rs.type)), tags$td(omFilterText(open.mat.URL)), tags$td(preregFilterText(prereg.type)),
 tags$td(odFilterText(open.data.URL)), tags$td(rcFilterText(open.code.URL)), tags$td(articleTypeFilterText(article.type)),
"</tr>"))
  
  #postCard = HTML(paste0("</div></div>", "\n"))
  #cardString = HTML(paste0(preCard,postCard))
  return(cardString)
}

rsFilterText <- function(rs.type) { if (toString(rs.type) != "NA") { return("rs") } }
omFilterText <- function(open.mat.URL) { if (toString(open.mat.URL) != "NA") { return("om") } }
preregFilterText <- function(prereg.type) { if (toString(prereg.type) != "NA") { return(trimws(prereg.type)) } }
odFilterText <- function(open.data.URL) { if (toString(open.data.URL) != "NA") { return("od") } }
rcFilterText <- function(open.code.URL) { if (toString(open.code.URL) != "NA") { return("rc") } }
articleTypeFilterText <- function(article.type) { if (toString(article.type) != "NA") { return(trimws(article.type)) } }

getArticleUniqueID <- function(authors, DOI) {
  #article DOI or space-stripped-authors if no DOI
  if (toString(DOI) != "NA") {
    return(trimws(DOI)) }
  else {
    return(gsub(" ", "", authors, fixed = TRUE))
  }
}
articleTypeLabels <- function(article.type, prereg.type, prereg.URL, rep.num, original.study, target.effects) { 
  if (toString(article.type)=="conceptual"){
    return ("<span class='label label-info-teal-darker' title='Article involves a conceptual/theoretical contribution'>Conceptual</span>") }
  else if (toString(article.type)=="reanalysis") {
    return ("<span class='label label-info-pecan' title='Article reports a reanalysis of previously published studies'>Reanalysis</span>")
  }
  else if (toString(article.type)=="reanalysis - reproducibility") {
    return ("<span class='label label-info-pecan' title='Article reports an analytic reproducibility reanalysis of a previously published article (repeating the same statistical analyses on existing data)'>Reanalysis - Reproducibility</span>")
  }
  else if (toString(article.type)=="reanalysis - robustness") {
    return ("<span class='label label-info-pecan' title='Article reports a robustness reanalysis of a previously published article (conducting different statistical analyses on existing data)'>Reanalysis - Robustness</span>")
  }
  else if (toString(article.type)=="reanalysis - meta-analysis") {
    return ("<span class='label label-info-pecan'
            title='Article reports a (traditional) meta-analysis of a target effect/phenomenon'>Reanalysis - Meta-analysis</span>")
  }
  else if (toString(article.type)=="reanalysis - meta-research") {
    return ("<span class='label label-info-pecan' 
            title='Article reports a reanalysis of results from previously published studies (meta-research).'>Reanalysis - Meta-research</span>")
  }
  else if (toString(article.type)=="commentary") {
    return ("<span class='label label-info-commentary' title='Article is a commentary on previous research'>Commentary</span>")
  }
  else if (toString(article.type)=="original") {
    if (toString(prereg.type)=="preregplusrr") {
      return (HTML(paste0("<span class='label label-info-black' 
            title='Article reports original empirical observations (not directly comparable to previous research'>Original</span>", 
             RR.label(prereg.URL))))
    }
    else {
      return ("<span class='label label-info-black' 
            title='Article reports original empirical observations (not directly comparable to previous research'>Original</span>") }
  }
  else if (toString(article.type)=="replication") {
    if (toString(prereg.type)=="preregplusrr") {
      return (HTML(paste0("<div style='display:inline;' class='popUpOnHover'>
													<span class='label label-info-brown'>Replication <span class='badge badge-repnum'>", NA.to.blank(trimws(rep.num)), "</span></span> 
                          <span class='toDisplayRepDetails popUpStyle'>
                          <span style='font-size:12px;'>Article reports ", NA.to.blank(trimws(rep.num)), " replications of ", 
                          NA.to.blank(trimws(original.study)), " ", NA.to.blank(trimws(target.effects)), "</span>
                          <br><br>
                          <span style='font-size:10px;color:gray;'>A replication is a study that uses a methodology that is 'close' or 'very close' 
                          to a previous study (see <a href='http://curatescience.org/logos/replication-taxonomy-v4_small.png' target='_blank'>replication taxonomy</a> for details).
                          </span></span></div>", RR.label(prereg.URL) )))
    }
    else {
      return (HTML(paste0("<div style='display:inline;' class='popUpOnHover'>
													<span class='label label-info-brown'>Replication <span class='badge badge-repnum'>", NA.to.blank(trimws(rep.num)), "</span></span> 
                          <span class='toDisplayRepDetails popUpStyle'>
                          <span style='font-size:12px;'>Article reports ", NA.to.blank(trimws(rep.num)), " replications of ", 
                          NA.to.blank(trimws(original.study)), " ", NA.to.blank(trimws(target.effects)), "</span>
                          <br><br>
                          <span style='font-size:10px;color:gray;'>A replication is a study that uses a methodology that is 'close' or 'very close' 
to a previous study (see <a href='http://curatescience.org/logos/replication-taxonomy-v4_small.png' target='_blank'>replication taxonomy</a> for details).
</span></span></div>"))) }
    }
}

RR.label <- function(prereg.URL) {
    if (toString(prereg.URL) != "NA")
    { return(HTML(paste0(" <span class='label label-info-red'><a href='", prereg.URL, "' target='_blank' class='black' 
                         title='Go to Registered Report preregistered study protocol.'>Registered Report</a></span>")))}
    else 
    { return(HTML(" <span class='label label-info-red' 
                  title='Registered Report preregistered study protocol not (yet) publicly available.'>Registered Report</span> "))}
}

fullTextComponents <- function(article.PDF.URL, PDF.views, PDF.downloads,	PDF.citations, article.HTML.URL, HTML.views, 
                               article.preprint.URL, preprint.views, preprint.downloads, metrics.date) { 
  full.text.components <- HTML(paste0("<div class='fulltextcomponent'>",
                                      PDF.icon.impact(article.PDF.URL, PDF.views, PDF.downloads, PDF.citations, metrics.date),
                                      article.HTML.label.impact(article.HTML.URL, HTML.views, metrics.date, article.PDF.URL),
                                      preprint.label.impact(article.preprint.URL, preprint.views, preprint.downloads, metrics.date, 
                                                            article.PDF.URL, article.HTML.URL),
                                      "</div>"))
  return (full.text.components)
}

PDF.icon.impact <- function(article.PDF.URL, PDF.views, PDF.downloads,	PDF.citations, metrics.date) {
  if(toString(article.PDF.URL) != "NA") {
    impact.div = HTML(paste0("<div class='impact-info'>", PDF.views.HTML(PDF.views, metrics.date), PDF.downloads.HTML(PDF.downloads, metrics.date), 
                                PDF.citations.HTML(PDF.citations, metrics.date), "</div>"))
    return(HTML(paste0(impact.div, " <a href='",article.PDF.URL,"' target='_blank' class='black' title='View PDF of this article (postprint).'>
                       <img src='logos/author-logos/sprite-icons/pdf.png' class='pdf-icon transparent full-text'></a> "))) }
}
article.HTML.label.impact <- function(article.HTML.URL, HTML.views, metrics.date, article.PDF.URL) {
  if(toString(article.HTML.URL) != "NA") {
    if(toString(article.PDF.URL) !="NA") {
      impact.div = HTML(paste0("<br><div class='impact-info'>", HTML.views.HTML(HTML.views, metrics.date), "</div>"))  
      return(HTML(paste0(impact.div, " <span class='label label-info-html full-text'><a href='",article.HTML.URL,"' target='_blank' 
                        class='black' title='View HTML version of this article (postprint).'>HTML</a></span> ")))} 
    else {
      impact.div = HTML(paste0("<div class='impact-info'>", HTML.views.HTML(HTML.views, metrics.date), "</div>"))  
      return(HTML(paste0(impact.div, " <span class='label label-info-html full-text'><a href='",article.HTML.URL,"' target='_blank' 
                       class='black' title='View HTML version of this article (postprint).'>HTML</a></span> ")))}
    }
}
preprint.label.impact <- function(article.preprint.URL, preprint.views, preprint.downloads, metrics.date, article.PDF.URL, article.HTML.URL) {
  if(toString(article.preprint.URL) !="NA") {
    if(toString(article.HTML.URL) != "NA" | toString(article.PDF.URL) != "NA") {
      impact.div = HTML(paste0("<br><div class='impact-info'>", preprint.views.HTML(preprint.views, metrics.date), 
                               preprint.downloads.HTML(preprint.downloads, metrics.date), "</div>"))  
      return(HTML(paste0(impact.div, " <span class='label label-info-purple full-text'><a href='",article.preprint.URL,"' target='_blank' 
                  class='black' title='View preprint of this article.'>", contentProviderIcon(article.preprint.URL), " Preprint</a></span>")))} 
    else {
      impact.div = HTML(paste0("<div class='impact-info'>", preprint.views.HTML(preprint.views, metrics.date), 
                               preprint.downloads.HTML(preprint.downloads, metrics.date), "</div>"))  
      return(HTML(paste0(impact.div, " <span class='label label-info-purple full-text'><a href='",article.preprint.URL,"' target='_blank' 
                  class='black' title='View preprint of this article.'>", contentProviderIcon(article.preprint.URL), " Preprint</a></span>")))}
  }
}

PDF.views.HTML <- function(PDF.views, metrics.date) {
  if(toString(PDF.views) != "NA") {
    return(HTML(paste0("<span title='Article PDF (postprint) has been viewed ", PDF.views, " times (as of ", metrics.date, ")'>
      <i class='fas fa-eye'></i> ", f_denom(strtoi(PDF.views), relative=1, pad.char = ''), " </span>")))}
}
PDF.downloads.HTML <- function(PDF.downloads, metrics.date) {
  if(toString(PDF.downloads) != "NA") {
    return(HTML(paste0("<span style='margin-left:7px;' title='Article PDF (postprint) has been downloaded ", PDF.downloads, " times (as of ", metrics.date, ")'>
      <i class='fas fa-download'></i> ", f_denom(strtoi(PDF.downloads), relative=1, pad.char = ''), " </span>")))}
}
PDF.citations.HTML <- function(PDF.citations, metrics.date) {
  if(toString(PDF.citations) != "NA") {
    return(HTML(paste0("<span style='margin-left:7px;' title='Article has been cited ", PDF.citations, " times (as of ", metrics.date, "; Google Scholar)'>
                       <i class='fas fa-quote-right'></i> ", f_denom(strtoi(PDF.citations), relative=1, pad.char = ''), " </span>")))}
}
HTML.views.HTML <- function(HTML.views, metrics.date) {
  if(toString(HTML.views) != "NA") {
    return(HTML(paste0("<span  title='HTML has been viewed ", HTML.views, " times (as of ", metrics.date, ")'>
                       <i class='fas fa-eye'></i> ", f_denom(strtoi(HTML.views), relative=1, pad.char = ''), " </span>")))}
}
preprint.views.HTML <- function(preprint.views, metrics.date) {
  if(toString(preprint.views) != "NA") {
    return(HTML(paste0("<span title='Preprint has been viewed ", preprint.views, " times (as of ", metrics.date, ")'>
      <i class='fas fa-eye'></i> ", f_denom(strtoi(preprint.views), relative=1, pad.char = ''), " </span>")))}
}
preprint.downloads.HTML <- function(preprint.downloads, metrics.date) {
  if(toString(preprint.downloads) != "NA") {
    return(HTML(paste0("<span style='margin-left:7px;' title='Preprint has been downloaded ", preprint.downloads, " times (as of ", metrics.date, ")'>
      <i class='fas fa-download'></i> ", f_denom(strtoi(preprint.downloads), relative=1, pad.char = ''), " </span>")))}
}

DOIcomponent <- function (DOI) {
  if (toString(DOI) != "NA") {
    return (HTML(paste0(" <div class='doi-dimmed'> <a href='https://dx.doi.org/" , DOI, "' target='_blank' class='doi-dimmed grey'>", DOI, "</a></div>")))
  }
}
getJournalName <- function (journal.name) {
  if (toString(journal.name) != "NA") {
    return (journal.name)
  }
}

transparencyComponent <- function(prereg.type, prereg.URL, open.mat.URL, open.data.URL, open.code.URL, rs.type,	disclosure.date, article.type) {
  #5 transparency badges
  if(toString(article.type) == "original" | toString(article.type) == "replication") {
    transparencyComponentString = HTML(paste0("<br><div class='transparency-line'>", prereg.badge.HTML(prereg.type, prereg.URL, article.type), 
                                              materials.badge.HTML(open.mat.URL, article.type), data.badge.HTML(open.data.URL, article.type), 
                                              code.badge.HTML(open.code.URL, article.type), rs.badge.HTML(rs.type, disclosure.date, article.type), "</div>"))
    return(transparencyComponentString)}
  #4 transparency badges
  else if (toString(article.type) == "reanalysis - meta-analysis") {
    transparencyComponentString = HTML(paste0("<br><div class='transparency-line'>", prereg.badge.HTML(prereg.type, prereg.URL, article.type), 
                                              data.badge.HTML(open.data.URL, article.type), code.badge.HTML(open.code.URL, article.type), 
                                              rs.badge.HTML(rs.type, disclosure.date, article.type), "</div>"))
    return(transparencyComponentString)}
  #3 transparency badges
  else if (toString(article.type) == "reanalysis (other)" | toString(article.type) == "reanalysis - reproducibility" 
           | toString(article.type) == "reanalysis - robustness" | toString(article.type) == "reanalysis - meta-research") {
    transparencyComponentString = HTML(paste0("<br><div class='transparency-line'>", prereg.badge.HTML(prereg.type, prereg.URL, article.type), 
                                              data.badge.HTML(open.data.URL, article.type), code.badge.HTML(open.code.URL, article.type), "</div>"))
    return(transparencyComponentString)}
  #5 transparency ("BONUS") badges - meaning no "unavailable badge" if no URL provided 
  else if (toString(article.type) == "conceptual" | toString(article.type) == "commentary") {
    if (toString(prereg.URL) != "NA" | toString(open.mat.URL) != "NA" | toString(open.data.URL) != "NA" | toString(open.code.URL) != "NA" 
        | toString(rs.type) != "NA") {
        transparencyComponentString = HTML(paste0("<br><div class='transparency-line'>", prereg.badge.HTML(prereg.type, prereg.URL, article.type), 
                                                  materials.badge.HTML(open.mat.URL, article.type), data.badge.HTML(open.data.URL, article.type), 
                                                  code.badge.HTML(open.code.URL, article.type), rs.badge.HTML(rs.type, disclosure.date, article.type), "</div>"))
        return(transparencyComponentString) }
    }
}

prereg.badge.HTML <- function(prereg.type, prereg.URL, article.type) 
{ if (toString(prereg.type) == "preregplusrr") #is an RR
  { if (toString(prereg.URL) != "NA") #is an RR *and* pre-reg protocol available
    { return(HTML(paste0("<div style='display:inline;' class='popUpOnHover'>
                           <img src='logos/author-logos/sprite-icons/preregistered-plus-available.png' class='shrunk-20 transparent'>
                           <span class='toDisplayBadgePopupOneLinkPrereg popUpStyle'>
                           <div style='min-width:200px;white-space:normal;'>
                           <span class='transp-popup-main-heading'>Preregistered protocol</span>
                           <div class='prereg-type-text'>Preregistered study design + analyses</div>
                           <a href='",prereg.URL,"' target='_blank'> <i class='fas fa-link'></i> ", 
                         contentProviderIcon(prereg.URL), " <i class='fas fa-external-link-alt'></i></a>
                           </div>
                           </span>
                           </div>")))
    }  
      else #RR, but pre-reg protocol unavailable
      { return(HTML("<a title='Registered Report preregistered study protocol not (yet) publicly available'>
                    <img src='logos/author-logos/sprite-icons/preregistered-plus-available.png' class='shrunk-20 transparent'></a> "))
      }
    }
  else #not an RR (but prereg or preregplus)
  { if (toString(prereg.type) == "preregplus") #pre-registered study design + analytic plans
  { return(HTML(paste0("<div style='display:inline;' class='popUpOnHover'>
                         <img src='logos/author-logos/sprite-icons/preregistered-plus-available.png' class='shrunk-20 transparent'>
                         <span class='toDisplayBadgePopupOneLinkPrereg popUpStyle'>
                         <div style='min-width:200px;white-space:normal;'>
                         <span class='transp-popup-main-heading'>Preregistered protocol</span>
                         <div class='prereg-type-text'>Preregistered study design + analyses</div>
                         <a href='",prereg.URL,"' target='_blank'> <i class='fas fa-link'></i> ", 
                         contentProviderIcon(prereg.URL), " <i class='fas fa-external-link-alt'></i></a>
                         </div>
                         </span>
                         </div>")))
  }
    else if (toString(prereg.type) == "prereg")  #pre-registered study design ONLY
    { return(HTML(paste0("<div style='display:inline;' class='popUpOnHover'>
                           <img src='logos/author-logos/sprite-icons/preregistered-available.png' class='shrunk-20 transparent'>
                           <span class='toDisplayBadgePopupOneLinkPrereg popUpStyle'>
                           <div style='min-width:200px;white-space:normal;'>
                           <span class='transp-popup-main-heading'>Preregistered protocol</span>
                           <div class='prereg-type-text'>Preregistered study design</div>
                           <a href='",prereg.URL,"' target='_blank'> <i class='fas fa-link'></i> ", 
                         contentProviderIcon(prereg.URL), " <i class='fas fa-external-link-alt'></i></a>
                           </div>
                           </span>
                           </div>")))
    }
    else if (toString(article.type) != "conceptual" & toString(article.type) != "commentary")
      #not an RR, nor pre-registered (nor conceptual nor commentary)
    { return(HTML("<a title='Preregistration information is not available'><img src='logos/author-logos/sprite-icons/preregistered-unavailable.png' 
                  class='shrunk-20'></a>"))
    }
    }
}
materials.badge.HTML <- function(open.mat.URL, article.type) {
  if (toString(open.mat.URL) != "NA") {
    return(HTML(paste0("<div style='display:inline;' class='popUpOnHover'>
													<img src='logos/author-logos/sprite-icons/materials-available.png' class='shrunk-20 transparent'>
													<span class='toDisplayBadgePopupOneLink popUpStyle'>
														<div style='min-width:200px;white-space:normal;'>
															<span class='transp-popup-main-heading'>Public Study Materials</span>
															<br>
															<a href='",open.mat.URL,"' target='_blank'> <i class='fas fa-link'></i> ", 
contentProviderIcon(open.mat.URL), " <i class='fas fa-external-link-alt'></i></a>
														</div>
													</span>
												</div>"))) }
  else if (toString(article.type) != "conceptual" & toString(article.type) != "commentary") {
    return(HTML("<a title='Public study materials information is not available'>
                <img src='logos/author-logos/sprite-icons/materials-unavailable.png' class='shrunk-20'></a> "))
  }
}
data.badge.HTML <- function(open.data.URL, article.type) {
  if (toString(open.data.URL) != "NA") {
    return(HTML(paste0("<div style='display:inline;' class='popUpOnHover'>
													<img src='logos/author-logos/sprite-icons/data-available.png' class='shrunk-20 transparent'>
                       <span class='toDisplayBadgePopupOneLink popUpStyle'>
                       <div style='min-width:200px;white-space:normal;'>
                       <span class='transp-popup-main-heading'>Public Data</span>
                       <br>
                       <a href='",open.data.URL,"' target='_blank'> <i class='fas fa-link'></i> ", 
contentProviderIcon(open.data.URL), " <i class='fas fa-external-link-alt'></i></a>
                       </div>
                       </span>
                       </div>"))) }
  else if (toString(article.type) != "conceptual" & toString(article.type) != "commentary") {
    return(HTML("<a title='Public data information is not available'>
                <img src='logos/author-logos/sprite-icons/data-unavailable.png' class='shrunk-20'></a> "))
  }
}
code.badge.HTML <- function(open.code.URL, article.type) {
  if (toString(open.code.URL) != "NA") {
    return(HTML(paste0("<div style='display:inline;' class='popUpOnHover'>
													<img src='logos/author-logos/sprite-icons/code-available.svg' class='shrunk-22 transparent'>
													<span class='toDisplayBadgePopupOneLink popUpStyle'>
														<div style='min-width:200px;white-space:normal;'>
															<span class='transp-popup-main-heading'>Public Code</span>
															<br>
															<a href='",open.code.URL,"' target='_blank'> <i class='fas fa-link'></i> ", 
contentProviderIcon(open.code.URL), " <i class='fas fa-external-link-alt'></i></a>
														</div>
													</span>
												</div>")))}
  else if (toString(article.type) != "conceptual" & toString(article.type) != "commentary") {
    return(HTML("<a title='Reproducible code information is not available'><img src='logos/author-logos/sprite-icons/code-unavailable.svg' 
                class='shrunk-22 transparent'></a>"))
  }
}

contentProviderIcon <- function (content.URL) {
  if ( grepl("osf",content.URL) == TRUE | grepl("psyarxiv",content.URL) == TRUE | grepl("openscienceframework",content.URL) == TRUE ) {
    return ( "<i class='ai ai-osf ai-2x small-icon'></i>") }
  else if ( grepl("figshare",content.URL) == TRUE) {
    return ( "<i class='ai ai-figshare ai-2x small-icon'></i>") }
  else if ( grepl("ssrn",content.URL) == TRUE) {
    return ( "<img src='logos/author-logos/sprite-icons/SSRN-logo.png' class='SSRN-logo'>") }
}
    
rs.badge.HTML <- function(rs.type, disclosure.date, article.type) {
  if(toString(rs.type) == "basic4.at.subm") {
    return(HTML(basic4.at.subm.HTML))}
  else if (toString(rs.type) == "basic4.retro") {
    return(HTML(paste0("<div style='display:inline;' class='popUpOnHover'>
                       <img src='logos/author-logos/sprite-icons/disclosure-available.png' class='shrunk-20 transparent'>
                       <span class='toDisplayBasic4Retroactive popUpStyle' style='padding-left:5px;white-space:nowrap;'>
                       <span class='transp-popup-main-heading'>Reporting Standards</span><br>                         
                       <span style='font-size:10px;font-weight:600;color:gray;'>Study complies with the 
                       <a href='http://psychdisclosure.org' target='_blank'>Basic 4 (retroactive)</a> 
                       <img src='logos/author-logos/sprite-icons/disclosure-available.png' class='shrunk-16 transparent'> reporting standard:</span>
                       <ol>
                       <li><strong>Excluded data (subjects/observations):</strong> Full details reported in article.</li>
                       <li><strong>Experimental conditions:</strong> Full details reported in article.</li>
                       <li><strong>Outcome measures:</strong> Full details reported in article.</li>
                       <li><strong>Sample size determination:</strong> Full details reported in article.</li>
                       </ol>
                       <span style='font-size:10px;color:red;'>Date of retroactive disclosure: ", disclosure.date, ".</span>
                       </span>
                       </div>")))
  }
  else if (toString(rs.type) == "basic7.retro") {
    return()
  }
  else if (toString(rs.type) == "MARS") {
    return(HTML(paste0("<div style='display:inline;' class='popUpOnHover'>
                       <img src='logos/author-logos/sprite-icons/disclosure-available.png' class='shrunk-20 transparent'>
                       <span class='toDisplayMARS popUpStyle' style='padding-left:5px;'>
                       <span class='transp-popup-main-heading'>Reporting Standards</span><br>
                       <span style='font-size:10px;font-weight:600;color:gray;'>Meta-analysis complies with the 
                       <a href='http://www.apa.org/pubs/journals/releases/amp-amp0000191.pdf' target='_blank'>MARS</a> 
                       <img src='logos/author-logos/sprite-icons/disclosure-available.png' class='shrunk-16 transparent'> reporting standard: 
                       <ul style='font-size:10px;line-height:normal;'>
                       <li><a href='http://www.apa.org/pubs/journals/releases/amp-amp0000191.pdf' target='_blank'>
                       APA's Meta-Analysis Reporting Standards (MARS; Table 9)</a> 
                       </li></span>
                       </ul>
                       </span>
                       </div>")))
  } 
  else if (toString(article.type) != "conceptual" & toString(article.type) != "commentary") {
    return(HTML(" <a title='Compliance to reporting standard information is not available'>
                <img src='logos/author-logos/sprite-icons/disclosure-unavailable.png' class='shrunk-20'></a>"))
  }
  }

basic4.at.subm.HTML <- "<div style='display:inline;' class='popUpOnHover'>
<img src='logos/author-logos/sprite-icons/disclosure-available.png' class='shrunk-20 transparent'>
<span class='toDisplayBasic4Submission popUpStyle' style='padding-left:5px;white-space:nowrap;'>
<span class='transp-popup-main-heading'>Reporting Standards</span><br>
<span style='font-size:10px;font-weight:600;color:gray;'>Study complies with the 
<a href='https://www.psychologicalscience.org/publications/psychological_science/ps-submissions#DISC' target='_blank'>
Basic 4 (at submission)</a> <img src='logos/author-logos/sprite-icons/disclosure-available.png' class='shrunk-16 transparent'> reporting standard:</span>
<ol>
<li><strong>Excluded data (subjects/observations):</strong> Full details reported in article.</li>
<li><strong>Experimental conditions:</strong> Full details reported in article.</li>
<li><strong>Outcome measures:</strong> Full details reported in article.</li>
<li><strong>Sample size determination:</strong> Full details reported in article.</li>
</ol>
</span>
</div>"

commentaries.label <- function(commentaries.URLs) {
  if(toString(commentaries.URLs) != "NA") {
    URLs.list <- strsplit(commentaries.URLs, ";")
    comment.list=""
    for (i in 1:lengths(URLs.list)) { 
      comment.list <- paste0(comment.list, "<li><a href='", URLs.list[[1]][i] , "' target='_blank'>Commentary ", toString(i), "</a></li>") }
    return(HTML(paste0("<div style='display:inline;' class='popUpOnHover'>
                       <span class='label label-info-commentaries' >Commentaries <span class='badge badge-repnum'>",
                       lengths(URLs.list), "</span></span> 
                       <span class='toDisplayCommentaries popUpStyle' style='padding-left:5px;white-space:nowrap;'>
                       Commentaries about this article: 
                       <ul style='font-size:12px;line-height:normal;'>"
                       , comment.list, "</ul></span></div>")))
  }
}


moreButton <- function (articleUniqueID) {
  return(HTML(paste0("<div class='more-button full-text' id='moreDiv-", articleUniqueID, "' style='margin-top:4px;'>	
    <button class='mdl-button mdl-js-button mdl-button--icon mdl-js-ripple-effect expand-button rotate' 
    onclick=\"toggle_visibility('abstract-section-", articleUniqueID, "', 'moreDiv-", articleUniqueID, "');\" 
    title='Toggle abstract, key figures, peer-review info, and other details.'>
      <i class='fa fa-chevron-down'></i>
        </button>
        </div>")))
}

keywordsHTML <- function(keywords) {
  if (toString(keywords) != "NA") {
    keywords.list <- strsplit(keywords, ";")
    keywords.HTML.list=""
    for (i in 1:lengths(keywords.list)) { 
      keywords.HTML.list <- paste0(keywords.HTML.list, " <span class='mdl-chip small-chip'>
                                   <span class='mdl-chip__text small-chip-text'>", trimws(keywords.list[[1]][i]) , "</span></span>") }
    return (HTML(paste0("<div class='key-words'>", keywords.HTML.list, "</div>")))
  }
}
competingInterestsHTML <- function(competing.interests) {
  if(toString(competing.interests) != "NA") {
    return(HTML(paste0("<strong title='Competing commercial or financial interests.'>Competing interests</strong>: ", competing.interests)))}
}
fundingSourcesHTML <- function(funding.sources) {
  if(toString(funding.sources) != "NA") {
    return(HTML(paste0("<strong>Funding sources</strong>: ", funding.sources)))}
}

editorHTML <- function(peer.review.editor) {
  if(toString(peer.review.editor) != "NA") { return(HTML(paste0("<strong>Editor</strong>: <span style='color:#009933;'>", peer.review.editor, "</span>")))}
}
reviewersHTML <- function(peer.reviewers) {
  if(toString(peer.reviewers) != "NA") { return(HTML(paste0("&nbsp;&nbsp;&nbsp;<strong>Reviewers</strong>: <span style='color:#009933;'>", peer.reviewers, "</span>")))}
}
openPeerReviewHTML <- function(peer.reviews.URL) {
  if(toString(peer.reviews.URL) != "NA") { 
    return(HTML(paste0("<div class='open-peer-review'>
      <a href='", peer.reviews.URL, "' target='_blank'><i class='fas fa-link'></i> Open peer review <i class='fas fa-external-link-alt'></i></a>
      </div>")))}
}
  
NA.to.blank <- function (x) {
  if (is.na(x)) {
    return(replace(x,is.na(x),""))  }
  else return(x)
}
getArticleListMetaData <- function (article.list.path) {
  if (grepl("http",article.list.path) == TRUE) {
    article.table <- read.csv(url(article.list.path), quote = "\"", stringsAsFactors=FALSE,encoding="UTF-8",na.strings="")
    return(article.table) }
  else {
    article.table <- read.csv(article.list.path, quote = "\"", stringsAsFactors=FALSE,encoding="UTF-8",na.strings="")
    return(article.table) }
}

article.list.path <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQ8VCNAmiqCZ-Cr4iKZqxwofhVwO3yKLVX8ITOYtRvbRWYT7YAQbk7pAiU4-k8OBevN8Qx_VkO9gBuY/pub?gid=994838614&single=true&output=csv'
#article.list.path <- 'C:/Users/Etienne LeBel/Google Drive/Curate Science/website/science-commons-alpha/author-article-list-meta-data/lorne.campbell.csv'

output.file.path = "C:/Users/Etienne LeBel/Google Drive/Curate Science/website/science-commons-alpha/author"

curateArticleList (getArticleListMetaData(article.list.path), yearHeadings = FALSE, cardWidth = 550, includeDOILinks = TRUE, 
                   articleWidgetBorder=FALSE, cardHoverEmphasis=TRUE, output.file.path) 