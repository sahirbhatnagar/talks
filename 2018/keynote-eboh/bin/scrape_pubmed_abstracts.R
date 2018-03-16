#' #######################################################################################
#' Functions to scrape Pubmed abstracts and search for words related to Software
#'
#' @description This code first extracts Pubmed abstracts by journal and year of
#'   publication. Then it counts the number of abstracts that contain a
#'   prespecified set of words. This script was used to produce the figures in
#'   my keynote presentation titled "Pick your favorite buzzword: Data Science,
#'   Machine Learning, Big Data. Data Science, Machine Learning, Big Data" at
#'   the 14th Annual Research day in the Department of Epidemiology,
#'   Biostatistics and Occupational Health at McGill University on March 16th,
#'   2018.
#'
#' @note The general idea of scraping PubMed abstracts come from Jager and Leek
#'   2014 (Biostatistics). The function \code{getAbstractsPmids} is taken
#'   directly from https://github.com/jtleek/swfdr
#'
#' #######################################################################################


# Load packages -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(RCurl)
pacman::p_load(XML)
pacman::p_load(tm)
pacman::p_load(tidytext)
pacman::p_load(magrittr)
pacman::p_load(dplyr)
pacman::p_load(reshape2)
pacman::p_load(ggplot2)
pacman::p_load(cowplot)



# Functions ---------------------------------------------------------------

#' A function to get all abstracts and pubmed ids for papers from the journal
#' "journaltitle" in the year "year" by scraping the Pubmed API.
#' @note Taken directly from https://github.com/jtleek/swfdr

getAbstractsPmids <- function(journaltitle,year){
  # esearch
  # browser()
  url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?"
  q = paste("db=pubmed&term=",gsub(" ","+",journaltitle),"[ta]+AND+",year,"[dp]&usehistory=y",sep="")
  esearch <- xmlTreeParse(getURL(paste(url, q, sep="")), useInternal = T)
  webenv  <- xmlValue(getNodeSet(esearch, "//WebEnv")[[1]])
  key     <- xmlValue(getNodeSet(esearch, "//QueryKey")[[1]])
  
  # efetch
  url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
  q   <- "db=pubmed&retmode=xml&rettype=abstract"
  efetch <- xmlTreeParse(getURL(paste(url, q, "&WebEnv=", webenv, "&query_key=", key, sep="")), useInternal = T)
  r = xmlRoot(efetch)
  
  n = xmlSize(r)
  abstracts = pmid = titles = rep(NA,n)
  for(i in 1:n){abstracts[i] =  xmlValue(r[[i]][[1]][["Article"]][["Abstract"]]); pmid[i] = xmlValue(r[[i]][[1]][["PMID"]]); titles[i] = xmlValue(r[[i]][[1]][["Article"]][["ArticleTitle"]]) }
  return(list(abstracts=abstracts,pmid=pmid,titles=titles))
}


#' A function to get proportion of words related in each abstract
getSoftwareRelatedWords <- function(pmids, abstracts, 
                                    related_words = c("package","packages", "software","script", 
                                                      "scripts","code", "R software", "R package", 
                                                      "R code", "Comprehensive R Archive Network", 
                                                      "CRAN", "Bioconductor","GitHub", "Bitbucket",
                                                      "Python","Julia", "SAS", "Matlab", "Matlab toolkit","SAS macro")) {
  # pmids=tmpData$pmid;abstracts=tmpData$abstracts
  # ensure exact matching
  rel_words <- do.call(c,lapply(related_words, function(i) sprintf("^%s$",i)))
  # a grouped data.frame by pmids of words that are in related_words
  # can have multiple rows for the same pmid if there is more than 1 match
  pmids_words <- data.frame(pmids = pmids, abstracts = abstracts, stringsAsFactors = F) %>%
    unnest_tokens(word, abstracts) %>% 
    group_by(pmids) %>% 
    filter(grepl(paste(rel_words, collapse = "|"),word, ignore.case = TRUE)) 
  # filter(word %in% related_words)
  
  
  # count number of lines per pmid
  pmids_words_count <- pmids_words %>% 
    count()
  
  return(list(n_match = nrow(pmids_words_count), 
              prop_match = nrow(pmids_words_count) / length(pmids),
              n_total = length(pmids),
              word_match = pmids_words))
  
}

trop <- RSkittleBrewer::RSkittleBrewer("trop")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", RSkittleBrewer::RSkittleBrewer()[2], "#0072B2", "#D55E00", "#CC79A7",trop[4])
plot(seq_along(cbbPalette), col = cbbPalette, pch=19, cex=3)
plot(seq_along(trop), col = trop, pch=19, cex=3)
# Define Journals and Years -----------------------------------------------

journals = c("Biometrika","J Am Stat Assoc", "Ann Stat", "Ann Appl Stat","J R Stat Soc Series B Stat Methodol",
             "Biostatistics", "Biometrics","Stat Med","Stat. Methods Med. Res")
journalsAbb = c("Biometrika","JASA", "Ann Stat", "Ann Appl Stat","JRSSB",
             "Biostatistics", "Biometrics","Stat Med","Stat. Methods Med. Res")
years = 1998:2017



# Extract and count related words from PubMed -----------------------------

npapers = matrix(NA,nrow=length(journals),ncol=length(years))
propmatch = matrix(NA,nrow=length(journals),ncol=length(years))
words_match <- c() #not recommended to grow a vector
abstracts_total <- c()

for(i in 1:length(journals)){
  for(j in 1:length(years)){
    cat(journals[i]); cat(" "); cat(years[j]); cat(" "); 
    tmpData = getAbstractsPmids(journals[i],years[j])
    if (length(tmpData$abstracts) ==1 & is.na(tmpData$abstracts[1])) {
      npapers[i,j] <- 0
      propmatch[i,j] <- NA  
    } else {
      npapers[i,j] = length(tmpData$abstracts)
      result <- getSoftwareRelatedWords(pmids = tmpData$pmid, abstracts = tmpData$abstracts)
      # minimum of 10 papers required
      if (length(tmpData$abstracts) < 10) {
        propmatch[i,j] <- NA 
      } else {
        propmatch[i,j] <- result$prop_match
      }
      words_match <- c(words_match, result$word_match$word)
      abstracts_total <- c(abstracts_total, result$n_total)
    }
  }
}

dimnames(npapers) <- list(journals, years)
dimnames(propmatch) <- list(journals, years)

saveRDS(npapers, file = "results/npapers.rds")
saveRDS(propmatch, file = "results/propmatch.rds")
saveRDS(words_match, file = "results/words_match.rds")
saveRDS(abstracts_total, file = "results/abstracts_total.rds")


# Figure for words matched ------------------------------------------------

df.w <- words_match %>% table %>% as.data.frame() %>% set_colnames(c("word","freq")) %>% 
  mutate(prop = prop.table(freq)) %>% arrange(prop)

props <- df.w$prop
names(props) <- df.w$word
cbbPalette2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

png("figures/word_proportions.png", width = 12, height = 8, units = "in", res = 125)
par(las=2) # make label text perpendicular to axis
par(mar=c(5,13,4,2)) # increase y-axis margin.
barplot(props, 
        horiz=TRUE, 
        las=1,
        xlim = c(0,0.5),
        cex.names = 2,
        cex.axis = 2,
        cex.lab = 2,
        cex.main = 2,
        col= cbbPalette2[-1],
        main = "Words Related to Software Found in Pubmed Abstracts\n(proportions sum to 1)", ylab = "",xlab = "Proportion")
dev.off()

# Figure for Proportion with a match -------------------------------------------------

df <- as.data.frame(propmatch)
df$journal <- journalsAbb
df.m <- reshape2::melt(df, id.vars = "journal")
df.m$journal <- factor(df.m$journal, 
                       levels = c("JRSSB", "Biometrika","JASA","Ann Stat","Ann Appl Stat",
                                  "Biostatistics", "Biometrics","Stat Med","Stat. Methods Med. Res"))
df.m$variable <- as.numeric(as.character(df.m$variable))

gg_sy <- theme(legend.position = "none", axis.text = element_text(size = 17), 
               axis.title = element_text(size = 20), legend.text = element_text(size = 20), 
               legend.title = element_text(size = 20),
               strip.text = element_text(size = 20,margin = margin(.1,0,.1,0, "cm")))
p1 <- df.m %>% filter(journal != "Stat Comput") %>% ggplot(., aes(x = variable, y = value, color = journal)) +
  geom_point(size=3) + ylim(c(-0.01,0.15)) + 
  geom_smooth(se = FALSE, size=2) +
  facet_wrap(~journal) + panel_border()+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90))+
  # scale_color_brewer(type = "div", palette = "Set1") +
  scale_color_manual(values = cbbPalette) +
  ylab("Proportion of Papers with Software Related Terms in Pubmed Abstract") +
  scale_x_discrete(limits=c(1998, 2002,2006, 2010, 2014, 2017)) +
  # scale_x_discrete(limits=c(1998, 2002,2006, 2010, 2014, 2017))+
  # geom_smooth(formula = y ~ splines::bs(x, 3), se = FALSE) +
  gg_sy

cowplot::save_plot("figures/propmatch.png", p1, base_height = 10, base_width = 15)

# Figure for number of papers Extracted by Jounal and Year --------------------------

df <- as.data.frame(npapers)
df$journal <- journalsAbb
df.m <- reshape2::melt(df, id.vars = "journal")
df.m$journal <- factor(df.m$journal,
                       levels = c("JRSSB", "Biometrika","JASA","Ann Stat","Ann Appl Stat",
                                              "Biostatistics", "Biometrics","Stat Med","Stat. Methods Med. Res"))
df.m$variable <- as.numeric(as.character(df.m$variable))

p2 <- df.m %>% filter(journal != "Stat Comput") %>% ggplot(., aes(x = variable, y = value, fill = journal)) +
  geom_col(colour="black") +
  # geom_smooth(span = 0.5, se = FALSE) +
  facet_wrap(~journal, scales = "free") + panel_border()+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = NULL))+
  # scale_fill_brewer(type = "div", palette = "Set1") +
  scale_fill_manual(values = cbbPalette) +
  ylab("Number of Abstracts Scraped from PubMed")+
  scale_x_discrete(limits=c(1998, 2002,2006, 2010, 2014, 2017))+
  # geom_smooth(formula = y ~ splines::bs(x, 3), se = FALSE) +
  gg_sy

cowplot::save_plot("figures/npapers.png", p2, base_height = 10, base_width = 15)
