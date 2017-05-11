##' ---
##' title: "Modelling of Constituency Campaign Spending in the 2015 UK General Election"
##' author: "Todd K. Hartman"
##' date: "`r format(Sys.Date())`"
##' output: "github_document"
##' ---

## rmarkdown::render("UK_Constituency_Spending_and_Polling_v3.R")

##' Housekeeping
## Load packages via 'pacman'
pacman::p_load(cem, dplyr, haven, plyr, readxl, survey, tidyr, zeligverse)

## Set display options
options("scipen" = 100, "digits" = 4)

## Set the working directory
# setwd("C:/users/fs1tkh/Dropbox/_RESEARCH/Pattie_Johnston_2015_UK_Election/Ashcroft_Polling_Project/Data/") # Mac OSX
# setwd("~/Dropbox/_RESEARCH/Pattie_Johnston_2015_UK_Election/Ashcroft_Polling_Project/Data/") # Mac OSX

##' Load the 2015 campaign spending data
## Download (or load if downloaded) from the Electoral Commission website
## http://www.electoralcommission.org.uk/find-information-by-subject/elections-and-referendums/past-elections-and-referendums/uk-general-elections/candidate-election-spending
url.15 <- "https://www.electoralcommission.org.uk/__data/assets/excel_doc/0004/199066/2015-UK-Parliament-spending-data.xlsx"
file.15 <- basename(url.15)  # Extract the filename
if (!file.exists(file.15))   # Only download if not in the working directory
    download.file(url = url.15, destfile = file.15, mode = "wb")
spend.15 <- read_excel(file.15)  # Load the dataset

## Subset the data
names(spend.15)
spend.15.sub <- subset(spend.15, select = c(`ConstituencyId`, `Party Name`, `Spend as % of Short Limit`))
spend.15.sub$ons <- spend.15.sub$`ConstituencyId`
spend.15.sub$party <- spend.15.sub$`Party Name`
spend.15.sub$shortpct15 <- spend.15.sub$`Spend as % of Short Limit`
spend.15.sub$`ConstituencyId` <- spend.15.sub$`Party Name` <- spend.15.sub$`Spend as % of Short Limit` <- NULL
write.csv(spend.15.sub, "UK_2015_LONG.csv", row.names = FALSE)


##' Load the 2010 campaign spending data
## Download (or load if downloaded) from the Electoral Commission website
url.10 <- "http://www.electoralcommission.org.uk/__data/assets/excel_doc/0020/150806/2010-UK-Parliament-spending-data-Excel.xls"
file.10 <- basename(url.10)  # Extract the filename
if (!file.exists(file.10))   # Only download if not in the working directory
    download.file(url = url.10, destfile = file.10, mode = "wb")
spend.10 <- suppressWarnings(read_excel(file.10, sheet = 3))  # Load the dataset

## Subset the data
names(spend.10)
spend.10.sub <- subset(spend.10, select = c(`ConstituencyId`, `Party Name`, `Spend as % of Short Limit`))
spend.10.sub$ons <- spend.10.sub$`ConstituencyId`
spend.10.sub$party <- spend.10.sub$`Party Name`
spend.10.sub$shortpct10 <- spend.10.sub$`Spend as % of Short Limit`
spend.10.sub$`ConstituencyId` <- spend.10.sub$`Party Name` <- spend.10.sub$`Spend as % of Short Limit` <- NULL
write.csv(spend.10.sub, "UK_2010_LONG.csv", row.names = FALSE)


##' Load the 2015 British Election Study constituency contextual results data
## Download (or load if downloaded) from the BES website
url.bes <- "http://www.britishelectionstudy.com/custom/uploads/2017/03/BES-2015-General-Election-results-file-v2.2.xlsx"
file.bes <- basename(url.bes)  # Extract the filename
if (!file.exists(file.bes))   # Only download if not in the working directory
    download.file(url = url.bes, destfile = file.bes, mode = "wb")
bes.wide <- read_excel(file.bes)  # Load the dataset

## Subset the data
names(bes.wide)
bes.wide.sub <- subset(bes.wide, select = c("ONSConstID", "Winner15", "Winner10", "Con15", "Lab15", 
                                            "LD15", "SNP15", "PC15", "UKIP15", "Green15", "Other15", 
                                            "Con10", "Lab10", "LD10", "SNP10", "PC10", 
                                            "UKIP10", "Green10", "BNP10"))

## Reshape BES data from wide to long
names(bes.wide.sub)
colnames(bes.wide.sub) <- c("ons", "winner15", "winner10", "Conservative.v2015", "Labour.v2015", 
                            "LibDem.v2015", "SNP.v2015", "PC.v2015", "UKIP.v2015", "Green.v2015", 
                            "Other.v2015", "Conservative.v2010", "Labour.v2010", "LibDem.v2010", 
                            "SNP.v2010", "PC.v2010", "UKIP.v2010", "Green.v2010", "Other.v2010")

bes.long <- 
    bes.wide.sub %>%
    gather(key, pct, -ons, -winner15, -winner10) %>%
    separate(key, into = c("party", "year"), sep = "\\.") %>%
    spread(year, pct)

write.csv(bes.long, "UK_BES_LONG.csv", row.names = FALSE)


##' Load the 2015 British Election Study Wave 1 Internet panel data
## Download (or load if downloaded) from the BES website
url.bes2 <- "http://www.britishelectionstudy.com/custom/uploads/2015/07/BES2015_W1_v5.0.dta"
file.bes2 <- basename(url.bes2)  # Extract the filename
if (!file.exists(file.bes2))   # Only download if not in the working directory
    download.file(url = url.bes2, destfile = file.bes2, mode = "wb")
bes.panel <- read_dta(file.bes2)  # Load the dataset

## Subset the data
names(bes.panel)
bes.panel.sub <- subset(bes.panel, select = c("onscode", "wt_full_W1", "generalElectionVote"))
colnames(bes.panel.sub) <- c("ons", "weight", "intention")
bes.panel.sub <- na.omit(bes.panel.sub)
bes.panel.sub$con.intent <- ifelse(bes.panel.sub$intention == 1, 1, 0)
bes.panel.sub$lab.intent <- ifelse(bes.panel.sub$intention == 2, 1, 0)
bes.panel.sub$ld.intent <- ifelse(bes.panel.sub$intention == 3, 1, 0)

## Generate constituency-level vote intetion
svy.weight <- svydesign(ids = ~1, data = bes.panel.sub, weights = bes.panel.sub$weight)
con.intent.pct <- svyby(~con.intent, by = ~ons, svymean, design = svy.weight)
lab.intent.pct <- svyby(~lab.intent, by = ~ons, svymean, design = svy.weight)
ld.intent.pct <- svyby(~ld.intent, by = ~ons, svymean, design = svy.weight)

vote.intent <- merge(con.intent.pct, lab.intent.pct, by = "ons")
vote.intent <- merge(vote.intent, ld.intent.pct, by = "ons")

vote.intent$con.intent <- vote.intent$con.intent*100
vote.intent$lab.intent <- vote.intent$lab.intent*100
vote.intent$ld.intent <- vote.intent$ld.intent*100

names(vote.intent)
vote.intent$se.x <- vote.intent$se.y <- vote.intent$se <- NULL

write.csv(vote.intent, "BES_Vote_Intention_Wave_1_2014.csv", row.names = FALSE)


##' Load the 2014-2015 Ashcroft polling data
## Download (or load if downloaded) from Github
url.polls <- "https://raw.githubusercontent.com/tkhartman/replication-ashcroft-polling-campaign-spending/master/Ashcroft_Poll_Results_Final.csv"
file.polls <- basename(url.polls)  # Extract the filename
if (!file.exists(file.polls))   # Only download if not in the working directory
  download.file(url = url.polls, destfile = file.polls, mode = "wb")
polls.wide <- read.csv(file.polls) # Author coded file

## Reshape Ashcroft data from wide to long
names(polls.wide)
polls.long <- 
    reshape(polls.wide, 
            direction = "long",
            varying = list(c(4:10), c(12:18), c(20:26), c(28:34)), 
            timevar = "party",
            v.names=c("poll.1", "poll.2", "poll.3", "poll.4"),
            times = c("Conservative", "Labour", "LibDem", "SNP", "UKIP", "Green", "Other"))
row.names(polls.long) <- polls.long$id <- NULL
polls.long$ons <- polls.long$ONSConstID
polls.long$ONSConstID <- NULL
write.csv(polls.long, "Ashcroft_LONG.csv")

##' Merge datasets
## Remove extraneous parties from the 2015 spending data
table(spend.15.sub$party)
party.15.keep <- c("Conservative Party", "Green Party", "Labour Party",
                "Labour Party / Co-operative Party", "Liberal Democrats", "Plaid Cymru - The Party of Wales",
                "Scottish National Party (SNP)", "UK Independence Party (UKIP)")

spend.15.sub <- subset(spend.15.sub, party %in% party.15.keep)

## Remove extraneous parties from the 2010 spending data
table(spend.10.sub$party)
party.10.keep <- c("Conservative and Unionist Party", "Green Party", "Labour Party", 
                  "Labour Party/Co-operative Party", "Liberal Democrats", "Plaid Cymru - The Party of Wales",
                   "Scottish National Party (SNP)", "UK Independence Party (UK I P)")

spend.10.sub <- subset(spend.10.sub, party %in% party.10.keep)

## Make party names consistent for merging
party.15.names <- c("Conservative", "Green", "Labour", "Labour", "LibDem", "PC", "SNP", "UKIP")
party.10.names <- c("Conservative", "Green", "Labour", "Labour", "LibDem", "PC", "SNP", "UKIP")

spend.15.sub$party <- mapvalues(spend.15.sub$party, from = party.15.keep, to = party.15.names)
spend.10.sub$party <- mapvalues(spend.10.sub$party, from = party.10.keep, to = party.10.names)

## Merge 2015 and 2010 spending data
f <- left_join(spend.15.sub, spend.10.sub, by = c("ons", "party"))

## Merge spending data with BES election results
ff <- left_join(bes.long, f, by = c("ons", "party"))

## Merge spending and election results with vote intention data
fff <- left_join(ff, vote.intent, by = "ons")

## Merge spending, election, and vote intention data with Ashcroft results
df <- left_join(fff, polls.long, by = c("ons", "party"))
write.csv(df, "Spending_Data_Merged_Final_May_2017.csv", row.names = FALSE)
     

############################################################################
##' Clear the workspace and start anew
rm(list=ls())
df <- read.csv("Spending_Data_Merged_Final_May_2017.csv")


##' Generate 2010 marginality score
## Find the highest vote share in 2010
df <- merge(df, aggregate(v2010 ~ ons, data = df, max),
                    by = "ons", suffixes = c("", ".1st"))

## Second highest vote share in 2010
df <- merge(df, aggregate(v2010 ~ ons, data = df, function(x){sort(na.omit(x), decreasing=T)[2]}),
            by = "ons", suffixes = c("", ".2nd"))

## 2010 marginality scores (absolute value)
df$con.margin10 <- ifelse(df$party == "Conservative", 
                          abs(ifelse(df$winner10 == "Conservative", df$v2010 - df$v2010.2nd, df$v2010 - df$v2010.1st)),
                          NA)

df$lab.margin10 <- ifelse(df$party == "Labour", 
                          abs(ifelse(df$winner10 == "Labour", df$v2010 - df$v2010.2nd, df$v2010 - df$v2010.1st)),
                          NA)

df$ld.margin10 <- ifelse(df$party == "LibDem", 
                         abs(ifelse(df$winner10 == "Liberal Democrat", df$v2010 - df$v2010.2nd, df$v2010 - df$v2010.1st)),
                         NA)

summary(df$con.margin10)
summary(df$lab.margin10)
summary(df$ld.margin10)

##' Generate 2015 constituency marginality measures based on Ashcroft polls
## Find the Ashcroft poll leader in 2015
df <- merge(df, aggregate(poll.1 ~ ons, data = df, max),
            by = "ons", suffixes = c("", ".1st"), all = TRUE)

## Ashcroft poll runner-up in 2015
df <- merge(df, aggregate(poll.1 ~ ons, data = df, function(x){sort(na.omit(x), decreasing=T)[2]}),
            by = "ons", suffixes = c("", ".2nd"), all = TRUE)

## Name the party leader in the most recent Ashcroft poll
poll.leader <- df %>% group_by(ons) %>% slice(which.max(poll.1)) %>% subset(select = c("ons", "party"))
poll.leader$poll.leader <- poll.leader$party
poll.leader$party <- NULL
df <- left_join(df, poll.leader, by = "ons")
rm(poll.leader)

## 2015 marginality scores (positive = leading in the latest poll)
df$con.margin15 <- ifelse(df$party == "Conservative", 
                          abs(ifelse(df$poll.leader == "Conservative", df$poll.1 - df$poll.1.2nd, df$poll.1 - df$poll.1.1st)),
                          NA)

df$lab.margin15 <- ifelse(df$party == "Labour", 
                          abs(ifelse(df$poll.leader == "Labour", df$poll.1 - df$poll.1.2nd, df$poll.1 - df$poll.1.1st)),
                          NA)

df$ld.margin15 <- ifelse(df$party == "LibDem", 
                         abs(ifelse(df$poll.leader == "LibDem", df$poll.1 - df$poll.1.2nd, df$poll.1 - df$poll.1.1st)),
                         NA)

summary(df$con.margin15)
summary(df$lab.margin15)
summary(df$ld.margin15)

## Ashcroft poll conducted?
df$ashcroft <- ifelse(df$poll.1.1st > 0, 1, 0)
df$ashcroft[is.na(df$ashcroft)] <- 0


##' Subset the merged data by party
con <- subset(df, party == "Conservative")
lab <- subset(df, party == "Labour")
ld <- subset(df, party == "LibDem")

##' Descriptive statistics for campaign spending
## Percent of 2015 short campaign constituency spending
summary(con$shortpct15)
summary(lab$shortpct15)
summary(ld$shortpct15)

## Potential 2015 missing data
length(con$shortpct15[con$shortpct15 == "0"]) 
length(lab$shortpct15[lab$shortpct15 == "0"]) 
length(ld$shortpct15[ld$shortpct15 == "0"])

## 2015 Spending data, removing 0s (which may be missing or true 0; cannot tell)
con$short15 <- ifelse(con$shortpct15 > 0, con$shortpct15, NA)
lab$short15 <- ifelse(lab$shortpct15 > 0, lab$shortpct15, NA)
ld$short15 <- ifelse(ld$shortpct15 > 0, ld$shortpct15, NA)

summary(con$short15)
summary(lab$short15)
summary(ld$short15)

## Percent of 2010 short campaign constituency spending
summary(con$shortpct10)
summary(lab$shortpct10)
summary(ld$shortpct10)

## 2010 Spending data 
con$short10 <- con$shortpct10
lab$short10 <- lab$shortpct10
ld$short10 <- ld$shortpct10 

summary(con$short10)
summary(lab$short10)
summary(ld$short10)

## 2010 election winner (dummy)
con$win10 <- ifelse(con$winner10 == "Conservative", 1, 0) 
lab$win10 <- ifelse(lab$winner10 == "Labour", 1, 0) 
ld$win10 <- ifelse(ld$winner10 == "Liberal Democrat", 1, 0) 

summary(con$win10)
summary(lab$win10)
summary(ld$win10)

## 2015 poll leader (dummy)
con$lead15 <- ifelse(con$poll.leader == "Conservative", 1, 0) 
lab$lead15 <- ifelse(lab$poll.leader == "Labour", 1, 0) 
ld$lead15 <- ifelse(ld$poll.leader == "LibDem", 1, 0) 

summary(con$lead15)
summary(lab$lead15)
summary(ld$lead15)


##' Results
## Conservative spending models
con.m1 <- lm(short15 ~ short10 + win10*con.margin10, data = con)                                       # Prior information only
con.m2 <- lm(short15 ~ short10 + win10*con.margin10 + con.intent + ashcroft, data = con)               # Plus Ashcroft poll
con.m3 <- lm(short15 ~ short10 + win10*con.margin10 + con.intent + con.margin15, data = con)           # Plus Ashcroft marginals

summary(con.m1)
summary(con.m2)
summary(con.m3)

## Labour spending models
lab.m1 <- lm(short15 ~ short10 + win10*lab.margin10, data = lab)                                       # Prior information only
lab.m2 <- lm(short15 ~ short10 + win10*lab.margin10 + lab.intent + ashcroft, data = lab)               # Plus Ashcroft poll
lab.m3 <- lm(short15 ~ short10 + win10*lab.margin10 + lab.intent + lab.margin15, data = lab)           # Plus Ashcroft marginals

summary(lab.m1)
summary(lab.m2)
summary(lab.m3)

## Liberal Democratic spending models
ld.m1 <- lm(short15 ~ short10 + win10*ld.margin10, data = ld)                                          # Prior information only
ld.m2 <- lm(short15 ~ short10 + win10*ld.margin10 + ld.intent + ashcroft, data = ld)                   # Plus Ashcroft poll
ld.m3 <- lm(short15 ~ short10 + win10*ld.margin10 + ld.intent + ld.margin15, data = ld)                # Plus Ashcroft marginals

summary(ld.m1)
summary(ld.m2)
summary(ld.m3)

## Table 1 - Prior information only
summary(con.m1)
summary(lab.m1)
summary(ld.m1)

## Table 2 - Prior information plus presence of Ashcroft poll and BES vote intention
summary(con.m2)
summary(lab.m2)
summary(ld.m2)

## Table 3 - Prior information plus Ashcroft poll marginals and BES vote intention
summary(con.m3)
summary(lab.m3)
summary(ld.m3)

##' Perform robustness checks using Coarsened Exact Matching (from the 'cem' package)
## Conservative Party matched data and results
con1 <- subset(con, select = c("short15", "short10", "win10", "con.margin10", "con.intent", "ashcroft"))
con1 <- na.omit(con1)
con.match <- cem(treatment = "ashcroft", data = con1, drop = "short15")
con.est <- att(con.match, short15 ~ short10 + win10*con.margin10 + con.intent + ashcroft, data = con1, model = "linear")
summary(con.est)
con.est

## Labour Party matched data and results
lab1 <- subset(lab, select = c("short15", "short10", "win10", "lab.margin10", "lab.intent", "ashcroft"))
lab1 <- na.omit(lab1)
lab.match <- cem(treatment = "ashcroft", data = lab1, drop = "short15")
lab.est <- att(lab.match, short15 ~ short10 + win10*lab.margin10 + lab.intent + ashcroft, data = lab1, model = "linear")
summary(lab.est)
lab.est

## Liberal Democrat Party matched data and results
ld1 <- subset(ld, select = c("short15", "short10", "win10", "ld.margin10", "ld.intent", "ashcroft"))
ld1 <- na.omit(ld1)
ld.match <- cem(treatment = "ashcroft", data = ld1, drop = "short15")
ld.est <- att(ld.match, short15 ~ short10 + win10*ld.margin10 + ld.intent + ashcroft, data = ld1, model = "linear")
summary(ld.est)
ld.est


