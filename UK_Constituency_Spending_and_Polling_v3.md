Modelling of Constituency Campaign Spending in the 2015 UK General Election
================
Todd K. Hartman
2017-05-10

``` r
## rmarkdown::render("UK_Constituency_Spending_and_Polling_v3.R")
```

Housekeeping

``` r
## Load packages via 'pacman'
pacman::p_load(dplyr, haven, plyr, readxl, survey, tidyr, zeligverse)

## Set display options
options("scipen" = 100, "digits" = 4)
```

Load the 2015 campaign spending data

``` r
## Download (or load if downloaded) from the Electoral Commission website
## http://www.electoralcommission.org.uk/find-information-by-subject/elections-and-referendums/past-elections-and-referendums/uk-general-elections/candidate-election-spending
url.15 <- "https://www.electoralcommission.org.uk/__data/assets/excel_doc/0004/199066/2015-UK-Parliament-spending-data.xlsx"
file.15 <- basename(url.15)  # Extract the filename
if (!file.exists(file.15))   # Only download if not in the working directory
    download.file(url = url.15, destfile = file.15, mode = "wb")
spend.15 <- read_excel(file.15)  # Load the dataset

## Subset the data
names(spend.15)
```

    ##  [1] "ConstituencyId"                     
    ##  [2] "EntityId"                           
    ##  [3] "CandId"                             
    ##  [4] "Forename"                           
    ##  [5] "Surname"                            
    ##  [6] "Party Name"                         
    ##  [7] "Constituency Name"                  
    ##  [8] "Constituency Type"                  
    ##  [9] "Area"                               
    ## [10] "Nation"                             
    ## [11] "Electorate (Limit)"                 
    ## [12] "Long Limit"                         
    ## [13] "Short Limit"                        
    ## [14] "Long Total Spend"                   
    ## [15] "Short Total Spend"                  
    ## [16] "Spend % of Long Limit"              
    ## [17] "Spend as % of Short Limit"          
    ## [18] "Long 3a Unpaid"                     
    ## [19] "Long 3a Disputed"                   
    ## [20] "Long 3a Notional"                   
    ## [21] "Long 3a Payments"                   
    ## [22] "Long 3a Total"                      
    ## [23] "Long 3b Advertising"                
    ## [24] "Long 3b Unsolicited"                
    ## [25] "Long 3b Transport"                  
    ## [26] "Long 3b Public Meetings"            
    ## [27] "Long 3b Agent and Staff"            
    ## [28] "Long 3b Accommodation"              
    ## [29] "Long 3b Total"                      
    ## [30] "Long Personal Expenses"             
    ## [31] "Long Candidate Money"               
    ## [32] "Long Donations Accepted (Over £50)" 
    ## [33] "Long Donations Rejected"            
    ## [34] "Short 3a Unpaid"                    
    ## [35] "Short 3a Disputed"                  
    ## [36] "Short 3a Notional"                  
    ## [37] "Short 3a Payments"                  
    ## [38] "Short 3a Total"                     
    ## [39] "Short 3b Advertising"               
    ## [40] "Short 3b Unsolicited"               
    ## [41] "Short 3b Transport"                 
    ## [42] "Short 3b Public Meetings"           
    ## [43] "Short 3b Agent and Staff"           
    ## [44] "Short 3b Accommodation"             
    ## [45] "Short 3b Total"                     
    ## [46] "Short Personal Expenses"            
    ## [47] "Short Candidate Money"              
    ## [48] "Short Donations Accepted (Over £50)"
    ## [49] "Short Donations Rejected"           
    ## [50] "EntityJointID"

``` r
spend.15.sub <- subset(spend.15, select = c(`ConstituencyId`, `Party Name`, `Spend as % of Short Limit`))
spend.15.sub$ons <- spend.15.sub$`ConstituencyId`
spend.15.sub$party <- spend.15.sub$`Party Name`
spend.15.sub$shortpct15 <- spend.15.sub$`Spend as % of Short Limit`
spend.15.sub$`ConstituencyId` <- spend.15.sub$`Party Name` <- spend.15.sub$`Spend as % of Short Limit` <- NULL
write.csv(spend.15.sub, "UK_2015_LONG.csv", row.names = FALSE)
```

Load the 2010 campaign spending data

``` r
## Download (or load if downloaded) from the Electoral Commission website
url.10 <- "http://www.electoralcommission.org.uk/__data/assets/excel_doc/0020/150806/2010-UK-Parliament-spending-data-Excel.xls"
file.10 <- basename(url.10)  # Extract the filename
if (!file.exists(file.10))   # Only download if not in the working directory
    download.file(url = url.10, destfile = file.10, mode = "wb")
spend.10 <- suppressWarnings(read_excel(file.10, sheet = 3))  # Load the dataset
```

    ## DEFINEDNAME: 21 00 00 01 0b 00 00 00 03 00 00 00 00 00 00 0d 3b 00 00 00 00 c0 0f 00 00 33 00 
    ## DEFINEDNAME: 21 00 00 01 0b 00 00 00 03 00 00 00 00 00 00 0d 3b 00 00 00 00 c0 0f 00 00 33 00 
    ## DEFINEDNAME: 21 00 00 01 0b 00 00 00 03 00 00 00 00 00 00 0d 3b 00 00 00 00 c0 0f 00 00 33 00 
    ## DEFINEDNAME: 21 00 00 01 0b 00 00 00 03 00 00 00 00 00 00 0d 3b 00 00 00 00 c0 0f 00 00 33 00

``` r
## Subset the data
names(spend.10)
```

    ##  [1] "ConstituencyId"                     
    ##  [2] "EntityId"                           
    ##  [3] "CandId"                             
    ##  [4] "Forename"                           
    ##  [5] "Surname"                            
    ##  [6] "Party Name"                         
    ##  [7] "Constituency Name"                  
    ##  [8] "Constituency Type"                  
    ##  [9] "Area"                               
    ## [10] "Nation"                             
    ## [11] "Votes"                              
    ## [12] "Votes %"                            
    ## [13] "Position"                           
    ## [14] "Electorate (Limit)"                 
    ## [15] "Long Limit"                         
    ## [16] "Short Limit"                        
    ## [17] "Long Total Spend"                   
    ## [18] "Short Total Spend"                  
    ## [19] "Spend as % of Long Limit"           
    ## [20] "Spend as % of Short Limit"          
    ## [21] "Long 3a Unpaid"                     
    ## [22] "Long 3a Disputed"                   
    ## [23] "Long 3a Notional"                   
    ## [24] "Long 3a Payments"                   
    ## [25] "Long 3a Total"                      
    ## [26] "Long 3b Advertising"                
    ## [27] "Long 3b Unsolicited"                
    ## [28] "Long 3b Transport"                  
    ## [29] "Long 3b Public Meetings"            
    ## [30] "Long 3b Agent and Staff"            
    ## [31] "Long 3b Accommodation"              
    ## [32] "Long 3b Total"                      
    ## [33] "Long Personal Expenses"             
    ## [34] "Long Candidate Money"               
    ## [35] "Long Donations Accepted (Over £50)" 
    ## [36] "Long Donations Rejected"            
    ## [37] "Short 3a Unpaid"                    
    ## [38] "Short 3a Disputed"                  
    ## [39] "Short 3a Notional"                  
    ## [40] "Short 3a Payments"                  
    ## [41] "Short 3a Total"                     
    ## [42] "Short 3b Advertising"               
    ## [43] "Short 3b Unsolicited"               
    ## [44] "Short 3b Transport"                 
    ## [45] "Short 3b Public Meetings"           
    ## [46] "Short 3b Agent and Staff"           
    ## [47] "Short 3b Accommodation"             
    ## [48] "Short 3b Total"                     
    ## [49] "Short Personal Expenses"            
    ## [50] "Short Candidate Money"              
    ## [51] "Short Donations Accepted (Over £50)"
    ## [52] "Short Donations Rejected"

``` r
spend.10.sub <- subset(spend.10, select = c(`ConstituencyId`, `Party Name`, `Spend as % of Short Limit`))
spend.10.sub$ons <- spend.10.sub$`ConstituencyId`
spend.10.sub$party <- spend.10.sub$`Party Name`
spend.10.sub$shortpct10 <- spend.10.sub$`Spend as % of Short Limit`
spend.10.sub$`ConstituencyId` <- spend.10.sub$`Party Name` <- spend.10.sub$`Spend as % of Short Limit` <- NULL
write.csv(spend.10.sub, "UK_2010_LONG.csv", row.names = FALSE)
```

Load the 2015 British Election Study constituency contextual results data

``` r
## Download (or load if downloaded) from the BES website
url.bes <- "http://www.britishelectionstudy.com/custom/uploads/2017/03/BES-2015-General-Election-results-file-v2.2.xlsx"
file.bes <- basename(url.bes)  # Extract the filename
if (!file.exists(file.bes))   # Only download if not in the working directory
    download.file(url = url.bes, destfile = file.bes, mode = "wb")
bes.wide <- read_excel(file.bes)  # Load the dataset

## Subset the data
names(bes.wide)
```

    ##   [1] "pano"                             "ONSConstID"                      
    ##   [3] "ConstituencyName"                 "Country"                         
    ##   [5] "Region"                           "ConstituencyType"                
    ##   [7] "Winner15"                         "Con15"                           
    ##   [9] "Lab15"                            "LD15"                            
    ##  [11] "SNP15"                            "PC15"                            
    ##  [13] "UKIP15"                           "Green15"                         
    ##  [15] "Other15"                          "Majority15"                      
    ##  [17] "Turnout15"                        "ConVote15"                       
    ##  [19] "LabVote15"                        "LDVote15"                        
    ##  [21] "SNPVote15"                        "PCVote15"                        
    ##  [23] "UKIPVote15"                       "GreenVote15"                     
    ##  [25] "BNPVote15"                        "TotalVote15"                     
    ##  [27] "RejectedVote15"                   "Electorate15"                    
    ##  [29] "SeatChange1015"                   "Con1015"                         
    ##  [31] "Lab1015"                          "LD1015"                          
    ##  [33] "SNP1015"                          "PC1015"                          
    ##  [35] "UKIP1015"                         "Green1015"                       
    ##  [37] "Winner10"                         "Con10"                           
    ##  [39] "Lab10"                            "LD10"                            
    ##  [41] "SNP10"                            "PC10"                            
    ##  [43] "UKIP10"                           "Green10"                         
    ##  [45] "BNP10"                            "Majority10"                      
    ##  [47] "Turn10"                           "ConVote10"                       
    ##  [49] "LabVote10"                        "LDVote10"                        
    ##  [51] "SNPVote10"                        "PCVote10"                        
    ##  [53] "UKIPVote10"                       "GreenVote10"                     
    ##  [55] "BNPVote10"                        "TotalVote10"                     
    ##  [57] "Electorate10"                     "ConPPC"                          
    ##  [59] "ConPPCsex"                        "ConPPCrace"                      
    ##  [61] "LabPPC"                           "LabPPCsex"                       
    ##  [63] "LabPPCrace"                       "LDPPC"                           
    ##  [65] "LDPPCsex"                         "LDPPCrace"                       
    ##  [67] "UKIPPPC"                          "UKIPPPCsex"                      
    ##  [69] "UKIPPPCrace"                      "SNPPPC"                          
    ##  [71] "SNPPPCsex"                        "SNPPPCrace"                      
    ##  [73] "PCPPC"                            "PCPPCsex"                        
    ##  [75] "PCPPCrace"                        "GreenPPC"                        
    ##  [77] "GreenPPCsex"                      "GreenPPCrace"                    
    ##  [79] "c11Population"                    "c11PopulationDensity"            
    ##  [81] "c11Male"                          "c11Female"                       
    ##  [83] "c11Households"                    "c11Communal"                     
    ##  [85] "c11Age0to4"                       "c11Age5to7"                      
    ##  [87] "c11Age8to9"                       "c11Age10to14"                    
    ##  [89] "c11Age15"                         "c11Age16to17"                    
    ##  [91] "c11Age18to19"                     "c11Age20to24"                    
    ##  [93] "c11Age25to29"                     "c11Age30to44"                    
    ##  [95] "c11Age45to59"                     "c11Age60to64"                    
    ##  [97] "c11Age65to74"                     "c11Age75to84"                    
    ##  [99] "c11Age85to89"                     "c11Age90plus"                    
    ## [101] "c11HouseOwned"                    "c11HouseOutright"                
    ## [103] "c11HouseMortgage"                 "c11HouseShared"                  
    ## [105] "c11HouseSocial"                   "c11HouseSocialLA"                
    ## [107] "c11HouseSocialOther"              "c11HousePrivate"                 
    ## [109] "c11HousePrivateLandlord"          "c11HousePrivateOther"            
    ## [111] "c11HouseRentFree"                 "c11HouseholdOnePerson"           
    ## [113] "c11HouseholdOnePerson65plus"      "c11HouseholdOnePersonOther"      
    ## [115] "c11HouseholdOneFamily"            "c11HouseholdOneFamily65plus"     
    ## [117] "c11HouseholdMarried"              "c11HouseholdMarriedNoChildren"   
    ## [119] "c11HouseholdMarriedDependents"    "c11HouseholdMarriedNondependents"
    ## [121] "c11HouseholdCohabit"              "c11HouseholdCohabitNoChildren"   
    ## [123] "c11HouseholdCohabitDependents"    "c11HouseholdCohabitNodependents" 
    ## [125] "c11HouseholdLone"                 "c11HouseholdLoneDependents"      
    ## [127] "c11HouseholdLoneNodependents"     "c11HouseholdOther"               
    ## [129] "c11HouseholdOtherDependents"      "c11HouseholdAllStudents"         
    ## [131] "c11HouseholdAll65plus"            "c11HouseholdAnyOther"            
    ## [133] "c11CarsNone"                      "c11CarsOne"                      
    ## [135] "c11CarsTwo"                       "c11CarsThree"                    
    ## [137] "c11CarsFour"                      "c11EthnicityWhite"               
    ## [139] "c11EthnicityMixed"                "c11EthnicityAsian"               
    ## [141] "c11EthnicityBlack"                "c11EthnicityOther"               
    ## [143] "c11EthnicityWhiteBritish"         "c11EthnicityWhiteIrish"          
    ## [145] "c11EthnicityWhiteTraveller"       "c11EthnicityWhiteOther"          
    ## [147] "c11EthnicityMixedCaribbean"       "c11EthnicityMixedAfrican"        
    ## [149] "c11EthnicityMixedAsian"           "c11EthnicityMixedOther"          
    ## [151] "c11EthnicityIndian"               "c11EthnicityPakistani"           
    ## [153] "c11EthnicityBangladeshi"          "c11EthnicityChinese"             
    ## [155] "c11EthnicityOtherAsian"           "c11EthnicityBlackAfrican"        
    ## [157] "c11EthnicityBlackCaribbean"       "c11EthnicityBlackOther"          
    ## [159] "c11EthnicityArab"                 "c11EthnicityAnyOther"            
    ## [161] "c11BornUK"                        "c11BornEngland"                  
    ## [163] "c11BornNI"                        "c11BornScotland"                 
    ## [165] "c11BornWales"                     "c11BornUKNotSpecified"           
    ## [167] "c11BornIreland"                   "c11BornOtherEU"                  
    ## [169] "c11BornOtherPre2004EU"            "c11BornPost2004EU"               
    ## [171] "c11BornOther"                     "c11PassportNone"                 
    ## [173] "c11PassportAny"                   "c11PassportUK"                   
    ## [175] "c11PassportIreland"               "c11PassportEU"                   
    ## [177] "c11PassportEuropeNotEU"           "c11PassportAfrica"               
    ## [179] "c11PassportAsia"                  "c11PassportNorthAmerica"         
    ## [181] "c11PassportCentralAmerica"        "c11PassportSouthAmerica"         
    ## [183] "c11PassportOceania"               "c11EnglishAll"                   
    ## [185] "c11EnglishOne"                    "c11EnglishChild"                 
    ## [187] "c11EnglishNone"                   "c11Christian"                    
    ## [189] "c11Buddhist"                      "c11Hindu"                        
    ## [191] "c11Jewish"                        "c11Muslim"                       
    ## [193] "c11Sikh"                          "c11ReligionOther"                
    ## [195] "c11NoReligion"                    "c11ReligionNotStated"            
    ## [197] "c11NSSECHigherManager"            "c11NSSECHigherProfessional"      
    ## [199] "c11NSSECLowerManager"             "c11NSSECIntermediate"            
    ## [201] "c11NSSECSmallEmployer"            "c11NSSECLowerSupervisor"         
    ## [203] "c11NSSECSemiRoutine"              "c11NSSECRoutine"                 
    ## [205] "c11NSSECNeverWorked"              "c11NSSECLongtermUnemployed"      
    ## [207] "c11EconomicActive"                "c11Employed"                     
    ## [209] "c11EmployedPartTime"              "c11EmployedFullTime"             
    ## [211] "c11SelfEmployed"                  "c11Unemployed"                   
    ## [213] "c11EconomicallyActiveStudent"     "c11EconomicInactive"             
    ## [215] "c11Retired"                       "c11EconomicallyInactiveStudent"  
    ## [217] "c11LookingAfterHome"              "c11LongTermSick"                 
    ## [219] "c11EconomicInactiveOther"         "c11Unemployed16to24"             
    ## [221] "c11Unemployed50to74"              "c11Neverworked"                  
    ## [223] "c11LongTermUnemployed"            "c11FulltimeStudent"              
    ## [225] "c11IndustryAgriculture"           "c11IndustryMining"               
    ## [227] "c11IndustryManufacturing"         "c11IndustryElectricitySupply"    
    ## [229] "c11IndustryWaterSupply"           "c11IndustryConstruction"         
    ## [231] "c11IndustryWholesale"             "c11IndustryTransport"            
    ## [233] "c11IndustryAccommodation"         "c11IndustryCommunication"        
    ## [235] "c11IndustryFinance"               "c11IndustryRealEstate"           
    ## [237] "c11IndustryProfessional"          "c11IndustryAdministrative"       
    ## [239] "c11IndustryPublicAdministration"  "c11IndustryEducation"            
    ## [241] "c11IndustrySocialWork"            "c11IndustryOther"                
    ## [243] "c11QualNone"                      "c11QualLevel1"                   
    ## [245] "c11QualLevel2"                    "c11QualApprentice"               
    ## [247] "c11QualLevel3"                    "c11QualLevel4"                   
    ## [249] "c11QualOther"                     "c11Degree"                       
    ## [251] "c11HealthVeryGood"                "c11HealthGood"                   
    ## [253] "c11HealthFair"                    "c11HealthBad"                    
    ## [255] "c11HealthVeryBad"                 "c11NoAdultsEmployed"             
    ## [257] "c11NoAdultsEmployedChildren"      "c11NoAdultsEmployedNoChildren"   
    ## [259] "c11DeprivedNone"                  "c11Deprived1"                    
    ## [261] "c11Deprived2"                     "c11Deprived3"                    
    ## [263] "c11Deprived4"                     "ConLongSpendPercent"             
    ## [265] "ConShortSpendPercent"             "LabLongSpendPercent"             
    ## [267] "LabShortSpendPercent"             "LDLongSpendPercent"              
    ## [269] "LDShortSpendPercent"              "SNPLongSpendPercent"             
    ## [271] "SNPShortSpendPercent"             "PCLongSpendPercent"              
    ## [273] "PCShortSpendPercent"              "UKIPLongSpendPercent"            
    ## [275] "UKIPShortSpendPercent"            "GreenLongSpendPercent"           
    ## [277] "GreenShortSpendPercent"

``` r
bes.wide.sub <- subset(bes.wide, select = c("ONSConstID", "Winner15", "Winner10", "Con15", "Lab15", 
                                            "LD15", "SNP15", "PC15", "UKIP15", "Green15", "Other15", 
                                            "Con10", "Lab10", "LD10", "SNP10", "PC10", 
                                            "UKIP10", "Green10", "BNP10"))

## Reshape BES data from wide to long
names(bes.wide.sub)
```

    ##  [1] "ONSConstID" "Winner15"   "Winner10"   "Con15"      "Lab15"     
    ##  [6] "LD15"       "SNP15"      "PC15"       "UKIP15"     "Green15"   
    ## [11] "Other15"    "Con10"      "Lab10"      "LD10"       "SNP10"     
    ## [16] "PC10"       "UKIP10"     "Green10"    "BNP10"

``` r
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
```

Load the 2015 British Election Study Wave 1 Internet panel data

``` r
## Download (or load if downloaded) from the BES website
url.bes2 <- "http://www.britishelectionstudy.com/custom/uploads/2015/07/BES2015_W1_v5.0.dta"
file.bes2 <- basename(url.bes2)  # Extract the filename
if (!file.exists(file.bes2))   # Only download if not in the working directory
    download.file(url = url.bes2, destfile = file.bes2, mode = "wb")
bes.panel <- read_dta(file.bes2)  # Load the dataset

## Subset the data
names(bes.panel)
```

    ##   [1] "id"                               "wt_core_W1"                      
    ##   [3] "wave1"                            "country"                         
    ##   [5] "starttime"                        "endtime"                         
    ##   [7] "wt_full_W1"                       "ordering"                        
    ##   [9] "pidfront"                         "vtfront"                         
    ##  [11] "getsBrandenburg"                  "getsEUTT"                        
    ##  [13] "getsHuddy"                        "getsPTV"                         
    ##  [15] "getsRedistTT"                     "getsTT"                          
    ##  [17] "playground"                       "condition"                       
    ##  [19] "pledgeRand"                       "eduChoice"                       
    ##  [21] "RandomID"                         "ukCitizen"                       
    ##  [23] "euCitizen"                        "commonwealthCitizen"             
    ##  [25] "otherCitizen"                     "mii"                             
    ##  [27] "MII_text"                         "bestOnMII"                       
    ##  [29] "bestOnMIIOth"                     "turnoutUKGeneral"                
    ##  [31] "generalElectionVote"              "generalElectionVoteOth"          
    ##  [33] "generalElectionCertainty"         "partyIdStrength"                 
    ##  [35] "partyId"                          "partyIdSqueeze"                  
    ##  [37] "partyIdOth"                       "polAttention"                    
    ##  [39] "likeCameron"                      "likeMiliband"                    
    ##  [41] "likeClegg"                        "likeSalmond"                     
    ##  [43] "likeWood"                         "likeFarage"                      
    ##  [45] "thatcherGood"                     "blairGood"                       
    ##  [47] "goodTimePurchase"                 "riskPoverty"                     
    ##  [49] "riskUnemployment"                 "econPersonalRetro"               
    ##  [51] "econGenRetro"                     "likeCon"                         
    ##  [53] "likeLab"                          "likeLD"                          
    ##  [55] "likeSNP"                          "likePC"                          
    ##  [57] "likeUKIP"                         "likeGrn"                         
    ##  [59] "likeBNP"                          "ptvCon"                          
    ##  [61] "ptvLab"                           "ptvLD"                           
    ##  [63] "ptvSNP"                           "ptvPC"                           
    ##  [65] "ptvUKIP"                          "ptvGrn"                          
    ##  [67] "ptvBNP"                           "responsibleDebtConservatives"    
    ##  [69] "responsibleDebtLiberalDems"       "responsibleDebtLabour"           
    ##  [71] "responsibleDebtUKBanks"           "responsibleDebtGlobalBanks"      
    ##  [73] "responsibleDebtTradeUnions"       "responsibleDebtEU"               
    ##  [75] "responsibleDebtOther"             "responsibleDebtNone"             
    ##  [77] "responsibleDebtDontKnow"          "cutsTooFarNational"              
    ##  [79] "cutsTooFarNHS"                    "cutsTooFarLocal"                 
    ##  [81] "privatTooFar"                     "enviroProtection"                
    ##  [83] "changeEconomy"                    "changeNHS"                       
    ##  [85] "changeEducation"                  "changeCostLive"                  
    ##  [87] "changeImmig"                      "changeCrime"                     
    ##  [89] "responsibleEconConservatives"     "responsibleEconLibDems"          
    ##  [91] "responsibleEconLabour"            "responsibleEconScottishGovt"     
    ##  [93] "responsibleEconWelshGovt"         "responsibleEconNone"             
    ##  [95] "responsibleEconDontKnow"          "responsibleNHSConservatives"     
    ##  [97] "responsibleNHSLibDems"            "responsibleNHSLabour"            
    ##  [99] "responsibleNHSScottishGovt"       "responsibleNHSWelshGovt"         
    ## [101] "responsibleNHSNone"               "responsibleNHSDontKnow"          
    ## [103] "responsibleEducationCon"          "responsibleEducationLD"          
    ## [105] "responsibleEducationLab"          "responsibleEducationScotGovt"    
    ## [107] "responsibleEducationWelshGovt"    "responsibleEducationNone"        
    ## [109] "responsibleEducationDontKnow"     "responsibleCostLiveCon"          
    ## [111] "responsibleCostLiveLD"            "responsibleCostLiveLab"          
    ## [113] "responsibleCostLiveScotGovt"      "responsibleCostLiveWelshGovt"    
    ## [115] "responsibleCostLiveNone"          "responsibleCostLiveDontKnow"     
    ## [117] "responsibleImmigConservatives"    "responsibleImmigLibDems"         
    ## [119] "responsibleImmigLabour"           "responsibleImmigScottishGovt"    
    ## [121] "responsibleImmigWelshGovt"        "responsibleImmigNone"            
    ## [123] "responsibleImmigDontKnow"         "responsibleCrimeConservatives"   
    ## [125] "responsibleCrimeLibDems"          "responsibleCrimeLabour"          
    ## [127] "responsibleCrimeScottishGovt"     "responsibleCrimeWelshGovt"       
    ## [129] "responsibleCrimeNone"             "responsibleCrimeDontKnow"        
    ## [131] "changeEconomyLab"                 "changeNHSLab"                    
    ## [133] "changeEducationLab"               "changeCostLiveLab"               
    ## [135] "changeImmigLab"                   "changeCrimeLab"                  
    ## [137] "euroTurnout"                      "euroElectionVote"                
    ## [139] "euroElectionVoteOth"              "euRefVote"                       
    ## [141] "eesEUIntegrationSelf"             "eesEUIntegrationCon"             
    ## [143] "eesEUIntegrationLab"              "eesEUIntegrationLD"              
    ## [145] "eesEUIntegrationUKIP"             "eesEUIntegrationSNP"             
    ## [147] "eesEUIntegrationPC"               "EUIntegrationSelf"               
    ## [149] "EUIntegrationCon"                 "EUIntegrationLab"                
    ## [151] "EUIntegrationLD"                  "EUIntegrationUKIP"               
    ## [153] "EUIntegrationSNP"                 "EUIntegrationPC"                 
    ## [155] "selfEUCertain"                    "certaintyEUCon"                  
    ## [157] "certaintyEULab"                   "certaintyEULD"                   
    ## [159] "certaintyEUUKIP"                  "difficultBlameCoalition"         
    ## [161] "singlePartyEffective"             "coalitionInTune"                 
    ## [163] "coalitionDontDeliver"             "immigEcon"                       
    ## [165] "immigCultural"                    "dutyToVote2"                     
    ## [167] "socialPressureVote"               "efficacyUnderstand"              
    ## [169] "efficacyTooMuchEffort"            "efficacyNotUnderstand"           
    ## [171] "efficacyPolCare"                  "redistSelf"                      
    ## [173] "redistCon"                        "redistLab"                       
    ## [175] "redistLD"                         "redistUKIP"                      
    ## [177] "redistSNP"                        "redistPC"                        
    ## [179] "selfRedistCertain"                "certaintyRedistCon"              
    ## [181] "certaintyRedistLab"               "certaintyRedistLD"               
    ## [183] "certaintyRedistUKIP"              "britishness"                     
    ## [185] "scottishness"                     "welshness"                       
    ## [187] "englishness"                      "reasonForUnemployment"           
    ## [189] "immigrantsWelfareState"           "govtHandouts"                    
    ## [191] "polForTheRich"                    "businessBonus"                   
    ## [193] "leftRight"                        "satDemUK"                        
    ## [195] "satDemScot"                       "satDemWales"                     
    ## [197] "satDemEng"                        "satDemEU"                        
    ## [199] "scotShareBurden"                  "approveUKGovt"                   
    ## [201] "approveScotGovt"                  "approveWelshGovt"                
    ## [203] "scotReferendumOutside"            "scotReferendumIntention"         
    ## [205] "scotReferendumTurnout"            "happyScotIndepResult"            
    ## [207] "scotRefExpectation"               "scotRefExpectationTurnout"       
    ## [209] "scotDevoMax"                      "expectationDevoScot"             
    ## [211] "expectationDevoWales"             "engFairShare"                    
    ## [213] "scotFairShare"                    "walesFairShare"                  
    ## [215] "scotIndepGoodEng"                 "englandGovern"                   
    ## [217] "devoResponsibleScotWelfare"       "devoResponsibleScotNHS"          
    ## [219] "devoResponsibleScotSchools"       "devoResponsibleScotDefence"      
    ## [221] "devoResponsibleScotTax"           "devoResponsibleScotPolice"       
    ## [223] "scotIndepEconomy"                 "scotIndepInequality"             
    ## [225] "scotIndepVoice"                   "cooperateRUKGBP"                 
    ## [227] "scotIndepJoinEU"                  "scotIndepMeBetterOff"            
    ## [229] "certaintyScotIndependence"        "certaintyScotUnion"              
    ## [231] "scotElectionVoteConst"            "scotElectionVoteConstOth"        
    ## [233] "scotIndepGoodWales"               "devoPrefWales"                   
    ## [235] "devoResponsibleWalesWelfare"      "devoResponsibleWalesNHS"         
    ## [237] "devoResponsibleWalesSchools"      "devoResponsibleWalesDefence"     
    ## [239] "devoResponsibleWalesTax"          "devoResponsibleWalesPolice"      
    ## [241] "devoResponsibleWalesJustice"      "welshElectionVoteConst"          
    ## [243] "welshElectionVoteConstOth"        "welshElectionVoteList"           
    ## [245] "welshElectionVoteListOth"         "expectationManipCheck"           
    ## [247] "pocketBookAccurateDV2"            "econPersonalProsp"               
    ## [249] "econGenProsp"                     "govtHandleCostLive"              
    ## [251] "govtHandleEcon"                   "govtHandleImmig"                 
    ## [253] "govtHandleNHS"                    "govtHandleEduc"                  
    ## [255] "govtHandleLevelCrime"             "labHandleCostLive"               
    ## [257] "labHandleEcon"                    "labHandleImmig"                  
    ## [259] "labHandleNHS"                     "labHandleEduc"                   
    ## [261] "labHandleLevelCrime"              "minIncomeWellOff"                
    ## [263] "minIncomeGetBy"                   "incomeWelfare"                   
    ## [265] "pidWeThey"                        "pidInterestedOthers"             
    ## [267] "pidCriticiseParty"                "pidCommonParty"                  
    ## [269] "pidRuinDay"                       "pidConnected"                    
    ## [271] "pidMyParty"                       "pidPraiseGood"                   
    ## [273] "promiseHealth"                    "promiseIDCards"                  
    ## [275] "promiseMigration"                 "promiseRetire"                   
    ## [277] "promiseTaxAllowance"              "promiseTuition"                  
    ## [279] "proposalHealth"                   "proposalIDCards"                 
    ## [281] "proposalMigration"                "proposalRetire"                  
    ## [283] "proposalTaxAllowance"             "proposalTuition"                 
    ## [285] "conPriorities_econ"               "conPriorities_costLive"          
    ## [287] "conPriorities_nhs"                "conPriorities_immig"             
    ## [289] "conPriorities_crime"              "conPriorities_school"            
    ## [291] "conPriorities_none"               "labPriorities_econ"              
    ## [293] "labPriorities_costLive"           "labPriorities_nhs"               
    ## [295] "labPriorities_immig"              "labPriorities_crime"             
    ## [297] "labPriorities_school"             "labPriorities_none"              
    ## [299] "ldPriorities_econ"                "ldPriorities_costLive"           
    ## [301] "ldPriorities_nhs"                 "ldPriorities_immig"              
    ## [303] "ldPriorities_crime"               "ldPriorities_school"             
    ## [305] "ldPriorities_none"                "ukipPriorities_econ"             
    ## [307] "ukipPriorities_costLive"          "ukipPriorities_nhs"              
    ## [309] "ukipPriorities_immig"             "ukipPriorities_crime"            
    ## [311] "ukipPriorities_school"            "ukipPriorities_none"             
    ## [313] "partyContact1new"                 "partyContactCon"                 
    ## [315] "partyContactLab"                  "partyContactLD"                  
    ## [317] "partyContactPC"                   "partyContactUKIP"                
    ## [319] "partyContactOtherParty"           "partyContactNone"                
    ## [321] "partyContactDK"                   "othContact"                      
    ## [323] "partyContactCon_1"                "partyContactCon_2"               
    ## [325] "partyContactCon_3"                "partyContactCon_4"               
    ## [327] "partyContactCon_5"                "partyContactCon_6"               
    ## [329] "partyContactCon_7"                "partyContactLab_1"               
    ## [331] "partyContactLab_2"                "partyContactLab_3"               
    ## [333] "partyContactLab_4"                "partyContactLab_5"               
    ## [335] "partyContactLab_6"                "partyContactLab_7"               
    ## [337] "partyContactLD_1"                 "partyContactLD_2"                
    ## [339] "partyContactLD_3"                 "partyContactLD_4"                
    ## [341] "partyContactLD_5"                 "partyContactLD_6"                
    ## [343] "partyContactLD_7"                 "partyContactPC_1"                
    ## [345] "partyContactPC_2"                 "partyContactPC_3"                
    ## [347] "partyContactPC_4"                 "partyContactPC_5"                
    ## [349] "partyContactPC_6"                 "partyContactPC_7"                
    ## [351] "partyContactUKIP_1"               "partyContactUKIP_2"              
    ## [353] "partyContactUKIP_3"               "partyContactUKIP_4"              
    ## [355] "partyContactUKIP_5"               "partyContactUKIP_6"              
    ## [357] "partyContactUKIP_7"               "partyContactOther_1"             
    ## [359] "partyContactOther_2"              "partyContactOther_3"             
    ## [361] "partyContactOther_4"              "partyContactOther_5"             
    ## [363] "partyContactOther_6"              "partyContactOther_7"             
    ## [365] "conUnited"                        "labUnited"                       
    ## [367] "ldUnited"                         "pcUnited"                        
    ## [369] "ukipUnited"                       "mpName"                          
    ## [371] "knowMP"                           "knowMPOth"                       
    ## [373] "countryOfBirth"                   "selfOccSupervise"                
    ## [375] "selfOccOrgSize"                   "selfOccEmployees"                
    ## [377] "selfOccSuperviseLast"             "selfOccOrgSizeLast"              
    ## [379] "selfOccEmployeesLast"             "generalElecCertainty"            
    ## [381] "miilabelcertainty"                "miilabel"                        
    ## [383] "work_type"                        "ageGroup"                        
    ## [385] "w1full"                           "w1core"                          
    ## [387] "pano"                             "onscode"                         
    ## [389] "constituencyid"                   "regionpcon"                      
    ## [391] "electoratepcon"                   "consh10pcon"                     
    ## [393] "ldsh10pcon"                       "grnsh10pcon"                     
    ## [395] "labsh10pcon"                      "pcsh10pcon"                      
    ## [397] "ukipsh10pcon"                     "bnpsh10pcon"                     
    ## [399] "snpsh10pcon"                      "othersh10pcon"                   
    ## [401] "winnersh10pcon"                   "runnerupsh10pcon"                
    ## [403] "marginsh10pcon"                   "turnout10pcon"                   
    ## [405] "runnerup10pcon"                   "winner10pcon"                    
    ## [407] "gender"                           "Age"                             
    ## [409] "marital"                          "housing"                         
    ## [411] "gor"                              "education"                       
    ## [413] "pcon"                             "profile_education_age"           
    ## [415] "profile_ethnicity"                "profile_lea"                     
    ## [417] "profile_oslaua"                   "profile_gross_household"         
    ## [419] "profile_gross_personal"           "profile_household_size"          
    ## [421] "profile_household_children"       "profile_newspaper_readership_201"
    ## [423] "profile_past_vote_2005"           "profile_past_vote_2010"          
    ## [425] "profile_religion"                 "profile_religion_denom"          
    ## [427] "personality_agreeableness"        "personality_conscientiousness"   
    ## [429] "personality_extraversion"         "personality_neuroticism"         
    ## [431] "personality_openness"             "ns_sec"                          
    ## [433] "ns_sec_analytic"                  "soc2010"                         
    ## [435] "lr1"                              "lr2"                             
    ## [437] "lr3"                              "lr4"                             
    ## [439] "lr5"                              "al1"                             
    ## [441] "al2"                              "al3"                             
    ## [443] "al4"                              "al5"                             
    ## [445] "blackEquality"                    "femaleEquality"                  
    ## [447] "gayEquality"                      "socialDesScale"                  
    ## [449] "socialDes_1"                      "socialDes_2"                     
    ## [451] "socialDes_3"                      "socialDes_4"                     
    ## [453] "socialDes_none"                   "socialDes_dk"                    
    ## [455] "riskTaking"                       "privatePrimarySchool"            
    ## [457] "privateSecondarySchool"           "neverPrivateSchool"              
    ## [459] "RPrivSchnew_dk"                   "workingStatus"                   
    ## [461] "preschoolChildrenInHousehold"     "schoolChildrenInHousehold"       
    ## [463] "sickOrElderlyInHousehold"         "noDependentsInHousehold"         
    ## [465] "speakWelsh"                       "prevJob"                         
    ## [467] "selfNumEmployees"                 "selfNumEmployeesLast"            
    ## [469] "headHouseholdPast"                "fatherNumEmployees"              
    ## [471] "motherNumEmployees"               "selfOccStatus"                   
    ## [473] "selfOccStatusLast"                "polKnowMiliband"                 
    ## [475] "polKnowClegg"                     "polKnowOsborne"                  
    ## [477] "polKnowMay"                       "polKnowBercow"                   
    ## [479] "subjectHE"                        "subjectHECurrent"

``` r
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
```

    ## [1] "ons"        "con.intent" "se.x"       "lab.intent" "se.y"      
    ## [6] "ld.intent"  "se"

``` r
vote.intent$se.x <- vote.intent$se.y <- vote.intent$se <- NULL

write.csv(vote.intent, "BES_Vote_Intention_Wave_1_2014.csv", row.names = FALSE)
```

Load the 2014-2015 Ashcroft polling data

``` r
## Download (or load if downloaded) from Github
url.polls <- "https://raw.githubusercontent.com/tkhartman/replication-ashcroft-polling-campaign-spending/master/Ashcroft_Poll_Results_Final.csv"
file.polls <- basename(url.polls)  # Extract the filename
if (!file.exists(file.polls))   # Only download if not in the working directory
  download.file(url = url.polls, destfile = file.polls, mode = "wb")
polls.wide <- read.csv(file.polls) # Author coded file

## Reshape Ashcroft data from wide to long
names(polls.wide)
```

    ##  [1] "ONSConstID" "polls"      "date.1"     "con.1"      "lab.1"     
    ##  [6] "ld.1"       "snp.1"      "ukip.1"     "grn.1"      "oth.1"     
    ## [11] "date_2"     "con.2"      "lab.2"      "ld.2"       "snp.2"     
    ## [16] "ukip.2"     "grn.2"      "oth.2"      "date_3"     "con.3"     
    ## [21] "lab.3"      "ld.3"       "snp.3"      "ukip.3"     "grn.3"     
    ## [26] "oth.3"      "date_4"     "con.4"      "lab.4"      "ld.4"      
    ## [31] "snp.4"      "ukip.4"     "grn.4"      "oth.4"

``` r
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
```

Merge datasets

``` r
## Remove extraneous parties from the 2015 spending data
table(spend.15.sub$party)
```

    ## 
    ##                            Action on Digital Addiction and Cyberstalking 
    ##                                                                        1 
    ##                                               Al-Zebabist Nation of Ooog 
    ##                                                                        1 
    ##                                                       All People's Party 
    ##                                                                        4 
    ##                            Alliance - Alliance Party of Northern Ireland 
    ##                                                                       18 
    ##                                             Alliance For Green Socialism 
    ##                                                                        4 
    ##                                                     Animal Welfare Party 
    ##                                                                        4 
    ##                                      Apni Party [De-registered 07/01/16] 
    ##                                                                        1 
    ##                                                     Apolitical Democrats 
    ##                                                                        1 
    ##                                              Beer, Baccy and Scratchings 
    ##                                                                        1 
    ##                                         Bournemouth Independent Alliance 
    ##                                                                        1 
    ##                                                 British Democratic Party 
    ##                                                                        1 
    ##                                                     British Independents 
    ##                                                                        1 
    ##                          British National Party [De-registered 08/01/16] 
    ##                                                                        8 
    ##                                        Campaign [De-registered 30/06/15] 
    ##                                                                        1 
    ##                                           Cannabis is Safer than Alcohol 
    ##                                                                       32 
    ##                                                     Children of the Atom 
    ##                                                                        1 
    ##                                     Christian Movement for Great Britain 
    ##                                                                        1 
    ## Christian Party "Proclaiming Christ's Lordship" [De-registered 04/11/15] 
    ##                                                                        9 
    ##                                               Christian Peoples Alliance 
    ##                                                                       17 
    ##                                       Class War [De-registered 06/07/15] 
    ##                                                                        7 
    ##                                                       Common Sense Party 
    ##                                                                        1 
    ##                                       Communist League Election Campaign 
    ##                                                                        2 
    ##                                               Communist Party of Britain 
    ##                                                                        9 
    ##                                                 Communities United Party 
    ##                                                                        5 
    ##                                                                Consensus 
    ##                                                                        1 
    ##                                          Conservative and Unionist Party 
    ##                                                                       16 
    ##                                                       Conservative Party 
    ##                                                                      631 
    ##                                                  Democratic Reform Party 
    ##                                                                        1 
    ##                                       Democratic Unionist Party - D.U.P. 
    ##                                                                       16 
    ##                               Digital Democracy [De-registered 09/02/16] 
    ##                                                                        1 
    ##                                                        English Democrats 
    ##                                                                       32 
    ##                                 Europeans Party [De-registered 04/11/15] 
    ##                                                                        1 
    ##                     Free Public Transport Party [De-registered 28/07/15] 
    ##                                                                        1 
    ##                                                        Give Me Back Elmo 
    ##                                                                        1 
    ##                                                              Green Party 
    ##                                                                      542 
    ##                                                Guildford Greenbelt Group 
    ##                                                                        1 
    ##                                                               Hoi Polloi 
    ##                                                                        1 
    ##                                                                 Humanity 
    ##                                                                        1 
    ##                                                 Independence from Europe 
    ##                                                                        5 
    ##                                                              Independent 
    ##                                                                      170 
    ##                    Independent Kidderminster Hospital and Health Concern 
    ##                                                                        1 
    ##                                    Independent Save Withybush Save Lives 
    ##                                                                        1 
    ##                                                 Independents for Bristol 
    ##                                                                        1 
    ##                                                Islam Zinda Baad Platform 
    ##                                                                        1 
    ##                                                   Justice For Men & Boys 
    ##                                                                        2 
    ##                              Keep It Real Party [De-registered 02/07/15] 
    ##                                                                        1 
    ##                                                             Labour Party 
    ##                                                                      600 
    ##                                        Labour Party / Co-operative Party 
    ##                                                                       31 
    ##                                      Land Party [De-registered 04/11/15] 
    ##                                                                        1 
    ##                                                               Left Unity 
    ##                                                                        3 
    ##                                            Lewisham People Before Profit 
    ##                                                                        2 
    ##                                                        Liberal Democrats 
    ##                                                                      631 
    ##                                                               Liberty GB 
    ##                                                                        3 
    ##                             Lincolnshire Independents Lincolnshire First 
    ##                                                                        5 
    ##                             Magna Carta Conservation Party Great Britain 
    ##                                                                        1 
    ##                                                               Mainstream 
    ##                                                                        1 
    ##                                        Manston Airport Independent Party 
    ##                                                                        1 
    ##                                   Mebyon Kernow - The Party for Cornwall 
    ##                                                                        6 
    ##                                   Movement for Active Democracy (M.A.D.) 
    ##                                                                        1 
    ##                                                           National Front 
    ##                                                                        7 
    ##                                             National Health Action Party 
    ##                                                                       11 
    ##                                 National Liberal Party - True Liberalism 
    ##                                                                        2 
    ##                                              New Independent Centralists 
    ##                                                                        1 
    ##                                      Official Monster Raving Loony Party 
    ##                                                                       16 
    ##                                                Party for a United Thanet 
    ##                                                                        2 
    ##                                                         Party of Dissent 
    ##                                                                        1 
    ##                                                                   Patria 
    ##                                                                        2 
    ##                       Patriotic Socialist Party [De-registered 05/01/16] 
    ##                                                                        2 
    ##                                            People Before Profit Alliance 
    ##                                                                        1 
    ##                                             People First - Gwerin Gyntaf 
    ##                                                                        1 
    ##                                                          Pirate Party UK 
    ##                                                                        6 
    ##                                         Plaid Cymru - The Party of Wales 
    ##                                                                       40 
    ##                             Population Party UK [De-registered 02/07/15] 
    ##                                                                        1 
    ##                                                   Putting Croydon First! 
    ##                                                                        1 
    ##                                                      Rebooting Democracy 
    ##                                                                        1 
    ##                                               Red Flag - Anti-Corruption 
    ##                                                                        2 
    ##                             Reduce VAT in Sport [De-registered 14/09/15] 
    ##                                                                        1 
    ##                                                 Residents for Uttlesford 
    ##                                                                        1 
    ##                                   Restore the Family For Children's Sake 
    ##                                                                        1 
    ##                                                     Rochdale First Party 
    ##                                                                        1 
    ##                                                 Save Hartlepool Hospital 
    ##                                                                        1 
    ##                                                     Scottish Green Party 
    ##                                                                       31 
    ##                                            Scottish National Party (SNP) 
    ##                                                                       59 
    ##                                                 Scottish Socialist Party 
    ##                                                                        4 
    ##                                  SDLP (Social Democratic & Labour Party) 
    ##                                                                       18 
    ##                                                                Sinn Féin 
    ##                                                                       18 
    ##                                                  Social Democratic Party 
    ##                                                                        2 
    ##                                                 Socialist Equality Party 
    ##                                                                        2 
    ##                                                   Socialist Labour Party 
    ##                                                                        8 
    ##                                                            Something New 
    ##                                                                        2 
    ##                             The 30-50 Coalition [De-registered 06/11/15] 
    ##                                                                        1 
    ##                                               The Above and Beyond Party 
    ##                                                                        5 
    ##                              The Birthday Party [De-registered 12/11/15] 
    ##                                                                        1 
    ##                                                         The Change Party 
    ##                                                                        1 
    ##                                                     The Democratic Party 
    ##                                                                        1 
    ##                                     The Eccentric Party of Great Britain 
    ##                                                                        1 
    ##                                                      The Evolution Party 
    ##                                                                        1 
    ##                                 The Independent Political Alliance Party 
    ##                                                                        1 
    ##                                      The Justice & Anti-Corruption Party 
    ##                                                                        2 
    ##                                                        The Liberal Party 
    ##                                                                        4 
    ##                                                    The Magna Carta Party 
    ##                                                                        1 
    ##                                                 The New Society of Worth 
    ##                                                                        1 
    ##                                                     The North East Party 
    ##                                                                        4 
    ##                                                       The Northern Party 
    ##                                                                        3 
    ##                                          The Party for Poole People Ltd. 
    ##                                                                        1 
    ##                     The Peace Party - Non-violence, Justice, Environment 
    ##                                                                        4 
    ##                               The Pilgrim Party [De-registered 28/01/16] 
    ##                                                                        1 
    ##                                         The Principles of Politics Party 
    ##                                                                        1 
    ##                                                      The Realists' Party 
    ##                                                                        1 
    ##                                           The Republican Socialist Party 
    ##                                                                        1 
    ##                                                        The Respect Party 
    ##                                                                        4 
    ##                            The Roman Party. Ave [De-registered 14/09/15] 
    ##                                                                        1 
    ##                                     The Socialist Party of Great Britain 
    ##                                                                       10 
    ##                                                      The Southport Party 
    ##                                                                        1 
    ##                                          The Speaker Seeking Re-election 
    ##                                                                        1 
    ##                                         The Sustainable Population Party 
    ##                                                                        1 
    ##                                                    The U(niversal) Party 
    ##                                                                        1 
    ##                                       The UK Progressive Democracy Party 
    ##                                                                        1 
    ##                                                           The Whig Party 
    ##                                                                        4 
    ##                                                        The Workers Party 
    ##                                                                        5 
    ##                                   Trade Unionist and Socialist Coalition 
    ##                                                                      128 
    ##                      Trade Unionist and Socialist Coalition / Left Unity 
    ##                                                                        7 
    ##                                         Traditional Unionist Voice - TUV 
    ##                                                                        7 
    ##                                                             Ubuntu Party 
    ##                                                                        2 
    ##                                             UK Independence Party (UKIP) 
    ##                                                                      624 
    ##                                                    Ulster Unionist Party 
    ##                                                                       15 
    ##                                                          Vapers in Power 
    ##                                                                        2 
    ##                               War Veteran's Pro-Traditional Family Party 
    ##                                                                        1 
    ##                                                 We Are The Reality Party 
    ##                                                                        3 
    ##                                                      Wessex Regionalists 
    ##                                                                        1 
    ##                                                       Wigan Independents 
    ##                                                                        1 
    ##                                              Workers Revolutionary Party 
    ##                                                                        7 
    ##                                                 World Peace Through Song 
    ##                                                                        1 
    ##                                                          Yorkshire First 
    ##                                                                       14 
    ##                                                 Young People's Party YPP 
    ##                                                                        2

``` r
party.15.keep <- c("Conservative Party", "Green Party", "Labour Party",
                "Labour Party / Co-operative Party", "Liberal Democrats", "Plaid Cymru - The Party of Wales",
                "Scottish National Party (SNP)", "UK Independence Party (UKIP)")

spend.15.sub <- subset(spend.15.sub, party %in% party.15.keep)

## Remove extraneous parties from the 2010 spending data
table(spend.10.sub$party)
```

    ## 
    ##                                "Christian Party ""Proclaiming Christ's Lordship"" 
    ##                                                                                65 
    ##                            "Clause 28, Children's Protection Christian Democrats" 
    ##                                                                                 1 
    ##                                           "National Liberal Party, The Third Way" 
    ##                                                                                 1 
    ##                            "The Peace Party - Non-violence, Justice, Environment" 
    ##                                                                                 3 
    ##                                                   A Vote Against MP Expense Abuse 
    ##                                                                                 1 
    ##                                                               All The South Party 
    ##                                                                                 1 
    ##                                     Alliance - Alliance Party of Northern Ireland 
    ##                                                                                18 
    ##                                                      Alliance For Green Socialism 
    ##                                                                                 6 
    ##                                                     Alliance for Workers' Liberty 
    ##                                                                                 1 
    ##                                                                     Animals Count 
    ##                                                                                 1 
    ##                                                   Anticapitalists - Workers Power 
    ##                                                                                 1 
    ##                                                              Apolitical Democrats 
    ##                                                                                 1 
    ##                                                               Best of a Bad Bunch 
    ##                                                                                 2 
    ##                                                            Blue Environment Party 
    ##                                                                                 1 
    ##                                                            British National Party 
    ##                                                                               326 
    ##                                               Bromsgrove Independent Conservative 
    ##                                                                                 1 
    ##                                                         Bushra Irfan of Blackburn 
    ##                                                                                 1 
    ##                                                              Cambridge Socialists 
    ##                                                                                 1 
    ##                                              Christian Movement for Great Britain 
    ##                                                                                 2 
    ##                                                        Christian Peoples Alliance 
    ##                                                                                15 
    ##                                                      Church of the Militant Elvis 
    ##                                                                                 1 
    ##                                           Citizens for Undead Rights and Equality 
    ##                                                                                 4 
    ##                                                                 City INDEPENDENTS 
    ##                                                                                 1 
    ##                                                                       Common Good 
    ##                                                                                 1 
    ##                                                                Common Sense Party 
    ##                                                                                 2 
    ##                                                Communist League Election Campaign 
    ##                                                                                 2 
    ##                                                        Communist Party of Britain 
    ##                                                                                 6 
    ##                                                   Conservative and Unionist Party 
    ##                                                                               630 
    ##                             Conservative and Unionist Party/Ulster Unionist Party 
    ##                                                                                17 
    ##                                                                   Cut The Deficit 
    ##                                                                                 1 
    ##                                                           Democratic Labour Party 
    ##                                                                                 1 
    ##                                                           Democratic Nationalists 
    ##                                                                                 2 
    ##                                                Democratic Unionist Party - D.U.P. 
    ##                                                                                16 
    ##                                                Direct Democracy (Communist) Party 
    ##                                                                                 1 
    ##                                                           English Democrats Party 
    ##                                                                                94 
    ##                                                        English Independence Party 
    ##                                                                                 1 
    ##                                                          Equal Parenting Alliance 
    ##                                                                                 2 
    ##                                                                 Fancy Dress Party 
    ##                                                                                 1 
    ##                                                        Freedom and Responsibility 
    ##                                                                                 1 
    ##                                                         Get Snouts Out The Trough 
    ##                                                                                 1 
    ##                                                                       Green Party 
    ##                                                                               309 
    ##                                                   Hugh Salmon for Battersea Party 
    ##                                                                                 1 
    ##                                                                          Humanity 
    ##                                                                                 1 
    ##                                                                      Impact Party 
    ##                                                                                 3 
    ##                                                                       Independent 
    ##                                                                               292 
    ##                              Independent Ealing Acton Communities Public Services 
    ##                                                                                 1 
    ##                             Independent Kidderminster Hospital and Health Concern 
    ##                                                                                 1 
    ##                                                 INDEPENDENT Leave-the-EU Alliance 
    ##                                                                                 1 
    ##                                                       Independent People Together 
    ##                                                                                 1 
    ##                                                   Independent Save Our Green Belt 
    ##                                                                                 1 
    ##                                                     Independent Voice for Halifax 
    ##                                                                                 1 
    ##                                                        Independents Federation UK 
    ##                                                                                 4 
    ##                                        Independents to Save Queen Mary's Hospital 
    ##                                                                                 1 
    ##                                                                      Integrity UK 
    ##                                                                                 1 
    ##                                                         Islam Zinda Baad Platform 
    ##                                                                                 1 
    ##                                                 Jannen will put Brent North First 
    ##                                                                                 1 
    ##                                                                     Justice Party 
    ##                                                                                 1 
    ##                                                                      Labour Party 
    ##                                                                               587 
    ##                                                   Labour Party/Co-operative Party 
    ##                                                                                43 
    ##                                                                        Land Party 
    ##                                                                                 1 
    ##                                                     Lewisham People Before Profit 
    ##                                                                                 1 
    ##                                                                 Liberal Democrats 
    ##                                                                               630 
    ##                                                                 Libertarian Party 
    ##                                                                                 2 
    ##                                      Lincolnshire Independents Lincolnshire First 
    ##                                                                                 3 
    ##                                       Local Liberals People Before Politics Party 
    ##                                                                                 1 
    ##                                                       Mansfield Independent Forum 
    ##                                                                                 1 
    ##                                    Matriarchal Party United Kingdom Great Britain 
    ##                                                                                 1 
    ##                                            Mebyon Kernow - The Party for Cornwall 
    ##                                                                                 6 
    ##                                                          Medway Independent Party 
    ##                                                                                 1 
    ##                                                              Middle England Party 
    ##                                                                                 1 
    ##                                                                Money Reform Party 
    ##                                                                                 1 
    ##                                            Movement for Active Democracy (M.A.D.) 
    ##                                                                                 1 
    ##                                                                    National Front 
    ##                                                                                15 
    ##                                                           Nationwide Reform Party 
    ##                                                                                 1 
    ##                                               Neath Port Talbot Independent Party 
    ##                                                                                 1 
    ##                                   New Independent Conservative Chelsea and Fulham 
    ##                                                                                 1 
    ##                                                         New Millennium Bean Party 
    ##                                                                                 1 
    ##                                                    No Candidate Deserves My Vote! 
    ##                                                                                 1 
    ##                                                                      Nobody Party 
    ##                                                                                 1 
    ##                                            Northampton - Save Our Public Services 
    ##                                                                                 1 
    ##                                               Official Monster Raving Loony Party 
    ##                                                                                24 
    ##                                                     People Before Profit Alliance 
    ##                                                                                 1 
    ##                                                                    People's Voice 
    ##                                                                                 1 
    ##                                                               Peoples Party Essex 
    ##                                                                                 1 
    ##                                                                   Pirate Party UK 
    ##                                                                                 7 
    ##                                                  Plaid Cymru - The Party of Wales 
    ##                                                                                40 
    ##                                                                Reduce Tax On Beer 
    ##                                                                                 1 
    ##                                                                 Reform 2000 Party 
    ##                                                                                 1 
    ##                                                                 Restoration Party 
    ##                                                                                 1 
    ##                          Revolutionary Communist Party Britain (Marxist-Leninist) 
    ##                                                                                 1 
    ##                                                         Save King George Hospital 
    ##                                                                                 1 
    ##                                                              Scottish Green Party 
    ##                                                                                20 
    ##                                                     Scottish National Party (SNP) 
    ##                                                                                59 
    ##                                                          Scottish Socialist Party 
    ##                                                                                10 
    ##                                                          Scrap Members Allowances 
    ##                                                                                 2 
    ##                                           SDLP (Social Democratic & Labour Party) 
    ##                                                                                18 
    ##                                                                         Sinn FÚin 
    ##                                                                                17 
    ##                                                           Social Democratic Party 
    ##                                                                                 2 
    ##                                                             Socialist Alternative 
    ##                                                                                 4 
    ##                                                          Socialist Equality Party 
    ##                                                                                 2 
    ##                                                            Socialist Labour Party 
    ##                                                                                22 
    ##                                        Solihull and Meriden Residents Association 
    ##                                                                                 2 
    ##                                                   Staffordshire Independent Group 
    ##                                                                                 1 
    ##                                                       Tamsin Omond To The Commons 
    ##                                                                                 1 
    ##                                                                    Tendring First 
    ##                                                                                 1 
    ##                                                       The Animal Protection Party 
    ##                                                                                 4 
    ##                                        The Buckinghamshire Campaign for Democracy 
    ##                                                                                 1 
    ##                                                             The Cornish Democrats 
    ##                                                                                 1 
    ##                                                                   The Joy of Talk 
    ##                                                                                 1 
    ##                                               The Justice & Anti-Corruption Party 
    ##                                                                                 2 
    ##                                                                 The Liberal Party 
    ##                                                                                 5 
    ##                                                      The Macclesfield Independent 
    ##                                                                                 1 
    ##                                                                     The New Party 
    ##                                                                                 1 
    ##                                                                      The P.N.D.P. 
    ##                                                                                 1 
    ##                                                                 The Respect Party 
    ##                                                                                10 
    ##                                                                 The Science Party 
    ##                                                                                 1 
    ##                                              The Socialist Party of Great Britain 
    ##                                                                                 1 
    ##                                                                       The Speaker 
    ##                                                                                 1 
    ##                                                   The True English (Poetry) Party 
    ##                                                                                 1 
    ##                                                                   The Youth Party 
    ##                                                                                 1 
    ##                                            Trade Unionist and Socialist Coalition 
    ##                                                                                31 
    ## Trade Unionist and Socialist Coalition/Solidarity - Scotland's Socialist Movement 
    ##                                                                                 4 
    ##                                                  Traditional Unionist Voice - TUV 
    ##                                                                                10 
    ##                                                                             Trust 
    ##                                                                                 2 
    ##                                                    UK Independence Party (UK I P) 
    ##                                                                               540 
    ##                                                                      United Voice 
    ##                                                                                 1 
    ##                                                     Unity For Peace And Socialism 
    ##                                                                                 1 
    ##                                         Virtue Currency Cognitive Appraisal Party 
    ##                                                                                 1 
    ##                                                               Wessex Regionalists 
    ##                                                                                 1 
    ##                                                       Workers Revolutionary Party 
    ##                                                                                 7 
    ##                                                                         You Party 
    ##                                                                                 1 
    ##                                             Your Right To Democracy Party Limited 
    ##                                                                                 3

``` r
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
```

    ## Warning in left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y): joining
    ## factor and character vector, coercing into character vector

``` r
write.csv(df, "Spending_Data_Merged_Final_May_2017.csv", row.names = FALSE)
     

############################################################################
```

Clear the workspace and start anew

``` r
rm(list=ls())
df <- read.csv("Spending_Data_Merged_Final_May_2017.csv")
```

Generate 2010 marginality score

``` r
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
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##       0      10      20      21      30      65    4424

``` r
summary(df$lab.margin10)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##       0      10      24      24      36      58    4425

``` r
summary(df$ld.margin10)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##       0      18      26      25      32      61    4425

Generate 2015 constituency marginality measures based on Ashcroft polls

``` r
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
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##       0       5      10      14      20      53    4889

``` r
summary(df$lab.margin15)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##       0       6      11      13      20      42    4889

``` r
summary(df$ld.margin15)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##       1      20      33      29      37      56    4889

``` r
## Ashcroft poll conducted?
df$ashcroft <- ifelse(df$poll.1.1st > 0, 1, 0)
df$ashcroft[is.na(df$ashcroft)] <- 0
```

Subset the merged data by party

``` r
con <- subset(df, party == "Conservative")
lab <- subset(df, party == "Labour")
ld <- subset(df, party == "LibDem")
```

Descriptive statistics for campaign spending

``` r
## Percent of 2015 short campaign constituency spending
summary(con$shortpct15)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.0    24.0    56.8    53.6    83.2    98.9       1

``` r
summary(lab$shortpct15)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.0    16.1    47.4    47.3    77.0   145.0       1

``` r
summary(ld$shortpct15)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    3.77    8.38   20.80   22.90  107.00       1

``` r
## Potential 2015 missing data
length(con$shortpct15[con$shortpct15 == "0"]) 
```

    ## [1] 36

``` r
length(lab$shortpct15[lab$shortpct15 == "0"]) 
```

    ## [1] 39

``` r
length(ld$shortpct15[ld$shortpct15 == "0"])
```

    ## [1] 106

``` r
## 2015 Spending data, removing 0s (which may be missing or true 0; cannot tell)
con$short15 <- ifelse(con$shortpct15 > 0, con$shortpct15, NA)
lab$short15 <- ifelse(lab$shortpct15 > 0, lab$shortpct15, NA)
ld$short15 <- ifelse(ld$shortpct15 > 0, ld$shortpct15, NA)

summary(con$short15)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.23   29.50   61.60   56.70   84.20   98.90      36

``` r
summary(lab$short15)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.24   19.10   49.70   50.30   78.90  145.00      39

``` r
summary(ld$short15)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.07    6.42   10.70   25.00   28.80  107.00     106

``` r
## Percent of 2010 short campaign constituency spending
summary(con$shortpct10)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.0    45.1    76.9    65.8    90.5   105.0       2

``` r
summary(lab$shortpct10)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.0    18.8    54.2    50.4    78.7   109.0       2

``` r
summary(ld$shortpct10)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    8.89   22.90   37.00   66.80  101.00       2

``` r
## 2010 Spending data 
con$short10 <- con$shortpct10
lab$short10 <- lab$shortpct10
ld$short10 <- ld$shortpct10 

summary(con$short10)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.0    45.1    76.9    65.8    90.5   105.0       2

``` r
summary(lab$short10)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.0    18.8    54.2    50.4    78.7   109.0       2

``` r
summary(ld$short10)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    8.89   22.90   37.00   66.80  101.00       2

``` r
## 2010 election winner (dummy)
con$win10 <- ifelse(con$winner10 == "Conservative", 1, 0) 
lab$win10 <- ifelse(lab$winner10 == "Labour", 1, 0) 
ld$win10 <- ifelse(ld$winner10 == "Liberal Democrat", 1, 0) 

summary(con$win10)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   0.000   0.484   1.000   1.000

``` r
summary(lab$win10)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   0.000   0.408   1.000   1.000

``` r
summary(ld$win10)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.0000  0.0000  0.0902  0.0000  1.0000

``` r
## 2015 poll leader (dummy)
con$lead15 <- ifelse(con$poll.leader == "Conservative", 1, 0) 
lab$lead15 <- ifelse(lab$poll.leader == "Labour", 1, 0) 
ld$lead15 <- ifelse(ld$poll.leader == "LibDem", 1, 0) 

summary(con$lead15)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.0     0.0     0.0     0.3     1.0     1.0     465

``` r
summary(lab$lead15)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.0     0.0     0.0     0.4     1.0     1.0     465

``` r
summary(ld$lead15)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.0     0.0     0.0     0.1     0.0     1.0     465

Results

``` r
## Conservative spending models
con.m1 <- lm(short15 ~ short10 + win10*con.margin10, data = con)                                       # Prior information only
con.m2 <- lm(short15 ~ short10 + win10*con.margin10 + con.intent + ashcroft, data = con)               # Plus Ashcroft poll
con.m3 <- lm(short15 ~ short10 + win10*con.margin10 + con.intent + con.margin15, data = con)           # Plus Ashcroft marginals

summary(con.m1)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * con.margin10, data = con)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -59.47 -14.25  -0.32  12.68  63.83 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)         35.2812     4.0200    8.78 < 0.0000000000000002 ***
    ## short10              0.3927     0.0391   10.05 < 0.0000000000000002 ***
    ## win10               13.8428     3.1072    4.46       0.000010037134 ***
    ## con.margin10        -0.6952     0.1012   -6.87       0.000000000017 ***
    ## win10:con.margin10   0.2530     0.1369    1.85                0.065 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.2 on 590 degrees of freedom
    ##   (37 observations deleted due to missingness)
    ## Multiple R-squared:  0.557,  Adjusted R-squared:  0.554 
    ## F-statistic:  186 on 4 and 590 DF,  p-value: <0.0000000000000002

``` r
summary(con.m2)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * con.margin10 + con.intent + 
    ##     ashcroft, data = con)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -52.60 -11.58   0.53   9.86  60.16 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)         27.6912     4.1427    6.68       0.000000000054 ***
    ## short10              0.3520     0.0361    9.74 < 0.0000000000000002 ***
    ## win10                4.6494     2.9798    1.56                0.119    
    ## con.margin10        -0.6329     0.0954   -6.63       0.000000000074 ***
    ## con.intent           0.1593     0.0820    1.94                0.053 .  
    ## ashcroft            20.4759     1.9551   10.47 < 0.0000000000000002 ***
    ## win10:con.margin10   0.6805     0.1409    4.83       0.000001749049 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.5 on 588 degrees of freedom
    ##   (37 observations deleted due to missingness)
    ## Multiple R-squared:  0.628,  Adjusted R-squared:  0.625 
    ## F-statistic:  166 on 6 and 588 DF,  p-value: <0.0000000000000002

``` r
summary(con.m3)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * con.margin10 + con.intent + 
    ##     con.margin15, data = con)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -45.86  -4.78   2.50   6.41  51.41 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)         70.4539     7.4288    9.48 < 0.0000000000000002 ***
    ## short10              0.2275     0.0603    3.77              0.00023 ***
    ## win10               -6.3618     3.0986   -2.05              0.04173 *  
    ## con.margin10        -0.8773     0.1550   -5.66          0.000000071 ***
    ## con.intent           0.2497     0.1184    2.11              0.03651 *  
    ## con.margin15        -0.5476     0.1767   -3.10              0.00230 ** 
    ## win10:con.margin10   1.0740     0.2840    3.78              0.00022 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13.3 on 156 degrees of freedom
    ##   (469 observations deleted due to missingness)
    ## Multiple R-squared:  0.792,  Adjusted R-squared:  0.784 
    ## F-statistic: 99.2 on 6 and 156 DF,  p-value: <0.0000000000000002

``` r
## Labour spending models
lab.m1 <- lm(short15 ~ short10 + win10*lab.margin10, data = lab)                                       # Prior information only
lab.m2 <- lm(short15 ~ short10 + win10*lab.margin10 + lab.intent + ashcroft, data = lab)               # Plus Ashcroft poll
lab.m3 <- lm(short15 ~ short10 + win10*lab.margin10 + lab.intent + lab.margin15, data = lab)           # Plus Ashcroft marginals

summary(lab.m1)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * lab.margin10, data = lab)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -59.27 -10.89  -0.81  10.92  87.99 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)         59.7005     3.7673   15.85 < 0.0000000000000002 ***
    ## short10              0.3643     0.0392    9.30 < 0.0000000000000002 ***
    ## win10              -18.2272     2.9965   -6.08  0.00000000212834975 ***
    ## lab.margin10        -1.2023     0.0910  -13.21 < 0.0000000000000002 ***
    ## win10:lab.margin10   1.0179     0.1232    8.26  0.00000000000000097 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.1 on 588 degrees of freedom
    ##   (39 observations deleted due to missingness)
    ## Multiple R-squared:  0.632,  Adjusted R-squared:  0.63 
    ## F-statistic:  253 on 4 and 588 DF,  p-value: <0.0000000000000002

``` r
summary(lab.m2)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * lab.margin10 + lab.intent + 
    ##     ashcroft, data = lab)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -63.98 -11.73  -0.84  10.66  87.34 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)         58.0589     4.7726   12.16 < 0.0000000000000002 ***
    ## short10              0.3644     0.0380    9.58 < 0.0000000000000002 ***
    ## win10              -10.8121     3.2493   -3.33              0.00093 ***
    ## lab.margin10        -1.0910     0.0988  -11.05 < 0.0000000000000002 ***
    ## lab.intent          -0.2189     0.0767   -2.85              0.00446 ** 
    ## ashcroft            11.0017     2.0315    5.42      0.0000000892283 ***
    ## win10:lab.margin10   0.9756     0.1364    7.15      0.0000000000025 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.5 on 586 degrees of freedom
    ##   (39 observations deleted due to missingness)
    ## Multiple R-squared:  0.656,  Adjusted R-squared:  0.653 
    ## F-statistic:  186 on 6 and 586 DF,  p-value: <0.0000000000000002

``` r
summary(lab.m3)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * lab.margin10 + lab.intent + 
    ##     lab.margin15, data = lab)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -69.27 -11.09   0.63  10.36  73.50 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value            Pr(>|t|)    
    ## (Intercept)         82.6534     8.9992    9.18 0.00000000000000028 ***
    ## short10              0.1669     0.0764    2.18              0.0306 *  
    ## win10               -6.1240     5.7599   -1.06              0.2894    
    ## lab.margin10        -1.2530     0.2053   -6.10 0.00000000815731028 ***
    ## lab.intent           0.0622     0.1619    0.38              0.7012    
    ## lab.margin15        -0.6718     0.2036   -3.30              0.0012 ** 
    ## win10:lab.margin10   1.0724     0.2818    3.81              0.0002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.2 on 153 degrees of freedom
    ##   (472 observations deleted due to missingness)
    ## Multiple R-squared:  0.66,   Adjusted R-squared:  0.647 
    ## F-statistic: 49.5 on 6 and 153 DF,  p-value: <0.0000000000000002

``` r
## Liberal Democratic spending models
ld.m1 <- lm(short15 ~ short10 + win10*ld.margin10, data = ld)                                          # Prior information only
ld.m2 <- lm(short15 ~ short10 + win10*ld.margin10 + ld.intent + ashcroft, data = ld)                   # Plus Ashcroft poll
ld.m3 <- lm(short15 ~ short10 + win10*ld.margin10 + ld.intent + ld.margin15, data = ld)                # Plus Ashcroft marginals

summary(ld.m1)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * ld.margin10, data = ld)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -62.39  -7.83  -1.91   4.56  76.02 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)        24.1359     3.2831    7.35     0.00000000000077 ***
    ## short10             0.2121     0.0314    6.76     0.00000000003637 ***
    ## win10              50.0863     4.1522   12.06 < 0.0000000000000002 ***
    ## ld.margin10        -0.5671     0.0949   -5.98     0.00000000419696 ***
    ## win10:ld.margin10   0.4998     0.2455    2.04                0.042 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.4 on 520 degrees of freedom
    ##   (107 observations deleted due to missingness)
    ## Multiple R-squared:  0.704,  Adjusted R-squared:  0.701 
    ## F-statistic:  309 on 4 and 520 DF,  p-value: <0.0000000000000002

``` r
summary(ld.m2)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * ld.margin10 + ld.intent + 
    ##     ashcroft, data = ld)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -62.98  -7.71  -1.93   4.57  75.48 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)        22.3470     3.5207    6.35        0.00000000048 ***
    ## short10             0.2074     0.0315    6.58        0.00000000012 ***
    ## win10              49.1805     4.4237   11.12 < 0.0000000000000002 ***
    ## ld.margin10        -0.5531     0.0960   -5.76        0.00000001454 ***
    ## ld.intent           0.2970     0.1205    2.46                0.014 *  
    ## ashcroft           -0.8958     1.7804   -0.50                0.615    
    ## win10:ld.margin10   0.4516     0.2461    1.83                0.067 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.4 on 518 degrees of freedom
    ##   (107 observations deleted due to missingness)
    ## Multiple R-squared:  0.707,  Adjusted R-squared:  0.704 
    ## F-statistic:  209 on 6 and 518 DF,  p-value: <0.0000000000000002

``` r
summary(ld.m3)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * ld.margin10 + ld.intent + 
    ##     ld.margin15, data = ld)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -42.64  -8.76  -1.43   6.88  50.19 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value      Pr(>|t|)    
    ## (Intercept)         48.984      7.348    6.67 0.00000000055 ***
    ## short10              0.110      0.062    1.77         0.079 .  
    ## win10               38.751      5.740    6.75 0.00000000035 ***
    ## ld.margin10         -0.443      0.195   -2.27         0.025 *  
    ## ld.intent            0.188      0.196    0.96         0.340    
    ## ld.margin15         -0.769      0.191   -4.03 0.00008976071 ***
    ## win10:ld.margin10    0.656      0.357    1.84         0.068 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.4 on 141 degrees of freedom
    ##   (484 observations deleted due to missingness)
    ## Multiple R-squared:  0.856,  Adjusted R-squared:  0.85 
    ## F-statistic:  140 on 6 and 141 DF,  p-value: <0.0000000000000002

``` r
## Table 1 - Prior information only
summary(con.m1)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * con.margin10, data = con)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -59.47 -14.25  -0.32  12.68  63.83 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)         35.2812     4.0200    8.78 < 0.0000000000000002 ***
    ## short10              0.3927     0.0391   10.05 < 0.0000000000000002 ***
    ## win10               13.8428     3.1072    4.46       0.000010037134 ***
    ## con.margin10        -0.6952     0.1012   -6.87       0.000000000017 ***
    ## win10:con.margin10   0.2530     0.1369    1.85                0.065 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.2 on 590 degrees of freedom
    ##   (37 observations deleted due to missingness)
    ## Multiple R-squared:  0.557,  Adjusted R-squared:  0.554 
    ## F-statistic:  186 on 4 and 590 DF,  p-value: <0.0000000000000002

``` r
summary(lab.m1)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * lab.margin10, data = lab)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -59.27 -10.89  -0.81  10.92  87.99 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)         59.7005     3.7673   15.85 < 0.0000000000000002 ***
    ## short10              0.3643     0.0392    9.30 < 0.0000000000000002 ***
    ## win10              -18.2272     2.9965   -6.08  0.00000000212834975 ***
    ## lab.margin10        -1.2023     0.0910  -13.21 < 0.0000000000000002 ***
    ## win10:lab.margin10   1.0179     0.1232    8.26  0.00000000000000097 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.1 on 588 degrees of freedom
    ##   (39 observations deleted due to missingness)
    ## Multiple R-squared:  0.632,  Adjusted R-squared:  0.63 
    ## F-statistic:  253 on 4 and 588 DF,  p-value: <0.0000000000000002

``` r
summary(ld.m1)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * ld.margin10, data = ld)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -62.39  -7.83  -1.91   4.56  76.02 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)        24.1359     3.2831    7.35     0.00000000000077 ***
    ## short10             0.2121     0.0314    6.76     0.00000000003637 ***
    ## win10              50.0863     4.1522   12.06 < 0.0000000000000002 ***
    ## ld.margin10        -0.5671     0.0949   -5.98     0.00000000419696 ***
    ## win10:ld.margin10   0.4998     0.2455    2.04                0.042 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.4 on 520 degrees of freedom
    ##   (107 observations deleted due to missingness)
    ## Multiple R-squared:  0.704,  Adjusted R-squared:  0.701 
    ## F-statistic:  309 on 4 and 520 DF,  p-value: <0.0000000000000002

``` r
## Table 2 - Prior information plus presence of Ashcroft poll and BES vote intention
summary(con.m2)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * con.margin10 + con.intent + 
    ##     ashcroft, data = con)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -52.60 -11.58   0.53   9.86  60.16 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)         27.6912     4.1427    6.68       0.000000000054 ***
    ## short10              0.3520     0.0361    9.74 < 0.0000000000000002 ***
    ## win10                4.6494     2.9798    1.56                0.119    
    ## con.margin10        -0.6329     0.0954   -6.63       0.000000000074 ***
    ## con.intent           0.1593     0.0820    1.94                0.053 .  
    ## ashcroft            20.4759     1.9551   10.47 < 0.0000000000000002 ***
    ## win10:con.margin10   0.6805     0.1409    4.83       0.000001749049 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.5 on 588 degrees of freedom
    ##   (37 observations deleted due to missingness)
    ## Multiple R-squared:  0.628,  Adjusted R-squared:  0.625 
    ## F-statistic:  166 on 6 and 588 DF,  p-value: <0.0000000000000002

``` r
summary(lab.m2)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * lab.margin10 + lab.intent + 
    ##     ashcroft, data = lab)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -63.98 -11.73  -0.84  10.66  87.34 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)         58.0589     4.7726   12.16 < 0.0000000000000002 ***
    ## short10              0.3644     0.0380    9.58 < 0.0000000000000002 ***
    ## win10              -10.8121     3.2493   -3.33              0.00093 ***
    ## lab.margin10        -1.0910     0.0988  -11.05 < 0.0000000000000002 ***
    ## lab.intent          -0.2189     0.0767   -2.85              0.00446 ** 
    ## ashcroft            11.0017     2.0315    5.42      0.0000000892283 ***
    ## win10:lab.margin10   0.9756     0.1364    7.15      0.0000000000025 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.5 on 586 degrees of freedom
    ##   (39 observations deleted due to missingness)
    ## Multiple R-squared:  0.656,  Adjusted R-squared:  0.653 
    ## F-statistic:  186 on 6 and 586 DF,  p-value: <0.0000000000000002

``` r
summary(ld.m2)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * ld.margin10 + ld.intent + 
    ##     ashcroft, data = ld)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -62.98  -7.71  -1.93   4.57  75.48 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)        22.3470     3.5207    6.35        0.00000000048 ***
    ## short10             0.2074     0.0315    6.58        0.00000000012 ***
    ## win10              49.1805     4.4237   11.12 < 0.0000000000000002 ***
    ## ld.margin10        -0.5531     0.0960   -5.76        0.00000001454 ***
    ## ld.intent           0.2970     0.1205    2.46                0.014 *  
    ## ashcroft           -0.8958     1.7804   -0.50                0.615    
    ## win10:ld.margin10   0.4516     0.2461    1.83                0.067 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.4 on 518 degrees of freedom
    ##   (107 observations deleted due to missingness)
    ## Multiple R-squared:  0.707,  Adjusted R-squared:  0.704 
    ## F-statistic:  209 on 6 and 518 DF,  p-value: <0.0000000000000002

``` r
## Table 3 - Prior information plus Ashcroft poll marginals and BES vote intention
summary(con.m3)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * con.margin10 + con.intent + 
    ##     con.margin15, data = con)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -45.86  -4.78   2.50   6.41  51.41 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)         70.4539     7.4288    9.48 < 0.0000000000000002 ***
    ## short10              0.2275     0.0603    3.77              0.00023 ***
    ## win10               -6.3618     3.0986   -2.05              0.04173 *  
    ## con.margin10        -0.8773     0.1550   -5.66          0.000000071 ***
    ## con.intent           0.2497     0.1184    2.11              0.03651 *  
    ## con.margin15        -0.5476     0.1767   -3.10              0.00230 ** 
    ## win10:con.margin10   1.0740     0.2840    3.78              0.00022 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13.3 on 156 degrees of freedom
    ##   (469 observations deleted due to missingness)
    ## Multiple R-squared:  0.792,  Adjusted R-squared:  0.784 
    ## F-statistic: 99.2 on 6 and 156 DF,  p-value: <0.0000000000000002

``` r
summary(lab.m3)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * lab.margin10 + lab.intent + 
    ##     lab.margin15, data = lab)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -69.27 -11.09   0.63  10.36  73.50 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value            Pr(>|t|)    
    ## (Intercept)         82.6534     8.9992    9.18 0.00000000000000028 ***
    ## short10              0.1669     0.0764    2.18              0.0306 *  
    ## win10               -6.1240     5.7599   -1.06              0.2894    
    ## lab.margin10        -1.2530     0.2053   -6.10 0.00000000815731028 ***
    ## lab.intent           0.0622     0.1619    0.38              0.7012    
    ## lab.margin15        -0.6718     0.2036   -3.30              0.0012 ** 
    ## win10:lab.margin10   1.0724     0.2818    3.81              0.0002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.2 on 153 degrees of freedom
    ##   (472 observations deleted due to missingness)
    ## Multiple R-squared:  0.66,   Adjusted R-squared:  0.647 
    ## F-statistic: 49.5 on 6 and 153 DF,  p-value: <0.0000000000000002

``` r
summary(ld.m3)
```

    ## 
    ## Call:
    ## lm(formula = short15 ~ short10 + win10 * ld.margin10 + ld.intent + 
    ##     ld.margin15, data = ld)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -42.64  -8.76  -1.43   6.88  50.19 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value      Pr(>|t|)    
    ## (Intercept)         48.984      7.348    6.67 0.00000000055 ***
    ## short10              0.110      0.062    1.77         0.079 .  
    ## win10               38.751      5.740    6.75 0.00000000035 ***
    ## ld.margin10         -0.443      0.195   -2.27         0.025 *  
    ## ld.intent            0.188      0.196    0.96         0.340    
    ## ld.margin15         -0.769      0.191   -4.03 0.00008976071 ***
    ## win10:ld.margin10    0.656      0.357    1.84         0.068 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.4 on 141 degrees of freedom
    ##   (484 observations deleted due to missingness)
    ## Multiple R-squared:  0.856,  Adjusted R-squared:  0.85 
    ## F-statistic:  140 on 6 and 141 DF,  p-value: <0.0000000000000002
