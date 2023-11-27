# 0. GOAL ----
# Retrieve the place where the letter was written and get its geographical 
# coordinates. Then, the letters will be plotted on a map through time in a 
# shiny app, allowing to compare it with central hubs of other writings.
# The place of writing is often but not always mentioned in the letter.

# With this information, we can make subgroups of letters based on geographical 
# location, and examine whether there are different ideas mentioned in the 
# letters. For example, we can compare the letters written in the US with those
# in Europe, or those written in the same city but in different years. One 
# problem: there are a lot of cities with the same name in different states or/and
# in different countries. If the place of writing is missing in the letter or 
# cannot be clearly specified, this script tries to extract the place of writing
# from other letters written by the same person within a specified time frame
# (30 days) to obtain the most likely place of writing.

# 1. LIBRARIES ----
library(tidyverse)
library(data.table)

# 2. READING ALL LETTER DATA FROM FOUNDERS ONLINE ----
letters <-  readRDS(file = "letters.rds")

# 3. EXTRACTING CITY NAMES FROM FIRST PART/SENTENCES OF THE LETTER ----

## 3.1. Extracting first sentence ----

# create subset of data to work with 
# TODO: optimize code, look into parallel processing with furrr to speed up the 
# analysis

letter_ss <- letters %>% filter(start.year >= 1700 & start.year <= 1790) %>% 
  select(authors,recipients,content)

# Extract first sentence in which location is mentioned
letter_ss$first <- stringr::word(string = letter_ss$content, start = 1, end = 10,
                                 sep = stringr::fixed(" "))

# Remove all special characters except space
letter_ss$first <- gsub("[^a-zA-Z ]", "", letter_ss$first)

letter_ss$first <- tolower(letter_ss$first)

# remove frequently occurring words that are not relevant to the city name
bad_words <- c("january", "february", "march", "april", "may", "june", "july", 
               "august","september","october","november","december",
               "janry", " jany","febry", "feby","decr", "octr", "octobr",
               "monday","tuesday", "wednesday", "thursday", "friday", "saturday","sunday",
               "1st","2nd","3rd","4th","5th","6th","7th", "7nd","8th","9th","10th","11th",
               "12th","13th","14th","15th","16th","17th","18th","19th","20th","21st",
               "22th","22nd","23th","23rd", "24th","25th","26th","27th","28th",
               "29th","30th","31th","31st", "22d", "maj gen", "most excellent", "morning",
               "to the president of the united states", "the president of the united states",
               "my best friend","most dear sir", "my dear son", "may it please your excellency",
               "my very dear friend", "not found from", "my dear friend and kinsman", "my dear george", 
               "my dear louisa", "my dearest louisa", "jonathan glover", "general washington",
               "william washington", "betty washington", "john washington","john stine washington","lund washington","martha washington", "samuel washington", "général washington", "sir washington",   "john glover", "nathanael greene","instructs greene", "john hancoc","robert hanson",
               "benjamin harrison", "robert hanson harrison,","william heath", "instructs heath", "nathaniel warner", "david waterbury",
               "to the citizens of","to the citizens of the county", "hamilton", "colonel", "sent wentworth",
               "fellow citizen of","fellow citizens of", "general", "peter webster", "john ine washington",
               "john augustine washington","james cleveland", "advance","allen mclane",
               "letter", "sir ", "colonel chester", "gentn", "harrison",  "stringer",
               "george washington", "franklin","john adams smith", "smith","dear johnson",
               "thomas jefferson","jefferson","dear jefferson", "john adams","warren","harriet",
               "transfer","shaw","post","only","portia","adams", "mon cher",
               "caroline","the united states","young","universal","humphreys",
               "united states","yours"," your","in order","yesterday","i had",
               "having had","we have","received", "you have", "dennis",
               "john langdon", "private", "septr","it is","my lord duke","monsieur",
               "thomas conway", "thomas nelson", "of the city","city of", 
               "benedict arnold", "brigantine", "vice president"," president",
               " men of","inclosed","of the", "to the","i have","gentlemen","copy",
               "copies","my lord", "page","office","agency","inclosing","i am",
               "mother", "papa","mamma","grandpapa","cousin","honoured","parents",
               "sister","brother","father","hond","madam","uncle","aunt", "honourd",
               "honorable", "honored", "the honor", "letters","news","contract",
               "decemr", "my dearest","my dear", "dear","my dearest friend","dearest","friend",
               "beloved","wife","james clinton","george clinton","henry clinton", "clinton",
               "general clinton","heath","james hill", "samuel holden","jedediah huntington",
               "samuel huntington"," lawrence"," henry lee","charles lee", "dear lee","orders lee",
               "camp near"," head qrs","hd qrs","in the vicinity of","head quarters","head qtr","benjamin lincoln",
               "lincoln","david mason"," george mason","morgan connor","daniel morgan","john morgan","thomas newton", "johnson",
               "not found to","edward norwood","joseph palmer","near","camp at","camp on","josiah quincy",
               "raleigh at sea","edmund randolph","mr. randolph","peyton randolph","randolph","raymond",
               "on reading", "in reading", "upon reading"," zebedee redding","philip van rensselaer",
               "joseph spencer", "oliver spencer","sterling complains","temple","thornton",
               "knox  baillie", "edward snickers","from the green", "bradford",
               "jonathan trumbull","joseph trumbull","to governors trumbull greene  weare",
               "verite union","union","vergennes", "walpole","william woodford"," instructs woodford",
               "john west", "lodge", "valentine", "todd", "john dickinson","day"," congress",
               "daniel smith", "philip", "william trent","hope", "anthony white","imperial",
               "saml cook", "john sullivan","the colony", "moore", "lord stirling","camp",
               "pray","william ramsay",	"alexander mcdougall", "william bartlett",
               "council","william cushing","george gregory", "liberty", "quarters",
               "henry babcock", "jones",  "joseph reed","joseph","lee", "arnold", 
               "stephen","esqr","head", "qrs", "before", "evening","response","light infantry",
               "lieutenant",  "benjamin", "tupper","register", "papers" ,"printed", 
               "burr bradley","james bowdoin","loammi baldwin","daniel cunyngham clymer",
               "committee","militia", "officers","augt","octbr","happy", "delivery",
               "acknowledge","septemr","septembr","sepr","return","pleasure",
               "commissioners","messenger"," jany","beg","leave","octbr","nights",
               "octor","extract","miles","baron","material"," governor","write",
               "wrote","writing","enclosed","resolves"," doctor","congress",
               "enclosd","background","conversations","honour","augst",
               "lieut","wheaton","informed","inclose","herewith","directed",
               "agreeable","army","assembly","representatives","ashamed",
               "articles","arrived","arrival", "situation","armstrong",
               "oclo","aprl","approve","approved","appointed","appointment",
               "applied","application","applications", "emigration", "period",
               "appears","apl","inform","accident","lovell","friendship","gray",
               "gerry","wilson","murray","genl washington","begs","imform", 
               "majo","otis","advice","company","williamson","benson","attorny",
               "barton","meredith","morgan","colo varick","spencer","paterson",
               "hammond","russell","jackson","girard","le roy","god","eliot",
               "taylor", "carroll","sweden","wayne","parker","sherman","mason",
               "stoddard","edwards","stark","replying","monroe","request","trumbull",
               "major", "steuben"," febr","indians","attacked","receipt"," glover",
               "gratz","greene","hancock","mckean","hugh mercer","james mercer",
               "dubois","ogden","returned","intended", "passing","thro",
               "left mount vernon","winthrop","captain turner", " capt turner",
               "enclose","ware bound","whately")

all_stop_words <- c(quanteda::stopwords("en"), 
                    stopwords::stopwords(source ="snowball"),  
                    stopwords::stopwords(source ="smart"),  
                    stopwords::stopwords(source ="nltk"),
                    stopwords::stopwords(source ="marimo"),
                    stopwords::stopwords(source ="stopwords-iso"),
                    "sir", "mr", "letter", "dear", "dr","dr—", "mrs", "pd", "sr", 
                    "january", "february", "march", "april", "may", "june", "july",
                    "august", "september","october","november","december", "thos",
                    "b","c","p","h","go", "aug", "sept","oct","nov","dec", "janry",
                    " jany","febry", "feby", "decr", "octr", "octobr","jan", 
                    "wm","tis", "ca","gw", "novr","monday","tuesday", "wednesday",
                    "thursday", "friday", "saturday","sunday", "octo", "novemr",
                    "1st","2nd","3rd","4th","5th","6th","7th", "7nd","8th","9th","10th","11th",
                    "12th","13th","14th","15th","16th","17th","18th","19th","20th",
                    "21st","22th","22nd","23th","23rd", "24th","25th","26th","27th",
                    "28th","29th","30th","31th","31st", "22d","tem","late",
                    "dearest","oclock", "o’clock","things","time","day","morning", "mr", "mr.","letter",
                    "mat","excellency","paper","les","circumstance","article","consequence",
                    "john","gentleman","reason","people","opinion","character",
                    "william", "favour","excelly","excellencys","excellency’s",
                    "williams","answer","shall","one","could","would", "upon","may","&","every","much","might", "with", "without","two","us","yet",
                    "since","also","therefore","however","never","ever","soon","say","take","give","well","see","mch","sir","mr","mr.","get","give",
                    "want","many","part","time", "wh","ditto","day","letter","esqr","mrs",
                    "letter","day","person","post","purpose","measure","answer",
                    "mat","subject","circumstance","manner","moment","gentleman",
                    "yesterday","instant","pa","week","par","night","event","object",
                    "paper","month","favour","favor", "reason","regard", "principle",
                    "matter","instance","question","time","inst", "degree","","occasion",
                    "honble","hour","behalf","particular","van","word", "correspondence",
                    "issue","lettre","mr") 

# These words in the stopwords list we actually do want to keep in the corpus
remove_from_stopwords <- c("new","pa","n","va","md","mill","ny","so","point",
                           "ny","nj","ga","nc","pa","vt", "md","nh","nh","sc","del",
                           "m","mt","st") 


all_stop_words <- all_stop_words[!all_stop_words %in% remove_from_stopwords]
all_stop_words <- c(all_stop_words, bad_words) 
all_stop_words <- unique(all_stop_words) # distinct stopwords

#all_stop_words <- all_stop_words %>% as.data.frame()

letter_ss$first <- tm::removeWords(letter_ss$first,all_stop_words)

# Remove words equal to or smaller than 1 characters
#letter_ss$first <- gsub("\\b\\w{1}\\b", "", letter_ss$first)

# Remove extra spaces created by the previous step
#letter_ss$first <- gsub("\\s{2,}", " ", letter_ss$first)

letter_ss$first <- str_squish(letter_ss$first)


# # Load the spaCy model that supports NER for your language. We'll use the English model
# spacy_initialize(model = "en_core_web_sm", ask = FALSE)
# 
# # Perform NER and extract entities
# parsed_data <- spacy_parse(letter_ss$first, 
#                          pos = FALSE,
#                          tag = FALSE,
#                          lemma = TRUE, 
#                          entity = TRUE, 
#                          multithread = TRUE)
# 
# 
# # Create an empty data frame to store the results
# results_df <- data.frame(city = character(0), 
#                          state = character(0), 
#                          country = character(0), 
#                          stringsAsFactors = FALSE)
# 
# # Create an empty list to store the results
# results_list <- list()
# 
# 
# # Iterate through the parsed data and extract entities for each document
# for (i in seq_along(parsed_text)) {
#   entities <- parsed_text[i]$entity
#   words <- parsed_text[i]$lemma
#   city <- ifelse("GPE_B" %in% entities, words[entities == "GPE_B"][1], NA)
#   state <- ifelse("GPE_B" %in% entities, words[entities == "GPE_B"][2], NA)
#   country <- ifelse("GPE_B" %in% entities, words[entities == "GPE_B"][3], NA)
#   
#   # Create a data frame for the extracted entities
#   entity_df <- data.frame(city = city, state = state, country = country, stringsAsFactors = FALSE)
#   
#   # Append the data frame to the results_list
#   results_list[[i]] <- entity_df
# }

letter_ss$first <- str_replace(letter_ss$first, "grosvenor square", "london")
letter_ss$first <- str_replace(letter_ss$first, "grovenor square", "london")
letter_ss$first <- str_replace(letter_ss$first, "grosr. sqr", "london")
letter_ss$first <- str_replace(letter_ss$first, "grosvr. square", "london")
letter_ss$first <- str_replace(letter_ss$first, "grovr. sqr.", "london")
letter_ss$first <- str_replace(letter_ss$first, "grosv. sqr.", "london")
letter_ss$first <- str_replace(letter_ss$first, "grosv. square", "london")
letter_ss$first <- str_replace(letter_ss$first, "grosv. square.", "london")
letter_ss$first <- str_replace(letter_ss$first, "grosvr. sqr.", "london")
letter_ss$first <- str_replace(letter_ss$first, "g. sqr", "london")
letter_ss$first <- str_replace(letter_ss$first, "g. sqr.", "london")
letter_ss$first <- str_replace(letter_ss$first, "grosr. sqr.", "london")
letter_ss$first <- str_replace(letter_ss$first, "grosv.sqr.", "london")
letter_ss$first <- str_replace(letter_ss$first, "grosvenor-square.", "london")
letter_ss$first <- str_replace(letter_ss$first, "soho square", "london")
#letter_ss$first <- str_replace(letter_ss$first, "dr.", "")
#letter_ss$first <- str_replace(letter_ss$first, "nov.", "")
#letter_ss$first <- str_replace(letter_ss$first, "aug.", "")
letter_ss$first <- str_replace(letter_ss$first, "sir.", "")
letter_ss$first <- str_replace(letter_ss$first, "auteuil", "paris")


# 4. CITY NAMES AND COORDINATES ----

## A) DATA SOURCES ----
# https://en.wikipedia.org/wiki/List_of_North_American_settlements_by_year_of_foundation
# https://opendata.stackexchange.com/questions/13613/is-there-a-dataset-for-historical-us-towns-and-roads
# https://query.wikidata.org/#select%20distinct%20%3FsLabel%20%3Flabel%20%3Fyear%20%3Fcoordinates%20%7B%0A%20%3Fs%20wdt%3AP31%2Fwdt%3AP279%2a%20wd%3AQ3327870%3B%0A%20%20%20%20wdt%3AP571%20%3Finception%3B%0A%20%20%20%20wdt%3AP625%20%3Fcoordinates%3B%0A%20%20%20%20wdt%3AP131%2Fwdt%3AP131%2Fwdt%3AP5086%20%3Flabel%0A%20bind%20%28year%28%3Finception%29%20as%20%3Fyear%29%0A%20filter%20%28%3Fyear%3C%3D1783%29%0A%20service%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22en%22%20%7D%0A%7D%20order%20by%20%3Fyear%20%3FsLabel

# select distinct ?sLabel ?label ?year ?coordinates {
#   ?s wdt:P31/wdt:P279* wd:Q3327870;
#   wdt:P571 ?inception;
#   wdt:P625 ?coordinates;
#   wdt:P131/wdt:P131/wdt:P5086 ?label
#   bind (year(?inception) as ?year)
#   filter (?year<=1783)
#   service wikibase:label { bd:serviceParam wikibase:language "en" }
# } order by ?year ?sLabel

# -> 1361 results

# city and coordinates data taken from:
# https://public.opendatasoft.com/explore/dataset/geonames-all-cities-with-a-population-1000/information/?disjunctive.cou_name_en&sort=name
#cities <- read.csv("us_cities.csv") %>% as.data.frame() %>% mutate(CITY=tolower(CITY))

#remove <- c("Point", "\\(", "\\)")
# There were several settlements named "Newport" in North America, including Newport, Rhode Island, and Newport, New Hampshire.
# Multiple settlements were named "Charleston," such as Charleston, South Carolina, and Charleston, New Hampshire.
# "Cambridge" was a name shared by different settlements, including Cambridge, Massachusetts, and Cambridge, New York.
# "Springfield" was a commonly used name, with Springfield, Massachusetts, and Springfield, Vermont, being prominent examples.
# Several settlements shared the name "Salem," including Salem, Massachusetts, and Salem, New Hampshire.

cities <- read.csv("../../Data/Cities/0_uscities_till1825.csv", sep = ',') %>%
    as.data.frame() %>% 
    rename(city = sLabel,
           state = label) %>% 
    mutate(city = tolower(city),
           coordinates = coordinates %>% 
               str_remove_all(paste(c("Point", "\\(", "\\)"), collapse = "|"))) %>% 
    separate("coordinates", c("longitude","latitude"), sep = " ",remove = T) %>% 
    mutate(longitude = as.numeric(longitude),
           latitude  = as.numeric(latitude)) %>% 
    distinct(city,state, .keep_all = TRUE)

#duplicate city names before 1783
duplicates <- cities$city[ cities$city %in% cities$city[duplicated(cities$city)] ] %>% 
    as.data.frame()
dim(duplicates)

#coord <- subset(cities, !(city %in% duplicates))

coord <- cities[!(duplicated(cities$city) | duplicated(cities$city, fromLast = TRUE)), ]
dim(coord)
coord <- coord %>% select(-year) %>% 
  rename(state2 = state)

uscities <- read.csv("../../Data/Cities/2_us_cities.csv", sep = ',') %>%
  as.data.frame() %>% 
  rename_with(tolower) %>% 
  mutate(city = tolower(city)) %>% 
  distinct(city,state_name, .keep_all = TRUE)


allcities <- read.csv("../../Data/Cities/allcities.csv", sep = ';') %>% as.data.frame() %>%
    mutate(Name = tolower(Name)) %>%
    rename(city = Name) %>%
    separate("Coordinates", c("latitude","longitude"), sep = " ",remove = F) %>%
    select(city,Country.name.EN ,Timezone,latitude,longitude) %>% 
    filter(str_detect(Timezone, "Europe|America"))

allcities$latitude <- as.numeric(gsub(",","",allcities$latitude))
allcities$longitude <- as.numeric(allcities$longitude)

# The (?i) modifier is used to make the regular expression 
# case-insensitive, and it surrounds the cities' names in a 
# capturing group. This is commonly used when you want to
# match text regardless of whether it's in uppercase or 
# lowercase. The entire regular expression, with the case-insensitive 
# modifier and word boundaries, is used to match city names in a 
# case-insensitive way and as whole words
pattern  <- paste0("(?i)(", paste0(cities$city,   collapse = "\\b|\\b"), "\\b)")
pattern2 <- paste0("(?i)(", paste0(uscities$city, collapse = "\\b|\\b"), "\\b)")

# 5. MATCHING CITIES IN THE LETTERS ----
# Includes some manual annotations for misspelled cities

# TODO: FUZZY MATCHING

letter_ss <- letter_ss %>% 
    ## A) city name ----------------------------------------------------------------
    mutate(city = case_when(str_detect(first, "montezillo") ~ "montezillo",
                            str_detect(first, "montizillo") ~ "montizillo",
                            str_detect(first, "montizello") ~ "montizello",
                            str_detect(first, "sirquincy")  ~ "quincy",
                            str_detect(first, "piscatua")   ~ "piscatua",
                            str_detect(first, "leyden")     ~ "leiden",
                            str_detect(first, "hague")      ~ "the hague",
                            str_detect(first, "allen town") ~ "allen town",
                            str_detect(first, "fishkill")   ~ "fishkill",
                            str_detect(first, "the haye")      ~ "the hague",
                            str_detect(first, "la haie")      ~ "the hague",
                            str_detect(first, "travellersrest")~ "travelers rest",
                            str_detect(first, "jacobs creek")~ "jacobs creek",
                            str_detect(first, "sunsbury")~ "sunbury",
                            str_detect(first, "esopus")~ "esopus",
                            str_detect(first, "gloster")~ "gloster",
                            str_detect(first, "cross creek")~ "cross creek",
                            str_detect(first, "marstrand")~ "marstrand",
                            str_detect(first, "masterland")~ "masterland",
                            str_detect(first, "the hage")   ~ "the hague",
                            str_detect(first, "epping forest")   ~ "epping forest",
                            str_detect(first, "new york")   ~ "new york",
                            str_detect(first, "newyork")    ~ "new york",
                            str_detect(first, "boston road")    ~ "boston road",
                            str_detect(first, "la coruña")  ~ "la coruña",
                            str_detect(first, "bourdeaux")  ~ "bordeaux",
                            str_detect(first, "bordeaux")  ~ "bordeaux",
                            str_detect(first, "jacksonburgh")  ~ "jacksonboro",
                            str_detect(first, "paimboeuf")  ~ "paimboeuf",
                            str_detect(first, "turners clove")  ~ "turners clove",
                            str_detect(first, "berkley county")  ~ "berkley county",
                            str_detect(first, "borden town")  ~ "borden town",
                            str_detect(first, "west point")  ~ "west point",
                            str_detect(first, "vaugirard")  ~ "paris",
                            str_detect(first, "newbury port")  ~ "newburyport",
                            str_detect(first, "nieuport")  ~ "newport",
                            str_detect(first, "new port")  ~ "newport",                    
                            str_detect(first, "madrid")  ~ "madrid",
                            str_detect(first, "neworleans") ~ "new orleans",
                            str_detect(first, "ampthill") ~ "ampthill",
                            str_detect(first, "leghorn")    ~ "livorno",
                            str_detect(first, "legn")    ~ "livorno",
                            str_detect(first, "orient")    ~ "lorient",
                            str_detect(first, "chaillot")    ~ "paris",
                            str_detect(first, "l orient")    ~ "lorient",
                            str_detect(first, "lorient")    ~ "lorient",
                            str_detect(first, "havre")    ~ "le havre",
                            str_detect(first, "ghent")    ~ "ghent",
                            str_detect(first, "helvoetsluys") ~ "hellevoetsluis",
                            str_detect(first, "leesburg") ~ "leesburg",
                            str_detect(first, "fort mercer") ~ "fort mercer",
                            str_detect(first, "hudsons river") ~ "hudsons river",
                            str_detect(first, "claverack")~ "claverack",
                            str_detect(first, "cortlandts manor") ~ "cortlandts manor",
                            str_detect(first, "newington green") ~ "newington green",
                            str_detect(first, "newingtongreen") ~ "newington green",
                            str_detect(first, "friendnewingtongreen") ~ "newington green",
                            str_detect(first, "whitemarsh")~ "whitemarsh",
                            str_detect(first, "bigg spring")~ "leesburg",
                            str_detect(first, "big spring")~ "leesburg",
                            str_detect(first, "princeton")~ "princeton",
                            str_detect(first, "fishkill")~ "fishkill",
                            str_detect(first, "nantes")~ "nantes",
                            str_detect(first, "perthamboy")~ "perth amboy",
                            str_detect(first, "fort pitt")~ "fort pitt",
                            str_detect(first, "harlem")~ "new york",
                            str_detect(first, "m vernon")~ "mount vernon",
                            str_detect(first, "mt vernon")~ "mount vernon",
                            str_detect(first, "elizabeth town")~ "elizabeth town",
                            str_detect(first, "elizabethtown")~ "elizabeth town",
                            str_detect(first, "elizth town")~ "elizabeth town",
                            str_detect(first, "frederick town")~ "frederick town",
                            str_detect(first, "east chester") ~ "east chester",
                            str_detect(first, "stony field") ~ "stony field",
                            str_detect(first, "richmond hill") ~ "richmond hill",
                            str_detect(first, "hampton falls") ~ "hampton falls",
                            str_detect(first, "philadelpa")    ~ "philadelphia",
                            str_detect(first, "hiladelphia")    ~ "philadelphia",
                            str_detect(first, "phyladelphia")  ~ "philadelphia",
                            str_detect(first, "philadelp")     ~ "philadelphia",
                            str_detect(first, "phildelphia")   ~ "philadelphia",
                            str_detect(first, "charlestown")   ~ "charlestown",
                            str_detect(first, "washington dc")   ~ "washington city",
                            str_detect(first, "washington city")   ~ "washington city",
                            str_detect(first, "petersburg")     ~ "saint petersburg",
                            str_detect(first, "st petersbourg") ~ "saint petersburg",
                            str_detect(first, "philad.")       ~ "philadelphia",
                            str_detect(first, "phila")         ~ "philadelphia",
                            str_detect(first, "hiladelphia") ~ "philadelphia",
                            str_detect(first, "fredericksbg") ~ "fredericksburg",
                            str_detect(first, "fredericksburg") ~ "fredericksburg",
                            str_detect(first, "city of washington") ~ "washington city",
                            str_detect(first, "george town") ~ "georgetown",
                            str_detect(first, "geo town") ~ "georgetown",
                            str_detect(first, "fort constitution") ~ "fort constitution",
                            str_detect(first, "fort george") ~ "fort george",
                            str_detect(first, "morrissania") ~ "new york",
                            str_detect(first, "eorg town") ~ "georgetown",
                            str_detect(first, "new ark") ~ "newark",
                            str_detect(first, "preakness") ~ "preakness",
                            str_detect(first, "winter hill") ~ "winter hill",
                            str_detect(first, "prospect hill") ~ "prospect hill",
                            str_detect(first, "valentines hill") ~ "valentines hill",
                            str_detect(first, "fairy hill") ~ "fairy hill",
                            str_detect(first, "rocky hill") ~ "rocky hill",
                            str_detect(first, "phills hill") ~ "phills hill",
                            str_detect(first, "castle hill") ~ "castle hill",
                            str_detect(first, "ashley hill") ~ "ashley hill",
                            str_detect(first, "bald hill") ~ "bald hill",
                            str_detect(first, "keelers hill") ~ "keelers hill",
                            str_detect(first, "hill new milford") ~ "new milford",
                            str_detect(first, "spring hill") ~ "spring hill",
                            str_detect(first, "malvan hill") ~ "malvan hill",
                            str_detect(first, "malvern hill") ~ "malvern hill",
                            str_detect(first, "malbern hill") ~ "malvern hill",
                            str_detect(first, "quaker hill") ~ "quaker hill",
                            str_detect(first, "chesnut hill") ~ "chesnut hill",
                            str_detect(first, "bottle hill") ~ "bottle hill",
                            str_detect(first, "round hill") ~ "round hill",
                            str_detect(first, "rockey hill") ~ "rockey hill",
                            str_detect(first, "rocky hill") ~ "rocky hill",
                            str_detect(first, "lundrocky hill") ~ "rocky hill",
                            str_detect(first, "r hill") ~ "rocky hill",
                            str_detect(first, "ocky hill") ~ "rocky hill",
                            str_detect(first, "peeks hill") ~ "peeks hill",
                            str_detect(first, "baltimore") ~ "baltimore",
                            str_detect(first, "hillsborough township") ~ "hillsborough township",
                            str_detect(first, "jamaica plain") ~ "jamaica plain",
                            str_detect(first, "wmsburg") ~ "williamsburg",
                            str_detect(first, "charles town") ~ "charlestown",
                            str_detect(first, "new kent") ~ "new kent",
                            str_detect(first, "okeepsie") ~ "poughkeepsie",
                            str_detect(first, "dobbsferry") ~ "dobbs ferry",
                            str_detect(first, "havredegrace") ~ "havre de grace",
                            str_detect(first, "fort lee") ~ "fort lee",
                            str_detect(first, "frankfort on the main") ~ "frankfurt am main",
                            str_detect(first, "bergen county") ~ "bergen county",
                            str_detect(first, "fort montgomery") ~ "fort montgomery",
                            str_detect(first, "orange town") ~ "orange town",
                            str_detect(first, "orangetown") ~ "orange town",
                            str_detect(first, "orange county") ~ "orange county",
                            str_detect(first, "passaic falls") ~ "passaic falls",
                            str_detect(first, "passy") ~ "paris",
                            str_detect(first, "passi") ~ "paris",
                            str_detect(first, "reading town") ~ "reading town",
                            str_detect(first, "reading furnace") ~ "reading furnace",
                            str_detect(first, "rye neck") ~ "rye neck",
                            str_detect(first, "brunswic") ~ "brunswick",
                            str_detect(first, "princetown") ~ "princetown",
                            str_detect(first, "mount vernon") ~ "mount vernon",
                            str_detect(first, "morris town") ~ "morristown",
                            str_detect(first, "ware church") ~ "ware church",
                            str_detect(first, "warwick furnace") ~ "warwick furnace",
                            str_detect(first, "warwick bucks") ~ "warwick bucks",
                            str_detect(first, "mount washington") ~ "mount washington",
                            str_detect(first, "mt washington") ~ "mount washington",
                            str_detect(first, "worcester township") ~ "worcester township",
                            str_detect(first, "n york") ~ "new york",
                            str_detect(first, "york town") ~ "york town",
                            str_detect(first, "highlands") ~ "highlands",  
                            str_detect(first, "new windsor") ~ "new windsor",
                            str_detect(first, "newhaven") ~ "new haven",
                            str_detect(first, "new utrecht") ~ "new york city",
                            str_detect(first, "plymo") ~ "plymouth",
                            str_detect(first, "plym") ~ "plymouth",
                            str_detect(first, "plimouth") ~ "plymouth",
                            str_detect(first, "piccadilly") ~ "london",
                            str_detect(first, "eltham") ~ "london",
                            str_detect(first, "chalsea") ~ "chelsea",
                            str_detect(first, "portobacco") ~ "port tobacco", 
                            str_detect(first, "porttobacco") ~ "port tobacco",
                            str_detect(first, "raspberry plain") ~ "leesburg",
                            str_detect(first, "rasberry plain") ~ "leesburg",
                            str_detect(first, "cheapside") ~ "cheapside",
                            str_detect(first, "skipton") ~ "skipton",
                            str_detect(first, "gunstonhall") ~ "gunston hall", 
                            str_detect(first, "gunston hall") ~ "gunston hall",
                            str_detect(first, "winchister") ~ "winchester", 
                            str_detect(first, "craven") ~ "london",
                            str_detect(first, "portsmo") ~ "portsmouth",
                            str_detect(first, "mount airy") ~ "mount airy", 
                            str_detect(first, "mountvernon") ~ "mount vernon",
                            str_detect(first, "mount vernon") ~ "mount vernon", 
                            str_detect(first, "mount varnon") ~ "mount vernon",
                            str_detect(first, "mt vernon") ~ "mount vernon", 
                            str_detect(first, "king william") ~ "king william", 
                            str_detect(first, "alexandria") ~ "alexandria", 
                            str_detect(first, "alexana") ~ "alexandria",
                            str_detect(first, "little england") ~ "little england",
                            str_detect(first, "stewarts crossings") ~ "stewarts crossings",
                            str_detect(first, "mass bay") ~ "massachusetts bay",
                            str_detect(first, "yough river") ~ "yough river",
                            str_detect(first, "ashly river") ~ "ashly river",
                            str_detect(first, "mattapony river") ~ "mattapony river",
                            str_detect(first, "warburton") ~ "warburton",
                            str_detect(first, "charlottesville") ~ "charlottesville",
                            str_detect(first, "braintre") ~ "braintree",
                            str_detect(first, "saratoga") ~ "saratoga",
                            str_detect(first, "tionderoga") ~ "tionderoga",
                            str_detect(first, "tyonderoga") ~ "tionderoga",
                            str_detect(first, "ticonderoga") ~ "tionderoga",
                            str_detect(first, "amstdm") ~ "amsterdam",
                            str_detect(first, "golden square") ~ "london",
                            str_detect(first, "london") ~ "london",
                            str_detect(first, "harford town") ~ "harford town",
                            str_detect(first, "campridge") ~ "cambridge",
                            str_detect(first, "cambreg") ~ "cambridge",
                            str_detect(first, "water town") ~ "watertown",
                            str_detect(first, "pecks kills")~ "peekskill",
                            str_detect(first, "peckskill")  ~ "peekskill",
                            str_detect(first, "peekskill")  ~ "peekskill",
                            str_detect(first, "peeks kill") ~ "peekskill",
                            str_detect(first, "peaks kiln") ~ "peekskill",
                            str_detect(first, "philipsburg")~ "philipsburg",
                            str_detect(first, "bethelem")~ "bethlehem",
                            str_detect(first, "heckenseck") ~ "heckenseck",
                            str_detect(first, "hackensack") ~ "hackensack",
                            str_detect(first, "falls of delaware") ~ "delaware falls",
                            str_detect(first, "falls township") ~ "falls township",
                            str_detect(first, "turin") ~ "turin",
                            str_detect(first, "sussex") ~ "sussex",
                            str_detect(first, "germantown") ~ "philadelphia",
                            str_detect(first, "bucks county") ~ "bucks county",
                            str_detect(first, "amwell") ~ "amwell",
                            str_detect(first, "corriels ferry") ~ "corriels ferry",
                            str_detect(first, "keiths") ~ "keiths",
                            str_detect(first, "versailles") ~ "versailles",
                            str_detect(first, "dijon") ~ "dijon",
                            str_detect(first, "paramus") ~ "paramus",
                            str_detect(first, "new town") ~ "new town",
                            str_detect(first, "haddonfield") ~ "haddonfield",
                            str_detect(first, "grafton street") ~ "dublin",
                            str_detect(first, "montego bay") ~ "montego bay",
                            str_detect(first, "oyster ponds") ~ "oyster pond",
                            str_detect(first, "baskenridge") ~ "basking ridge",
                            str_detect(first, "white plains") ~ "white plains",
                            str_detect(first, "whiteplains") ~ "white plains",
                            # corunna
                            # bilbao
                            # brest 
                            # msterdam
                            # rotterdam
                            # ferrol
                            # bayonne
                            # minden
                            # bruxelles
                            # dobbss ferrey
                            # st germain
                            # lausanne
                            # amiens
                            # libourne
                            # vlles
                            # dunkirk
                            
                            .default = stringr::str_extract(letter_ss$first, pattern))) %>% 
  ## B) state name ---------------------------------------------------------------
    mutate(state = case_when(str_detect(first, " mass") ~ "MA",
                         str_detect(first, " ny ") ~ "NY",
                         str_detect(first, " ny") ~ "NY",
                         str_detect(first, " nj ") ~ "NJ",
                         str_detect(first, " nj") ~ "NJ",
                         str_detect(first, " nj") ~ "NJ",
                         str_detect(first, " ga ") ~ "GA",
                         str_detect(first, " nc ") ~ "NC",
                         str_detect(first, " pa ") ~ "PA",
                         str_detect(first, " vt ") ~ "VT",
                         str_detect(first, " md ") ~ "MD",
                         str_detect(first, " nh ") ~ "NH",
                         str_detect(first, " nh") ~ "NH",
                         str_detect(first, " sc ") ~ "SC",
                         str_detect(first, " del ") ~ "DE",
                         str_detect(first, "newyork") ~ "NY",
                         str_detect(first, "n york") ~ "NY",
                         str_detect(first, "penn") ~ "PA",
                         str_detect(first, "chester pa") ~ "PA",
                         str_detect(first, "county pa") ~ "PA",
                         str_detect(first, "chesnut hill pa") ~ "PA",
                         str_detect(first, "dc") ~ "DC",
                         str_detect(first, "maryland") ~ "MD",
                         str_detect(first, "havre de grace") ~ "MD",
                         str_detect(first, "george town potomac") ~ "DC",
                         str_detect(first, "george town columa") ~ "DC",
                         str_detect(first, "george town columbia") ~ "DC",
                         str_detect(first, "west point") ~ "NY",
                         str_detect(first,"alabama") ~ "AL",
                         str_detect(first,"alaska") ~"AK",
                         str_detect(first,"arizona") ~"AZ",
                         str_detect(first,"arkansas") ~"AR",
                         str_detect(first,"american samoa") ~"AS",
                         str_detect(first,"california") ~"CA",
                         str_detect(first,"colorado") ~"CO",
                         str_detect(first,"washington co") ~"CO",
                         str_detect(first,"cola") ~"CO",
                         str_detect(first,"coa") ~"CO",
                         str_detect(first,"coo") ~"CO",
                         str_detect(first,"george town co") ~"CO",
                         str_detect(first,"connecticut") ~"CT",
                         str_detect(first,"conn") ~"CT",
                         str_detect(first,"danbury conn") ~"CT",
                         str_detect(first,"delaware") ~"DE",
                         str_detect(first,"dover del") ~"DE",
                         str_detect(first,"district of columbia") ~"DC",
                         str_detect(first,"columbia") ~"DC",
                         str_detect(first,"florida") ~"FL",
                         str_detect(first,"georgia") ~"GA",
                         str_detect(first,"guam") ~"GU",
                         str_detect(first,"hawaii") ~"HI",
                         str_detect(first,"idaho") ~"ID",
                         str_detect(first,"illinois") ~"IL",
                         str_detect(first,"alexandria") ~ "VA",
                         str_detect(first,"salem") ~ "VA",
                         str_detect(first,"indiana") ~"IN",
                         str_detect(first,"iowa") ~"IA",
                         str_detect(first,"kansas") ~"KS",
                         str_detect(first,"kentucky") ~"KY",
                         str_detect(first,"louisiana") ~"LA",
                         str_detect(first,"maine") ~"ME",
                         str_detect(first,"maryland") ~"MD",
                         str_detect(first,"massachusetts") ~"MA",
                         str_detect(first,"massts") ~"MA",
                         str_detect(first,"braintree") ~"MA",
                         str_detect(first,"jacksonboro") ~"SC",
                         str_detect(first,"london") ~"-",
                         str_detect(first,"paris") ~ "-",
                         #str_detect(first,"philadelphia") ~"PA",
                         str_detect(city, "philadelphia") ~ "PA",
                         str_detect(first,"mount vernon") ~"VA",
                         str_detect(first,"michigan") ~"MI",
                         str_detect(first,"minnesota") ~"MN",
                         str_detect(first,"mississippi") ~"MS",
                         str_detect(first,"missouri") ~"MO",
                         str_detect(first,"montana") ~"MT",
                         str_detect(first,"nebraska") ~"NE",
                         str_detect(first,"nevada") ~"NV",
                         str_detect(first,"new hampshire") ~"NH",
                         str_detect(first,"newhamshire") ~"NH",
                         str_detect(first,"hampshire") ~"NH",
                         str_detect(first,"n hamps") ~"NH",
                         str_detect(first,"charlestown nh") ~"NH",
                         str_detect(first,"durham nh") ~"NH",
                         str_detect(first,"exeter nh") ~"NH",
                         str_detect(first,"new hampshire") ~"NH",
                         str_detect(first,"newhampshire") ~"NH",
                         str_detect(first,"new hampshe") ~"NH",
                         str_detect(first,"new jersey") ~"NJ",
                         str_detect(first,"n jersy") ~"NJ",
                         str_detect(first,"n jersey") ~"NJ",
                         str_detect(first,"new mexico") ~"NM",
                         str_detect(first,"new york") ~"NY",
                         str_detect(city,"new york") ~"NY",
                         str_detect(first,"state of n york") ~"NY",
                         str_detect(first,"washington ny") ~"NY",
                         str_detect(first,"washington nyst") ~"NY",
                         str_detect(first,"north carolina") ~"NC",
                         str_detect(first,"no carolina") ~"NC",
                         str_detect(first,"north carolinia") ~"NC",
                         str_detect(first,"northcarolina") ~"NC",
                         str_detect(first,"n carolina") ~"NC",
                         str_detect(first,"ncar") ~"NC",
                         str_detect(first,"edenton nc") ~"NC",
                         str_detect(first,"mount airy") ~ "NC", 
                         str_detect(first,"north dakota") ~"ND",
                         str_detect(first,"northern mariana islands") ~"MP",
                         str_detect(first,"ohio") ~"OH",
                         str_detect(first,"oklahoma") ~"OK",
                         str_detect(first,"oregon") ~"OR",
                         str_detect(first,"pennsylvania") ~"PA",
                         str_detect(first,"pensilvania") ~"PA",
                         str_detect(first,"pensylvania") ~"PA",
                         str_detect(first,"penna") ~"PA",
                         str_detect(first,"pensylva") ~"PA",
                         str_detect(first,"bristol pa") ~"PA",
                         str_detect(first,"carlisle pa") ~"PA",
                         str_detect(first,"doylestown pa") ~"PA",
                         str_detect(first,"goshen pa") ~"PA",
                         str_detect(first,"easton pa") ~"PA",
                         str_detect(first,"at the forks of delaware") ~"PA",
                         str_detect(first,"puerto rico") ~"PR",
                         str_detect(first,"rhode island") ~"RI",
                         str_detect(first,"state rhode islandc") ~"RI",
                         str_detect(first,"rhode isld") ~"RI",
                         str_detect(first,"rhodeisland") ~"RI",
                         str_detect(first,"east greenwich ri") ~"RI",
                         str_detect(first,"east greenwich r island") ~"RI",
                         str_detect(first,"tiverton ri") ~"RI",
                         str_detect(first,"bristol ri") ~"RI",
                         str_detect(first,"south carolina") ~"SC",
                         str_detect(first,"so carolina") ~"SC",
                         str_detect(first,"camden sc") ~"SC",
                         str_detect(first,"charleston sc") ~"SC",
                         str_detect(first,"south dakota") ~"SD",
                         str_detect(first,"tennessee") ~"TN",
                         str_detect(first,"texas") ~"TX",
                         str_detect(first,"trust territories") ~"TT",
                         str_detect(first,"utah") ~"UT",
                         str_detect(first,"vermont") ~"VT",
                         str_detect(first,"virginia") ~ "VA",
                         str_detect(first,"virga") ~ "VA",
                         str_detect(first,"chesterfield co ho") ~ "VA",
                         str_detect(first,"chesterfield ct hs") ~ "VA",
                         str_detect(first,"chesterfield ct ho") ~ "VA",
                         str_detect(first,"chesterfield ct") ~ "VA",
                         str_detect(first,"chesterfield court ho") ~ "VA",
                         str_detect(first,"chesterfield court house") ~ "VA",
                         str_detect(first,"chesterfield co") ~ "VA",
                         str_detect(first, "gunstonhall") ~ "VA", 
                         str_detect(first, "gunston hall") ~ "VA", 
                         str_detect(first,"dumfries va") ~ "VA",
                         str_detect(first, "king william") ~ "VA", 
                         str_detect(first,"virgin islands") ~"VI",
                         str_detect(first,"washington") ~"WA",
                         str_detect(first,"west virginia") ~"WV",
                         str_detect(first,"wisconsin") ~"WI",
                         str_detect(first,"wyoming") ~"WY",
                         str_detect(first, " va") ~ "VA",
                         str_detect(first, "hiladelphia")  ~ "PA",
                         str_detect(first, "providence")  ~ "RI",
                         str_detect(first, "boston")  ~ "MA",
                         str_detect(first, "baltimore")  ~ "MD",
                         str_detect(first, "monticello")  ~ "VA",
                         str_detect(first, "wilmington d")  ~ "DE")) %>% 
  ## C) country name  ------------------------------------------------------------
    mutate(country = case_when(str_detect(first, "england") ~ "england",
                        str_detect(city, "london") ~ "england",
                        str_detect(first, "mill prison plymouth") ~ "england",
                        str_detect(first, "mill prison") ~ "england",
                        str_detect(first, "brittany") ~ "france", 
                        str_detect(first, "new orleans") ~ "USA",
                        str_detect(first, "orleans") ~ "france",
                        str_detect(city, "paris") ~ "france", 
                        str_detect(first, "nantes") ~ "france",
                        str_detect(first, "france") ~ "france",
                        str_detect(first, "turin") ~ "italy",
                        str_detect(first, "spain") ~ "spain",
                        str_detect(city, "marstrand")~ "sweden",
                        str_detect(city, "masterland")~ "sweden",
                        str_detect(first, "normandy") ~ "france",
                        str_detect(first, "austria") ~ "austria"))

unique(letter_ss$city)
table(letter_ss$city)

letter_ss <- letter_ss %>%
  mutate(country = case_when(!is.na(state) & state != "-" ~"USA",
                           TRUE ~ NA))

letter_ss <- letter_ss %>%
  left_join(coord %>% select(city,state2), by = "city")

# 6. MOST LIKELY PLACE OF PUBLICATION/WRITING ----

## A) create a time variable for each letter ----
letter_ss <- letter_ss %>% 
    mutate(time = date(from),
           timestart = min(time), 
           
           # number of days from earliest sending date in revolution (1775-04-19)
           #to sending date of current row 
           timediff = as.numeric(time - timestart)) %>%
    arrange(time) %>% 
    select(-time,-timestart) 

# subset of 
temp_city <- letter_ss %>% 
  select(authors, timediff, city) %>% 
  rename(timedifference = timediff) %>%
  filter(!is.na(city))

# Convert to data.table
setDT(temp_city) 

temp_state <- letter_ss %>% 
  select(authors,timediff,state) %>% 
  rename(timedifference = timediff) %>%
  filter(!is.na(state))

# Convert to data.table
setDT(temp_state) 

## B) create functions to find most likely place and state of publication if missing ----

#   This function can be applied when the place of publication/ writing of an 
#   author at a particular time point is unknown. If so, search for a time span
#   of -30 and +30 days around that time point to see if the author has sent 
#   another letter in which the location is being mentioned. If so, return that 
#   location as the most likely place where the letter was written. This location
#   can then be used/copied, assuming that the author lived in the same place 
#   during the time interval of max 30 days

destination_city <- function(rec, dt) {
  
  setDT(temp_city) # Convert temp to a data.table
  
  # Create an index on the authors and time-difference columns
  setkeyv(temp_city, c("authors", "timedifference"))
  
  # Filter the rows using vectorized filtering
  temp2 <- temp_city[authors %in% rec & timedifference >= (dt - 30) & timedifference <= (dt + 30)] %>% 
  
  # Find the row with the minimum absolute difference
  slice(which.min(abs(dt - timedifference))) 
  #temp2 <- temp2[abs(dt - timedifference) == min(abs(dt - timedifference))]
  
  destin <- ifelse(nrow(temp2) == 1, temp2$city, NA)
  return(destin)
}

#TODO: replace city with state in the above function

# find state
destination_state <- function(rec, dt) {
  # Convert temp to a data.table
  
  # Create an index on the authors and time-difference columns
  setkeyv(temp_state, c("authors", "timedifference"))
  
  # Filter the rows using vectorized filtering
  temp2 <- temp_state[authors %in% rec & timedifference >= (dt - 30) & timedifference <= (dt + 30)] %>% 
    
    # Find the row with the minimum absolute difference
    slice(which.min(abs(dt - timedifference))) 
  #temp2 <- temp2[abs(dt - timedifference) == min(abs(dt - timedifference))]
  
  destin <- ifelse(nrow(temp2) == 1, temp2$state, NA)
  
  return(destin) # returns with a retrieved city name, if any
}

# subset of data to check the city and state functions
destination_input <- letter_ss %>% slice(1:1000)

#check how many are missing before applying function
sum(is.na(destination_input$city)) #259 na's
sum(is.na(destination_input$state)) #598 na's
n_distinct(destination_input$city)

# retrieve cities from surrounding data if missing
destination_input$city <- ifelse(is.na(destination_input$city), 
            purrr::map2(destination_input$authors,   # input vector 1 name of author
                        destination_input$timediff,  # input vector 2 time difference
                        destination_city),           # function to apply
                        destination_input$city) %>%  # city name when it was not missing
                        unlist()

#check how many are missing after applying city function
sum(is.na(destination_input$city)) #149 na's

# if city name is still missing, try to look it up in us_cities df
destination_input <- destination_input %>% 
    as.data.frame() %>% 
    mutate(city_sent_from = case_when(!is.na(city)  ~ city,
              .default = stringr::str_extract(destination_input$first, pattern2))) %>% 
    select(-city)

sum(is.na(destination_input$city_sent_from)) #116 na's

# retrieve state from surrounding data if missing
destination_input$state <- ifelse(is.na(destination_input$state), 
             purrr::map2(destination_input$authors,
                         destination_input$timediff,
                         destination_state),
                         destination_input$state) %>% 
                         unlist()

#check how many are missing after applying state function
sum(is.na(destination_input$state)) #496 na's

destination_input <- destination_input %>% 
  rename(state_sent_from = state) %>% 
  select(authors:content,first,timediff,city_sent_from,state_sent_from,country)

# 7. MOST LIKELY PLACE OF RECEIPT ----

# Has the recipient of a letter been an author himself of a 
# letter in which the place is mentioned? We search again in a time
# span of 30 days before and 30 days after the moment in which 
# the recipient got the letter for possible information on his/her
# location. If so, we have the most likely place where the letter
# was sent to in addition to the knowledge where the letter was written. This 
# can be of usage when one want to estimate the travel time of the letter (i.e.
# in the context of optimizing delta t, see time -respecting network analysis
# with pathpy elsewhere in this project).

# retrieve place where recipient was from surrounding data
city_sent_to <- purrr::map2(destination_input$recipients,
                            destination_input$timediff,
                            destination_city) %>% 
                            unlist()

# retrieve statewhere recipient was from surrounding data
state_sent_to <- purrr::map2(destination_input$recipients,
                             destination_input$timediff,
                             destination_state) %>% 
                             unlist()

# adding vectors of city and state where recipient lived to the original df 
destination_input <- cbind(destination_input,city_sent_to,state_sent_to) 
glimpse(destination_input)


# 8. GET COORDINATES OF PLACE OF PUBLICATION AND RECEIPT ----

# get longitude and latitude information from external file
destination_input <- destination_input %>% 
  left_join(cities, by = c("city_sent_from" = "city",
                           "state_sent_from" = "state")) %>% 
  rename(lon_from = longitude, lat_from = latitude)

destination_input <- destination_input %>% 
  left_join(cities, by = c("city_sent_to" = "city",
                           "state_sent_to" = "state")) %>% 
  rename(lon_to = longitude, lat_to = latitude)

destination_input$city_sent_from <- ifelse(is.na(destination_input$city_sent_from), 
                                           destination_input %>% left_join(coord, by = c("city_sent_from" = "city")),
                                  destination_input$state) %>% 
  
  
# first merge with unique cities coordinates
destination_input <- destination_input %>% left_join(coord, by = c("city_sent_from" = "city")) %>% 
  rename(lon_from = longitude, lat_from = latitude)

destination_input <- destination_input %>% left_join(coord, by = c("city_sent_to" = "city")) %>% 
  rename(lon_to = longitude, lat_to = latitude)


# if state is not empty, we use state in the join to get the correct cities coordinates  
destination_input$state <- ifelse(is.na(destination_input$state), 
                                  purrr::map2(destination_input$authors,destination_input$timediff,destination_state),
                                  destination_input$state) %>% 

  destination_input <- destination_input %>% as.data.frame() %>% 
  mutate(thijs = case_when(!is.na(city_sent_from) & !is.na(state_sent_from)  ~ city,
                                    .default = stringr::str_extract(destination_input$first, pattern2))) %>% 
  select(-city)

# if state is empty
letter_ss <- letter_ss %>% left_join(cities, by = "city") 

letter_ss <- letter_ss %>% 
  group_by(city,longitude, latitude) %>% 
    mutate(city_count = n()) %>% 
  ungroup()

# 9. MAP COORDINATES ON WORLD MAP THROUGH TIME ----

# - Do the same with coordinates of books (ecco and evans) so see major hubs of
#   publication in the US and UK through time 
# - this can be used to spot potential influence of written books on letters  
# - see shiny app
