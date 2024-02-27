# 0. GOAL ----
# Retrieve the place where the letter was written and get its geographical
# coordinates. Then, the letters will be plotted on a map. This geodata
# allows to find central hubs of writings. Also, with this information we can
# make subgroups of letters based on geographical location, and examine whether
# there are different ideas mentioned in the letters. For example, we can
# compare the letters written in the US with those in Europe, or those written
# in the same city but in different years.

# There are a lot of cities with the same name in different states or/
# and in different countries. If the place of writing is missing in the letter or
# cannot be clearly specified, this script tries to extract the place of writing
# from other letters written by the same person within a specified time frame
# (30 days) to obtain the most likely place of writing.

# 1. LIBRARIES ----
library(tidyverse)
library(data.table)
library(future)
library(stringi)
library(furrr)
library(quanteda)
library(NLP)
require(openNLP)
require(openNLPdata)

# 2. LOAD PREPROCESSED FO LETTER DATA ----
letter_ss <- readRDS(file = "data/processed/founders/ffc_preprocessed.rds") %>%
  as.data.frame() %>%
  slice(1:1000)

# 3. EXTRACTING AND PREPROCESSING FIRST SENTENCE ----

## 1. Preprocessing ----
# Extract first sentence in which place of publication is often mentioned
letter_ss$first_sentence <- word(string = letter_ss$text,
                                 start  = 1,
                                 end    = 15,
                                 sep    = fixed(" "))

# Extract text between [ and ] and put it in a separate column
letter_ss$location <- str_extract(letter_ss$first_sentence, "\\[(.*?)\\]")
letter_ss$location <- gsub("[0-9]", "", letter_ss$location)
letter_ss$location <- tolower(letter_ss$location)
letter_ss$location <- gsub("[^a-zA-Z -]", "", letter_ss$location)

# Add spaces around commas, replace long dash with space, add spaces after 'Sir'
add_spaces_and_replace <- function(text) {
  # Replace comma with space around it
  text <- gsub(',', ' , ', text)
  # Replace long dash (em dash) with space
  text <- gsub('—', ' ', text)
  # Add space after 'Sir' if it is directly followed by another word
  text <- gsub('Sir', 'Sir ', text)
  # Add space after dot if it is followed by another word
  text <- gsub('Sir.', 'Sir ', text)
  # Return the modified text
  return(text)
}

# Applying the function to first_sentence
letter_ss$first_sentence <- sapply(letter_ss$first_sentence, add_spaces_and_replace)

# Remove all special characters in first sentence except space, dash
letter_ss$first_sentence <- gsub("[^a-zA-Z -]", "", letter_ss$first_sentence)

## 1. Remove sender and receiver names from first sentence ----
# We split the words in sender_receiver_names, constructs a regular
# expression pattern, and then removes those words from the first_sentence if
# present, thereby further reducing the string and removing irrelevant nouns

num_cores <- parallel::detectCores()
cat("Number of CPU cores:", num_cores, "\n")

# Set up the multisession plan
plan(multisession, workers = num_cores - 1)

print("Start")
tic <- Sys.time()

# Get all names from senders and receivers
letter_ss <- letter_ss %>%
  mutate(sender_receiver_names = paste(authors, recipients, sep = " "),
         # Remove all non-alphanumeric characters (excluding spaces)
         sender_receiver_names = gsub("[^a-zA-Z0-9 ]", "", sender_receiver_names),
         # Remove all words that are shorter than 2 characters
         sender_receiver_names = str_replace_all(sender_receiver_names, "\\b\\w{1}\\b", "")
  )

# Remove names from first_sentence that occur in sender_receiver_names
remove_names_from_first_sentence <- function(names, sentence) {

  # Split the names into individual words
  words_to_remove <- unlist(str_split(names, "\\s+"))

  # Create a pattern for removing names
  pattern <- paste0("\\b", paste0(words_to_remove, collapse = "\\b|\\b"), "\\b")

  # Compile the regex pattern for better performance
  regex_pattern <- str_replace_all(pattern, fixed(" "), ".*?")

  # Replace all occurrences of the pattern in the sentence with an empty string
  cleaned_sentence <- str_replace_all(sentence, regex_pattern, "")

  return(cleaned_sentence)
}

# Parallelise the process of removing names
letter_ss <- letter_ss %>%
  mutate(
    first_sentence = future_map2(
                .x = sender_receiver_names,
                .y = first_sentence,
                .f = remove_names_from_first_sentence,
         .progress = TRUE
    )
  )

toc <- Sys.time()
print(paste0("Removing the names took ", round(toc - tic, digits = 2),"seconds"))

# Explicitly stop parallel processing
plan("sequential")

# Applying the function
#letter_ss <- letter_ss %>%
#  mutate(first_sentence = remove_names_from_first_sentence(sender_receiver_names, first_sentence)) #%>%
  #select(-sender_receiver_names)

## 3. Part-of-speech tagging ----
# Placenames generally belong to the category NNP (proper noun, singular)
source("scripts/functions/pos_tag.R")

# Remove empty, or very small first sentences
letter_ss <- letter_ss %>%
  filter(!is.na(first_sentence) &
           str_length(str_trim(first_sentence)) > 0 &
           str_trim(first_sentence) != "") %>%
  mutate(first_sentence = str_squish(first_sentence)) %>%
  filter(str_count(str_trim(first_sentence), "\\S+") >= 5
  )

# POS tagging on the first sentence
letter_ss$city_extraction <- pos_tag(letter_ss$first_sentence, pos_filter = c("NNP"))
letter_ss$first_sentence  <- tolower(letter_ss$first_sentence)
letter_ss$city_extraction <- tolower(letter_ss$city_extraction)

# Remove frequently occurring words that are not relevant for retrieving city
# and state name. This makes the string to search in much shorter!
bad_words <- c("january", "february", "march", "april", "may", "june", "july",
               "august","september","october","november","december", "duplicate",
               "janry", " jany","febry", "feby","decr", "octr", "octobr","rogers",
               " februar", "monday","tuesday", "wednesday", "thursday", "friday",
               "saturday", "letter not found from", "document not found","fryday",
               "sunday","1st","2nd","3rd","4th","5th","6th","7th", "7nd","8th",
               "9th","10th","11th", "12th","13th","14th","15th","16th","17th",
               "18th","19th","20th","21st","22th","22nd","23th","23rd","24th",
               "25th","26th","27th","28th", "29th","30th","31th","31st", "22d",
               "maj gen", "most excellent", "morning","waggons","waggon",
               "to the president of the united states","independant","independent",
               "the president of the united states",
               "president of the united states","my best friend","most dear sir",
               "my dear son", "my dear grandson","may it please your excellency",
               "my very dear friend",
               "not found from", "my dear friend and kinsman", "my dear george",
               "my dear louisa", "my dearest louisa", "jonathan glover","humble",
               "general washington","william washington", "betty washington",
               "john stine washington","lund washington","acknd","university",
               "martha washington", "samuel washington", "général washington",
               "john glover", "nathanael greene","instructs greene", "john hancoc",
               "robert hanson","benjamin harrison", "robert hanson harrison,",
               "robert harrison","answers","answer","answered","answering",
               "monseigneur","schuyler","noble","scott","brave",
               "describes","described","describe","brigadier","brig gen","patrick",
               "william heath", "instructs heath","nathaniel warner","opportunity",
               "deposit","power","coleman","greenleaf","cooper","street","wyatt",
               "david waterbury", "loving","manuscripts","manuscript", "honrd",
               "to the citizens of","to the citizens of the county", "colonel",
               "sent wentworth", "fellow citizen of","fellow citizens of","honbl",
               "peter webster","john ine washington", "honourable",
               "business", "class", " duly", "document", "drafted", "friends",
               "friend","account","accounts","inclosd","inclosed","inclose","incloses",
               "inclosing","inclosure","john augustine washington","james cleveland",
               "advance","allen mclane","letter", "sir ", "colonel chester",
               "gentn", "harrison",  "stringer","george washington", "franklin",
               "john adams smith", "smith","dear johnson","thomas jefferson",
               "jefferson","dear jefferson", "john adams","warren","harriet",
               "transfer","shaw","post","only","portia","adamss", "mon cher",
               "caroline","young","universal","humphreys","missing","revd",
               "yours"," your","in order","yesterday","i had","copy","copied",
               "having had","we have","receive","received","recived","reced",
               "recieved","you have","receiving","recieving","copying",
               "edward newenham", "serves","served","send","sent","essay", "dennis",
               "john langdon", "private", "septr","it is","my lord duke","monsieur",
               "thomas conway", "thomas nelson", "of the city","city of",
               "benedict arnold", "brigantine", "vice president"," president",
               " men of","inclosed","of the", "to the","i have","gentlemen","copy",
               "copies","my lord", "page","office","agency","inclosing","i am",
               "mother", "papa","mamma","grandpapa","cousin","honoured","parents",
               "sister","brother","father","hond","madam","uncle","aunt", "honourd",
               "honorable", "honored","honord", "the honor", "honor", "letters",
               "news","contract","honours"," yrself",
               "decemr", "my dearest","my dear", "dear","my dearest friend",
               "dearest","friend"," friendly","beloved","wife","james clinton",
               "george clinton","henry clinton", "general clinton",
               "heath","james hill", "samuel holden","jedediah huntington",
               "samuel huntington"," lawrence"," henry lee","charles lee",
               "dear lee","orders lee","camp near"," head qrs","hd qrs",
               "in the vicinity of","head quarters","head qtr","benjamin lincoln",
               "book","library", "pleasd","pleased", "lincoln","david mason",
               " george mason","morgan connor","daniel morgan","john morgan",
               "thomas newton", "johnson","not found to","edward norwood", "colo",
               "joseph palmer","near","camp at","camp on","josiah quincy",
               "desired","desire","raleigh at sea","edmund randolph","mr. randolph",
               "peyton randolph","randolph","raymond","concern","concerned",
               "advices","advice","adviced","on reading", "in reading",
               "upon reading"," zebedee redding","philip van rensselaer",
               "joseph spencer", "oliver spencer","sterling complains","temple",
               "thornton","knox  baillie", "edward snickers","from the green",
               "bradford", "jonathan trumbull","joseph trumbull","john west",
               "lodge","ensign", "recruit", "assisting","assist",
               "to governors trumbull greene  weare","verite union","union",
               "vergennes", "walpole","william woodford"," instructs woodford",
               "receivd", "received","favours", "mail","written", "wrote",
               "writing", "writings", "writen", "writings", "history","glad",
               "common","acknowleged","acknowlege","acknowledgd","acknowledged",
               "acknowledges","marquis","james madison", "general	stirling",
               "valentine", "todd", "john dickinson","day"," congress",
               "daniel smith", " bearer", "mercer","decem",
               "philip", "william trent","hope", "anthony white","imperial",
               "saml cook", "john sullivan","sullivan","the colony", "moore",
               "lord stirling","invoice","doctor","stephens","consulting","consult",
               "camp", "pray","william ramsay",	"alexander mcdougall",
               "william bartlett","editorial","commissary","genl",
               "council","william cushing","george gregory", "liberty", "quarters",
               "henry babcock", "jones",  "joseph reed","joseph","lee", "arnold",
               "stephen","esqr","esq", "head", "qrs", "before", "evening",
               "response","light infantry","lieutenant","benjamin", "tupper",
               "register", "papers" ,"printed","burr bradley","james bowdoin",
               "loammi baldwin","daniel cunyngham clymer","committee","militia",
               "officers","augt","octbr","happy", "delivery","acknowledge",
               "septemr","septembr","sepr","return","pleasure","commissioners",
               "messenger","beg","leave","octbr","nights","octor",
               "extract","miles","baron","material","governor","write",
               "wrote","writing","enclosed","resolves"," doctor","congress",
               "enclosd","background","conversations","honour","augst",
               "lieut","wheaton","informed","informs","inform", "inclose",
               "agreeable","army","assembly","representatives","ashamed",
               "articles","arrived","arrival", "situation","armstrong",
               "oclo","aprl","approve","approved","appointed","appointment",
               "applied","application","applications", "emigration", "period",
               "appears","apl","inform","accident","lovell","friendship","gray",
               "gerry","wilson","murray","genl washington","begs","imform",
               "majo","otis","advice","company","williamson","benson","attorny",
               "barton","meredith","morgan","colo varick","spencer","paterson",
               "hammond","russell","jackson","girard","le roy","god","eliot",
               "taylor", "carroll","wayne","parker","sherman","novbr",
               "stoddard","edwards","stark","replying","monroe","request",
               "trumbull","captn", "commissioner","agent"," weekly", "monthly",
               "major", "steuben"," febr","indians","attacked","receipt"," glover",
               "gratz","greene","hancock","mckean","hugh mercer","james mercer",
               "dubois","ogden","returned","intended", "passing","thro",
               "left mount vernon","winthrop","captain turner", " capt turner",
               "enclose","encloses","ware bound","whately","franklins",
               "sir", "mr", "letter", "dear", "dr","dr—", "mrs", "pd", "sr",
               "january", "february", "march", "april", "may", "june", "july",
               "august", "september","october","november","december", "decbr",
               "thos","royal","society","esquire"," instructions","ensign",
               "b","c","p","h","go", "aug", "sept","oct","nov","dec", "janry",
               "jany","febry", "feby", "decr", "octr", "octobr","jan",
               "wm","tis","ca","gw", "novr","monday","tuesday", "wednesday",
               "thursday", "friday", "saturday","saturdays","sunday", "octo",
               "novemr","assemblys","regiment","commanding", "officer",
               "1st","2nd","3rd","4th","5th","6th","7th", "7nd","8th","9th",
               "10th","11th","12th","13th","14th","15th","16th","17th","decm",
               "18th","19th","20th","21st","22th","22nd","23th","23rd","septm",
               "24th","25th","26th","27th","28th","29th","30th","31th",
               "31st", "22d","tem","late","dearest","oclock", "o’clock","decre",
               "things","time","day","morning", "mr", "mr.","letter",
               "mat","excellency","paper","les","circumstance","article",
               "consequence","john","gentleman","reason","people","opinion",
               "character","william", "favour","excelly","excellencys",
               "excellency’s","williams","answer","shall","one","could","president",
               "would", "upon","may","&","every","much","might", "with", "petition",
               "without","two","us","yet","since","also","therefore",
               "however","never","ever","soon","say","take","give","well",
               "see","mch","sir","mr","mr.","get","give","want","many",
               "part","time", "wh","ditto","day","letter","mrs",
               "letter","day","person","post","purpose","measure","answer",
               "mat","subject","circumstance","manner","moment","gentleman",
               "yesterday","instant","week","par","night","event","gentlmen",
               "object","paper","month","favour","favor","favored", "reason",
               "regard","hble","vessel","hamilton","soldiers","speaker",
               "principle","matter","instance","question","time","inst","favorable",
               "degree","","occasion","honble","hour","behalf","particular",
               "van","word", "correspondence","issue","lettre","printing",
               "ladies","recd","yr favr","favr"," favors","considered","proposal",
               "delivered","deliver","trouble","communicated","communicate",
               "announcing","announced","announces","announcing","announced",
               "recieve","lord","secretary","transmitting","transmitted","transmit",
               "esteemed","esteem","mention","submitted","submit","submitting",
               "personally","person","personage","personages","personage’s",
               "addressing","addressed","address","proposing",
               "propose","proposed","proposes","respect","respecting","respects",
               "respected","dated","date","dated","dates","newspaper",
               "proceedings","herewith","directed","forget","forgot","forgotten",
               "reply","replied","replies","acknowledging","noon",
               "venerable","yrs","yr","majr","learn","learned","learnt","regret",
               "heard","hear","finished","finish","finishing","finished",
               "ocr", "stating","dcr","professor","indenture","handed",
               "offers", "acknowledgments","deeply", "regretted","continue",
               "continued","waggon","permit me","permit","apologies",
               "consideration","consider","considered","considering",
               "expressed","express","expresses","expressing","previous",
               "requested","request","requests","requesting","confidential",
               "publication","unexpected","unexpectedly","expect","expects",
               "expected","afternoon","minutes","return","returned","returns",
               "returning","son","sons","john quincy adams","josiah quincy",
               "jay","war","daughter","thomas","charlotte","sepbr","sepber",
               "money","lady","commission","senate","chamber","journal","mama",
               "grandfather","grandmama","house"," author","nephew","united",
               "grandmother","hotel","record","tomorow","tomorrow",
               "captain","commandant","exellency"," edward", "coles",
               "introduces","clark","meeting","colony","samuel chase",
               "roman","catholic church","connor"," mayor", "cutler",
               "claiborne","parke custis","america","revenue",
               "general","james cook","enemy","count","minister",
               "honors","cook")

all_stop_words <- c(quanteda::stopwords("en"),
                    stopwords::stopwords(source ="snowball"),
                    stopwords::stopwords(source ="smart"),
                    stopwords::stopwords(source ="nltk"),
                    stopwords::stopwords(source ="marimo"),
                    stopwords::stopwords(source ="stopwords-iso"))


# These words in the stopwords list we actually do want to keep in the corpus
# because these are often used as abbreviations for american states
remove_from_stopwords <- c("al","ct","de", "ga","il","in","ky","la","md",
                           "ma","mi","mn","ms","mo","nh","nj","ny","nc","oh",
                           "pa","ri","sc","vt","va","new","n","mill","point","st",
                           "del","m","haven","great","us")

all_stop_words <- all_stop_words[!all_stop_words %in% remove_from_stopwords]
all_stop_words <- c(all_stop_words, bad_words)
all_stop_words <- unique(all_stop_words)
#all_stop_words <- all_stop_words %>% as.data.frame()

# Further remove stopwords from extraction process(where the place of publication is in)
letter_ss$city_extraction <- tm::removeWords(letter_ss$city_extraction, all_stop_words)
letter_ss$city_extraction <- str_squish(letter_ss$city_extraction)
letter_ss$first_sentence  <- str_squish(letter_ss$first_sentence)

# 4. CITY NAMES AND COORDINATES ----

## A) All us-cities before 1835 ----
# From https://query.wikidata.org/
us_historical_cities <- read.csv("data/external/1_uscities_till1835.csv", sep = ',') %>%
  as.data.frame() %>%
  rename(city        = sLabel,
         state       = label) %>%
  mutate(city        = tolower(city),
         state       = tolower(state),
         coordinates = coordinates %>%
           str_remove_all(paste(c("Point", "\\(", "\\)"), collapse = "|"))) %>%
  separate("coordinates", c("longitude","latitude"), sep = " ",remove = T) %>%
  mutate(longitude   = as.numeric(longitude),
         latitude    = as.numeric(latitude)) %>%
  # remove city-state duplicates
  distinct(city,state,.keep_all = TRUE) %>%
  mutate(country = "united states") %>%
  select(city,state,country,latitude,longitude)

## B) All currently-existing US cities with population > 1000 ----
# From https://public.opendatasoft.com/explore/dataset/geonames-all-cities-with-a-population-1000/information/?disjunctive.cou_name_en&sort=name
all_us_cities <- read.csv("data/external/2_us_cities.csv", sep = ',') %>%
  as.data.frame() %>%
  rename_with(tolower) %>%
  mutate(city       = tolower(city),
         state      = tolower(state_code),
         county     = tolower(county),
         full_state = tolower(state_name)) %>%
  distinct(city,state,.keep_all = TRUE) %>%
  mutate(country = "united states") %>%
  select(city,state,country,latitude,longitude)


## C) All cities in the world with a population > 1000 ----
# From https://public.opendatasoft.com/
world_cities <- read.csv("data/external/3_allcities.csv", sep = ';') %>%
  as.data.frame() %>%
  rename(city       = Name,
         state      = Admin1.Code,
         country    = Country.name.EN,
         citynames  = Alternate.Names,
         population = Population) %>%

  mutate(city       = tolower(city),
         state      = tolower(state),
         country    = tolower(country),
         citynames  = tolower(citynames)) %>%

  # we'll focus on cities in Europe and North America
  filter(str_detect(Timezone, "Europe") | LABEL.EN == "United States") %>%
  separate("Coordinates", c("latitude","longitude"), sep = " ",remove = F) %>%
  select(city,state, citynames, country, population,latitude,longitude) %>%
  filter(country != "") %>%
  mutate(state = if_else(str_detect(state, "^[0-9]"), NA_character_, state),
         citynames = str_replace(citynames, "\\s\\s.*", ""),
         citynames = paste(city, citynames, sep = ",")) %>%
  rowwise() %>%
  mutate(citynames = str_c(unique(str_split(citynames, ",")[[1]]), collapse = ",") %>%
           str_remove(",+$"),
         latitude  = as.numeric(gsub(",","",latitude)),
         longitude = as.numeric(longitude)
         )

# unique citynames across all dataframes
unique_cities <- rbind(us_historical_cities %>% distinct(city),
                       all_us_cities        %>% distinct(city),
                       world_cities         %>% distinct(city)) %>%
                  distinct(city)

# coordinates for all cities
city_coordinates <- rbind(us_historical_cities,
                          all_us_cities,
                          world_cities %>%
                         select(city,state,country,latitude,longitude)) %>%
  distinct(city,state,country,.keep_all = TRUE)

# 5. MATCHING CITIES IN THE LETTERS ----
# We first extract the city-name, regardless for the moment whether there are
# duplicates or not. We'll deal with that later in the script

## A) Extract city name ----
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "grosvenor square", "london")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "grovenor square", "london")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "grosr. sqr", "london")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "grosvr. square", "london")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "grovr. sqr.", "london")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "grosv. sqr.", "london")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "grosv. square", "london")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "grosv. square.", "london")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "grosvr. sqr.", "london")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "g. sqr", "london")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "g. sqr.", "london")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "grosr. sqr.", "london")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "grosv.sqr.", "london")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "grosvenor-square.", "london")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "soho square", "london")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "sir.", "")
letter_ss$city_extraction <- str_replace(letter_ss$city_extraction, "auteuil", "paris")

letter_ss <- letter_ss %>%
  mutate(city = case_when(str_detect(city_extraction, "montezillo") ~ "montezillo",
                          str_detect(city_extraction, "montizillo") ~ "montizillo",
                          str_detect(city_extraction, "montizello") ~ "montizello",
                          str_detect(city_extraction, "sirquincy")  ~ "quincy",
                          str_detect(city_extraction, "piscatua")   ~ "piscatua",
                          str_detect(city_extraction, "leyden")     ~ "leiden",
                          str_detect(city_extraction, "hague")      ~ "the hague",
                          str_detect(city_extraction, "allen town") ~ "allen town",
                          str_detect(city_extraction, "fishkill")   ~ "fishkill",
                          str_detect(city_extraction, "fish kill")   ~ "fishkill",
                          str_detect(city_extraction, "m town")   ~ "morristown",
                          str_detect(city_extraction, "morris")   ~ "morristown",
                          str_detect(city_extraction, "morris-town")  ~ "morristown",
                          str_detect(city_extraction, "westpoint")  ~ "west point",
                          str_detect(city_extraction, "dennant castle")   ~ "dennant castle",
                          str_detect(city_extraction, "jockey hollow")   ~ "jockey hollow",
                          str_detect(city_extraction, "the haye")      ~ "the hague",
                          str_detect(city_extraction, "la haye")      ~ "the hague",
                          str_detect(city_extraction, "la haie")      ~ "the hague",
                          str_detect(city_extraction, "lahaie")      ~ "the hague",
                          str_detect(city_extraction, "travellersrest") ~ "travelers rest",
                          str_detect(city_extraction, "jacobs creek")   ~ "jacobs creek",
                          str_detect(city_extraction, "sunsbury")       ~ "sunbury",
                          str_detect(city_extraction, "esopus")         ~ "esopus",
                          str_detect(city_extraction, "gloster")        ~ "gloster",
                          str_detect(city_extraction, "cross creek")    ~ "cross creek",
                          str_detect(city_extraction, "marstrand")      ~ "marstrand",
                          str_detect(city_extraction, "masterland")     ~ "masterland",
                          str_detect(city_extraction, "dunquerque")     ~ "dunkerque",
                          str_detect(city_extraction, "the hage")       ~ "the hague",
                          str_detect(city_extraction, "nants")          ~ "nantes",
                          str_detect(city_extraction, "green mountain") ~ "green mountain",
                          str_detect(city_extraction, "epping forest")  ~ "epping forest",
                          #str_detect(city_extraction, "new york")       ~ "new york",
                          str_detect(city_extraction, "newyork")        ~ "new york",
                          str_detect(city_extraction, "boston road")    ~ "boston road",
                          str_detect(city_extraction, "la coruña")  ~ "la coruña",
                          str_detect(city_extraction, "bourdeaux")  ~ "bordeaux",
                          str_detect(city_extraction, "bordeaux")  ~ "bordeaux",
                          str_detect(city_extraction, "geo town")  ~ "george town",
                          str_detect(city_extraction, "jacksonburgh")  ~ "jacksonboro",
                          str_detect(city_extraction, "paimboeuf")  ~ "paimboeuf",
                          str_detect(city_extraction, "spring gardin")  ~ "spring gardin",
                          str_detect(city_extraction, "springgardin")  ~ "spring gardin",
                          str_detect(city_extraction, "stewart crossing")  ~ "stewart crossing",
                          str_detect(city_extraction, "st croix")  ~ "st croix",
                          str_detect(city_extraction, "tortola")  ~ "tortola",
                          str_detect(city_extraction, "turners clove")  ~ "turners clove",
                          str_detect(city_extraction, "berkley county")  ~ "berkley county",
                          str_detect(city_extraction, "borden town")  ~ "borden town",
                          str_detect(city_extraction, "west point")  ~ "west point",
                          str_detect(city_extraction, "vaugirard")  ~ "paris",
                          str_detect(city_extraction, "fort clinton")  ~ "fort clinton",
                          str_detect(city_extraction, "taapan")  ~ "tappan",
                          str_detect(city_extraction, "tapan")  ~ "tappan",
                          str_detect(city_extraction, "newbury port")  ~ "newburyport",
                          str_detect(city_extraction, "nieuport")  ~ "newport",
                          str_detect(city_extraction, "new port")  ~ "newport",
                          str_detect(city_extraction, "madrid")  ~ "madrid",
                          str_detect(city_extraction, "neworleans") ~ "new orleans",
                          str_detect(city_extraction, "n orleans") ~ "new orleans",
                          str_detect(city_extraction, "new-orleans") ~ "new orleans",
                          str_detect(city_extraction, "ampthill") ~ "ampthill",
                          str_detect(city_extraction, "leghorn")    ~ "livorno",
                          str_detect(city_extraction, "bremo")    ~ "bremo",
                          str_detect(city_extraction, "legn")    ~ "livorno",
                          str_detect(city_extraction, "orient")    ~ "lorient",
                          str_detect(city_extraction, "chaillot")    ~ "paris",
                          str_detect(city_extraction, "nw orleans")    ~ "new orleans",
                          str_detect(city_extraction, "l orient")    ~ "lorient",
                          str_detect(city_extraction, "lorient")    ~ "lorient",
                          str_detect(city_extraction, "havre")    ~ "le havre",
                          str_detect(city_extraction, "middle brook")    ~ "middlebrook",
                          str_detect(city_extraction, "ghent")    ~ "ghent",
                          str_detect(city_extraction, "helvoetsluys") ~ "hellevoetsluis",
                          str_detect(city_extraction, "leesburg") ~ "leesburg",
                          str_detect(city_extraction, "fort mercer") ~ "fort mercer",
                          str_detect(city_extraction, "bayone") ~ "bayonne",
                          str_detect(city_extraction, "montpr") ~ "montpellier",
                          str_detect(city_extraction, "montper") ~ "montpellier",
                          str_detect(city_extraction, "montplr") ~ "montpellier",
                          str_detect(city_extraction, "hudsons river") ~ "hudsons river",
                          str_detect(city_extraction, "claverack")~ "claverack",
                          str_detect(city_extraction, "cortlandts manor") ~ "cortlandts manor",
                          str_detect(city_extraction, "newington green") ~ "newington green",
                          str_detect(city_extraction, "newingtongreen") ~ "newington green",
                          str_detect(city_extraction, "friendnewingtongreen") ~ "newington green",
                          str_detect(city_extraction, "whitemarsh")~ "whitemarsh",
                          str_detect(city_extraction, "bigg spring")~ "leesburg",
                          str_detect(city_extraction, "big spring")~ "leesburg",
                          str_detect(city_extraction, "princeton")~ "princeton",
                          str_detect(city_extraction, "fishkill")~ "fishkill",
                          str_detect(city_extraction, "nantes")~ "nantes",
                          str_detect(city_extraction, "perthamboy")~ "perth amboy",
                          str_detect(city_extraction, "fort pitt")~ "fort pitt",
                          str_detect(city_extraction, "harlem")~ "new york",
                          str_detect(city_extraction, "m vernon")~ "mount vernon",
                          str_detect(city_extraction, "mt vernon")~ "mount vernon",
                          str_detect(city_extraction, "elizabeth town")~ "elizabeth town",
                          str_detect(city_extraction, "elizabethtown")~ "elizabeth town",
                          str_detect(city_extraction, "elizth town")~ "elizabeth town",
                          str_detect(city_extraction, "eliza town")~ "elizabeth town",
                          str_detect(city_extraction, "frederick town")~ "frederick town",
                          str_detect(city_extraction, "stony point")~ "stony point",
                          str_detect(city_extraction, "east chester") ~ "east chester",
                          str_detect(city_extraction, "stony field") ~ "stony field",
                          str_detect(city_extraction, "richmond hill") ~ "richmond hill",
                          str_detect(city_extraction, "richd") ~ "richmond",
                          str_detect(city_extraction, "hampton falls") ~ "hampton falls",
                          str_detect(city_extraction, "philadelpa")    ~ "philadelphia",
                          str_detect(city_extraction, "phia")    ~ "philadelphia",
                          str_detect(city_extraction, "hiladelphia")    ~ "philadelphia",
                          str_detect(city_extraction, "phyladelphia")  ~ "philadelphia",
                          str_detect(city_extraction, "philadelp")     ~ "philadelphia",
                          str_detect(city_extraction, "phildelphia")   ~ "philadelphia",
                          str_detect(city_extraction, "philadelphie")   ~ "philadelphia",
                          str_detect(city_extraction, "charlestown")   ~ "charlestown",
                          str_detect(city_extraction, "washington dc")   ~ "washington",
                          str_detect(city_extraction, "washington city")   ~ "washington",
                          str_detect(city_extraction, "petersburg")     ~ "saint petersburg",
                          str_detect(city_extraction, "st petersbourg") ~ "saint petersburg",
                          str_detect(city_extraction, "philad.")       ~ "philadelphia",
                          str_detect(city_extraction, "phila")         ~ "philadelphia",
                          str_detect(city_extraction, "hiladelphia") ~ "philadelphia",
                          str_detect(city_extraction, "fredericksbg") ~ "fredericksburg",
                          str_detect(city_extraction, "fredricksburg") ~ "fredericksburg",
                          str_detect(city_extraction, "fredriksburgh") ~ "fredericksburg",
                          str_detect(city_extraction, "frederiksburg") ~ "fredericksburg",
                          str_detect(city_extraction, "frederiksburgh") ~ "fredericksburg",
                          str_detect(city_extraction, "fredrixbrg") ~ "fredericksburg",
                          str_detect(city_extraction, "la rolchelle") ~ "la rolchelle",
                          str_detect(city_extraction, "fredericksburg") ~ "fredericksburg",
                          str_detect(city_extraction, "city of washington") ~ "washington",
                          str_detect(city_extraction, "frankfort-on-main") ~ "frankfurt am main",
                          str_detect(city_extraction, "frankfort--main") ~ "frankfurt am main",
                          str_detect(city_extraction, "frankfurt main") ~ "frankfurt am main",
                          str_detect(city_extraction, "frankfurt--main") ~ "frankfurt am main",
                          str_detect(city_extraction, "frankfort on the maine") ~ "frankfurt am main",
                          str_detect(city_extraction, "frankfort sur le main") ~ "frankfurt am main",
                          str_detect(city_extraction, "philidelphia") ~ "philadelphia",
                          str_detect(city_extraction, "george-town") ~ "georgetown",
                          str_detect(city_extraction, "george town") ~ "georgetown",
                          str_detect(city_extraction, "geo town") ~ "georgetown",
                          str_detect(city_extraction, "fort constitution") ~ "fort constitution",
                          str_detect(city_extraction, "fort george") ~ "fort george",
                          str_detect(city_extraction, "morrissania") ~ "new york",
                          str_detect(city_extraction, "eorg town") ~ "georgetown",
                          str_detect(city_extraction, "new ark") ~ "newark",
                          str_detect(city_extraction, "preakness") ~ "preakness",
                          str_detect(city_extraction, "winter hill") ~ "winter hill",
                          str_detect(city_extraction, "prospect hill") ~ "prospect hill",
                          str_detect(city_extraction, "valentines hill") ~ "valentines hill",
                          str_detect(city_extraction, "fairy hill") ~ "fairy hill",
                          str_detect(city_extraction, "rocky hill") ~ "rocky hill",
                          str_detect(city_extraction, "phills hill") ~ "phills hill",
                          str_detect(city_extraction, "castle hill") ~ "castle hill",
                          str_detect(city_extraction, "ashley hill") ~ "ashley hill",
                          str_detect(city_extraction, "bald hill") ~ "bald hill",
                          str_detect(city_extraction, "keelers hill") ~ "keelers hill",
                          str_detect(city_extraction, "hill new milford") ~ "new milford",
                          str_detect(city_extraction, "spring hill") ~ "spring hill",
                          str_detect(city_extraction, "malvan hill") ~ "malvan hill",
                          str_detect(city_extraction, "malvern hill") ~ "malvern hill",
                          str_detect(city_extraction, "malbern hill") ~ "malvern hill",
                          str_detect(city_extraction, "quaker hill") ~ "quaker hill",
                          str_detect(city_extraction, "chesnut hill") ~ "chesnut hill",
                          str_detect(city_extraction, "bottle hill") ~ "bottle hill",
                          str_detect(city_extraction, "round hill") ~ "round hill",
                          str_detect(city_extraction, "rockey hill") ~ "rockey hill",
                          str_detect(city_extraction, "rocky hill") ~ "rocky hill",
                          str_detect(city_extraction, "lundrocky hill") ~ "rocky hill",
                          str_detect(city_extraction, "r hill") ~ "rocky hill",
                          str_detect(city_extraction, "ocky hill") ~ "rocky hill",
                          str_detect(city_extraction, "peeks hill") ~ "peeks hill",
                          str_detect(city_extraction, "pecks kill") ~ "pecks kill",
                          str_detect(city_extraction, "pecks-kill") ~ "pecks kill",
                          str_detect(city_extraction, "baltimore") ~ "baltimore",
                          str_detect(city_extraction, "hillsborough township") ~ "hillsborough township",
                          str_detect(city_extraction, "jamaica plain") ~ "jamaica plain",
                          str_detect(city_extraction, "wmsburg") ~ "williamsburg",
                          str_detect(city_extraction, "charles town") ~ "charlestown",
                          str_detect(city_extraction, "continental village") ~ "continental village",
                          str_detect(city_extraction, "new kent") ~ "new kent",
                          str_detect(city_extraction, "okeepsie") ~ "poughkeepsie",
                          str_detect(city_extraction, "pokepsie") ~ "poughkeepsie",
                          str_detect(city_extraction, "pougkeepsie") ~ "poughkeepsie",
                          str_detect(city_extraction, "chs town") ~ "charlestown",
                          str_detect(city_extraction, "poghkeepsie") ~ "poughkeepsie",
                          str_detect(city_extraction, "dobbsferry") ~ "dobbs ferry",
                          str_detect(city_extraction, "havredegrace") ~ "havre de grace",
                          str_detect(city_extraction, "fort lee") ~ "fort lee",
                          str_detect(city_extraction, "frankfort on the main") ~ "frankfurt am main",
                          str_detect(city_extraction, "bergen county") ~ "bergen county",
                          str_detect(city_extraction, "stanwich") ~ "stanwich",
                          str_detect(city_extraction, "beverwick") ~ "beverwick",
                          str_detect(city_extraction, "farmindell") ~ "farmindell",
                          str_detect(city_extraction, "fort montgomery") ~ "fort montgomery",
                          str_detect(city_extraction, "orange town") ~ "orange town",
                          str_detect(city_extraction, "orangetown") ~ "orange town",
                          str_detect(city_extraction, "orange county") ~ "orange county",
                          str_detect(city_extraction, "passaic falls") ~ "passaic falls",
                          str_detect(city_extraction, "passy") ~ "paris",
                          str_detect(city_extraction, "passi") ~ "paris",
                          str_detect(city_extraction, "bruxelles") ~ "brussels",
                          str_detect(city_extraction, "londn") ~ "london",
                          str_detect(city_extraction, "reading town") ~ "reading town",
                          str_detect(city_extraction, "reading furnace") ~ "reading furnace",
                          str_detect(city_extraction, "rye neck") ~ "rye neck",
                          str_detect(city_extraction, "brunswic") ~ "brunswick",
                          str_detect(city_extraction, "princetown") ~ "princetown",
                          str_detect(city_extraction, "mount vernon") ~ "mount vernon",
                          str_detect(city_extraction, "morris town") ~ "morristown",
                          str_detect(city_extraction, "ware church") ~ "ware church",
                          str_detect(city_extraction, "warwick furnace") ~ "warwick furnace",
                          str_detect(city_extraction, "warwick bucks") ~ "warwick bucks",
                          str_detect(city_extraction, "mount washington") ~ "mount washington",
                          str_detect(city_extraction, "mt washington") ~ "mount washington",
                          str_detect(city_extraction, "worcester township") ~ "worcester township",
                          str_detect(city_extraction, "n york") ~ "new york",
                          str_detect(city_extraction, "york town") ~ "yorktown",
                          str_detect(city_extraction, "highlands") ~ "highlands",
                          str_detect(city_extraction, "new windsor") ~ "new windsor",
                          str_detect(city_extraction, "new haven") ~ "new haven",
                          str_detect(city_extraction, "newhaven") ~ "new haven",
                          str_detect(city_extraction, "new utrecht") ~ "new york city",
                          str_detect(city_extraction, "plymo") ~ "plymouth",
                          str_detect(city_extraction, "plym") ~ "plymouth",
                          str_detect(city_extraction, "plimouth") ~ "plymouth",
                          str_detect(city_extraction, "piccadilly") ~ "london",
                          str_detect(city_extraction, "chalsea") ~ "chelsea",
                          str_detect(city_extraction, "portobacco") ~ "port tobacco",
                          str_detect(city_extraction, "porttobacco") ~ "port tobacco",
                          str_detect(city_extraction, "raspberry plain") ~ "leesburg",
                          str_detect(city_extraction, "rasberry plain") ~ "leesburg",
                          str_detect(city_extraction, "cheapside") ~ "cheapside",
                          str_detect(city_extraction, "skipton") ~ "skipton",
                          str_detect(city_extraction, "gunstonhall") ~ "gunston hall",
                          str_detect(city_extraction, "gunston hall") ~ "gunston hall",
                          str_detect(city_extraction, "winchister") ~ "winchester",
                          str_detect(city_extraction, "craven") ~ "london",
                          str_detect(city_extraction, "portsmo") ~ "portsmouth",
                          str_detect(city_extraction, "mount airy") ~ "mount airy",
                          str_detect(city_extraction, "mountvernon") ~ "mount vernon",
                          str_detect(city_extraction, "mount vernon") ~ "mount vernon",
                          str_detect(city_extraction, "mount varnon") ~ "mount vernon",
                          str_detect(city_extraction, "mt vernon") ~ "mount vernon",
                          str_detect(city_extraction, "king william") ~ "king william",
                          str_detect(city_extraction, "alexandria") ~ "alexandria",
                          str_detect(city_extraction, "alixandria") ~ "alexandria",
                          str_detect(city_extraction, "esthertown") ~ "estherton",
                          str_detect(city_extraction, "estherton") ~ "estherton",
                          str_detect(city_extraction, "eastown") ~ "easton",
                          str_detect(city_extraction, "alexana") ~ "alexandria",
                          str_detect(city_extraction, "little england") ~ "little england",
                          str_detect(city_extraction, "stewarts crossings") ~ "stewarts crossings",
                          str_detect(city_extraction, "mass bay") ~ "massachusetts bay",
                          str_detect(city_extraction, "yough river") ~ "yough river",
                          str_detect(city_extraction, "ashly river") ~ "ashly river",
                          str_detect(city_extraction, "mattapony river") ~ "mattapony river",
                          str_detect(city_extraction, "warburton") ~ "warburton",
                          str_detect(city_extraction, "charlottesville") ~ "charlottesville",
                          str_detect(city_extraction, "braintre") ~ "braintree",
                          str_detect(city_extraction, "saratoga") ~ "saratoga",
                          str_detect(city_extraction, "tionderoga") ~ "tionderoga",
                          str_detect(city_extraction, "tyonderoga") ~ "tionderoga",
                          str_detect(city_extraction, "ticonderoga") ~ "tionderoga",
                          str_detect(city_extraction, "amstdm") ~ "amsterdam",
                          str_detect(city_extraction, "golden square") ~ "london",
                          str_detect(city_extraction, "london") ~ "london",
                          str_detect(city_extraction, "harford town") ~ "harford town",
                          str_detect(city_extraction, "campridge") ~ "cambridge",
                          str_detect(city_extraction, "cambreg") ~ "cambridge",
                          str_detect(city_extraction, "water town") ~ "watertown",
                          str_detect(city_extraction, "pecks kills")~ "peekskill",
                          str_detect(city_extraction, "peckskill")  ~ "peekskill",
                          str_detect(city_extraction, "peekskill")  ~ "peekskill",
                          str_detect(city_extraction, "peeks kill") ~ "peekskill",
                          str_detect(city_extraction, "peaks kiln") ~ "peekskill",
                          str_detect(city_extraction, "philipsburg")~ "philipsburg",
                          str_detect(city_extraction, "bethelem")~ "bethlehem",
                          str_detect(city_extraction, "heckenseck") ~ "heckenseck",
                          str_detect(city_extraction, "hackensack") ~ "hackensack",
                          str_detect(city_extraction, "falls of delaware") ~ "delaware falls",
                          str_detect(city_extraction, "falls township") ~ "falls township",
                          str_detect(city_extraction, "turin") ~ "turin",
                          str_detect(city_extraction, "sussex") ~ "sussex",
                          str_detect(city_extraction, "germantown") ~ "philadelphia",
                          str_detect(city_extraction, "bucks county") ~ "bucks county",
                          str_detect(city_extraction, "amwell") ~ "amwell",
                          str_detect(city_extraction, "corriels ferry") ~ "corriels ferry",
                          str_detect(city_extraction, "keiths") ~ "keiths",
                          str_detect(city_extraction, "versailles") ~ "versailles",
                          str_detect(city_extraction, "dijon") ~ "dijon",
                          str_detect(city_extraction, "paramus") ~ "paramus",
                          str_detect(city_extraction, "new town") ~ "new town",
                          str_detect(city_extraction, "haddonfield") ~ "haddonfield",
                          str_detect(city_extraction, "grafton street") ~ "dublin",
                          str_detect(city_extraction, "montego bay") ~ "montego bay",
                          str_detect(city_extraction, "oyster ponds") ~ "oyster pond",
                          str_detect(city_extraction, "baskenridge") ~ "basking ridge",
                          str_detect(city_extraction, "white plains") ~ "white plains",
                          str_detect(city_extraction, "whiteplains") ~ "white plains",
                          str_detect(city_extraction, "rouen") ~ "rouen",
                          str_detect(city_extraction, "pownalborough") ~ "pownalborough",
                          str_detect(city_extraction, "frenchmans bay") ~ "frenchmans bay",
                          str_detect(city_extraction, "fayette ville") ~ "fayette ville",
                          str_detect(city_extraction, "tarboh") ~ "tarboh",
                          str_detect(city_extraction, "mountpleasant") ~ "mount pleasant",
                          str_detect(city_extraction, "mount plesant") ~ "mount pleasant",
                          str_detect(city_extraction, "algiers") ~ "algiers",
                          str_detect(city_extraction, "eppington") ~ "eppington",
                          str_detect(city_extraction, "springhouse") ~ "spring house",
                          str_detect(city_extraction, "haie") ~ "the hague",
                          str_detect(city_extraction, "bush hill") ~ "philadelphia",
                          str_detect(city_extraction, "brussells") ~ "brussells",
                          str_detect(city_extraction, "versills") ~ "versailles",
                          str_detect(city_extraction, "avignon") ~ "avignon",
                          str_detect(city_extraction, "dunkerque") ~ "dunkerque",
                          str_detect(city_extraction, "dunkuerque") ~ "dunkerque",
                          str_detect(city_extraction, "castelnaudarry") ~ "castelnaudarry",
                          str_detect(city_extraction, "cheam") ~ "london",
                          str_detect(city_extraction, "amstdn") ~ "amsterdam",
                          str_detect(city_extraction, "amst") ~ "amsterdam",
                          str_detect(city_extraction, "pompton") ~ "pompton",
                          str_detect(city_extraction, "londres") ~ "london",
                          str_detect(city_extraction, "bridgeTown barbadoes") ~ "bridgeTown barbadoes",
                          str_detect(city_extraction, "sannois") ~ "sannois",
                          str_detect(city_extraction, "maidenhead") ~ "maidenhead",
                          str_detect(city_extraction, "reedyisland") ~ "reedy island",
                          str_detect(city_extraction, "stenton") ~ "stenton",
                          str_detect(city_extraction, "quebec") ~ "quebec",
                          str_detect(city_extraction, "lond") ~ "london",
                          str_detect(city_extraction, "eliz town") ~ "elizabeth town",
                          str_detect(city_extraction, "coldengham") ~ "coldengham",
                          str_detect(city_extraction, "coldenghame") ~ "coldengham",
                          str_detect(city_extraction, "st germain") ~ "saint germain",
                          str_detect(city_extraction, "lincolnsinnfields") ~ "london",
                          str_detect(city_extraction, "wills creek") ~ "wills creek",
                          str_detect(city_extraction, "fort dunwiddie") ~ "fort dunwiddie",
                          str_detect(city_extraction, "williamsburgh") ~ "williamsburg",
                          str_detect(city_extraction, "williamsbg") ~ "williamsburg",
                          str_detect(city_extraction, "wmburgh") ~ "williamsburg",
                          str_detect(city_extraction, "williamsburgh") ~ "williamsburg",
                          str_detect(city_extraction, "williamsbgh") ~ "williamsburg",
                          str_detect(city_extraction, "allenstown") ~ "allen's town",
                          str_detect(city_extraction, "dogues neck") ~ "dogues neck",
                          str_detect(city_extraction, "maidstone") ~ "maidstone",
                          str_detect(city_extraction, "fort loudoun") ~ "fort loudoun",
                          str_detect(city_extraction, "ort loudoun") ~ "fort loudoun",
                          str_detect(city_extraction, "lewisfort loudoun") ~ "fort loudoun",
                          str_detect(city_extraction, "fort fredrick") ~ "fort frederick",
                          str_detect(city_extraction, "richmd county") ~ "richmond county",
                          str_detect(city_extraction, "bladensbg") ~ "bladensburg",
                          str_detect(city_extraction, "carlile") ~ "carlisle",
                          str_detect(city_extraction, "raystown") ~ "raystown",
                          str_detect(city_extraction, "raestown") ~ "raystown",
                          str_detect(city_extraction, "raes town") ~ "raystown",
                          str_detect(city_extraction, "rays town") ~ "raystown",
                          str_detect(city_extraction, "reas town") ~ "raystown",
                          str_detect(city_extraction, "pensyla") ~ "pennsylvania",
                          str_detect(city_extraction, "pensylva") ~ "pennsylvania",
                          str_detect(city_extraction, "cresaps") ~ "cresap's",
                          str_detect(city_extraction, "montreall") ~ "montreal",
                          str_detect(city_extraction, "wellingborough") ~ "wellingborough",
                          str_detect(city_extraction, "fairhill") ~ "fairhill",
                          str_detect(city_extraction, "wanstead") ~ "london",
                          str_detect(city_extraction, "bulskin") ~ "bullskin",
                          str_detect(city_extraction, "bullskin") ~ "bullskin",
                          str_detect(city_extraction, "bulskn") ~ "bullskin",
                          str_detect(city_extraction, "yorke") ~ "york",
                          str_detect(city_extraction, "fort adams") ~ "fort adams",
                          str_detect(city_extraction, "fort augusta") ~ "fort augusta",
                          str_detect(city_extraction, "antigua") ~ "antigua",
                          str_detect(city_extraction, "newcastle tyne") ~ "newcastle on tyne",
                          str_detect(city_extraction, "mt airy") ~ "mount airy",
                          str_detect(first_sentence, "scituate") ~ "scituate",
                          .default = NA_character_))

# save results
saveRDS(letter_ss, file = "data/interim/geo_extraction.rds")

# if city name is still missing, try to look it up in unique_cities list of cities
num_cores <- parallel::detectCores()
cat("Number of CPU cores:", num_cores, "\n")

# Set up the multisession plan
plan(multisession, workers = num_cores - 1)

print("Start")
tic <- Sys.time()

# match city names in a case-insensitive way and as whole words
pattern  <- paste0("(?i)(", paste0(unique_cities$city, collapse = "\\b|\\b"), "\\b)")

# Define the function to extract city based on the condition
extract_city <- function(city_extraction, city) {
  city[is.na(city)] <- str_extract(city_extraction[is.na(city)], pattern)
  return(city)
}

# Parallelise the city extraction process
letter_ss <- letter_ss %>%
  mutate(
    city = future_map2(
      .x = city_extraction,
      .y = city,
      .f = extract_city,
      .progress = TRUE
    )
  )

toc <- Sys.time()
print(paste0("Matching the city name took ", round(toc - tic, digits = 2)))

# Explicitly stop parallel processing
plan("sequential")

# unlist city column
letter_ss <- letter_ss %>%
  mutate(city = sapply(city, function(lst) paste(lst, collapse = ",")))

# save results
saveRDS(letter_ss, file = "data/interim/geo_extraction.rds")
#letter_ss <- readRDS("data/interim/geo_extraction.rds") # city only

city_table <- table(letter_ss$city)
# Filter out occurrences with a minimum frequency of 6
filtered_city_table <- city_table[city_table >= 10]


## B) Extract us-state name ----

#In 1835, there were only 24 U.S. states.
# We look for full state names and state abbreviations
state_names <- c("delaware", "pennsylvania", "new jersey", "georgia", "connecticut",
                 "massachusetts", "maryland", "south carolina", "new hampshire",
                 "virginia", "new york", "north carolina", "rhode island", "vermont",
                 "kentucky", "tennessee", "ohio", "louisiana", "indiana",
                 "mississippi", "illinois", "alabama", "maine", "missouri",
                 "district of columbia")

state_names <- sort(state_names)

state_abbreviations <- c("de", "pa", "nj",
                         "ga", "ct", "ma",
                         "md", "sc", "nh",
                         "va", "ny", "nc",
                         "ri", "vt", "ky",
                         "tn", "oh", "la",
                         "in", "ms", "il",
                         "al", "me", "mo",
                         "dc")
state_abbreviations <- sort(state_abbreviations)

# Define common abbreviations using 3 or 4 characters
state_abbreviations_common_3_4 <- c("ala" , "conn", "del" ,"kent",
                                    "geo" , "ill", "indi" ,"lou",
                                    "mary", "mass","hamp", "jers", "caro", "ohio",
                                    "penn", "tenn", "verm", "virg")
state_abbreviations_common_3_4 <- sort(state_abbreviations_common_3_4)

state_patterns <- paste0("(?i)\\b(", paste0(c(state_abbreviations,
                                              state_abbreviations_common_3_4,
                                              state_names), collapse = "|"), ")\\b")

# Extract state information from location variable, i.e., the information
# between brackets in the original text
letter_ss$state <- str_extract(letter_ss$location, state_patterns)

# Next, we remove state information from location variable
letter_ss$location <- mapply(function(loc, st) gsub(paste0(",?\\s*", st, "\\s*"), "", loc),
                             letter_ss$location, letter_ss$state)

# Next, we look for information in city extraction variable
state_abbreviations <- c("pa", "nj",
                         "ga", "ct", "md",
                         "sc", "nh", "ny",
                         "nc", "ri", "vt", "ky",
                         "tn", "oh","ms", "mo", "dc")

state_patterns <- paste0("(?i)\\b(", paste0(c(state_abbreviations_common_3_4,
                                              state_abbreviations,
                                              state_names), collapse = "|"), ")\\b")

# Update state column only when it is NA and city_extraction is not NA
letter_ss <- letter_ss %>%
  mutate(state = if_else(is.na(state) & !is.na(city_extraction),
                         str_extract(city_extraction, state_patterns),
                         state))

# Extract state information directly from first_sentence if state is still NA
letter_ss <- letter_ss %>%
  mutate(state = if_else(is.na(state),
                         str_extract(first_sentence, state_patterns),
                         state))

# replace full spelled-out state names with their abbreviations
letter_ss <- letter_ss %>%
  mutate(state = case_when(
    state == "alabama"        ~ str_replace(state, "alabama", "al"),
    state == "connecticut"    ~ str_replace(state, "connecticut", "ct"),
    state == "conn"           ~ str_replace(state, "conn", "ct"),
    state == "delaware"       ~ str_replace(state, "delaware", "de"),
    state == "del"            ~ str_replace(state, "del", "de"),
    state == "georgia"        ~ str_replace(state, "georgia", "ga"),
    state == "illinois"       ~ str_replace(state, "illinois", "il"),
    state == "ill"            ~ str_replace(state, "ill", "il"),
    state == "indiana"        ~ str_replace(state, "indiana", "in"),
    state == "kentucky"       ~ str_replace(state, "kentucky", "ky"),
    state == "louisiana"      ~ str_replace(state, "louisiana", "la"),
    state == "maine"          ~ str_replace(state, "maine", "me"),
    state == "main"           ~ str_replace(state, "main", "me"),
    state == "maryland"       ~ str_replace(state, "maryland", "md"),
    state == "massachusetts"  ~ str_replace(state, "massachusetts", "ma"),
    state == "mass"           ~ str_replace(state, "mass", "ma"),
    state == "mississippi"    ~ str_replace(state, "mississippi", "ms"),
    state == "missouri"       ~ str_replace(state, "missouri", "mo"),
    state == "new hampshire"  ~ str_replace(state, "new hampshire", "nh"),
    state == "new jersey"     ~ str_replace(state, "new jersey", "nj"),
    state == "new york"       ~ str_replace(state, "new york", "ny"),
    state == "north carolina" ~ str_replace(state, "north carolina", "nc"),
    state == "ohio"           ~ str_replace(state, "ohio", "oh"),
    state == "pennsylvania"   ~ str_replace(state, "pennsylvania", "pa"),
    state == "rhode island"   ~ str_replace(state, "rhode island", "ri"),
    state == "south carolina" ~ str_replace(state, "south carolina", "sc"),
    state == "tennessee"      ~ str_replace(state, "tennessee", "tn"),
    state == "vermont"        ~ str_replace(state, "vermont", "vt"),
    state == "virginia"       ~ str_replace(state, "virginia", "va"),
    state == "virg"           ~ str_replace(state, "virg", "va"),
    state == "washington"     ~ str_replace(state, "washington", "dc"),
    state == "district of columbia" ~ str_replace(state, "district of columbia", "dc"),
    TRUE ~ state  # Keep the original value if not found in the mapping
  ))

distinct(letter_ss, state)

# Define the state mapping rules
 state_mapping <- case_when(
  str_detect(letter_ss$first_sentence," mass") ~ "ma",
  str_detect(letter_ss$first_sentence,"newyork") ~ "ny",
  str_detect(letter_ss$first_sentence,"n york") ~ "ny",
  str_detect(letter_ss$first_sentence,"penn") ~ "pa",
  str_detect(letter_ss$first_sentence,"chester pa") ~ "pa",
  str_detect(letter_ss$first_sentence,"county pa") ~ "pa",
  str_detect(letter_ss$first_sentence,"chesnut hill pa") ~ "pa",
  str_detect(letter_ss$first_sentence,"dc") ~ "dc",
  str_detect(letter_ss$first_sentence,"maryland") ~ "md",
  str_detect(letter_ss$first_sentence,"havre de grace") ~ "md",
  str_detect(letter_ss$first_sentence,"george town potomac") ~ "dc",
  str_detect(letter_ss$first_sentence,"george town columa") ~ "dc",
  str_detect(letter_ss$first_sentence,"george town columbia") ~ "dc",
  str_detect(letter_ss$first_sentence,"west point") ~ "ny",
  str_detect(letter_ss$first_sentence,"alabama") ~ "al",
  str_detect(letter_ss$first_sentence,"washington") ~"dc",
  str_detect(letter_ss$first_sentence,"washington co") ~"co",
  str_detect(letter_ss$first_sentence,"george town co") ~"co",
  str_detect(letter_ss$first_sentence,"connecticut") ~"ct",
  str_detect(letter_ss$first_sentence,"conn") ~"ct",
  str_detect(letter_ss$first_sentence,"danbury conn") ~"ct",
  str_detect(letter_ss$first_sentence,"delaware") ~"de",
  str_detect(letter_ss$first_sentence,"dover del") ~"de",
  str_detect(letter_ss$first_sentence,"columbia") ~"dc",
  str_detect(letter_ss$first_sentence,"georgia") ~"ga",
  str_detect(letter_ss$first_sentence,"guam") ~"gu",
  str_detect(letter_ss$first_sentence,"illinois") ~"il",
  str_detect(letter_ss$first_sentence,"alexandria") ~ "va",
  str_detect(letter_ss$first_sentence,"salem") ~ "va",
  str_detect(letter_ss$first_sentence,"indiana") ~"in",
  str_detect(letter_ss$first_sentence,"kentucky") ~"ky",
  str_detect(letter_ss$first_sentence,"louisiana") ~"la",
  str_detect(letter_ss$first_sentence,"maine") ~"me",
  str_detect(letter_ss$first_sentence,"maryland") ~"md",
  str_detect(letter_ss$first_sentence,"massachusetts") ~"ma",
  str_detect(letter_ss$first_sentence,"massts") ~"ma",
   str_detect(letter_ss$first_sentence,"braintree") ~"ma",
  str_detect(letter_ss$first_sentence,"jacksonboro") ~"sc",
  str_detect(letter_ss$first_sentence,"mount vernon") ~"va",
  str_detect(letter_ss$first_sentence,"mississippi") ~"ms",
  str_detect(letter_ss$first_sentence,"missouri") ~"mo",
  str_detect(letter_ss$first_sentence,"new hampshire") ~"nh",
  str_detect(letter_ss$first_sentence,"newhamshire") ~"nh",
  str_detect(letter_ss$first_sentence,"hampshire") ~"nh",
  str_detect(letter_ss$first_sentence,"n hamps") ~"nh",
  str_detect(letter_ss$first_sentence,"charlestown nh") ~"nh",
  str_detect(letter_ss$first_sentence,"durham nh") ~"nh",
  str_detect(letter_ss$first_sentence,"exeter nh") ~"nh",
  str_detect(letter_ss$first_sentence,"new hampshire") ~"nh",
  str_detect(letter_ss$first_sentence,"newhampshire") ~"nh",
  str_detect(letter_ss$first_sentence,"new hampshe") ~"nh",
  str_detect(letter_ss$first_sentence,"new jersey") ~"nj",
  str_detect(letter_ss$first_sentence,"n jersy") ~"nj",
  str_detect(letter_ss$first_sentence,"n jersey") ~"nj",
  str_detect(letter_ss$first_sentence,"state of n york") ~"ny",
  str_detect(letter_ss$first_sentence,"washington ny") ~"ny",
  str_detect(letter_ss$first_sentence,"washington nyst") ~"ny",
  str_detect(letter_ss$first_sentence,"north carolina") ~"nc",
  str_detect(letter_ss$first_sentence,"no carolina") ~"nc",
  str_detect(letter_ss$first_sentence,"north carolinia") ~"nc",
  str_detect(letter_ss$first_sentence,"northcarolina") ~"nc",
   str_detect(letter_ss$first_sentence,"n carolina") ~"nc",
   str_detect(letter_ss$first_sentence,"ncar") ~"nc",
   str_detect(letter_ss$first_sentence,"edenton nc") ~"nc",
   str_detect(letter_ss$first_sentence,"mount airy") ~ "nc",
   str_detect(letter_ss$first_sentence,"north dakota") ~"nd",
   str_detect(letter_ss$first_sentence,"northern mariana islands") ~"mp",
   str_detect(letter_ss$first_sentence,"ohio") ~"oh",
   str_detect(letter_ss$first_sentence,"oklahoma") ~"ok",
   str_detect(letter_ss$first_sentence,"oregon") ~"or",
   str_detect(letter_ss$first_sentence,"pennsylvania") ~"pa",
   str_detect(letter_ss$first_sentence,"pensilvania") ~"pa",
   str_detect(letter_ss$first_sentence,"pensylvania") ~"pa",
   str_detect(letter_ss$first_sentence,"penna") ~"pa",
   str_detect(letter_ss$first_sentence,"pensylva") ~"pa",
   str_detect(letter_ss$first_sentence,"bristol pa") ~"pa",
   str_detect(letter_ss$first_sentence,"carlisle pa") ~"pa",
   str_detect(letter_ss$first_sentence,"doylestown pa") ~"pa",
   str_detect(letter_ss$first_sentence,"goshen pa") ~"pa",
   str_detect(letter_ss$first_sentence,"easton pa") ~"pa",
   str_detect(letter_ss$first_sentence,"at the forks of delaware") ~"pa",
   str_detect(letter_ss$first_sentence,"puerto rico") ~"pr",
   str_detect(letter_ss$first_sentence,"rhode island") ~"ri",
   str_detect(letter_ss$first_sentence,"state rhode islandc") ~"ri",
   str_detect(letter_ss$first_sentence,"rhode isld") ~"ri",
   str_detect(letter_ss$first_sentence,"rhodeisland") ~"ri",
   str_detect(letter_ss$first_sentence,"east greenwich ri") ~"ri",
   str_detect(letter_ss$first_sentence,"east greenwich r island") ~"ri",
   str_detect(letter_ss$first_sentence,"tiverton ri") ~"ri",
   str_detect(letter_ss$first_sentence,"bristol ri") ~"ri",
   str_detect(letter_ss$first_sentence,"south carolina") ~"sc",
   str_detect(letter_ss$first_sentence,"so carolina") ~"sc",
   str_detect(letter_ss$first_sentence,"camden sc") ~"sc",
   str_detect(letter_ss$first_sentence,"charleston sc") ~"sc",
   str_detect(letter_ss$first_sentence,"tennessee") ~"tn",
   str_detect(letter_ss$first_sentence,"trust territories") ~"tt",
   str_detect(letter_ss$first_sentence,"vermont") ~"vt",
   str_detect(letter_ss$first_sentence,"virginia") ~ "va",
   str_detect(letter_ss$first_sentence,"virga") ~ "va",
   str_detect(letter_ss$first_sentence,"chesterfield co ho") ~ "va",
   str_detect(letter_ss$first_sentence,"chesterfield ct hs") ~ "va",
   str_detect(letter_ss$first_sentence,"chesterfield ct ho") ~ "va",
   str_detect(letter_ss$first_sentence,"chesterfield ct") ~ "va",
   str_detect(letter_ss$first_sentence,"chesterfield court ho") ~ "va",
   str_detect(letter_ss$first_sentence,"chesterfield court house") ~ "va",
   str_detect(letter_ss$first_sentence,"chesterfield co") ~ "va",
   str_detect(letter_ss$first_sentence,"gunstonhall") ~ "va",
   str_detect(letter_ss$first_sentence,"gunston hall") ~ "va",
   str_detect(letter_ss$first_sentence,"dumfries va") ~ "va",
   str_detect(letter_ss$first_sentence,"virgin islands") ~"vi",
   str_detect(letter_ss$first_sentence,"hiladelphia")  ~ "pa",
   str_detect(letter_ss$first_sentence,"philadelphia")  ~ "pa",
   str_detect(letter_ss$first_sentence,"providence")  ~ "ri",
   str_detect(letter_ss$first_sentence,"boston")  ~ "ma",
   str_detect(letter_ss$first_sentence,"baltimore") ~ "md",
   str_detect(letter_ss$first_sentence,"monticello")  ~ "va",
   str_detect(letter_ss$first_sentence,"wilmington d")  ~ "de",

   str_detect(letter_ss$city ,"coldengham") ~"ny",
   str_detect(letter_ss$city ,"nashville") ~"tn",
   str_detect(letter_ss$city ,"natchez") ~"ms",
   str_detect(letter_ss$city ,"bremo") ~"va",
   str_detect(letter_ss$city ,"new orleans") ~"la",
   str_detect(letter_ss$city, "philadelphia") ~ "pa",
   TRUE ~ NA_character_
 )

# Apply the state mapping rules
 letter_ss$state <- case_when(
   !is.na(letter_ss$state) ~ letter_ss$state,
   TRUE ~ state_mapping
 )

# 6. MOST LIKELY PLACE OF PUBLICATION ----

## A) create a time variable for each letter ----
letter_ss <- letter_ss %>%
  mutate(time = date(sending_date),
         timestart = min(time),

         # number of days from earliest sending date in revolution (1775-04-19)
         #to sending date of current row
         timediff = as.numeric(time - timestart)) %>%
  arrange(time) %>%
  select(-time,-timestart)

# subset of data where city information is available
temp_city <- letter_ss %>%
  select(authors, timediff, city) %>%
  rename(timedifference = timediff) %>%
  filter(!is.na(city))

# Convert to data.table
setDT(temp_city)

# subset of data where state information is available
temp_state <- letter_ss %>%
  select(authors,timediff,state) %>%
  rename(timedifference = timediff) %>%
  filter(!is.na(state))

# Convert to data.table
setDT(temp_state)

## B) functions to find most likely place and state of publication ----

#   This function can be applied when the place, state and/or country of
#   publication of an author at a particular time point is unknown. If so,
#   search for a time span of -30 and +30 days around that time point to see if
#   the author has sent another letter in which the location is being mentioned.
#   If so, this location can then be used/copied as the most likely place, state
#   or country where the letter was written, assuming that the author lived in
#   the same place during the time interval of max 30 days.

# find city of publication
city_send_from <- function(rec, dt) {

  setDT(temp_city) # Convert temp to a data.table

  # Create an index on the authors and time-difference columns
  setkeyv(temp_city, c("authors", "timedifference"))

  # Filter the rows using vectorized filtering
  temp2 <- temp_city[authors %in% rec & timedifference >= (dt - 30) & timedifference <= (dt + 30)] %>%

    # Find the row with the minimum absolute difference
    slice(which.min(abs(dt - timedifference)))

  destin <- ifelse(nrow(temp2) == 1, temp2$city, NA)
  return(destin)
}

# find state of publication
state_send_from <- function(rec, dt) {

  # Create an index on the authors and time-difference columns
  setkeyv(temp_state, c("authors", "timedifference"))

  # Filter the rows using vectorized filtering
  temp2 <- temp_state[authors %in% rec & timedifference >= (dt - 30) & timedifference <= (dt + 30)] %>%

    # Find the row with the minimum absolute difference
    slice(which.min(abs(dt - timedifference)))

  destin <- ifelse(nrow(temp2) == 1, temp2$state, NA)
  return(destin) # returns with a retrieved city name, if any
}

# retrieve cityname from surrounding data if still missing
letter_ss$city <- ifelse(is.na(letter_ss$city),
                          map2(letter_ss$authors,   # input vector 1 name of author
                               letter_ss$timediff,  # input vector 2 time difference
                               city_send_from),     # function to apply
                         letter_ss$city) %>%        # city name when it was not missing
                         unlist()

# retrieve state name from surrounding data if still missing
letter_ss$state <- ifelse(is.na(letter_ss$state),
                           map2(letter_ss$authors,   # input vector 1 name of author
                                letter_ss$timediff,  # input vector 2 time difference
                                state_send_from),    # function to apply
                         letter_ss$state) %>%        # state name when it was not missing in the first place
                         unlist()

# Replace "NA" with real NA
letter_ss$city[letter_ss$city == "NA"] <- NA

letter_ss$city  <- trimws(letter_ss$city)
letter_ss$state <- trimws(letter_ss$state)

#check how many cities are still missing after applying state function
sum(is.na(letter_ss$city)) # 2345

#check how many states are still missing after applying state function
sum(is.na(letter_ss$state)) # 2831

# save results
saveRDS(letter_ss, file = "data/interim/geo_extraction_c_s.rds") # city and state
letter_ss <- readRDS("data/interim/geo_extraction_c_s.rds")

# 7. Extract country name ----

# Keep only the unique cities names from the whole world (i.e., America and Europe)
df_unique_cities <- world_cities %>% distinct(city, .keep_all = TRUE)

# Extract unique countries from the dataset
unique_countries  <- paste0("(?i)(", paste0(unique(df_unique_cities$country),
                                            collapse = "\\b|\\b"), "\\b)")

# worldcities and their country selected based on highest population
world_cities_max_pop <- world_cities %>%
  group_by(city) %>%
    filter(population == max(population)) %>%
  ungroup() %>%
  select(country, city, state, population)

extract_state <- function(text, city, state) {

  if (!is.na(city) & is.na(state)) {
    state <- world_cities_max_pop$state[world_cities_max_pop$city == city]
  }
  return(ifelse(length(state) > 0, state, NA))
}

# Apply the function
letter_ss$state <- mapply(extract_state,
                          letter_ss$first_sentence,
                          letter_ss$city,
                          letter_ss$state)

state_abbreviations <- c("de", "pa", "nj",
                         "ga", "ct", "ma",
                         "md", "sc", "nh",
                         "va", "ny", "nc",
                         "ri", "vt", "ky",
                         "tn", "oh", "la",
                         "in", "ms", "il",
                         "al", "me", "mo",
                         "dc")

extract_country <- function(text, city, state) {

  if (!is.na(state) & state %in% state_abbreviations) {
  #if (!is.na(state) & state != "eng" ) {
    return("united states")
  }

  if (!is.na(state) & state == "eng") {
    return("united kingdom")
  }

  #if (!is.na(city) & is.na(state)) {
  #  country <- df_unique_cities$country[df_unique_cities$city == city]

   if (!is.na(city) & is.na(state)) {
    country <- world_cities_max_pop$country[world_cities_max_pop$city == city]
  } else {
    country_mentioned <- stringr::str_extract(text, pattern = unique_countries)

    if (length(country_mentioned) > 0) {
      return(ifelse(country_mentioned[1] != "", country_mentioned[1], NA))
    } else {
      return(NA)
    }
  }

  return(ifelse(length(country) > 0, country, NA))
}

letter_ss$country <- mapply(extract_country,
                            letter_ss$first_sentence,
                            letter_ss$city,
                            letter_ss$state)


temp_country <- letter_ss %>%
  select(authors,timediff,country) %>%
  rename(timedifference = timediff) %>%
  filter(!is.na(country))

# Convert to data.table
setDT(temp_country)

# find country of publication
country_send_from <- function(rec, dt) {

  # Create an index on the authors and time-difference columns
  setkeyv(temp_country, c("authors", "timedifference"))

  # Filter the rows using vectorized filtering
  temp2 <- temp_country[authors %in% rec & timedifference >= (dt - 30) & timedifference <= (dt + 30)] %>%

    # Find the row with the minimum absolute difference
    slice(which.min(abs(dt - timedifference)))

  destin <- ifelse(nrow(temp2) == 1, temp2$country, NA)
  return(destin) # returns with a retrieved country name, if any
}

# retrieve country name from surrounding data if still missing
letter_ss$country <- ifelse(is.na(letter_ss$country),
                         map2(letter_ss$authors,     # input vector 1 name of author
                              letter_ss$timediff,    # input vector 2 time difference
                              country_send_from),    # function to apply
                              letter_ss$country) %>% # city name when it was not missing
                              unlist()

letter_ss <- letter_ss %>%
  mutate(country = case_when(
    city == "brest"        ~ str_replace(country, "belarus", "france"),
    city == "paris"        ~ str_replace(country, "united states", "france"),
    TRUE ~ country
  )
)

# 7. MERGE WITH CITY COORDINATES

## merge with longitude and latitude information
letter_ss <- letter_ss %>%
  left_join(city_coordinates,
            by = c("city"    = "city",
                   "state"   = "state",
                   "country" = "country")) %>%
  select(-timediff)
  #select(-timediff, -city_extraction, -first_sentence)

# save results
saveRDS(letter_ss, file = "data/processed/founders/founders_geo_ref.rds")
