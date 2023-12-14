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

# 2. LOAD PREPROCESSED FO LETTER DATA ----
letters <-  readRDS(file = "letters.rds")

# 3. EXTRACTING CITY NAMES FROM FIRST SENTENCES ----

## 3.1. Extracting first sentence ----

letter_ss <- letters %>% select(-amount,-diff)

# Extract first sentence in which place of publication is mentioned
letter_ss$first <- stringr::word(string = letter_ss$content,
                                 start = 1, end = 13,
                                 sep = stringr::fixed(" "))

# Remove all special characters except space
letter_ss$first <- gsub("[^a-zA-Z ]", "", letter_ss$first)

letter_ss$first <- tolower(letter_ss$first)

# remove frequently occurring words that are not relevant for retrieving city
# and state name
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
               "william washington", "betty washington", "john washington","john stine washington",
               "lund washington","martha washington", "samuel washington", "général washington", "john glover", "nathanael greene","instructs greene", "john hancoc","robert hanson",
               "benjamin harrison", "robert hanson harrison,","william heath", "instructs heath", "nathaniel warner", "david waterbury",
               "to the citizens of","to the citizens of the county", "colonel", "sent wentworth",
               "fellow citizen of","fellow citizens of", "general", "peter webster", "john ine washington",
               "john augustine washington","james cleveland", "advance","allen mclane",
               "letter", "sir ", "colonel chester", "gentn", "harrison",  "stringer",
               "george washington", "franklin","john adams smith", "smith","dear johnson",
               "thomas jefferson","jefferson","dear jefferson", "john adams","warren","harriet",
               "transfer","shaw","post","only","portia","adams", "mon cher",
               "caroline","the united states","young","universal","humphreys",
               "united states","yours"," your","in order","yesterday","i had",
               "having had","we have","received", "you have", "dennis",
               "edward newenham",
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
                    "1st","2nd","3rd","4th","5th","6th","7th", "7nd","8th","9th",
                    "10th","11th","12th","13th","14th","15th","16th","17th",
                    "18th","19th","20th","21st","22th","22nd","23th","23rd",
                    "24th","25th","26th","27th","28th","29th","30th","31th",
                    "31st", "22d","tem","late","dearest","oclock", "o’clock",
                    "things","time","day","morning", "mr", "mr.","letter",
                    "mat","excellency","paper","les","circumstance","article",
                    "consequence","john","gentleman","reason","people","opinion",
                    "character","william", "favour","excelly","excellencys",
                    "excellency’s","williams","answer","shall","one","could",
                    "would", "upon","may","&","every","much","might", "with",
                    "without","two","us","yet","since","also","therefore",
                    "however","never","ever","soon","say","take","give","well",
                    "see","mch","sir","mr","mr.","get","give","want","many",
                    "part","time", "wh","ditto","day","letter","esqr","mrs",
                    "letter","day","person","post","purpose","measure","answer",
                    "mat","subject","circumstance","manner","moment","gentleman",
                    "yesterday","instant","pa","week","par","night","event",
                    "object","paper","month","favour","favor", "reason","regard",
                    "principle","matter","instance","question","time","inst",
                    "degree","","occasion","honble","hour","behalf","particular",
                    "van","word", "correspondence","issue","lettre","mr")

# These words in the stopwords list we actually do want to keep in the corpus
# because these are often used as abbreviations for american states
remove_from_stopwords <- c("al","ak","az","ar","ca","co","ct","de","fl","ga",
                           "hi","id","il","in","ia","ks","ky","la","me","md",
                           "ma","mi","mn","ms","mo","mt","ne","nv","nh","nj",
                           "nm","ny","nc","nd","oh","ok","or","pa","ri","sc",
                           "sd","tn","tx","ut","vt","va","wa","wv","wi","wy",
                           "new","pa","n","va","md","mill","ny","so","point",
                           "nj","ga","nc","pa","vt","md","del","m","st")

all_stop_words <- all_stop_words[!all_stop_words %in% remove_from_stopwords]
all_stop_words <- c(all_stop_words, bad_words)
all_stop_words <- unique(all_stop_words)

# remove stopwords from first sentence
letter_ss$first <- tm::removeWords(letter_ss$first, all_stop_words)
letter_ss$first <- str_squish(letter_ss$first)

# 4. CITY NAMES AND COORDINATES ----

## A) US cities grounded before 1825 ----
# https://en.wikipedia.org/wiki/List_of_North_American_settlements_by_year_of_foundation
# https://opendata.stackexchange.com/questions/13613/is-there-a-dataset-for-historical-us-towns-and-roads
# https://query.wikidata.org/#select%20distinct%20%3FsLabel%20%3Flabel%20%3Fyear%20%3Fcoordinates%20%7B%0A%20%3Fs%20wdt%3AP31%2Fwdt%3AP279%2a%20wd%3AQ3327870%3B%0A%20%20%20%20wdt%3AP571%20%3Finception%3B%0A%20%20%20%20wdt%3AP625%20%3Fcoordinates%3B%0A%20%20%20%20wdt%3AP131%2Fwdt%3AP131%2Fwdt%3AP5086%20%3Flabel%0A%20bind%20%28year%28%3Finception%29%20as%20%3Fyear%29%0A%20filter%20%28%3Fyear%3C%3D1783%29%0A%20service%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22en%22%20%7D%0A%7D%20order%20by%20%3Fyear%20%3FsLabel
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

## B) All existing US cities (population > 1000) ----
# data taken from https://public.opendatasoft.com/explore/dataset/geonames-all-cities-with-a-population-1000/information/?disjunctive.cou_name_en&sort=name
uscities <- read.csv("../../Data/Cities/2_us_cities.csv", sep = ',') %>%
  as.data.frame() %>%
  rename_with(tolower) %>%
  mutate(city = tolower(city)) %>%
  distinct(city,state_name, .keep_all = TRUE)

## C) All cities in the world ----
allcities <- read.csv("../../Data/Cities/allcities.csv", sep = ';') %>%
  as.data.frame() %>%
  mutate(Name = tolower(Name)) %>%
  rename(city = Name) %>%
  separate("Coordinates", c("latitude","longitude"), sep = " ",remove = F) %>%
  select(city,Country.name.EN ,Timezone,latitude,longitude) %>%

  # we only want cities in Europe and america
  filter(str_detect(Timezone, "Europe|America"))

allcities$latitude  <- as.numeric(gsub(",","",allcities$latitude))
allcities$longitude <- as.numeric(allcities$longitude)

# This expression, with the case-insensitive modifier and word boundaries, is
# used to match city names in a case-insensitive way and as whole words
# < 1825
pattern  <- paste0("(?i)(", paste0(cities$city,   collapse = "\\b|\\b"), "\\b)")

# 5. MATCHING CITIES IN THE LETTERS ----

## A) city name ----
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
letter_ss$first <- str_replace(letter_ss$first, "sir.", "")
letter_ss$first <- str_replace(letter_ss$first, "auteuil", "paris")

letter_ss <- letter_ss %>%
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
                          str_detect(first, "new haven") ~ "new haven",
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
                          str_detect(first, "rouen") ~ "rouen",
                          str_detect(first, "pownalborough") ~ "pownalborough",
                          str_detect(first, "frenchmans bay") ~ "frenchmans bay",
                          str_detect(first, "fayette ville") ~ "fayette ville",
                          str_detect(first, "tarboh") ~ "tarboh",
                          str_detect(first, "mountpleasant") ~ "mount pleasant",
                          str_detect(first, "mount plesant") ~ "mount pleasant",
                          str_detect(first, "algiers") ~ "algiers",
                          str_detect(first, "eppington") ~ "eppington",
                          str_detect(first, "springhouse") ~ "spring house",
                          str_detect(first, "haie") ~ "the hague",
                          str_detect(first, "bush hill") ~ "philadelphia",
                          str_detect(first, "brussells") ~ "brussells",
                          str_detect(first, "versills") ~ "versailles",
                          str_detect(first, "avignon") ~ "avignon",
                          str_detect(first, "dunkerque") ~ "dunkerque",
                          str_detect(first, "castelnaudarry") ~ "castelnaudarry",
                          str_detect(first, "cheam") ~ "london",
                          str_detect(first, "londres") ~ "london",
                          str_detect(first, "bridgeTown barbadoes") ~ "bridgeTown barbadoes",
                          str_detect(first, "sannois") ~ "sannois",
                          str_detect(first, "maidenhead") ~ "maidenhead",
                          str_detect(first, "reedyisland") ~ "reedy island",
                          str_detect(first, "stenton") ~ "stenton",
                          str_detect(first, "quebec") ~ "quebec",
                          str_detect(first, "lond") ~ "london",
                          str_detect(first, "londn") ~ "london",
                          str_detect(first, "eliz town") ~ "elizabeth town",
                          str_detect(first, "coldengham") ~ "coldengham",
                          str_detect(first, "coldenghame") ~ "coldengham",
                          str_detect(first, "st germain") ~ "saint germain",
                          str_detect(first, "lincolnsinnfields") ~ "london",
                          str_detect(first, "wills creek") ~ "wills creek",
                          str_detect(first, "fort dunwiddie") ~ "fort dunwiddie",
                          str_detect(first, "williamsburgh") ~ "williamsburg",
                          str_detect(first, "williamsbg") ~ "williamsburg",
                          str_detect(first, "wmburgh") ~ "williamsburg",
                          str_detect(first, "williamsburgh") ~ "williamsburg",
                          str_detect(first, "williamsbgh") ~ "williamsburg",
                          str_detect(first, "allenstown") ~ "allen's town",
                          str_detect(first, "dogues neck") ~ "dogues neck",
                          str_detect(first, "maidstone") ~ "maidstone",
                          str_detect(first, "fort loudoun") ~ "fort loudoun",
                          str_detect(first, "ort loudoun") ~ "fort loudoun",
                          str_detect(first, "lewisfort loudoun") ~ "fort loudoun",
                          str_detect(first, "fort fredrick") ~ "fort frederick",
                          str_detect(first, "richmd county") ~ "richmond county",
                          str_detect(first, "bladensbg") ~ "bladensburg",
                          str_detect(first, "carlile") ~ "carlisle",
                          str_detect(first, "raystown") ~ "raystown",
                          str_detect(first, "raestown") ~ "raystown",
                          str_detect(first, "raes town") ~ "raystown",
                          str_detect(first, "rays town") ~ "raystown",
                          str_detect(first, "reas town") ~ "raystown",
                          str_detect(first, "pensyla") ~ "pennsylvania",
                          str_detect(first, "pensylva") ~ "pennsylvania",
                          str_detect(first, "cresaps") ~ "cresap's",
                          str_detect(first, "montreall") ~ "montreal",
                          str_detect(first, "wellingborough") ~ "wellingborough",
                          str_detect(first, "fairhill") ~ "fairhill",
                          str_detect(first, "wanstead") ~ "london",
                          str_detect(first, "bulskin") ~ "bullskin",
                          str_detect(first, "bullskin") ~ "bullskin",
                          str_detect(first, "bulskn") ~ "bullskin",
                          str_detect(first, "yorke") ~ "york",

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

                          # extract city name from
                          .default = stringr::str_extract(letter_ss$first, pattern)))

## B) state name ----

# looking for both state abbreviations and full state names
# Define state codes with full names
state_codes <- c(
  "ala" = "Alabama", "alas" = "Alaska", "az" = "Arizona", "ar" = "Arkansas", "cal" = "California",
  "co" = "Colorado", "ct" = "Connecticut", "del" = "Delaware", "fl" = "Florida", "ga" = "Georgia",
  "hi" = "Hawaii", "id" = "Idaho", "il" = "Illinois", "ind" = "Indiana", "ia" = "Iowa",
  "ks" = "Kansas", "ky" = "Kentucky", "louis" = "Louisiana", "mai" = "Maine", "md" = "Maryland",
  "ma" = "Massachusetts", "mi" = "Michigan", "mn" = "Minnesota", "ms" = "Mississippi",
  "mo" = "Missouri", "mt" = "Montana", "ne" = "Nebraska", "nv" = "Nevada", "nh" = "New Hampshire",
  "nj" = "New Jersey", "nm" = "New Mexico", "ny" = "New York", "nc" = "North Carolina",
  "nd" = "North Dakota", "oh" = "Ohio", "ok" = "Oklahoma", "ore" = "Oregon", "pa" = "Pennsylvania",
  "ri" = "Rhode Island", "sc" = "South Carolina", "sd" = "South Dakota", "tn" = "Tennessee",
  "tx" = "Texas", "ut" = "Utah", "vt" = "Vermont", "va" = "Virginia", "wa" = "Washington",
  "wv" = "West Virginia", "wi" = "Wisconsin", "wy" = "Wyoming"
)

# Define state names
state_names <- c("alabama", "alaska", "arizona", "arkansas", "california",
                 "colorado", "connecticut", "delaware", "florida", "georgia",
                 "hawaii", "idaho", "illinois", "indiana", "iowa",
                 "kansas", "kentucky", "louisiana", "maine", "maryland",
                 "massachusetts", "michigan", "minnesota", "mississippi",
                 "missouri", "montana", "nebraska", "nevada", "new hampshire",
                 "new jersey", "new mexico", "new york", "north carolina",
                 "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania",
                 "rhode island", "south carolina", "south dakota", "tennessee",
                 "texas", "utah", "vermont", "virginia", "washington",
                 "west virginia", "wisconsin", "wyoming")

# Define state abbreviations
state_abbreviations <- c("ak", "az", "ar", "co", "ct", "fl", "ga",
                         "hi", "id", "il", "ia", "ks", "ky", "la", "md",
                         "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh", "nj",
                         "nm", "ny", "nc", "nd", "oh", "pa", "ri", "sc",
                         "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi", "wy")


# Define common abbreviations using 3 or 4 characters
state_abbreviations_common_3_4 <- c("ala", "alas", "ari", "ark", "cali", "colo", "conn", "del",
                                    "fla", "geo", "haw", "ida", "ill", "indi", "iowa", "kans",
                                    "kent", "lou", "main", "mary", "mass", "mich", "minn",
                                    "miss", "mouri", "mont", "neb", "neva", "hamp", "jers",
                                    "mexi", "york", "caro", "dako", "ohio", "okla", "oreg",
                                    "penn", "rhode", "caro", "dako", "tenn", "texa", "utah",
                                    "verm", "virg", "wash", "west", "wisc", "wyo")

# Create a comprehensive pattern with spaces
comprehensive_pattern <- paste0("(?i)(", paste(c(names(state_codes),
                                                 state_abbreviations, state_names),
                                               collapse = "\\b|\\b"), "\\b)")

# Extract state information from the 'letter_ss' dataframe
letter_ss$state <- str_extract(letter_ss$first, comprehensive_pattern)

# Define the state mapping rules
state_mapping <- case_when(
  str_detect(letter_ss$first, " mass") ~ "MA",
  str_detect(letter_ss$first, "newyork") ~ "NY",
  str_detect(letter_ss$first, "n york") ~ "NY",
  str_detect(letter_ss$first, "penn") ~ "PA",
  str_detect(letter_ss$first, "chester pa") ~ "PA",
  str_detect(letter_ss$first, "county pa") ~ "PA",
  str_detect(letter_ss$first, "chesnut hill pa") ~ "PA",
  str_detect(letter_ss$first, "dc") ~ "DC",
  str_detect(letter_ss$first, "maryland") ~ "MD",
  str_detect(letter_ss$first, "havre de grace") ~ "MD",
  str_detect(letter_ss$first, "george town potomac") ~ "DC",
  str_detect(letter_ss$first, "george town columa") ~ "DC",
  str_detect(letter_ss$first, "george town columbia") ~ "DC",
  str_detect(letter_ss$first, "west point") ~ "NY",
  str_detect(letter_ss$first,"alabama") ~ "AL",
  str_detect(letter_ss$first,"alaska") ~"AK",
  str_detect(letter_ss$first,"arizona") ~"AZ",
  str_detect(letter_ss$first,"arkansas") ~"AR",
  str_detect(letter_ss$first,"american samoa") ~"AS",
  str_detect(letter_ss$first,"california") ~"CA",
  str_detect(letter_ss$first,"colorado") ~"CO",
  str_detect(letter_ss$first,"washington co") ~"CO",
  str_detect(letter_ss$first,"cola") ~"CO",
  str_detect(letter_ss$first,"coa") ~"CO",
  str_detect(letter_ss$first,"coo") ~"CO",
  str_detect(letter_ss$first,"george town co") ~"CO",
  str_detect(letter_ss$first,"connecticut") ~"CT",
  str_detect(letter_ss$first,"conn") ~"CT",
  str_detect(letter_ss$first,"danbury conn") ~"CT",
  str_detect(letter_ss$first,"delaware") ~"DE",
  str_detect(letter_ss$first,"dover del") ~"DE",
  str_detect(letter_ss$first,"district of columbia") ~"DC",
  str_detect(letter_ss$first,"columbia") ~"DC",
  str_detect(letter_ss$first,"florida") ~"FL",
  str_detect(letter_ss$first,"georgia") ~"GA",
  str_detect(letter_ss$first,"guam") ~"GU",
  str_detect(letter_ss$first,"hawaii") ~"HI",
  str_detect(letter_ss$first,"idaho") ~"ID",
  str_detect(letter_ss$first,"illinois") ~"IL",
  str_detect(letter_ss$first,"alexandria") ~ "VA",
  str_detect(letter_ss$first,"salem") ~ "VA",
  str_detect(letter_ss$first,"indiana") ~"IN",
  str_detect(letter_ss$first,"iowa") ~"IA",
  str_detect(letter_ss$first,"kansas") ~"KS",
  str_detect(letter_ss$first,"kentucky") ~"KY",
  str_detect(letter_ss$first,"louisiana") ~"LA",
  str_detect(letter_ss$first,"maine") ~"ME",
  str_detect(letter_ss$first,"maryland") ~"MD",
  str_detect(letter_ss$first,"massachusetts") ~"MA",
  str_detect(letter_ss$first,"massts") ~"MA",
  str_detect(letter_ss$first,"braintree") ~"MA",
  str_detect(letter_ss$first,"jacksonboro") ~"SC",
  str_detect(letter_ss$first,"london") ~"-",
  str_detect(letter_ss$first,"paris") ~ "-",
  str_detect(letter_ss$city, "philadelphia") ~ "PA",
  str_detect(letter_ss$first,"mount vernon") ~"VA",
  str_detect(letter_ss$first,"michigan") ~"MI",
  str_detect(letter_ss$first,"minnesota") ~"MN",
  str_detect(letter_ss$first,"mississippi") ~"MS",
  str_detect(letter_ss$first,"missouri") ~"MO",
  str_detect(letter_ss$first,"montana") ~"MT",
  str_detect(letter_ss$first,"nebraska") ~"NE",
  str_detect(letter_ss$first,"nevada") ~"NV",
  str_detect(letter_ss$first,"new hampshire") ~"NH",
  str_detect(letter_ss$first,"newhamshire") ~"NH",
  str_detect(letter_ss$first,"hampshire") ~"NH",
  str_detect(letter_ss$first,"n hamps") ~"NH",
  str_detect(letter_ss$first,"charlestown nh") ~"NH",
  str_detect(letter_ss$first,"durham nh") ~"NH",
  str_detect(letter_ss$first,"exeter nh") ~"NH",
  str_detect(letter_ss$first,"new hampshire") ~"NH",
  str_detect(letter_ss$first,"newhampshire") ~"NH",
  str_detect(letter_ss$first,"new hampshe") ~"NH",
  str_detect(letter_ss$first,"new jersey") ~"NJ",
  str_detect(letter_ss$first,"n jersy") ~"NJ",
  str_detect(letter_ss$first,"n jersey") ~"NJ",
  str_detect(letter_ss$first,"new mexico") ~"NM",
  str_detect(letter_ss$first,"new york") ~"NY",
  str_detect(letter_ss$city,"new york") ~"NY",
  str_detect(letter_ss$first,"state of n york") ~"NY",
  str_detect(letter_ss$first,"washington ny") ~"NY",
  str_detect(letter_ss$first,"washington nyst") ~"NY",
  str_detect(letter_ss$first,"north carolina") ~"NC",
  str_detect(letter_ss$first,"no carolina") ~"NC",
  str_detect(letter_ss$first,"north carolinia") ~"NC",
  str_detect(letter_ss$first,"northcarolina") ~"NC",
  str_detect(letter_ss$first,"n carolina") ~"NC",
  str_detect(letter_ss$first,"ncar") ~"NC",
  str_detect(letter_ss$first,"edenton nc") ~"NC",
  str_detect(letter_ss$first,"mount airy") ~ "NC",
  str_detect(letter_ss$first,"north dakota") ~"ND",
  str_detect(letter_ss$first,"northern mariana islands") ~"MP",
  str_detect(letter_ss$first,"ohio") ~"OH",
  str_detect(letter_ss$first,"oklahoma") ~"OK",
  str_detect(letter_ss$first,"oregon") ~"OR",
  str_detect(letter_ss$first,"pennsylvania") ~"PA",
  str_detect(letter_ss$first,"pensilvania") ~"PA",
  str_detect(letter_ss$first,"pensylvania") ~"PA",
  str_detect(letter_ss$first,"penna") ~"PA",
  str_detect(letter_ss$first,"pensylva") ~"PA",
  str_detect(letter_ss$first,"bristol pa") ~"PA",
  str_detect(letter_ss$first,"carlisle pa") ~"PA",
  str_detect(letter_ss$first,"doylestown pa") ~"PA",
  str_detect(letter_ss$first,"goshen pa") ~"PA",
  str_detect(letter_ss$first,"easton pa") ~"PA",
  str_detect(letter_ss$first,"at the forks of delaware") ~"PA",
  str_detect(letter_ss$first,"puerto rico") ~"PR",
  str_detect(letter_ss$first,"rhode island") ~"RI",
  str_detect(letter_ss$first,"state rhode islandc") ~"RI",
  str_detect(letter_ss$first,"rhode isld") ~"RI",
  str_detect(letter_ss$first,"rhodeisland") ~"RI",
  str_detect(letter_ss$first,"east greenwich ri") ~"RI",
  str_detect(letter_ss$first,"east greenwich r island") ~"RI",
  str_detect(letter_ss$first,"tiverton ri") ~"RI",
  str_detect(letter_ss$first,"bristol ri") ~"RI",
  str_detect(letter_ss$first,"south carolina") ~"SC",
  str_detect(letter_ss$first,"so carolina") ~"SC",
  str_detect(letter_ss$first,"camden sc") ~"SC",
  str_detect(letter_ss$first,"charleston sc") ~"SC",
  str_detect(letter_ss$first,"south dakota") ~"SD",
  str_detect(letter_ss$first,"tennessee") ~"TN",
  str_detect(letter_ss$first,"texas") ~"TX",
  str_detect(letter_ss$first,"trust territories") ~"TT",
  str_detect(letter_ss$first,"utah") ~"UT",
  str_detect(letter_ss$first,"vermont") ~"VT",
  str_detect(letter_ss$first,"virginia") ~ "VA",
  str_detect(letter_ss$first,"virga") ~ "VA",
  str_detect(letter_ss$first,"chesterfield co ho") ~ "VA",
  str_detect(letter_ss$first,"chesterfield ct hs") ~ "VA",
  str_detect(letter_ss$first,"chesterfield ct ho") ~ "VA",
  str_detect(letter_ss$first,"chesterfield ct") ~ "VA",
  str_detect(letter_ss$first,"chesterfield court ho") ~ "VA",
  str_detect(letter_ss$first,"chesterfield court house") ~ "VA",
  str_detect(letter_ss$first,"chesterfield co") ~ "VA",
  str_detect(letter_ss$first, "gunstonhall") ~ "VA",
  str_detect(letter_ss$first, "gunston hall") ~ "VA",
  str_detect(letter_ss$first,"dumfries va") ~ "VA",
  str_detect(letter_ss$first, "king william") ~ "VA",
  str_detect(letter_ss$first,"virgin islands") ~"VI",
  str_detect(letter_ss$first,"washington") ~"WA",
  str_detect(letter_ss$first,"west virginia") ~"WV",
  str_detect(letter_ss$first,"wisconsin") ~"WI",
  str_detect(letter_ss$first,"wyoming") ~"WY",
  str_detect(letter_ss$first, " va") ~ "VA",
  str_detect(letter_ss$first, "hiladelphia")  ~ "PA",
  str_detect(letter_ss$first, "providence")  ~ "RI",
  str_detect(letter_ss$first, "boston")  ~ "MA",
  str_detect(letter_ss$first, "baltimore")  ~ "MD",
  str_detect(letter_ss$first, "monticello")  ~ "VA",
  str_detect(letter_ss$first, "wilmington d")  ~ "DE",
  TRUE ~ NA_character_
)

# Apply the state mapping rules
letter_ss$state <- case_when(
  !is.na(letter_ss$state) ~ letter_ss$state,
  TRUE ~ state_mapping
)

# Filter out non-duplicate cities from the external dataset
non_duplicate_cities <- cities %>%
  group_by(city) %>%
  filter(n() == 1) %>%
  ungroup()

# Left join the filtered external data with letter_ss to get missing state
# information. Now, letter_ss contains both the extracted state information and
# filled-in state information from the external file
letter_ss <- left_join(letter_ss, non_duplicate_cities %>% select(city, state),
                       by = c("city" = "city")) %>%
  mutate(state = ifelse(!is.na(state.x), state.x, state.y)) %>%
  select(-state.x, -state.y)


## C) country name ----

# Keep only the unique cities
df_unique_cities <- allcities %>% distinct(city, .keep_all = TRUE) %>%
  rename(country = Country.name.EN)

# Extract unique countries
unique_countries  <- paste0("(?i)(", paste0(unique(df_unique_cities$country),
                                            collapse = "\\b|\\b"), "\\b)")
unique_countries <- tolower(unique_countries)

# Function to extract country information from letter
extract_country <- function(text, city, state) {

  # if american state is available, then it always has to be the United States
  if (!is.na(state) & state != "-") {
    return("United States")

  } else if (!is.na(city) & is.na(state)) {
    # if the city is known, we can try and match it with our data which has unique city
    country <- df_unique_cities$country[df_unique_cities$city == city]
    return(ifelse(length(country) > 0, country, NA))

  } else {
    # Check if country is explicitly mentioned in the text
    country_mentioned <- stringr::str_extract(text, pattern = unique_countries)

    # If mentioned, return it
    if (length(country_mentioned) > 0) {

      print(paste("Country Mentioned:", country_mentioned))
      # Check for empty strings and treat them as NA
      return(ifelse(country_mentioned[1] != "", country_mentioned[1], NA))

    } else {
      # Return NA when no country is mentioned
      return(NA)
    }
  }
}

# Apply the function
letter_ss$country <- mapply(extract_country,
                            letter_ss$first,
                            letter_ss$city,
                            letter_ss$state)

# some manual corrections for country
letter_ss <- letter_ss %>%
  mutate(country = case_when(
    str_detect(first, "mill prison plymouth") ~ "england",
    str_detect(first, "mill prison") ~ "england",
    str_detect(first, "brittany")    ~ "france",
    str_detect(first, "new orleans") ~ "USA",
    str_detect(first, "orleans")   ~ "france",
    str_detect(first, "nantes")    ~ "france",
    str_detect(first, "turin")     ~ "italy",
    str_detect(first, "normandy")  ~ "france",

    str_detect(city, "london")     ~ "england",
    str_detect(city, "paris")      ~ "france",
    str_detect(city, "marstrand")  ~ "sweden",
    str_detect(city, "masterland") ~ "sweden"))


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

# retrieve cities from surrounding data if missing
letter_ss$city <- ifelse(is.na(letter_ss$city),
                         purrr::map2(letter_ss$authors,   # input vector 1 name of author
                                     letter_ss$timediff,  # input vector 2 time difference
                                     city_send_from),     # function to apply
                         letter_ss$city) %>%  # city name when it was not missing
  unlist()

#check how many are missing after applying city function
sum(is.na(letter_ss$city))

# all us-cities
pattern2 <- paste0("(?i)(", paste0(uscities$city, collapse = "\\b|\\b"), "\\b)")

# if city name is still missing, try to look it up in us_cities large dataframe
letter_ss <- letter_ss %>%
  as.data.frame() %>%
  mutate(city_sent_from = case_when(!is.na(city)  ~ city,
                                    .default = stringr::str_extract(letter_ss$first, pattern2))) %>%
  select(-city)

sum(is.na(letter_ss$city_sent_from)) #116 na's

# retrieve state from surrounding data if missing
letter_ss$state <- ifelse(is.na(letter_ss$state),
                          purrr::map2(letter_ss$authors,
                                      letter_ss$timediff,
                                      state_send_from),

                          letter_ss$state) %>%
  unlist()

#check how many are missing after applying state function
sum(is.na(letter_ss$state))

letter_ss <- letter_ss %>%
  rename(state_sent_from = state) %>%
  select(authors:content,first,timediff,
         city_sent_from,
         state_sent_from,
         country)

# 7. GET COORDINATES OF PLACE OF SENDING ---- TODO: check if this is correct!!!!

# get longitude and latitude information from external file
letter_ss <- letter_ss %>%
  left_join(cities, by = c("city_sent_from"  = "city",
                           "state_sent_from" = "state")) %>%
  rename(lon_from = longitude, lat_from = latitude)

letter_ss$city_sent_from <- ifelse(is.na(letter_ss$city_sent_from),
                                   letter_ss %>% left_join(non_duplicate_cities, by = c("city_sent_from" = "city")),
                                   letter_ss$state)

# first merge with unique cities coordinates
letter_ss <- letter_ss %>% left_join(non_duplicate_cities, by = c("city_sent_from" = "city")) %>%
  rename(lon_from = longitude, lat_from = latitude)

# if state is not empty, we use state in the join to get the correct cities coordinates
letter_ss$state <- ifelse(is.na(letter_ss$state),
                          purrr::map2(letter_ss$authors,letter_ss$timediff,state_send_from),
                          letter_ss$state)

letter_ss <- letter_ss %>% as.data.frame() %>%
  mutate(thijs = case_when(!is.na(city_sent_from) & !is.na(state_sent_from)  ~ city,
                           .default = stringr::str_extract(letter_ss$first, pattern2))) %>%
  select(-city)

# if state is empty
#letter_ss <- letter_ss %>% left_join(cities, by = "city")

letter_ss <- letter_ss %>%
  group_by(city,longitude, latitude) %>%
  mutate(city_count = n()) %>%
  ungroup()



