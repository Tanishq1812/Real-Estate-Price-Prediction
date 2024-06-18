setwd("E:\\R Programming Language")

hd=read.csv("housing_train.csv",stringsAsFactors = FALSE)

library(dplyr)

glimpse(hd)

hd=hd %>% 
  mutate(Rooms=as.numeric(Rooms),
         Price=as.numeric(Price),
         Distance=as.numeric(Distance))

hd=hd %>% 
  mutate(Bedroom=ifelse(is.na(Bedroom2),
                        3,
                        Bedroom2),
         Bathroom=ifelse(is.na(Bathroom),
                         2,
                         Bathroom),
         Car=ifelse(is.na(Car),
                    1,
                    Car),
         Landsize=ifelse(is.na(Landsize),
                         0,
                         Landsize)) %>% 
  select(-Bedroom2)

glimpse(hd)

hd=hd %>% 
  mutate(buildingArea=ifelse(is.na(BuildingArea),
                             round(mean(BuildingArea,na.rm=T)),
                             BuildingArea)) %>% 
  select(-BuildingArea)

glimpse(hd)

hd=hd %>% 
  mutate(Type_h=as.numeric(Type=="h"),
         Type_u=as.numeric(Type=="u")) %>% 
  select(-Type)

glimpse(hd)

table(hd$Method)

hd=hd %>% 
  mutate(Method_PI=as.numeric(Method=="PI"),
         Method_S=as.numeric(Method=="S"),
         Method_SP=as.numeric(Method=="SP"),
         Method_VB=as.numeric(Method=="VB")) %>% 
  select(-Method)

glimpse(hd)

table(hd$YearBuilt)

sort(table(hd$YearBuilt),decreasing=T)

hd=hd %>% 
  mutate(YearBuilt=ifelse(is.na(YearBuilt),
                          0,
                          YearBuilt))

hd=hd %>% 
  mutate(YearBuilt_1970=as.numeric(YearBuilt=="1970"),
         YearBuilt_1960=as.numeric(YearBuilt=="1960"),
         YearBuilt_1950=as.numeric(YearBuilt=="1950"),
         YearBuilt_1900=as.numeric(YearBuilt=="1900"),
         YearBuilt_1930=as.numeric(YearBuilt=="1930"),
         YearBuilt_1920=as.numeric(YearBuilt=="1920"),
         YearBuilt_2000=as.numeric(YearBuilt=="2000"),
         YearBuilt_1910=as.numeric(YearBuilt=="1910"),
         YearBuilt_1890=as.numeric(YearBuilt=="1890"),
         YearBuilt_1940=as.numeric(YearBuilt=="1940"),
         YearBuilt_missing=as.numeric(YearBuilt=="0"),
         YearBuilt_Other=as.numeric(YearBuilt %in% c ("1980", "2012", "1975", "2005", "1965", "2010",
                                                      "2013", "1990", "1995", "2009", "2007", "2011",
                                                      "1925", "1955", "2014", "2008", "1935", "2006",
                                                      "2002", "1880","2004", "1915","1998", "2001", 
                                                      "1997", "2003", "2015", "1985", "1999", "2016",
                                                      "1968", "1993", "1996", "1945","1994","1905",
                                                      "1966", "1938" , "1948", "1978", "1992","1928",
                                                      "1958", "1972", "1973", "1974", "1967", "1969",
                                                      "1976", "1926", "1971", "1977", "1988", "1991",
                                                      "1850", "1885", "1888", "1923", "1927", "1934",
                                                      "1939", "1941", "1943", "1953", "1963", "1979",
                                                      "1986", "1987", "2017", "1912", "1946", "1947",
                                                      "1982", "1886", "1887", "1895", "1902", "1904",
                                                      "1906", "1908", "1919","1924", "1937", "1949", 
                                                      "1951", "1957", "1962", "1964", "1989", "1830",
                                                      "1856", "1860", "1870", "1875","1877", "1884", 
                                                      "1889", "1893", "1894", "1897", "1901", "1903",
                                                      "1907", "1913", "1914", "1916", "1917", "1918", 
                                                      "1922", "1929", "1932", "1933", "1942", "1952", 
                                                      "1954", "1956", "1959", "1981","1983", "1984", "2018"))) %>% 
  select(-YearBuilt)

glimpse(hd)

sort(table(hd$Postcode),decreasing=T)

hd=hd %>% 
  mutate(Postcode_3073=as.numeric(Postcode=="3073"),
         Postcode_3020=as.numeric(Postcode=="3020"),
         Postcode_3165=as.numeric(Postcode=="3165"),
         Postcode_3046=as.numeric(Postcode=="3046"),
         Postcode_3121=as.numeric(Postcode=="3121"),
         Postcode_3032=as.numeric(Postcode=="3032"),
         Postcode_3040=as.numeric(Postcode=="3040"),
         Postcode_3058=as.numeric(Postcode=="3058"),
         Postcode_3163=as.numeric(Postcode=="3163"),
         Postcode_other=as.numeric(Postcode %in% c ("3204", "3072" ,"3182", "3141" ,"3012", "3056" ,"3084",
                                                    "3011", "3181", "3186" ,"3146", "3015", "3122", "3070", "3188", "3042", "3101", "3187",
                                                    "3104", "3081", "3127", "3031", "3145" ,"3044", "3207", "3124", "3071", "3147", "3039",
                                                    "3068", "3108", "3079", "3103", "3013", "3142", "3033", "3184", "3016" ,"3123" ,"3107",
                                                    "3041", "3055" ,"3000", "3143" ,"3125" ,"3166", "3025", "3078" ,"3105", "3034" ,"3206",
                                                    "3060", "3018", "3189", "3205", "3057", "3051", "3144", "3167", "3185", "3066", "3067",
                                                    "3128", "3162", "3148" ,"3126", "3053" ,"3102", "3019" ,"3065", "3006", "3054", "3052",
                                                    "3087" ,"3161", "3002", "3003", "3085", "3043", "3183", "3021" ,"3047", "3083", "3061","3008"))) %>% 
  select(-Postcode)

glimpse(hd)

sort(table(hd$CouncilArea),decreasing = T)

hd=hd %>% 
  mutate(CouncilArea=ifelse(CouncilArea=="",0,CouncilArea))

sort(table(hd$CouncilArea),decreasing = T)

hd=hd %>% 
  mutate(CouncilArea_Boroondara=as.numeric(CouncilArea=="Boroondara"),
         CouncilArea_Moreland=as.numeric(CouncilArea=="Moreland"),
         CouncilArea_MooneeValley=as.numeric(CouncilArea=="Moonee Valley"),
         CouncilArea_Darebin=as.numeric(CouncilArea=="Darebin"),
         CouncilArea_GlenEira=as.numeric(CouncilArea=="Glen Eira"),
         CouncilArea_Stonnington=as.numeric(CouncilArea=="Stonnington"),
         CouncilArea_Bayside=as.numeric(CouncilArea=="Bayside"),    
         CouncilArea_Maribyrnong=as.numeric(CouncilArea=="Maribyrnong"), 
         CouncilArea_Yarra=as.numeric(CouncilArea=="Yarra"),
         CouncilArea_PortPhillip=as.numeric(CouncilArea=="Port Phillip"),
         CouncilArea_missing=as.numeric(CouncilArea=="0"),
         CouncilArea_other=as.numeric(CouncilArea %in% "Banyule","Melbourne","Hobsons Bay","Brimbank",
                                      "Manningham","Whitehorse","Monash","Kingston","Hume")) %>% 
  select(-CouncilArea)

glimpse(hd)

sort(table(hd$Suburb),decreasing = T)

hd=hd %>% 
  mutate(Suburb_Reservoir=as.numeric(Suburb=="Reservoir"),
         Suburb_BentleighEast=as.numeric(Suburb=="Benleigh East"),
         Suburb_Richmond=as.numeric(Suburb=="Richmond"),
         Suburb_Preston=as.numeric(Suburb=="Preston"),
         Suburb_StKilda=as.numeric(Suburb=="St Kilda"),
         Suburb_StKilda=as.numeric(Suburb=="South Yarra "),
         Suburb_other=as.numeric(Suburb %in% c("Brunswick","Glenroy",
                                               "Essendon","Brighton","Glen Iris","Coburg",
                                               "Hawthorn","Northcote","Kew","Brighton East",
                                               "Balwyn North","Bentleigh","Pascoe Vale","Port Melbourne",
                                               "Malvern East","Camberwell","Carnegie","Footscray",
                                               "Thornbury","Moonee Ponds","Doncaster","Ascot Vale",
                                               "Balwyn","Hampton","Newport","Toorak",
                                               "Yarraville","Keilor East","Prahran","Elwood",
                                               "Sunshine","Hawthorn East","Maribyrnong", "Templestowe Lower", 
                                               "Brunswick West","Surrey Hills","Kensington","Sunshine West",
                                               "Ivanhoe","Melbourne","Armadale","Williamstown", 
                                               "Burwood","Ormond","Sunshine North","Altona North", 
                                               "Bulleen","Avondale Heights","Fitzroy North","Strathmore", 
                                               "Fawkner","Ashburton","West Footscray","Maidstone", 
                                               "Moorabbin","Rosanna","South Melbourne","Airport West", 
                                               "Brunswick East", "Heidelberg Heights","Niddrie","Altona", 
                                               "Heidelberg West","Murrumbeena","Coburg North","North Melbourne", 
                                               "Oakleigh South","Malvern","Collingwood","Hadfield", 
                                               "Windsor","Abbotsford","Ashwood","Box Hill", 
                                               "Albert Park","Seddon","Elsternwick","Chadstone", 
                                               "Clifton Hill","Oak Park","Canterbury","Carlton", 
                                               "Flemington","Kew East","Mont Albert","Fairfield", 
                                               "Heidelberg","Oakleigh","Braybrook","Fitzroy", 
                                               "Hampton East","Viewbank","Caulfield South","Southbank", 
                                               "Carlton North","Hughesdale","Alphington","Kingsville", 
                                               "Parkville","Glen Huntly","Aberfeldie","Caulfield North", 
                                               "Watsonia","Albion","Essendon West","Spotswood", 
                                               "Middle Park","East Melbourne","West Melbourne","Yallambie", 
                                               "Gowanbrae","Balaclava","Eaglemont","Ivanhoe East", 
                                               "South Kingsville","Cremorne","Kealba","Keilor Park", 
                                               "Williamstown North","Essendon North","Jacana","Brooklyn", 
                                               "Kingsbury","Bellfield","Caulfield","East Strathmore Heights", 
                                               "Caulfield","Burnley","Campbellfield","Ripponlea", 
                                               "Docklands","Gardenvale","Seaholme","Travancore", 
                                               "Princes Hill","Kooyong" ))) %>% 
  select(-Suburb)

glimpse(hd)

sort(table(hd$SellerG),decreasing = T)

hd=hd %>% 
  mutate(SellerG_Nelson=as.numeric(SellerG=="Nelson"),
         SellerG_Jellis=as.numeric(SellerG=="Jellis"),
         SellerG_hockingstuart=as.numeric(SellerG=="hockingstuart"),
         SellerG_Barry=as.numeric(SellerG=="Barry"),
         SellerG_Marshall=as.numeric(SellerG=="Marshall"),
         SellerG_Buxton=as.numeric(SellerG=="Buxton"),
         SellerG_Buxton=as.numeric(SellerG=="Ray"),
         SellerG_Buxton=as.numeric(SellerG=="Biggin"),
         SellerG_other=as.numeric(SellerG %in% c("Brad","Woodards","Fletchers","RT",
                                                 "Greg","Miles","Sweeney","Jas","Gary","McGrath",
                                                 "Noel","Hodges","Kay","Stockdale","Williams","Village",
                                                 "Douglas","Love","Raine","Rendina","Chisholm","Cayzer",
                                                 "Collins","Harcourts","LITTLE","Burnham","Dingle","Nick",
                                                 "Moonee","Eview","Peter","Thomson","RW","Alexkarbon",
                                                 "Bells","McDonald","YPA","Considine","C21","Edward","Wilson",
                                                 "Frank","Barlow","GL","Walshe","Beller","Harrington","Paul",
                                                 "Caine","Haughton","Lindellas","Philip","Purplebricks","Abercromby's",
                                                 "ROdney","Melbourne","Whiting","Brace","Buckingham","Castran","Gunn&Co",
                                                 "Chambers","MICM","Tim","Darren","Hunter","J","Pride","Trimson","Cristopher",
                                                 "Morleys","HAR","O'Donoghues","Re","W.B.","William","Bekdon","Pagan","Domain",
                                                 "Holland","Parkes","Prof.","Sotheby's","Anderson","ASL","Bayside","Compton",
                                                 "D'Aprano","First","FN","Garvey","Kelly","Leased","Matthew","Morrison",
                                                 "New","Nicholson","O'Brien","Owen","Raine&Horne","Thomas","Allens","Ascend",
                                                 "Assisi","Australian","Buxton/Advantage","Changing","Charlton","David",
                                                 "Dixon","Galldon","Grantham","Hamilton","Jason","JMRE","Ken","Maddison",
                                                 "Nguyen","RE","Red","ROss","Scott","Walsh","Weda","Wood","Airport","Allan",
                                                 "Appleby","Besser","Blue","Buxton/Find","Calder","CASTRAN","Century","Coventry",
                                                 "Crane","Del","Direct","Elite","Fletchers/One","Geoff","Ham","hockingstuart/Advantage",
                                                 "hockingstuart/Buxton","hockingstuart/Village","Homes","Hooper","Iconek",
                                                 "iOne","iTRAK","Johnston","Joseph","Karen","LJ","Lucas","Luxe","Luxton",
                                                 "Meadows","Naison","Nardella","Oak","One","Parkinson","Professionals","Propertyau",
                                                 "Prowse","R&H","Redina","S&L","Vic","VICPROP","Weast","Win","Zahn"))) %>% 
  select(-SellerG)

glimpse(hd)         

hd=hd %>% 
  select(-Address)

glimpse(hd)

apply(hd,2,function(x) sum(is.na(x)))

glimpse(hd)

set.seed(58)
s=sample(1:nrow(hd),0.8*nrow(hd))
hd_train2=hd[s,]
hd_train=hd[-s,]

hd_test=read.csv("housing_test.csv",stringsAsFactors = F)

hd_test=hd_test %>% 
  mutate(Rooms=as.numeric(Rooms),
         Distance=as.numeric(Distance))

hd_test=hd_test %>% 
  mutate(Bedroom=ifelse(is.na(Bedroom2),
                        3,
                        Bedroom2),
         Bathroom=ifelse(is.na(Bathroom),
                         2,
                         Bathroom),
         Car=ifelse(is.na(Car),
                    1,
                    Car),
         Landsize=ifelse(is.na(Landsize),
                         0,
                         Landsize)) %>% 
  select(-Bedroom2)

glimpse(hd_test)

hd_test=hd_test %>% 
  mutate(buildingArea=ifelse(is.na(BuildingArea),
                             round(mean(BuildingArea,na.rm=T)),
                             BuildingArea)) %>% 
  select(-BuildingArea)

glimpse(hd_test)

hd_test=hd_test %>% 
  mutate(Type_h=as.numeric(Type=="h"),
         Type_u=as.numeric(Type=="u")) %>% 
  select(-Type)

glimpse(hd_test)

table(hd_test$Method)

hd_test=hd_test %>% 
  mutate(Method_PI=as.numeric(Method=="PI"),
         Method_S=as.numeric(Method=="S"),
         Method_SP=as.numeric(Method=="SP"),
         Method_VB=as.numeric(Method=="VB")) %>% 
  select(-Method)

glimpse(hd_test)

table(hd_test$YearBuilt)

sort(table(hd_test$YearBuilt),decreasing=T)

hd_test=hd_test %>% 
  mutate(YearBuilt=ifelse(is.na(YearBuilt),
                          0,
                          YearBuilt))

hd_test=hd_test%>% 
  mutate(YearBuilt_1970=as.numeric(YearBuilt=="1970"),
         YearBuilt_1960=as.numeric(YearBuilt=="1960"),
         YearBuilt_1950=as.numeric(YearBuilt=="1950"),
         YearBuilt_1900=as.numeric(YearBuilt=="1900"),
         YearBuilt_1930=as.numeric(YearBuilt=="1930"),
         YearBuilt_1920=as.numeric(YearBuilt=="1920"),
         YearBuilt_2000=as.numeric(YearBuilt=="2000"),
         YearBuilt_1910=as.numeric(YearBuilt=="1910"),
         YearBuilt_1890=as.numeric(YearBuilt=="1890"),
         YearBuilt_1940=as.numeric(YearBuilt=="1940"),
         YearBuilt_missing=as.numeric(YearBuilt=="0"),
         YearBuilt_Other=as.numeric(YearBuilt %in% c ("1980", "2012", "1975", "2005", "1965", "2010",
                                                      "2013", "1990", "1995", "2009", "2007", "2011",
                                                      "1925", "1955", "2014", "2008", "1935", "2006",
                                                      "2002", "1880","2004", "1915","1998", "2001", 
                                                      "1997", "2003", "2015", "1985", "1999", "2016",
                                                      "1968", "1993", "1996", "1945","1994","1905",
                                                      "1966", "1938" , "1948", "1978", "1992","1928",
                                                      "1958", "1972", "1973", "1974", "1967", "1969",
                                                      "1976", "1926", "1971", "1977", "1988", "1991",
                                                      "1850", "1885", "1888", "1923", "1927", "1934",
                                                      "1939", "1941", "1943", "1953", "1963", "1979",
                                                      "1986", "1987", "2017", "1912", "1946", "1947",
                                                      "1982", "1886", "1887", "1895", "1902", "1904",
                                                      "1906", "1908", "1919","1924", "1937", "1949", 
                                                      "1951", "1957", "1962", "1964", "1989", "1830",
                                                      "1856", "1860", "1870", "1875","1877", "1884", 
                                                      "1889", "1893", "1894", "1897", "1901", "1903",
                                                      "1907", "1913", "1914", "1916", "1917", "1918", 
                                                      "1922", "1929", "1932", "1933", "1942", "1952", 
                                                      "1954", "1956", "1959", "1981","1983", "1984", "2018"))) %>% 
  select(-YearBuilt)

glimpse(hd_test)

sort(table(hd_test$Postcode),decreasing=T)

hd_test=hd_test %>% 
  mutate(Postcode_3073=as.numeric(Postcode=="3073"),
         Postcode_3020=as.numeric(Postcode=="3020"),
         Postcode_3165=as.numeric(Postcode=="3165"),
         Postcode_3046=as.numeric(Postcode=="3046"),
         Postcode_3121=as.numeric(Postcode=="3121"),
         Postcode_3032=as.numeric(Postcode=="3032"),
         Postcode_3040=as.numeric(Postcode=="3040"),
         Postcode_3058=as.numeric(Postcode=="3058"),
         Postcode_3163=as.numeric(Postcode=="3163"),
         Postcode_other=as.numeric(Postcode %in% c ("3204", "3072" ,"3182", "3141" ,"3012", "3056" ,"3084",
                                                    "3011", "3181", "3186" ,"3146", "3015", "3122", "3070", "3188", "3042", "3101", "3187",
                                                    "3104", "3081", "3127", "3031", "3145" ,"3044", "3207", "3124", "3071", "3147", "3039",
                                                    "3068", "3108", "3079", "3103", "3013", "3142", "3033", "3184", "3016" ,"3123" ,"3107",
                                                    "3041", "3055" ,"3000", "3143" ,"3125" ,"3166", "3025", "3078" ,"3105", "3034" ,"3206",
                                                    "3060", "3018", "3189", "3205", "3057", "3051", "3144", "3167", "3185", "3066", "3067",
                                                    "3128", "3162", "3148" ,"3126", "3053" ,"3102", "3019" ,"3065", "3006", "3054", "3052",
                                                    "3087" ,"3161", "3002", "3003", "3085", "3043", "3183", "3021" ,"3047", "3083", "3061","3008"))) %>% 
  select(-Postcode)

glimpse(hd_test)

sort(table(hd_test$CouncilArea),decreasing = T)

hd_test=hd_test %>% 
  mutate(CouncilArea=ifelse(CouncilArea=="",0,CouncilArea))

sort(table(hd_test$CouncilArea),decreasing = T)

hd_test=hd_test %>% 
  mutate(CouncilArea_Boroondara=as.numeric(CouncilArea=="Boroondara"),
         CouncilArea_Moreland=as.numeric(CouncilArea=="Moreland"),
         CouncilArea_MooneeValley=as.numeric(CouncilArea=="Moonee Valley"),
         CouncilArea_Darebin=as.numeric(CouncilArea=="Darebin"),
         CouncilArea_GlenEira=as.numeric(CouncilArea=="Glen Eira"),
         CouncilArea_Stonnington=as.numeric(CouncilArea=="Stonnington"),
         CouncilArea_Bayside=as.numeric(CouncilArea=="Bayside"),
         CouncilArea_Maribyrnong=as.numeric(CouncilArea=="Maribyrnong"),
         CouncilArea_Yarra=as.numeric(CouncilArea=="Yarra"),
         CouncilArea_PortPhillip=as.numeric(CouncilArea=="Port Phillip"),
         CouncilArea_missing=as.numeric(CouncilArea=="0"),
         CouncilArea_other=as.numeric(CouncilArea %in% "Banyule","Melbourne","Hobsons Bay","Brimbank",
                                      "Manningham","Whitehorse","Monash","Kingston","Hume")) %>% 
  select(-CouncilArea)

glimpse(hd_test)

sort(table(hd_test$Suburb),decreasing = T)

hd_test=hd_test %>% 
  mutate(Suburb_Reservoir=as.numeric(Suburb=="Reservoir"),
         Suburb_BentleighEast=as.numeric(Suburb=="Benleigh East"),
         Suburb_Richmond=as.numeric(Suburb=="Richmond"),
         Suburb_Preston=as.numeric(Suburb=="Preston"),
         Suburb_StKilda=as.numeric(Suburb=="St Kilda"),
         Suburb_StKilda=as.numeric(Suburb=="South Yarra"),
         Suburb_other=as.numeric(Suburb %in% c("Brunswick","Glenroy",
                                               "Essendon","Brighton","Glen Iris","Coburg",
                                               "Hawthorn","Northcote","Kew","Brighton East",
                                               "Balwyn North","Bentleigh","Pascoe Vale","Port Melbourne",
                                               "Malvern East","Camberwell","Carnegie","Footscray",
                                               "Thornbury","Moonee Ponds","Doncaster","Ascot Vale",
                                               "Balwyn","Hampton","Newport","Toorak",
                                               "Yarraville","Keilor East","Prahran","Elwood",
                                               "Sunshine","Hawthorn East","Maribyrnong", "Templestowe Lower", 
                                               "Brunswick West","Surrey Hills","Kensington","Sunshine West",
                                               "Ivanhoe","Melbourne","Armadale","Williamstown", 
                                               "Burwood","Ormond","Sunshine North","Altona North", 
                                               "Bulleen","Avondale Heights","Fitzroy North","Strathmore", 
                                               "Fawkner","Ashburton","West Footscray","Maidstone", 
                                               "Moorabbin","Rosanna","South Melbourne","Airport West", 
                                               "Brunswick East", "Heidelberg Heights","Niddrie","Altona", 
                                               "Heidelberg West","Murrumbeena","Coburg North","North Melbourne", 
                                               "Oakleigh South","Malvern","Collingwood","Hadfield", 
                                               "Windsor","Abbotsford","Ashwood","Box Hill", 
                                               "Albert Park","Seddon","Elsternwick","Chadstone", 
                                               "Clifton Hill","Oak Park","Canterbury","Carlton", 
                                               "Flemington","Kew East","Mont Albert","Fairfield", 
                                               "Heidelberg","Oakleigh","Braybrook","Fitzroy", 
                                               "Hampton East","Viewbank","Caulfield South","Southbank", 
                                               "Carlton North","Hughesdale","Alphington","Kingsville", 
                                               "Parkville","Glen Huntly","Aberfeldie","Caulfield North", 
                                               "Watsonia","Albion","Essendon West","Spotswood", 
                                               "Middle Park","East Melbourne","West Melbourne","Yallambie", 
                                               "Gowanbrae","Balaclava","Eaglemont","Ivanhoe East", 
                                               "South Kingsville","Cremorne","Kealba","Keilor Park", 
                                               "Williamstown North","Essendon North","Jacana","Brooklyn", 
                                               "Kingsbury","Bellfield","Caulfield","East Strathmore Heights", 
                                               "Caulfield","Burnley","Campbellfield","Ripponlea", 
                                               "Docklands","Gardenvale","Seaholme","Travancore", 
                                               "Princes Hill","Kooyong" ))) %>% 
  select(-Suburb)

glimpse(hd_test)

sort(table(hd_test$SellerG),decreasing = T)

hd_test=hd_test %>% 
  mutate(SellerG_Nelson=as.numeric(SellerG=="Nelson"),
         SellerG_Jellis=as.numeric(SellerG=="Jellis"),
         SellerG_hockingstuart=as.numeric(SellerG=="hockingstuart"),
         SellerG_Barry=as.numeric(SellerG=="Barry"),
         SellerG_Marshall=as.numeric(SellerG=="Marshall"),
         SellerG_Buxton=as.numeric(SellerG=="Buxton"),
         SellerG_Buxton=as.numeric(SellerG=="Ray"),
         SellerG_Buxton=as.numeric(SellerG=="Biggin"),
         SellerG_other=as.numeric(SellerG %in% c("Brad","Woodards","Fletchers","RT",
                                                 "Greg","Miles","Sweeney","Jas","Gary","McGrath",
                                                 "Noel","Hodges","Kay","Stockdale","Williams","Village",
                                                 "Douglas","Love","Raine","Rendina","Chisholm","Cayzer",
                                                 "Collins","Harcourts","LITTLE","Burnham","Dingle","Nick",
                                                 "Moonee","Eview","Peter","Thomson","RW","Alexkarbon",
                                                 "Bells","McDonald","YPA","Considine","C21","Edward","Wilson",
                                                 "Frank","Barlow","GL","Walshe","Beller","Harrington","Paul",
                                                 "Caine","Haughton","Lindellas","Philip","Purplebricks","Abercromby's",
                                                 "ROdney","Melbourne","Whiting","Brace","Buckingham","Castran","Gunn&Co",
                                                 "Chambers","MICM","Tim","Darren","Hunter","J","Pride","Trimson","Cristopher",
                                                 "Morleys","HAR","O'Donoghues","Re","W.B.","William","Bekdon","Pagan","Domain",
                                                 "Holland","Parkes","Prof.","Sotheby's","Anderson","ASL","Bayside","Compton",
                                                 "D'Aprano","First","FN","Garvey","Kelly","Leased","Matthew","Morrison",
                                                 "New","Nicholson","O'Brien","Owen","Raine&Horne","Thomas","Allens","Ascend",
                                                 "Assisi","Australian","Buxton/Advantage","Changing","Charlton","David",
                                                 "Dixon","Galldon","Grantham","Hamilton","Jason","JMRE","Ken","Maddison",
                                                 "Nguyen","RE","Red","ROss","Scott","Walsh","Weda","Wood","Airport","Allan",
                                                 "Appleby","Besser","Blue","Buxton/Find","Calder","CASTRAN","Century","Coventry",
                                                 "Crane","Del","Direct","Elite","Fletchers/One","Geoff","Ham","hockingstuart/Advantage",
                                                 "hockingstuart/Buxton","hockingstuart/Village","Homes","Hooper","Iconek",
                                                 "iOne","iTRAK","Johnston","Joseph","Karen","LJ","Lucas","Luxe","Luxton",
                                                 "Meadows","Naison","Nardella","Oak","One","Parkinson","Professionals","Propertyau",
                                                 "Prowse","R&H","Redina","S&L","Vic","VICPROP","Weast","Win","Zahn"))) %>% 
  select(-SellerG)

glimpse(hd_test)         

hd_test=hd_test %>% 
  select(-Address)

glimpse(hd_test)

apply(hd_test,2,function(x) sum(is.na(x)))

glimpse(hd_test)
