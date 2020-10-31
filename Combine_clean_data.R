# Combine and clean ten years of CDC data

install.packages("plyr")

rm(list = ls()) # removes all variables in r
library("plyr")

setwd("/Users/noahferrel/Desktop/Test")
load("data/testdf.Rda")

setwd("/Users/noahferrel/Desktop/Test")
load("DATA/BI_DATA_sample.Rda")
ind = as.integer(rownames(BI_DATA_sample)) 

setwd("/Users/noahferrel/Desktop/Test/DATA/CDC 10 Years of Data")
file_names = list.files()
E = lapply(list.files(),function(x) read.csv(x,header = T))
e = E
w = list(
  lapply(E,function(x) names(x)[grep("DRUGID", names(x))]),
  lapply(E,function(x) names(x)[grep("CONTSUB", names(x))]),
  lapply(1:10,function(x) if(x<=8){c("DIAG1","DIAG2","DIAG3")}else{c("DIAG1","DIAG2","DIAG3","DIAG4","DIAG5")} ),
  lapply(1:10,function(x) if(x<=8){c("RFV1","RFV2","RFV3")}else{c("RFV1","RFV2","RFV3","RFV4","RFV5")} )
)

combine_columns = function(df_L,col_L){
  sub_df_L = mapply(function(x,y) x[,y] ,df_L,col_L ) 
  new_df = do.call(rbind.fill,sub_df_L)
  return(new_df)
}

newDF = do.call(cbind, lapply(w,function(x) combine_columns(E,x) ))

#summary(newDF)
#test = newDF$DRUGID9
#summary(as.factor(test))

whitespace2NA = function(vec){
  myclass = class(vec)
  vec =  as.character(vec)
  vec[trimws(vec)==""] = NA
  func = get( paste0("as.",myclass) )
  vec = func(vec)
  return(vec)
}

newDF = as.data.frame(lapply(newDF,function(x)   whitespace2NA(x) ))

setwd("/Users/noahferrel/Desktop/Test/DATA")
load("new_names.Rda")
names(newDF) = new_names

dash_to_0 = function(v){ # changes all the dashes to 0 not needed for this project
  g = class(v)
  v = as.character(v)
  v = gsub("-", "0", v)
  x = get(paste0("as.",g))
  v= x(v)
  return (v)
}

dash_to_nothing = function(v){#changes all the dashes to ""
  g = class(v) #saves class of input
  v = as.character(v) #coersed to a character
  v = gsub("-", "", v)#changes - to ""
  x = get(paste0("as.",g)) #gets what as.class  is and stores it in x
  v= x(v) #uses the as.class function to change it back into what it was
  return (v)
}

newDF = as.data.frame(lapply(newDF,function(x)  dash_to_nothing(x) )) # goes through newDF and takes the dashes out

setwd("/Users/noahferrel/Desktop/Test/results")
save(newDF,file = "newDF.Rda")


setwd("/Users/noahferrel/Desktop/Test/DATA") 
codes = read.csv("OpioidCodesOnly.csv", header = F) #set to false because there is not a header
op_codes = as.vector(codes)[,1] #makes it into a vector
op_codes = op_codes[!(op_codes %in% c("d04766","a11242","d03826","n09045","n11008"))]#takes out the codes i dont want
op_codes = unique(op_codes)# takes out the repeated codes so there are only unique codes 



setwd("/Users/noahferrel/Desktop/Test/DATA")
load("ICD9_codes.RDA") #loads ICD9
load("RFV_codes.RDA") #loads RFV


bicols = function(myDF, mylist){
  bicol = function(df,myvec){
    binat = sapply(df, function(x) (x %in% myvec)*1 )#if it is in the list it is a 1 if not it is a zero
    return ((rowSums(binat)>0)*1)#gives a true or false then makes it 1 or 0
  }
  biDF = sapply(mylist, function(x) bicol(myDF, x))#goes over the data frame using bicol 
  return(as.data.frame(biDF)) #returns what we want as a df
}

newDF_DIAG = newDF[names(newDF)[grep("DIAG",names(newDF))]] #subsets data with just DIAG

Diabetes_ICD9_bi = as.data.frame(bicols(newDF_DIAG,Diabetes_ICD9)) # give 1 or 0 for Diabetes
Alcohol_ICD9_bi = as.data.frame(bicols(newDF_DIAG,Alcohol_ICD9)) # give 1 or 0 for Alcohol
Pain_ICD9_bi = as.data.frame(bicols(newDF_DIAG,Pain_ICD9)) # give 1 or 0 for Pain
Mental_ICD9_bi = as.data.frame(bicols(newDF_DIAG,Mental_ICD9)) # give 1 or 0 for Mental


newDF_RFV = newDF[names(newDF)[grep("RFV",names(newDF))]] #subsets data with just RFV
codes_RFV_bi = as.data.frame(bicols(newDF_RFV,codes_RFV))# give 1 or 0 for RFV


lOP =  list(OP=op_codes)
DrugId_Df = newDF[names(newDF)[grep("DRUGID",names(newDF))]] #sets df with only DRUGID columns
OP = bicols(DrugId_Df,lOP)# give 1 or 0 for OP


newDF_Contsub = newDF[names(newDF)[grep("CONTSUB",names(newDF))]]# #subsets data with just CONTSUB
contsub = list(s2=2,s3=3,s4=4,s5=5,s6=6)
CONTSUB = as.data.frame(bicols(newDF_Contsub,contsub))# give 1 or 0 for CONTSUB

BI_DATA = cbind(Pain_ICD9_bi, Mental_ICD9_bi, Alcohol_ICD9_bi, Diabetes_ICD9_bi, codes_RFV_bi, OP, CONTSUB) # combined “BI_DATA”

ind = as.integer(rownames(BI_DATA_sample))
BIDATASUBSET = BI_DATA[ind,]
all( (BI_DATA[ind,]) == BI_DATA_sample ) #check to see if my data matches sample data


setwd("/Users/noahferrel/Desktop/Test/Results")
save(BI_DATA,file = "bidata_clean.Rda")

#bicols test

a = df_char 
b = df_fac
c = df_int
d = df_num

test_list = list(c(1),c(2),c(3)) #test list to see if bicols works

A = bicols(a,test_list) #tests bicols over different types of data frames
B = bicols(b,test_list)
C = bicols(c,test_list)
D = bicols(d,test_list)

all(A == B) #check to see if all equal 
all(A == C)
all(A == D)
