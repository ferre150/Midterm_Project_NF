# Combine and clean ten years of CDC data'

#install.packages("plyr")

library("plyr")

load("data/testdf.Rda")
load("data/BI_DATA_sample.Rda")
ind = as.integer(rownames(BI_DATA_sample)) 

### Read in data, clean, and combine

## Read in the data

setwd("data/CDC 10 Years of Data") # change the directory to where the CDC data is
file_names = list.files() # names of all the CDC csv files 
E = lapply(list.files(),function(x) read.csv(x,header = T)) # using lapply to create a list of data frames
# each data frame in the list 'E' corresponds to a CSV file

w = list( # a list that contains 4 elements, each element holds the names of the columns from each csv that we want.
  lapply(E,function(x) names(x)[grep("DRUGID", names(x))]), # grab the column names that contain DRUGID, not all columns have the same amount of DRUGID specified columns
  lapply(E,function(x) names(x)[grep("CONTSUB", names(x))]),  # grab the column names that contain CONTSUB, not all columns have the same amount of CONTSUB specified columns
  lapply(1:10,function(x) if(x<=8){c("DIAG1","DIAG2","DIAG3")}else{c("DIAG1","DIAG2","DIAG3","DIAG4","DIAG5")} ),# the first 8 csv files have 3 DIAG columns and the remainder have 5
  lapply(1:10,function(x) if(x<=8){c("RFV1","RFV2","RFV3")}else{c("RFV1","RFV2","RFV3","RFV4","RFV5")} )# the first 8 csv files have 3 RFV columns and the remainder have 5
)

## Combine the 10 different csv files into a single dataframe

combine_columns = function(df_L,col_L){ # the function grabs the column names from t
  sub_df_L = mapply(function(x,y) x[,y] ,df_L,col_L ) 
  new_df = do.call(rbind.fill,sub_df_L)
  return(new_df)
}

newDF = do.call(cbind, lapply(w,function(x) combine_columns(E,x) )) # combines the data into one dataframe

## Cleaning the data

whitespace2NA = function(vec){
  myclass = class(vec) #saves class of input
  vec =  as.character(vec) #coersed to a character
  vec[trimws(vec)==""] = NA #changes "" to NA
  func = get( paste0("as.",myclass) ) #gets what as.class  is and stores it in x
  vec = func(vec) #uses the as.class function to change it back into what it was
  return(vec)
}

newDF = as.data.frame(lapply(newDF,function(x)   whitespace2NA(x) )) # changes all the rows that had "" to NA

setwd("..")
load("new_names.Rda")
names(newDF) = new_names

dash_to_nothing = function(v){#changes all the dashes to ""
  g = class(v) #saves class of input
  v = as.character(v) #coersed to a character
  v = gsub("-", "", v)#changes - to ""
  x = get(paste0("as.",g)) #gets what as.class  is and stores it in x
  v= x(v) #uses the as.class function to change it back into what it was
  return (v)
}

newDF = as.data.frame(lapply(newDF,function(x)  dash_to_nothing(x) )) # goes through newDF and takes the dashes out

setwd("../results")
save(newDF,file = "newDF.Rda")


## Changing the data to binary

setwd("../data") 
codes = read.csv("OpioidCodesOnly.csv", header = F) #set to false because there is not a header
param_codes = read.csv("Parameter_OP_codes.csv", header = T)
op_codes = as.vector(codes)[,1] #makes it into a vector
op_codes = op_codes[!(op_codes %in% c(param_codes))]#takes out the codes i dont want
op_codes = unique(op_codes)# takes out the repeated codes so there are only unique codes 


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

setwd("../data")
ind = as.integer(rownames(BI_DATA_sample))
BIDATASUBSET = BI_DATA[ind,]
all( (BI_DATA[ind,]) == BI_DATA_sample ) #check to see if my data matches sample data


setwd("../results")
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

##  Logistic Regression

library(MASS)
library(pROC)


for(x in names(BI_DATA)){
  BI_DATA[[x]] = as.factor(BI_DATA[[x]])
}


summary(BI_DATA)


## DIABETES
log_model = glm(Diabetes~., data = BI_DATA, family = binomial)
improve = glm(Diabetes~p.arth+p.back+p.chest+p.chol+p.headache+p.pelv+p.nephro+R_circ+R_endo+s2+s6, data = BI_DATA, family = binomial)
summary(log_model)
summary(improve)

final_model = stepAIC(improve, trace = F)
summary(final_model)

pred_dibep = predict(final_model,type="response")
Diab_curve = roc(BI_DATA$Diabetes,pred_dibep,plot = T,main = "ROC Curve for Diabetes") # Area under the curve: 0.6239

Diab_dist = (Diab_curve$sensitivities)^2 + (Diab_curve$specificities)^2
Diab_ind = which(Diab_dist == min(Diab_dist))
Diab_sen = Diab_curve$sensitivities[Diab_ind]
Diab_spec = Diab_curve$specificities[Diab_ind]
Diab_curve$thresholds[Diab_ind] # The optimal threshold for Diabetes is 0.01360379 to maximize sen and spec
points(Diab_curve$specificities[Diab_ind],Diab_curve$sensitivities[Diab_ind],pch=16,col=3)
text(x = Diab_curve$specificities[Diab_ind], y = Diab_curve$sensitivities[Diab_ind], labels = "The Optimal Threshold: 0.01", pos = 2)
text(0,0.4,labels = "Area under the curve: 0.6239")


## ALCOHOLISM
ALC_model = glm(Alcohol~., data = BI_DATA, family = binomial)
ALC_improve = glm(Alcohol~p.ab+p.dent+p.headache+p.nonfrac+Diabetes+R_circ+R_nerv+R_skin+R_circ+R_mental+OP+s2+s3+s4+s5+s6,data = BI_DATA, family = binomial)
summary(ALC_model)
summary(ALC_improve)

ALC_final = stepAIC(ALC_improve, trace = F)
summary(ALC_final)

pred_ALC = predict(ALC_final,type="response")
ALC_curve = roc(BI_DATA$Alcohol,pred_ALC,plot = T)

ALC_dist = (ALC_curve$sensitivities)^2 + (ALC_curve$specificities)^2
ALC_ind = which(ALC_dist == min(ALC_dist))
ALC_sen = ALC_curve$sensitivities[ALC_ind]
ALC_spec = ALC_curve$specificities[ALC_ind]
ALC_curve$thresholds[ALC_ind] # The optimal threshold for ALC is 0.03123456 to maximize sen and spec
points(ALC_curve$specificities[ALC_ind],ALC_curve$sensitivities[ALC_ind],pch=16,col=3)
text(x = ALC_curve$specificities[ALC_ind], y = ALC_curve$sensitivities[ALC_ind], labels = "The Optimal Threshold: 0.03", pos = 2)

