library(compare);library(gdata); library(psych); library(corrplot); 
library(dplyr); library(multiplex); library(moments); library(qgraph); library(psych)

## load data ##
data<-readRDS('ABCD_5.1.rds')

# change characters to numeric
indx<-sapply(data, is.character)
data[indx]<-lapply(data[indx], function(x) as.numeric(x))

# read variables names
internalizing.variables<-read.csv("VariableNames_IntModel5.1.csv", header=FALSE)

# unlist for comparison
internalizing.variables<-unlist(internalizing.variables)

# get colnames of main data
internalizing.colnames<-colnames(data)

# unlist for comparison
internalizing.colnames<-unlist(internalizing.colnames)

# find the index of main data that are selected variables
internalizing.indx<-unique(match(internalizing.variables, internalizing.colnames))

# make data frame of selected variables
#Note: If error says undefined columns, make sure all variable names are actually included in the main data column names
internalizing.data<-data[,internalizing.colnames[internalizing.indx]]
internalizing.data<-as.data.frame(internalizing.data)


# create split halves subsets for esem and cfa 
set.seed(1234)
#check col numbers for (subject, psweight, famid, siten, nppsweight)
which(colnames(data)=="subnum_char_b")
which(colnames(data)=="ps_weight_b")
which(colnames(data)=="rel_family_id_b")
which(colnames(data)=="siten_b")
which(colnames(data)=="np_ps_weight")
# extract variables for factor analysis (subject, psweight, famid, siten, wt_NR_mwacs)
data.fa<-data[,c(50958, 38679, 40034, 50959, 35671)]
internalizing.data.modeling<-cbind(data.fa, internalizing.data)
indx<-sapply(internalizing.data.modeling, is.factor) 
# some variables are saved as factors in the new version (eg. siten) Change them to numeric for NA conversion. Otherwise, <NA> will not be converted to .
internalizing.data.modeling[indx]<-lapply(internalizing.data.modeling[indx], function(x) as.numeric(x))
# esem data: 5,934, cfa data: 5,934
esem_data<-sample_n(internalizing.data.modeling, 5934)
cfa_data<-internalizing.data.modeling[-c(esem_data$subnum_char_b),]

## save split half .dat files for Mplus running ##
# replace NA with .
esem_data[is.na(esem_data)] <- "." 
cfa_data[is.na(cfa_data)]<-"."
## 1. save data to run with wt_NR_mwacs (decided over wt_NR_cmwacs)
# exclude subjects with missing wt_NR_mwacs
Missing_weight_esem<-which(esem_data$np_ps_weight==".") # missing N= 32
Missing_weight_cfa<-which(cfa_data$np_ps_weight==".") # missing N = 32
esem_data_weight<-esem_data[-c(Missing_weight_esem),] # N = 5902
cfa_data_weight<-cfa_data[-c(Missing_weight_cfa),] # N = 5902
# write dat
write.dat(esem_data_weight, "esem_data")
write.dat(cfa_data_weight, "cfa_data")
# save variable names (same for esem and cfa data)
write.csv(names(esem_data_weight), "esem_data_names.csv")

#two dat files and a csv listing variables names created from steps above will be used in mplus modeling
#the dat files include all internalizing items, so in mplus scripts, adjust use vars section of script accordingly after item exclusions


##Exclusions##
##NOTE: Change sample size if applicable##

## Exclusion criteria 1: Endorsement > 99.5% or < .5%
# make count table and proportion table for Ordinal Variables (CBCL)
count_table<-as.matrix(list(data=NA))
prop_table<-as.matrix(list(data=NA))

for(i in 1:ncol(internalizing.data)){
  count_table[[i]]<-table(internalizing.data[i])
  prop_table[[i]]<-count_table[[i]]/11868*100 
}

count_table<-setNames(count_table, colnames(internalizing.data))
prop_table<-setNames(prop_table, colnames(internalizing.data))

# get the list of variables with endorsement greater than 99.5%
for(i in 1:ncol(internalizing.data)){
  if(any(prop_table[[i]]>99.5)){
    print(names(prop_table[i]))
  }
}

# get the list of variables with endorsement less than .5%
for(i in 1:ncol(internalizing.data)){
  if(any(prop_table[[i]]<.5)){
    print(names(prop_table[i]))
  }
}

#If values are printed, it suggests almost 99.5% of responses for that item were 0 or there are responses with less than .5% endorsement
#Spot check in the prop_table and count_table to ensure 

##ITEMS PRINTED IN STEP ABOVE: 
#cbcl_q18_p_b cbcl_q30_p_b cbcl_q51_p_b cbcl_q54_p_b cbcl_q56g_p_b cbcl_q91_p_b cbcl_q111_p_b 
#ksads_2_12_p_b, ksads_23_149_p_b, ksads_7_24_p_b
#for all items above, need to either exclude or combine responses, do this in the steps below

## for dichotomous variables: exclude because cannot be combined ##
which(colnames(internalizing.data)=="ksads_2_12_p_b")
which(colnames(internalizing.data)=="ksads_23_149_p_b")
which(colnames(internalizing.data)=="ksads_7_24_p_b")
internalizing.data<-internalizing.data[,c(1:50, 52:58, 60, 62:65)]

## for ordinal variables: combine responses with low endorsement ##
# 1. cbcl_q18_p_b
table(internalizing.data$cbcl_q18_p_b)/11868*100
internalizing.data$cbcl_q18_p_b[internalizing.data$cbcl_q18_p_b==2]<-1 # combine 1 and 2
table(internalizing.data$cbcl_q18_p_b)/11868*100
# 2. cbcl_q30_p_b
table(internalizing.data$cbcl_q30_p_b)/11868*100
internalizing.data$cbcl_q30_p_b[internalizing.data$cbcl_q30_p_b==2]<-1 # combine 1 and 2
table(internalizing.data$cbcl_q30_p_b)/11868*100
# 3. cbcl_q51_p_b
table(internalizing.data$cbcl_q51_p_b)/11868*100
internalizing.data$cbcl_q51_p_b[internalizing.data$cbcl_q51_p_b==2]<-1 # combine 1 and 2
table(internalizing.data$cbcl_q51_p_b)/11868*100
# 4. cbcl_q54_p_b
table(internalizing.data$cbcl_q54_p_b)/11868*100
internalizing.data$cbcl_q54_p_b[internalizing.data$cbcl_q54_p_b==2]<-1 # combine 1 and 2
table(internalizing.data$cbcl_q54_p_b)/11868*100
# 5. cbcl_q56g_p_b
table(internalizing.data$cbcl_q56g_p_b)/11868*100
internalizing.data$cbcl_q56g_p_b[internalizing.data$cbcl_q56g_p_b==2]<-1 # combine 1 and 2
table(internalizing.data$cbcl_q56g_p_b)/11868*100
# 6. cbcl_q91_p_b
table(internalizing.data$cbcl_q91_p_b)/11868*100
internalizing.data$cbcl_q91_p_b[internalizing.data$cbcl_q91_p_b==2]<-1 # combine 1 and 2
table(internalizing.data$cbcl_q91_p_b)/11868*100
# 7. cbcl_q111_p_b
table(internalizing.data$cbcl_q111_p_b)/11868*100
internalizing.data$cbcl_q111_p_b[internalizing.data$cbcl_q111_p_b==2]<-1 # combine 1 and 2
table(internalizing.data$cbcl_q111_p_b)/11868*100

## CHECK ENDORSEMENT AGAIN TO MAKE SURE RESOLVED ##
## exclusion criteria 1: Endorsement < .5%
# make count table and proportion table
count_table<-as.matrix(list(data=NA))
prop_table<-as.matrix(list(data=NA))

for(i in 1:ncol(internalizing.data)){
  count_table[[i]]<-table(internalizing.data[i])
  prop_table[[i]]<-count_table[[i]]/11876*100
}

count_table<-setNames(count_table, colnames(internalizing.data))
prop_table<-setNames(prop_table, colnames(internalizing.data))

# get the list of variables with endorsement less than .5% 
for(i in 1:ncol(internalizing.data)){
  if(any(prop_table[[i]]<.5)){
    print(names(prop_table[i]))
  }
}

# get the list of variables with endorsement greater than 99.5% 
for(i in 1:ncol(internalizing.data)){
  if(any(prop_table[[i]]>99.5)){
    print(names(prop_table[i]))
  }
}

#DATA CHECKS
## check mean, sd, min, max for all variables. check for any weird values ##
mean.data<-sapply(internalizing.data, mean, na.rm=TRUE)
sd.data<-sapply(internalizing.data, sd, na.rm=TRUE)
min.data<-sapply(internalizing.data, min, na.rm=TRUE)
max.data<-sapply(internalizing.data, max, na.rm=TRUE)
values.data<-cbind(mean.data, sd.data, min.data, max.data)
write.csv(values.data, "Internalizing_Values.csv")

## exclusion criteria: check for correlation > .9 or < -.9 
##if none printed, none meet that threshold 

xcor <- cor_auto(internalizing.data)
names<-colnames(xcor)
sublist<-list()
item1.name<-list()
item2.name<-list()
coeff<-list()
correlation_table<-as.matrix(list(data=NA))
for(i in 1:ncol(xcor)){
  for (j in 1:ncol(xcor)){
    if(i!=j){
      if(xcor[i,j] > .9){ # switch to < -.9
        item1.name<-names[i]
        item2.name<-names[j]
        coeff<-xcor[i,j]
        print(item1.name)
        print(item2.name)
        print(coeff)
      }
    }
  }
}


xcor <- cor_auto(internalizing.data)
names<-colnames(xcor)
sublist<-list()
item1.name<-list()
item2.name<-list()
coeff<-list()
correlation_table<-as.matrix(list(data=NA))
for(i in 1:ncol(xcor)){
  for (j in 1:ncol(xcor)){
    if(i!=j){
      if(xcor[i,j] < -.9){ 
        item1.name<-names[i]
        item2.name<-names[j]
        coeff<-xcor[i,j]
        print(item1.name)
        print(item2.name)
        print(coeff)
      }
    }
  }
}

## correlation pairs higher than .9 ##
### positive (> .9)
# 1. "ksads_10_45_p_b" (Excessive worries more days than not) "ksads_10_47_p_b" (Worrying has lasted at least 6 months): 0.999
# 2. "ksads_14_80_p_b" (Easily distracted) "ksads_14_76_p_b" (Difficulty sustaining attention): 0.918

## retain more representative variables and remove redundant variables ##
internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_14_80_p_b"))]
internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_10_47_p_b"))]


## exclusion criteria: contingency table with 0

#check to see if zeros in contingency table of all items
intdata.matrix <- data.matrix(internalizing.data)
polychoric(intdata.matrix)$rho
#if the line above yields warning, means there are zeros 

#code below will print column numbers of item pairs which have zeros
res <- matrix(NA,1,2)
colnames(res) <- c("column1","column2")
for (i in 1:ncol(internalizing.data)) {
  for (j in 1:ncol(internalizing.data)) {
    if (i != j) {
      min_frequency <- min(table(data.frame(internalizing.data[,i],internalizing.data[,j])))
      if (min_frequency == 0) {
        res <- rbind(res,matrix(c(i,j),1,2))
      }}
  }}
res

#pairs with 0 cells 
#column1 column2
#[2,]      11      32
#[3,]      27      54
#[5,]      32      54
#[6,]      49      50
#[7,]      49      51
#[8,]      49      52
#[15,]      60      59

#check which items they correspond to
colnames(internalizing.data)

#11 & 32: "cbcl_q31_p_b" (Fears they might think or do something bad)  & "cbcl_q77_p_b" (Sleeps more than most kids)
#27 & 54: "cbcl_q65_p_b"(Refuses to talk) & "ksads_23_145_p_B" (Wishes/Better off dead)
#32 & 54: "cbcl_q77_p_b" (Sleeps more than most kids) & "ksads_23_145_p_b" (Wishes/Better off dead)
#49 & 50: "ksads_21_134_p_b" (History of traumatic event) & "ksads_21_135_p_b" (Efforts to avoids thoughts of trauma)
#49 & 51: "ksads_21_134_p_b" (History of traumatic event) & "ksads_21_137_p_b" (Nightmares)
#49 & 52: "ksads_21_134_p_b" (History of traumatic event) & "ksads_21_139_p_b" (Distress at internal reminders of trauma)
#59 & 60: "ksads_9_34_p_b" (Marked fear of phobic object) & "ksads_9_37_p_b" (Active avoidance of phobic object)


#check with Toni about which items to exclude, then do so below 
internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_21_135_p_b"))]
internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_21_137_p_b"))]
internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_21_139_p_b"))]
internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_9_37_p_b"))]
internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_23_145_p_b"))]
internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="cbcl_q77_p_b"))]

#RE CHECK ZERO CELLS
#check to see if zeros in contingency table of all items
intdata.matrix <- data.matrix(internalizing.data)
polychoric(intdata.matrix)$rho
#code below will print column numbers of item pairs which have zeros
res <- matrix(NA,1,2)
colnames(res) <- c("column1","column2")
for (i in 1:ncol(internalizing.data)) {
  for (j in 1:ncol(internalizing.data)) {
    if (i != j) {
      min_frequency <- min(table(data.frame(internalizing.data[,i],internalizing.data[,j])))
      if (min_frequency == 0) {
        res <- rbind(res,matrix(c(i,j),1,2))
      }}
  }}
res

#any additional exclusions after running mplus due to warnings/errors
internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_23_147_p_b"))]

#save csv with item names
write.csv(names(internalizing.data), "IncludedItems_names.csv")


## ICLUST to identify items forming doublets ##
##ask Toni if this step is necessary, sometimes skip
xcor_updated<-cor_auto(internalizing.data)
clust<-ICLUST(xcor_updated)
pdf(ICLUST(xcor_updated), height=30, width=35)
#use plots window to export PDF to check ICLUST

#ICLUST results: item pairs forming doublets (>=.85)
#remove one from each doublet pair (usually remove the one that has lower variability in responses)
#then run steps above again with new item set to verify new item pool passes all steps

#in intDSM cbcl & ksads item set the doublets are: 
#ksads_9_37_p & ksads_9_34_p
#cbcl_q56f_p_b & cbcl_q56c_p_b
#ksads_21_135_p & ksads_21_134_p
#ksads_22_141_p & cbcl_q100_p_b
#cbcl_q50_p_b & cbcl_q45_p_b
#cbcl_q35_p_b & cbcl_q33_p_b
#ksads_1_3_p & ksads_1_1_p
#ksads_23_147_p & cbcl_q91_p_b
#ksads_14_76_p & cbcl_q08_p_b
#ksads_21_137_p & cbcl_q47_p_b

#if excluding items from doublets, keep those with higher variability
#internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_9_37_p"))]
#internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="cbcl_q56c_p_b"))]
#internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_21_135_p"))]
#internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_22_141_p"))]
#internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="cbcl_q45_p_b"))]
#internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="cbcl_q35_p_b"))]
#internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_1_1_p"))]
#internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_23_147_p"))]
#internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_14_76_p"))]
#internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_21_137_p"))]


### RECHECK DOUBLETS ###

## ICLUST to identify items forming doublets ##
#xcor_updated<-cor_auto(internalizing.data)
#clust<-ICLUST(xcor_updated)
#pdf(ICLUST(xcor_updated), height=30, width=35)
#use plots window to export PDF to check ICLUST

## addition doublets: 
#cbcl_q10_p_b & cbcl_q08_p_b
#ksads_23_145_p & cbcl_q91_p_b
#ksads_21_139_p & ksads_21_134_p

#remove the items which have lower SD
#internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="cbcl_q10_p_b"))]
#internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_23_145_p"))]
#internalizing.data<-internalizing.data[,-c(which(colnames(internalizing.data)=="ksads_21_139_p"))]


### RECHECK DOUBLETS ###

## ICLUST to identify items forming doublets ##
#xcor_updated<-cor_auto(internalizing.data)
#clust<-ICLUST(xcor_updated)
#pdf(ICLUST(xcor_updated), height=30, width=35)
#use plots window to export PDF to check ICLUST

#additional doublets: none!

## with final set of items, create scree plot to check number of factors ##

#xcor <- polychoric(internalizing.data)$rho # calculate inter-item correlations
#VSS.scree(xcor) #scree plot
#eigen(xcor)$values[1]/eigen(xcor)$values[2] # ratio of 1st/2nd eigenvalues
#eigen(xcor)$values[2]/eigen(xcor)$values[3] # ratio of 1st/2nd eigenvalues
#eigen(xcor)$values[3]/eigen(xcor)$values[4] # ratio of 1st/2nd eigenvalues

##SAVE INT ITEMS DAT FILE WITH IDS##

#check col numbers for (subject)
which(colnames(data)=="subnum_char_b")
# extract variables for factor analysis (subject)
subID<-data[,c(50958)]
internalizing.items<-cbind(subID, internalizing.data)

# replace NA with .
internalizing.items[is.na(internalizing.items)] <- "." 
write.csv(internalizing.items, "IntModel_itemdata.csv", row.names = FALSE)

############## BRAIN ANALYSES PREP #############################


## Make datasets with brain variables for brain analyses ##
library(multiplex)

## LOAD FULL DATA ##
all.data<-readRDS('ABCD_5.1.rds')
all.data.names<-colnames(all.data)
all_id<-all.data$subnum_char_b
  
## VOLUME ##

## Trim full data set to include only covariates, brain structure variables, and excluded_missing
structure.names<-read.csv("structure_names.csv", header=TRUE)
structure.names<-structure.names[,1]
structure.indx<-unique(match(structure.names,all.data.names))
structure.data<-all.data[,structure.indx]
structure.data<-as.data.frame(structure.data)

### because variance is too high for volume, let's reduce by dividing vol by 1000
#which(colnames(GMV.data)=="smri_vol_cdk_banksstslh_b")
#which(colnames(GMV.data)=="smri_vol_cdk_insularh_b")
#which(colnames(GMV.data)=="smri_vol_scs_crbcortexlh_b")
#which(colnames(GMV.data)=="smri_vol_scs_vedcrh_b")
#GMV.data[,42:128]<-GMV.data[,42:128]/1000

# LOAD PREPROCESSED INTERNALIZING DATA AND MERGE WITH TRIMMED BRAIN DATA
internalizing.data<-read.csv('IntModel_itemdata.csv')
IntModel.structure<-merge(internalizing.data, structure.data, by="subnum_char_b")

## for medication variable: dummy code for include (1, took no meds) and exclude (0, has meds) ##
## code response 3 ("Took no medications") as 1 and responses 0,1,2,4 as 0
table(IntModel.structure$brought_medications_b)
IntModel.structure$brought_medications_b[IntModel.structure$brought_medications_b==1]<-0 # make 1 into 0
IntModel.structure$brought_medications_b[IntModel.structure$brought_medications_b==2]<-0 # make 2 into 0
IntModel.structure$brought_medications_b[IntModel.structure$brought_medications_b==4]<-0 # make 4 into 0
IntModel.structure$brought_medications_b[IntModel.structure$brought_medications_b==3]<-1 # make 3 into 1
table(IntModel.structure$brought_medications_b)
names(IntModel.structure)[names(IntModel.structure) == "brought_medications_b"] <- "medications_dummy_b"
table(IntModel.structure$medications_dummy_b)

# replace NAs with .
IntModel.structure[is.na(IntModel.structure)] <- "."

#make exclusions for brain data QC using exclude_missing: include 1s and exclude 0s, excludes N=1050 in 5.1
IntModel.structure.exclude<-which(IntModel.structure$excluded_missing==0)
IntModel.structure<-IntModel.structure[-c(IntModel.structure.exclude),]
#make exclusions for missing qc indicator, excludes N=64 in 5.1
IntModel.structure.noqc<-which(IntModel.structure$excluded_missing==".")
IntModel.structure<-IntModel.structure[-c(IntModel.structure.noqc),]
#make exclusions for missing COIL variables N=41 in 5.1
#IntModel.structure.nocoil<-which(IntModel.structure$COIL2_b==".")
#IntModel.structure<-IntModel.structure[-c(IntModel.structure.nocoil),]

# change characters to numeric
indx<-sapply(IntModel.structure, is.character)
IntModel.structure[indx]<-lapply(IntModel.structure[indx], function(x) as.numeric(x))

#MAKE FACTORS AS NECESSARY AND VERIFY
IntModel.structure$FEMALE_b <- as.factor(IntModel.structure$FEMALE_b)
is.factor(IntModel.structure$FEMALE_b)
IntModel.structure$device_2 <- as.factor(IntModel.structure$device_2)
is.factor(IntModel.structure$device_2)
IntModel.structure$device_3 <- as.factor(IntModel.structure$device_3)
is.factor(IntModel.structure$device_3)
IntModel.structure$device_4 <- as.factor(IntModel.structure$device_4)
is.factor(IntModel.structure$device_4)
IntModel.structure$device_5 <- as.factor(IntModel.structure$device_5)
is.factor(IntModel.structure$device_5)
IntModel.structure$device_6 <- as.factor(IntModel.structure$device_6)
is.factor(IntModel.structure$device_6)
IntModel.structure$device_7 <- as.factor(IntModel.structure$device_7)
is.factor(IntModel.structure$device_7)
IntModel.structure$device_8 <- as.factor(IntModel.structure$device_8)
is.factor(IntModel.structure$device_8)
IntModel.structure$device_9 <- as.factor(IntModel.structure$device_9)
is.factor(IntModel.structure$device_9)
IntModel.structure$device_10 <- as.factor(IntModel.structure$device_10)
is.factor(IntModel.structure$device_10)
IntModel.structure$device_11 <- as.factor(IntModel.structure$device_11)
is.factor(IntModel.structure$device_11)
IntModel.structure$device_12 <- as.factor(IntModel.structure$device_12)
is.factor(IntModel.structure$device_12)
IntModel.structure$device_13 <- as.factor(IntModel.structure$device_13)
is.factor(IntModel.structure$device_13)
IntModel.structure$device_14 <- as.factor(IntModel.structure$device_14)
is.factor(IntModel.structure$device_14)
IntModel.structure$device_15 <- as.factor(IntModel.structure$device_15)
is.factor(IntModel.structure$device_15)
IntModel.structure$device_16 <- as.factor(IntModel.structure$device_16)
is.factor(IntModel.structure$device_16)
IntModel.structure$device_17 <- as.factor(IntModel.structure$device_17)
is.factor(IntModel.structure$device_17)
IntModel.structure$device_18 <- as.factor(IntModel.structure$device_18)
is.factor(IntModel.structure$device_18)
IntModel.structure$device_19 <- as.factor(IntModel.structure$device_19)
is.factor(IntModel.structure$device_19)
IntModel.structure$device_20 <- as.factor(IntModel.structure$device_20)
is.factor(IntModel.structure$device_20)
IntModel.structure$device_21 <- as.factor(IntModel.structure$device_21)
is.factor(IntModel.structure$device_21)
IntModel.structure$device_22 <- as.factor(IntModel.structure$device_22)
is.factor(IntModel.structure$device_22)
IntModel.structure$device_23 <- as.factor(IntModel.structure$device_23)
is.factor(IntModel.structure$device_23)
IntModel.structure$device_24 <- as.factor(IntModel.structure$device_24)
is.factor(IntModel.structure$device_24)
IntModel.structure$device_25 <- as.factor(IntModel.structure$device_25)
is.factor(IntModel.structure$device_25)
IntModel.structure$device_26 <- as.factor(IntModel.structure$device_26)
is.factor(IntModel.structure$device_26)
IntModel.structure$device_27 <- as.factor(IntModel.structure$device_27)
is.factor(IntModel.structure$device_27)
IntModel.structure$device_28 <- as.factor(IntModel.structure$device_28)
is.factor(IntModel.structure$device_28)
IntModel.structure$device_29 <- as.factor(IntModel.structure$device_29)
is.factor(IntModel.structure$device_29)
IntModel.structure$COIL2_b <- as.factor(IntModel.structure$COIL2_b)
is.factor(IntModel.structure$COIL2_b)
IntModel.structure$COIL3_b <- as.factor(IntModel.structure$COIL3_b)
is.factor(IntModel.structure$COIL3_b)
IntModel.structure$COIL4_b <- as.factor(IntModel.structure$COIL4_b)
is.factor(IntModel.structure$COIL4_b)
IntModel.structure$COIL5_b <- as.factor(IntModel.structure$COIL5_b)
is.factor(IntModel.structure$COIL5_b)
IntModel.structure$medications_dummy_b <- as.factor(IntModel.structure$medications_dummy_b)
is.factor(IntModel.structure$medications_dummy_b)

is.numeric(IntModel.structure$cbcl_q05_p_b)
is.numeric(IntModel.structure$ksads_1_1_p_b)
is.numeric(IntModel.structure$interview_age_b)
is.numeric(IntModel.structure$motion_continuous)
is.numeric(IntModel.structure$smri_vol_cdk_banksstslh_b)
is.numeric(IntModel.structure$smri_thick_cdk_banksstslh_b)
is.numeric(IntModel.structure$smri_area_cdk_banksstslh_b)

# replace NAs with .
IntModel.structure[is.na(IntModel.structure)] <- "."

## save .dat for Mplus
write.dat(IntModel.structure, "IntModel.structure")
write.csv(names(IntModel.structure), "IntModel.structure.names.csv")


## make subset of data with only those who do not have medications
#include 1s and exclude 0s
IntModel.structure.excludemeds<-which(IntModel.structure$medications_dummy_b==0)
IntModel.structure.nomeds<-IntModel.structure[-c(IntModel.structure.excludemeds),]
## save no meds subset as .dat for Mplus
write.dat(IntModel.structure.nomeds, "IntModel.structure.nomeds")
write.csv(names(IntModel.structure.nomeds), "IntModel.structure.nomeds.names.csv")


## merge stimulant data and create subsets ##
data<-readRDS('ABCD_5.1.rds')
stim.data<-read.csv("meds.csv")
merge.stim<-merge(data, stim.data, by="src_subject_id")
#get subnum_char into file with stimulant data
which(colnames(merge.stim)=="subnum_char_b")
which(colnames(merge.stim)=="stimulant_b")
merge.stim.sub<-merge.stim[,c(54759, 54850)]
#merge stimulant data with int model and structure data
IntModel.structure.wstim<-merge(merge.stim.sub, IntModel.structure, by="subnum_char_b")
#make subset excluding stimulants
IntModel.structure.excludestim<-which(IntModel.structure.wstim$stimulant_b==1)
IntModel.structure.nostim<-IntModel.structure[-c(IntModel.structure.excludestim),]
# change characters to numeric
indx<-sapply(IntModel.structure.nostim, is.character)
IntModel.structure.nostim[indx]<-lapply(IntModel.structure.nostim[indx], function(x) as.numeric(x))
#MAKE FACTORS AS NECESSARY AND VERIFY
IntModel.structure.nostim$FEMALE_b <- as.factor(IntModel.structure.nostim$FEMALE_b)
is.factor(IntModel.structure.nostim$FEMALE_b)
IntModel.structure.nostim$device_2 <- as.factor(IntModel.structure.nostim$device_2)
is.factor(IntModel.structure.nostim$device_2)
IntModel.structure.nostim$device_3 <- as.factor(IntModel.structure.nostim$device_3)
is.factor(IntModel.structure.nostim$device_3)
IntModel.structure.nostim$device_4 <- as.factor(IntModel.structure.nostim$device_4)
is.factor(IntModel.structure.nostim$device_4)
IntModel.structure.nostim$device_5 <- as.factor(IntModel.structure.nostim$device_5)
is.factor(IntModel.structure.nostim$device_5)
IntModel.structure.nostim$device_6 <- as.factor(IntModel.structure.nostim$device_6)
is.factor(IntModel.structure.nostim$device_6)
IntModel.structure.nostim$device_7 <- as.factor(IntModel.structure.nostim$device_7)
is.factor(IntModel.structure.nostim$device_7)
IntModel.structure.nostim$device_8 <- as.factor(IntModel.structure.nostim$device_8)
is.factor(IntModel.structure.nostim$device_8)
IntModel.structure.nostim$device_9 <- as.factor(IntModel.structure.nostim$device_9)
is.factor(IntModel.structure.nostim$device_9)
IntModel.structure.nostim$device_10 <- as.factor(IntModel.structure.nostim$device_10)
is.factor(IntModel.structure.nostim$device_10)
IntModel.structure.nostim$device_11 <- as.factor(IntModel.structure.nostim$device_11)
is.factor(IntModel.structure.nostim$device_11)
IntModel.structure.nostim$device_12 <- as.factor(IntModel.structure.nostim$device_12)
is.factor(IntModel.structure.nostim$device_12)
IntModel.structure.nostim$device_13 <- as.factor(IntModel.structure.nostim$device_13)
is.factor(IntModel.structure.nostim$device_13)
IntModel.structure.nostim$device_14 <- as.factor(IntModel.structure.nostim$device_14)
is.factor(IntModel.structure.nostim$device_14)
IntModel.structure.nostim$device_15 <- as.factor(IntModel.structure.nostim$device_15)
is.factor(IntModel.structure.nostim$device_15)
IntModel.structure.nostim$device_16 <- as.factor(IntModel.structure.nostim$device_16)
is.factor(IntModel.structure.nostim$device_16)
IntModel.structure.nostim$device_17 <- as.factor(IntModel.structure.nostim$device_17)
is.factor(IntModel.structure.nostim$device_17)
IntModel.structure.nostim$device_18 <- as.factor(IntModel.structure.nostim$device_18)
is.factor(IntModel.structure.nostim$device_18)
IntModel.structure.nostim$device_19 <- as.factor(IntModel.structure.nostim$device_19)
is.factor(IntModel.structure.nostim$device_19)
IntModel.structure.nostim$device_20 <- as.factor(IntModel.structure.nostim$device_20)
is.factor(IntModel.structure.nostim$device_20)
IntModel.structure.nostim$device_21 <- as.factor(IntModel.structure.nostim$device_21)
is.factor(IntModel.structure.nostim$device_21)
IntModel.structure.nostim$device_22 <- as.factor(IntModel.structure.nostim$device_22)
is.factor(IntModel.structure.nostim$device_22)
IntModel.structure.nostim$device_23 <- as.factor(IntModel.structure.nostim$device_23)
is.factor(IntModel.structure.nostim$device_23)
IntModel.structure.nostim$device_24 <- as.factor(IntModel.structure.nostim$device_24)
is.factor(IntModel.structure.nostim$device_24)
IntModel.structure.nostim$device_25 <- as.factor(IntModel.structure.nostim$device_25)
is.factor(IntModel.structure.nostim$device_25)
IntModel.structure.nostim$device_26 <- as.factor(IntModel.structure.nostim$device_26)
is.factor(IntModel.structure.nostim$device_26)
IntModel.structure.nostim$device_27 <- as.factor(IntModel.structure.nostim$device_27)
is.factor(IntModel.structure.nostim$device_27)
IntModel.structure.nostim$device_28 <- as.factor(IntModel.structure.nostim$device_28)
is.factor(IntModel.structure.nostim$device_28)
IntModel.structure.nostim$device_29 <- as.factor(IntModel.structure.nostim$device_29)
is.factor(IntModel.structure.nostim$device_29)
IntModel.structure.nostim$COIL2_b <- as.factor(IntModel.structure.nostim$COIL2_b)
is.factor(IntModel.structure.nostim$COIL2_b)
IntModel.structure.nostim$COIL3_b <- as.factor(IntModel.structure.nostim$COIL3_b)
is.factor(IntModel.structure.nostim$COIL3_b)
IntModel.structure.nostim$COIL4_b <- as.factor(IntModel.structure.nostim$COIL4_b)
is.factor(IntModel.structure.nostim$COIL4_b)
IntModel.structure.nostim$COIL5_b <- as.factor(IntModel.structure.nostim$COIL5_b)
is.factor(IntModel.structure.nostim$COIL5_b)
IntModel.structure.nostim$medications_dummy_b <- as.factor(IntModel.structure.nostim$medications_dummy_b)
is.factor(IntModel.structure.nostim$medications_dummy_b)
is.numeric(IntModel.structure.nostim$cbcl_q05_p_b)
is.numeric(IntModel.structure.nostim$ksads_1_1_p_b)
is.numeric(IntModel.structure.nostim$interview_age_b)
is.numeric(IntModel.structure.nostim$motion_continuous)
is.numeric(IntModel.structure.nostim$smri_vol_cdk_banksstslh_b)
is.numeric(IntModel.structure.nostim$smri_thick_cdk_banksstslh_b)
is.numeric(IntModel.structure.nostim$smri_area_cdk_banksstslh_b)
# replace NAs with .
IntModel.structure.nostim[is.na(IntModel.structure.nostim)] <- "."
## save .dat for Mplus
write.dat(IntModel.structure.nostim, "IntModel.structure.nostim")
write.csv(names(IntModel.structure.nostim), "IntModel.structure.nostim.names.csv")


