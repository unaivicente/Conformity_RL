
#Load the data from the different files

#################################

#Individual file reading: Every var will have a 'I' in its name for an easier reading

data_I01 = read.csv('1I11-1I12.csv', sep=';', header=T)
data_I02 = read.csv('1I21-1I22.csv', sep=';', header=T)
data_I03 = read.csv('1I31-1I32.csv', sep=';', header=T)
data_I04 = read.csv('1I41-1I42.csv', sep=';', header=T)
data_I05 = read.csv('1I51-1I52.csv', sep=';', header=T)
data_I06 = read.csv('1I61-1I62.csv', sep=';', header=T)
data_I07 = read.csv('1I71-1I72.csv', sep=';', header=T)
data_I08 = read.csv('1I81-1I82.csv', sep=';', header=T)
data_I09 = read.csv('1I91-1I92.csv', sep=';', header=T)
data_I10 = read.csv('1I101-1I102.csv', sep=';', header=T)
data_I11 = read.csv('1I111-1I112.csv', sep=';', header=T)
data_I12 = read.csv('1I121-1I122.csv', sep=';', header=T)
data_I13 = read.csv('1I131-1I132.csv', sep=';', header=T)
data_I14 = read.csv('1I141-1I142.csv', sep=';', header=T)
data_I15 = read.csv('1I151-1I152.csv', sep=';', header=T)
data_I16 = read.csv('1I161-1I162.csv', sep=';', header=T)
data_I17 = read.csv('1I171-1I172.csv', sep=';', header=T)
data_I18 = read.csv('1I181-1I182.csv', sep=';', header=T)
data_I19 = read.csv('1I241-1I242.csv', sep=';', header=T)
data_I20 = read.csv('1I251-1I252.csv', sep=';', header=T)



#Adding a new column with an IDCode

data_I01$ID <- "I01"
data_I02$ID <- "I02"
data_I03$ID <- "I03"
data_I04$ID <- "I04"
data_I05$ID <- "I05"
data_I06$ID <- "I06"
data_I07$ID <- "I07"
data_I08$ID <- "I08"
data_I09$ID <- "I09"
data_I10$ID <- "I10"
data_I11$ID <- "I11"
data_I12$ID <- "I12"
data_I13$ID <- "I13"
data_I14$ID <- "I14"
data_I15$ID <- "I15"
data_I16$ID <- "I16"
data_I17$ID <- "I17"
data_I18$ID <- "I18"
data_I19$ID <- "I19"
data_I20$ID <- "I20"


#Merging data

total_I <- rbind(data_I01,data_I02,data_I03,data_I04,data_I05,data_I06,data_I07,data_I08,data_I09,data_I10,data_I11,data_I12,data_I13,data_I14,data_I15,data_I16,data_I17,data_I18,data_I19,data_I20)
total_I$Group <- as.factor(rep('I'))

#################################

#Cooperative file reading: Every var will have a 'C' in its name for an easier reading

data_C01 = read.csv('1C21-1C22.csv', sep=';', header=T)
data_C02 = read.csv('1C31-1C32.csv', sep=';', header=T)
data_C03 = read.csv('1C41-1C42.csv', sep=';', header=T)
data_C04 = read.csv('1C61-1C62.csv', sep=';', header=T)
data_C05 = read.csv('1C81-1C82.csv', sep=';', header=T)
data_C06 = read.csv('1C91-1C92.csv', sep=';', header=T)
data_C07 = read.csv('1C101-1C102.csv', sep=';', header=T)
data_C08 = read.csv('1C111-1C112.csv', sep=';', header=T)
data_C09 = read.csv('1C121-1C122.csv', sep=';', header=T)
data_C10 = read.csv('1C131-1C132.csv', sep=';', header=T)
data_C11 = read.csv('1C151-1C152.csv', sep=';', header=T)
data_C12 = read.csv('1C161-1C162.csv', sep=';', header=T)
data_C13 = read.csv('1C171-1C172.csv', sep=';', header=T)
data_C14 = read.csv('1C191-1C192.csv', sep=';', header=T)
data_C15 = read.csv('1C211-1C212.csv', sep=';', header=T)
data_C16 = read.csv('1C221-1C222.csv', sep=';', header=T)
data_C17 = read.csv('1C231-1C232.csv', sep=';', header=T)
data_C18 = read.csv('1C241-1C242.csv', sep=';', header=T)
data_C19 = read.csv('1C251-1C252.csv', sep=';', header=T)
data_C20 = read.csv('1C261-1C262.csv', sep=';', header=T)


data_C01$ID <- "C01"
data_C02$ID <- "C02"
data_C03$ID <- "C03"
data_C04$ID <- "C04"
data_C05$ID <- "C05"
data_C06$ID <- "C06"
data_C07$ID <- "C07"
data_C08$ID <- "C08"
data_C09$ID <- "C09"
data_C10$ID <- "C10"
data_C11$ID <- "C11"
data_C12$ID <- "C12"
data_C13$ID <- "C13"
data_C14$ID <- "C14"
data_C15$ID <- "C15"
data_C16$ID <- "C16"
data_C17$ID <- "C17"
data_C18$ID <- "C18"
data_C19$ID <- "C19"
data_C20$ID <- "C20"

#Merging data

total_C <- rbind(data_C01,data_C02,data_C03,data_C04,data_C05,data_C06,data_C07,data_C08,data_C09,data_C10,data_C11,data_C12,data_C13,data_C14,data_C15,data_C16,data_C17,data_C18,data_C19,data_C20)
total_C$Group <- as.factor(rep('C'))

totaltask <- rbind(total_I,total_C)
totaltask$User.D <- abs(totaltask$User.1-totaltask$User.2)





#Get the difference for intra-user coefficient for User 1

totaltask$Intra_U1 <- vector(length = dim(totaltask)[1])
for(i in seq(1,dim(totaltask)[1],by=3)){
  totaltask$Intra_U1[c(i,i+1,i+2)] <- rep(totaltask$User.1[i]-totaltask$User.1[i+2],each = 3)
}

#Get the difference for intra-user coefficient for User 2

totaltask$Intra_U2 <- vector(length = dim(totaltask)[1])
for(i in seq(1,dim(totaltask)[1],by=3)){
  totaltask$Intra_U2[c(i,i+1,i+2)] <- rep(totaltask$User.2[i]-totaltask$User.2[i+2],each = 3)
}

#BOTH

totaltask$Inter_div <- vector(length = dim(totaltask)[1])
for(i in seq(1,dim(totaltask)[1])){
  totaltask$Inter_div[i] <- totaltask$User.1[i]-totaltask$User.2[i]
}


totaltask$Inter_div_abs <- as.integer(lapply(totaltask$Inter_div,abs))


#Convert to a dichotomical variable (with equal when equal)

totaltask$Lead <- vector(length = dim(totaltask)[1])
for (i in seq(1,dim(totaltask)[1])) {
  totaltask$Lead[i] <- 
    if (totaltask$Intra_U1[i]>totaltask$Intra_U2[i]){
      print("2")}
  else if (totaltask$Intra_U1[i]<totaltask$Intra_U2[i]){
    print("1")}
  else {
    print("0")
  }
}

#Now print Lead
library(plyr)

totaltask$Lead <- factor(totaltask$Lead)


totaltask$win <-  vector(length = nrow(totaltask))

for(i in seq(1,nrow(totaltask),300)){
  ct <- (as.data.frame(table(totaltask$Lead[c(i:(i+299))])))
  totaltask$win[c(i:(i+299))] <- 
    if (ct$Freq[2]>ct$Freq[3]){
      rep(1)}
  else {
    rep(2)}
}


#Get the discrepancy value among subjects

#for User 1

totaltask$Intra_U1dif <- vector(length = dim(totaltask)[1])
for(i in seq(1,dim(totaltask)[1],by=3)){
  totaltask$Intra_U1dif[c(i,i+1,i+2)] <- c(0,(totaltask$User.1[i]-totaltask$User.1[i+1]),(totaltask$User.1[i+1]-totaltask$User.1[i+2]))
}

#for User 2

totaltask$Intra_U2dif <- vector(length = dim(totaltask)[1])
for(i in seq(1,dim(totaltask)[1],by=3)){
  totaltask$Intra_U2dif[c(i,i+1,i+2)] <- c(0,(totaltask$User.2[i]-totaltask$User.2[i+1]),(totaltask$User.2[i+1]-totaltask$User.2[i+2]))
}

#for both

totaltask$Inter_div <- vector(length = dim(totaltask)[1])
for(i in seq(1,dim(totaltask)[1])){
  totaltask$Inter_div[i] <- totaltask$User.1[i]-totaltask$User.2[i]
}


totaltask$Inter_div_abs <- as.integer(lapply(totaltask$Inter_div,abs))


#Separate data in 2 dataframes

taskdfU1 <- totaltask[,!(names(totaltask) %in% c("User.2","Intra_U2","Intra_U2dif"))]
taskdfU2 <- totaltask[,!(names(totaltask) %in% c("User.1","Intra_U1","Intra_U1dif"))]

#Print subject identificator in the dyad per df

taskdfU1$Suj <- vector(length = dim(taskdfU1)[1])
for(i in seq(1,dim(taskdfU1)[1])){
  taskdfU1$Suj[i] <- print(1)
}


taskdfU2$Suj <- vector(length = dim(taskdfU2)[1])
for(i in seq(1,dim(taskdfU2)[1])){
  taskdfU2$Suj[i] <- print(2)
}

taskdfU1$winner <- vector(length = dim(taskdfU1)[1])
for(i in seq(1,dim(taskdfU1)[1])){
  if (taskdfU1$win[i] == 1){
    taskdfU1$winner[i] <- print("Winner")}
  else {
    taskdfU1$winner[i] <- print("Loser")}
}


taskdfU2$winner <- vector(length = dim(taskdfU2)[1])
for(i in seq(1,dim(taskdfU2)[1])){
  if (taskdfU2$win[i] == 2){
    taskdfU2$winner[i] <- print("Winner")}
  else {
    taskdfU2$winner[i] <- print("Loser")}
}


#Rename columns to same name to merge

names(taskdfU1)[names(taskdfU1) == "User.1"] <- "User"
names(taskdfU1)[names(taskdfU1) == "Intra_U1"] <- "Intra"
names(taskdfU1)[names(taskdfU1) == "Intra_U1dif"] <- "Intra_dif"

names(taskdfU2)[names(taskdfU2) == "User.2"] <- "User"
names(taskdfU2)[names(taskdfU2) == "Intra_U2"] <- "Intra"
names(taskdfU2)[names(taskdfU2) == "Intra_U2dif"] <- "Intra_dif"


#Write a csv

totaltask <- merge(x=taskdfU1,y=taskdfU2,all=TRUE)
totaltask <- with(totaltask,totaltask[order(ID, Suj, TrialRep),])
names(totaltask)[names(totaltask) == "Suj"] <- "DyadSuj"
totaltask$AbsSuj <- rep(1:80, each=480)

write.csv(totaltask, "totaldata.csv")
