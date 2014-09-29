---
title: "Nepal Trust Survey 2014: Preliminary Findings "
author: "Prabin Babu Dhakal"
date: "September 22, 2014"
---
##Synopsis
The Trust survey is conducted all over the world to find citizen's trust on public organizations and professions. The trust data was collected in Nepal, in 2014 with about 2400 respondents. The respondents were selected from 48 different Village Development Committees (VDCs) of different constituencies across the country. The respondents were selected from the voter's list. These data was entered into the computer and saved to different filenames for each constituencies. 

Here, the data is read, merged, cleaned and frequency tables and plos are generated for Exploratory analysis.

```{r echo=FALSE}
#Little bit of housekeeping
#Check and install missing packages that are required
#list.of.packages <- c("ggplot2","foreign","memisc")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

#set working directory
setwd("~/Google Drive/Trust Survey 2014/nepal-trust-2014/")
library(foreign)
library(plyr)
library(ggplot2)
```

##Read all individual SPSS files
All the data files from differnt constituencies are fed to the analysis.
```{r readInitialFile, message=FALSE,warning=FALSE,cache=TRUE}
#list all files in current working directory
fileList<-dir()
#SPSS files have .sav extension. filter files with .sav
fileList<-fileList[grep(".sav",fileList)]
#read first file to make initial data frame
data<-read.spss(fileList[1],to.data.frame=TRUE,reencode='utf-8')
#Keep all but first file already read
fileList<-fileList[2:length(fileList)]
```

```{r readDataFromAllFiles,message=FALSE,warning=FALSE,cache=TRUE}
#read.spss gives warnings, but it can be safely ignored
for(file in fileList) {
    readData<-read.spss(file,to.data.frame=TRUE,reencode='utf-8')
    data<-rbind(data,readData[,1:202])
}
```

##Cleaning Data
###Removind Blank columns and rows
The original data contains 202 columns. Some might have created extra column names unknowingly while entering data. Some might have inserted blank records in SPSS. We will discard all the records that does not contain either respondentID, or Gender or Age. We will also discard all the columns right to column number 202.
```{r removeExtraColumnsAndRows}
dim(data)
data<-data[(!is.na(data$id)) | (!is.na(data$aq1)) | (!is.na(data$aq2)),]
dim(data)
```

###Rename column names of dataset to variable labels
Column names should be renamed to some meaningful names. The labels obtained from variable labels attribute of SPSS file should be cleaned before using as column names of data frame.
```{r changeNamesOfColumns}
#Find variable labels #variableLabels<-attr(mydata,"variable.labels")
variableLabels<-character()
for(i in 1:202){variableLabels[i]<-attr(data,"variable.labels")[[i]]}
#Converts spaces and punctuations to dots(.)
names(data)<-make.names(variableLabels,unique=TRUE)
#Following code removes all punctuation marks too
#names(data)<-gsub("[ [:punct:]]", "" , variableLabels)
```

#Remove other specific  noises in data
```{r}
#removing age less than 18
#data[data[,6]<18 & !is.na(data[,6]),6]<-NA
data[data$Age..Current.<18 & !is.na(data$Age..Current.]$Age..Current.<-NA
```

#Now Frequency tables and graphs for all columns
The actual question to the respondents begin from column index 5 and end at column no 202, so we will analyze frequency for culumn numbers from 5 to 202.
```{r printFrequencyTableAndGraph,cache=TRUE,message=FALSE,warning=FALSE,echo=TRUE}
for(i in 5:202){
    x<-data[,i]
    #remove NA before calculating mean
    x <- x[!is.na(x)]
    #Remove "Don't Know" and NA from x
    x<-x[x!="Don't know" & x!="Don't Know" & !is.na(x)]

    #calculate frequency table
    c<-count(x)
    #find percent and mean of frequency
    y<-cbind(c,"percent"=c[,2]/sum(c[2])*100)#,mean=mean(count$freq)
    #Horizontal and vertical mean
    hm<-mean(y$percent) #mean of percentages
    vm<-mean(as.numeric(x))

    #Change NA to "Not Available"
    #levels(y$x)<-c(levels(y$x),"Not Available")
    #y[is.na(y$x),1]<-"Not Available"
    #Remove NA after calculating percent
    #y<-y[!is.na(y$x),]

    #Plot factors x by percent
    g<-ggplot(y,aes(x=x,y=percent))
    g<-g+geom_bar(stat="identity",color="Red",fill="Maroon")
    g<-g+labs(title=variableLabels[i],x=names(data[,i]),y="Percent")
    g<-g+geom_text(aes(label=paste(round(percent,digits=2),"%",sep="")),size = 3, hjust = 0.5, vjust = -1, position ="stack")
    g<-g+theme_bw()
    g<-g+theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
    g<-g+geom_hline(aes(yintercept=hm))
    #g<-g+geom_text(aes(0,m,label = m, vjust = -1))
    #g<-g+scale_y_continuous(breaks = sort(c(seq(min(y$percent), max(y$percent), length.out=5), m)))
    g<-g+geom_vline(aes(xintercept=vm))
    print(g)
    print(y)
    print(hm)
    print(vm)
}

```



###Now combining Multiple response question involvement in institutions
```{r combineQuestion11}
q11<-data[,15:26]
for(i in 1:length(q11)){q11[,i]<-as.numeric(q11[,i])}
m<-as.matrix(q11)
involve<-col(m)[m==1]
#inovolve[!is.na(involve)]
involve<-involve[(!is.na(involve)) & (involve<12)]
#Now calculate freq and percentages
c<-count(involve)
#find percent and mean of frequency
y<-cbind(c,"percent"=c[,2]/sum(c[2])*100)
y<-as.data.frame(y)
y
```




##Now analyzing and generating histogram for trust characteristics for Individual
```{r trustCharactersticsPeople}
library(ggplot2)
trustPeople<-data.frame(Col=unlist(data[,57:61]))
trustPeople<-as.data.frame(trustPeople[!is.na(trustPeople$Col),])
names(trustPeople)<-c("Col")
#sort(unique(bq21))
d<-table(trustPeople)
    print(names(d))
    g<-ggplot(trustPeople,aes(trustPeople$Col))
    g<-g+geom_bar(color="Red",fill="Salmon")
    g<-g+labs(title="Trust basis for individual",x="Basis",y="Frequency")
    g<-g+theme_bw()
    g<-g+theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
    print(g)
    print(d)
```

##Now analyzing and generating histogram for trust basis for Institution
```{r trustCharactersticsInstitution}
trustPeople<-data.frame(Col=unlist(data[,62:66]))
trustPeople<-as.data.frame(trustPeople[!is.na(trustPeople$Col),])
names(trustPeople)<-c("Col")
#sort(unique(bq21))
d<-table(trustPeople)
    print(names(d))
    g<-ggplot(trustPeople,aes(trustPeople$Col))
    g<-g+geom_bar(color="Red",fill="Maroon")
    g<-g+labs(title="Trust basis for individual",x="Basis",y="Frequency")
    g<-g+theme_bw()
    g<-g+theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
    g<-g+geom_line(stat = "hline", yintercept = "mean")
    print(g)
    #percent<-as.data.frame(prop.table(d*100)
    print(d)
```


###Gender vs bribe asked
```{r genderVersusBribeAsked}
qplot(data$Corruption..In.recent.years.have.you.or.your.family.asked.by.bureaucrats.about.bribe.,data=data,fill=Gender,xlab="Have You ever been asked for bribe?")
```


##All the data
```{r}
data
```