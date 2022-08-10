# Clearing work space and garbage collection ------------------------------
rm(list=ls())
gc()
library(lavaan)
library(psych)
library(pwr)
# Importing and cleaning data ---------------------------------------------
setwd("/home/seth/Documents/UCR/Folders/Kellon/Revision Analyses")
# Importing day 1 data from qualtrics with lines 1 & 3 removed
day1data = read.csv("day1data.csv", header=T, na.strings = "", 
                    stringsAsFactors = F, strip.white = T)
# Removing needless columns
colnames(day1data)
day1data=day1data[,-c(3:4,7:21,30:33,46:49,55:58,77:80)]
# Name columns
colnames(day1data)[1:12]=c("StartDate1.1", "EndDate1.1", "Progress1.1", "Duration1.1",
                           "FirstName1.1", "LastName1.1", "Email1.1", "Age1.1", "Sex1.1",
                           "Ethnicity1.1", "Other1.1", "ParentsEducation1.1")
colnames(day1data)[13:24]=c("PositiveAffect1.1", "NegativeAffect1.1", "PositiveAffect2.1",
                            "NegativeAffect2.1","NegativeAffect3.1","NegativeAffect4.1",
                            "PositiveAffect3.1", "NegativeAffect5.1","PositiveAffect4.1",
                            "PositiveAffect5.1","NegativeAffect6.1","PositiveAffect6.1")
colnames(day1data)[25:29] = paste0("LifeSatisfaction", 1:5, ".1")
colnames(day1data)[30:35] = paste0("Connectedness", 1:6, ".1")
colnames(day1data)[36:41] = paste0("Competence", 1:6, ".1")
colnames(day1data)[42:47] = paste0("Autonomy", 1:6, ".1")
colnames(day1data)[48]=c("Condition1.1")
# Setting correct data types
# str(day1data) <--use this to view structure
day1data$StartDate1.1=as.POSIXct(day1data$StartDate1.1, format="%m/%d/%Y %H:%M")
day1data$EndDate1.1=as.POSIXct(day1data$EndDate1.1, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day1data$FullName = paste0(day1data$FirstName1.1, day1data$LastName1.1)
day1data$Duplicate1.1 = as.numeric(duplicated(day1data$FullName) | duplicated(day1data$FullName, fromLast = T))
day1data$MinProgress1.1 = 0
day1data$MinProgress1.1[day1data$Progress1.1 == min(day1data$Progress1.1)] = 1
# plyr::count(day1data$Progress1.1)
day1data$MinProgress1.1=F
day1data$MinProgress1.1[day1data$Progress1.1==min(day1data$Progress1.1)]=T
# Subsetting data
day1data=day1data[day1data$Duplicate1.1==F & day1data$MinProgress1.1==F,]
# Importing day 2 data from qualtrics with lines 1 & 3 removed ------------
day2data = read.csv("day2data.csv", header=T, na.strings = "",
                    stringsAsFactors = F, strip.white = T)
#Removing needless columns
colnames(day2data)
day2data=day2data[,-c(3:4,7:9,13:17, 19:22, 35:38, 44:47, 66:69)]
#Name columns
colnames(day2data)[1:7]=c("StartDate1.2", "EndDate1.2", "Progress1.2", "Duration1.2",
                          "LastName1.2", "FirstName1.2", "Email1.2")
colnames(day2data)[8]=c("Recall1.2")
colnames(day2data)[9:20]=c("PositiveAffect1.2", "NegativeAffect1.2", "PositiveAffect2.2",
                           "NegativeAffect2.2","NegativeAffect3.2","NegativeAffect4.2",
                           "PositiveAffect3.2", "NegativeAffect5.2","PositiveAffect4.2",
                           "PositiveAffect5.2","NegativeAffect6.2","PositiveAffect6.2")
colnames(day2data)[21:25] = paste0("LifeSatisfaction", 1:5, ".2")
colnames(day2data)[26:31] = paste0("Connectedness", 1:6, ".2")
colnames(day2data)[32:37] = paste0("Competence", 1:6, ".2")
colnames(day2data)[38:43] = paste0("Autonomy", 1:6, ".2")
colnames(day2data)[44]=c("Condition1.2")
# Setting correct data types
day2data$StartDate1.2=as.POSIXct(day2data$StartDate1.2, format="%m/%d/%Y %H:%M")
day2data$EndDate1.2=as.POSIXct(day2data$EndDate1.2, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day2data$FullName = paste0(day2data$FirstName1.2, day2data$LastName1.2)
day2data$Duplicate1.2 = as.numeric(duplicated(day2data$FullName) | duplicated(day2data$FullName, fromLast = T))
day2data$MinProgress1.2 = 0
day2data$MinProgress1.2[day2data$Progress1.2 == min(day2data$Progress1.2)] = 1
#Subsetting data
day2data=day2data[day2data$Duplicate1.2==F & day2data$MinProgress1.2==F,]

# Importing day 3 data from qualtrics with lines 1 & 3 removed ------------
day3data = read.csv("day3data.csv", header=T, na.strings = "",
                    stringsAsFactors = F, strip.white = T)
#Removing needless columns
colnames(day3data)
day3data=day3data[,-c(3:4,7:9,13:17, 30:33, 39:42, 61:64, 66:69, 71:74,76)]
#Name columns
colnames(day3data)[1:7]=c("StartDate1.3", "EndDate1.3", "Progress1.3", "Duration1.3",
                          "LastName1.3", "FirstName1.3", "Email1.3")
colnames(day3data)[8:19]=c("PositiveAffect1.3", "NegativeAffect1.3", "PositiveAffect2.3",
                           "NegativeAffect2.3","NegativeAffect3.3","NegativeAffect4.3",
                           "PositiveAffect3.3", "NegativeAffect5.3","PositiveAffect4.3",
                           "PositiveAffect5.3","NegativeAffect6.3","PositiveAffect6.3")
colnames(day3data)[20:24] = paste0("LifeSatisfaction", 1:5, ".3")
colnames(day3data)[25:30] = paste0("Connectedness", 1:6, ".3")
colnames(day3data)[31:36] = paste0("Competence", 1:6, ".3")
colnames(day3data)[37:42] = paste0("Autonomy", 1:6, ".3")
colnames(day3data)[43:44] = c("PerformManipulationCheck1.1", "RecallManipulationCheck1.1")
colnames(day3data)[45]=c("Condition1.3")
# Setting correct data types
day3data$StartDate1.3=as.POSIXct(day3data$StartDate1.3, format="%m/%d/%Y %H:%M")
day3data$EndDate1.3=as.POSIXct(day3data$EndDate1.3, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day3data$FullName = paste0(day3data$FirstName1.3, day3data$LastName1.3)
day3data$Duplicate1.3 = as.numeric(duplicated(day3data$FullName) | duplicated(day3data$FullName, fromLast = T))
day3data$MinProgress1.3 = 0
day3data$MinProgress1.3[day3data$Progress1.3 == min(day3data$Progress1.3)] = 1
#Subsetting data
day3data=day3data[day3data$Duplicate1.3==F & day3data$MinProgress1.3==F,]

# Merging data ------------------------------------------------------------

recdata=merge(day1data,day2data,by="FullName",all=T,sort=F)
recdata=merge(recdata,day3data,by="FullName",all=T,sort=F)
#Remove entries not done on an acceptable date
recdata$Days1.2=round(recdata$EndDate1.2,units="days") - round(recdata$EndDate1.1,units="days")
recdata$Days1.3=round(recdata$EndDate1.3,units="days") - round(recdata$EndDate1.2,units="days")
recdata[(recdata$Days1.2 > 2 | is.na(recdata$Days1.2)==T), grep(".2$",colnames(recdata))]=NA
recdata[(recdata$Days1.3 > 2 | is.na(recdata$Days1.3)==T), grep(".3$",colnames(recdata))]=NA
#removing people not assigned a condition at time 1 (didn't finish day 1)
recdata$Condition=recdata$Condition1.1
recdata=recdata[,-grep("Condition1", colnames(recdata))]
recdata=recdata[!is.na(recdata$Condition),]
#removing people who weren't paying attention
recdata$maxcons = 0
for (i in 1:nrow(recdata)) {
  recdata$maxcons[i] = max(rle(recdata[i,-grep("Date", colnames(recdata))])$lengths)
}
recdata=recdata[recdata$maxcons < 14,]
#manipulation check
plyr::count(recdata$RecallManipulationCheck1.1)
plyr::count(recdata$PerformManipulationCheck1.1)
recdata=recdata[recdata$RecallManipulationCheck1.1!=1 | 
                  is.na(recdata$RecallManipulationCheck1.1),]
recdata=recdata[recdata$PerformManipulationCheck1.1!=1 | 
                  is.na(recdata$PerformManipulationCheck1.1),]
#make condition code variables
recdata$ConditionCode1=0
recdata$ConditionCode1[recdata$Condition==2 | recdata$Condition==4]=1
recdata$ConditionCode2=0
recdata$ConditionCode2[recdata$Condition==1 | recdata$Condition==2]=1
recdata$ConditionCode3=recdata$ConditionCode1 * recdata$ConditionCode2
#reverse coding items
revnames=rep(c("Connectedness", "Competence", "Autonomy"), 3)
revnums=rep(c(2, 4, 6), each=3)
revs=paste0(revnames,revnums)
recdata[,grep(paste0("^", revs, collapse = "|"), colnames(recdata))]=8-
  recdata[,grep(paste0("^", revs, collapse = "|"), colnames(recdata))]

cor(recdata[,grepl("Connectedness", colnames(recdata)) & grepl("1$", colnames(recdata))]
    , use="pairwise.complete.obs")
#Asian and Latino dummy codes
recdata$Asian1.1 = 0
recdata$Asian1.1[recdata$Ethnicity1.1==2]=1
recdata$Latino1.1 = 0
recdata$Latino1.1[recdata$Ethnicity1.1==6]=1
#Renaming Conditions
recdata$Condition[recdata$Condition == "1"] <- "Perform"
recdata$Condition[recdata$Condition == "2"] <- "PerformAndRecall"
recdata$Condition[recdata$Condition == "3"] <- "Control"
recdata$Condition[recdata$Condition == "4"] <- "Recall"
# Long Models Function ----------------------------------------------------
longmodels = function(measures, dataset, revinfo = NULL, multidims = NULL, cutvaritems = T, minitems = 3,
                      timespace = "Week", knot = NULL, growthshape = "line", growthshape2 = "line", mods = NULL,
                      tvcovs = NULL, overridesolgmeqs = NULL, write = T, updates = T, controlflwforintv = T) {
  require(psych)
  require(lavaan)
  require(ggplot2)
  require(stringr)
  numcondcodes = length(grep("ConditionCode", colnames(dataset)))
  #reverse scoring
  if(is.null(revinfo) == F) {
    for (measure in 1:length(revinfo)) {
      revmeasinfo = revinfo[[measure]]
      revmeasname = revmeasinfo[1]
      revnums = as.numeric(revmeasinfo[2:length(revmeasinfo)])
      revnums = grep(paste0(revnums, collapse = "|"), 
                     gsub(revmeasname, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",revmeasname), colnames(dataset))])))
      revmeasdata = dataset[,grep(paste0("^",revmeasname), colnames(dataset))]
      revmeasmax = max(revmeasdata, na.rm = T)
      revmeasmin = min(revmeasdata, na.rm = T)
      revmeasdata[,revnums] = (revmeasmax-revmeasmin+2)-revmeasdata[,revnums]
      dataset = dataset[,-grep(paste0("^",revmeasname), colnames(dataset))]
      dataset = cbind(dataset, revmeasdata)
    }
  }
  #creating parcels for multdimensional constructs
  if(is.null(multidims) == F) {
    for (multidim in names(multidims)){
      timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",multidims[multidim][[1]][1]), colnames(dataset))]))
      facets = multidims[multidim][[1]]
      for (timepoint in timepoints) {
        for (facet in facets) {
          ncols = ncol(dataset)
          dataset[,ncols+1] = rowMeans(dataset[grepl(facet, colnames(dataset)) & grepl(paste0("\\.",timepoint), colnames(dataset))], na.rm = T)
          colnames(dataset)[ncols+1] = paste0(multidim, grep(facet, facets),".", timepoint)
        }
      }
    }
  }
  origdata = dataset
  #measurement invariance
  chgcfis = data.frame(matrix(ncol = 3, nrow = 0))
  measinveqs = data.frame(matrix(ncol = 4, nrow = 0))
  configuralmodel = NULL
  weakmodel = NULL
  measinvmodels = list()
  chgcfisvect = NULL
  heywoodlog = NULL
  #create function to test measurement invariance
  measinvtest = function(measure) {
    chgcfisvect <<- NULL 
    prevcfi = NULL
    measinveqsvect = NULL
    timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))
    itemnums = unique(gsub(measure, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))
    for (invtype in c("configural", "weak", "strong", "strict")) {
      #bulding lavaan equations
      spemeasinveqs = NULL
      if (invtype == "configural") {
        #defining latent variables with unequal loadings (configural)
        for (timepoint in timepoints) {
          if(length(itemnums) == 2) {
            nextline = paste0(measure,".",timepoint," =~ ", paste0("a", timepoint, "*",measure,itemnums,".", timepoint, collapse = "+"), "\n")
            spemeasinveqs = paste0(spemeasinveqs, nextline)
          }else {
            nextline = paste0(measure,".",timepoint," =~ ", paste0(measure,itemnums,".", timepoint, collapse = "+"), "\n")
            spemeasinveqs = paste0(spemeasinveqs, nextline)
          }
        }
      } else {
        #defining latenet variables with equal loadings (weak)
        for (timepoint in timepoints) {
          if(length(itemnums) == 2) {
            nextline = paste0(measure,".",timepoint," =~ ", paste0("a*",measure,itemnums,".", timepoint, collapse = "+"), "\n")
          } else {
            lvname = paste0(measure,".",timepoint," =~ ")
            itemeqs = NULL
            counter = 1
            for (itemnum in itemnums) {
              itemeqs = paste0(itemeqs, paste0("a",counter,"*",measure,itemnum,".", timepoint, "+"))
              counter = counter+1
            }
            nextline = paste0(lvname, itemeqs, "\n")
          }
          spemeasinveqs = paste0(spemeasinveqs, nextline)
        }
      }
      #constraing intercepts
      if (grepl(paste0(c("strong","strict"), collapse="|"), invtype) == T) {
        for (timepoint in timepoints) {
          counter = 1
          for (itemnum in itemnums) {
            nextline = paste0(measure,itemnum,".",timepoint,"~b",counter,"*1","\n")
            spemeasinveqs = paste0(spemeasinveqs, nextline)
            counter = counter+1
          }
        }
      }
      #constraining residuals
      if (invtype == "strict") {
        for (timepoint in timepoints) {
          counter = 1
          for (itemnum in itemnums) {
            nextline = paste0(measure,itemnum,".",timepoint,"~~c",counter,"*",measure,itemnum,".",timepoint,"\n")
            spemeasinveqs = paste0(spemeasinveqs, nextline)
            counter = counter+1
          }
        }
      }
      #correlating residuals
      for (itemnum in itemnums) {
        for (timepoint in timepoints) {
          durations = as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))) - as.numeric(timepoint)
          durations = durations[durations>0]
          for (duration in durations) {
            nextline = paste0(measure,itemnum,".",timepoint,"~~d",
                              duration+((as.numeric(itemnum)-1)*(max(as.numeric(timepoints))-min(as.numeric(timepoints)))),
                              "*",measure,itemnum,".",as.numeric(timepoint)+duration,"\n")
            spemeasinveqs = paste0(spemeasinveqs, nextline)
          }
        }
      }
      spemeasinveqs = gsub("\\+\n", "\n", spemeasinveqs)
      measinveqsvect = c(measinveqsvect, spemeasinveqs)
      continue = 1
      while (continue == 1) {
        if (invtype == "configural") {
          measinvmodel = lavaan(spemeasinveqs, dataset, missing = "fiml", int.ov.free = T, int.lv.free = F,
                                auto.fix.first = T, auto.var = T, auto.cov.lv.x = T)
        } else {
          measinvmodel = lavaan(spemeasinveqs, dataset, missing = "fiml", int.ov.free = T, int.lv.free = T,
                                auto.fix.first = T, auto.var = T, auto.cov.lv.x = T) 
        }
        sink("semmodels.txt", append = T)
        summary(measinvmodel, fit.measures = T, standardized = T)
        sink()
        parest = parameterEstimates(measinvmodel)
        variances = parest[parest$op == "~~" & parest$lhs == parest$rhs,]
        heywooditems = variances$lhs[variances$est.std < 0]
        if (length(heywooditems) != 0) {
          heywoodlog = c(heywoodlog, paste0(measure, invtype, heywooditems))
          if (invtype == "strict") {
            for (heywooditem in heywooditems) {
              if (length(grep(heywooditem, parest$lhs[grep("=~",parest$op)])) > 0) {
                heywoodfix = paste0(heywooditem, "~~.001*", heywooditem, "\n")
                spemeasinveqs = paste0(spemeasinveqs, heywoodfix)
              } else {
                cnum = str_match(spemeasinveqs, paste0("\n",heywooditem,"~~c(.*?)\\*",heywooditem,"\n"))[,2]
                heywoodfix = paste0("c",cnum,"==.001\n")
                spemeasinveqs = paste0(spemeasinveqs, heywoodfix)
              }
            }
          } else {
            heywoodfix = paste0(paste0(heywooditems, "~~.001*", heywooditems, collapse = "\n"), "\n")
            spemeasinveqs = paste0(spemeasinveqs, heywoodfix)
          }
        } else {continue = 0}
      }
      measinvmodels[[paste0(measure, invtype, "model")]] <<- measinvmodel
      if (invtype != "configural") {
        chgcfi = prevcfi - fitmeasures(measinvmodel, "cfi")
        chgcfisvect <<- c(chgcfisvect,chgcfi)
      }
      prevcfi = fitmeasures(measinvmodel, "cfi")
      if (invtype == "configural") {
        configuralmodel <<- measinvmodel
      }
      if (invtype == "weak") {
        weakmodel <<- measinvmodel
      }
    }
    chgcfis <<- rbind(chgcfis, chgcfisvect)
    colnames(chgcfis) <<- c("Weak", "Strong", "Strict")
    rownames(chgcfis)[nrow(chgcfis)] <<- measure
    measinveqs <<- rbind(measinveqs, measinveqsvect, stringsAsFactors = F)
    colnames(measinveqs) <<- c("configural", "weak", "strong", "strict")
    if ((length(grep(measure, rownames(measinveqs))) > 0) & (nrow(measinveqs) > 0))  {measinveqs <<- measinveqs[-nrow(measinveqs),]}
    rownames(measinveqs)[nrow(measinveqs)] <<- measure
  }
  cutitems = NULL
  for (measure in c(measures, tvcovs)) {
    if (length(unique(gsub(measure, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))) > 1) {
      #do measinvtest for each measure and then redo based on chgcfis
      measinvtest(measure)
      minitemshit = 0
      tempcutitems = NULL
      if (cutvaritems == T) {
        #check for weak invariance and if not, delete items based on variance in loadings
        while (chgcfisvect[1] > .01) {
          stdsol = standardizedsolution(configuralmodel)
          loadings = stdsol[stdsol$op == "=~", 3:4]
          timepoints = length(unique(gsub(".*\\.","", loadings[,1])))
          loadingmat = matrix(loadings[,2], ncol = timepoints)
          loadingmat = cbind(loadingmat, NA)
          loadingmat[,ncol(loadingmat)] = apply(loadingmat,1,sd, na.rm = T)
          itemnums = gsub(paste0(measure,"|\\..*"), "",  loadings[1:(nrow(loadings)/timepoints),1])
          rownames(loadingmat) = itemnums
          item2remove = rownames(loadingmat)[grep(max(loadingmat[,ncol(loadingmat)]), loadingmat[,ncol(loadingmat)])]
          tempcutitems = c(tempcutitems, paste0(measure, item2remove))
          dataset = dataset[,-grep(paste0(measure, item2remove), colnames(dataset))]
          datasetcols = colnames(dataset)
          measurecols = datasetcols[grep(measure, datasetcols)]
          if (length(unique(gsub(paste0(measure,"|\\..*"), "", measurecols))) < minitems) {minitemshit=1;break}
          chgcfis = chgcfis[-grep(measure,rownames(chgcfis)),]
          measinveqs = measinveqs[-grep(measure,rownames(measinveqs)),]
          measinvtest(measure)
        }
        #check for strong invariance and if not, delete items based on variance in intercepts
        if (sum(grepl(measure, tvcovs)) == 0) {
          while (chgcfisvect[2] > .01) {
            stdsol = standardizedsolution(weakmodel)
            intercepts = stdsol[stdsol$op == "~1", c(1,4)]
            timepoints = length(unique(gsub(".*\\.","", intercepts[,1])))
            intercepts = intercepts[1:(nrow(intercepts)-timepoints),]
            interceptsmat = matrix(intercepts[,2], ncol = timepoints)
            interceptsmat = cbind(interceptsmat, NA)
            interceptsmat[,ncol(interceptsmat)] = apply(interceptsmat,1,sd, na.rm = T)
            itemnums = gsub(paste0(measure,"|\\..*"), "",  intercepts[1:(nrow(intercepts)/timepoints),1])
            rownames(interceptsmat) = itemnums
            item2remove = rownames(interceptsmat)[grep(max(interceptsmat[,ncol(interceptsmat)]), interceptsmat[,ncol(interceptsmat)])]
            tempcutitems = c(tempcutitems, paste0(measure, item2remove))
            dataset = dataset[,-grep(paste0(measure, item2remove), colnames(dataset))]
            datasetcols = colnames(dataset)
            measurecols = datasetcols[grep(measure, datasetcols)]
            if (length(unique(gsub(paste0(measure,"|\\..*"), "", measurecols))) < minitems) {minitemshit=1;break}
            chgcfis = chgcfis[-grep(measure,rownames(chgcfis)),]
            measinveqs = measinveqs[-grep(measure,rownames(measinveqs)),]
            measinvtest(measure)
          }
        }
        #if hit min item threshold, just use all original items
        if (minitemshit == 1) {
          tempcutitems = NULL
          dataset = dataset[,!grepl(paste0("^",measure), colnames(dataset))]
          temp2data = origdata[,grepl(paste0("^",measure), colnames(origdata))]
          dataset = cbind(dataset, temp2data)
          chgcfis = chgcfis[-grep(measure, rownames(chgcfis)),]
          measinveqs = measinveqs[-grep(measure, rownames(measinveqs)),]
          measinvtest(measure)
        }
      }
      cutitems = c(cutitems, tempcutitems)
      if (updates == T) {message("Measurement invariance testing of ", gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure), " is complete")}
    }
  }
  #alphas and omegas
  alphas = NULL
  omegas = NULL
  for (measure in c(union(measures, mods),tvcovs)) {
    timepoints = suppressWarnings(as.numeric(unique(gsub(".*\\.", "", colnames(dataset)))))
    timepoints = timepoints[!is.na(timepoints)]
    timepoints/10
    for (timepoint in timepoints) {
      cols = grepl(gsub("0", "", paste0(timepoint,"$")),colnames(dataset)) & grepl(paste0("^",measure), colnames(dataset))
      if (sum(cols) > 1) {
        rel = suppressMessages(suppressWarnings(omega(dataset[,cols], 1, fm = "ml", plot = F)))
        alpha = rel$alpha
        omega = rel$omega.tot
      } else {
        alpha = NA
        omega = NA
      }
      alphas = c(alphas, alpha)
      omegas = c(omegas, omega)
    }
  }
  alphas = as.data.frame(t(matrix(alphas, nrow = length(timepoints))))
  colnames(alphas) = paste(timespace, timepoints)
  rownames(alphas) = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",c(union(measures, mods),tvcovs))
  if (write == T) {write.csv(alphas, "alphas.csv")}
  omegas = as.data.frame(t(matrix(omegas, nrow = length(timepoints))))
  colnames(omegas) = paste(timespace, timepoints)
  rownames(omegas) = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",c(union(measures, mods),tvcovs))
  if (write == T) {write.csv(omegas, "omegas.csv")}
  if (updates == T) {message("Alphas and omegas have been computed")}
  #plots based on latent variable scores
  for (measure in measures) {
    timepoints = as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))
    itemnums = unique(gsub(measure, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))
    if (length(itemnums) == 1) {
      folgmdata = dataset[,grep(paste0(paste0("^", measure),"|Condition$"),colnames(dataset))]
      tempfolgmdata1 = folgmdata[,grep("Condition$", colnames(folgmdata))]
      tempfolgmdata2 = folgmdata[,-grep("Condition$", colnames(folgmdata))]
      folgmdata = cbind(tempfolgmdata1, tempfolgmdata2)
      colnames(folgmdata)[1] = "Condition"
      folgmdata[,2:ncol(folgmdata)] = (folgmdata[,2:ncol(folgmdata)] - mean(folgmdata[,2], na.rm = T))/sd(folgmdata[,2], na.rm = T)
      lvdescs = describeBy(folgmdata[,2:ncol(folgmdata)], folgmdata$Condition, mat = T)
      lvdescs$timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))
      lvdescs$timepoints = as.numeric(sort(lvdescs$timepoints))
    } else {
      if (chgcfis[measure,3] < .01) {
        lvploteqs = paste0(measinveqs[measure, 4], measure, ".", min(timepoints, na.rm = T),"~~1*",measure, ".", min(timepoints, na.rm = T),"\n")
      } else {
        lvploteqs = paste0(measinveqs[measure, 3], measure, ".", min(timepoints, na.rm = T),"~~1*",measure, ".", min(timepoints, na.rm = T),"\n")
      }
      lvploteqs = paste0(lvploteqs, measure, ".", min(timepoints, na.rm = T),"~0*1")
      continue = 1
      while (continue == 1) {
        lvplotmodel = lavaan(lvploteqs, dataset, missing = "fiml", int.ov.free = T, int.lv.free = T,
                             auto.fix.first = F, auto.var = T, auto.cov.lv.x = T)
        sink("semmodels.txt", append = T)
        summary(lvplotmodel, fit.measures = T, standardized = T)
        sink()
        parest = parameterEstimates(lvplotmodel)
        variances = parest[parest$op == "~~" & parest$lhs == parest$rhs,]
        heywooditems = variances$lhs[variances$est.std < 0]
        if (length(heywooditems) != 0) {
          heywoodlog = c(heywoodlog, paste0(measure, "lvplot", heywooditems))
          if (chgcfis[measure,3] < .01) {
            for (heywooditem in heywooditems) {
              if (length(grep(heywooditem, parest$lhs[grep("=~",parest$op)])) > 0) {
                heywoodfix = paste0(heywooditem, "~~.001*", heywooditem, "\n")
                lvploteqs = paste0(lvploteqs, heywoodfix)
              } else {
                cnum = str_match(lvploteqs, paste0("\n",heywooditem,"~~c(.*?)\\*",heywooditem,"\n"))[,2]
                heywoodfix = paste0("c",cnum,"==.001\n")
                lvploteqs = paste0(lvploteqs, heywoodfix)
              }
            }
          } else {
            heywoodfix = paste0(paste0(heywooditems, "~~.001*", heywooditems, collapse = "\n"), "\n")
            lvploteqs = paste0(lvploteqs, heywoodfix)
          }
        } else {continue = 0}
      }
      lvplotscores = lavPredict(lvplotmodel)
      lvplotscores = data.frame(lvplotscores, dataset$Condition)
      for (colnum in 1:(ncol(lvplotscores)-1)) {
        lvplotscores[,colnum] = as.numeric(lvplotscores[,colnum])
      }
      lvdescs = describeBy(lvplotscores, lvplotscores[,ncol(lvplotscores)], mat = T)
      lvdescs = lvdescs[1:(nrow(lvdescs)-(numcondcodes+1)),]
      lvdescs$timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))
      lvdescs$timepoints = as.numeric(sort(lvdescs$timepoints))
      lvdescs$group1 = gsub("([[:lower:]])([[:upper:]])", "\\1\n\\2",lvdescs$group1)
    }
    if(write == T) {
      tiff(paste0(measure, "LVplot.tiff"), width = 1600, height = 1200, bg = "transparent")
      lvplot = ggplot(data=lvdescs, aes(x=timepoints, y=mean, group=group1, colour=group1)) +
        geom_line(size = 5) +
        geom_point() +
        geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=1, size = 2, position=position_dodge(0)) +
        labs(x = timespace, y = measure) + #
        scale_colour_discrete(name="Condition\n",
                              labels= paste0("\n", gsub(" ", "\n", unique(lvdescs$group1)), "\n")) +
        theme(legend.background = element_rect(color = "black", linetype = "solid", fill = "transparent"),
              legend.text = element_text(size=48), legend.title = element_text(size=48),
              panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA), legend.key = element_blank(),
              axis.text.x = element_text(size = 40), axis.text.y = element_text(size = 40),
              axis.title.x = element_text(size = 48), axis.title.y = element_text(size = 48),
              plot.background = element_rect(fill = "transparent",colour = NA)) +
        guides(color = guide_legend(override.aes = list(size = 15))) + 
        scale_x_continuous(breaks = as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))),
                           labels = as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))) +
        scale_y_continuous(name = paste0(gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure), " (",timespace," ",min(timepoints, na.rm = T), " SDs)"),minor_breaks = 0)
      print(lvplot)
      dev.off()
    }
  }
  if (updates == T) {message("Figures have been created")}
  # Second-order latent growth models
  solgmfits = data.frame(matrix(ncol = 8, nrow = 0))
  solgmlvscores = data.frame(matrix(ncol = 0, nrow = nrow(dataset)))
  solgmeqs = data.frame(matrix(ncol = 1, nrow = 0))
  solgmmodels = list()
  measureadds = NULL
  for (measure in measures) {
    if (is.null(overridesolgmeqs) == F) {
      if(nchar(overridesolgmeqs[measure,])>0) {
        spesolgmeqs = overrridesolgmeqs[measure,]
      }
    } else {
      itemnums = unique(gsub(measure, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))
      timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))
      if (length(itemnums) == 1) {
        folgmdata = dataset[,grep(paste0("^", measure),colnames(dataset))]
        folgmdata = (folgmdata - mean(folgmdata[,1], na.rm = T))/sd(folgmdata[,1], na.rm = T)
        spefolgmeqs = paste0(measure,"intercept =~ ", paste0("1*",measure,"1.", timepoints, collapse = "+"), "\n")
        spefolgmeqs = paste0(spefolgmeqs, measure, "intercept ~ 0*1", "\n")
        spefolgmeqs = paste0(spefolgmeqs, measure, "intercept ~~ 1*", measure, "intercept", "\n")
        if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
          spefolgmeqs = paste0(spefolgmeqs, measure, "intvslope ~~ 0*", measure, "intvslope", "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "intvslopedist =~ 1*", measure, "intvslope", "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "intvslopedist ~ 1", "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "intvslope ~ ", measure, "intercept", "\n")
        }
        timepointsminus1 = as.numeric(timepoints) - 1
        if (is.null(knot) == T | (suppressWarnings(max(as.numeric(timepoints), na.rm = T)) <= ifelse(is.null(knot),0,knot))) {
          if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spefolgmeqs = paste0(spefolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",measure,"1.", timepoints, collapse = "+"), "\n")
          }
        } else {
          timepointsminus1[timepointsminus1 > (knot-1)] = knot-1
          if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
          timepointnums = as.numeric(timepoints)
          timepointnums[timepointnums <= knot] = 0
          timepointnums[timepointnums > knot] = timepointnums[timepointnums > knot] - knot
          if (growthshape2 != "line") {timepointnums = eval(call(growthshape2, (timepointnums)))}
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spefolgmeqs = paste0(spefolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",measure,"1.", timepoints, collapse = "+"), "\n")
          }
          spefolgmeqs = paste0(spefolgmeqs, measure, "flwslope =~ ", paste0(timepointnums, "*",measure,"1.", timepoints, collapse = "+"), "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "flwslope ~~ 0*", measure, "flwslope", "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "flwslopedist =~ 1*", measure, "flwslope", "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "flwslopedist ~ 1", "\n")
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spefolgmeqs = paste0(spefolgmeqs, measure, "flwslope ~ ", measure, "intercept\n")
          } else {
            spefolgmeqs = paste0(spefolgmeqs, measure, "flwslope ~~ ", measure, "intercept\n")
          }
          if (controlflwforintv == F) {
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spefolgmeqs = paste0(spefolgmeqs, measure, "flwslopedist ~~", measure, "intvslopedist\n")
            }
          } else {
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spefolgmeqs = paste0(spefolgmeqs, measure, "flwslope ~", measure, "intvslope\n")
            }
          }
        }
        a = 1
        if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {a=0}
        if ((!is.null(knot)) + 2 == (length(timepoints)+a)) {
          spefolgmeqs = paste0(spefolgmeqs, paste0(measure, "1.",timepoints, "~~0*", measure, "1.",timepoints, collapse = "\n"), "\n")
        }
        continue = 1
        while (continue == 1) {
          folgmmodel = lavaan(spefolgmeqs, folgmdata, missing = "fiml", int.lv.free = F,
                              auto.fix.first = F, auto.var = T, auto.cov.lv.x = F)
          sink("semmodels.txt", append = T)
          summary(folgmmodel, fit.measures = T, standardized = T)
          sink()
          parest = parameterEstimates(folgmmodel)
          variances = parest[parest$op == "~~" & parest$lhs == parest$rhs,]
          heywooditems = variances$lhs[variances$est < 0]
          if (length(heywooditems) != 0) {
            heywoodlog = c(heywoodlog, paste0(measure, "folgm", heywooditems))
            heywoodfix = paste0(paste0(heywooditems, "~~.001*", heywooditems, collapse = "\n"), "\n")
            spefolgmeqs = paste0(spefolgmeqs, heywoodfix)
          } else {continue = 0}
        }
        folgmmodelscores = as.data.frame(lavPredict(folgmmodel))
        folgmmodelscores = folgmmodelscores[, grepl("intercept|slope", colnames(folgmmodelscores))]
        solgmlvscores = cbind(solgmlvscores, folgmmodelscores)
        solgmfits = rbind.data.frame(solgmfits, fitmeasures(folgmmodel, c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower",
                                                                          "rmsea.ci.upper", "srmr")))
        colnames(solgmfits) = c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
        rownames(solgmfits)[nrow(solgmfits)] = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure)
        solgmeqs = rbind(solgmeqs, spefolgmeqs)
        solgmeqs[,1] = as.character(solgmeqs[,1])
        colnames(solgmeqs) = "solgmeqs"
        rownames(solgmeqs)[nrow(solgmeqs)] = measure
        solgmmodels[[paste0(measure, "folgmmodel")]] = folgmmodel
        if (updates == T) {message("First-order latent growth model of ", gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure), " has been created")}
      } else {
        if (chgcfis[measure,3] < .01) {
          spesolgmeqs = measinveqs[measure, 4]
        } else {
          spesolgmeqs = measinveqs[measure, 3]
        }
        spesolgmeqs = paste0(spesolgmeqs, measure, "intercept =~ ", paste0("1*",measure,".", timepoints, collapse = "+"), "\n")
        spesolgmeqs = paste0(spesolgmeqs, measure, "intercept ~ 0*1", "\n")
        spesolgmeqs = paste0(spesolgmeqs, measure, "intercept ~~ 1*", measure, "intercept", "\n")
        if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
          spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope ~~ 0*", measure, "intvslope", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intvslopedist =~ 1*", measure, "intvslope", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intvslopedist ~ 1", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope ~ ", measure, "intercept", "\n")
        }
        timepointsminus1 = as.numeric(timepoints) - 1
        if (is.null(knot) == T) {
          if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",measure,".", timepoints, collapse = "+"), "\n")
          }
        } else {
          timepointsminus1[timepointsminus1 > (knot-1)] = knot-1
          if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
          timepointnums = as.numeric(timepoints)
          timepointnums[timepointnums <= knot] = 0
          timepointnums[timepointnums > knot] = timepointnums[timepointnums > knot] - knot
          if (growthshape2 != "line") {timepointnums = eval(call(growthshape2, (timepointnums)))}
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",measure,".", timepoints, collapse = "+"), "\n")
          }
          spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope =~ ", paste0(timepointnums, "*",measure,".", timepoints, collapse = "+"), "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~~ 0*", measure, "flwslope", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist =~ 1*", measure, "flwslope", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist ~ 1", "\n")
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~ ", measure, "intercept\n")
          } else {
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~~ ", measure, "intercept\n")
          }
          if (controlflwforintv == F) {
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist ~~", measure, "intvslopedist\n")
            }
          } else {
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~", measure, "intvslope\n")
            }
          }
        }
        a = 1
        if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {a=0}
        if ((!is.null(knot)) + 2 == (length(timepoints)+a)) {
          spesolgmeqs = paste0(spesolgmeqs, paste0(measure, ".",timepoints, "~~0*", measure, ".",timepoints, collapse = "\n"), "\n")
        }
        continue = 1
        while (continue == 1) {
          solgmmodel = lavaan(spesolgmeqs, dataset, missing = "fiml", int.ov.free = T,
                              auto.fix.first = F, auto.var = T, auto.cov.lv.x = F)
          sink("semmodels.txt", append = T)
          summary(solgmmodel, fit.measures = T, standardized = T)
          sink()
          parest = parameterEstimates(solgmmodel)
          variances = parest[parest$op == "~~" & parest$lhs == parest$rhs,]
          heywooditems = variances$lhs[variances$est.std < 0]
          if (length(heywooditems) != 0) {
            heywoodlog = c(heywoodlog, paste0(measure, "solgm", heywooditems))
            if (chgcfis[measure,3] < .01) {
              for (heywooditem in heywooditems) {
                if (length(grep(heywooditem, parest$lhs[grep("=~",parest$op)])) > 0) {
                  heywoodfix = paste0(heywooditem, "~~.001*", heywooditem, "\n")
                  spesolgmeqs = paste0(spesolgmeqs, heywoodfix)
                } else {
                  cnum = str_match(spesolgmeqs, paste0("\n",heywooditem,"~~c(.*?)\\*",heywooditem,"\n"))[,2]
                  heywoodfix = paste0("c",cnum,"==.001\n")
                  spesolgmeqs = paste0(spesolgmeqs, heywoodfix)
                }
              }
            } else {
              heywoodfix = paste0(paste0(heywooditems, "~~.001*", heywooditems, collapse = "\n"), "\n")
              spesolgmeqs = paste0(spesolgmeqs, heywoodfix)
            }
          } else {continue = 0}
        }
        solgmmodelscores = as.data.frame(lavPredict(solgmmodel))
        solgmmodelscores = solgmmodelscores[, grepl("intercept|slope", colnames(solgmmodelscores))]
        solgmlvscores = cbind(solgmlvscores, solgmmodelscores)
        solgmfits = rbind.data.frame(solgmfits, fitmeasures(solgmmodel, c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower",
                                                                          "rmsea.ci.upper", "srmr")))
        colnames(solgmfits) = c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
        rownames(solgmfits)[nrow(solgmfits)] = measure
        solgmeqs = rbind(solgmeqs, spesolgmeqs)
        solgmeqs[,1] = as.character(solgmeqs[,1])
        colnames(solgmeqs) = "solgmeqs"
        rownames(solgmeqs)[nrow(solgmeqs)] = measure
        solgmmodels[[paste0(measure, "solgmmodel")]] = solgmmodel
        if (updates == T) {message("Second-order latent growth model of ", gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure), " has been created")}
        if (chgcfis[measure,2] > .01 | chgcfis[measure,1] > .01) {
          if (chgcfis[measure,1] < .01) {
            spesolgmeqs = measinveqs[measure, 2]
          } else {
            spesolgmeqs = measinveqs[measure, 1]
          }
          measure = paste0(measure, "2")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intercept =~ ", paste0("1*",substr(measure, 1, nchar(measure)-1),".", timepoints, collapse = "+"), "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intercept ~ 0*1", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intercept ~~ 1*", measure, "intercept", "\n")
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope ~~ 0*", measure, "intvslope", "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslopedist =~ 1*", measure, "intvslope", "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslopedist ~ 1", "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope ~ ", measure, "intercept", "\n")
          }
          timepointsminus1 = as.numeric(timepoints) - 1
          if (is.null(knot) == T) {
            if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",substr(measure, 1, nchar(measure)-1),".", timepoints, collapse = "+"), "\n")
            }
          } else {
            timepointsminus1[timepointsminus1 > (knot-1)] = knot-1
            if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
            timepointnums = as.numeric(timepoints)
            timepointnums[timepointnums <= knot] = 0
            timepointnums[timepointnums > knot] = timepointnums[timepointnums > knot] - knot
            if (growthshape2 != "line") {timepointnums = eval(call(growthshape2, (timepointnums)))}
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",substr(measure, 1, nchar(measure)-1),".", timepoints, collapse = "+"), "\n")
            }
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope =~ ", paste0(timepointnums, "*",substr(measure, 1, nchar(measure)-1),".", timepoints, collapse = "+"), "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~~ 0*", measure, "flwslope", "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist =~ 1*", measure, "flwslope", "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist ~ 1", "\n")
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~ ", measure, "intercept\n")
            } else {
              spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~~ ", measure, "intercept\n")
            }
            if (controlflwforintv == F) {
              if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
                spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist ~~", measure, "intvslopedist\n")
              }
            } else {
              if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
                spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~", measure, "intvslope\n")
              }
            }
          }
          a = 1
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {a=0}
          if ((!is.null(knot)) + 2 == (length(timepoints)+a)) {
            spesolgmeqs = paste0(spesolgmeqs, paste0(substr(measure, 1, nchar(measure)-1), ".",timepoints, "~~0*", substr(measure, 1, nchar(measure)-1), ".",timepoints, collapse = "\n"), "\n")
          }
          if(chgcfis[substr(measure, 1, nchar(measure)-1),1] > .01) {
            tp1eqs = paste0(substr(measure, 1, nchar(measure)-1), ".1=~",paste0(substr(measure, 1, nchar(measure)-1), itemnums, ".1", collapse = "+"))
            tp1model = cfa(tp1eqs, dataset, missing = "fiml", std.lv = T)
            loading = parameterEstimates(tp1model)[1,4]
            spesolgmeqs = gsub(paste0("=~ ", substr(measure, 1, nchar(measure)-1), itemnums[1]), paste0("=~ ", loading,"*",substr(measure, 1, nchar(measure)-1), itemnums[1]), spesolgmeqs)
          }
          continue = 1
          while (continue == 1) {
            solgmmodel = lavaan(spesolgmeqs, dataset, missing = "fiml", int.ov.free = T,
                                auto.fix.first = F, auto.var = T, auto.cov.lv.x = F)
            sink("semmodels.txt", append = T)
            summary(solgmmodel, fit.measures = T, standardized = T)
            sink()
            parest = parameterEstimates(solgmmodel)
            variances = parest[parest$op == "~~" & parest$lhs == parest$rhs,]
            heywooditems = variances$lhs[variances$est.std < 0]
            if (length(heywooditems) != 0) {
              heywoodlog = c(heywoodlog, paste0(measure, "solgm", heywooditems))
              heywoodfix = paste0(paste0(heywooditems, "~~.001*", heywooditems, collapse = "\n"), "\n")
              spesolgmeqs = paste0(spesolgmeqs, heywoodfix)
            } else {continue = 0}
          }
          solgmmodelscores = as.data.frame(lavPredict(solgmmodel))
          solgmmodelscores = solgmmodelscores[, grepl("intercept|slope", colnames(solgmmodelscores))]
          solgmlvscores = cbind(solgmlvscores, solgmmodelscores)
          solgmfits = rbind.data.frame(solgmfits, fitmeasures(solgmmodel, c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower",
                                                                            "rmsea.ci.upper", "srmr")))
          colnames(solgmfits) = c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
          rownames(solgmfits)[nrow(solgmfits)] = measure
          solgmeqs = rbind(solgmeqs, spesolgmeqs)
          solgmeqs[,1] = as.character(solgmeqs[,1])
          colnames(solgmeqs) = "solgmeqs"
          rownames(solgmeqs)[nrow(solgmeqs)] = measure
          solgmmodels[[paste0(measure, "solgmmodel")]] = solgmmodel
          measureadds = c(measureadds, measure)
        }
      }
    }
  }
  dataset = cbind(dataset, solgmlvscores)
  measures = c(measures, measureadds)
  #condition effects
  if (numcondcodes > 0) {
    condeffects = as.data.frame(matrix(ncol = 5, nrow = 0))
    slopes = colnames(dataset)[grep("slope$", colnames(dataset))]
    intercepts = gsub("slope", "intercept", slopes)
    intercepts = gsub("intv", "", intercepts)
    intercepts = gsub("flw", "", intercepts)
    for (i in 1:length(slopes)) {
      slope = slopes[i]
      intercept = intercepts[i]
      condcontrol = intercept
      if (grepl("flw", slope) & controlflwforintv & (gsub("flw", "intv", slope) %in% slopes)) {
        condcontrol = paste0(condcontrol, "+", gsub("flw", "intv", slope))
      }
      specondeqs = paste0(slope, "~ ", condcontrol, "+", paste0("ConditionCode",1:numcondcodes, collapse = "+"))
      specondmodel = sem(specondeqs, dataset, missing = "fiml", fixed.x = T)
      parest = parameterestimates(specondmodel)
      specondeffects = parest[parest$op == "~",c(1, 3, 4, 8, 9, 7)]
      colnames(specondeffects) = c("SlopeOutcome", "Predictor", "b", "lowerCI", "upperCI", "p")
      specondeffects$slopeperiod = NA
      specondeffects$slopeperiod[grepl("intvslope", specondeffects$SlopeOutcome)] = "Intervention"
      specondeffects$slopeperiod[grepl("flwslope", specondeffects$SlopeOutcome)] = "Follow-up"
      specondeffects$SlopeOutcome = gsub("intvslope|flwslope", "", specondeffects$SlopeOutcome)
      specondeffects$SlopeOutcome = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",specondeffects$SlopeOutcome)
      specondeffects$Predictor = gsub("intvslope", "InterventionSlope", specondeffects$Predictor)
      specondeffects$Predictor = gsub("intercept", "Initial", specondeffects$Predictor)
      specondeffects$Predictor[grepl("Initial", specondeffects$Predictor)] = paste0("Initial", gsub("Initial", "", specondeffects$Predictor[grepl("Initial", specondeffects$Predictor)]))
      specondeffects$Predictor = gsub("ConditionCode", "Conditioncode", specondeffects$Predictor)
      specondeffects$Predictor = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",specondeffects$Predictor)
      specondeffects$Predictor = gsub("Conditioncode", "ConditionCode", specondeffects$Predictor)
      specondeffects = specondeffects[,c(1, 7, 2:6)]
      condeffects = rbind(condeffects, specondeffects, stringsAsFactors = F)
    }
    colnames(condeffects) = c("SlopeOutcome", "Period", "Predictor", "b", "lowerCI", "upperCI", "p")
    if (write == T) {write.csv(condeffects, "condeffects.csv", row.names = F)}
    #growth rates of each condition
    growthdescs = describeBy(dataset[,grepl("slope", colnames(dataset))&!grepl("dist", colnames(dataset))], dataset$Condition, mat = T)
    growthdescs$slope = substr(rownames(growthdescs), 1, nchar(rownames(growthdescs))-1)
    growthdescs$slopeperiod = NA
    growthdescs$slopeperiod[grepl("intvslope", growthdescs$slope)] = "Intervention"
    growthdescs$slopeperiod[grepl("flwslope", growthdescs$slope)] = "follow-up"
    growthdescs$slope = gsub("intvslope|flwslope", "", growthdescs$slope)
    growthdescs$slope = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",growthdescs$slope)
    growthdescs = growthdescs[,c("slope", "slopeperiod", "group1", "mean", "se", "n")]
    growthdescs$LowerCI = growthdescs$mean-qnorm(.975)*growthdescs$se
    growthdescs$UpperCI = growthdescs$mean+qnorm(.975)*growthdescs$se
    growthdescs$t = abs(growthdescs$mean)/growthdescs$se
    growthdescs$p = pt(growthdescs$t, growthdescs$n-1, lower.tail = F)
    growthdescs = growthdescs[,c("slope", "slopeperiod", "group1", "mean", "LowerCI", "UpperCI", "p")] 
    colnames(growthdescs)[1:4] = c("Measure", "Period", "Condition", "Growth Rate")
    rownames(growthdescs) = NULL
    if (write == T) {write.csv(growthdescs, "growthdescs.csv", row.names = F)}
  }
  #within person effect - both within conditions and overall
  if (is.null(knot) == F) {
    slopediffs = data.frame(matrix(nrow = 0, ncol = (length(unique(dataset$Condition))+1)*5))
    for (measure in measures) {
      timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))
      if ((suppressWarnings(max(as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))), na.rm = T)) 
           > knot) & (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < knot)) {
        diffscores = dataset[,grepl(paste0(measure, "intvslope"), colnames(dataset))&!grepl("dist", colnames(dataset))] - 
          dataset[,grepl(paste0(measure, "flwslope"), colnames(dataset))&!grepl("dist", colnames(dataset))] 
        m = mean(diffscores, na.rm = T)
        se = sd(diffscores, na.rm = T)/sqrt(length(diffscores[is.na(diffscores) == F]))
        lci = m-qnorm(.975)*se
        uci = m+qnorm(.975)*se
        p = t.test(diffscores)$p.value
        diffvect = c(m, lci, uci, p)
        for (cond in unique(dataset$Condition)){
          diffscores2 = diffscores[dataset$Condition == cond]
          m = mean(diffscores2, na.rm = T)
          se = sd(diffscores2, na.rm = T)/sqrt(length(diffscores2[is.na(diffscores2) == F]))
          lci = m-qnorm(.975)*se
          uci = m+qnorm(.975)*se
          p = t.test(diffscores2)$p.value
          diffvect = c(diffvect, m, lci, uci, p)
        }
        slopediffs = rbind(slopediffs, diffvect)
        rownames(slopediffs)[nrow(slopediffs)] = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure)
      }
    }
    colnames(slopediffs) = paste0(rep(c("Overall", unique(dataset$Condition)), each = 4),
                                  rep(c(" MeanDiff", " LowerCI", " UpperCI", " p"), length(unique(dataset$Condition))+1))
    if (write == T) {write.csv(slopediffs, "slopediffs.csv")}
  }
  if (updates == T) {message("Effects of condition on slopes (if applicable) have been computed")}
  #moderation
  #creating moderation variables
  if (is.null(mods) == F | is.null(tvcovs) == F) {
    moddata = data.frame(matrix(ncol = 0, nrow = nrow(dataset)))
    for (mod in mods) {
      factordata = dataset[,grep(paste0("^",mod), colnames(dataset))]
      timepoints = unique(gsub(".*\\.","", colnames(dataset)[grepl(paste0("^",mod), colnames(dataset)) & grepl("[[:digit:]]$", colnames(dataset))]))
      firsttimepoint = min(as.numeric(timepoints))
      factorscoreeqs = paste0("intvInitial",mod, "=~", paste0(colnames(dataset)[grepl(paste0("^",mod), colnames(dataset)) &
                                                                                  grepl(paste0(firsttimepoint, "$"), colnames(dataset))], collapse = "+"), "\n")
      if (is.null(knot) == F) {
        if (sum(grepl(paste0("^",mod), colnames(dataset)) & grepl(paste0(knot, "$"), colnames(dataset))) != 0) {
          factorscoreeqs = paste0(factorscoreeqs, "flwInitial",mod, "=~", paste0(colnames(dataset)[grepl(paste0("^",mod), colnames(dataset)) &
                                                                                                     grepl(paste0(knot, "$"), colnames(dataset))], collapse = "+"))
        }
      }
      factorscoremodel = cfa(factorscoreeqs, dataset, missing = "fiml", std.lv = T, auto.fix.first = F, auto.cov.lv.x = T)
      factorscore = lavPredict(factorscoremodel)
      moddata = cbind(moddata, factorscore)
    }
    for (tvcov in tvcovs){
      timepoints = as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grepl(paste0("^",tvcov), colnames(dataset))& grepl("[[:digit:]]$", colnames(dataset))])))
      if (length(unique(gsub(tvcov, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",tvcov), colnames(dataset))])))) == 1) {
        if (is.null(knot) == F) {
          tvcoveqs = paste0("intvAverage",tvcov, "=~ ", paste0(tvcov, "1.", timepoints[timepoints<=knot], 
                                                               collapse = "+"), "\n")
        } else {
          tvcoveqs = paste0("intvAverage",tvcov, "=~ ", paste0(tvcov, "1.", timepoints, 
                                                               collapse = "+"), "\n")
        }
        if (is.null(knot) == F) {
          if (length(timepoints[timepoints > knot]) > 0) {
            tvcoveqs = paste0(tvcoveqs, "flwAverage",tvcov, "=~ ", paste0(tvcov, "1.", timepoints[timepoints>knot], collapse = "+")) 
          }
        }
      } else {
        tvcoveqs = ifelse(chgcfis[tvcov,2] > .01, measinveqs[tvcov,2], ifelse(chgcfis[tvcov,3] > .01, measinveqs[tvcov,3], measinveqs[tvcov,4]))
        if (is.null(knot) == F) {
          tvcoveqs = paste0(tvcoveqs, "intvAverage",tvcov, "=~ ", paste0(tvcov, ".", timepoints[timepoints<=knot], 
                                                                         collapse = "+"), "\n")
        } else {
          tvcoveqs = paste0(tvcoveqs, "intvAverage",tvcov, "=~ ", paste0(tvcov, ".", timepoints, 
                                                                         collapse = "+"), "\n")
        }
        if (is.null(knot) == F) {
          if (length(timepoints[timepoints > knot]) > 0) {
            tvcoveqs = paste0(tvcoveqs, "flwAverage",tvcov, "=~ ", paste0(tvcov, ".", timepoints[timepoints>knot], collapse = "+"))
          }
        }
      }
      tvcovmodel = cfa(tvcoveqs, dataset, missing = "fiml", std.lv = T)
      tvcovvals = lavPredict(tvcovmodel)
      tempcolnames = colnames(tvcovvals)[grep("Average", colnames(tvcovvals))]
      tvcovvals = as.data.frame(tvcovvals[,grep("Average", colnames(tvcovvals))])
      colnames(tvcovvals) = tempcolnames
      moddata = cbind(moddata, tvcovvals)
    }
    dataset = cbind(dataset, moddata)
    modeffects = as.data.frame(matrix(ncol = 6, nrow = 0))
    slopes = colnames(dataset)[grep("slope$", colnames(dataset))]
    intercepts = gsub("slope", "intercept", slopes)
    intercepts = gsub("intv", "", intercepts)
    intercepts = gsub("flw", "", intercepts)
    for (mod in colnames(moddata)) {
      for (slope in slopes) {
        if((str_count(paste0(mod,slope), coll("intv", T)) == 2) | (str_count(paste0(mod,slope), coll("flw", T)) == 2)) {
          intercept = intercepts[grep(slope, slopes)]
          modcontrol = paste0("dataset$", intercept)
          if (grepl("flw", slope) & controlflwforintv & (gsub("flw", "intv", slope) %in% slopes)) {
            modcontrol = paste0(modcontrol, "+", paste0("dataset$", gsub("flw", "intv", slope)))
          }
          modmodel = eval(parse(text = paste0("summary(lm(dataset[,slope] ~ ", modcontrol, "+", paste0("dataset$",mod,"*", "dataset$ConditionCode",1:numcondcodes, collapse = "+"), "))$coefficients")))
          modvals = as.data.frame(modmodel[-1,], stringsAsFactors = F)
          modvals$LowerCI = modvals[,1]-qnorm(.975)*modvals[,2]
          modvals$UpperCI = modvals[,1]+qnorm(.975)*modvals[,2]
          modvals = modvals[,c(1,5,6,4)]
          modvals$Outcome = slope
          modvals$Predictor = rownames(modvals)
          rownames(modvals) = NULL
          modvals$Predictor = gsub("dataset\\$","",modvals$Predictor)
          modvals = modvals[!grepl("^ConditionCode",modvals$Predictor),]
          modvals = modvals[,c(5,6,1:4)]
          modeffects = rbind(modeffects, modvals)
        }
      }
    }
    colnames(modeffects)[6] = "p"
    colnames(modeffects)[3] = "b"
    modeffects$Outcome = gsub("intvslope", "InterventionSlope", modeffects$Outcome)
    modeffects$Outcome = gsub("flwslope", "Follow-upSlope", modeffects$Outcome)
    modeffects$Outcome = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",modeffects$Outcome)
    modeffects$Outcome = gsub("2", "2 ", modeffects$Outcome)
    modeffects$Period = NA
    modeffects$Period[grepl("Intervention", modeffects$Outcome)] = "Intervention"
    modeffects$Period[grepl("Follow-up", modeffects$Outcome)] = "Follow-up"
    modeffects$Predictor = gsub("intvslope", "InterventionSlope", modeffects$Predictor)
    modeffects$Predictor= gsub("intv", "", modeffects$Predictor)
    modeffects$Predictor = gsub("flw", "", modeffects$Predictor)
    modeffects$Predictor = gsub("intercept", "Initial", modeffects$Predictor)
    modeffects$Predictor[grepl("Initial", modeffects$Predictor)] = paste0("Initial", gsub("Initial", "", modeffects$Predictor[grepl("Initial", modeffects$Predictor)]))
    modeffects$Predictor = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",modeffects$Predictor)
    modeffects$Predictor = gsub("Code", "Code ", modeffects$Predictor)
    modeffects$Predictor = gsub(":", " x ", modeffects$Predictor)
    modeffects = modeffects[,c(2, 1, 7, 3:6)]
    modeffects$predcon = modeffects$Predictor
    modeffects$predcon = gsub("Initial ","",modeffects$predcon)
    modeffects$predcon = gsub("Average ","",modeffects$predcon)
    modeffects$predcon = gsub(" x Condition Code.","",modeffects$predcon)
    modeffects = modeffects[order(modeffects$predcon, modeffects$Outcome),]
    modeffects = modeffects[,1:7]
    if (write == T) {write.csv(modeffects, "modeffects.csv", row.names = F)}
  }
  if (updates == T) {message("Effects of moderators, and time-varying covariates on slopes (if applicable) have been computed")}
  #adding level of invariance to chgcfis and renaming rows to include spaces
  if(nrow(chgcfis) > 0) {
    rownames(chgcfis) = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",rownames(chgcfis))
    chgcfis$measinvlevel = as.character(NA)
    for (i in 1:nrow(chgcfis)) {
      if (chgcfis[i,"Weak"] > .01) {
        chgcfis$measinvlevel[i] = "Configural"
      } else if (chgcfis[i,"Strong"] > .01) {
        chgcfis$measinvlevel[i] = "Weak"
      } else if (chgcfis[i,"Strict"] > .01) {
        chgcfis$measinvlevel[i] = "Strong"
      } else {
        chgcfis$measinvlevel[i] = "Strict"
      }
    }
    colnames(chgcfis)[4] = "Measurement Invariance Level"
  }
  if (write == T) {write.csv(chgcfis, "changecfis.csv")}
  colnames(solgmfits) = c("ChiSquare", "df", "CFI", "TLI", "RMSEA", "RMSEALowerCI", "RMSEAUpperCI", "SRMR")
  rownames(solgmfits) = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",rownames(solgmfits))
  if (write == T) {write.csv(solgmfits, "solgmfits.csv")}
  if (write == T) {write.csv(cutitems, "cutitems.csv")}
  if (write == T) {write.csv(heywoodlog, "heywoodlog.csv")}
  if (write == T) {write.csv(dataset, "longmodelsdata.csv")}
  #exporting list of relevant info
  outlist = list(data=dataset, changecfis=chgcfis, cutitems = cutitems, measinveqs = measinveqs, measinvmodels = measinvmodels, 
                 alphas = alphas, omegas = omegas, solgmfits = solgmfits, solgmeqs = solgmeqs, solgmmodels = solgmmodels, 
                 heywoodlog = heywoodlog)
  if (numcondcodes > 0) {outlist = c(outlist, list(condeffects = condeffects, growthdescs = growthdescs))}
  if (is.null(knot) == F) {outlist = c(outlist, list(slopediffs = slopediffs))}
  if (is.null(mods) == F | is.null(tvcovs) == F) {outlist = c(outlist, list(modeffects = modeffects))}
  outlist
}
# Long Models Analyses ----------------------------------------------------
setwd("/Users/davidko/Desktop/Recall Study/Analyses1")
reclongmodels = longmodels(
  measures = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
             "Autonomy"), 
  dataset = recdata,
  revinfo = NULL,
  multidims = NULL,
  cutvaritems = T,
  minitems = 4,
  timespace = "Day",
  knot = 2,
  growthshape = "line",
  growthshape2 = "line",
  mods = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
           "Autonomy", "Sex", "Asian", "Latino"),
  tvcovs = NULL,
  overridesolgmeqs = NULL,
  write = T,
  updates = T,
  controlflwforintv = F
)

# Dropout Analyses ----

longmodel.data = reclongmodels$data
longmodel.data$Dropout.2 = is.na(longmodel.data$EndDate1.2)
longmodel.data$Dropout.3 = is.na(longmodel.data$EndDate1.3)
corr.test(longmodel.data[, grep("^intvInitial", colnames(longmodel.data))[1:6]],
          longmodel.data[, grep("^Dropout", colnames(longmodel.data))], 
          adjust = "none")

# Power Analyses

pwr.r.test(n = 480, power = .8)

# Number of Acts Moderation -----------------------------------------------
moddata = reclongmodels$data
moddata = moddata[(moddata$Condition == "Perform") | (moddata$Condition == "PerformAndRecall"),]
summary(lm(PositiveAffectintvslope ~ PositiveAffectintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(NegativeAffectintvslope ~ NegativeAffectintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(LifeSatisfactionintvslope ~ LifeSatisfactionintercept +Condition + PerformManipulationCheck1.1, moddata))
summary(lm(Competenceintvslope ~ Competenceintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(Connectednessintvslope ~ Connectednessintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(Autonomyintvslope ~ Autonomyintercept + Condition + PerformManipulationCheck1.1, moddata))

modout = data.frame(matrix(NA, nrow = 6, ncol = 4)) #change nrow to 7
colnames(modout) = c("Day1 -> 2 b [95% CI]", "Day 1 -> 2 p", "Day1 -> 3 b [95% CI]", "Day 1 -> 3 p")
rownames(modout) = c("Positive Affect", "Negative Affect", "Life Satisfaction", "Competence", "Connectedness", 
                     "Autonomy")
rowcounter = 1
for (outcome in c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
                  "Autonomy")) {
  eval(parse(text = paste0("info = summary(lm(", outcome, "intvslopedist 
                           ~ Condition + PerformManipulationCheck1.1,moddata))")))
  info = info$coefficients
  info = info[3,c(1:2,4)]
  info = unname(info)
  b = info[1]
  se = info[2]
  p = info[3]
  p = round(p, 3)
  lci = b - qnorm(.975)*se
  uci = b + qnorm(.975)*se
  bci = paste0(round(b, 2), " [",round(lci, 2), ", ", round(uci, 2),"]") 
  modout[rowcounter, 1:2] = c(bci, p)
  rowcounter = rowcounter+1
}
rm(rowcounter, info, b, se, p, lci, uci, bci)

# Day 2 removed -----------------------------------------------------------
recdata2=recdata[,-grep("2$", colnames(recdata))[1:(length(grep("2$", colnames(recdata)))-1)]]
# Long Models Analyses #2 -------------------------------------------------
setwd("/Users/davidko/Desktop/Recall Study/Analyses2")
reclongmodels2 = longmodels(
  measures = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", 
               "Competence", "Connectedness", "Autonomy"), 
  dataset = recdata2,
  revinfo = NULL,
  multidims = NULL,
  cutvaritems = T,
  minitems = 4,
  timespace = "Day",
  knot = NULL,
  growthshape = "line",
  growthshape2 = "line",
  mods = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
           "Autonomy", "Sex", "Asian", "Latino"),
  tvcovs = NULL,
  overridesolgmeqs = NULL,
  write = T,
  updates = T,
  controlflwforintv = F
)

# Number of Acts Moderation -----------------------------------------------
moddata = reclongmodels2$data
moddata = moddata[(moddata$Condition == "Perform") | (moddata$Condition == "PerformAndRecall"),]
summary(lm(PositiveAffectintvslope ~ PositiveAffectintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(NegativeAffectintvslope ~ NegativeAffectintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(LifeSatisfactionintvslope ~ LifeSatisfactionintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(Competenceintvslope ~ Competenceintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(Connectednessintvslope ~ Connectednessintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(Autonomyintvslope ~ Autonomyintercept + Condition + PerformManipulationCheck1.1, moddata))

rowcounter = 1
for (outcome in c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
                  "Autonomy")) {
  eval(parse(text = paste0("info = summary(lm(", outcome, "intvslopedist 
                           ~ Condition + PerformManipulationCheck1.1,moddata))")))
  info = info$coefficients
  info = info[3,c(1:2,4)]
  info = unname(info)
  b = info[1]
  se = info[2]
  p = info[3]
  p = round(p, 3)
  lci = b - qnorm(.975)*se
  uci = b + qnorm(.975)*se
  bci = paste0(round(b, 2), " [",round(lci, 2), ", ", round(uci, 2),"]") 
  modout[rowcounter, 3:4] = c(bci, p)
  rowcounter = rowcounter+1
}
rm(rowcounter, info, b, se, p, lci, uci, bci)
write.csv(modout, "modout.csv")
# Cell Sizes --------------------------------------------------------------
describeBy(recdata$PositiveAffect1.1, recdata$Condition, mat = T) [,c(2,4)]
describeBy(recdata$PositiveAffect1.2, recdata$Condition, mat = T) [,c(2,4)]
describeBy(recdata$PositiveAffect1.3, recdata$Condition, mat = T) [,c(2,4)]
# Perform vs Perform and Recall -------------------------------------------
#Ran all code up to and including long models function creation
unique(recdata$Condition)
pdata = recdata[grep("Perform", recdata$Condition),]
colnames(pdata)
pdata = pdata[, -c(147, 148)]
pdata$ConditionCode1 = 0
pdata$ConditionCode1[grep("Recall", pdata$Condition)] = 1
setwd("/Users/davidko/Desktop/Recall Study/Analyses3")
reclongmodels = longmodels(
  measures = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
               "Autonomy"), 
  dataset = pdata,
  revinfo = NULL,
  multidims = NULL,
  cutvaritems = T,
  minitems = 4,
  timespace = "Day",
  knot = 2,
  growthshape = "line",
  growthshape2 = "line",
  mods = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
           "Autonomy", "Sex", "Asian", "Latino"),
  meds = NULL,
  tvcovs = NULL,
  overridesolgmeqs = NULL,
  write = T,
  updates = T,
  controlflwforintv = F
)
#Day 1 to Day 3
pdata2=pdata[,-grep("2$", colnames(pdata))[1:(length(grep("2$", colnames(pdata)))-1)]]
colnames(pdata2)
setwd("/Users/davidko/Desktop/Recall Study/Analyses4")
reclongmodels2 = longmodels(
  measures = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", 
               "Competence", "Connectedness", "Autonomy"), 
  dataset = pdata2,
  revinfo = NULL,
  multidims = NULL,
  cutvaritems = T,
  minitems = 4,
  timespace = "Day",
  knot = NULL,
  growthshape = "line",
  growthshape2 = "line",
  mods = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
           "Autonomy", "Sex", "Asian", "Latino"),
  meds = NULL,
  tvcovs = NULL,
  overridesolgmeqs = NULL,
  write = T,
  updates = T,
  controlflwforintv = F
)





# Everything above is basically scrap


# Pairwise Comparison (Each condition to control Days 1-2) -----------------------------------------------------
# Importing and cleaning data ---------------------------------------------
setwd("/Users/davidko/Desktop/Recall Study")
# Importing day 1 data from qualtrics with lines 1 & 3 removed
day1data = read.csv("day1data.csv", header=T, na.strings = "", 
                    stringsAsFactors = F, strip.white = T)
# Removing needless columns
colnames(day1data)
day1data=day1data[,-c(3:4,7:21,30:33,46:49,55:58,77:80)]
# Name columns
colnames(day1data)[1:12]=c("StartDate1.1", "EndDate1.1", "Progress1.1", "Duration1.1",
                           "FirstName1.1", "LastName1.1", "Email1.1", "Age1.1", "Sex1.1",
                           "Ethnicity1.1", "Other1.1", "ParentsEducation1.1")
colnames(day1data)[13:24]=c("PositiveAffect1.1", "NegativeAffect1.1", "PositiveAffect2.1",
                            "NegativeAffect2.1","NegativeAffect3.1","NegativeAffect4.1",
                            "PositiveAffect3.1", "NegativeAffect5.1","PositiveAffect4.1",
                            "PositiveAffect5.1","NegativeAffect6.1","PositiveAffect6.1")
colnames(day1data)[25:29] = paste0("LifeSatisfaction", 1:5, ".1")
colnames(day1data)[30:35] = paste0("Connectedness", 1:6, ".1")
colnames(day1data)[36:41] = paste0("Competence", 1:6, ".1")
colnames(day1data)[42:47] = paste0("Autonomy", 1:6, ".1")
colnames(day1data)[48]=c("Condition1.1")
# Setting correct data types
# str(day1data) <--use this to view structure
day1data$StartDate1.1=as.POSIXct(day1data$StartDate1.1, format="%m/%d/%Y %H:%M")
day1data$EndDate1.1=as.POSIXct(day1data$EndDate1.1, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day1data$FullName = paste0(day1data$FirstName1.1, day1data$LastName1.1)
day1data$Duplicate1.1 = as.numeric(duplicated(day1data$FullName) | duplicated(day1data$FullName, fromLast = T))
day1data$MinProgress1.1 = 0
day1data$MinProgress1.1[day1data$Progress1.1 == min(day1data$Progress1.1)] = 1
# plyr::count(day1data$Progress1.1)
day1data$MinProgress1.1=F
day1data$MinProgress1.1[day1data$Progress1.1==min(day1data$Progress1.1)]=T
# Subsetting data
day1data=day1data[day1data$Duplicate1.1==F & day1data$MinProgress1.1==F,]
# Importing day 2 data from qualtrics with lines 1 & 3 removed ------------
day2data = read.csv("day2data.csv", header=T, na.strings = "",
                    stringsAsFactors = F, strip.white = T)
#Removing needless columns
colnames(day2data)
day2data=day2data[,-c(3:4,7:9,13:17, 19:22, 35:38, 44:47, 66:69)]
#Name columns
colnames(day2data)[1:7]=c("StartDate1.2", "EndDate1.2", "Progress1.2", "Duration1.2",
                          "LastName1.2", "FirstName1.2", "Email1.2")
colnames(day2data)[8]=c("Recall1.2")
colnames(day2data)[9:20]=c("PositiveAffect1.2", "NegativeAffect1.2", "PositiveAffect2.2",
                           "NegativeAffect2.2","NegativeAffect3.2","NegativeAffect4.2",
                           "PositiveAffect3.2", "NegativeAffect5.2","PositiveAffect4.2",
                           "PositiveAffect5.2","NegativeAffect6.2","PositiveAffect6.2")
colnames(day2data)[21:25] = paste0("LifeSatisfaction", 1:5, ".2")
colnames(day2data)[26:31] = paste0("Connectedness", 1:6, ".2")
colnames(day2data)[32:37] = paste0("Competence", 1:6, ".2")
colnames(day2data)[38:43] = paste0("Autonomy", 1:6, ".2")
colnames(day2data)[44]=c("Condition1.2")
# Setting correct data types
day2data$StartDate1.2=as.POSIXct(day2data$StartDate1.2, format="%m/%d/%Y %H:%M")
day2data$EndDate1.2=as.POSIXct(day2data$EndDate1.2, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day2data$FullName = paste0(day2data$FirstName1.2, day2data$LastName1.2)
day2data$Duplicate1.2 = as.numeric(duplicated(day2data$FullName) | duplicated(day2data$FullName, fromLast = T))
day2data$MinProgress1.2 = 0
day2data$MinProgress1.2[day2data$Progress1.2 == min(day2data$Progress1.2)] = 1
#Subsetting data
day2data=day2data[day2data$Duplicate1.2==F & day2data$MinProgress1.2==F,]

# Importing day 3 data from qualtrics with lines 1 & 3 removed ------------
day3data = read.csv("day3data.csv", header=T, na.strings = "",
                    stringsAsFactors = F, strip.white = T)
#Removing needless columns
colnames(day3data)
day3data=day3data[,-c(3:4,7:9,13:17, 30:33, 39:42, 61:64, 66:69, 71:74,76)]
#Name columns
colnames(day3data)[1:7]=c("StartDate1.3", "EndDate1.3", "Progress1.3", "Duration1.3",
                          "LastName1.3", "FirstName1.3", "Email1.3")
colnames(day3data)[8:19]=c("PositiveAffect1.3", "NegativeAffect1.3", "PositiveAffect2.3",
                           "NegativeAffect2.3","NegativeAffect3.3","NegativeAffect4.3",
                           "PositiveAffect3.3", "NegativeAffect5.3","PositiveAffect4.3",
                           "PositiveAffect5.3","NegativeAffect6.3","PositiveAffect6.3")
colnames(day3data)[20:24] = paste0("LifeSatisfaction", 1:5, ".3")
colnames(day3data)[25:30] = paste0("Connectedness", 1:6, ".3")
colnames(day3data)[31:36] = paste0("Competence", 1:6, ".3")
colnames(day3data)[37:42] = paste0("Autonomy", 1:6, ".3")
colnames(day3data)[43:44] = c("PerformManipulationCheck1.1", "RecallManipulationCheck1.1")
colnames(day3data)[45]=c("Condition1.3")
# Setting correct data types
day3data$StartDate1.3=as.POSIXct(day3data$StartDate1.3, format="%m/%d/%Y %H:%M")
day3data$EndDate1.3=as.POSIXct(day3data$EndDate1.3, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day3data$FullName = paste0(day3data$FirstName1.3, day3data$LastName1.3)
day3data$Duplicate1.3 = as.numeric(duplicated(day3data$FullName) | duplicated(day3data$FullName, fromLast = T))
day3data$MinProgress1.3 = 0
day3data$MinProgress1.3[day3data$Progress1.3 == min(day3data$Progress1.3)] = 1
#Subsetting data
day3data=day3data[day3data$Duplicate1.3==F & day3data$MinProgress1.3==F,]

# Merging data ------------------------------------------------------------

recdata=merge(day1data,day2data,by="FullName",all=T,sort=F)
recdata=merge(recdata,day3data,by="FullName",all=T,sort=F)
#Remove entries not done on an acceptable date
recdata$Days1.2=round(recdata$EndDate1.2,units="days") - round(recdata$EndDate1.1,units="days")
recdata$Days1.3=round(recdata$EndDate1.3,units="days") - round(recdata$EndDate1.2,units="days")
recdata[(recdata$Days1.2 > 2 | is.na(recdata$Days1.2)==T), grep(".2$",colnames(recdata))]=NA
recdata[(recdata$Days1.3 > 2 | is.na(recdata$Days1.3)==T), grep(".3$",colnames(recdata))]=NA
#removing people not assigned a condition at time 1 (didn't finish day 1)
recdata$Condition=recdata$Condition1.1
recdata=recdata[,-grep("Condition1", colnames(recdata))]
recdata=recdata[!is.na(recdata$Condition),]
#removing people who weren't paying attention
recdata$maxcons = 0
for (i in 1:nrow(recdata)) {
  recdata$maxcons[i] = max(rle(recdata[i,-grep("Date", colnames(recdata))])$lengths)
}
recdata=recdata[recdata$maxcons < 14,]
#manipulation check
plyr::count(recdata$RecallManipulationCheck1.1)
plyr::count(recdata$PerformManipulationCheck1.1)
recdata=recdata[recdata$RecallManipulationCheck1.1!=1 | 
                  is.na(recdata$RecallManipulationCheck1.1),]
recdata=recdata[recdata$PerformManipulationCheck1.1!=1 | 
                  is.na(recdata$PerformManipulationCheck1.1),]
#make condition code variables
recdata$ConditionCode1=0
recdata$ConditionCode1[recdata$Condition==1]=1
recdata$ConditionCode2=0
recdata$ConditionCode2[recdata$Condition==4]=1
recdata$ConditionCode3=0
recdata$ConditionCode3[recdata$Condition==2]=1
#reverse coding items
revnames=rep(c("Connectedness", "Competence", "Autonomy"), 3)
revnums=rep(c(2, 4, 6), each=3)
revs=paste0(revnames,revnums)
recdata[,grep(paste0("^", revs, collapse = "|"), colnames(recdata))]=8-
  recdata[,grep(paste0("^", revs, collapse = "|"), colnames(recdata))]

cor(recdata[,grepl("Connectedness", colnames(recdata)) & grepl("1$", colnames(recdata))]
    , use="pairwise.complete.obs")
#Asian and Latino dummy codes
recdata$Asian1.1 = 0
recdata$Asian1.1[recdata$Ethnicity1.1==2]=1
recdata$Latino1.1 = 0
recdata$Latino1.1[recdata$Ethnicity1.1==6]=1
#Renaming Conditions
recdata$Condition[recdata$Condition == "1"] <- "Perform"
recdata$Condition[recdata$Condition == "2"] <- "PerformAndRecall"
recdata$Condition[recdata$Condition == "3"] <- "Control"
recdata$Condition[recdata$Condition == "4"] <- "Recall"
# Long Models Function ----------------------------------------------------
longmodels = function(measures, dataset, revinfo = NULL, multidims = NULL, cutvaritems = T, minitems = 3,
                      timespace = "Week", knot = NULL, growthshape = "line", growthshape2 = "line", mods = NULL,
                      tvcovs = NULL, overridesolgmeqs = NULL, write = T, updates = T, controlflwforintv = T) {
  require(psych)
  require(lavaan)
  require(ggplot2)
  require(stringr)
  numcondcodes = length(grep("ConditionCode", colnames(dataset)))
  #reverse scoring
  if(is.null(revinfo) == F) {
    for (measure in 1:length(revinfo)) {
      revmeasinfo = revinfo[[measure]]
      revmeasname = revmeasinfo[1]
      revnums = as.numeric(revmeasinfo[2:length(revmeasinfo)])
      revnums = grep(paste0(revnums, collapse = "|"), 
                     gsub(revmeasname, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",revmeasname), colnames(dataset))])))
      revmeasdata = dataset[,grep(paste0("^",revmeasname), colnames(dataset))]
      revmeasmax = max(revmeasdata, na.rm = T)
      revmeasmin = min(revmeasdata, na.rm = T)
      revmeasdata[,revnums] = (revmeasmax-revmeasmin+2)-revmeasdata[,revnums]
      dataset = dataset[,-grep(paste0("^",revmeasname), colnames(dataset))]
      dataset = cbind(dataset, revmeasdata)
    }
  }
  #creating parcels for multdimensional constructs
  if(is.null(multidims) == F) {
    for (multidim in names(multidims)){
      timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",multidims[multidim][[1]][1]), colnames(dataset))]))
      facets = multidims[multidim][[1]]
      for (timepoint in timepoints) {
        for (facet in facets) {
          ncols = ncol(dataset)
          dataset[,ncols+1] = rowMeans(dataset[grepl(facet, colnames(dataset)) & grepl(paste0("\\.",timepoint), colnames(dataset))], na.rm = T)
          colnames(dataset)[ncols+1] = paste0(multidim, grep(facet, facets),".", timepoint)
        }
      }
    }
  }
  origdata = dataset
  #measurement invariance
  chgcfis = data.frame(matrix(ncol = 3, nrow = 0))
  measinveqs = data.frame(matrix(ncol = 4, nrow = 0))
  configuralmodel = NULL
  weakmodel = NULL
  measinvmodels = list()
  chgcfisvect = NULL
  heywoodlog = NULL
  #create function to test measurement invariance
  measinvtest = function(measure) {
    chgcfisvect <<- NULL 
    prevcfi = NULL
    measinveqsvect = NULL
    timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))
    itemnums = unique(gsub(measure, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))
    for (invtype in c("configural", "weak", "strong", "strict")) {
      #bulding lavaan equations
      spemeasinveqs = NULL
      if (invtype == "configural") {
        #defining latent variables with unequal loadings (configural)
        for (timepoint in timepoints) {
          if(length(itemnums) == 2) {
            nextline = paste0(measure,".",timepoint," =~ ", paste0("a", timepoint, "*",measure,itemnums,".", timepoint, collapse = "+"), "\n")
            spemeasinveqs = paste0(spemeasinveqs, nextline)
          }else {
            nextline = paste0(measure,".",timepoint," =~ ", paste0(measure,itemnums,".", timepoint, collapse = "+"), "\n")
            spemeasinveqs = paste0(spemeasinveqs, nextline)
          }
        }
      } else {
        #defining latenet variables with equal loadings (weak)
        for (timepoint in timepoints) {
          if(length(itemnums) == 2) {
            nextline = paste0(measure,".",timepoint," =~ ", paste0("a*",measure,itemnums,".", timepoint, collapse = "+"), "\n")
          } else {
            lvname = paste0(measure,".",timepoint," =~ ")
            itemeqs = NULL
            counter = 1
            for (itemnum in itemnums) {
              itemeqs = paste0(itemeqs, paste0("a",counter,"*",measure,itemnum,".", timepoint, "+"))
              counter = counter+1
            }
            nextline = paste0(lvname, itemeqs, "\n")
          }
          spemeasinveqs = paste0(spemeasinveqs, nextline)
        }
      }
      #constraing intercepts
      if (grepl(paste0(c("strong","strict"), collapse="|"), invtype) == T) {
        for (timepoint in timepoints) {
          counter = 1
          for (itemnum in itemnums) {
            nextline = paste0(measure,itemnum,".",timepoint,"~b",counter,"*1","\n")
            spemeasinveqs = paste0(spemeasinveqs, nextline)
            counter = counter+1
          }
        }
      }
      #constraining residuals
      if (invtype == "strict") {
        for (timepoint in timepoints) {
          counter = 1
          for (itemnum in itemnums) {
            nextline = paste0(measure,itemnum,".",timepoint,"~~c",counter,"*",measure,itemnum,".",timepoint,"\n")
            spemeasinveqs = paste0(spemeasinveqs, nextline)
            counter = counter+1
          }
        }
      }
      #correlating residuals
      for (itemnum in itemnums) {
        for (timepoint in timepoints) {
          durations = as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))) - as.numeric(timepoint)
          durations = durations[durations>0]
          for (duration in durations) {
            nextline = paste0(measure,itemnum,".",timepoint,"~~d",
                              duration+((as.numeric(itemnum)-1)*(max(as.numeric(timepoints))-min(as.numeric(timepoints)))),
                              "*",measure,itemnum,".",as.numeric(timepoint)+duration,"\n")
            spemeasinveqs = paste0(spemeasinveqs, nextline)
          }
        }
      }
      spemeasinveqs = gsub("\\+\n", "\n", spemeasinveqs)
      measinveqsvect = c(measinveqsvect, spemeasinveqs)
      continue = 1
      while (continue == 1) {
        if (invtype == "configural") {
          measinvmodel = lavaan(spemeasinveqs, dataset, missing = "fiml", int.ov.free = T, int.lv.free = F,
                                auto.fix.first = T, auto.var = T, auto.cov.lv.x = T)
        } else {
          measinvmodel = lavaan(spemeasinveqs, dataset, missing = "fiml", int.ov.free = T, int.lv.free = T,
                                auto.fix.first = T, auto.var = T, auto.cov.lv.x = T) 
        }
        sink("semmodels.txt", append = T)
        summary(measinvmodel, fit.measures = T, standardized = T)
        sink()
        parest = parameterEstimates(measinvmodel)
        variances = parest[parest$op == "~~" & parest$lhs == parest$rhs,]
        heywooditems = variances$lhs[variances$est.std < 0]
        if (length(heywooditems) != 0) {
          heywoodlog = c(heywoodlog, paste0(measure, invtype, heywooditems))
          if (invtype == "strict") {
            for (heywooditem in heywooditems) {
              if (length(grep(heywooditem, parest$lhs[grep("=~",parest$op)])) > 0) {
                heywoodfix = paste0(heywooditem, "~~.001*", heywooditem, "\n")
                spemeasinveqs = paste0(spemeasinveqs, heywoodfix)
              } else {
                cnum = str_match(spemeasinveqs, paste0("\n",heywooditem,"~~c(.*?)\\*",heywooditem,"\n"))[,2]
                heywoodfix = paste0("c",cnum,"==.001\n")
                spemeasinveqs = paste0(spemeasinveqs, heywoodfix)
              }
            }
          } else {
            heywoodfix = paste0(paste0(heywooditems, "~~.001*", heywooditems, collapse = "\n"), "\n")
            spemeasinveqs = paste0(spemeasinveqs, heywoodfix)
          }
        } else {continue = 0}
      }
      measinvmodels[[paste0(measure, invtype, "model")]] <<- measinvmodel
      if (invtype != "configural") {
        chgcfi = prevcfi - fitmeasures(measinvmodel, "cfi")
        chgcfisvect <<- c(chgcfisvect,chgcfi)
      }
      prevcfi = fitmeasures(measinvmodel, "cfi")
      if (invtype == "configural") {
        configuralmodel <<- measinvmodel
      }
      if (invtype == "weak") {
        weakmodel <<- measinvmodel
      }
    }
    chgcfis <<- rbind(chgcfis, chgcfisvect)
    colnames(chgcfis) <<- c("Weak", "Strong", "Strict")
    rownames(chgcfis)[nrow(chgcfis)] <<- measure
    measinveqs <<- rbind(measinveqs, measinveqsvect, stringsAsFactors = F)
    colnames(measinveqs) <<- c("configural", "weak", "strong", "strict")
    if ((length(grep(measure, rownames(measinveqs))) > 0) & (nrow(measinveqs) > 0))  {measinveqs <<- measinveqs[-nrow(measinveqs),]}
    rownames(measinveqs)[nrow(measinveqs)] <<- measure
  }
  cutitems = NULL
  for (measure in c(measures, tvcovs)) {
    if (length(unique(gsub(measure, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))) > 1) {
      #do measinvtest for each measure and then redo based on chgcfis
      measinvtest(measure)
      minitemshit = 0
      tempcutitems = NULL
      if (cutvaritems == T) {
        #check for weak invariance and if not, delete items based on variance in loadings
        while (chgcfisvect[1] > .01) {
          stdsol = standardizedsolution(configuralmodel)
          loadings = stdsol[stdsol$op == "=~", 3:4]
          timepoints = length(unique(gsub(".*\\.","", loadings[,1])))
          loadingmat = matrix(loadings[,2], ncol = timepoints)
          loadingmat = cbind(loadingmat, NA)
          loadingmat[,ncol(loadingmat)] = apply(loadingmat,1,sd, na.rm = T)
          itemnums = gsub(paste0(measure,"|\\..*"), "",  loadings[1:(nrow(loadings)/timepoints),1])
          rownames(loadingmat) = itemnums
          item2remove = rownames(loadingmat)[grep(max(loadingmat[,ncol(loadingmat)]), loadingmat[,ncol(loadingmat)])]
          tempcutitems = c(tempcutitems, paste0(measure, item2remove))
          dataset = dataset[,-grep(paste0(measure, item2remove), colnames(dataset))]
          datasetcols = colnames(dataset)
          measurecols = datasetcols[grep(measure, datasetcols)]
          if (length(unique(gsub(paste0(measure,"|\\..*"), "", measurecols))) < minitems) {minitemshit=1;break}
          chgcfis = chgcfis[-grep(measure,rownames(chgcfis)),]
          measinveqs = measinveqs[-grep(measure,rownames(measinveqs)),]
          measinvtest(measure)
        }
        #check for strong invariance and if not, delete items based on variance in intercepts
        if (sum(grepl(measure, tvcovs)) == 0) {
          while (chgcfisvect[2] > .01) {
            stdsol = standardizedsolution(weakmodel)
            intercepts = stdsol[stdsol$op == "~1", c(1,4)]
            timepoints = length(unique(gsub(".*\\.","", intercepts[,1])))
            intercepts = intercepts[1:(nrow(intercepts)-timepoints),]
            interceptsmat = matrix(intercepts[,2], ncol = timepoints)
            interceptsmat = cbind(interceptsmat, NA)
            interceptsmat[,ncol(interceptsmat)] = apply(interceptsmat,1,sd, na.rm = T)
            itemnums = gsub(paste0(measure,"|\\..*"), "",  intercepts[1:(nrow(intercepts)/timepoints),1])
            rownames(interceptsmat) = itemnums
            item2remove = rownames(interceptsmat)[grep(max(interceptsmat[,ncol(interceptsmat)]), interceptsmat[,ncol(interceptsmat)])]
            tempcutitems = c(tempcutitems, paste0(measure, item2remove))
            dataset = dataset[,-grep(paste0(measure, item2remove), colnames(dataset))]
            datasetcols = colnames(dataset)
            measurecols = datasetcols[grep(measure, datasetcols)]
            if (length(unique(gsub(paste0(measure,"|\\..*"), "", measurecols))) < minitems) {minitemshit=1;break}
            chgcfis = chgcfis[-grep(measure,rownames(chgcfis)),]
            measinveqs = measinveqs[-grep(measure,rownames(measinveqs)),]
            measinvtest(measure)
          }
        }
        #if hit min item threshold, just use all original items
        if (minitemshit == 1) {
          tempcutitems = NULL
          dataset = dataset[,!grepl(paste0("^",measure), colnames(dataset))]
          temp2data = origdata[,grepl(paste0("^",measure), colnames(origdata))]
          dataset = cbind(dataset, temp2data)
          chgcfis = chgcfis[-grep(measure, rownames(chgcfis)),]
          measinveqs = measinveqs[-grep(measure, rownames(measinveqs)),]
          measinvtest(measure)
        }
      }
      cutitems = c(cutitems, tempcutitems)
      if (updates == T) {message("Measurement invariance testing of ", gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure), " is complete")}
    }
  }
  #alphas and omegas
  alphas = NULL
  omegas = NULL
  for (measure in c(union(measures, mods),tvcovs)) {
    timepoints = suppressWarnings(as.numeric(unique(gsub(".*\\.", "", colnames(dataset)))))
    timepoints = timepoints[!is.na(timepoints)]
    timepoints/10
    for (timepoint in timepoints) {
      cols = grepl(gsub("0", "", paste0(timepoint,"$")),colnames(dataset)) & grepl(paste0("^",measure), colnames(dataset))
      if (sum(cols) > 1) {
        rel = suppressMessages(suppressWarnings(omega(dataset[,cols], 1, fm = "ml", plot = F)))
        alpha = rel$alpha
        omega = rel$omega.tot
      } else {
        alpha = NA
        omega = NA
      }
      alphas = c(alphas, alpha)
      omegas = c(omegas, omega)
    }
  }
  alphas = as.data.frame(t(matrix(alphas, nrow = length(timepoints))))
  colnames(alphas) = paste(timespace, timepoints)
  rownames(alphas) = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",c(union(measures, mods),tvcovs))
  if (write == T) {write.csv(alphas, "alphas.csv")}
  omegas = as.data.frame(t(matrix(omegas, nrow = length(timepoints))))
  colnames(omegas) = paste(timespace, timepoints)
  rownames(omegas) = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",c(union(measures, mods),tvcovs))
  if (write == T) {write.csv(omegas, "omegas.csv")}
  if (updates == T) {message("Alphas and omegas have been computed")}
  #plots based on latent variable scores
  for (measure in measures) {
    timepoints = as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))
    itemnums = unique(gsub(measure, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))
    if (length(itemnums) == 1) {
      folgmdata = dataset[,grep(paste0(paste0("^", measure),"|Condition$"),colnames(dataset))]
      tempfolgmdata1 = folgmdata[,grep("Condition$", colnames(folgmdata))]
      tempfolgmdata2 = folgmdata[,-grep("Condition$", colnames(folgmdata))]
      folgmdata = cbind(tempfolgmdata1, tempfolgmdata2)
      colnames(folgmdata)[1] = "Condition"
      folgmdata[,2:ncol(folgmdata)] = (folgmdata[,2:ncol(folgmdata)] - mean(folgmdata[,2], na.rm = T))/sd(folgmdata[,2], na.rm = T)
      lvdescs = describeBy(folgmdata[,2:ncol(folgmdata)], folgmdata$Condition, mat = T)
      lvdescs$timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))
      lvdescs$timepoints = as.numeric(sort(lvdescs$timepoints))
    } else {
      if (chgcfis[measure,3] < .01) {
        lvploteqs = paste0(measinveqs[measure, 4], measure, ".", min(timepoints, na.rm = T),"~~1*",measure, ".", min(timepoints, na.rm = T),"\n")
      } else {
        lvploteqs = paste0(measinveqs[measure, 3], measure, ".", min(timepoints, na.rm = T),"~~1*",measure, ".", min(timepoints, na.rm = T),"\n")
      }
      lvploteqs = paste0(lvploteqs, measure, ".", min(timepoints, na.rm = T),"~0*1")
      continue = 1
      while (continue == 1) {
        lvplotmodel = lavaan(lvploteqs, dataset, missing = "fiml", int.ov.free = T, int.lv.free = T,
                             auto.fix.first = F, auto.var = T, auto.cov.lv.x = T)
        sink("semmodels.txt", append = T)
        summary(lvplotmodel, fit.measures = T, standardized = T)
        sink()
        parest = parameterEstimates(lvplotmodel)
        variances = parest[parest$op == "~~" & parest$lhs == parest$rhs,]
        heywooditems = variances$lhs[variances$est.std < 0]
        if (length(heywooditems) != 0) {
          heywoodlog = c(heywoodlog, paste0(measure, "lvplot", heywooditems))
          if (chgcfis[measure,3] < .01) {
            for (heywooditem in heywooditems) {
              if (length(grep(heywooditem, parest$lhs[grep("=~",parest$op)])) > 0) {
                heywoodfix = paste0(heywooditem, "~~.001*", heywooditem, "\n")
                lvploteqs = paste0(lvploteqs, heywoodfix)
              } else {
                cnum = str_match(lvploteqs, paste0("\n",heywooditem,"~~c(.*?)\\*",heywooditem,"\n"))[,2]
                heywoodfix = paste0("c",cnum,"==.001\n")
                lvploteqs = paste0(lvploteqs, heywoodfix)
              }
            }
          } else {
            heywoodfix = paste0(paste0(heywooditems, "~~.001*", heywooditems, collapse = "\n"), "\n")
            lvploteqs = paste0(lvploteqs, heywoodfix)
          }
        } else {continue = 0}
      }
      lvplotscores = lavPredict(lvplotmodel)
      lvplotscores = data.frame(lvplotscores, dataset$Condition)
      for (colnum in 1:(ncol(lvplotscores)-1)) {
        lvplotscores[,colnum] = as.numeric(lvplotscores[,colnum])
      }
      lvdescs = describeBy(lvplotscores, lvplotscores[,ncol(lvplotscores)], mat = T)
      lvdescs = lvdescs[1:(nrow(lvdescs)-(numcondcodes+1)),]
      lvdescs$timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))
      lvdescs$timepoints = as.numeric(sort(lvdescs$timepoints))
      lvdescs$group1 = gsub("([[:lower:]])([[:upper:]])", "\\1\n\\2",lvdescs$group1)
    }
    if(write == T) {
      tiff(paste0(measure, "LVplot.tiff"), width = 1600, height = 1200, bg = "transparent")
      lvplot = ggplot(data=lvdescs, aes(x=timepoints, y=mean, group=group1, colour=group1)) +
        geom_line(size = 5) +
        geom_point() +
        geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=1, size = 2, position=position_dodge(0)) +
        labs(x = timespace, y = measure) + #
        scale_colour_discrete(name="Condition\n",
                              labels= paste0("\n", gsub(" ", "\n", unique(lvdescs$group1)), "\n")) +
        theme(legend.background = element_rect(color = "black", linetype = "solid", fill = "transparent"),
              legend.text = element_text(size=48), legend.title = element_text(size=48),
              panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA), legend.key = element_blank(),
              axis.text.x = element_text(size = 40), axis.text.y = element_text(size = 40),
              axis.title.x = element_text(size = 48), axis.title.y = element_text(size = 48),
              plot.background = element_rect(fill = "transparent",colour = NA)) +
        guides(color = guide_legend(override.aes = list(size = 15))) + 
        scale_x_continuous(breaks = as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))),
                           labels = as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))) +
        scale_y_continuous(name = paste0(gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure), " (",timespace," ",min(timepoints, na.rm = T), " SDs)"),minor_breaks = 0)
      print(lvplot)
      dev.off()
    }
  }
  if (updates == T) {message("Figures have been created")}
  # Second-order latent growth models
  solgmfits = data.frame(matrix(ncol = 8, nrow = 0))
  solgmlvscores = data.frame(matrix(ncol = 0, nrow = nrow(dataset)))
  solgmeqs = data.frame(matrix(ncol = 1, nrow = 0))
  solgmmodels = list()
  measureadds = NULL
  for (measure in measures) {
    if (is.null(overridesolgmeqs) == F) {
      if(nchar(overridesolgmeqs[measure,])>0) {
        spesolgmeqs = overrridesolgmeqs[measure,]
      }
    } else {
      itemnums = unique(gsub(measure, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))
      timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))
      if (length(itemnums) == 1) {
        folgmdata = dataset[,grep(paste0("^", measure),colnames(dataset))]
        folgmdata = (folgmdata - mean(folgmdata[,1], na.rm = T))/sd(folgmdata[,1], na.rm = T)
        spefolgmeqs = paste0(measure,"intercept =~ ", paste0("1*",measure,"1.", timepoints, collapse = "+"), "\n")
        spefolgmeqs = paste0(spefolgmeqs, measure, "intercept ~ 0*1", "\n")
        spefolgmeqs = paste0(spefolgmeqs, measure, "intercept ~~ 1*", measure, "intercept", "\n")
        if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
          spefolgmeqs = paste0(spefolgmeqs, measure, "intvslope ~~ 0*", measure, "intvslope", "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "intvslopedist =~ 1*", measure, "intvslope", "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "intvslopedist ~ 1", "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "intvslope ~ ", measure, "intercept", "\n")
        }
        timepointsminus1 = as.numeric(timepoints) - 1
        if (is.null(knot) == T | (suppressWarnings(max(as.numeric(timepoints), na.rm = T)) <= ifelse(is.null(knot),0,knot))) {
          if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spefolgmeqs = paste0(spefolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",measure,"1.", timepoints, collapse = "+"), "\n")
          }
        } else {
          timepointsminus1[timepointsminus1 > (knot-1)] = knot-1
          if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
          timepointnums = as.numeric(timepoints)
          timepointnums[timepointnums <= knot] = 0
          timepointnums[timepointnums > knot] = timepointnums[timepointnums > knot] - knot
          if (growthshape2 != "line") {timepointnums = eval(call(growthshape2, (timepointnums)))}
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spefolgmeqs = paste0(spefolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",measure,"1.", timepoints, collapse = "+"), "\n")
          }
          spefolgmeqs = paste0(spefolgmeqs, measure, "flwslope =~ ", paste0(timepointnums, "*",measure,"1.", timepoints, collapse = "+"), "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "flwslope ~~ 0*", measure, "flwslope", "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "flwslopedist =~ 1*", measure, "flwslope", "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "flwslopedist ~ 1", "\n")
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spefolgmeqs = paste0(spefolgmeqs, measure, "flwslope ~ ", measure, "intercept\n")
          } else {
            spefolgmeqs = paste0(spefolgmeqs, measure, "flwslope ~~ ", measure, "intercept\n")
          }
          if (controlflwforintv == F) {
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spefolgmeqs = paste0(spefolgmeqs, measure, "flwslopedist ~~", measure, "intvslopedist\n")
            }
          } else {
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spefolgmeqs = paste0(spefolgmeqs, measure, "flwslope ~", measure, "intvslope\n")
            }
          }
        }
        a = 1
        if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {a=0}
        if ((!is.null(knot)) + 2 == (length(timepoints)+a)) {
          spefolgmeqs = paste0(spefolgmeqs, paste0(measure, "1.",timepoints, "~~0*", measure, "1.",timepoints, collapse = "\n"), "\n")
        }
        continue = 1
        while (continue == 1) {
          folgmmodel = lavaan(spefolgmeqs, folgmdata, missing = "fiml", int.lv.free = F,
                              auto.fix.first = F, auto.var = T, auto.cov.lv.x = F)
          sink("semmodels.txt", append = T)
          summary(folgmmodel, fit.measures = T, standardized = T)
          sink()
          parest = parameterEstimates(folgmmodel)
          variances = parest[parest$op == "~~" & parest$lhs == parest$rhs,]
          heywooditems = variances$lhs[variances$est < 0]
          if (length(heywooditems) != 0) {
            heywoodlog = c(heywoodlog, paste0(measure, "folgm", heywooditems))
            heywoodfix = paste0(paste0(heywooditems, "~~.001*", heywooditems, collapse = "\n"), "\n")
            spefolgmeqs = paste0(spefolgmeqs, heywoodfix)
          } else {continue = 0}
        }
        folgmmodelscores = as.data.frame(lavPredict(folgmmodel))
        folgmmodelscores = folgmmodelscores[, grepl("intercept|slope", colnames(folgmmodelscores))]
        solgmlvscores = cbind(solgmlvscores, folgmmodelscores)
        solgmfits = rbind.data.frame(solgmfits, fitmeasures(folgmmodel, c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower",
                                                                          "rmsea.ci.upper", "srmr")))
        colnames(solgmfits) = c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
        rownames(solgmfits)[nrow(solgmfits)] = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure)
        solgmeqs = rbind(solgmeqs, spefolgmeqs)
        solgmeqs[,1] = as.character(solgmeqs[,1])
        colnames(solgmeqs) = "solgmeqs"
        rownames(solgmeqs)[nrow(solgmeqs)] = measure
        solgmmodels[[paste0(measure, "folgmmodel")]] = folgmmodel
        if (updates == T) {message("First-order latent growth model of ", gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure), " has been created")}
      } else {
        if (chgcfis[measure,3] < .01) {
          spesolgmeqs = measinveqs[measure, 4]
        } else {
          spesolgmeqs = measinveqs[measure, 3]
        }
        spesolgmeqs = paste0(spesolgmeqs, measure, "intercept =~ ", paste0("1*",measure,".", timepoints, collapse = "+"), "\n")
        spesolgmeqs = paste0(spesolgmeqs, measure, "intercept ~ 0*1", "\n")
        spesolgmeqs = paste0(spesolgmeqs, measure, "intercept ~~ 1*", measure, "intercept", "\n")
        if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
          spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope ~~ 0*", measure, "intvslope", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intvslopedist =~ 1*", measure, "intvslope", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intvslopedist ~ 1", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope ~ ", measure, "intercept", "\n")
        }
        timepointsminus1 = as.numeric(timepoints) - 1
        if (is.null(knot) == T) {
          if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",measure,".", timepoints, collapse = "+"), "\n")
          }
        } else {
          timepointsminus1[timepointsminus1 > (knot-1)] = knot-1
          if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
          timepointnums = as.numeric(timepoints)
          timepointnums[timepointnums <= knot] = 0
          timepointnums[timepointnums > knot] = timepointnums[timepointnums > knot] - knot
          if (growthshape2 != "line") {timepointnums = eval(call(growthshape2, (timepointnums)))}
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",measure,".", timepoints, collapse = "+"), "\n")
          }
          spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope =~ ", paste0(timepointnums, "*",measure,".", timepoints, collapse = "+"), "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~~ 0*", measure, "flwslope", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist =~ 1*", measure, "flwslope", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist ~ 1", "\n")
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~ ", measure, "intercept\n")
          } else {
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~~ ", measure, "intercept\n")
          }
          if (controlflwforintv == F) {
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist ~~", measure, "intvslopedist\n")
            }
          } else {
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~", measure, "intvslope\n")
            }
          }
        }
        a = 1
        if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {a=0}
        if ((!is.null(knot)) + 2 == (length(timepoints)+a)) {
          spesolgmeqs = paste0(spesolgmeqs, paste0(measure, ".",timepoints, "~~0*", measure, ".",timepoints, collapse = "\n"), "\n")
        }
        continue = 1
        while (continue == 1) {
          solgmmodel = lavaan(spesolgmeqs, dataset, missing = "fiml", int.ov.free = T,
                              auto.fix.first = F, auto.var = T, auto.cov.lv.x = F)
          sink("semmodels.txt", append = T)
          summary(solgmmodel, fit.measures = T, standardized = T)
          sink()
          parest = parameterEstimates(solgmmodel)
          variances = parest[parest$op == "~~" & parest$lhs == parest$rhs,]
          heywooditems = variances$lhs[variances$est.std < 0]
          if (length(heywooditems) != 0) {
            heywoodlog = c(heywoodlog, paste0(measure, "solgm", heywooditems))
            if (chgcfis[measure,3] < .01) {
              for (heywooditem in heywooditems) {
                if (length(grep(heywooditem, parest$lhs[grep("=~",parest$op)])) > 0) {
                  heywoodfix = paste0(heywooditem, "~~.001*", heywooditem, "\n")
                  spesolgmeqs = paste0(spesolgmeqs, heywoodfix)
                } else {
                  cnum = str_match(spesolgmeqs, paste0("\n",heywooditem,"~~c(.*?)\\*",heywooditem,"\n"))[,2]
                  heywoodfix = paste0("c",cnum,"==.001\n")
                  spesolgmeqs = paste0(spesolgmeqs, heywoodfix)
                }
              }
            } else {
              heywoodfix = paste0(paste0(heywooditems, "~~.001*", heywooditems, collapse = "\n"), "\n")
              spesolgmeqs = paste0(spesolgmeqs, heywoodfix)
            }
          } else {continue = 0}
        }
        solgmmodelscores = as.data.frame(lavPredict(solgmmodel))
        solgmmodelscores = solgmmodelscores[, grepl("intercept|slope", colnames(solgmmodelscores))]
        solgmlvscores = cbind(solgmlvscores, solgmmodelscores)
        solgmfits = rbind.data.frame(solgmfits, fitmeasures(solgmmodel, c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower",
                                                                          "rmsea.ci.upper", "srmr")))
        colnames(solgmfits) = c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
        rownames(solgmfits)[nrow(solgmfits)] = measure
        solgmeqs = rbind(solgmeqs, spesolgmeqs)
        solgmeqs[,1] = as.character(solgmeqs[,1])
        colnames(solgmeqs) = "solgmeqs"
        rownames(solgmeqs)[nrow(solgmeqs)] = measure
        solgmmodels[[paste0(measure, "solgmmodel")]] = solgmmodel
        if (updates == T) {message("Second-order latent growth model of ", gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure), " has been created")}
        if (chgcfis[measure,2] > .01 | chgcfis[measure,1] > .01) {
          if (chgcfis[measure,1] < .01) {
            spesolgmeqs = measinveqs[measure, 2]
          } else {
            spesolgmeqs = measinveqs[measure, 1]
          }
          measure = paste0(measure, "2")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intercept =~ ", paste0("1*",substr(measure, 1, nchar(measure)-1),".", timepoints, collapse = "+"), "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intercept ~ 0*1", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intercept ~~ 1*", measure, "intercept", "\n")
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope ~~ 0*", measure, "intvslope", "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslopedist =~ 1*", measure, "intvslope", "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslopedist ~ 1", "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope ~ ", measure, "intercept", "\n")
          }
          timepointsminus1 = as.numeric(timepoints) - 1
          if (is.null(knot) == T) {
            if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",substr(measure, 1, nchar(measure)-1),".", timepoints, collapse = "+"), "\n")
            }
          } else {
            timepointsminus1[timepointsminus1 > (knot-1)] = knot-1
            if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
            timepointnums = as.numeric(timepoints)
            timepointnums[timepointnums <= knot] = 0
            timepointnums[timepointnums > knot] = timepointnums[timepointnums > knot] - knot
            if (growthshape2 != "line") {timepointnums = eval(call(growthshape2, (timepointnums)))}
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",substr(measure, 1, nchar(measure)-1),".", timepoints, collapse = "+"), "\n")
            }
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope =~ ", paste0(timepointnums, "*",substr(measure, 1, nchar(measure)-1),".", timepoints, collapse = "+"), "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~~ 0*", measure, "flwslope", "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist =~ 1*", measure, "flwslope", "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist ~ 1", "\n")
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~ ", measure, "intercept\n")
            } else {
              spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~~ ", measure, "intercept\n")
            }
            if (controlflwforintv == F) {
              if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
                spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist ~~", measure, "intvslopedist\n")
              }
            } else {
              if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
                spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~", measure, "intvslope\n")
              }
            }
          }
          a = 1
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {a=0}
          if ((!is.null(knot)) + 2 == (length(timepoints)+a)) {
            spesolgmeqs = paste0(spesolgmeqs, paste0(substr(measure, 1, nchar(measure)-1), ".",timepoints, "~~0*", substr(measure, 1, nchar(measure)-1), ".",timepoints, collapse = "\n"), "\n")
          }
          if(chgcfis[substr(measure, 1, nchar(measure)-1),1] > .01) {
            tp1eqs = paste0(substr(measure, 1, nchar(measure)-1), ".1=~",paste0(substr(measure, 1, nchar(measure)-1), itemnums, ".1", collapse = "+"))
            tp1model = cfa(tp1eqs, dataset, missing = "fiml", std.lv = T)
            loading = parameterEstimates(tp1model)[1,4]
            spesolgmeqs = gsub(paste0("=~ ", substr(measure, 1, nchar(measure)-1), itemnums[1]), paste0("=~ ", loading,"*",substr(measure, 1, nchar(measure)-1), itemnums[1]), spesolgmeqs)
          }
          continue = 1
          while (continue == 1) {
            solgmmodel = lavaan(spesolgmeqs, dataset, missing = "fiml", int.ov.free = T,
                                auto.fix.first = F, auto.var = T, auto.cov.lv.x = F)
            sink("semmodels.txt", append = T)
            summary(solgmmodel, fit.measures = T, standardized = T)
            sink()
            parest = parameterEstimates(solgmmodel)
            variances = parest[parest$op == "~~" & parest$lhs == parest$rhs,]
            heywooditems = variances$lhs[variances$est.std < 0]
            if (length(heywooditems) != 0) {
              heywoodlog = c(heywoodlog, paste0(measure, "solgm", heywooditems))
              heywoodfix = paste0(paste0(heywooditems, "~~.001*", heywooditems, collapse = "\n"), "\n")
              spesolgmeqs = paste0(spesolgmeqs, heywoodfix)
            } else {continue = 0}
          }
          solgmmodelscores = as.data.frame(lavPredict(solgmmodel))
          solgmmodelscores = solgmmodelscores[, grepl("intercept|slope", colnames(solgmmodelscores))]
          solgmlvscores = cbind(solgmlvscores, solgmmodelscores)
          solgmfits = rbind.data.frame(solgmfits, fitmeasures(solgmmodel, c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower",
                                                                            "rmsea.ci.upper", "srmr")))
          colnames(solgmfits) = c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
          rownames(solgmfits)[nrow(solgmfits)] = measure
          solgmeqs = rbind(solgmeqs, spesolgmeqs)
          solgmeqs[,1] = as.character(solgmeqs[,1])
          colnames(solgmeqs) = "solgmeqs"
          rownames(solgmeqs)[nrow(solgmeqs)] = measure
          solgmmodels[[paste0(measure, "solgmmodel")]] = solgmmodel
          measureadds = c(measureadds, measure)
        }
      }
    }
  }
  dataset = cbind(dataset, solgmlvscores)
  measures = c(measures, measureadds)
  #condition effects
  if (numcondcodes > 0) {
    condeffects = as.data.frame(matrix(ncol = 5, nrow = 0))
    slopes = colnames(dataset)[grep("slope$", colnames(dataset))]
    intercepts = gsub("slope", "intercept", slopes)
    intercepts = gsub("intv", "", intercepts)
    intercepts = gsub("flw", "", intercepts)
    for (i in 1:length(slopes)) {
      slope = slopes[i]
      intercept = intercepts[i]
      condcontrol = intercept
      if (grepl("flw", slope) & controlflwforintv & (gsub("flw", "intv", slope) %in% slopes)) {
        condcontrol = paste0(condcontrol, "+", gsub("flw", "intv", slope))
      }
      specondeqs = paste0(slope, "~ ", condcontrol, "+", paste0("ConditionCode",1:numcondcodes, collapse = "+"))
      specondmodel = sem(specondeqs, dataset, missing = "fiml", fixed.x = T)
      parest = parameterestimates(specondmodel)
      specondeffects = parest[parest$op == "~",c(1, 3, 4, 8, 9, 7)]
      colnames(specondeffects) = c("SlopeOutcome", "Predictor", "b", "lowerCI", "upperCI", "p")
      specondeffects$slopeperiod = NA
      specondeffects$slopeperiod[grepl("intvslope", specondeffects$SlopeOutcome)] = "Intervention"
      specondeffects$slopeperiod[grepl("flwslope", specondeffects$SlopeOutcome)] = "Follow-up"
      specondeffects$SlopeOutcome = gsub("intvslope|flwslope", "", specondeffects$SlopeOutcome)
      specondeffects$SlopeOutcome = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",specondeffects$SlopeOutcome)
      specondeffects$Predictor = gsub("intvslope", "InterventionSlope", specondeffects$Predictor)
      specondeffects$Predictor = gsub("intercept", "Initial", specondeffects$Predictor)
      specondeffects$Predictor[grepl("Initial", specondeffects$Predictor)] = paste0("Initial", gsub("Initial", "", specondeffects$Predictor[grepl("Initial", specondeffects$Predictor)]))
      specondeffects$Predictor = gsub("ConditionCode", "Conditioncode", specondeffects$Predictor)
      specondeffects$Predictor = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",specondeffects$Predictor)
      specondeffects$Predictor = gsub("Conditioncode", "ConditionCode", specondeffects$Predictor)
      specondeffects = specondeffects[,c(1, 7, 2:6)]
      condeffects = rbind(condeffects, specondeffects, stringsAsFactors = F)
    }
    colnames(condeffects) = c("SlopeOutcome", "Period", "Predictor", "b", "lowerCI", "upperCI", "p")
    if (write == T) {write.csv(condeffects, "condeffects.csv", row.names = F)}
    #growth rates of each condition
    growthdescs = describeBy(dataset[,grepl("slope", colnames(dataset))&!grepl("dist", colnames(dataset))], dataset$Condition, mat = T)
    growthdescs$slope = substr(rownames(growthdescs), 1, nchar(rownames(growthdescs))-1)
    growthdescs$slopeperiod = NA
    growthdescs$slopeperiod[grepl("intvslope", growthdescs$slope)] = "Intervention"
    growthdescs$slopeperiod[grepl("flwslope", growthdescs$slope)] = "follow-up"
    growthdescs$slope = gsub("intvslope|flwslope", "", growthdescs$slope)
    growthdescs$slope = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",growthdescs$slope)
    growthdescs = growthdescs[,c("slope", "slopeperiod", "group1", "mean", "se", "n")]
    growthdescs$LowerCI = growthdescs$mean-qnorm(.975)*growthdescs$se
    growthdescs$UpperCI = growthdescs$mean+qnorm(.975)*growthdescs$se
    growthdescs$t = abs(growthdescs$mean)/growthdescs$se
    growthdescs$p = pt(growthdescs$t, growthdescs$n-1, lower.tail = F)
    growthdescs = growthdescs[,c("slope", "slopeperiod", "group1", "mean", "LowerCI", "UpperCI", "p")] 
    colnames(growthdescs)[1:4] = c("Measure", "Period", "Condition", "Growth Rate")
    rownames(growthdescs) = NULL
    if (write == T) {write.csv(growthdescs, "growthdescs.csv", row.names = F)}
  }
  #within person effect - both within conditions and overall
  if (is.null(knot) == F) {
    slopediffs = data.frame(matrix(nrow = 0, ncol = (length(unique(dataset$Condition))+1)*5))
    for (measure in measures) {
      timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))
      if ((suppressWarnings(max(as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))), na.rm = T)) 
           > knot) & (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < knot)) {
        diffscores = dataset[,grepl(paste0(measure, "intvslope"), colnames(dataset))&!grepl("dist", colnames(dataset))] - 
          dataset[,grepl(paste0(measure, "flwslope"), colnames(dataset))&!grepl("dist", colnames(dataset))] 
        m = mean(diffscores, na.rm = T)
        se = sd(diffscores, na.rm = T)/sqrt(length(diffscores[is.na(diffscores) == F]))
        lci = m-qnorm(.975)*se
        uci = m+qnorm(.975)*se
        p = t.test(diffscores)$p.value
        diffvect = c(m, lci, uci, p)
        for (cond in unique(dataset$Condition)){
          diffscores2 = diffscores[dataset$Condition == cond]
          m = mean(diffscores2, na.rm = T)
          se = sd(diffscores2, na.rm = T)/sqrt(length(diffscores2[is.na(diffscores2) == F]))
          lci = m-qnorm(.975)*se
          uci = m+qnorm(.975)*se
          p = t.test(diffscores2)$p.value
          diffvect = c(diffvect, m, lci, uci, p)
        }
        slopediffs = rbind(slopediffs, diffvect)
        rownames(slopediffs)[nrow(slopediffs)] = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure)
      }
    }
    colnames(slopediffs) = paste0(rep(c("Overall", unique(dataset$Condition)), each = 4),
                                  rep(c(" MeanDiff", " LowerCI", " UpperCI", " p"), length(unique(dataset$Condition))+1))
    if (write == T) {write.csv(slopediffs, "slopediffs.csv")}
  }
  if (updates == T) {message("Effects of condition on slopes (if applicable) have been computed")}
  #moderation
  #creating moderation variables
  if (is.null(mods) == F | is.null(tvcovs) == F) {
    moddata = data.frame(matrix(ncol = 0, nrow = nrow(dataset)))
    for (mod in mods) {
      factordata = dataset[,grep(paste0("^",mod), colnames(dataset))]
      timepoints = unique(gsub(".*\\.","", colnames(dataset)[grepl(paste0("^",mod), colnames(dataset)) & grepl("[[:digit:]]$", colnames(dataset))]))
      firsttimepoint = min(as.numeric(timepoints))
      factorscoreeqs = paste0("intvInitial",mod, "=~", paste0(colnames(dataset)[grepl(paste0("^",mod), colnames(dataset)) &
                                                                                  grepl(paste0(firsttimepoint, "$"), colnames(dataset))], collapse = "+"), "\n")
      if (is.null(knot) == F) {
        if (sum(grepl(paste0("^",mod), colnames(dataset)) & grepl(paste0(knot, "$"), colnames(dataset))) != 0) {
          factorscoreeqs = paste0(factorscoreeqs, "flwInitial",mod, "=~", paste0(colnames(dataset)[grepl(paste0("^",mod), colnames(dataset)) &
                                                                                                     grepl(paste0(knot, "$"), colnames(dataset))], collapse = "+"))
        }
      }
      factorscoremodel = cfa(factorscoreeqs, dataset, missing = "fiml", std.lv = T, auto.fix.first = F, auto.cov.lv.x = T)
      factorscore = lavPredict(factorscoremodel)
      moddata = cbind(moddata, factorscore)
    }
    for (tvcov in tvcovs){
      timepoints = as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grepl(paste0("^",tvcov), colnames(dataset))& grepl("[[:digit:]]$", colnames(dataset))])))
      if (length(unique(gsub(tvcov, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",tvcov), colnames(dataset))])))) == 1) {
        if (is.null(knot) == F) {
          tvcoveqs = paste0("intvAverage",tvcov, "=~ ", paste0(tvcov, "1.", timepoints[timepoints<=knot], 
                                                               collapse = "+"), "\n")
        } else {
          tvcoveqs = paste0("intvAverage",tvcov, "=~ ", paste0(tvcov, "1.", timepoints, 
                                                               collapse = "+"), "\n")
        }
        if (is.null(knot) == F) {
          if (length(timepoints[timepoints > knot]) > 0) {
            tvcoveqs = paste0(tvcoveqs, "flwAverage",tvcov, "=~ ", paste0(tvcov, "1.", timepoints[timepoints>knot], collapse = "+")) 
          }
        }
      } else {
        tvcoveqs = ifelse(chgcfis[tvcov,2] > .01, measinveqs[tvcov,2], ifelse(chgcfis[tvcov,3] > .01, measinveqs[tvcov,3], measinveqs[tvcov,4]))
        if (is.null(knot) == F) {
          tvcoveqs = paste0(tvcoveqs, "intvAverage",tvcov, "=~ ", paste0(tvcov, ".", timepoints[timepoints<=knot], 
                                                                         collapse = "+"), "\n")
        } else {
          tvcoveqs = paste0(tvcoveqs, "intvAverage",tvcov, "=~ ", paste0(tvcov, ".", timepoints, 
                                                                         collapse = "+"), "\n")
        }
        if (is.null(knot) == F) {
          if (length(timepoints[timepoints > knot]) > 0) {
            tvcoveqs = paste0(tvcoveqs, "flwAverage",tvcov, "=~ ", paste0(tvcov, ".", timepoints[timepoints>knot], collapse = "+"))
          }
        }
      }
      tvcovmodel = cfa(tvcoveqs, dataset, missing = "fiml", std.lv = T)
      tvcovvals = lavPredict(tvcovmodel)
      tempcolnames = colnames(tvcovvals)[grep("Average", colnames(tvcovvals))]
      tvcovvals = as.data.frame(tvcovvals[,grep("Average", colnames(tvcovvals))])
      colnames(tvcovvals) = tempcolnames
      moddata = cbind(moddata, tvcovvals)
    }
    dataset = cbind(dataset, moddata)
    modeffects = as.data.frame(matrix(ncol = 6, nrow = 0))
    slopes = colnames(dataset)[grep("slope$", colnames(dataset))]
    intercepts = gsub("slope", "intercept", slopes)
    intercepts = gsub("intv", "", intercepts)
    intercepts = gsub("flw", "", intercepts)
    for (mod in colnames(moddata)) {
      for (slope in slopes) {
        if((str_count(paste0(mod,slope), coll("intv", T)) == 2) | (str_count(paste0(mod,slope), coll("flw", T)) == 2)) {
          intercept = intercepts[grep(slope, slopes)]
          modcontrol = paste0("dataset$", intercept)
          if (grepl("flw", slope) & controlflwforintv & (gsub("flw", "intv", slope) %in% slopes)) {
            modcontrol = paste0(modcontrol, "+", paste0("dataset$", gsub("flw", "intv", slope)))
          }
          modmodel = eval(parse(text = paste0("summary(lm(dataset[,slope] ~ ", modcontrol, "+", paste0("dataset$",mod,"*", "dataset$ConditionCode",1:numcondcodes, collapse = "+"), "))$coefficients")))
          modvals = as.data.frame(modmodel[-1,], stringsAsFactors = F)
          modvals$LowerCI = modvals[,1]-qnorm(.975)*modvals[,2]
          modvals$UpperCI = modvals[,1]+qnorm(.975)*modvals[,2]
          modvals = modvals[,c(1,5,6,4)]
          modvals$Outcome = slope
          modvals$Predictor = rownames(modvals)
          rownames(modvals) = NULL
          modvals$Predictor = gsub("dataset\\$","",modvals$Predictor)
          modvals = modvals[!grepl("^ConditionCode",modvals$Predictor),]
          modvals = modvals[,c(5,6,1:4)]
          modeffects = rbind(modeffects, modvals)
        }
      }
    }
    colnames(modeffects)[6] = "p"
    colnames(modeffects)[3] = "b"
    modeffects$Outcome = gsub("intvslope", "InterventionSlope", modeffects$Outcome)
    modeffects$Outcome = gsub("flwslope", "Follow-upSlope", modeffects$Outcome)
    modeffects$Outcome = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",modeffects$Outcome)
    modeffects$Outcome = gsub("2", "2 ", modeffects$Outcome)
    modeffects$Period = NA
    modeffects$Period[grepl("Intervention", modeffects$Outcome)] = "Intervention"
    modeffects$Period[grepl("Follow-up", modeffects$Outcome)] = "Follow-up"
    modeffects$Predictor = gsub("intvslope", "InterventionSlope", modeffects$Predictor)
    modeffects$Predictor= gsub("intv", "", modeffects$Predictor)
    modeffects$Predictor = gsub("flw", "", modeffects$Predictor)
    modeffects$Predictor = gsub("intercept", "Initial", modeffects$Predictor)
    modeffects$Predictor[grepl("Initial", modeffects$Predictor)] = paste0("Initial", gsub("Initial", "", modeffects$Predictor[grepl("Initial", modeffects$Predictor)]))
    modeffects$Predictor = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",modeffects$Predictor)
    modeffects$Predictor = gsub("Code", "Code ", modeffects$Predictor)
    modeffects$Predictor = gsub(":", " x ", modeffects$Predictor)
    modeffects = modeffects[,c(2, 1, 7, 3:6)]
    modeffects$predcon = modeffects$Predictor
    modeffects$predcon = gsub("Initial ","",modeffects$predcon)
    modeffects$predcon = gsub("Average ","",modeffects$predcon)
    modeffects$predcon = gsub(" x Condition Code.","",modeffects$predcon)
    modeffects = modeffects[order(modeffects$predcon, modeffects$Outcome),]
    modeffects = modeffects[,1:7]
    if (write == T) {write.csv(modeffects, "modeffects.csv", row.names = F)}
  }
  if (updates == T) {message("Effects of moderators, and time-varying covariates on slopes (if applicable) have been computed")}
  #adding level of invariance to chgcfis and renaming rows to include spaces
  if(nrow(chgcfis) > 0) {
    rownames(chgcfis) = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",rownames(chgcfis))
    chgcfis$measinvlevel = as.character(NA)
    for (i in 1:nrow(chgcfis)) {
      if (chgcfis[i,"Weak"] > .01) {
        chgcfis$measinvlevel[i] = "Configural"
      } else if (chgcfis[i,"Strong"] > .01) {
        chgcfis$measinvlevel[i] = "Weak"
      } else if (chgcfis[i,"Strict"] > .01) {
        chgcfis$measinvlevel[i] = "Strong"
      } else {
        chgcfis$measinvlevel[i] = "Strict"
      }
    }
    colnames(chgcfis)[4] = "Measurement Invariance Level"
  }
  if (write == T) {write.csv(chgcfis, "changecfis.csv")}
  colnames(solgmfits) = c("ChiSquare", "df", "CFI", "TLI", "RMSEA", "RMSEALowerCI", "RMSEAUpperCI", "SRMR")
  rownames(solgmfits) = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",rownames(solgmfits))
  if (write == T) {write.csv(solgmfits, "solgmfits.csv")}
  if (write == T) {write.csv(cutitems, "cutitems.csv")}
  if (write == T) {write.csv(heywoodlog, "heywoodlog.csv")}
  if (write == T) {write.csv(dataset, "longmodelsdata.csv")}
  #exporting list of relevant info
  outlist = list(data=dataset, changecfis=chgcfis, cutitems = cutitems, measinveqs = measinveqs, measinvmodels = measinvmodels, 
                 alphas = alphas, omegas = omegas, solgmfits = solgmfits, solgmeqs = solgmeqs, solgmmodels = solgmmodels, 
                 heywoodlog = heywoodlog)
  if (numcondcodes > 0) {outlist = c(outlist, list(condeffects = condeffects, growthdescs = growthdescs))}
  if (is.null(knot) == F) {outlist = c(outlist, list(slopediffs = slopediffs))}
  if (is.null(mods) == F | is.null(tvcovs) == F) {outlist = c(outlist, list(modeffects = modeffects))}
  outlist
}
# Long Models Analyses ----------------------------------------------------
setwd("/Users/davidko/Desktop/Recall Study/Analyses5")
reclongmodels = longmodels(
  measures = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
               "Autonomy"), 
  dataset = recdata,
  revinfo = NULL,
  multidims = NULL,
  cutvaritems = T,
  minitems = 4,
  timespace = "Day",
  knot = 2,
  growthshape = "line",
  growthshape2 = "line",
  mods = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
           "Autonomy", "Sex", "Asian", "Latino"),
  tvcovs = NULL,
  overridesolgmeqs = NULL,
  write = T,
  updates = T,
  controlflwforintv = F
)
#Look at manipulation check effects manually




# Number of Acts Moderation -----------------------------------------------
moddata = reclongmodels$data
moddata = moddata[(moddata$Condition == "Perform") | (moddata$Condition == "PerformAndRecall"),]
summary(lm(PositiveAffectintvslope ~ PositiveAffectintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(NegativeAffectintvslope ~ NegativeAffectintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(LifeSatisfactionintvslope ~ LifeSatisfactionintercept +Condition + PerformManipulationCheck1.1, moddata))
summary(lm(Competenceintvslope ~ Competenceintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(Connectednessintvslope ~ Connectednessintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(Autonomyintvslope ~ Autonomyintercept + Condition + PerformManipulationCheck1.1, moddata))

modout = data.frame(matrix(NA, nrow = 6, ncol = 4))
colnames(modout) = c("Day1 -> 2 b [95% CI]", "Day 1 -> 2 p", "Day1 -> 3 b [95% CI]", "Day 1 -> 3 p")
rownames(modout) = c("Positive Affect", "Negative Affect", "Life Satisfaction", "Competence", "Connectedness", 
                     "Autonomy")
rowcounter = 1
for (outcome in c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
                  "Autonomy")) {
  eval(parse(text = paste0("info = summary(lm(", outcome, "intvslope 
                           ~ ", outcome, "intercept + Condition + PerformManipulationCheck1.1,moddata))")))
  info = info$coefficients
  info = info[4,c(1:2,4)]
  info = unname(info)
  b = info[1]
  se = info[2]
  p = info[3]
  p = round(p, 3)
  lci = b - qnorm(.975)*se
  uci = b + qnorm(.975)*se
  bci = paste0(round(b, 2), " [",round(lci, 2), ", ", round(uci, 2),"]") 
  modout[rowcounter, 1:2] = c(bci, p)
  rowcounter = rowcounter+1
}
rm(rowcounter, info, b, se, p, lci, uci, bci)
# Performing vs Recalling -------------------------------------------------
pvrdata = reclongmodels$data
pvrdata = pvrdata[ pvrdata$Condition != "Control",]
pvrdata$Condition = as.factor(pvrdata$Condition)
PA12mod1 = lm(PositiveAffectintvslope ~ PositiveAffectintercept, data = pvrdata)
PA12mod2 = lm(PositiveAffectintvslope ~ PositiveAffectintercept + Condition, data = pvrdata)
anova(PA12mod1, PA12mod2)
#p = 0.6143 (not significant)
NA12mod1 = lm(NegativeAffectintvslope ~ NegativeAffectintercept, data = pvrdata)
NA12mod2 = lm(NegativeAffectintvslope ~ NegativeAffectintercept + Condition, data = pvrdata)
anova(NA12mod1, NA12mod2)
#p = 0.5447
LS12mod1 = lm(LifeSatisfactionintvslope ~ LifeSatisfactionintercept, data = pvrdata)
LS12mod2 = lm(LifeSatisfactionintvslope ~ LifeSatisfactionintercept + Condition, data = pvrdata)
anova(LS12mod1, LS12mod2)
#p = 0.05753
Comp12mod1 = lm(Competenceintvslope ~ Competenceintercept, data = pvrdata)
Comp12mod2 = lm(Competenceintvslope ~ Competenceintercept + Condition, data = pvrdata)
anova(Comp12mod1, Comp12mod2)
#p = 0.9622
Con12mod1 = lm(Connectednessintvslope ~ Connectednessintercept, data = pvrdata)
Con12mod2 = lm(Connectednessintvslope ~ Connectednessintercept + Condition, data = pvrdata)
anova(Con12mod1, Con12mod2)
#p = 0.05603
Aut12mod1 = lm(Autonomyintvslope ~ Autonomyintercept, data = pvrdata)
Aut12mod2 = lm(Autonomyintvslope ~ Autonomyintercept + Condition, data = pvrdata)
anova(Aut12mod1, Aut12mod2)
#p = 0.05738
# Pairwise Comparison 2 (Every experimental to control Days 1-2) ---------------------------------------------------
# Importing and cleaning data ---------------------------------------------
setwd("/Users/davidko/Desktop/Recall Study")
# Importing day 1 data from qualtrics with lines 1 & 3 removed
day1data = read.csv("day1data.csv", header=T, na.strings = "", 
                    stringsAsFactors = F, strip.white = T)
# Removing needless columns
colnames(day1data)
day1data=day1data[,-c(3:4,7:21,30:33,46:49,55:58,77:80)]
# Name columns
colnames(day1data)[1:12]=c("StartDate1.1", "EndDate1.1", "Progress1.1", "Duration1.1",
                           "FirstName1.1", "LastName1.1", "Email1.1", "Age1.1", "Sex1.1",
                           "Ethnicity1.1", "Other1.1", "ParentsEducation1.1")
colnames(day1data)[13:24]=c("PositiveAffect1.1", "NegativeAffect1.1", "PositiveAffect2.1",
                            "NegativeAffect2.1","NegativeAffect3.1","NegativeAffect4.1",
                            "PositiveAffect3.1", "NegativeAffect5.1","PositiveAffect4.1",
                            "PositiveAffect5.1","NegativeAffect6.1","PositiveAffect6.1")
colnames(day1data)[25:29] = paste0("LifeSatisfaction", 1:5, ".1")
colnames(day1data)[30:35] = paste0("Connectedness", 1:6, ".1")
colnames(day1data)[36:41] = paste0("Competence", 1:6, ".1")
colnames(day1data)[42:47] = paste0("Autonomy", 1:6, ".1")
colnames(day1data)[48]=c("Condition1.1")
# Setting correct data types
# str(day1data) <--use this to view structure
day1data$StartDate1.1=as.POSIXct(day1data$StartDate1.1, format="%m/%d/%Y %H:%M")
day1data$EndDate1.1=as.POSIXct(day1data$EndDate1.1, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day1data$FullName = paste0(day1data$FirstName1.1, day1data$LastName1.1)
day1data$Duplicate1.1 = as.numeric(duplicated(day1data$FullName) | duplicated(day1data$FullName, fromLast = T))
day1data$MinProgress1.1 = 0
day1data$MinProgress1.1[day1data$Progress1.1 == min(day1data$Progress1.1)] = 1
# plyr::count(day1data$Progress1.1)
day1data$MinProgress1.1=F
day1data$MinProgress1.1[day1data$Progress1.1==min(day1data$Progress1.1)]=T
# Subsetting data
day1data=day1data[day1data$Duplicate1.1==F & day1data$MinProgress1.1==F,]
# Importing day 2 data from qualtrics with lines 1 & 3 removed ------------
day2data = read.csv("day2data.csv", header=T, na.strings = "",
                    stringsAsFactors = F, strip.white = T)
#Removing needless columns
colnames(day2data)
day2data=day2data[,-c(3:4,7:9,13:17, 19:22, 35:38, 44:47, 66:69)]
#Name columns
colnames(day2data)[1:7]=c("StartDate1.2", "EndDate1.2", "Progress1.2", "Duration1.2",
                          "LastName1.2", "FirstName1.2", "Email1.2")
colnames(day2data)[8]=c("Recall1.2")
colnames(day2data)[9:20]=c("PositiveAffect1.2", "NegativeAffect1.2", "PositiveAffect2.2",
                           "NegativeAffect2.2","NegativeAffect3.2","NegativeAffect4.2",
                           "PositiveAffect3.2", "NegativeAffect5.2","PositiveAffect4.2",
                           "PositiveAffect5.2","NegativeAffect6.2","PositiveAffect6.2")
colnames(day2data)[21:25] = paste0("LifeSatisfaction", 1:5, ".2")
colnames(day2data)[26:31] = paste0("Connectedness", 1:6, ".2")
colnames(day2data)[32:37] = paste0("Competence", 1:6, ".2")
colnames(day2data)[38:43] = paste0("Autonomy", 1:6, ".2")
colnames(day2data)[44]=c("Condition1.2")
# Setting correct data types
day2data$StartDate1.2=as.POSIXct(day2data$StartDate1.2, format="%m/%d/%Y %H:%M")
day2data$EndDate1.2=as.POSIXct(day2data$EndDate1.2, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day2data$FullName = paste0(day2data$FirstName1.2, day2data$LastName1.2)
day2data$Duplicate1.2 = as.numeric(duplicated(day2data$FullName) | duplicated(day2data$FullName, fromLast = T))
day2data$MinProgress1.2 = 0
day2data$MinProgress1.2[day2data$Progress1.2 == min(day2data$Progress1.2)] = 1
#Subsetting data
day2data=day2data[day2data$Duplicate1.2==F & day2data$MinProgress1.2==F,]

# Importing day 3 data from qualtrics with lines 1 & 3 removed ------------
day3data = read.csv("day3data.csv", header=T, na.strings = "",
                    stringsAsFactors = F, strip.white = T)
#Removing needless columns
colnames(day3data)
day3data=day3data[,-c(3:4,7:9,13:17, 30:33, 39:42, 61:64, 66:69, 71:74,76)]
#Name columns
colnames(day3data)[1:7]=c("StartDate1.3", "EndDate1.3", "Progress1.3", "Duration1.3",
                          "LastName1.3", "FirstName1.3", "Email1.3")
colnames(day3data)[8:19]=c("PositiveAffect1.3", "NegativeAffect1.3", "PositiveAffect2.3",
                           "NegativeAffect2.3","NegativeAffect3.3","NegativeAffect4.3",
                           "PositiveAffect3.3", "NegativeAffect5.3","PositiveAffect4.3",
                           "PositiveAffect5.3","NegativeAffect6.3","PositiveAffect6.3")
colnames(day3data)[20:24] = paste0("LifeSatisfaction", 1:5, ".3")
colnames(day3data)[25:30] = paste0("Connectedness", 1:6, ".3")
colnames(day3data)[31:36] = paste0("Competence", 1:6, ".3")
colnames(day3data)[37:42] = paste0("Autonomy", 1:6, ".3")
colnames(day3data)[43:44] = c("PerformManipulationCheck1.1", "RecallManipulationCheck1.1")
colnames(day3data)[45]=c("Condition1.3")
# Setting correct data types
day3data$StartDate1.3=as.POSIXct(day3data$StartDate1.3, format="%m/%d/%Y %H:%M")
day3data$EndDate1.3=as.POSIXct(day3data$EndDate1.3, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day3data$FullName = paste0(day3data$FirstName1.3, day3data$LastName1.3)
day3data$Duplicate1.3 = as.numeric(duplicated(day3data$FullName) | duplicated(day3data$FullName, fromLast = T))
day3data$MinProgress1.3 = 0
day3data$MinProgress1.3[day3data$Progress1.3 == min(day3data$Progress1.3)] = 1
#Subsetting data
day3data=day3data[day3data$Duplicate1.3==F & day3data$MinProgress1.3==F,]

# Merging data ------------------------------------------------------------

recdata=merge(day1data,day2data,by="FullName",all=T,sort=F)
recdata=merge(recdata,day3data,by="FullName",all=T,sort=F)
#Remove entries not done on an acceptable date
recdata$Days1.2=round(recdata$EndDate1.2,units="days") - round(recdata$EndDate1.1,units="days")
recdata$Days1.3=round(recdata$EndDate1.3,units="days") - round(recdata$EndDate1.2,units="days")
recdata[(recdata$Days1.2 > 2 | is.na(recdata$Days1.2)==T), grep(".2$",colnames(recdata))]=NA
recdata[(recdata$Days1.3 > 2 | is.na(recdata$Days1.3)==T), grep(".3$",colnames(recdata))]=NA
#removing people not assigned a condition at time 1 (didn't finish day 1)
recdata$Condition=recdata$Condition1.1
recdata=recdata[,-grep("Condition1", colnames(recdata))]
recdata=recdata[!is.na(recdata$Condition),]
#removing people who weren't paying attention
recdata$maxcons = 0
for (i in 1:nrow(recdata)) {
  recdata$maxcons[i] = max(rle(recdata[i,-grep("Date", colnames(recdata))])$lengths)
}
recdata=recdata[recdata$maxcons < 14,]
#manipulation check
plyr::count(recdata$RecallManipulationCheck1.1)
plyr::count(recdata$PerformManipulationCheck1.1)
recdata=recdata[recdata$RecallManipulationCheck1.1!=1 | 
                  is.na(recdata$RecallManipulationCheck1.1),]
recdata=recdata[recdata$PerformManipulationCheck1.1!=1 | 
                  is.na(recdata$PerformManipulationCheck1.1),]
#make condition code variables
recdata$ConditionCode1=1
recdata$ConditionCode1[recdata$Condition==3]=0
#reverse coding items
revnames=rep(c("Connectedness", "Competence", "Autonomy"), 3)
revnums=rep(c(2, 4, 6), each=3)
revs=paste0(revnames,revnums)
recdata[,grep(paste0("^", revs, collapse = "|"), colnames(recdata))]=8-
  recdata[,grep(paste0("^", revs, collapse = "|"), colnames(recdata))]

cor(recdata[,grepl("Connectedness", colnames(recdata)) & grepl("1$", colnames(recdata))]
    , use="pairwise.complete.obs")
#Asian and Latino dummy codes
recdata$Asian1.1 = 0
recdata$Asian1.1[recdata$Ethnicity1.1==2]=1
recdata$Latino1.1 = 0
recdata$Latino1.1[recdata$Ethnicity1.1==6]=1
#Renaming Conditions
recdata$Condition[recdata$Condition == "1"] <- "Perform"
recdata$Condition[recdata$Condition == "2"] <- "PerformAndRecall"
recdata$Condition[recdata$Condition == "3"] <- "Control"
recdata$Condition[recdata$Condition == "4"] <- "Recall"
# Long Models Function ----------------------------------------------------
longmodels = function(measures, dataset, revinfo = NULL, multidims = NULL, cutvaritems = T, minitems = 3,
                      timespace = "Week", knot = NULL, growthshape = "line", growthshape2 = "line", mods = NULL,
                      tvcovs = NULL, overridesolgmeqs = NULL, write = T, updates = T, controlflwforintv = T) {
  require(psych)
  require(lavaan)
  require(ggplot2)
  require(stringr)
  numcondcodes = length(grep("ConditionCode", colnames(dataset)))
  #reverse scoring
  if(is.null(revinfo) == F) {
    for (measure in 1:length(revinfo)) {
      revmeasinfo = revinfo[[measure]]
      revmeasname = revmeasinfo[1]
      revnums = as.numeric(revmeasinfo[2:length(revmeasinfo)])
      revnums = grep(paste0(revnums, collapse = "|"), 
                     gsub(revmeasname, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",revmeasname), colnames(dataset))])))
      revmeasdata = dataset[,grep(paste0("^",revmeasname), colnames(dataset))]
      revmeasmax = max(revmeasdata, na.rm = T)
      revmeasmin = min(revmeasdata, na.rm = T)
      revmeasdata[,revnums] = (revmeasmax-revmeasmin+2)-revmeasdata[,revnums]
      dataset = dataset[,-grep(paste0("^",revmeasname), colnames(dataset))]
      dataset = cbind(dataset, revmeasdata)
    }
  }
  #creating parcels for multdimensional constructs
  if(is.null(multidims) == F) {
    for (multidim in names(multidims)){
      timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",multidims[multidim][[1]][1]), colnames(dataset))]))
      facets = multidims[multidim][[1]]
      for (timepoint in timepoints) {
        for (facet in facets) {
          ncols = ncol(dataset)
          dataset[,ncols+1] = rowMeans(dataset[grepl(facet, colnames(dataset)) & grepl(paste0("\\.",timepoint), colnames(dataset))], na.rm = T)
          colnames(dataset)[ncols+1] = paste0(multidim, grep(facet, facets),".", timepoint)
        }
      }
    }
  }
  origdata = dataset
  #measurement invariance
  chgcfis = data.frame(matrix(ncol = 3, nrow = 0))
  measinveqs = data.frame(matrix(ncol = 4, nrow = 0))
  configuralmodel = NULL
  weakmodel = NULL
  measinvmodels = list()
  chgcfisvect = NULL
  heywoodlog = NULL
  #create function to test measurement invariance
  measinvtest = function(measure) {
    chgcfisvect <<- NULL 
    prevcfi = NULL
    measinveqsvect = NULL
    timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))
    itemnums = unique(gsub(measure, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))
    for (invtype in c("configural", "weak", "strong", "strict")) {
      #bulding lavaan equations
      spemeasinveqs = NULL
      if (invtype == "configural") {
        #defining latent variables with unequal loadings (configural)
        for (timepoint in timepoints) {
          if(length(itemnums) == 2) {
            nextline = paste0(measure,".",timepoint," =~ ", paste0("a", timepoint, "*",measure,itemnums,".", timepoint, collapse = "+"), "\n")
            spemeasinveqs = paste0(spemeasinveqs, nextline)
          }else {
            nextline = paste0(measure,".",timepoint," =~ ", paste0(measure,itemnums,".", timepoint, collapse = "+"), "\n")
            spemeasinveqs = paste0(spemeasinveqs, nextline)
          }
        }
      } else {
        #defining latenet variables with equal loadings (weak)
        for (timepoint in timepoints) {
          if(length(itemnums) == 2) {
            nextline = paste0(measure,".",timepoint," =~ ", paste0("a*",measure,itemnums,".", timepoint, collapse = "+"), "\n")
          } else {
            lvname = paste0(measure,".",timepoint," =~ ")
            itemeqs = NULL
            counter = 1
            for (itemnum in itemnums) {
              itemeqs = paste0(itemeqs, paste0("a",counter,"*",measure,itemnum,".", timepoint, "+"))
              counter = counter+1
            }
            nextline = paste0(lvname, itemeqs, "\n")
          }
          spemeasinveqs = paste0(spemeasinveqs, nextline)
        }
      }
      #constraing intercepts
      if (grepl(paste0(c("strong","strict"), collapse="|"), invtype) == T) {
        for (timepoint in timepoints) {
          counter = 1
          for (itemnum in itemnums) {
            nextline = paste0(measure,itemnum,".",timepoint,"~b",counter,"*1","\n")
            spemeasinveqs = paste0(spemeasinveqs, nextline)
            counter = counter+1
          }
        }
      }
      #constraining residuals
      if (invtype == "strict") {
        for (timepoint in timepoints) {
          counter = 1
          for (itemnum in itemnums) {
            nextline = paste0(measure,itemnum,".",timepoint,"~~c",counter,"*",measure,itemnum,".",timepoint,"\n")
            spemeasinveqs = paste0(spemeasinveqs, nextline)
            counter = counter+1
          }
        }
      }
      #correlating residuals
      for (itemnum in itemnums) {
        for (timepoint in timepoints) {
          durations = as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))) - as.numeric(timepoint)
          durations = durations[durations>0]
          for (duration in durations) {
            nextline = paste0(measure,itemnum,".",timepoint,"~~d",
                              duration+((as.numeric(itemnum)-1)*(max(as.numeric(timepoints))-min(as.numeric(timepoints)))),
                              "*",measure,itemnum,".",as.numeric(timepoint)+duration,"\n")
            spemeasinveqs = paste0(spemeasinveqs, nextline)
          }
        }
      }
      spemeasinveqs = gsub("\\+\n", "\n", spemeasinveqs)
      measinveqsvect = c(measinveqsvect, spemeasinveqs)
      continue = 1
      while (continue == 1) {
        if (invtype == "configural") {
          measinvmodel = lavaan(spemeasinveqs, dataset, missing = "fiml", int.ov.free = T, int.lv.free = F,
                                auto.fix.first = T, auto.var = T, auto.cov.lv.x = T)
        } else {
          measinvmodel = lavaan(spemeasinveqs, dataset, missing = "fiml", int.ov.free = T, int.lv.free = T,
                                auto.fix.first = T, auto.var = T, auto.cov.lv.x = T) 
        }
        sink("semmodels.txt", append = T)
        summary(measinvmodel, fit.measures = T, standardized = T)
        sink()
        parest = parameterEstimates(measinvmodel)
        variances = parest[parest$op == "~~" & parest$lhs == parest$rhs,]
        heywooditems = variances$lhs[variances$est.std < 0]
        if (length(heywooditems) != 0) {
          heywoodlog = c(heywoodlog, paste0(measure, invtype, heywooditems))
          if (invtype == "strict") {
            for (heywooditem in heywooditems) {
              if (length(grep(heywooditem, parest$lhs[grep("=~",parest$op)])) > 0) {
                heywoodfix = paste0(heywooditem, "~~.001*", heywooditem, "\n")
                spemeasinveqs = paste0(spemeasinveqs, heywoodfix)
              } else {
                cnum = str_match(spemeasinveqs, paste0("\n",heywooditem,"~~c(.*?)\\*",heywooditem,"\n"))[,2]
                heywoodfix = paste0("c",cnum,"==.001\n")
                spemeasinveqs = paste0(spemeasinveqs, heywoodfix)
              }
            }
          } else {
            heywoodfix = paste0(paste0(heywooditems, "~~.001*", heywooditems, collapse = "\n"), "\n")
            spemeasinveqs = paste0(spemeasinveqs, heywoodfix)
          }
        } else {continue = 0}
      }
      measinvmodels[[paste0(measure, invtype, "model")]] <<- measinvmodel
      if (invtype != "configural") {
        chgcfi = prevcfi - fitmeasures(measinvmodel, "cfi")
        chgcfisvect <<- c(chgcfisvect,chgcfi)
      }
      prevcfi = fitmeasures(measinvmodel, "cfi")
      if (invtype == "configural") {
        configuralmodel <<- measinvmodel
      }
      if (invtype == "weak") {
        weakmodel <<- measinvmodel
      }
    }
    chgcfis <<- rbind(chgcfis, chgcfisvect)
    colnames(chgcfis) <<- c("Weak", "Strong", "Strict")
    rownames(chgcfis)[nrow(chgcfis)] <<- measure
    measinveqs <<- rbind(measinveqs, measinveqsvect, stringsAsFactors = F)
    colnames(measinveqs) <<- c("configural", "weak", "strong", "strict")
    if ((length(grep(measure, rownames(measinveqs))) > 0) & (nrow(measinveqs) > 0))  {measinveqs <<- measinveqs[-nrow(measinveqs),]}
    rownames(measinveqs)[nrow(measinveqs)] <<- measure
  }
  cutitems = NULL
  for (measure in c(measures, tvcovs)) {
    if (length(unique(gsub(measure, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))) > 1) {
      #do measinvtest for each measure and then redo based on chgcfis
      measinvtest(measure)
      minitemshit = 0
      tempcutitems = NULL
      if (cutvaritems == T) {
        #check for weak invariance and if not, delete items based on variance in loadings
        while (chgcfisvect[1] > .01) {
          stdsol = standardizedsolution(configuralmodel)
          loadings = stdsol[stdsol$op == "=~", 3:4]
          timepoints = length(unique(gsub(".*\\.","", loadings[,1])))
          loadingmat = matrix(loadings[,2], ncol = timepoints)
          loadingmat = cbind(loadingmat, NA)
          loadingmat[,ncol(loadingmat)] = apply(loadingmat,1,sd, na.rm = T)
          itemnums = gsub(paste0(measure,"|\\..*"), "",  loadings[1:(nrow(loadings)/timepoints),1])
          rownames(loadingmat) = itemnums
          item2remove = rownames(loadingmat)[grep(max(loadingmat[,ncol(loadingmat)]), loadingmat[,ncol(loadingmat)])]
          tempcutitems = c(tempcutitems, paste0(measure, item2remove))
          dataset = dataset[,-grep(paste0(measure, item2remove), colnames(dataset))]
          datasetcols = colnames(dataset)
          measurecols = datasetcols[grep(measure, datasetcols)]
          if (length(unique(gsub(paste0(measure,"|\\..*"), "", measurecols))) < minitems) {minitemshit=1;break}
          chgcfis = chgcfis[-grep(measure,rownames(chgcfis)),]
          measinveqs = measinveqs[-grep(measure,rownames(measinveqs)),]
          measinvtest(measure)
        }
        #check for strong invariance and if not, delete items based on variance in intercepts
        if (sum(grepl(measure, tvcovs)) == 0) {
          while (chgcfisvect[2] > .01) {
            stdsol = standardizedsolution(weakmodel)
            intercepts = stdsol[stdsol$op == "~1", c(1,4)]
            timepoints = length(unique(gsub(".*\\.","", intercepts[,1])))
            intercepts = intercepts[1:(nrow(intercepts)-timepoints),]
            interceptsmat = matrix(intercepts[,2], ncol = timepoints)
            interceptsmat = cbind(interceptsmat, NA)
            interceptsmat[,ncol(interceptsmat)] = apply(interceptsmat,1,sd, na.rm = T)
            itemnums = gsub(paste0(measure,"|\\..*"), "",  intercepts[1:(nrow(intercepts)/timepoints),1])
            rownames(interceptsmat) = itemnums
            item2remove = rownames(interceptsmat)[grep(max(interceptsmat[,ncol(interceptsmat)]), interceptsmat[,ncol(interceptsmat)])]
            tempcutitems = c(tempcutitems, paste0(measure, item2remove))
            dataset = dataset[,-grep(paste0(measure, item2remove), colnames(dataset))]
            datasetcols = colnames(dataset)
            measurecols = datasetcols[grep(measure, datasetcols)]
            if (length(unique(gsub(paste0(measure,"|\\..*"), "", measurecols))) < minitems) {minitemshit=1;break}
            chgcfis = chgcfis[-grep(measure,rownames(chgcfis)),]
            measinveqs = measinveqs[-grep(measure,rownames(measinveqs)),]
            measinvtest(measure)
          }
        }
        #if hit min item threshold, just use all original items
        if (minitemshit == 1) {
          tempcutitems = NULL
          dataset = dataset[,!grepl(paste0("^",measure), colnames(dataset))]
          temp2data = origdata[,grepl(paste0("^",measure), colnames(origdata))]
          dataset = cbind(dataset, temp2data)
          chgcfis = chgcfis[-grep(measure, rownames(chgcfis)),]
          measinveqs = measinveqs[-grep(measure, rownames(measinveqs)),]
          measinvtest(measure)
        }
      }
      cutitems = c(cutitems, tempcutitems)
      if (updates == T) {message("Measurement invariance testing of ", gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure), " is complete")}
    }
  }
  #alphas and omegas
  alphas = NULL
  omegas = NULL
  for (measure in c(union(measures, mods),tvcovs)) {
    timepoints = suppressWarnings(as.numeric(unique(gsub(".*\\.", "", colnames(dataset)))))
    timepoints = timepoints[!is.na(timepoints)]
    timepoints/10
    for (timepoint in timepoints) {
      cols = grepl(gsub("0", "", paste0(timepoint,"$")),colnames(dataset)) & grepl(paste0("^",measure), colnames(dataset))
      if (sum(cols) > 1) {
        rel = suppressMessages(suppressWarnings(omega(dataset[,cols], 1, fm = "ml", plot = F)))
        alpha = rel$alpha
        omega = rel$omega.tot
      } else {
        alpha = NA
        omega = NA
      }
      alphas = c(alphas, alpha)
      omegas = c(omegas, omega)
    }
  }
  alphas = as.data.frame(t(matrix(alphas, nrow = length(timepoints))))
  colnames(alphas) = paste(timespace, timepoints)
  rownames(alphas) = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",c(union(measures, mods),tvcovs))
  if (write == T) {write.csv(alphas, "alphas.csv")}
  omegas = as.data.frame(t(matrix(omegas, nrow = length(timepoints))))
  colnames(omegas) = paste(timespace, timepoints)
  rownames(omegas) = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",c(union(measures, mods),tvcovs))
  if (write == T) {write.csv(omegas, "omegas.csv")}
  if (updates == T) {message("Alphas and omegas have been computed")}
  #plots based on latent variable scores
  for (measure in measures) {
    timepoints = as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))
    itemnums = unique(gsub(measure, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))
    if (length(itemnums) == 1) {
      folgmdata = dataset[,grep(paste0(paste0("^", measure),"|Condition$"),colnames(dataset))]
      tempfolgmdata1 = folgmdata[,grep("Condition$", colnames(folgmdata))]
      tempfolgmdata2 = folgmdata[,-grep("Condition$", colnames(folgmdata))]
      folgmdata = cbind(tempfolgmdata1, tempfolgmdata2)
      colnames(folgmdata)[1] = "Condition"
      folgmdata[,2:ncol(folgmdata)] = (folgmdata[,2:ncol(folgmdata)] - mean(folgmdata[,2], na.rm = T))/sd(folgmdata[,2], na.rm = T)
      lvdescs = describeBy(folgmdata[,2:ncol(folgmdata)], folgmdata$Condition, mat = T)
      lvdescs$timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))
      lvdescs$timepoints = as.numeric(sort(lvdescs$timepoints))
    } else {
      if (chgcfis[measure,3] < .01) {
        lvploteqs = paste0(measinveqs[measure, 4], measure, ".", min(timepoints, na.rm = T),"~~1*",measure, ".", min(timepoints, na.rm = T),"\n")
      } else {
        lvploteqs = paste0(measinveqs[measure, 3], measure, ".", min(timepoints, na.rm = T),"~~1*",measure, ".", min(timepoints, na.rm = T),"\n")
      }
      lvploteqs = paste0(lvploteqs, measure, ".", min(timepoints, na.rm = T),"~0*1")
      continue = 1
      while (continue == 1) {
        lvplotmodel = lavaan(lvploteqs, dataset, missing = "fiml", int.ov.free = T, int.lv.free = T,
                             auto.fix.first = F, auto.var = T, auto.cov.lv.x = T)
        sink("semmodels.txt", append = T)
        summary(lvplotmodel, fit.measures = T, standardized = T)
        sink()
        parest = parameterEstimates(lvplotmodel)
        variances = parest[parest$op == "~~" & parest$lhs == parest$rhs,]
        heywooditems = variances$lhs[variances$est.std < 0]
        if (length(heywooditems) != 0) {
          heywoodlog = c(heywoodlog, paste0(measure, "lvplot", heywooditems))
          if (chgcfis[measure,3] < .01) {
            for (heywooditem in heywooditems) {
              if (length(grep(heywooditem, parest$lhs[grep("=~",parest$op)])) > 0) {
                heywoodfix = paste0(heywooditem, "~~.001*", heywooditem, "\n")
                lvploteqs = paste0(lvploteqs, heywoodfix)
              } else {
                cnum = str_match(lvploteqs, paste0("\n",heywooditem,"~~c(.*?)\\*",heywooditem,"\n"))[,2]
                heywoodfix = paste0("c",cnum,"==.001\n")
                lvploteqs = paste0(lvploteqs, heywoodfix)
              }
            }
          } else {
            heywoodfix = paste0(paste0(heywooditems, "~~.001*", heywooditems, collapse = "\n"), "\n")
            lvploteqs = paste0(lvploteqs, heywoodfix)
          }
        } else {continue = 0}
      }
      lvplotscores = lavPredict(lvplotmodel)
      lvplotscores = data.frame(lvplotscores, dataset$Condition)
      for (colnum in 1:(ncol(lvplotscores)-1)) {
        lvplotscores[,colnum] = as.numeric(lvplotscores[,colnum])
      }
      lvdescs = describeBy(lvplotscores, lvplotscores[,ncol(lvplotscores)], mat = T)
      lvdescs = lvdescs[1:(nrow(lvdescs)-(length(unique(dataset$Condition)))),]
      lvdescs$timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))
      lvdescs$timepoints = as.numeric(sort(lvdescs$timepoints))
      lvdescs$group1 = gsub("([[:lower:]])([[:upper:]])", "\\1\n\\2",lvdescs$group1)
    }
    if(write == T) {
      tiff(paste0(measure, "LVplot.tiff"), width = 1600, height = 1200, bg = "transparent")
      lvplot = ggplot(data=lvdescs, aes(x=timepoints, y=mean, group=group1, colour=group1)) +
        geom_line(size = 5) +
        geom_point() +
        geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=1, size = 2, position=position_dodge(0)) +
        labs(x = timespace, y = measure) + #
        scale_colour_discrete(name="Condition\n",
                              labels= paste0("\n", gsub(" ", "\n", unique(lvdescs$group1)), "\n")) +
        theme(legend.background = element_rect(color = "black", linetype = "solid", fill = "transparent"),
              legend.text = element_text(size=48), legend.title = element_text(size=48),
              panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA), legend.key = element_blank(),
              axis.text.x = element_text(size = 40), axis.text.y = element_text(size = 40),
              axis.title.x = element_text(size = 48), axis.title.y = element_text(size = 48),
              plot.background = element_rect(fill = "transparent",colour = NA)) +
        guides(color = guide_legend(override.aes = list(size = 15))) + 
        scale_x_continuous(breaks = as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))),
                           labels = as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))) +
        scale_y_continuous(name = paste0(gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure), " (",timespace," ",min(timepoints, na.rm = T), " SDs)"),minor_breaks = 0)
      print(lvplot)
      dev.off()
    }
  }
  if (updates == T) {message("Figures have been created")}
  # Second-order latent growth models
  solgmfits = data.frame(matrix(ncol = 8, nrow = 0))
  solgmlvscores = data.frame(matrix(ncol = 0, nrow = nrow(dataset)))
  solgmeqs = data.frame(matrix(ncol = 1, nrow = 0))
  solgmmodels = list()
  measureadds = NULL
  for (measure in measures) {
    if (is.null(overridesolgmeqs) == F) {
      if(nchar(overridesolgmeqs[measure,])>0) {
        spesolgmeqs = overrridesolgmeqs[measure,]
      }
    } else {
      itemnums = unique(gsub(measure, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))])))
      timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))
      if (length(itemnums) == 1) {
        folgmdata = dataset[,grep(paste0("^", measure),colnames(dataset))]
        folgmdata = (folgmdata - mean(folgmdata[,1], na.rm = T))/sd(folgmdata[,1], na.rm = T)
        spefolgmeqs = paste0(measure,"intercept =~ ", paste0("1*",measure,"1.", timepoints, collapse = "+"), "\n")
        spefolgmeqs = paste0(spefolgmeqs, measure, "intercept ~ 0*1", "\n")
        spefolgmeqs = paste0(spefolgmeqs, measure, "intercept ~~ 1*", measure, "intercept", "\n")
        if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
          spefolgmeqs = paste0(spefolgmeqs, measure, "intvslope ~~ 0*", measure, "intvslope", "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "intvslopedist =~ 1*", measure, "intvslope", "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "intvslopedist ~ 1", "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "intvslope ~ ", measure, "intercept", "\n")
        }
        timepointsminus1 = as.numeric(timepoints) - 1
        if (is.null(knot) == T | (suppressWarnings(max(as.numeric(timepoints), na.rm = T)) <= ifelse(is.null(knot),0,knot))) {
          if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spefolgmeqs = paste0(spefolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",measure,"1.", timepoints, collapse = "+"), "\n")
          }
        } else {
          timepointsminus1[timepointsminus1 > (knot-1)] = knot-1
          if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
          timepointnums = as.numeric(timepoints)
          timepointnums[timepointnums <= knot] = 0
          timepointnums[timepointnums > knot] = timepointnums[timepointnums > knot] - knot
          if (growthshape2 != "line") {timepointnums = eval(call(growthshape2, (timepointnums)))}
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spefolgmeqs = paste0(spefolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",measure,"1.", timepoints, collapse = "+"), "\n")
          }
          spefolgmeqs = paste0(spefolgmeqs, measure, "flwslope =~ ", paste0(timepointnums, "*",measure,"1.", timepoints, collapse = "+"), "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "flwslope ~~ 0*", measure, "flwslope", "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "flwslopedist =~ 1*", measure, "flwslope", "\n")
          spefolgmeqs = paste0(spefolgmeqs, measure, "flwslopedist ~ 1", "\n")
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spefolgmeqs = paste0(spefolgmeqs, measure, "flwslope ~ ", measure, "intercept\n")
          } else {
            spefolgmeqs = paste0(spefolgmeqs, measure, "flwslope ~~ ", measure, "intercept\n")
          }
          if (controlflwforintv == F) {
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spefolgmeqs = paste0(spefolgmeqs, measure, "flwslopedist ~~", measure, "intvslopedist\n")
            }
          } else {
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spefolgmeqs = paste0(spefolgmeqs, measure, "flwslope ~", measure, "intvslope\n")
            }
          }
        }
        a = 1
        if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {a=0}
        if ((!is.null(knot)) + 2 == (length(timepoints)+a)) {
          spefolgmeqs = paste0(spefolgmeqs, paste0(measure, "1.",timepoints, "~~0*", measure, "1.",timepoints, collapse = "\n"), "\n")
        }
        continue = 1
        while (continue == 1) {
          folgmmodel = lavaan(spefolgmeqs, folgmdata, missing = "fiml", int.lv.free = F,
                              auto.fix.first = F, auto.var = T, auto.cov.lv.x = F)
          sink("semmodels.txt", append = T)
          summary(folgmmodel, fit.measures = T, standardized = T)
          sink()
          parest = parameterEstimates(folgmmodel)
          variances = parest[parest$op == "~~" & parest$lhs == parest$rhs,]
          heywooditems = variances$lhs[variances$est < 0]
          if (length(heywooditems) != 0) {
            heywoodlog = c(heywoodlog, paste0(measure, "folgm", heywooditems))
            heywoodfix = paste0(paste0(heywooditems, "~~.001*", heywooditems, collapse = "\n"), "\n")
            spefolgmeqs = paste0(spefolgmeqs, heywoodfix)
          } else {continue = 0}
        }
        folgmmodelscores = as.data.frame(lavPredict(folgmmodel))
        folgmmodelscores = folgmmodelscores[, grepl("intercept|slope", colnames(folgmmodelscores))]
        solgmlvscores = cbind(solgmlvscores, folgmmodelscores)
        solgmfits = rbind.data.frame(solgmfits, fitmeasures(folgmmodel, c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower",
                                                                          "rmsea.ci.upper", "srmr")))
        colnames(solgmfits) = c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
        rownames(solgmfits)[nrow(solgmfits)] = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure)
        solgmeqs = rbind(solgmeqs, spefolgmeqs)
        solgmeqs[,1] = as.character(solgmeqs[,1])
        colnames(solgmeqs) = "solgmeqs"
        rownames(solgmeqs)[nrow(solgmeqs)] = measure
        solgmmodels[[paste0(measure, "folgmmodel")]] = folgmmodel
        if (updates == T) {message("First-order latent growth model of ", gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure), " has been created")}
      } else {
        if (chgcfis[measure,3] < .01) {
          spesolgmeqs = measinveqs[measure, 4]
        } else {
          spesolgmeqs = measinveqs[measure, 3]
        }
        spesolgmeqs = paste0(spesolgmeqs, measure, "intercept =~ ", paste0("1*",measure,".", timepoints, collapse = "+"), "\n")
        spesolgmeqs = paste0(spesolgmeqs, measure, "intercept ~ 0*1", "\n")
        spesolgmeqs = paste0(spesolgmeqs, measure, "intercept ~~ 1*", measure, "intercept", "\n")
        if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
          spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope ~~ 0*", measure, "intvslope", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intvslopedist =~ 1*", measure, "intvslope", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intvslopedist ~ 1", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope ~ ", measure, "intercept", "\n")
        }
        timepointsminus1 = as.numeric(timepoints) - 1
        if (is.null(knot) == T) {
          if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",measure,".", timepoints, collapse = "+"), "\n")
          }
        } else {
          timepointsminus1[timepointsminus1 > (knot-1)] = knot-1
          if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
          timepointnums = as.numeric(timepoints)
          timepointnums[timepointnums <= knot] = 0
          timepointnums[timepointnums > knot] = timepointnums[timepointnums > knot] - knot
          if (growthshape2 != "line") {timepointnums = eval(call(growthshape2, (timepointnums)))}
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",measure,".", timepoints, collapse = "+"), "\n")
          }
          spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope =~ ", paste0(timepointnums, "*",measure,".", timepoints, collapse = "+"), "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~~ 0*", measure, "flwslope", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist =~ 1*", measure, "flwslope", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist ~ 1", "\n")
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~ ", measure, "intercept\n")
          } else {
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~~ ", measure, "intercept\n")
          }
          if (controlflwforintv == F) {
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist ~~", measure, "intvslopedist\n")
            }
          } else {
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~", measure, "intvslope\n")
            }
          }
        }
        a = 1
        if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {a=0}
        if ((!is.null(knot)) + 2 == (length(timepoints)+a)) {
          spesolgmeqs = paste0(spesolgmeqs, paste0(measure, ".",timepoints, "~~0*", measure, ".",timepoints, collapse = "\n"), "\n")
        }
        continue = 1
        while (continue == 1) {
          solgmmodel = lavaan(spesolgmeqs, dataset, missing = "fiml", int.ov.free = T,
                              auto.fix.first = F, auto.var = T, auto.cov.lv.x = F)
          sink("semmodels.txt", append = T)
          summary(solgmmodel, fit.measures = T, standardized = T)
          sink()
          parest = parameterEstimates(solgmmodel)
          variances = parest[parest$op == "~~" & parest$lhs == parest$rhs,]
          heywooditems = variances$lhs[variances$est.std < 0]
          if (length(heywooditems) != 0) {
            heywoodlog = c(heywoodlog, paste0(measure, "solgm", heywooditems))
            if (chgcfis[measure,3] < .01) {
              for (heywooditem in heywooditems) {
                if (length(grep(heywooditem, parest$lhs[grep("=~",parest$op)])) > 0) {
                  heywoodfix = paste0(heywooditem, "~~.001*", heywooditem, "\n")
                  spesolgmeqs = paste0(spesolgmeqs, heywoodfix)
                } else {
                  cnum = str_match(spesolgmeqs, paste0("\n",heywooditem,"~~c(.*?)\\*",heywooditem,"\n"))[,2]
                  heywoodfix = paste0("c",cnum,"==.001\n")
                  spesolgmeqs = paste0(spesolgmeqs, heywoodfix)
                }
              }
            } else {
              heywoodfix = paste0(paste0(heywooditems, "~~.001*", heywooditems, collapse = "\n"), "\n")
              spesolgmeqs = paste0(spesolgmeqs, heywoodfix)
            }
          } else {continue = 0}
        }
        solgmmodelscores = as.data.frame(lavPredict(solgmmodel))
        solgmmodelscores = solgmmodelscores[, grepl("intercept|slope", colnames(solgmmodelscores))]
        solgmlvscores = cbind(solgmlvscores, solgmmodelscores)
        solgmfits = rbind.data.frame(solgmfits, fitmeasures(solgmmodel, c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower",
                                                                          "rmsea.ci.upper", "srmr")))
        colnames(solgmfits) = c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
        rownames(solgmfits)[nrow(solgmfits)] = measure
        solgmeqs = rbind(solgmeqs, spesolgmeqs)
        solgmeqs[,1] = as.character(solgmeqs[,1])
        colnames(solgmeqs) = "solgmeqs"
        rownames(solgmeqs)[nrow(solgmeqs)] = measure
        solgmmodels[[paste0(measure, "solgmmodel")]] = solgmmodel
        if (updates == T) {message("Second-order latent growth model of ", gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure), " has been created")}
        if (chgcfis[measure,2] > .01 | chgcfis[measure,1] > .01) {
          if (chgcfis[measure,1] < .01) {
            spesolgmeqs = measinveqs[measure, 2]
          } else {
            spesolgmeqs = measinveqs[measure, 1]
          }
          measure = paste0(measure, "2")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intercept =~ ", paste0("1*",substr(measure, 1, nchar(measure)-1),".", timepoints, collapse = "+"), "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intercept ~ 0*1", "\n")
          spesolgmeqs = paste0(spesolgmeqs, measure, "intercept ~~ 1*", measure, "intercept", "\n")
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope ~~ 0*", measure, "intvslope", "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslopedist =~ 1*", measure, "intvslope", "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslopedist ~ 1", "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope ~ ", measure, "intercept", "\n")
          }
          timepointsminus1 = as.numeric(timepoints) - 1
          if (is.null(knot) == T) {
            if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",substr(measure, 1, nchar(measure)-1),".", timepoints, collapse = "+"), "\n")
            }
          } else {
            timepointsminus1[timepointsminus1 > (knot-1)] = knot-1
            if (growthshape != "line") {timepointsminus1 = eval(call(growthshape, (timepointsminus1)))}
            timepointnums = as.numeric(timepoints)
            timepointnums[timepointnums <= knot] = 0
            timepointnums[timepointnums > knot] = timepointnums[timepointnums > knot] - knot
            if (growthshape2 != "line") {timepointnums = eval(call(growthshape2, (timepointnums)))}
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spesolgmeqs = paste0(spesolgmeqs, measure, "intvslope =~ ", paste0(timepointsminus1, "*",substr(measure, 1, nchar(measure)-1),".", timepoints, collapse = "+"), "\n")
            }
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope =~ ", paste0(timepointnums, "*",substr(measure, 1, nchar(measure)-1),".", timepoints, collapse = "+"), "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~~ 0*", measure, "flwslope", "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist =~ 1*", measure, "flwslope", "\n")
            spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist ~ 1", "\n")
            if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
              spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~ ", measure, "intercept\n")
            } else {
              spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~~ ", measure, "intercept\n")
            }
            if (controlflwforintv == F) {
              if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
                spesolgmeqs = paste0(spesolgmeqs, measure, "flwslopedist ~~", measure, "intvslopedist\n")
              }
            } else {
              if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {
                spesolgmeqs = paste0(spesolgmeqs, measure, "flwslope ~", measure, "intvslope\n")
              }
            }
          }
          a = 1
          if (is.null(knot) == T | (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < ifelse(is.null(knot),0,knot))) {a=0}
          if ((!is.null(knot)) + 2 == (length(timepoints)+a)) {
            spesolgmeqs = paste0(spesolgmeqs, paste0(substr(measure, 1, nchar(measure)-1), ".",timepoints, "~~0*", substr(measure, 1, nchar(measure)-1), ".",timepoints, collapse = "\n"), "\n")
          }
          if(chgcfis[substr(measure, 1, nchar(measure)-1),1] > .01) {
            tp1eqs = paste0(substr(measure, 1, nchar(measure)-1), ".1=~",paste0(substr(measure, 1, nchar(measure)-1), itemnums, ".1", collapse = "+"))
            tp1model = cfa(tp1eqs, dataset, missing = "fiml", std.lv = T)
            loading = parameterEstimates(tp1model)[1,4]
            spesolgmeqs = gsub(paste0("=~ ", substr(measure, 1, nchar(measure)-1), itemnums[1]), paste0("=~ ", loading,"*",substr(measure, 1, nchar(measure)-1), itemnums[1]), spesolgmeqs)
          }
          continue = 1
          while (continue == 1) {
            solgmmodel = lavaan(spesolgmeqs, dataset, missing = "fiml", int.ov.free = T,
                                auto.fix.first = F, auto.var = T, auto.cov.lv.x = F)
            sink("semmodels.txt", append = T)
            summary(solgmmodel, fit.measures = T, standardized = T)
            sink()
            parest = parameterEstimates(solgmmodel)
            variances = parest[parest$op == "~~" & parest$lhs == parest$rhs,]
            heywooditems = variances$lhs[variances$est.std < 0]
            if (length(heywooditems) != 0) {
              heywoodlog = c(heywoodlog, paste0(measure, "solgm", heywooditems))
              heywoodfix = paste0(paste0(heywooditems, "~~.001*", heywooditems, collapse = "\n"), "\n")
              spesolgmeqs = paste0(spesolgmeqs, heywoodfix)
            } else {continue = 0}
          }
          solgmmodelscores = as.data.frame(lavPredict(solgmmodel))
          solgmmodelscores = solgmmodelscores[, grepl("intercept|slope", colnames(solgmmodelscores))]
          solgmlvscores = cbind(solgmlvscores, solgmmodelscores)
          solgmfits = rbind.data.frame(solgmfits, fitmeasures(solgmmodel, c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower",
                                                                            "rmsea.ci.upper", "srmr")))
          colnames(solgmfits) = c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
          rownames(solgmfits)[nrow(solgmfits)] = measure
          solgmeqs = rbind(solgmeqs, spesolgmeqs)
          solgmeqs[,1] = as.character(solgmeqs[,1])
          colnames(solgmeqs) = "solgmeqs"
          rownames(solgmeqs)[nrow(solgmeqs)] = measure
          solgmmodels[[paste0(measure, "solgmmodel")]] = solgmmodel
          measureadds = c(measureadds, measure)
        }
      }
    }
  }
  dataset = cbind(dataset, solgmlvscores)
  measures = c(measures, measureadds)
  #condition effects
  if (numcondcodes > 0) {
    condeffects = as.data.frame(matrix(ncol = 5, nrow = 0))
    slopes = colnames(dataset)[grep("slope$", colnames(dataset))]
    intercepts = gsub("slope", "intercept", slopes)
    intercepts = gsub("intv", "", intercepts)
    intercepts = gsub("flw", "", intercepts)
    for (i in 1:length(slopes)) {
      slope = slopes[i]
      intercept = intercepts[i]
      condcontrol = intercept
      if (grepl("flw", slope) & controlflwforintv & (gsub("flw", "intv", slope) %in% slopes)) {
        condcontrol = paste0(condcontrol, "+", gsub("flw", "intv", slope))
      }
      specondeqs = paste0(slope, "~ ", condcontrol, "+", paste0("ConditionCode",1:numcondcodes, collapse = "+"))
      specondmodel = sem(specondeqs, dataset, missing = "fiml", fixed.x = T)
      parest = parameterestimates(specondmodel)
      specondeffects = parest[parest$op == "~",c(1, 3, 4, 8, 9, 7)]
      colnames(specondeffects) = c("SlopeOutcome", "Predictor", "b", "lowerCI", "upperCI", "p")
      specondeffects$slopeperiod = NA
      specondeffects$slopeperiod[grepl("intvslope", specondeffects$SlopeOutcome)] = "Intervention"
      specondeffects$slopeperiod[grepl("flwslope", specondeffects$SlopeOutcome)] = "Follow-up"
      specondeffects$SlopeOutcome = gsub("intvslope|flwslope", "", specondeffects$SlopeOutcome)
      specondeffects$SlopeOutcome = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",specondeffects$SlopeOutcome)
      specondeffects$Predictor = gsub("intvslope", "InterventionSlope", specondeffects$Predictor)
      specondeffects$Predictor = gsub("intercept", "Initial", specondeffects$Predictor)
      specondeffects$Predictor[grepl("Initial", specondeffects$Predictor)] = paste0("Initial", gsub("Initial", "", specondeffects$Predictor[grepl("Initial", specondeffects$Predictor)]))
      specondeffects$Predictor = gsub("ConditionCode", "Conditioncode", specondeffects$Predictor)
      specondeffects$Predictor = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",specondeffects$Predictor)
      specondeffects$Predictor = gsub("Conditioncode", "ConditionCode", specondeffects$Predictor)
      specondeffects = specondeffects[,c(1, 7, 2:6)]
      condeffects = rbind(condeffects, specondeffects, stringsAsFactors = F)
    }
    colnames(condeffects) = c("SlopeOutcome", "Period", "Predictor", "b", "lowerCI", "upperCI", "p")
    if (write == T) {write.csv(condeffects, "condeffects.csv", row.names = F)}
    #growth rates of each condition
    growthdescs = describeBy(dataset[,grepl("slope", colnames(dataset))&!grepl("dist", colnames(dataset))], dataset$Condition, mat = T)
    growthdescs$slope = substr(rownames(growthdescs), 1, nchar(rownames(growthdescs))-1)
    growthdescs$slopeperiod = NA
    growthdescs$slopeperiod[grepl("intvslope", growthdescs$slope)] = "Intervention"
    growthdescs$slopeperiod[grepl("flwslope", growthdescs$slope)] = "follow-up"
    growthdescs$slope = gsub("intvslope|flwslope", "", growthdescs$slope)
    growthdescs$slope = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",growthdescs$slope)
    growthdescs = growthdescs[,c("slope", "slopeperiod", "group1", "mean", "se", "n")]
    growthdescs$LowerCI = growthdescs$mean-qnorm(.975)*growthdescs$se
    growthdescs$UpperCI = growthdescs$mean+qnorm(.975)*growthdescs$se
    growthdescs$t = abs(growthdescs$mean)/growthdescs$se
    growthdescs$p = pt(growthdescs$t, growthdescs$n-1, lower.tail = F)
    growthdescs = growthdescs[,c("slope", "slopeperiod", "group1", "mean", "LowerCI", "UpperCI", "p")] 
    colnames(growthdescs)[1:4] = c("Measure", "Period", "Condition", "Growth Rate")
    rownames(growthdescs) = NULL
    if (write == T) {write.csv(growthdescs, "growthdescs.csv", row.names = F)}
  }
  #within person effect - both within conditions and overall
  if (is.null(knot) == F) {
    slopediffs = data.frame(matrix(nrow = 0, ncol = (length(unique(dataset$Condition))+1)*5))
    for (measure in measures) {
      timepoints = unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))
      if ((suppressWarnings(max(as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grep(paste0("^",measure), colnames(dataset))]))), na.rm = T)) 
           > knot) & (suppressWarnings(min(as.numeric(timepoints), na.rm = T)) < knot)) {
        diffscores = dataset[,grepl(paste0(measure, "intvslope"), colnames(dataset))&!grepl("dist", colnames(dataset))] - 
          dataset[,grepl(paste0(measure, "flwslope"), colnames(dataset))&!grepl("dist", colnames(dataset))] 
        m = mean(diffscores, na.rm = T)
        se = sd(diffscores, na.rm = T)/sqrt(length(diffscores[is.na(diffscores) == F]))
        lci = m-qnorm(.975)*se
        uci = m+qnorm(.975)*se
        p = t.test(diffscores)$p.value
        diffvect = c(m, lci, uci, p)
        for (cond in unique(dataset$Condition)){
          diffscores2 = diffscores[dataset$Condition == cond]
          m = mean(diffscores2, na.rm = T)
          se = sd(diffscores2, na.rm = T)/sqrt(length(diffscores2[is.na(diffscores2) == F]))
          lci = m-qnorm(.975)*se
          uci = m+qnorm(.975)*se
          p = t.test(diffscores2)$p.value
          diffvect = c(diffvect, m, lci, uci, p)
        }
        slopediffs = rbind(slopediffs, diffvect)
        rownames(slopediffs)[nrow(slopediffs)] = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",measure)
      }
    }
    colnames(slopediffs) = paste0(rep(c("Overall", unique(dataset$Condition)), each = 4),
                                  rep(c(" MeanDiff", " LowerCI", " UpperCI", " p"), length(unique(dataset$Condition))+1))
    if (write == T) {write.csv(slopediffs, "slopediffs.csv")}
  }
  if (updates == T) {message("Effects of condition on slopes (if applicable) have been computed")}
  #moderation
  #creating moderation variables
  if (is.null(mods) == F | is.null(tvcovs) == F) {
    moddata = data.frame(matrix(ncol = 0, nrow = nrow(dataset)))
    for (mod in mods) {
      factordata = dataset[,grep(paste0("^",mod), colnames(dataset))]
      timepoints = unique(gsub(".*\\.","", colnames(dataset)[grepl(paste0("^",mod), colnames(dataset)) & grepl("[[:digit:]]$", colnames(dataset))]))
      firsttimepoint = min(as.numeric(timepoints))
      factorscoreeqs = paste0("intvInitial",mod, "=~", paste0(colnames(dataset)[grepl(paste0("^",mod), colnames(dataset)) &
                                                                                  grepl(paste0(firsttimepoint, "$"), colnames(dataset))], collapse = "+"), "\n")
      if (is.null(knot) == F) {
        if (sum(grepl(paste0("^",mod), colnames(dataset)) & grepl(paste0(knot, "$"), colnames(dataset))) != 0) {
          factorscoreeqs = paste0(factorscoreeqs, "flwInitial",mod, "=~", paste0(colnames(dataset)[grepl(paste0("^",mod), colnames(dataset)) &
                                                                                                     grepl(paste0(knot, "$"), colnames(dataset))], collapse = "+"))
        }
      }
      factorscoremodel = cfa(factorscoreeqs, dataset, missing = "fiml", std.lv = T, auto.fix.first = F, auto.cov.lv.x = T)
      factorscore = lavPredict(factorscoremodel)
      moddata = cbind(moddata, factorscore)
    }
    for (tvcov in tvcovs){
      timepoints = as.numeric(unique(gsub(".*\\.","", colnames(dataset)[grepl(paste0("^",tvcov), colnames(dataset))& grepl("[[:digit:]]$", colnames(dataset))])))
      if (length(unique(gsub(tvcov, "",gsub("\\..*","", colnames(dataset)[grep(paste0("^",tvcov), colnames(dataset))])))) == 1) {
        if (is.null(knot) == F) {
          tvcoveqs = paste0("intvAverage",tvcov, "=~ ", paste0(tvcov, "1.", timepoints[timepoints<=knot], 
                                                               collapse = "+"), "\n")
        } else {
          tvcoveqs = paste0("intvAverage",tvcov, "=~ ", paste0(tvcov, "1.", timepoints, 
                                                               collapse = "+"), "\n")
        }
        if (is.null(knot) == F) {
          if (length(timepoints[timepoints > knot]) > 0) {
            tvcoveqs = paste0(tvcoveqs, "flwAverage",tvcov, "=~ ", paste0(tvcov, "1.", timepoints[timepoints>knot], collapse = "+")) 
          }
        }
      } else {
        tvcoveqs = ifelse(chgcfis[tvcov,2] > .01, measinveqs[tvcov,2], ifelse(chgcfis[tvcov,3] > .01, measinveqs[tvcov,3], measinveqs[tvcov,4]))
        if (is.null(knot) == F) {
          tvcoveqs = paste0(tvcoveqs, "intvAverage",tvcov, "=~ ", paste0(tvcov, ".", timepoints[timepoints<=knot], 
                                                                         collapse = "+"), "\n")
        } else {
          tvcoveqs = paste0(tvcoveqs, "intvAverage",tvcov, "=~ ", paste0(tvcov, ".", timepoints, 
                                                                         collapse = "+"), "\n")
        }
        if (is.null(knot) == F) {
          if (length(timepoints[timepoints > knot]) > 0) {
            tvcoveqs = paste0(tvcoveqs, "flwAverage",tvcov, "=~ ", paste0(tvcov, ".", timepoints[timepoints>knot], collapse = "+"))
          }
        }
      }
      tvcovmodel = cfa(tvcoveqs, dataset, missing = "fiml", std.lv = T)
      tvcovvals = lavPredict(tvcovmodel)
      tempcolnames = colnames(tvcovvals)[grep("Average", colnames(tvcovvals))]
      tvcovvals = as.data.frame(tvcovvals[,grep("Average", colnames(tvcovvals))])
      colnames(tvcovvals) = tempcolnames
      moddata = cbind(moddata, tvcovvals)
    }
    dataset = cbind(dataset, moddata)
    modeffects = as.data.frame(matrix(ncol = 6, nrow = 0))
    slopes = colnames(dataset)[grep("slope$", colnames(dataset))]
    intercepts = gsub("slope", "intercept", slopes)
    intercepts = gsub("intv", "", intercepts)
    intercepts = gsub("flw", "", intercepts)
    for (mod in colnames(moddata)) {
      for (slope in slopes) {
        if((str_count(paste0(mod,slope), coll("intv", T)) == 2) | (str_count(paste0(mod,slope), coll("flw", T)) == 2)) {
          intercept = intercepts[grep(slope, slopes)]
          modcontrol = paste0("dataset$", intercept)
          if (grepl("flw", slope) & controlflwforintv & (gsub("flw", "intv", slope) %in% slopes)) {
            modcontrol = paste0(modcontrol, "+", paste0("dataset$", gsub("flw", "intv", slope)))
          }
          modmodel = eval(parse(text = paste0("summary(lm(dataset[,slope] ~ ", modcontrol, "+", paste0("dataset$",mod,"*", "dataset$ConditionCode",1:numcondcodes, collapse = "+"), "))$coefficients")))
          modvals = as.data.frame(modmodel[-1,], stringsAsFactors = F)
          modvals$LowerCI = modvals[,1]-qnorm(.975)*modvals[,2]
          modvals$UpperCI = modvals[,1]+qnorm(.975)*modvals[,2]
          modvals = modvals[,c(1,5,6,4)]
          modvals$Outcome = slope
          modvals$Predictor = rownames(modvals)
          rownames(modvals) = NULL
          modvals$Predictor = gsub("dataset\\$","",modvals$Predictor)
          modvals = modvals[!grepl("^ConditionCode",modvals$Predictor),]
          modvals = modvals[,c(5,6,1:4)]
          modeffects = rbind(modeffects, modvals)
        }
      }
    }
    colnames(modeffects)[6] = "p"
    colnames(modeffects)[3] = "b"
    modeffects$Outcome = gsub("intvslope", "InterventionSlope", modeffects$Outcome)
    modeffects$Outcome = gsub("flwslope", "Follow-upSlope", modeffects$Outcome)
    modeffects$Outcome = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",modeffects$Outcome)
    modeffects$Outcome = gsub("2", "2 ", modeffects$Outcome)
    modeffects$Period = NA
    modeffects$Period[grepl("Intervention", modeffects$Outcome)] = "Intervention"
    modeffects$Period[grepl("Follow-up", modeffects$Outcome)] = "Follow-up"
    modeffects$Predictor = gsub("intvslope", "InterventionSlope", modeffects$Predictor)
    modeffects$Predictor= gsub("intv", "", modeffects$Predictor)
    modeffects$Predictor = gsub("flw", "", modeffects$Predictor)
    modeffects$Predictor = gsub("intercept", "Initial", modeffects$Predictor)
    modeffects$Predictor[grepl("Initial", modeffects$Predictor)] = paste0("Initial", gsub("Initial", "", modeffects$Predictor[grepl("Initial", modeffects$Predictor)]))
    modeffects$Predictor = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",modeffects$Predictor)
    modeffects$Predictor = gsub("Code", "Code ", modeffects$Predictor)
    modeffects$Predictor = gsub(":", " x ", modeffects$Predictor)
    modeffects = modeffects[,c(2, 1, 7, 3:6)]
    modeffects$predcon = modeffects$Predictor
    modeffects$predcon = gsub("Initial ","",modeffects$predcon)
    modeffects$predcon = gsub("Average ","",modeffects$predcon)
    modeffects$predcon = gsub(" x Condition Code.","",modeffects$predcon)
    modeffects = modeffects[order(modeffects$predcon, modeffects$Outcome),]
    modeffects = modeffects[,1:7]
    if (write == T) {write.csv(modeffects, "modeffects.csv", row.names = F)}
  }
  if (updates == T) {message("Effects of moderators, and time-varying covariates on slopes (if applicable) have been computed")}
  #adding level of invariance to chgcfis and renaming rows to include spaces
  if(nrow(chgcfis) > 0) {
    rownames(chgcfis) = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",rownames(chgcfis))
    chgcfis$measinvlevel = as.character(NA)
    for (i in 1:nrow(chgcfis)) {
      if (chgcfis[i,"Weak"] > .01) {
        chgcfis$measinvlevel[i] = "Configural"
      } else if (chgcfis[i,"Strong"] > .01) {
        chgcfis$measinvlevel[i] = "Weak"
      } else if (chgcfis[i,"Strict"] > .01) {
        chgcfis$measinvlevel[i] = "Strong"
      } else {
        chgcfis$measinvlevel[i] = "Strict"
      }
    }
    colnames(chgcfis)[4] = "Measurement Invariance Level"
  }
  if (write == T) {write.csv(chgcfis, "changecfis.csv")}
  colnames(solgmfits) = c("ChiSquare", "df", "CFI", "TLI", "RMSEA", "RMSEALowerCI", "RMSEAUpperCI", "SRMR")
  rownames(solgmfits) = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2",rownames(solgmfits))
  if (write == T) {write.csv(solgmfits, "solgmfits.csv")}
  if (write == T) {write.csv(cutitems, "cutitems.csv")}
  if (write == T) {write.csv(heywoodlog, "heywoodlog.csv")}
  if (write == T) {write.csv(dataset, "longmodelsdata.csv")}
  #exporting list of relevant info
  outlist = list(data=dataset, changecfis=chgcfis, cutitems = cutitems, measinveqs = measinveqs, measinvmodels = measinvmodels, 
                 alphas = alphas, omegas = omegas, solgmfits = solgmfits, solgmeqs = solgmeqs, solgmmodels = solgmmodels, 
                 heywoodlog = heywoodlog)
  if (numcondcodes > 0) {outlist = c(outlist, list(condeffects = condeffects, growthdescs = growthdescs))}
  if (is.null(knot) == F) {outlist = c(outlist, list(slopediffs = slopediffs))}
  if (is.null(mods) == F | is.null(tvcovs) == F) {outlist = c(outlist, list(modeffects = modeffects))}
  outlist
}
# Long Models Analyses ----------------------------------------------------
setwd("/Users/davidko/Desktop/Recall Study/Analyses6")
reclongmodels = longmodels(
  measures = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
               "Autonomy"), 
  dataset = recdata,
  revinfo = NULL,
  multidims = NULL,
  cutvaritems = T,
  minitems = 4,
  timespace = "Day",
  knot = 2,
  growthshape = "line",
  growthshape2 = "line",
  mods = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
           "Autonomy", "Sex", "Asian", "Latino"),
  tvcovs = NULL,
  overridesolgmeqs = NULL,
  write = T,
  updates = T,
  controlflwforintv = F
)
#Look at manipulation check effects manually



# Removed Day 2 Pairwise Comparison (Each condition to control Days 1-3) ---------------------------------------
# Importing and cleaning data ---------------------------------------------
setwd("/Users/davidko/Desktop/Recall Study")
# Importing day 1 data from qualtrics with lines 1 & 3 removed
day1data = read.csv("day1data.csv", header=T, na.strings = "", 
                    stringsAsFactors = F, strip.white = T)
# Removing needless columns
colnames(day1data)
day1data=day1data[,-c(3:4,7:21,30:33,46:49,55:58,77:80)]
# Name columns
colnames(day1data)[1:12]=c("StartDate1.1", "EndDate1.1", "Progress1.1", "Duration1.1",
                           "FirstName1.1", "LastName1.1", "Email1.1", "Age1.1", "Sex1.1",
                           "Ethnicity1.1", "Other1.1", "ParentsEducation1.1")
colnames(day1data)[13:24]=c("PositiveAffect1.1", "NegativeAffect1.1", "PositiveAffect2.1",
                            "NegativeAffect2.1","NegativeAffect3.1","NegativeAffect4.1",
                            "PositiveAffect3.1", "NegativeAffect5.1","PositiveAffect4.1",
                            "PositiveAffect5.1","NegativeAffect6.1","PositiveAffect6.1")
colnames(day1data)[25:29] = paste0("LifeSatisfaction", 1:5, ".1")
colnames(day1data)[30:35] = paste0("Connectedness", 1:6, ".1")
colnames(day1data)[36:41] = paste0("Competence", 1:6, ".1")
colnames(day1data)[42:47] = paste0("Autonomy", 1:6, ".1")
colnames(day1data)[48]=c("Condition1.1")
# Setting correct data types
# str(day1data) <--use this to view structure
day1data$StartDate1.1=as.POSIXct(day1data$StartDate1.1, format="%m/%d/%Y %H:%M")
day1data$EndDate1.1=as.POSIXct(day1data$EndDate1.1, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day1data$FullName = paste0(day1data$FirstName1.1, day1data$LastName1.1)
day1data$Duplicate1.1 = as.numeric(duplicated(day1data$FullName) | duplicated(day1data$FullName, fromLast = T))
day1data$MinProgress1.1 = 0
day1data$MinProgress1.1[day1data$Progress1.1 == min(day1data$Progress1.1)] = 1
# plyr::count(day1data$Progress1.1)
day1data$MinProgress1.1=F
day1data$MinProgress1.1[day1data$Progress1.1==min(day1data$Progress1.1)]=T
# Subsetting data
day1data=day1data[day1data$Duplicate1.1==F & day1data$MinProgress1.1==F,]
# Importing day 2 data from qualtrics with lines 1 & 3 removed ------------
day2data = read.csv("day2data.csv", header=T, na.strings = "",
                    stringsAsFactors = F, strip.white = T)
#Removing needless columns
colnames(day2data)
day2data=day2data[,-c(3:4,7:9,13:17, 19:22, 35:38, 44:47, 66:69)]
#Name columns
colnames(day2data)[1:7]=c("StartDate1.2", "EndDate1.2", "Progress1.2", "Duration1.2",
                          "LastName1.2", "FirstName1.2", "Email1.2")
colnames(day2data)[8]=c("Recall1.2")
colnames(day2data)[9:20]=c("PositiveAffect1.2", "NegativeAffect1.2", "PositiveAffect2.2",
                           "NegativeAffect2.2","NegativeAffect3.2","NegativeAffect4.2",
                           "PositiveAffect3.2", "NegativeAffect5.2","PositiveAffect4.2",
                           "PositiveAffect5.2","NegativeAffect6.2","PositiveAffect6.2")
colnames(day2data)[21:25] = paste0("LifeSatisfaction", 1:5, ".2")
colnames(day2data)[26:31] = paste0("Connectedness", 1:6, ".2")
colnames(day2data)[32:37] = paste0("Competence", 1:6, ".2")
colnames(day2data)[38:43] = paste0("Autonomy", 1:6, ".2")
colnames(day2data)[44]=c("Condition1.2")
# Setting correct data types
day2data$StartDate1.2=as.POSIXct(day2data$StartDate1.2, format="%m/%d/%Y %H:%M")
day2data$EndDate1.2=as.POSIXct(day2data$EndDate1.2, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day2data$FullName = paste0(day2data$FirstName1.2, day2data$LastName1.2)
day2data$Duplicate1.2 = as.numeric(duplicated(day2data$FullName) | duplicated(day2data$FullName, fromLast = T))
day2data$MinProgress1.2 = 0
day2data$MinProgress1.2[day2data$Progress1.2 == min(day2data$Progress1.2)] = 1
#Subsetting data
day2data=day2data[day2data$Duplicate1.2==F & day2data$MinProgress1.2==F,]

# Importing day 3 data from qualtrics with lines 1 & 3 removed ------------
day3data = read.csv("day3data.csv", header=T, na.strings = "",
                    stringsAsFactors = F, strip.white = T)
#Removing needless columns
colnames(day3data)
day3data=day3data[,-c(3:4,7:9,13:17, 30:33, 39:42, 61:64, 66:69, 71:74,76)]
#Name columns
colnames(day3data)[1:7]=c("StartDate1.3", "EndDate1.3", "Progress1.3", "Duration1.3",
                          "LastName1.3", "FirstName1.3", "Email1.3")
colnames(day3data)[8:19]=c("PositiveAffect1.3", "NegativeAffect1.3", "PositiveAffect2.3",
                           "NegativeAffect2.3","NegativeAffect3.3","NegativeAffect4.3",
                           "PositiveAffect3.3", "NegativeAffect5.3","PositiveAffect4.3",
                           "PositiveAffect5.3","NegativeAffect6.3","PositiveAffect6.3")
colnames(day3data)[20:24] = paste0("LifeSatisfaction", 1:5, ".3")
colnames(day3data)[25:30] = paste0("Connectedness", 1:6, ".3")
colnames(day3data)[31:36] = paste0("Competence", 1:6, ".3")
colnames(day3data)[37:42] = paste0("Autonomy", 1:6, ".3")
colnames(day3data)[43:44] = c("PerformManipulationCheck1.1", "RecallManipulationCheck1.1")
colnames(day3data)[45]=c("Condition1.3")
# Setting correct data types
day3data$StartDate1.3=as.POSIXct(day3data$StartDate1.3, format="%m/%d/%Y %H:%M")
day3data$EndDate1.3=as.POSIXct(day3data$EndDate1.3, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day3data$FullName = paste0(day3data$FirstName1.3, day3data$LastName1.3)
day3data$Duplicate1.3 = as.numeric(duplicated(day3data$FullName) | duplicated(day3data$FullName, fromLast = T))
day3data$MinProgress1.3 = 0
day3data$MinProgress1.3[day3data$Progress1.3 == min(day3data$Progress1.3)] = 1
#Subsetting data
day3data=day3data[day3data$Duplicate1.3==F & day3data$MinProgress1.3==F,]

# Merging data ------------------------------------------------------------

recdata=merge(day1data,day2data,by="FullName",all=T,sort=F)
recdata=merge(recdata,day3data,by="FullName",all=T,sort=F)
#Remove entries not done on an acceptable date
recdata$Days1.2=round(recdata$EndDate1.2,units="days") - round(recdata$EndDate1.1,units="days")
recdata$Days1.3=round(recdata$EndDate1.3,units="days") - round(recdata$EndDate1.2,units="days")
recdata[(recdata$Days1.2 > 2 | is.na(recdata$Days1.2)==T), grep(".2$",colnames(recdata))]=NA
recdata[(recdata$Days1.3 > 2 | is.na(recdata$Days1.3)==T), grep(".3$",colnames(recdata))]=NA
#removing people not assigned a condition at time 1 (didn't finish day 1)
recdata$Condition=recdata$Condition1.1
recdata=recdata[,-grep("Condition1", colnames(recdata))]
recdata=recdata[!is.na(recdata$Condition),]
#removing people who weren't paying attention
recdata$maxcons = 0
for (i in 1:nrow(recdata)) {
  recdata$maxcons[i] = max(rle(recdata[i,-grep("Date", colnames(recdata))])$lengths)
}
recdata=recdata[recdata$maxcons < 14,]
#manipulation check
plyr::count(recdata$RecallManipulationCheck1.1)
plyr::count(recdata$PerformManipulationCheck1.1)
recdata=recdata[recdata$RecallManipulationCheck1.1!=1 | 
                  is.na(recdata$RecallManipulationCheck1.1),]
recdata=recdata[recdata$PerformManipulationCheck1.1!=1 | 
                  is.na(recdata$PerformManipulationCheck1.1),]
#make condition code variables
recdata$ConditionCode1=0
recdata$ConditionCode1[recdata$Condition==1]=1
recdata$ConditionCode2=0
recdata$ConditionCode2[recdata$Condition==4]=1
recdata$ConditionCode3=0
recdata$ConditionCode3[recdata$Condition==2]=1
#reverse coding items
revnames=rep(c("Connectedness", "Competence", "Autonomy"), 3)
revnums=rep(c(2, 4, 6), each=3)
revs=paste0(revnames,revnums)
recdata[,grep(paste0("^", revs, collapse = "|"), colnames(recdata))]=8-
  recdata[,grep(paste0("^", revs, collapse = "|"), colnames(recdata))]

cor(recdata[,grepl("Connectedness", colnames(recdata)) & grepl("1$", colnames(recdata))]
    , use="pairwise.complete.obs")
#Asian and Latino dummy codes
recdata$Asian1.1 = 0
recdata$Asian1.1[recdata$Ethnicity1.1==2]=1
recdata$Latino1.1 = 0
recdata$Latino1.1[recdata$Ethnicity1.1==6]=1
#Renaming Conditions
recdata$Condition[recdata$Condition == "1"] <- "Perform"
recdata$Condition[recdata$Condition == "2"] <- "PerformAndRecall"
recdata$Condition[recdata$Condition == "3"] <- "Control"
recdata$Condition[recdata$Condition == "4"] <- "Recall"
# Day 2 removed -----------------------------------------------------------
recdata2=recdata[,-grep("2$", colnames(recdata))[1:(length(grep("2$", colnames(recdata)))-1)]]
# Long Models Analyses #2 -------------------------------------------------
setwd("/Users/davidko/Desktop/Recall Study/Analyses7")
reclongmodels2 = longmodels(
  measures = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", 
               "Competence", "Connectedness", "Autonomy"), 
  dataset = recdata2,
  revinfo = NULL,
  multidims = NULL,
  cutvaritems = T,
  minitems = 4,
  timespace = "Day",
  knot = NULL,
  growthshape = "line",
  growthshape2 = "line",
  mods = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
           "Autonomy", "Sex", "Asian", "Latino"),
  tvcovs = NULL,
  overridesolgmeqs = NULL,
  write = T,
  updates = T,
  controlflwforintv = F
)

# Number of Acts Moderation -----------------------------------------------
moddata = reclongmodels2$data
moddata = moddata[(moddata$Condition == "Perform") | (moddata$Condition == "PerformAndRecall"),]
summary(lm(PositiveAffectintvslope ~ PositiveAffectintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(NegativeAffectintvslope ~ NegativeAffectintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(LifeSatisfactionintvslope ~ LifeSatisfactionintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(Competenceintvslope ~ Competenceintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(Connectednessintvslope ~ Connectednessintercept + Condition + PerformManipulationCheck1.1, moddata))
summary(lm(Autonomyintvslope ~ Autonomyintercept + Condition + PerformManipulationCheck1.1, moddata))

rowcounter = 1
for (outcome in c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
                  "Autonomy")) {
  eval(parse(text = paste0("info = summary(lm(", outcome, "intvslope
                           ~ ", outcome, "intercept + Condition + PerformManipulationCheck1.1,moddata))")))
  info = info$coefficients
  info = info[4,c(1:2,4)]
  info = unname(info)
  b = info[1]
  se = info[2]
  p = info[3]
  p = round(p, 3)
  lci = b - qnorm(.975)*se
  uci = b + qnorm(.975)*se
  bci = paste0(round(b, 2), " [",round(lci, 2), ", ", round(uci, 2),"]") 
  modout[rowcounter, 3:4] = c(bci, p)
  rowcounter = rowcounter+1
}
rm(rowcounter, info, b, se, p, lci, uci, bci)
write.csv(modout, "modout.csv")
# Performing vs Recalling -------------------------------------------------
pvrdata2 = reclongmodels2$data
pvrdata2 = pvrdata2[pvrdata2$Condition != "Control",]
pvrdata2$Condition = as.factor(pvrdata2$Condition)
PA13mod1 = lm(PositiveAffectintvslope ~ PositiveAffectintercept, data = pvrdata2)
PA13mod2 = lm(PositiveAffectintvslope ~ PositiveAffectintercept + Condition, data = pvrdata2)
anova(PA13mod1, PA13mod2)
#p = 0.2849 
NA13mod1 = lm(NegativeAffectintvslope ~ NegativeAffectintercept, data = pvrdata2)
NA13mod2 = lm(NegativeAffectintvslope ~ NegativeAffectintercept + Condition, data = pvrdata2)
anova(NA13mod1, NA13mod2)
#p = 0.3666
LS13mod1 = lm(LifeSatisfactionintvslope ~ LifeSatisfactionintercept, data = pvrdata2)
LS13mod2 = lm(LifeSatisfactionintvslope ~ LifeSatisfactionintercept + Condition, data = pvrdata2)
anova(LS13mod1, LS13mod2)
#p = 0.943
Comp13mod1 = lm(Competenceintvslope ~ Competenceintercept, data = pvrdata2)
Comp13mod2 = lm(Competenceintvslope ~ Competenceintercept + Condition, data = pvrdata2)
anova(Comp13mod1, Comp13mod2)
#p = 0.2449
Con13mod1 = lm(Connectednessintvslope ~ Connectednessintercept, data = pvrdata2)
Con13mod2 = lm(Connectednessintvslope ~ Connectednessintercept + Condition, data = pvrdata2)
anova(Con13mod1, Con13mod2)
#p = 0.3538
Aut13mod1 = lm(Autonomyintvslope ~ Autonomyintercept, data = pvrdata2)
Aut13mod2 = lm(Autonomyintvslope ~ Autonomyintercept + Condition, data = pvrdata2)
anova(Aut13mod1, Aut13mod2)
#p = 0.9677
# Removed Day 2 Pairwise Comparison 2 (Every experimental to control Days 1-3) -------------------------------------
# Importing and cleaning data ---------------------------------------------
setwd("/Users/davidko/Desktop/Recall Study")
# Importing day 1 data from qualtrics with lines 1 & 3 removed
day1data = read.csv("day1data.csv", header=T, na.strings = "", 
                    stringsAsFactors = F, strip.white = T)
# Removing needless columns
colnames(day1data)
day1data=day1data[,-c(3:4,7:21,30:33,46:49,55:58,77:80)]
# Name columns
colnames(day1data)[1:12]=c("StartDate1.1", "EndDate1.1", "Progress1.1", "Duration1.1",
                           "FirstName1.1", "LastName1.1", "Email1.1", "Age1.1", "Sex1.1",
                           "Ethnicity1.1", "Other1.1", "ParentsEducation1.1")
colnames(day1data)[13:24]=c("PositiveAffect1.1", "NegativeAffect1.1", "PositiveAffect2.1",
                            "NegativeAffect2.1","NegativeAffect3.1","NegativeAffect4.1",
                            "PositiveAffect3.1", "NegativeAffect5.1","PositiveAffect4.1",
                            "PositiveAffect5.1","NegativeAffect6.1","PositiveAffect6.1")
colnames(day1data)[25:29] = paste0("LifeSatisfaction", 1:5, ".1")
colnames(day1data)[30:35] = paste0("Connectedness", 1:6, ".1")
colnames(day1data)[36:41] = paste0("Competence", 1:6, ".1")
colnames(day1data)[42:47] = paste0("Autonomy", 1:6, ".1")
colnames(day1data)[48]=c("Condition1.1")
# Setting correct data types
# str(day1data) <--use this to view structure
day1data$StartDate1.1=as.POSIXct(day1data$StartDate1.1, format="%m/%d/%Y %H:%M")
day1data$EndDate1.1=as.POSIXct(day1data$EndDate1.1, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day1data$FullName = paste0(day1data$FirstName1.1, day1data$LastName1.1)
day1data$Duplicate1.1 = as.numeric(duplicated(day1data$FullName) | duplicated(day1data$FullName, fromLast = T))
day1data$MinProgress1.1 = 0
day1data$MinProgress1.1[day1data$Progress1.1 == min(day1data$Progress1.1)] = 1
# plyr::count(day1data$Progress1.1)
day1data$MinProgress1.1=F
day1data$MinProgress1.1[day1data$Progress1.1==min(day1data$Progress1.1)]=T
# Subsetting data
day1data=day1data[day1data$Duplicate1.1==F & day1data$MinProgress1.1==F,]
# Importing day 2 data from qualtrics with lines 1 & 3 removed ------------
day2data = read.csv("day2data.csv", header=T, na.strings = "",
                    stringsAsFactors = F, strip.white = T)
#Removing needless columns
colnames(day2data)
day2data=day2data[,-c(3:4,7:9,13:17, 19:22, 35:38, 44:47, 66:69)]
#Name columns
colnames(day2data)[1:7]=c("StartDate1.2", "EndDate1.2", "Progress1.2", "Duration1.2",
                          "LastName1.2", "FirstName1.2", "Email1.2")
colnames(day2data)[8]=c("Recall1.2")
colnames(day2data)[9:20]=c("PositiveAffect1.2", "NegativeAffect1.2", "PositiveAffect2.2",
                           "NegativeAffect2.2","NegativeAffect3.2","NegativeAffect4.2",
                           "PositiveAffect3.2", "NegativeAffect5.2","PositiveAffect4.2",
                           "PositiveAffect5.2","NegativeAffect6.2","PositiveAffect6.2")
colnames(day2data)[21:25] = paste0("LifeSatisfaction", 1:5, ".2")
colnames(day2data)[26:31] = paste0("Connectedness", 1:6, ".2")
colnames(day2data)[32:37] = paste0("Competence", 1:6, ".2")
colnames(day2data)[38:43] = paste0("Autonomy", 1:6, ".2")
colnames(day2data)[44]=c("Condition1.2")
# Setting correct data types
day2data$StartDate1.2=as.POSIXct(day2data$StartDate1.2, format="%m/%d/%Y %H:%M")
day2data$EndDate1.2=as.POSIXct(day2data$EndDate1.2, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day2data$FullName = paste0(day2data$FirstName1.2, day2data$LastName1.2)
day2data$Duplicate1.2 = as.numeric(duplicated(day2data$FullName) | duplicated(day2data$FullName, fromLast = T))
day2data$MinProgress1.2 = 0
day2data$MinProgress1.2[day2data$Progress1.2 == min(day2data$Progress1.2)] = 1
#Subsetting data
day2data=day2data[day2data$Duplicate1.2==F & day2data$MinProgress1.2==F,]

# Importing day 3 data from qualtrics with lines 1 & 3 removed ------------
day3data = read.csv("day3data.csv", header=T, na.strings = "",
                    stringsAsFactors = F, strip.white = T)
#Removing needless columns
colnames(day3data)
day3data=day3data[,-c(3:4,7:9,13:17, 30:33, 39:42, 61:64, 66:69, 71:74,76)]
#Name columns
colnames(day3data)[1:7]=c("StartDate1.3", "EndDate1.3", "Progress1.3", "Duration1.3",
                          "LastName1.3", "FirstName1.3", "Email1.3")
colnames(day3data)[8:19]=c("PositiveAffect1.3", "NegativeAffect1.3", "PositiveAffect2.3",
                           "NegativeAffect2.3","NegativeAffect3.3","NegativeAffect4.3",
                           "PositiveAffect3.3", "NegativeAffect5.3","PositiveAffect4.3",
                           "PositiveAffect5.3","NegativeAffect6.3","PositiveAffect6.3")
colnames(day3data)[20:24] = paste0("LifeSatisfaction", 1:5, ".3")
colnames(day3data)[25:30] = paste0("Connectedness", 1:6, ".3")
colnames(day3data)[31:36] = paste0("Competence", 1:6, ".3")
colnames(day3data)[37:42] = paste0("Autonomy", 1:6, ".3")
colnames(day3data)[43:44] = c("PerformManipulationCheck1.1", "RecallManipulationCheck1.1")
colnames(day3data)[45]=c("Condition1.3")
# Setting correct data types
day3data$StartDate1.3=as.POSIXct(day3data$StartDate1.3, format="%m/%d/%Y %H:%M")
day3data$EndDate1.3=as.POSIXct(day3data$EndDate1.3, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day3data$FullName = paste0(day3data$FirstName1.3, day3data$LastName1.3)
day3data$Duplicate1.3 = as.numeric(duplicated(day3data$FullName) | duplicated(day3data$FullName, fromLast = T))
day3data$MinProgress1.3 = 0
day3data$MinProgress1.3[day3data$Progress1.3 == min(day3data$Progress1.3)] = 1
#Subsetting data
day3data=day3data[day3data$Duplicate1.3==F & day3data$MinProgress1.3==F,]

# Merging data ------------------------------------------------------------

recdata=merge(day1data,day2data,by="FullName",all=T,sort=F)
recdata=merge(recdata,day3data,by="FullName",all=T,sort=F)
#Remove entries not done on an acceptable date
recdata$Days1.2=round(recdata$EndDate1.2,units="days") - round(recdata$EndDate1.1,units="days")
recdata$Days1.3=round(recdata$EndDate1.3,units="days") - round(recdata$EndDate1.2,units="days")
recdata[(recdata$Days1.2 > 2 | is.na(recdata$Days1.2)==T), grep(".2$",colnames(recdata))]=NA
recdata[(recdata$Days1.3 > 2 | is.na(recdata$Days1.3)==T), grep(".3$",colnames(recdata))]=NA
#removing people not assigned a condition at time 1 (didn't finish day 1)
recdata$Condition=recdata$Condition1.1
recdata=recdata[,-grep("Condition1", colnames(recdata))]
recdata=recdata[!is.na(recdata$Condition),]
#removing people who weren't paying attention
recdata$maxcons = 0
for (i in 1:nrow(recdata)) {
  recdata$maxcons[i] = max(rle(recdata[i,-grep("Date", colnames(recdata))])$lengths)
}
recdata=recdata[recdata$maxcons < 14,]
#manipulation check
plyr::count(recdata$RecallManipulationCheck1.1)
plyr::count(recdata$PerformManipulationCheck1.1)
recdata=recdata[recdata$RecallManipulationCheck1.1!=1 | 
                  is.na(recdata$RecallManipulationCheck1.1),]
recdata=recdata[recdata$PerformManipulationCheck1.1!=1 | 
                  is.na(recdata$PerformManipulationCheck1.1),]
#make condition code variables
recdata$ConditionCode1=1
recdata$ConditionCode1[recdata$Condition==3]=0
#reverse coding items
revnames=rep(c("Connectedness", "Competence", "Autonomy"), 3)
revnums=rep(c(2, 4, 6), each=3)
revs=paste0(revnames,revnums)
recdata[,grep(paste0("^", revs, collapse = "|"), colnames(recdata))]=8-
  recdata[,grep(paste0("^", revs, collapse = "|"), colnames(recdata))]

cor(recdata[,grepl("Connectedness", colnames(recdata)) & grepl("1$", colnames(recdata))]
    , use="pairwise.complete.obs")
#Asian and Latino dummy codes
recdata$Asian1.1 = 0
recdata$Asian1.1[recdata$Ethnicity1.1==2]=1
recdata$Latino1.1 = 0
recdata$Latino1.1[recdata$Ethnicity1.1==6]=1
#Renaming Conditions
recdata$Condition[recdata$Condition == "1"] <- "Perform"
recdata$Condition[recdata$Condition == "2"] <- "PerformAndRecall"
recdata$Condition[recdata$Condition == "3"] <- "Control"
recdata$Condition[recdata$Condition == "4"] <- "Recall"
# Day 2 removed -----------------------------------------------------------
recdata2=recdata[,-grep("2$", colnames(recdata))[1:(length(grep("2$", colnames(recdata)))-1)]]
# Long Models Analyses #2 -------------------------------------------------
setwd("/Users/davidko/Desktop/Recall Study/Analyses8")
reclongmodels2 = longmodels(
  measures = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", 
               "Competence", "Connectedness", "Autonomy"), 
  dataset = recdata2,
  revinfo = NULL,
  multidims = NULL,
  cutvaritems = T,
  minitems = 4,
  timespace = "Day",
  knot = NULL,
  growthshape = "line",
  growthshape2 = "line",
  mods = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", "Competence", "Connectedness", 
           "Autonomy", "Sex", "Asian", "Latino"),
  tvcovs = NULL,
  overridesolgmeqs = NULL,
  write = T,
  updates = T,
  controlflwforintv = F
)




# Removed Day 2 Pairwise Comparison (Each condition to control Days 2-3) ---------------------------------------
# Importing and cleaning data ---------------------------------------------
setwd("/Users/davidko/Desktop/Recall Study")
# Importing day 2 data from qualtrics with lines 1 & 3 removed ------------
day2data = read.csv("day2data.csv", header=T, na.strings = "",
                    stringsAsFactors = F, strip.white = T)
#Removing needless columns
colnames(day2data)
day2data=day2data[,-c(3:4,7:9,13:17, 19:22, 35:38, 44:47, 66:69)]
#Name columns
colnames(day2data)[1:7]=c("StartDate1.1", "EndDate1.1", "Progress1.1", "Duration1.1",
                          "LastName1.1", "FirstName1.1", "Email1.1")
colnames(day2data)[8]=c("Recall1.1")
colnames(day2data)[9:20]=c("PositiveAffect1.1", "NegativeAffect1.1", "PositiveAffect2.1",
                           "NegativeAffect2.1","NegativeAffect3.1","NegativeAffect4.1",
                           "PositiveAffect3.1", "NegativeAffect5.1","PositiveAffect4.1",
                           "PositiveAffect5.1","NegativeAffect6.1","PositiveAffect6.1")
colnames(day2data)[21:25] = paste0("LifeSatisfaction", 1:5, ".1")
colnames(day2data)[26:31] = paste0("Connectedness", 1:6, ".1")
colnames(day2data)[32:37] = paste0("Competence", 1:6, ".1")
colnames(day2data)[38:43] = paste0("Autonomy", 1:6, ".1")
colnames(day2data)[44]=c("Condition1.1")
# Setting correct data types
day2data$StartDate1.1=as.POSIXct(day2data$StartDate1.1, format="%m/%d/%Y %H:%M")
day2data$EndDate1.1=as.POSIXct(day2data$EndDate1.1, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day2data$FullName = paste0(day2data$FirstName1.1, day2data$LastName1.1)
day2data$Duplicate1.1 = as.numeric(duplicated(day2data$FullName) | duplicated(day2data$FullName, fromLast = T))
day2data$MinProgress1.1 = 0
day2data$MinProgress1.1[day2data$Progress1.1 == min(day2data$Progress1.1)] = 1
#Subsetting data
day2data=day2data[day2data$Duplicate1.1==F & day2data$MinProgress1.1==F,]
# Importing day 3 data from qualtrics with lines 1 & 3 removed ------------
day3data = read.csv("day3data.csv", header=T, na.strings = "",
                    stringsAsFactors = F, strip.white = T)
#Removing needless columns
colnames(day3data)
day3data=day3data[,-c(3:4,7:9,13:17, 30:33, 39:42, 61:64, 66:69, 71:74,76)]
#Name columns
colnames(day3data)[1:7]=c("StartDate1.2", "EndDate1.2", "Progress1.2", "Duration1.2",
                          "LastName1.2", "FirstName1.2", "Email1.2")
colnames(day3data)[8:19]=c("PositiveAffect1.2", "NegativeAffect1.2", "PositiveAffect2.2",
                           "NegativeAffect2.2","NegativeAffect3.2","NegativeAffect4.2",
                           "PositiveAffect3.2", "NegativeAffect5.2","PositiveAffect4.2",
                           "PositiveAffect5.2","NegativeAffect6.2","PositiveAffect6.2")
colnames(day3data)[20:24] = paste0("LifeSatisfaction", 1:5, ".2")
colnames(day3data)[25:30] = paste0("Connectedness", 1:6, ".2")
colnames(day3data)[31:36] = paste0("Competence", 1:6, ".2")
colnames(day3data)[37:42] = paste0("Autonomy", 1:6, ".2")
colnames(day3data)[43:44] = c("PerformManipulationCheck1.1", "RecallManipulationCheck1.1")
colnames(day3data)[45]=c("Condition1.2")
# Setting correct data types
day3data$StartDate1.2=as.POSIXct(day3data$StartDate1.2, format="%m/%d/%Y %H:%M")
day3data$EndDate1.2=as.POSIXct(day3data$EndDate1.2, format="%m/%d/%Y %H:%M")
# Variables for cleaning the data
day3data$FullName = paste0(day3data$FirstName1.2, day3data$LastName1.2)
day3data$Duplicate1.2 = as.numeric(duplicated(day3data$FullName) | duplicated(day3data$FullName, fromLast = T))
day3data$MinProgress1.2 = 0
day3data$MinProgress1.2[day3data$Progress1.2 == min(day3data$Progress1.2)] = 1
#Subsetting data
day3data=day3data[day3data$Duplicate1.2==F & day3data$MinProgress1.2==F,]
# Merging data ------------------------------------------------------------
recdata=merge(day2data,day3data,by="FullName",all=T,sort=F)
#Remove entries not done on an acceptable date
recdata$Days1.2=round(recdata$EndDate1.2,units="days") - round(recdata$EndDate1.1,units="days")
recdata[(recdata$Days1.2 > 2 | is.na(recdata$Days1.2)==T), grep(".2$",colnames(recdata))]=NA
#removing people not assigned a condition at time 1 (didn't finish day 1)
recdata$Condition=recdata$Condition1.1
recdata=recdata[,-grep("Condition1", colnames(recdata))]
recdata=recdata[!is.na(recdata$Condition),]
#removing people who weren't paying attention
recdata$maxcons = 0
for (i in 1:nrow(recdata)) {
  recdata$maxcons[i] = max(rle(recdata[i,-grep("Date", colnames(recdata))])$lengths)
}
recdata=recdata[recdata$maxcons < 14,]
#manipulation check
plyr::count(recdata$RecallManipulationCheck1.1)
plyr::count(recdata$PerformManipulationCheck1.1)
recdata=recdata[recdata$RecallManipulationCheck1.1!=1 | 
                  is.na(recdata$RecallManipulationCheck1.1),]
recdata=recdata[recdata$PerformManipulationCheck1.1!=1 | 
                  is.na(recdata$PerformManipulationCheck1.1),]
#subsetting to only people that recalled
recdata = recdata[recdata$Condition == 2 | recdata$Condition == 4,]
#make condition code variables
recdata$ConditionCode1=0
recdata$ConditionCode1[recdata$Condition==2]=1
#reverse coding items
revnames=rep(c("Connectedness", "Competence", "Autonomy"), 3)
revnums=rep(c(2, 4, 6), each=3)
revs=paste0(revnames,revnums)
recdata[,grep(paste0("^", revs, collapse = "|"), colnames(recdata))]=8-
  recdata[,grep(paste0("^", revs, collapse = "|"), colnames(recdata))]

cor(recdata[,grepl("Connectedness", colnames(recdata)) & grepl("1$", colnames(recdata))]
    , use="pairwise.complete.obs")
#Asian and Latino dummy codes
recdata$Asian1.1 = 0
recdata$Asian1.1[recdata$Ethnicity1.1==2]=1
recdata$Latino1.1 = 0
recdata$Latino1.1[recdata$Ethnicity1.1==6]=1
#Renaming Conditions
recdata$Condition[recdata$Condition == "2"] <- "PerformAndRecall"
recdata$Condition[recdata$Condition == "4"] <- "Recall"
# Long Models Analyses #2 -------------------------------------------------
setwd("/Users/davidko/Desktop/Recall Study/AnalysesDay23")
reclongmodels2 = longmodels(
  measures = c("PositiveAffect", "NegativeAffect", "LifeSatisfaction", 
               "Competence", "Connectedness", "Autonomy"), 
  dataset = recdata,
  revinfo = NULL,
  multidims = NULL,
  cutvaritems = T,
  minitems = 4,
  timespace = "Day",
  knot = NULL,
  growthshape = "line",
  growthshape2 = "line",
  mods = NULL,
  tvcovs = NULL,
  overridesolgmeqs = NULL,
  write = T,
  updates = T,
  controlflwforintv = F
)
