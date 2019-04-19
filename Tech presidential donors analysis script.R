### ANALYZING TECH DONORS TO PRESIDENTIAL CAMPAIGNS
### By Casey Tolan
### An analysis of presidential candidate campaign finance data to determine which candidates are getting the most donations from tech workers
### Quarter 1 2019 data for each candidate downloaded from the Federal Election Commission website
### Used for data analysis in this San Jose Mercury News article: https://www.mercurynews.com/2019/04/19/elizabeth-warren-president-tech-campaign-donations-berine-sanders-kamala-harris/

# Getting started
rm(list = ls()) # clear existing variables
directory <- read.csv("data/PresDirectory.csv",stringsAsFactors = F) # list of candidates and CSV files

resultsChart <- data.frame(matrix(ncol = 4, nrow = 1)) # this will contain the results of our analysis
colnames(resultsChart) <- c("CandidateName","IndividualItemizedTotal","BigTechTotal","ProgrammerTotal")

pb <- txtProgressBar(min = 0, max = nrow(directory), style = 3) # visualizing progress bar
for (row in 1:nrow(directory)) {

  # Get our data
  candidateName <- directory$CandidateName[row]
  rawReport <- read.csv(paste("data/Fundraising-reports-Q1/",directory$File[row],".csv",sep=""),stringsAsFactors=F)
  fecReport <- select(rawReport,FormType,CommitteeID,LastName,FirstName,Prefix,Suffix,Address1,Address2,City,State,Zip,ElectionCode,ContribDate,ContribAmount,AggregateAmount,Employer,Occupation,MemoCode)
  
  # Filter out individual donors
  indivContrib <-filter(fecReport, FormType == "SA17A", MemoCode != "X" ) # Individual donors
  totalSum <- sum(indivContrib[,]$ContribAmount) # Sum of all individual itemized contributions before refunds subtracted
  
  # Deal with refunds -- add them into contribution database as negative
  refunds <-filter(fecReport, FormType == "SB28A", MemoCode != "X" ) # Refunds
  refunds <- mutate(refunds, ContribAmount = ContribAmount * -1)
  donationsWithRefunds <- rbind(indivContrib,refunds) # The main contribution + refund database we will work with
  
  # Big tech analysis -- which donors are employees of Big Four tech firms
  bigTechDonors <- filter(donationsWithRefunds, grepl("Alphabet|Facebook|Google|Apple|Amazon",Employer))
  # Get rid of false positives and save them to review later
  bigTechDropped <- filter(bigTechDonors, grepl("Appleseed|Golden|Applewood|Applegate|Urgent|Applejack|Adecco|Outreach",Employer)) # Getting rid of false positives
  bigTechDonors <- filter(bigTechDonors, !grepl("Appleseed|Golden|Applewood|Applegate|Urgent|Applejack|Adecco|Outreach",Employer)) # Getting rid of false positives
  bigTechTotal <- sum(bigTechDonors[,]$ContribAmount)

  # Software engineer analysis -- which donors describe their occupation as software engineer or programmer
  programmerDonors <- filter(donationsWithRefunds, grepl("Software Engineer|Software engineer|software engineer|Programmer|programmer",Occupation))
  programmerTotal <- sum(programmerDonors[,]$ContribAmount)
  
  # Saving final results to export at the end
  myResults <- c(candidateName,totalSum,bigTechTotal,programmerTotal)
  resultsChart <- rbind(resultsChart,myResults)
  
  # Making it easier to read reports later by replacing committee ID number with candidate name -- optional
  if (nrow(bigTechDonors) > 0) { bigTechDonors[,]$CommitteeID = candidateName }
  if (nrow(programmerDonors) > 0) { programmerDonors[,]$CommitteeID = candidateName }
  if (nrow(bigTechDropped) > 0) { bigTechDropped[,]$CommitteeID = candidateName }
  
  # Saving list of donations to export at the end
  if (row == 1) {
    allBigTechDonors <- bigTechDonors
    allProgrammerDonors <- programmerDonors
    allbigTechDropped <- bigTechDropped
  } else {
    allBigTechDonors <- rbind(allBigTechDonors,bigTechDonors) 
    allProgrammerDonors <- rbind(allProgrammerDonors,programmerDonors) 
    allbigTechDropped <- rbind(allbigTechDropped,bigTechDropped) 
  }

  setTxtProgressBar(pb,row) # to visualize progress bar

}

write.csv(resultsChart, "output/FullResultsChart.csv")
write.csv(allBigTechDonors, "output/BigTechDonorsList.csv")
write.csv(allProgrammerDonors, "output/ProgrammerDonorsList.csv")
write.csv(allbigTechDropped, "output/BitTechDonorsDroppedList.csv")