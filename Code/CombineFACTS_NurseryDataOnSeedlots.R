##Combine FACTS data with nursery/seedlots data
setwd("/Volumes/GoogleDrive/My Drive/Seed Transfer Project")
package.list <- c("here", "tidyverse", "sf", "readxl", "stringdist","data.table", "geosphere","dplyr")
  
  ## Installing them if they aren't already on the computer
  new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  ## And loading them
  for(i in package.list){library(i, character.only = T)}
  
##Load in seedlot data from nurseries
  NurseryData<-as.data.table(read.csv(here("Data", "Merging", "NurseryDataCombined2023.csv")))

#Standardize nursery names with FACTS
  NurseryData$NurseryCode<-ifelse(NurseryData$Nursery=="Bessey","BES",
        ifelse(NurseryData$Nursery=="CdA","CDA",
        ifelse(NurseryData$Nursery=="Lucky","LPK",
        NurseryData$Nursery)))

#Facts, formatted and cleaned up to have seedlot data 
  FS<-as.data.table(read.csv(here("Data","Merging","STDnolocs.csv")))
  FS[, X := NULL]
  FS$SUID_OG<-as.character(FS$SUID_sd2)
  table(str_length(FS$ID)) #Recreated SUID
  table(str_length(FS$SUID_OG)) #Original SUID
  table(FS$Region)
  dim(FS)

##Aug 15th change - Exclude rows that don't specify nursery from FACTS data
table(FS$NurseryCode, useNA="always")  
FS<-FS[NurseryCode!=""]
  
  
#Merge FS and NurseryData into "FSND"
#Allow for partial matches if any part of seedlot in Nursery Data matches FS (which might have species name included in the front from tidying)
#Require that nursery matches between nursery and FACTS data, because sometimes there are duplicated seedlots among different nurseries

merged_rows <- list()
# Loop through each row in NurseryData 
for (i in 1:nrow(NurseryData)) {
  nd_seedlot <- NurseryData$Seedlot[i]
  
  # Check for complete matches first, requiring both Seedlot and NurseryCode to match
  complete_matches <- FS[Seedlot == nd_seedlot & NurseryCode == NurseryData$NurseryCode[i]]
  if (nrow(complete_matches) > 0) {
    complete_matches[, Matched_Substring := "Complete Match"]
    complete_matches <- cbind(complete_matches, NurseryData[i, !"Seedlot"])
    merged_rows[[i]] <- complete_matches
  } else {
    # If not a complete match, then check for substring matches that have at least 4 matching characters, also requiring a matching nursery code
    substring_matches <- FS[grepl(nd_seedlot, Seedlot) & NurseryCode == NurseryData$NurseryCode[i]]
    if (nrow(substring_matches) > 0) {
      # Substring match found
      if (nchar(nd_seedlot) >= 4) { #check the length of the string
        substring_matches[, Matched_Substring := nd_seedlot]
        substring_matches <- cbind(substring_matches, NurseryData[i, !"Seedlot"])
        merged_rows[[i]] <- substring_matches
      }}}}
##
# Combine the results into a single data.table
FSND <- rbindlist(merged_rows)

unique(FSND$Matched_Substring)
table(FSND$Matched_Substring)
table(FSND$Nursery)
table(FSND$Region)

dim(FSND)
length(unique(FSND$ID))
length(unique(FSND$SUID_OG))
length(unique(FSND$Seedlot))

##Filter so that if it is not a perfect match, species have to match (first 4 charachters to exclude numbers and PIPO=PIPOS)
first_four <- function(x) {substr(x, 1, 4)}
partial_matches <- FSND[  Matched_Substring != "Complete Match" & first_four(SeedlotSpp) == first_four(SppCode)]
complete_matches <- FSND[Matched_Substring == "Complete Match"]
MS<- rbindlist(list(complete_matches, partial_matches)) #MS=Merged Seedlots

##Also they need to actually have spatial data associated with where seeds were collected
#Which rows have SeedLAT available? (this dropped 24 seedlots)
#MS<-MS[!is.na(SeedLAT)]


##Drop duplicates, prioritize complete_matches
MS <- MS[order(-(Matched_Substring == "Complete Match"))]
#MS[duplicated(MS, by = c("ID", "Seedlot"))]
dim(MS)

# Create a new combined column for NurseryCode
MS[NurseryCode != NurseryCode, NurseryCode := NurseryCode]

MS<-MS[,-159]

FS[!is.na(Seedlot)] #FACTS data that originally had seedlots: n=17,296
length(unique(NurseryData$Seedlot)) #6271 seedlots in 2023 nursery data
length(unique(FS$Seedlot))
length(unique(FS$SUID))
length(unique(FS$SUID_OG))
dim(MS)
length(unique(MS$Seedlot))
length(unique(MS$ID))
length(unique(MS$SUID_OG)) 
table(MS$Nursery, useNA="always") #From all 3 nurseries
table(MS$SeedlotSpp, useNA="always")
length(MS$SeedlotSpp)
table(MS$Region)

setwd("/Volumes/GoogleDrive/My Drive/Seed Transfer Project/Data/Merging/")
write.csv(MS,"FACTS_Nursery_5Aug.csv", row.names =FALSE)





###Exploration
#Plot seedlots by region
unique_seedlots_per_region <- MS%>%
  group_by(Region) %>%  summarise(UniqueSeedlotCount = n_distinct(Seedlot), .groups = 'drop') %>%
  mutate(Region = as.factor(Region))

ggplot(unique_seedlots_per_region, aes(x = Region, y = UniqueSeedlotCount, fill = Region)) +
  geom_col(color = "black", width = 0.8) + # geom_col uses y for height, add black border
  scale_fill_viridis_d(option = "viridis") + # A visually appealing color palette
  labs(title = "Number of Unique Seedlots by Region", subtitle="Matched with nursery data (Dataset 'MS')",
       x = "Region",
       y = "Number of Unique Seedlots",
       fill = "Region") +
  theme_classic() +  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10), 
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none") +
  geom_text(aes(label = UniqueSeedlotCount),
            vjust = -0.5, # Position slightly above the bar
            size = 4,     # Font size for the labels
            color = "black")

