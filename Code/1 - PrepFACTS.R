##Prep FACTS data
#What in regions 1-6 has seedlots available
##Updated 15 Aug 2025
##Anya Metcalfe

# List of packages required for the script
package.list <- c("here", "tidyverse", "sf", "readxl", "stringdist", "data.table")
# Install packages if needed
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
for (i in package.list) {library(i, character.only = TRUE)}


#########################
###### Load Raw Data ####
#########################
setwd("/Volumes/GoogleDrive/My Drive/Seed Transfer Project/")
## Get all files extracted from the national FACTS tabular dataset (Paula SQL script - by state)
survFiles_paths <- list.files(here("Data", "Field", "UnitLevelSurvival_Raw", "CSV"),
                              pattern = "\\.csv$", # Matches files ending with .csv
                              full.names = TRUE)

# Read the header (and drop the first column) from one file to get all column names
sample_file_for_cols <- fread(survFiles_paths[1], nrows = 0, drop = 1)
all_column_names <- names(sample_file_for_cols)

# Create a 'colClasses' vector that forces ALL these columns to 'character'
all_cols_as_char <- setNames(rep("character", length(all_column_names)), all_column_names)

# Read each CSV file, dropping the first column, and force ALL columns to character class (avoids misinterp of classes)
survData <- rbindlist(lapply(survFiles_paths, function(file_path) {
  fread(file_path, drop = 1, colClasses = all_cols_as_char)}))

##Read in a single file path for QC (eg. CA)
#sdsub<- read.csv(survFiles_paths[2], header = TRUE, stringsAsFactors = FALSE)

# Extract a region column from SU_SECURITY_ID and create a new column
# Remove fully duplicated rows (based on all columns)
# This creates `sd2` as the working data.table for cleaning.
sd2<- as.data.table(unique(survData))

#######################################
##Drop bad records#####################
#######################################
sd2<-sd2[SUID!=""]
sd2<-sd2[CREATED_BY!="RMACT Migration"]  
sd2<-sd2[CREATED_BY!="RMRIS Migration"]  

######################################################################
#### Clean Up Duplicated Column Names Content (warning from fread) ###
######################################################################
# List of column names that might have duplicates and whose content should be merged
columns_to_merge_content <- c("EVENT_CN","SCI_SPECIES_CODE","SCI_SPECIES_TYPE_CODE","ELEVATION",
  "CREATED_BY", "CREATED_DATE", "CREATED_IN_INSTANCE","MODIFIED_BY","MODIFIED_DATE","MODIFIED_IN_INSTANCE")

# Iterate through each specified column name to merge content and remove duplicates
for (col_name in columns_to_merge_content) {
  col_indices <- which(names(sd2) == col_name)
  if (length(col_indices) > 1) {
    message("  Merging values for column: '", col_name, "' (found ", length(col_indices), " instances)")
    values_to_merge <- lapply(col_indices, function(idx) sd2[[idx]])
    merged_values <- Reduce(function(vec1, vec2) {
      mapply(function(x, y) {
        if (!is.na(x)) {return(x)
        } # If x is not NA, use x
        if (!is.na(y)) {return(y)
        } # Otherwise, if y is not NA, use y
        return(NA) # Otherwise, both are NA, return NA
      }, vec1, vec2, SIMPLIFY = TRUE) # SIMPLIFY = TRUE ensures a vector output
    }, values_to_merge)
    sd2[[col_indices[1]]] <- merged_values
    # Delete the subsequent duplicate columns
    cols_to_delete_indices <- sort(col_indices[-1], decreasing = TRUE) # All indices except the first one
    for (idx in cols_to_delete_indices) {
      sd2[[idx]] <- NULL}
  } else if (length(col_indices) == 1) {message("  Column '", col_name, "' found but is not duplicated. Skipping merging for this column.")
  } else {message("  Column '", col_name, "' not found in sd2. Skipping.")}}

#########################################################################
######## Initial Data Cleaning and SUID (Stand Unit ID) Preparation #####
#########################################################################
# Preserve Original SUID column before modifications
sd2$SUID_sd2 <- sd2$SUID

## Reformat SUID to get it out of scientific notation
sd2[, SUID:= ifelse(grepl("E", SUID),format(as.numeric(SUID), scientific = FALSE),SUID)]

# Pad SUID to 19 characters if its 18 (based on observed pattern)
# Add zero to any SUID that is less than 19 characters and doesn't start with zero
table(str_length(sd2$SUID))
sd2[, ID := ifelse(startsWith(SUID, "0") & nchar(SUID) == 19, SUID, paste0("0", SUID))]
sd2 <- sd2[SUID != "0"] # Remove any rows that might have become just "0"
table(str_length(sd2$SUID))

# Pad SU_SECURITY_ID to always be 6 characters (Region + Forest + Admin Forest)
sd2[, SU_SECURITY_ID := str_pad(SU_SECURITY_ID, width = 6, side = "left", pad = "0")]

# Extract Facts ID from column AU_ID_NAME_ORG, and pad it to end with 0 if too short
sd2[, FACTS_ID := substr(AU_ID_NAME_ORG, 1, 10)]

# Pad SUBUNIT to always be 3 characters and set to "000" if NA
sd2[, SUBUNIT := str_pad(SUBUNIT, width = 3, side = "left", pad = "0")]
sd2[is.na(SUBUNIT), SUBUNIT := "000"]

#Region col
sd2[, Region := substr(SU_SECURITY_ID, 1, 2)]
table(sd2$Region)

# Plot of regions in raw data (total rows - NOT unique SUID!!)
region_counts <- as.data.frame(table(sd2$Region))
names(region_counts) <- c("Region", "Count")

ggplot(region_counts, aes(x = Region, y = Count, fill = Region)) +
  geom_col(color = "black") +
  scale_fill_viridis_d() +
  labs(title = "Number of Records by Region",
       x = "Region", y = "Number of Records") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5), # Keep labels horizontal
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  geom_text(aes(label = Count), vjust = -0.5, size = 3.5, color = "black") # Add count labels on top

## Recreate the SUID field so that all are 19 characters and follow USFS standard SUID format
# Example: SUID 05 14 51 0010001000 005 - Region Number = 05, Admin Forest Number =14,
# District Number = 51, FACTS ID = 0010001000, Subunit = 005
sd2[, ID := paste0(SU_SECURITY_ID, FACTS_ID, SUBUNIT)]

## Compare new 'ID' with original 'SUID'
table(str_length(sd2$SUID_sd2))
table(str_length(sd2$ID))
sd2[ID != SUID, .N] # Count of records where new ID matches original SUID (345841 different, 174185 same)


######################
##### Species Data ###
######################
## Check if every row has a species - NO (as seen by sum(is.na(sd2$SppCode)))
sd2[, SppCode := SCI_SPECIES_CODE]

## Clean up and standardize SppCode values
sd2[SppCode == "2PLANT", SppCode := "2TREE"]
sd2[SppCode == "2TEN", SppCode := "2TREE"]
sd2[SppCode == "2TN", SppCode := "2TREE"]
sd2[SppCode == "2AB", SppCode := "2TREE"]
sd2[SppCode == "2PLANT", SppCode := "2TREE"] # Duplicated entry
sd2[SppCode == "2SD", SppCode := "2TREE"]
sd2[SppCode == "2TEN", SppCode := "2TREE"] # Duplicated entry
sd2[SppCode == "2TN", SppCode := "2TREE"] # Duplicated entry
sd2[SppCode == "ABIES", SppCode := "ABPR"]
sd2[SppCode == "ABLA2", SppCode := "ABLA"]
sd2[SppCode == "ABLAA", SppCode := "ABLA"]
sd2[SppCode == "ABMAS", SppCode := "ABMA"]
sd2[SppCode == "ACMA3", SppCode := "ACMA"]
sd2[SppCode == "DOUGL", SppCode := "PSME"]
sd2[SppCode == "TSME", SppCode := "TSHE"]
sd2[SppCode %in% c("LACO", "LAOC2", "LAOC3"), SppCode := "LAOC"]
sd2[SppCode == "LARIX", SppCode := "LAOC"]
sd2[SppCode %in% c("PICOC", "PICOL", "PICO3"), SppCode := "PICO"]
sd2[SppCode %in% c("PIEN1", "PIEN2"), SppCode := "PIEN"]
sd2[SppCode %in% c("PIMO3", "PIMO5"), SppCode := "PIMO"]
sd2[SppCode %in% c("PINUS", "P1P0", "PIPOC", "PIPOP", "PIPOS","PIPO2", "PIPOJ", "PIPOW", "PIPU"), SppCode := "PIPO"]
sd2[SppCode %in% c("PSEUD7", "PSMEG", "PSMEM", "PSMA", "PSME2"), SppCode := "PSME"] # Duplicated block
sd2[SppCode %in% c("PSEUD7", "PSMEG", "PSMEM", "PSMA", "PSME2"), SppCode := "PSME"] # Duplicated block

table(sd2$SppCode) #Many missing SppCode

########################
####### Seedlot Data ###
########################
sd2$Seedlot<-sd2$SEEDLOT
sd2$Seedlot<-ifelse(sd2$Seedlot=="",NA,sd2$Seedlot) #Ensure blanks are ==NA

##Clean seedlot data (wrapped in a function for later use)
clean_seedlot_data <- function(dt) {
  # Create a copy of the data.table to avoid modifying the original object passed to the function
  # This ensures the function is non-destructive on the input variable outside its scope.
  dt_cleaned <- copy(dt)
  
  # --- Input Validation (Optional but Recommended for robust functions) ---
  if (!inherits(dt_cleaned, "data.table")) {
    stop("Input 'dt' must be a data.table.")}
  if (!"Seedlot" %in% names(dt_cleaned)) {
    stop("Column 'Seedlot' not found in the input data.table.")}
  if (!"SppCode" %in% names(dt_cleaned)) {
    warning("Column 'SppCode' not found. Prefixing based on SppCode will be skipped.")
    spp_code_present <- FALSE
  } else {spp_code_present <- TRUE}
  
  # Ensure Seedlot is character for string operations
  if (!is.character(dt_cleaned$Seedlot)) {
    dt_cleaned[, Seedlot := as.character(Seedlot)]}
  
  # Make all seedlot characters capital letters
  dt_cleaned[, Seedlot := toupper(Seedlot)]
  # Remove all characters that are not numbers or letters
  dt_cleaned[, Seedlot := gsub("[^A-Za-z0-9]", "", Seedlot)]
  # Convert "0" (as a character string) to NA in the Seedlot column
  dt_cleaned[, Seedlot := fifelse(Seedlot == "0", NA_character_, Seedlot)]
  # Reorder Seedlot if it matches the pattern (e.g., moves trailing two letters to the front)
  # This applies to the 'cleaned' Seedlot, e.g., "123AB" -> "AB123"
  dt_cleaned[, Seedlot := sub("^(.*[0-9])([A-Za-z]{2})$", "\\2\\1", Seedlot)]
  
  # Set Seedlot to NA if its length is less than 4 characters (and it's not already NA)
  dt_cleaned[nchar(Seedlot) < 4 & !is.na(Seedlot), Seedlot := NA_character_]
  
  # --- Add SppCode prefix where applicable ---
  if (spp_code_present) {
    # Define the SppCode to Prefix mapping lookup table
    spp_prefix_map <- data.table(
      SppCode = c(
        "PIPO", "ABMAS", "ABMA", "ABCO", "ABPR", "ACMA", "CADE27", "PSME", "PSMEM",
        "LAOC", "LADE5", "LIDE", "PIAL", "PICO", "PILA", "PIMO", "PIMO3", "PISTM",
        "PIJE", "PIEN", "PIEN1", "PIFL2", "TSHE", "SABE2", "SEGI2", "THPL"),
      Prefix = c(
        "PP", "RF", "RF", "WF", "NF", "BM", "WF", "DF", "DF",
        "WL", "EL", "LP", "WB", "LP", "SP", "WWP", "WWP", "EWP",
        "PJ", "ES", "ES", "FP", "TH", "S2", "GS", "WR"))
    
    # Add a temporary column for the calculated prefix by joining with spp_prefix_map
    dt_cleaned[spp_prefix_map, on = "SppCode", calculated_prefix := i.Prefix]
    
    # Conditionally update Seedlot with the prefix:
    # Only apply if SppCode is NOT NA AND calculated_prefix is NOT NA AND
    # the existing Seedlot is NOT NA AND the calculated_prefix does not match
    # the first two characters of the existing Seedlot.
    dt_cleaned[!is.na(SppCode) & !is.na(calculated_prefix) & !is.na(Seedlot) &
                 substr(Seedlot, 1, 2) != calculated_prefix,
               Seedlot := paste0(calculated_prefix, Seedlot)]
    
    # Specific SppCode-based Seedlot assignments that don't fit the general prefix pattern
    dt_cleaned[SppCode == "2TN" & grepl("^[0-9]+$", Seedlot), Seedlot := NA_character_]
    
    # Specific Seedlot string replacements (e.g., "PSME..." -> "DF...")
    dt_cleaned[grepl("^PSME", Seedlot), Seedlot := sub("^PSME", "DF", Seedlot)]
    dt_cleaned[grepl("^RTB", Seedlot), Seedlot := sub("^RTB", "WWP", Seedlot)]
  # Remove the temporary prefix column
    dt_cleaned[, calculated_prefix := NULL]
  } else {    message("Skipping SppCode-based prefixing as 'SppCode' column is missing.")}
  
  # --- Manual fixes based on Seedlot_FromNotes (Conditional Inclusion) ---
  # These lines are highly specific and depend on 'Seedlot_FromNotes' column being present
  # and specific values in it. They will only run if the column exists.
  if ("Seedlot_FromNotes" %in% names(dt_cleaned)) {
    # Assuming these are literal fixes, not patterns after initial cleaning
    dt_cleaned[Seedlot_FromNotes == "5870075", Seedlot := "LP05870075"]
    # This rule assigns a string with a comma; consider if Seedlot should be a single ID.
    dt_cleaned[Seedlot_FromNotes == "LP05890074", Seedlot := "LP05890074, LP05840068"]
  } else {
    message("Skipping 'Seedlot_FromNotes' specific fixes as the column is missing.")}
  
# --- Final cleanup of NA-like strings ---
dt_cleaned[Seedlot == "XXXX", Seedlot := NA_character_]
dt_cleaned[Seedlot == "XXXXX", Seedlot := NA_character_]

return(dt_cleaned)
}
sd2<-clean_seedlot_data(sd2) #Run and assign the function

###########################################
### ## Add Planting Date Columns ##########
###########################################
# Date_Completed generally has the most unique and flushed out values of the many date fields
    sd2[, DATE_COMPLETED := as.Date(DATE_COMPLETED, format = "%m/%d/%Y")]
    sd2[, Year_COMPLETED := year(DATE_COMPLETED)]
    
    ## Create new columns for Min and Max Planting Date,
    ## based on `ID` and only using activity dates from planting events
    sd2[, `:=`(FirstPlantingDate = if (any(ACTIVITY %in% c("Plant Trees", "Fill-in or Replant Trees"))) {
      min(DATE_COMPLETED[ACTIVITY %in% c("Plant Trees", "Fill-in or Replant Trees")], na.rm = TRUE)
    } else {NA},
    LastPlantingDate = if (any(ACTIVITY %in% c("Plant Trees", "Fill-in or Replant Trees"))) {
      max(DATE_COMPLETED[ACTIVITY %in% c("Plant Trees", "Fill-in or Replant Trees")], na.rm = TRUE)
    } else {NA}), by = ID]
    
    ## New column nPlantings and nSurveys: total number of planting events/surveys for any ID
    sd2[, `:=`( nPlantings = sum(ACTIVITY %in% c("Plant Trees", "Fill-in or Replant Trees")),
                nSurveys = sum(ACTIVITY == "Plantation Survival Survey")), by = ID]
    
    ###Planting Date
    
    ## Planting year is (first vs. last) or actual year of planting for planting activities
    sd2[, PlantingDate := fifelse(ACTIVITY %in% c("Fill-in or Replant Trees", "Plant Trees"),
                                  DATE_COMPLETED,
                                  fifelse(DATE_COMPLETED < LastPlantingDate,
                                          FirstPlantingDate, LastPlantingDate)), 
        by = .(ID,SppCode,Seedlot)]
    sd2$PlantingDate <- as.Date(sd2$PlantingDate)
    
    ggplot(sd2) +  geom_histogram(aes(x = PlantingDate), fill = "lightblue", col = "black", binwidth=1000) +  theme_classic()
    
# Move other useful seed data from Planting Activities to all rows
# that have the same ID and planting year (fills NAs based on the first non-NA entry)
    sd2$NurseryCode <- sd2$NURSERY_CODE
    sd2$SeedlingActualUnits <- sd2$SEED_ACTUAL_NBR_UNITS
    sd2$SeedlingUOM <- sd2$SEEDLING_UOM
    sd2$Seedzone <- sd2$SEED_BREEDING_ZONE
    
    # Create a lookup for all relevant seed-related columns
    sd_lookup <- sd2[, .(
      NurseryCode = first(NURSERY_CODE),
      SeedlingActualUnits = first(SEEDLING_ACTUAL_NBR_UNITS),
      SeedlingUOM = first(SEEDLING_UOM),
      SEED_SOURCE_CODE = first(SEED_SOURCE_CODE),
      SEEDLING_STOCK_TYPE_CODE = first(SEEDLING_STOCK_TYPE_CODE),
      Seedzone = first(Seedzone)),
      by = .(ID, SppCode, PlantingDate)]
    
    # Fill NA values in main data.table using the lookup table
    sd2[is.na(NurseryCode), NurseryCode := sd_lookup[.SD, on = .(ID, SppCode, PlantingDate), NurseryCode]]
    sd2[is.na(SeedlingActualUnits), SeedlingActualUnits := sd_lookup[.SD, on = .(ID, SppCode, PlantingDate), SeedlingActualUnits]]
    sd2[is.na(SeedlingUOM), SeedlingUOM := sd_lookup[.SD, on = .(ID, SppCode, PlantingDate), SeedlingUOM]]
    sd2[is.na(SEED_SOURCE_CODE), SEED_SOURCE_CODE := sd_lookup[.SD, on = .(ID, SppCode, PlantingDate), SEED_SOURCE_CODE]]
    sd2[is.na(SEEDLING_STOCK_TYPE_CODE), SEEDLING_STOCK_TYPE_CODE := sd_lookup[.SD, on = .(ID, SppCode, PlantingDate), SEEDLING_STOCK_TYPE_CODE]]
    sd2[is.na(Seedzone), Seedzone := sd_lookup[.SD, on = .(ID, SppCode, PlantingDate), Seedzone]]
    

############################################
####CREATE NON_SURVIVAL-DATASET#############
############################################
##Reduce columns and collapse into non-survival FACTS data for seed transfer dataset
NSD<-sd2[,c(139,136,137,140,29,74,48,111,112,14:144,149,147,148,134,150,151)]
grouping_cols <- c("ID", "SppCode", "Seedlot")
other_cols <- setdiff(names(NSD), grouping_cols)


##Region 1 tomfoolery
#NSD with bonus region 1 data that has good seedlot data
R1BonusLots<-as.data.table(read.csv(here("Data","Field","UnitLevelSurvival_Raw","R1_SurvivalData_UpdatedCoords.csv")))
setnames(R1BonusLots,12,"SppCode")
R1BonusLots$Region<-"01"
#Create an appx date base on year + season
R1BonusLots[, `:=`(temp_month_day = fifelse(Season == "Spring", "03-01",
                  fifelse(Season == "spring", "03-01",
                  fifelse(Season == "Summer", "06-01",
                  fifelse(Season == "Fall", "09-01", NA_character_)))))]
#Combine PlantingYear with the determined month and day, and convert to Date class
R1BonusLots[, PlantingDate:= ymd(paste(PlantingYear, temp_month_day, sep = "-"))]
R1BonusLots[, temp_month_day := NULL]

r1<-R1BonusLots[,c(6,39,12,25,19,40,24,15)]
r1<-clean_seedlot_data(r1) ##Runs script from above to reformat and standardize seedlot codes

##ASSUME THAT R1 SEEDLOTS ARE ALL CDA## **source spreadsheet did not provide nursery code and this is most common nursery for R1
r1$NurseryCode<-"CDA"


#Get rid of any pre Summer 2022 Region 1 data
STD<- NSD[!(Region == "01" & PlantingDate < as.Date("2022-06-01"))]

#Combine with R1BonusLots
r1[, SEED_SOURCE_CODE := as.character(SEED_SOURCE_CODE)]
STD<- rbindlist(list(STD, r1), fill = TRUE)

#Remove duplicate rows
STD<-STD[!duplicated(STD)]

#Remove anything without seedlot
STD<-STD[!is.na(STD$Seedlot)]
setnames(STD,"NBR_PLANTED_TREES_STAKED","nPlanted")
setnames(STD,"NBR_STAKED_TREES_SURVIVING","nSurvived")

###EXPORT to merge with nursery data
setwd("/Volumes/GoogleDrive/My Drive/Seed Transfer Project/Data/Merging/")
write.csv(STD,"STDnolocs.csv")

dim(STD)
length(unique(STD$Seedlot))





###PLOTS###

##Plot of unique seedlots (no nursery match) by region
####Barplot of seedlots by region
unique_seedlot_counts_by_region <- STD %>%
  filter(!is.na(Region)) %>% 
  group_by(Region) %>%
  summarise(UniqueSeedlotCount = n_distinct(Seedlot, na.rm = TRUE), # Count unique non-NA Seedlot values
    .groups = 'drop') %>%  arrange(Region)

# 2. Create the bar plot
ggplot(unique_seedlot_counts_by_region, aes(x = Region, y = UniqueSeedlotCount, fill = Region)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) + 
  geom_text(aes(label = UniqueSeedlotCount), vjust = -0.5, size = 3.5) + # counts on top of bars
  labs(title = "Number of Unique Seedlots per Region (FACTS)", subtitle="(not matched with seed/planting locations)",
    x = "Region",
    y = "Count of Unique Seedlots") +
  theme_minimal() + # A clean theme
  theme(
    axis.text.x = element_text(hjust = 1), 
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5) )

##Really so many in region 5?
STD %>%
  filter(Region == "05") %>%
  distinct(Seedlot)
