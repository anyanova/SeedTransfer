Code to prep FACTS data and then merge it with NURSERY data and PLANTING LOCS.
1. Prep Facts - Output file: STDnolocs.csv
2. Merge with Nursery Data - Output file: FACTS_Nursery_5Aug.csv
3. Merge with Planting Locations - Output Files: SeedTransferData_Aug2025.RDS (No survival Data) AND SeedTransferData_PlusSurvFromMarshallAndNeb_Aug2025.RDS (Some survival data)
4. Clean up location geometries and add in elevation using ElevatR - Output File: SeedTransferData_Elev_Aug2025.RDS
5. Extract TopoTerra data from OSF and merge with STDsf - Output File: SeedTransferData_OSFclim_Aug2025.rds
6. Build density plots with TopoTerra data - Output Files: STDsf_sorted_by_geodist.csv & 6pan_geoclim.png
7. Build dataset with survival data - *in progress*
