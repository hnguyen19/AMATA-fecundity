The clean and raw data and code are available in this repository. The clean data set is also available at Iowa State University's DataShare.

Variable explaination in each data sheet is provided below:  

#### General variables (column names) that exist in all the data sheets
+ Plot: Two-digit plot identification at the experiment with the first digit represents the block number (1, 2, 3, 4) and the second the plot number (1, 2, ..., 9).   
+ Block: Block number (1, 2, 3, 4).  
+ Side: E(ast) or W(est), to double check if the Corn_weed_management designation is correctly recorded according to the experiment map.   
+ Crop: Crop species name.  
+ Crop_ID: Alpha-numeric code for the combination of crop species (first letter in species name) and the rotation (2-, 3-, or 4-year rotation) to which they belonged
+ Corn_weed_management: `conv` or `low` - the weed management applied to the corn phase.
+ Year: The year that the data was collected, 2018 or 2019.  

#### AMATA_18.csv and AMATA_19.csv (AMATA is the Bayer code for *Amaranthus tuberculatus*), these two sheets contains the totals by experimental unit for plant density, plant biomass, number of female and male, and female proportion)
+ AMATA_number: Total number of waterhemp plants found in eight quadrats within an experimental unit (eu).
+ AMATA_gram: Total aboveground mass of AMATA_number in eight quadrats within each eu.
+ sample_area_m_sq: Total sampled area per eu in meter squared.
+ g_m_sq: Total aboveground mass of AMATA per eu in gram per meter squared .
+ plant_m_sq: Total AMATA density in each eu in plant per meter squared.
+ Rotation: cropping system: 2-year, 3-year, or 4-year  
+ Total: Number of sexed waterhemp plants in the whole eu (Total > AMATA_number because the whole eu was scouted until 100 plants were sexed or as many as possible)
+ Female: Number of female waterhemp plants in the whole eu.  
+ Male: Number of male waterhemp plants in the whole eu.  
+ F_prop; Female/Total.

*Note: Sex ratio evaluation procedure was different in 2018 and 2019.*

#### fecundity_18.csv (all the plants in this sheet were female), this sheet contains individual dried aboveground mass and number of seeds
+ Herbicide: The herbicide regime applied to the corn and soybean phase of a particular cropping system *might remove*
+ id: Plant Identity within an eu
+ Biomass: Dried aboveground mass of an individual plant
+ Seed: Number of seed of an individual plant
+ bt: A dummy variable created by combining the block and treatment (CropID x Corn_weed_management) to accommodate the `gls`'s compound symmetry.  


#### sexed19_mis.csv
+ Quadrat: Identification of a quadrat (1,2,...8) within an eu.
+ Cohort: The cohort (1, 2, ..., 6) that a plant belonged. 
+ Unknown: Number of plants whose sex is unknown in a quadrat within an eu. 
+ Female: Number of female plants in a quadrat within an eu. 
+ Male: Number of male plants in a quadrat within an eu. 
+ ID: The combination of Plot, Side, Quadrat, and Cohort
+ Number_begin: Number of emerged seedling at the beginning of the crop season.
+ Total_sexed: Female \+ Male per each quadrat in each eu.  

#### sexed19_m24.csv - 24 simulated data sets based on sexed19_mis.csv using the `mice` package. 
+ .imp: 1,2,...,24 imputation identification 
+ Female: Number of female plants in an eu, tallied over all quadrats. 
+ Male: Number of male plants in an eu, tallied over all quadrats. 
+ Total: Female \+ Male in an eu, tallied over all quadrats. 
