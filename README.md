# Berry Species Distributions

Data and code used to generate species distribution models and
in situ conservation scores included in the manuscript submission 
"Berries as a case study for crop wild relative conservation, use, 
and public engagement in Canada"

Please see metadata.xlsx for detailed data description of "berry_list" which is used for Supplemental Table S2 in the accepted paper.

## Data
### berries_list.csv
A list of 'berry' taxa that are native or naturalized in Canada. Includes brief
information on use, conservation status and genetic proximity to crop species where applicable

### geo_data
#### canada.geojson
shapefile used to define geographic area of Canada  

#### canada_ecoregions_clipped.geojson  
Level III ecoregions (obtained from U.S. Environmental Protection Agency) clipped to
canada.geojson spatial extent  

#### canada_provinces.geojson  
Multipolygon shapefile including unique polygons for each province area  

#### out_of_bound_ranges_provincial.csv  
List of provincial regions where species-specific occurrences potentially do not reflect
established populations

### occurrence_data  
#### berries_mapped.xlsx
notes on which berry species had issues in the initial creation of the species distribution maps, including the 13 species of Crataegus which were ultimately unable to be resolved.  

#### berry_occurrence.csv and berry_occurrence_part2.csv
GBIF records for all 206 Canadian berry species considered. Was too big for processing for
me to fit all in one csv file.  

#### occurrence_data_cleaned.csv  
This is the actual occurrence data used to train and test the species distribution models.
In 'Rfile_data_prep.R' I took the raw occurrence data 'berry_occurrence.csv' and 'berry_occurrence_part2.csv', and:  
- removed unnecessary columns,  
- removed rows with no lat/long data  
- removed duplicated observations of the same species from the same lat/long  
- filtered out points within 'out_of_bound_ranges_provincial.csv'  

#### occurrence_data_cleaned_thinned.csv
Used spThin() (with two rounds) to thin occurrence points within 10km to reduce spatial autocorrelation. The individual species files are in the folder 'thinned_occurrence_data', but here are combined into one .csv with all species. These data used to generate the species distribution models that are stacked in Figure S1

#### occurrence_data_cleaned_thinned.csv
occurrence_data_cleaned_thinned.csv minus any occurrence points from Crataegus spp.. These data used to generate the species distribution models that are stacked in Figure 1.  

### wc2.1_5m_bio_masked
Includes rasters for the standard 19 bioclimatic variables (https://www.worldclim.org/data/bioclim.html) and level III ecoregions, clipped to Canada.  

## R files
### Rfile_data_prep.R  
- clean occurrence data;  
- crop and mask bioclimatic data to appropriate geographic area.  

#### Rfile_model_sd.R  
Given cleaned occurrence data and bioclimatic data, test and train species specific models of habitat suitability in Canada using a MAXENT algorithm. Run in a batch across all species or all species excluding Crataegus, but can also be modified to run individually.

#### Rfile_model_output_visualization.R  
Plot Figures 1 and S1

#### Rfile_conservation_scores.R  
Use individual species distribution models and protected area data to calculate overlap/mismatch between suitable habitat (modeled in Rfile_model_sd.R) and Canada's protected areas.

## Outputs
### Stack and SSDM_no_crataegus
Currently not synced with github becuase of the file size, these two stacks include the output species distributions from running the maxent models, including uncertainty and model diagnostics. Will try to upload in pieces to overcome max file size upload constraints.  

### output_tables_and_figures
#### figure_1.pdf  
Berry species richness (excluding Cratagues spp.) mapped across Canada (A). Species distribution was mapped for each species and converted to a binary - habitat suitable or unsuitable for occurrence. The number of species for which habitat is suitable in each ~10km x 10km grid cell is summed to yield potential local species richness. Potential local species richness is mapped relative to the maximum number of species, from pink (low species richness) to green (high species richness). Protected areas are mapped in grey to display overlap/mismatch between protected areas and potential species richness. (B) and (C) display inset maps to focus overlap/mismatch in the regions of Canada with highest berry species richness, southwestern and southeastern Canada respectively. 

#### figure_S1.pdf
Berry species richness (including Cratagues spp.) mapped across Canada (A). Species distribution was mapped for each species and converted to a binary - habitat suitable or unsuitable for occurrence. The number of species for which habitat is suitable in each ~10km x 10km grid cell is summed to yield potential local species richness. Protected areas are mapped in grey to display overlap/mismatch between protected areas and potential species richness. (B) and (C) display inset maps to focus overlap/mismatch in the regions of Canada with highest berry species richness, southwestern and southeastern Canada respectively. 

#### in_situ_conservation_scores.csv
The caluclated proportion of suitable habitat for each species that overlaps with Canada's protected areas. Ranges from a score of 0/0% (no overlap between suitable habitat and protected area) to 1/100% (complete overlap). The mean overlap is 11.6%, sd = 5.9%.

#### test_protected_area_comparison_Amelanchier_cusickii.pdf
Demonstation of method used to calculate in situ conservation scores. For the test species, Amelanchier cusickii, blue cells represent area where habitat is predicted to be suitable but not protected and grey cells represent suitable habitat areas that overlap with the protected areas (black). In situ conservation score is then calculated as protected area overlap (grey cells) divided by total potential range size (blue cells + grey cells). 
