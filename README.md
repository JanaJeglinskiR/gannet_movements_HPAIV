# gannet_movements_HPAIV
movement analysis of GPS tracked northern gannets


at submission state raw data is supplied in csv format
required data are:
1) GPS_tracking_data_gannet_15-2.csv
columns:
Year - year of data collection
GMT - date Time stamp in UTC
Latitude - geographic coordinate of Latitude
Longitude - geographic coordinate of Longitude
BIRD_ID - ID of GPS tracked gannet


2) Existing_gannet_coloniesNEAtlantic.csv
relevant columns:
Colony - name of gannet colony
ColId - Id of colony
Lat_rough - geographic coordinate of Latitude
Long_rough - geographic coordinate of Longitude


3) ref_data_April2022.csv
relevant columns:
animal.id - Bird_ID
animal_mates - Pair partner
animal.sex - sex of gannet

high resolution shapefile of country polygons can be obtained from Natural Earth:
https://www.naturalearthdata.com/downloads/10m-physical-vectors/
chose file ne_10m_admin_0_countries.shp



required code files to run analysis and create figures are
1) Analysis-colony_visit & max distance_HPAIV_gannet.R (also generates Figure1E)
2) Figure1D.R
3) Figure1_ABC.R

additional scripts allow accessing data from movebank and preparing the data for the analyses



