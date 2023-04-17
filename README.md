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


3) Existing_gannet_coloniesNEAtlantic.csv
relevant columns:
Colony - name of gannet colony
ColId - Id of colony
Lat_rough - geographic coordinate of Latitude
Long_rough - geographic coordinate of Longitude


5) ref_data_April2022.csv
relevant columns:
animal.id - Bird_ID
animal_mates - Pair partner
animal.sex - sex of gannet

7) high resolution shapefile of country polygons obtrained from Natural Earth supplied as ne_10m_admin_0_countries.shp



required code files to run analysis and create figures are
1) Analysis-colony_visist& max distamce_HPAIV_gannet.R (also generates Figure1E)
2) Figure1D.R
3) Figure1_ABC.R



