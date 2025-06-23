#Turbidity

#"A standard optical parameter measuring water turbidity or the inverse of transparency is the attenuation of downwelling light at 490 nm,
#Kd490 or simply Kd (m-1).  Currently the Kd data are included not at 1 km resolution but at 4 km resoltion. Otherwise the grid is the same.
#The current Kd data are based on the ESA Ocean Colour Climate Change Initiative (OC-CCI) version 5.0
#(Sathyendranath et al., 2019, https://esa-oceancolour-cci.org/).  The Lee et al. (2005) equation and bbw from Zhang et al. (2009) are used."

#https://spg-satdata.ucsd.edu/ca1km/

#California merged satellite-derived 1-km dataset
#Mati Kahru, mkahru@ucsd.edu    Updated : 9/20/2021

#The full-resolution (1-km) datasets with numerical values are in HDF4 format.
#The full image is 3840 pixels wide and 3405 pixels high.

#The latitudes and longitudes corresponding to each pixel can be obtained form HDF4 file cal_aco_3840_Latitude_Longitude.hdf  in http://wimsoft.com/CAL/files/.
#The scaling equations using pixel value (PV) as unsigned byte (from 0 to 255) are:

#SST (deg C) = 0.15 * PV - 3.0

#Chl (mg m-3) = 10^(0.015 * PV - 2.0), i.e. 10 to the power of 0.015 * PV - 2.0.

#Pixel values 0 and 255 (and the corresponding scaled values) are considered invalid and must be excluded in any statistics.

#The following example downloads all *.hdf files from folder California_Current/1978/C1978_chl_month/:
wget -r -l1 -nd --no-parent -A.hdf http://www.wimsoft.com/CAL/1978/C1978_chl_month/