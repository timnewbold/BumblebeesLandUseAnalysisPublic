import platform
import socket

# Define the locations of programs
if 'WCMC-LT-01636' == socket.gethostname():
    # Tim's Windows PC
    arcpy = 'c:/python26/ArcGIS10.0/python.exe'
    python = 'c:/Python34/python.exe'
    R = 'c:/Program Files/R/R-3.1.2/bin/x64/Rscript.exe'
elif 'WCMC-PC-01686' == socket.gethostname():
    arcpy = 'c:/python26/ArcGIS10.0/python.exe'
    python = 'c:/Python34/python.exe'
    R = 'c:/Program Files/R/R-3.1.2/bin/x64/Rscript.exe'
elif 'ucbttne-PC' == socket.gethostname():
    # Tim's UCL PC
    arcpy = 'c:/python27/ArcGIS10.3/python.exe'
    python = 'c:/Python35/python.exe'
    R = 'c:/Program Files/R/R-3.3.2/bin/x64/Rscript.exe'
elif 'ucbttne-LT' == socket.gethostname():
    # Tim's UCL laptop
    arcpy = 'c:/python27/ArcGIS10.3/python.exe'
    python = 'c:/Python35/python.exe'
    R = 'c:/Program Files/R/R-3.2.2/bin/x64/Rscript.exe'
elif 'ucbttne-PC2' == socket.gethostname():
    arcpy = 'c:/python27/ArcGIS10.4/python.exe'
    python = 'c:/Python36/python.exe'
    R = 'c:/Program Files/R/R-3.3.2/bin/x64/Rscript.exe'
elif 'ucbttne-LT2'==socket.gethostname():
    arcpy = 'c:/python27/ArcGIS10.4/python.exe'
    python = 'c:/Python36/python.exe'
    R = 'c:/Program Files/R/R-3.3.2/bin/x64/Rscript.exe'

STAGES = [ ('1', R,              '1_PrepareMapData.R'),
           ('2', R,              '2_PrepareDiversityData.R'),
           ('3', R,              '3_RunSpeciesLevelModels.R'),
           ('4', R,              '4_PlotSpeciesLevelModels.R'),
           ('5', R,              '5_ProjectModelsLandUse.R'),
           ('6', R,              '6_ProjectModelsClimate.R'),
           ('7', R,              '7_ProjectModelsLandUseAndClimate.R'),
           ('8', R,              '8_ProjectModelsLandUseAndClimateNoClimateChange.R'),
           ('9', R,              '9_CompareProjections.R'),
           ('10', R,             '10_ProjectModelsLandUseFuture.R'),
           ('12', R,             '12_ProjectModelsLandUseAndClimateFuture.R'),
           ('13', R,             '13_ProjectModelsLandUseAndClimateNoClimateChangeFuture.R'),
           ('14', R,             '14_CompareProjectionsFuture.R'),
           ('15', R,             '15_PlotSites.R'),
           ('16', R,             '16_ReferencesPreprocess.R'),
           ('17', python,        '17_ProcessEndnote.py'),
           ('18', R,             '18_MapBumblebeeDependentCrops.R'),
           ('19', R,             '19_CompareProjectionsWithBumblebeeDependentCropProduction.R'),
           ('20', R,             '20_PrepareRawClimateData.R'),
           ('21', R,             '21_CompareRawCimateModels.R'),
         ]
