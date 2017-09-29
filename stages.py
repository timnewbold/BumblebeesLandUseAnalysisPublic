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

STAGES = [ ('1', R,              '1_PrepareDiversityData.R'),
           ('2', R,              '2_PrepareSiteData.R'),
           ('3', R,              '3_RunModels.R'),
         ]
