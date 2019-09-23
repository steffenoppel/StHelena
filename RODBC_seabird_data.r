#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UTILITY SCRIPT TO EXTRACT DATA FROM THE SEABIRD TRACKING DATABASE FOR ST HELENA IN 32-bit R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(RODBC)
### Load data from database
#setwd("S:/ConSci/DptShare/SteffenOppel/RSPB/UKOT/StHelena/Science/Birds/seabirds")
setwd("C:/STEFFEN/RSPB/UKOT/StHelena/Science/Birds/seabirds")
db <- odbcConnectAccess2007('StHelena_seabird_tracking.accdb')
tracks <- sqlQuery(db, "SELECT * FROM EXPORT_trip_split")
deployments <- sqlQuery(db, "SELECT * FROM Logger_deployments")
retrievals <- sqlQuery(db, "SELECT * FROM retrievals")
seamounts <- sqlQuery(db, "SELECT * FROM Seamounts")
nests <- sqlQuery(db, "SELECT * FROM nests")
odbcClose(db)
rm(db)
save.image("C:\\STEFFEN\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds\\Seabird_Tracking_Data.RData")







