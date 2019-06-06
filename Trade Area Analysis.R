############################################################################################################
########################################     Loading Libraries    ##########################################
############################################################################################################
a <- Sys.time()
print(a)
library(dplyr)
library(readxl)
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

############################################################################################################
#####################################     Setting Working Dir Path    ######################################
############################################################################################################
a <- Sys.time()
print(a)
setwd("C:/Users/vivek.kumar8/OneDrive - Shell/NFR Pricing Framework/DE/Site segmentation")
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)


############################################################################################################
############     Loading Alteryx Output with Site combination and Common distinct customers      ###########
############################################################################################################
a <- Sys.time()
print(a)
input_data=read.csv("Site_Customer_Combination_Out_SiteIDs_Saperated.csv",header=TRUE)
summary(input_data)
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

############################################################################################################
############################     extracting data needed and getting quartiles    ###########################
############################################################################################################
a <- Sys.time()
print(a)
edge_data = input_data %>% select(Site_combi1,Site_combi2,Overlap_cnt)
colnames(edge_data) = c("from","to","weight")
head(edge_data)

quantile(edge_data$weight,c(0.25,0.5,0.6,0.7,0.8,0.9,0.95,0.96,0.97,0.98,0.99))
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)


############################################################################################################
#############################     Reading GSD Data for Latest Store Details    #############################
############################################################################################################
a <- Sys.time()
print(a)
input_data1=read_xls("20190531_GERMANY_SiteData.xls",
                     sheet = "Site_Basic_Data", 
                     col_names = TRUE, 
                     col_types = c("text", "numeric", "text",
                                   "text", "text", "text", "numeric", 
                                   "text", "text", "date", "date", "numeric",
                                   "text", "numeric", "numeric", "numeric",
                                   "numeric", "text", "text", "text",
                                   "text", "text", "text", "text", "text",
                                   "text", "date", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "text", "text", "text",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "text", "text", "text", "numeric",
                                   "numeric", "text", "text", "text",
                                   "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", 
                                   "numeric", "numeric", "date", "numeric", 
                                   "date", "text", "text", "date", "numeric"))

names(input_data1)[1] <- "Site_Type"
names(input_data1)[2] <- "Global_Site_Id"
names(input_data1)[3] <- "Site_Name"
names(input_data1)[4] <- "Site_Short_Name"
names(input_data1)[5] <- "Site_Street_Address"
names(input_data1)[6] <- "City_Name"
names(input_data1)[7] <- "City_code"
names(input_data1)[8] <- "Country"
names(input_data1)[9] <- "Site_Status"
names(input_data1)[10] <- "Site_Opening_Date_(DD–Mon–YYYY)"
names(input_data1)[11] <- "Site_Closing_Date_(DD–Mon–YYYY)"
names(input_data1)[12] <- "County_Name"
names(input_data1)[13] <- "Territory_Name"
names(input_data1)[14] <- "Postal_Code"
names(input_data1)[15] <- "Tel_Country_Code"
names(input_data1)[16] <- "Telephone_Number"
names(input_data1)[17] <- "Fax_Number"
names(input_data1)[18] <- "Organization_Affiliation_(Site_Brand)"
names(input_data1)[19] <- "Organization_Affiliation_code_(Site_Brand)"
names(input_data1)[20] <- "Organization_Affiliation_Type_Value_(Site_Brand_Type)"
names(input_data1)[21] <- "Real_Estate_Type"
names(input_data1)[22] <- "Operating_Platform"
names(input_data1)[23] <- "Customer/Retailer_Email"
names(input_data1)[24] <- "Nera_Risk_Code"
names(input_data1)[25] <- "VMI_Indicator"
names(input_data1)[26] <- "Currency"
names(input_data1)[27] <- "Currency_Start_Date_(DD–Mon–YYYY)"
names(input_data1)[28] <- "Visibility_Score"
names(input_data1)[29] <- "Accessibility_Score"
names(input_data1)[30] <- "Site_Score"
names(input_data1)[31] <- "Buying_Area_Score_(Trade_Area_Score)"
names(input_data1)[32] <- "Traffic_Flow_Score"
names(input_data1)[33] <- "Site_Location_Score"
names(input_data1)[34] <- "Road_Type"
names(input_data1)[35] <- "Network_Segment–Primary"
names(input_data1)[36] <- "Network_Segment–Secondary"
names(input_data1)[37] <- "Site_Frontage"
names(input_data1)[38] <- "Site_Depth"
names(input_data1)[39] <- "Site_Size_Area"
names(input_data1)[40] <- "Shop_Area"
names(input_data1)[41] <- "Latitude"
names(input_data1)[42] <- "Longitude"
names(input_data1)[43] <- "Site_Shop_Format_Type"
names(input_data1)[44] <- "Loyalty_Program_Indicator"
names(input_data1)[45] <- "FPS_Indicator"
names(input_data1)[46] <- "Pricing_Area_Code"
names(input_data1)[47] <- "Pricing_Area_Name"
names(input_data1)[48] <- "Product_At_Site_CNG_(Yes/_No)"
names(input_data1)[49] <- "Product_At_Site_Premium_Gasoline_(Yes/No)"
names(input_data1)[50] <- "Product_At_Site_Premium_Diesel_(Yes/No)"
names(input_data1)[51] <- "Product_At_Site_Fuelsave_Midgrade_Gasoline_(Yes/No)"
names(input_data1)[52] <- "Product_At_Site_Fuelsave_Regular_Diesel_(Yes/No)"
names(input_data1)[53] <- "Product_At_Site_Midgrade_Gasoline_(Yes/No)"
names(input_data1)[54] <- "Product_At_Site_Low_Octane_Gasoline_(Yes/No)"
names(input_data1)[55] <- "Product_At_Site_Regular_Diesel_(Yes/No)"
names(input_data1)[56] <- "Product_At_Site_Autogas_LPG_(Yes/No)"
names(input_data1)[57] <- "Product_At_Site_Auto/RV_Propane_(Yes/No)"
names(input_data1)[58] <- "Product_At_Site_Hydrogen_(Yes/No)"
names(input_data1)[59] <- "Product_At_Site_Kerosene_(Yes/No)"
names(input_data1)[60] <- "Product_At_Site_Super_Premium_Gasoline_(Yes/No)"
names(input_data1)[61] <- "Product_At_Site_Unleaded_Super_(Yes/No)"
names(input_data1)[62] <- "Product_At_Site_Truck_Diesel_(Yes/No)"
names(input_data1)[63] <- "Product_At_Site_Super_98_(Yes/No)"
names(input_data1)[64] <- "Product_At_Site_GTL_(Yes/No)"
names(input_data1)[65] <- "Product_At_Site_Fuelsave_98_(Yes/No)"
names(input_data1)[66] <- "Product_At_Site_LNG_(Yes/No)"
names(input_data1)[67] <- "Product_At_Site_DieselFit_(Yes/No)"
names(input_data1)[68] <- "Product_At_Site_Shell_Recharge_(Yes/No)"
names(input_data1)[69] <- "Facility_–_24_Hour_Fuel_Service_(Yes/No)"
names(input_data1)[70] <- "Facility_–_Ad_Blue_(Yes/No)"
names(input_data1)[71] <- "Facility_–_Air_&_Water_(Yes/No)"
names(input_data1)[72] <- "Facility_–_ATM_(Yes/No)"
names(input_data1)[73] <- "Facility_–_Bakery_Shop_(Yes/No)"
names(input_data1)[74] <- "Facility_–_Bottled_Gas_(Yes/No)"
names(input_data1)[75] <- "Facility_–_Car_Wash_(Yes/No)"
names(input_data1)[76] <- "Facility_–_Carwash_MPay_(Yes/No)"
names(input_data1)[77] <- "Facility_–_Credit_Cards_(Yes/No)"
names(input_data1)[78] <- "Facility_–_Disability_Assistance_(Yes/No)"
names(input_data1)[79] <- "Facility_Type_–_External_Card_Reader_(Yes/No)"
names(input_data1)[80] <- "Facility_Type_–_Facilities_for_disabled_(Yes/No)"
names(input_data1)[81] <- "Facility_Type_–_Food_Offerings_(Yes/No)"
names(input_data1)[82] <- "Facility_Type_–_Fuel_cards_(Yes/No)"
names(input_data1)[83] <- "Facility_Type_–_Fuel_Service_(Yes/No)"
names(input_data1)[84] <- "Facility_Type_–_HGV_Lane_(Yes/No)"
names(input_data1)[85] <- "Facility_Type_–_High_Speed_Diesel_Pump_(Yes/No)"
names(input_data1)[86] <- "Facility_Type_–_Loyalty_cards_(Yes/No)"
names(input_data1)[87] <- "Facility_Type_–_Max_Vehicle_size_Access_(Yes/No)"
names(input_data1)[88] <- "Facility_Type_–_Mobile_Loyalty_(Yes/No)"
names(input_data1)[89] <- "Facility_Type_–_Mobile_Payment_(Yes/No)"
names(input_data1)[90] <- "Facility_Type_–_Multiple_Site_ID_(Yes/No)"
names(input_data1)[91] <- "Facility_Type_–_Number_of_Pumps_(Yes/No)"
names(input_data1)[92] <- "Facility_Type_–_Partner_Loyalty_Accepted_(Yes/No)"
names(input_data1)[93] <- "Facility_Type_–_Quick_Lubes_(Yes/No)"
names(input_data1)[94] <- "Facility_Type_–_Service_Bay_(Yes/No)"
names(input_data1)[95] <- "Facility_Type_–_Shower_(Yes/No)"
names(input_data1)[96] <- "Facility_Type_–_SIP_Azure_(Yes/No)"
names(input_data1)[97] <- "Facility_Type_–_Third_Party_Rental_(Yes/No)"
names(input_data1)[98] <- "Facility_Type_–_Toll_Vignettes_(Yes/No)"
names(input_data1)[99] <- "Facility_Type_–_Transportation_Service_(Yes/No)"
names(input_data1)[100] <- "Facility_Type_–_Truckport_(Yes/No)"
names(input_data1)[101] <- "Facility_Type_–_Type_of_Parking_(Yes/No)"
names(input_data1)[102] <- "Facility_Type_–_Vacuum_(Yes/No)"
names(input_data1)[103] <- "Facility_Type_–_Vehicle_Identity_System_(Yes/No)"
names(input_data1)[104] <- "Facility_Type_–_Water_Closet_/Toilet_(Yes/No)"
names(input_data1)[105] <- "Facility_–_WIFI_(Yes/No)"
names(input_data1)[106] <- "Brand_Covenant_Desc"
names(input_data1)[107] <- "Brand_Covenant_Expiry_Date"
names(input_data1)[108] <- "Data_Create_Date_(DD–Mon–YYYY_hh:mm:ss)"
names(input_data1)[109] <- "Data_Created_By"
names(input_data1)[110] <- "Last_Update_Time_Stamp_(DD–Mon–YYYY_hh:mm:ss)"
names(input_data1)[111] <- "Last_Updated_By"
names(input_data1)[112] <- "Last_Update_Type_Desc"
names(input_data1)[113] <- "Currency_End_Date_(DD–Mon–YYYY)"
names(input_data1)[114] <- "Core_Status"


input_data2=read.csv("DESiteDetails.csv", header = TRUE)
summary(input_data1)
#summary(input_data2)
Check01 <- input_data1[(is.character(input_data1$Latitude)==TRUE ||
                          is.character(input_data1$Longitude)==TRUE ||
                          is.na(input_data1$Latitude)==TRUE ||
                          is.na(input_data1$Longitude)==TRUE ||
                          input_data1$Latitude == 0 || 
                          input_data1$Longitude == 0 ||
                          input_data1$Latitude == "0" ||
                          input_data1$Longitude == "0"),]

b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)


############################################################################################################
##############################     extracting data needed and performing QC    #############################
############################################################################################################
a <- Sys.time()
print(a)
table(input_data1$Site_Status)

node_data = input_data1[which(input_data1$Site_Status=="Active" &
                           is.nan(input_data1$Latitude)==FALSE &
                           is.nan(input_data1$Longitude)==FALSE &
                           is.na(input_data1$Latitude)==FALSE &
                           is.na(input_data1$Longitude)==FALSE),
                        which(colnames(input_data1)%in%c("Global_Site_Id","Latitude","Longitude"))]
node_data1 = input_data2[,c(2,17,18)]
head(node_data)
#head(node_data1)

combi_check_01 <- merge(edge_data,
                        node_data,
                        by.x = "from",
                        by.y = "Global_Site_Id",
                        all.x = TRUE)
edge_data <- combi_check_01[(is.na(combi_check_01$Latitude)==TRUE &
                             is.na(combi_check_01$Longitude)==TRUE),
                            c(1:3)]

combi_check_02 <- combi_check_01[
  (is.na(combi_check_01$Latitude)==TRUE ||
     is.na(combi_check_01$Longitude)==TRUE ||
     combi_check_01$Latitude == 0 || 
     combi_check_01$Longitude == 0 ||
     combi_check_01$Latitude == "0" ||
     combi_check_01$Longitude == "0" ||
     is.character(combi_check_01$Longitude)==TRUE ||
     is.character(combi_check_01$Latitude)==TRUE),]
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

############################################################################################################
#############################     Reading GSD Data for Latest Store Details    #############################
############################################################################################################
library(igraph)

a <- Sys.time()
print(a)
g <- graph_from_data_frame(edge_data, directed = FALSE, vertices = node_data1)
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

a <- Sys.time()
print(a)
vcount(g)
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

a <- Sys.time()
print(a)
ecount(g)
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

a <- Sys.time()
print(a)
edge_attr(g)
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

a <- Sys.time()
print(a)
vertex_attr(g)
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

#################################################################

a <- Sys.time()
print(a)
deg=degree(g)
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

a <- Sys.time()
print(a)
str=strength(g)
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)
#betw=betweenness(g)

a <- Sys.time()
print(a)
V(g)$degree = deg
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

a <- Sys.time()
print(a)
V(g)$strength = str
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)
#V(g)$betweenness = betw

#################################################################

#51 is the 95th percentile
a <- Sys.time()
print(a)
visit_count <- 73
g1 <- delete_edges(g, E(g)[[weight<=visit_count]])
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

a <- Sys.time()
print(a)
ecount(g1)
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

a <- Sys.time()
print(a)
vcount(g1)
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

#################################################################

a <- Sys.time()
print(a)
gc = edge.betweenness.community(g1)
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

a <- Sys.time()
plot(gc,g1)
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

a <- Sys.time()
print(a)
sizes(gc)[sizes(gc)>1]
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

a <- Sys.time()
print(a)
modularity(gc)
b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)

#################################################################

a <- Sys.time()
print(a)
community_data <- data.frame(
             cbind(
             node = gc$names,
             degree = vertex_attr(g1)$degree, 
             strength = vertex_attr(g1)$strength, 
             betweeness = vertex_attr(g1)$betweenness, 
             membership=gc$membership,
             )
             )

community_data <- as.data.frame(community_data)


DE_TradeAreaMap <- merge(input_data2[,c(1:18)],
                         community_data,
                         by.x = "RETAIL_SITE_ID",
                         by.y = "node",
                         x.all = TRUE
                         )


write.csv(DE_TradeAreaMap,paste("DE_TradeAreaMap_",visit_count,".csv",sep = ""),row.names = FALSE)

b <- Sys.time()
print(b)
c <- paste0(as.integer(as.numeric(b-a,units="days")), " Days ", as.integer(as.numeric(b-a,units="hours"))-(as.integer(as.numeric(b-a,units="days"))*24), " Hours ", as.integer(as.numeric(b-a,units="mins"))-(as.integer(as.numeric(b-a,units="hours"))*60), " Minutes ", as.integer(as.numeric(b-a,units="secs"))-(as.integer(as.numeric(b-a,units="mins"))*60), " Seconds")
print(c)


