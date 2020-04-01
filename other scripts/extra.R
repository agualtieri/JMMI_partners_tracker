#### extra analysis


#North Splits
org_JMMI_North<-org_JMMI%>%
  filter(governorate_ID %in% North)

org_JMMI_North_Count<-org_JMMI_North%>%
  group_by(Date, org)%>%
  dplyr::summarise(total_obs=dplyr::n(),dist_visited=dplyr::n_distinct(district_ID), gov_visisted=dplyr::n_distinct(governorate_ID))

org_test<-filter(org_JMMI_North,org=="SCI")
org_JMMI_North_Matrix<-org_test%>%
  dplyr::mutate(area = paste0(governorate_name,"-",district_name))%>%
  group_by(Date,area, org)%>%
  dplyr::summarise(total_obs=dplyr::n())%>%
  dplyr::select(-org)%>%
  spread(area,total_obs)


orgs_North<-as.vector(unique(org_JMMI_North$org))
govs_North<-as.vector(unique(org_JMMI_North$governorate_name))

# South Splits
org_JMMI_South<-org_JMMI%>%
  filter(governorate_ID %in% South)
orgs_South<-as.vector(unique(org_JMMI_South$org)) 

# Contested Splits
org_JMMI_Contested<-org_JMMI%>%
  filter(governorate_ID %in% Contested)
orgs_Contested<-as.vector(unique(org_JMMI_Contested$org))

###