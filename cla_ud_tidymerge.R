# CLA Underlying Data Tidy and Merge Script
library(tidyverse)
library(fuzzyjoin)
library(ggrepel)

# Import varlists from meta documents -------------------------------------

varlist_2011 <- "
New_geog_code, New geography code
geog_l, Geography level
geog_c, Geography code
geog_n, Geography name
CLA_Mar2011, Children looked after at 31 March 2011
CLA_2011, Children looked after during the year ending 31 March 2011
CLA_stp2011, Children looked after during the year ending 31 March 2011 who were only looked after under a series of short term placements
CLA_male, Male children looked after at 31 March 2011
CLA_female, Female children looked after at 31 March 2011
CLA_U1, Children looked after at 31 March 2011 aged under 1 year 	
CLA_1to4, Children looked after at 31 March 2011 aged 1 to 4 years
CLA_5to9, Children looked after at 31 March 2011 aged 5 to 9 years 
CLA_10to15, Children looked after at 31 March 2011 aged 10 to 15 years
CLA_16over, Children looked after at 31 March 2011 aged 16 years and over 
CLA_White, Children looked after at 31 March 2011 from a white ethnic background
CLA_Mixed, Children looked after at 31 March 2011 from a mixed ethnic background
CLA_Asian, Children looked after at 31 March 2011 from an Asian ethnic backgriuind
CLA_Black, Children looked after at 31 March 2011 from a Black ethnic background
CLA_EOTH, Children looked after at 31 March 2011 from any other ethnic backgriound
CLA_Oth, Children looked after at 31 March 2011 where ethnic background is not known 
CLA_Fost, Children looked after at 31 March 2011 in foster placements
CLA_Adopt, Children looked after at 31 March 2011 placed for adoption 
CLA_Parent, Children looked after at 31 March 2011 placed with parents
CLA_Ocom, Children looked after at 31 March 2011 placed in other placements within the community 
CLA_Secure, Children looked after at 31 March 2011 placed in secure units children’s homes and hostels 
CLA_Ores, Children looked after at 31 March 2011 placed in other residential accommodation 
CLA_RSch, Children looked after at 31 March 2011 placed in residential schools
CLA_Miss, Children looked after at 31 March 2011 missing from their agreed placement for more than 24 hours
CLA_OthPl, Children looked after at 31 March 2011 in other placements 
CLA_ICO, Children looked after at 31 March 2011 under an interim care order 
CLA_FCO, Children looked after at 31 March 2011 under a full care order 
CLA_FrAd, Children looked after at 31 March 2011 who are freed for adoption 
CLA_PlaceO	, Children looked after at 31 March 2011 who have a placement order 
CLA_S20, Children looked after at 31 March 2011 under section 20 
CLA_YJLS, Children looked after at 31 March 2011 under a youth justice legal status 
CLA_CPG, Children looked after at 31 March 2011 under child protection grounds 
CLA_UASC, Children looked after at 31 March 2011 who are unaccompanied asylum seekers 
CLA_Moth, Children looked after at 31 March 2011 who are mothers 
CLA_OwnP, Children looked after at 31 March 2011 where placement is provided by the LAs own provision 
CLA_OthLA, Children looked after at 31 March 2011 under where placement is provided by another LA 
CLA_OthPP, Children looked after at 31 March 2011 where placement is provided by other public provision 
CLA_Priv, Children looked after at 31 March 2011 where placement is provided by private provision 
CLA_Vol, Children looked after at 31 March 2011 where placement is provided by voluntary provision 
CLA_Par, Children looked after at 31 March 2011 where placement is provided by parents 
CLA_Nrep, Children looked after at 31 March 2011 where placement provider is not reported 
CLA_1Pla, Children looked after at 31 March 2011 with one placement during the year 
CLA_2PLa, Children looked after at 31 March 2011 with two placements during the year 
CLA_3Pla, Children looked after at 31 March 2011 with 3 or more placements during the year 
CLA_P2yrs, Children looked after at 31 March 2011 living in the same placemnt for at least 2 years 
CLA_Review	, Children looked after at 31 March 2011 whose reviews were carried out on time 
CLA_InBound, Children looked after at 31 March 2011  placed within LA bounfary 
CLA_Outbound, Children looked after at 31 March 2011 placed outside of the LA boundary 
CLA_ExtPl, Children looked after at 31 March 2011 by in the LAs bundary by another LA 
CLA_NetGain, Children looked after at 31 March 2011 – net gain of children by an LA
CLA_started2011, Children who started to be looked after during the year ending 31 March 2011
CLA_taken2011, Children who were taken into care during the year ending 31 March 2011 
SCLA_male, Male children who started to be looked after during the year ending 31 March 2011
SCLA_female, Female children who started to be looked after during the year ending 31 March 2011
SCLA_U1 , Children who started to be looked after during the year ending 31 March 2011 aged under 1 
SCLA_1to4, Children who started to be looked after during the year ending 31 March 2011 aged 1 to 4 years 
SCLA_5to9, Children who started to be looked after during the year ending 31 March 2011 aged 5 to 9 years 
SCLA_10to15, Children who started to be looked after during the year ending 31 March 2011 aged 0 to 15 years 
SCLA_16over, Children who started to be looked after during the year ending 31 March 2011 aged 16 years and over 
SCLA_AbNeg, Children who started to be looked after during the year ending 31 March 2011 due to abuse or neglect 
SCLA_Cdisab, Children who started to be looked after during the year ending 31 March 2011 due to the child’s disability 
SCLA_ParIll, Children who started to be looked after during the year ending 31 March 2011 due to parental illness or disability 
SCLA_FAcSt	, Children who started to be looked after during the year ending 31 March 2011 due to family in acute stress
SCLA_FD, Children who started to be looked after during the year ending 31 March 2011 due to family dysfunction 
SCLA_SUB, Children who started to be looked after during the year ending 31 March 2011 due to socially unacceptable behaviour 
SCLA_LI, Children who started to be looked after during the year ending 31 March 2011 due to low income 
SCLA_AbsPar, Children who started to be looked after during the year ending 31 March 2011 due to absent parenting 
SCLA_ICO, Children who started to be looked after during the year ending 31 March 2011 under an interim care order 
SCLA_FCO, Children who started to be looked after during the year ending 31 March 2011 under a full care order 
SCLA_POG, Children who started to be looked after during the year ending 31 March 2011 where a placement order has been granted 
SCLA_S20, Children who started to be looked after during the year ending 31 March 2011 under section 20 
SCLA_UPP, Children who started to be looked after during the year ending 31 March 2011 under police protection 
SCLA_SEPO	, Children who started to be looked after during the year ending 31 March 2011 subject to an emergency protection order 
SCLA_UCAO, Children who started to be looked after during the year ending 31 March 2011 under a child assessment order 
SCLA_ONCT, Children who started to be looked after during the year ending 31 March 2011 on remand or committed for trial 
SCLA_PACE	, Children who started to be looked after during the year ending 31 March 2011 under the police and criminal evidence act 
SCLA_SORR, Children who started to be looked after during the year ending 31 March 2011 under a supervision order with residence requirement 
CLA_cease, Children who ceased to be looked after during the year ending 31 March 2011	
CLA_cease16, Children aged 16 who ceased to be looked after during the year ending 31 March 2011
CLA_ceamal, Male children who ceased to be looked after during the year ending 31 March 2011
CLA_ceafe, Female children who ceased to be looked after during the year ending 31 March 2011	
CLA_cea1, Children who ceased to be looked after during the year ending 31 March 2011 aged under 1
CLA_cea14,	Children who ceased to be looked after during the year ending 31 March 2011 aged 1 to 4 years 
CLA_cea59,	Children who ceased to be looked after during the year ending 31 March 2011 aged 5 to 9 years 
CLA_cea1015, Children who ceased to be looked after during the year ending 31 March 2011 aged 10 to 15 years 	
CLA_cea16, Children who ceased to be looked after during the year ending 31 March 2011 aged 16 years and over 	
CLA_ceaAdop, Children who ceased to be looked after during the year ending 31 March 2011 due to adoption - application unopposed
CLA_ceaAdop2, Children who ceased to be looked after during the year ending 31 March 2011- consent dispensed with
CLA-ceaDied, Children who ceased to be looked after during the year ending 31 March 2011 as they have died 
CLA_ceataken, Children who ceased to be looked after during the year ending 31 March 2011 - care taken by another LA
CLA_ceaPar, Children who ceased to be looked after during the year ending 31 March 2011 - returned home to live with parents or relatives
CLA_ceaROG, Children who ceased to be looked after during the year ending 31 March 2011 - residence order granted
CLA_ceaSpecG, Children who ceased to be looked after during the year ending 31 March 2011 - special guardianship order made to former foster carers
CLA_ceaSpecG2, Children who ceased to be looked after during the year ending 31 March 2011 - special guardianship order made to carers other than former foster carers
CLA_ceaIndLiv, Children who ceased to be looked after during the year ending 31 March 2011 - moved into independent living (with supportive accommodation)
CLA_ceaIndLiv2, Children who ceased to be looked after during the year ending 31 March 2011 - moved into independent living (with no formalised support)
CLA_cea_tran_res, Children who ceased to be looked after during the year ending 31 March 2011 - transferred to residential care funded by adult social services	
CLA_cea_sen_cust, Children who ceased to be looked after during the year ending 31 March 2011 - sentenced to custody
CLA_cea_OthRea, Children who ceased to be looked after during the year ending 31 March 2011 - care ceased for any other reason
CLA_cease, Children who ceased to be looked after during the year ending 31 March 2011
CLA_ceaAdop, Children who ceased to be looked after during the year ending 31 March 2011 due to adoption 
CLA_Adop_PL12mnths, Children who ceased to be looked after during the year ending 31 March 2011 due to adoption - number of children adopted during the year who were placed for adoption within 12 months
All_aged19, All children now aged 19 years old who were looked after on 1 April 2007 then aged 16 years old	
LA_nit	, Local authority not in touch with child
Act_HE, Number whose activity is higher education i.e. studies beyond A level
Act_OE, Number whose activity is education other than higher education	
Act_TE, Number whose activity is training or employment
Act_Neet_ill, Number who are not in education training or employment due to illness or disability	
Act_NEET_oth, Number who are not in education training or employment due to other reasons
Acc_P	, Number accommodated with parents or relatives 	
Acc_FFC, Number accommodated with former foster carers
Acc_CH, Number accommodated in community homes 	
Acc_SITA, Number accommodated in semi-independent transitional accommodation 	
Acc_SL, Number accommodated in supported lodgings
Acc_OL, Number accommodated in ordinary lodgings	
Acc_F	, Number accommodated in foyers
Acc_IL, Number accommodated in independent living	
Acc_EA, Number accommodated in emergency accommodation 	
Acc_BB, Number in bed and breakfast accommodation
Acc_C	, Number who are in custody  	
Acc_OTH, Number in other accommodation
Acc_SUIT, Total number in suitable accommodation
"

varlist_2011 <- read_csv(varlist_2011, col_names = c("varname", "description"))

varlist_2012 <- "
New_geog_code, New geography code
geog_l, Geography level
geog_c, Geography code
geog_n, Geography name
CLA_Mar2012, Children looked after at 31 March 2012
CLA_2012, Children looked after during the year ending 31 March 2012
CLA_stp2012, Children looked after during the year ending 31 March 2012 who were only looked after under a series of short term placements
CLA_male, Male children looked after at 31 March 2012
CLA_female, Female children looked after at 31 March 2012
CLA_U1, Children looked after at 31 March 2012 aged under 1 year 	
CLA_1to4, Children looked after at 31 March 2012 aged 1 to 4 years
CLA_5to9, Children looked after at 31 March 2012 aged 5 to 9 years 
CLA_10to15, Children looked after at 31 March 2012 aged 10 to 15 years
CLA_16over, Children looked after at 31 March 2012 aged 16 years and over 
CLA_White, Children looked after at 31 March 2012 from a white ethnic background
CLA_Mixed, Children looked after at 31 March 2012 from a mixed ethnic background
CLA_Asian, Children looked after at 31 March 2012 from an Asian ethnic backgriuind
CLA_Black, Children looked after at 31 March 2012 from a Black ethnic background
CLA_EOTH, Children looked after at 31 March 2012 from any other ethnic backgriound
CLA_Oth, Children looked after at 31 March 2012 where ethnic background is not known 
CLA_Fost, Children looked after at 31 March 2012 in foster placements
CLA_Adopt, Children looked after at 31 March 2012 placed for adoption 
CLA_Parent, Children looked after at 31 March 2012 placed with parents
CLA_Ocom, Children looked after at 31 March 2012 placed in other placements within the community 
CLA_Secure, Children looked after at 31 March 2012 placed in secure units children’s homes and hostels 
CLA_Ores, Children looked after at 31 March 2012 placed in other residential accommodation 
CLA_RSch, Children looked after at 31 March 2012 placed in residential schools
CLA_Miss, Children looked after at 31 March 2012 missing from their agreed placement for more than 24 hours
CLA_OthPl, Children looked after at 31 March 2012 in other placements 
CLA_ICO, Children looked after at 31 March 2012 under an interim care order 
CLA_FCO, Children looked after at 31 March 2012 under a full care order 
CLA_FrAd, Children looked after at 31 March 2012 who are freed for adoption 
CLA_PlaceO	, Children looked after at 31 March 2012 who have a placement order 
CLA_S20, Children looked after at 31 March 2012 under section 20 
CLA_YJLS, Children looked after at 31 March 2012 under a youth justice legal status 
CLA_CPG, Children looked after at 31 March 2012 under child protection grounds 
CLA_UASC, Children looked after at 31 March 2012 who are unaccompanied asylum seekers 
CLA_Moth, Children looked after at 31 March 2012 who are mothers 
CLA_OwnP, Children looked after at 31 March 2012 where placement is provided by the LAs own provision 
CLA_OthLA, Children looked after at 31 March 2012 under where placement is provided by another LA 
CLA_OthPP, Children looked after at 31 March 2012 where placement is provided by other public provision 
CLA_Priv, Children looked after at 31 March 2012 where placement is provided by private provision 
CLA_Vol, Children looked after at 31 March 2012 where placement is provided by voluntary provision 
CLA_Par, Children looked after at 31 March 2012 where placement is provided by parents 
CLA_Nrep, Children looked after at 31 March 2012 where placement provider is not reported 
CLA_InBound, Children looked after at 31 March 2012  placed within LA bounfary 
CLA_Outbound, Children looked after at 31 March 2012 placed outside of the LA boundary 
CLA_ExtPl, Children looked after at 31 March 2012 by in the LAs bundary by another LA 
CLA_NetGain, Children looked after at 31 March 2012 – net gain of children by an LA
CLA_started2012, Children who started to be looked after during the year ending 31 March 2012
CLA_taken2012, Children who were taken into care during the year ending 31 March 2012 
SCLA_male, Male children who started to be looked after during the year ending 31 March 2012
SCLA_female, Female children who started to be looked after during the year ending 31 March 2012
SCLA_U1 , Children who started to be looked after during the year ending 31 March 2012 aged under 1 
SCLA_1to4, Children who started to be looked after during the year ending 31 March 2012 aged 1 to 4 years 
SCLA_5to9, Children who started to be looked after during the year ending 31 March 2012 aged 5 to 9 years 
SCLA_10to15, Children who started to be looked after during the year ending 31 March 2012 aged 0 to 15 years 
SCLA_16over, Children who started to be looked after during the year ending 31 March 2012 aged 16 years and over 
SCLA_AbNeg, Children who started to be looked after during the year ending 31 March 2012 due to abuse or neglect 
SCLA_Cdisab, Children who started to be looked after during the year ending 31 March 2012 due to the child’s disability 
SCLA_ParIll, Children who started to be looked after during the year ending 31 March 2012 due to parental illness or disability 
SCLA_FAcSt	, Children who started to be looked after during the year ending 31 March 2012 due to family in acute stress
SCLA_FD, Children who started to be looked after during the year ending 31 March 2012 due to family dysfunction 
SCLA_SUB, Children who started to be looked after during the year ending 31 March 2012 due to socially unacceptable behaviour 
SCLA_LI, Children who started to be looked after during the year ending 31 March 2012 due to low income 
SCLA_AbsPar, Children who started to be looked after during the year ending 31 March 2012 due to absent parenting 
SCLA_ICO, Children who started to be looked after during the year ending 31 March 2012 under an interim care order 
SCLA_FCO, Children who started to be looked after during the year ending 31 March 2012 under a full care order 
SCLA_POG, Children who started to be looked after during the year ending 31 March 2012 where a placement order has been granted 
SCLA_S20, Children who started to be looked after during the year ending 31 March 2012 under section 20 
SCLA_UPP, Children who started to be looked after during the year ending 31 March 2012 under police protection 
SCLA_SEPO	, Children who started to be looked after during the year ending 31 March 2012 subject to an emergency protection order 
SCLA_UCAO, Children who started to be looked after during the year ending 31 March 2012 under a child assessment order 
SCLA_ONCT, Children who started to be looked after during the year ending 31 March 2012 on remand or committed for trial 
SCLA_PACE	, Children who started to be looked after during the year ending 31 March 2012 under the police and criminal evidence act 
SCLA_SORR, Children who started to be looked after during the year ending 31 March 2012 under a supervision order with residence requirement 
CLA_cease, Children who ceased to be looked after during the year ending 31 March 2012	
CLA_cease16, Children aged 16 who ceased to be looked after during the year ending 31 March 2012
CLA_ceamal, Male children who ceased to be looked after during the year ending 31 March 2012
CLA_ceafe, Female children who ceased to be looked after during the year ending 31 March 2012	
CLA_cea1, Children who ceased to be looked after during the year ending 31 March 2012 aged under 1
CLA_cea14,	Children who ceased to be looked after during the year ending 31 March 2012 aged 1 to 4 years 
CLA_cea59,	Children who ceased to be looked after during the year ending 31 March 2012 aged 5 to 9 years 
CLA_cea1015, Children who ceased to be looked after during the year ending 31 March 2012 aged 10 to 15 years 	
CLA_cea16, Children who ceased to be looked after during the year ending 31 March 2012 aged 16 years and over 	
CLA_ceaAdop, Children who ceased to be looked after during the year ending 31 March 2012 due to adoption - application unopposed
CLA_ceaAdop2, Children who ceased to be looked after during the year ending 31 March 2012- consent dispensed with
CLA-ceaDied, Children who ceased to be looked after during the year ending 31 March 2012 as they have died 
CLA_ceataken, Children who ceased to be looked after during the year ending 31 March 2012 - care taken by another LA
CLA_ceaPar, Children who ceased to be looked after during the year ending 31 March 2012 - returned home to live with parents or relatives
CLA_ceaROG, Children who ceased to be looked after during the year ending 31 March 2012 - residence order granted
CLA_ceaSpecG, Children who ceased to be looked after during the year ending 31 March 2012 - special guardianship order made to former foster carers
CLA_ceaSpecG2, Children who ceased to be looked after during the year ending 31 March 2012 - special guardianship order made to carers other than former foster carers
CLA_ceaIndLiv, Children who ceased to be looked after during the year ending 31 March 2012 - moved into independent living (with supportive accommodation)
CLA_ceaIndLiv2, Children who ceased to be looked after during the year ending 31 March 2012 - moved into independent living (with no formalised support)
CLA_cea_tran_res, Children who ceased to be looked after during the year ending 31 March 2012 - transferred to residential care funded by adult social services	
CLA_cea_sen_cust, Children who ceased to be looked after during the year ending 31 March 2012 - sentenced to custody
CLA_cea_OthRea, Children who ceased to be looked after during the year ending 31 March 2012 - care ceased for any other reason
CLA_cease, Children who ceased to be looked after during the year ending 31 March 2012
CLA_ceaAdop, Children who ceased to be looked after during the year ending 31 March 2012 due to adoption 
All_aged19, All children now aged 19 years old who were looked after on 1 April 2007 then aged 16 years old	
LA_nit	, Local authority not in touch with child
Act_HE, Number whose activity is higher education i.e. studies beyond A level
Act_OE, Number whose activity is education other than higher education	
Act_TE, Number whose activity is training or employment
Act_Neet_ill, Number who are not in education training or employment due to illness or disability	
Act_NEET_oth, Number who are not in education training or employment due to other reasons
Acc_P	, Number accommodated with parents or relatives 	
Acc_FFC, Number accommodated with former foster carers
Acc_CH, Number accommodated in community homes 	
Acc_SITA, Number accommodated in semi-independent transitional accommodation 	
Acc_SL, Number accommodated in supported lodgings
Acc_OL, Number accommodated in ordinary lodgings	
Acc_F	, Number accommodated in foyers
Acc_IL, Number accommodated in independent living	
Acc_EA, Number accommodated in emergency accommodation 	
Acc_BB, Number in bed and breakfast accommodation
Acc_C	, Number who are in custody  	
Acc_OTH, Number in other accommodation
Acc_SUIT, Total number in suitable accommodation
All_aged19, All children now aged 19 years old who were looked after on 1 April 2007 then aged 16 years old	
LA_nit	, Local authority not in touch with child
Act_HE, Number whose activity is higher education i.e. studies beyond A level
Act_OE, Number whose activity is education other than higher education	
Act_TE, Number whose activity is training or employment
Act_Neet_ill, Number who are not in education training or employment due to illness or disability	
Act_NEET_oth, Number who are not in education training or employment due to other reasons
Acc_P	, Number accommodated with parents or relatives 	
Acc_FFC, Number accommodated with former foster carers
Acc_CH, Number accommodated in community homes 	
Acc_SITA, Number accommodated in semi-independent transitional accommodation 	
Acc_SL, Number accommodated in supported lodgings
Acc_OL, Number accommodated in ordinary lodgings	
Acc_F	, Number accommodated in foyers
Acc_IL, Number accommodated in independent living	
Acc_EA, Number accommodated in emergency accommodation 	
Acc_BB, Number in bed and breakfast accommodation
Acc_C	, Number who are in custody  	
Acc_OTH, Number in other accommodation
Acc_SUIT, Total number in suitable accommodation
"

varlist_2012 <- read_csv(varlist_2012, col_names = c("varname", "description"))


varlist_2013 <- "
New_geog_code, New geography code
geog_l, Geography level
geog_c, Geography code
geog_n, Geography name
CLA_Mar2013, Children looked after at 31 March 2013
CLA_2013, Children looked after during the year ending 31 March 2013
CLA_stp2013, Children looked after during the year ending 31 March 2013 who were only looked after under a series of short term placements
CLA_male, Male children looked after at 31 March 2013
CLA_female, Female children looked after at 31 March 2013
CLA_U1, Children looked after at 31 March 2013 aged under 1 year 	
CLA_1to4, Children looked after at 31 March 2013 aged 1 to 4 years
CLA_5to9, Children looked after at 31 March 2013 aged 5 to 9 years 
CLA_10to15, Children looked after at 31 March 2013 aged 10 to 15 years
CLA_16over, Children looked after at 31 March 2013 aged 16 years and over 
CLA_White, Children looked after at 31 March 2013 from a white ethnic background
CLA_Mixed, Children looked after at 31 March 2013 from a mixed ethnic background
CLA_Asian, Children looked after at 31 March 2013 from an Asian ethnic background
CLA_Black, Children looked after at 31 March 2013 from a Black ethnic background
CLA_EOTH, Children looked after at 31 March 2013 from any other ethnic background
CLA_Oth, Children looked after at 31 March 2013 where ethnic background is not known 
CLA_Fost, Children looked after at 31 March 2013 in foster placements
CLA_Adopt, Children looked after at 31 March 2013 placed for adoption 
CLA_Parent, Children looked after at 31 March 2013 placed with parents
CLA_Ocom, Children looked after at 31 March 2013 placed in other placements within the community 
CLA_Secure, Children looked after at 31 March 2013 placed in secure units children’s homes and hostels 
CLA_Ores, Children looked after at 31 March 2013 placed in other residential accommodation 
CLA_RSch, Children looked after at 31 March 2013 placed in residential schools
CLA_Miss, Children looked after at 31 March 2013 missing from their agreed placement for more than 24 hours
CLA_OthPl, Children looked after at 31 March 2013 in other placements 
CLA_ICO, Children looked after at 31 March 2013 under an interim care order 
CLA_FCO, Children looked after at 31 March 2013 under a full care order 
CLA_FrAd, Children looked after at 31 March 2013 who are freed for adoption 
CLA_PlaceO	, Children looked after at 31 March 2013 who have a placement order 
CLA_S20, Children looked after at 31 March 2013 under section 20 
CLA_YJLS, Children looked after at 31 March 2013 under a youth justice legal status 
CLA_CPG, Children looked after at 31 March 2013 under child protection grounds 
CLA_UASC, Children looked after at 31 March 2013 who are unaccompanied asylum seekers 
CLA_Moth, Children looked after at 31 March 2013 who are mothers 
CLA_OwnP, Children looked after at 31 March 2013 where placement is provided by the LAs own provision 
CLA_OthLA, Children looked after at 31 March 2013 under where placement is provided by another LA 
CLA_OthPP, Children looked after at 31 March 2013 where placement is provided by other public provision 
CLA_Priv, Children looked after at 31 March 2013 where placement is provided by private provision 
CLA_Vol, Children looked after at 31 March 2013 where placement is provided by voluntary provision 
CLA_Par, Children looked after at 31 March 2013 where placement is provided by parents 
CLA_Nrep, Children looked after at 31 March 2013 where placement provider is not reported 
CLA_1Pla, Children looked after at 31 March with one placement	
CLA_2Pla, Children looked after at 31 March with two placements	
CLA_3Pla, Children looked after at 31 March with three placements	
CLA_P2yrs, Looked after children aged under 16 at 31 March who had been looked after continuously for at least 2.5 years who were living in the same placement for at least 2 years
CLA_InBound, Children looked after at 31 March 2013 placed within LA boundary 
CLA_Outbound, Children looked after at 31 March 2013 placed outside of the LA boundary 
CLA_ExtPl, Children looked after at 31 March 2013 by in the LAs bundary by another LA 
CLA_NetGain, Children looked after at 31 March 2013 – net gain of children by an LA
CLA_started2013, Children who started to be looked after during the year ending 31 March 2013
CLA_taken2013, Children who were taken into care during the year ending 31 March 2013 
SCLA_male, Male children who started to be looked after during the year ending 31 March 2013
SCLA_female, Female children who started to be looked after during the year ending 31 March 2013
SCLA_U1 , Children who started to be looked after during the year ending 31 March 2013 aged under 1 
SCLA_1to4, Children who started to be looked after during the year ending 31 March 2013 aged 1 to 4 years 
SCLA_5to9, Children who started to be looked after during the year ending 31 March 2013 aged 5 to 9 years 
SCLA_10to15, Children who started to be looked after during the year ending 31 March 2013 aged 0 to 15 years 
SCLA_16over, Children who started to be looked after during the year ending 31 March 2013 aged 16 years and over 
SCLA_AbNeg, Children who started to be looked after during the year ending 31 March 2013 due to abuse or neglect 
SCLA_Cdisab, Children who started to be looked after during the year ending 31 March 2013 due to the child’s disability 
SCLA_ParIll, Children who started to be looked after during the year ending 31 March 2013 due to parental illness or disability 
SCLA_FAcSt	, Children who started to be looked after during the year ending 31 March 2013 due to family in acute stress
SCLA_FD, Children who started to be looked after during the year ending 31 March 2013 due to family dysfunction 
SCLA_SUB, Children who started to be looked after during the year ending 31 March 2013 due to socially unacceptable behaviour 
SCLA_LI, Children who started to be looked after during the year ending 31 March 2013 due to low income 
SCLA_AbsPar, Children who started to be looked after during the year ending 31 March 2013 due to absent parenting 
SCLA_ICO, Children who started to be looked after during the year ending 31 March 2013 under an interim care order 
SCLA_FCO, Children who started to be looked after during the year ending 31 March 2013 under a full care order 
SCLA_POG, Children who started to be looked after during the year ending 31 March 2013 where a placement order has been granted 
SCLA_S20, Children who started to be looked after during the year ending 31 March 2013 under section 20 
SCLA_UPP, Children who started to be looked after during the year ending 31 March 2013 under police protection 
SCLA_SEPO	, Children who started to be looked after during the year ending 31 March 2013 subject to an emergency protection order 
SCLA_UCAO, Children who started to be looked after during the year ending 31 March 2013 under a child assessment order 
SCLA_ONCT, Children who started to be looked after during the year ending 31 March 2013 on remand or committed for trial 
SCLA_PACE	, Children who started to be looked after during the year ending 31 March 2013 under the police and criminal evidence act 
SCLA_SORR, Children who started to be looked after during the year ending 31 March 2013 under a supervision order with residence requirement 
CLA_cease, Children who ceased to be looked after during the year ending 31 March 2013	
CLA_cease16, Children aged 16 who ceased to be looked after during the year ending 31 March 2013
CLA_ceamal, Male children who ceased to be looked after during the year ending 31 March 2013
CLA_ceafe, Female children who ceased to be looked after during the year ending 31 March 2013	
CLA_cea1, Children who ceased to be looked after during the year ending 31 March 2013 aged under 1
CLA_cea14,	Children who ceased to be looked after during the year ending 31 March 2013 aged 1 to 4 years 
CLA_cea59,	Children who ceased to be looked after during the year ending 31 March 2013 aged 5 to 9 years 
CLA_cea1015, Children who ceased to be looked after during the year ending 31 March 2013 aged 10 to 15 years 	
CLA_cea16, Children who ceased to be looked after during the year ending 31 March 2013 aged 16 years and over 	
CLA_ceaAdop, Children who ceased to be looked after during the year ending 31 March 2013 due to adoption - application unopposed
CLA_ceaAdop2, Children who ceased to be looked after during the year ending 31 March 2013- consent dispensed with
CLA-ceaDied, Children who ceased to be looked after during the year ending 31 March 2013 as they have died 
CLA_ceataken, Children who ceased to be looked after during the year ending 31 March 2013 - care taken by another LA
CLA_ceaPar, Children who ceased to be looked after during the year ending 31 March 2013 - returned home to live with parents or relatives
CLA_ceaROG, Children who ceased to be looked after during the year ending 31 March 2013 - residence order granted
CLA_ceaSpecG, Children who ceased to be looked after during the year ending 31 March 2013 - special guardianship order made to former foster carers
CLA_ceaSpecG2, Children who ceased to be looked after during the year ending 31 March 2013 - special guardianship order made to carers other than former foster carers
CLA_ceaIndLiv, Children who ceased to be looked after during the year ending 31 March 2013 - moved into independent living (with supportive accommodation)
CLA_ceaIndLiv2, Children who ceased to be looked after during the year ending 31 March 2013 - moved into independent living (with no formalised support)
CLA_cea_tran_res, Children who ceased to be looked after during the year ending 31 March 2013 - transferred to residential care funded by adult social services	
CLA_cea_sen_cust, Children who ceased to be looked after during the year ending 31 March 2013 - sentenced to custody
CLA_cea_OthRea, Children who ceased to be looked after during the year ending 31 March 2013 - care ceased for any other reason
CLA_cease, Children who ceased to be looked after during the year ending 31 March 2013
CLA_ceaAdop, Children who ceased to be looked after during the year ending 31 March 2013 due to adoption
CLA_Adop_PL12mnths, Number of children adopted during the year who were placed for adoption within 12 months 
All_aged19, All children now aged 19 years old who were looked after on 1 April 2010 then aged 16 years old	
LA_nit	, Local authority not in touch with child
Act_HE, Number whose activity is higher education i.e. studies beyond A level
Act_OE, Number whose activity is education other than higher education	
Act_TE, Number whose activity is training or employment
Act_Neet_ill, Number who are not in education training or employment due to illness or disability	
Act_NEET_oth, Number who are not in education training or employment due to other reasons
Acc_P	, Number accommodated with parents or relatives 	
Acc_FFC, Number accommodated with former foster carers
Acc_CH, Number accommodated in community homes 	
Acc_SITA, Number accommodated in semi-independent transitional accommodation 	
Acc_SL, Number accommodated in supported lodgings
Acc_OL, Number accommodated in ordinary lodgings	
Acc_F	, Number accommodated in foyers
Acc_IL, Number accommodated in independent living	
Acc_EA, Number accommodated in emergency accommodation 	
Acc_BB, Number in bed and breakfast accommodation
Acc_C	, Number who are in custody  	
Acc_OTH, Number in other accommodation
Acc_SUIT, Total number in suitable accommodation
All_aged19, All children now aged 19 years old who were looked after on 1 April 2010 then aged 16 years old	
LA_nit	, Local authority not in touch with child
Act_HE, Number whose activity is higher education i.e. studies beyond A level
Act_OE, Number whose activity is education other than higher education	
Act_TE, Number whose activity is training or employment
Act_Neet_ill, Number who are not in education training or employment due to illness or disability	
Act_NEET_oth, Number who are not in education training or employment due to other reasons
Acc_P	, Number accommodated with parents or relatives 	
Acc_FFC, Number accommodated with former foster carers
Acc_CH, Number accommodated in community homes 	
Acc_SITA, Number accommodated in semi-independent transitional accommodation 	
Acc_SL, Number accommodated in supported lodgings
Acc_OL, Number accommodated in ordinary lodgings	
Acc_F	, Number accommodated in foyers
Acc_IL, Number accommodated in independent living	
Acc_EA, Number accommodated in emergency accommodation 	
Acc_BB, Number in bed and breakfast accommodation
Acc_C	, Number who are in custody  	
Acc_OTH, Number in other accommodation
Acc_SUIT, Total number in suitable accommodation

"

varlist_2013 <- read_csv(varlist_2013, col_names = c("varname", "description"))

varlist_2014 <- "
New_geog_code, New geography code
geog_l, Geography level
geog_c, Geography code
geog_n, Geography name
CLA_Mar2014, Children looked after at 31 March 2014
CLA_2014, Children looked after during the year ending 31 March 2014
CLA_stp2014, Children looked after during the year ending 31 March 2014 who were only looked after under a series of short term placements
CLA_male, Mae children looked after at 31 March 2014
CLA_female, Female children looked after at 31 March 2014
CLA_U1, Children looked after at 31 March 2014 aged under 1 year 	
CLA_1to4, Children looked after at 31 March 2014 aged 1 to 4 years
CLA_5to9, Children looked after at 31 March 2014 aged 5 to 9 years 
CLA_10to15, Children looked after at 31 March 2014 aged 10 to 15 years
CLA_16over, Children looked after at 31 March 2014 aged 16 years and over 
CLA_White, Children looked after at 31 March 2014 from a white ethnic background
CLA_Mixed, Children looked after at 31 March 2014 from a mixed ethnic background
CLA_Asian, Children looked after at 31 March 2014 from an Asian ethnic background
CLA_Black, Children looked after at 31 March 2014 from a Black ethnic background
CLA_EOTH, Children looked after at 31 March 2014 from any other ethnic background
CLA_Oth, Children looked after at 31 March 2014 where ethnic background is not known 
CLA_Fost, Children looked after at 31 March 2014 in foster placements
CLA_Adopt, Children looked after at 31 March 2014 placed for adoption 
CLA_Parent, Children looked after at 31 March 2014 placed with parents
CLA_Ocom, Children looked after at 31 March 2014 placed in other placements within the community 
CLA_Secure, Children looked after at 31 March 2014 placed in secure units children?s homes and hostels 
CLA_Ores, Children looked after at 31 March 2014 placed in other residential accommodation 
CLA_RSch, Children looked after at 31 March 2014 placed in residential schools
CLA_Miss, Children looked after at 31 March 2014 missing from their agreed placement for more than 24 hours
CLA_OthPl, Children looked after at 31 March 2014 in other placements 
CLA_ICO, Children looked after at 31 March 2014 under an interim care order 
CLA_FCO, Children looked after at 31 March 2014 under a full care order 
CLA_FrAd, Children looked after at 31 March 2014 who are freed for adoption 
CLA_PlaceO	, Children looked after at 31 March 2014 who have a placement order 
CLA_S20, Children looked after at 31 March 2014 under section 20 
CLA_CPG, Children looked after at 31 March 2014 under child protection grounds 
CLA_YJLS, Children looked after at 31 March 2014 under a youth justice legal status 
CLA_UASC, Children looked after at 31 March 2014 who are unaccompanied asylum seekers 
CLA_Moth, Children looked after at 31 March 2014 who are mothers 
CLA_OwnP, Children looked after at 31 March 2014 where placement is provided by the LAs own provision 
CLA_OthLA, Children looked after at 31 March 2014 under where placement is provided by another LA 
CLA_OthPP, Children looked after at 31 March 2014 where placement is provided by other public provision 
CLA_Priv, Children looked after at 31 March 2014 where placement is provided by private provision 
CLA_Vol, Children looked after at 31 March 2014 where placement is provided by voluntary provision 
CLA_Par, Children looked after at 31 March 2014 where placement is provided by parents 
CLA_Nrep, Children looked after at 31 March 2014 where placement provider is not reported 
CLA_1Pla, Children looked after at 31 March with one placement	
CLA_2Pla, Children looked after at 31 March with two placements	
CLA_3Pla, Children looked after at 31 March with three placements	
CLA_P2yrs, Looked after children aged under 16 at 31 March who had been looked after continuously for at least 2.5 years who were living in the same placement for at least 2 years
CLA_InBound, Children looked after at 31 March 2014 placed within LA boundary 
CLA_Outbound, Children looked after at 31 March 2014 placed outside of the LA boundary 
CLA_ExtPl, Children looked after at 31 March 2014 by in the LAs bundary by another LA 
CLA_NetGain, Children looked after at 31 March 2014 ? net gain of children by an LA
CLA_started2014, Children who started to be looked after during the year ending 31 March 2014
CLA_taken2014, Children who were taken into care during the year ending 31 March 2014 
SCLA_male, Male children who started to be looked after during the year ending 31 March 2014
SCLA_female, Female children who started to be looked after during the year ending 31 March 2014
SCLA_U1 , Children who started to be looked after during the year ending 31 March 2014 aged under 1 
SCLA_1to4, Children who started to be looked after during the year ending 31 March 2014 aged 1 to 4 years 
SCLA_5to9, Children who started to be looked after during the year ending 31 March 2014 aged 5 to 9 years 
SCLA_10to15, Children who started to be looked after during the year ending 31 March 2014 aged 0 to 15 years 
SCLA_16over, Children who started to be looked after during the year ending 31 March 2014 aged 16 years and over 
SCLA_AbNeg, Children who started to be looked after during the year ending 31 March 2014 due to abuse or neglect 
SCLA_Cdisab, Children who started to be looked after during the year ending 31 March 2014 due to the child?s disability 
SCLA_ParIll, Children who started to be looked after during the year ending 31 March 2014 due to parental illness or disability 
SCLA_FAcSt	, Children who started to be looked after during the year ending 31 March 2014 due to family in acute stress
SCLA_FD, Children who started to be looked after during the year ending 31 March 2014 due to family dysfunction 
SCLA_SUB, Children who started to be looked after during the year ending 31 March 2014 due to socially unacceptable behaviour 
SCLA_LI, Children who started to be looked after during the year ending 31 March 2014 due to low income 
SCLA_AbsPar, Children who started to be looked after during the year ending 31 March 2014 due to absent parenting 
SCLA_ICO, Children who started to be looked after during the year ending 31 March 2014 under an interim care order 
SCLA_FCO, Children who started to be looked after during the year ending 31 March 2014 under a full care order 
SCLA_POG, Children who started to be looked after during the year ending 31 March 2014 where a placement order has been granted 
SCLA_S20, Children who started to be looked after during the year ending 31 March 2014 under section 20 
SCLA_UPP, Children who started to be looked after during the year ending 31 March 2014 under police protection 
SCLA_SEPO	, Children who started to be looked after during the year ending 31 March 2014 subject to an emergency protection order 
SCLA_UCAO, Children who started to be looked after during the year ending 31 March 2014 under a child assessment order 
SCLA_ONCT, Children who started to be looked after during the year ending 31 March 2014 on remand or committed for trial 
SCLA_PACE	, Children who started to be looked after during the year ending 31 March 2014 under the police and criminal evidence act 
SCLA_SORR, Children who started to be looked after during the year ending 31 March 2014 under a supervision order with residence requirement 
CLA_cease, Children who ceased to be looked after during the year ending 31 March 2014	
CLA_cease16, Children aged 16 who ceased to be looked after during the year ending 31 March 2014
CLA_ceamal, Male children who ceased to be looked after during the year ending 31 March 2014
CLA_ceafe, Female children who ceased to be looked after during the year ending 31 March 2014	
CLA_cea1, Children who ceased to be looked after during the year ending 31 March 2014 aged under 1
CLA_cea14,	Children who ceased to be looked after during the year ending 31 March 2014 aged 1 to 4 years 
CLA_cea59,	Children who ceased to be looked after during the year ending 31 March 2014 aged 5 to 9 years 
CLA_cea1015, Children who ceased to be looked after during the year ending 31 March 2014 aged 10 to 15 years 	
CLA_cea16, Children who ceased to be looked after during the year ending 31 March 2014 aged 16 years and over 	
CLA_ceaAdop, Children who ceased to be looked after during the year ending 31 March 2014 due to adoption - application unopposed
CLA_ceaAdop2, Children who ceased to be looked after during the year ending 31 March 2014- consent dispensed with
CLA_ceaDied, Children who ceased to be looked after during the year ending 31 March 2014 as they have died 
CLA_ceataken, Children who ceased to be looked after during the year ending 31 March 2014 - care taken by another LA
CLA_ceaPar, Children who ceased to be looked after during the year ending 31 March 2014 - returned home to live with parents or relatives
CLA_ceaROG, Children who ceased to be looked after during the year ending 31 March 2014 - residence order granted
CLA_ceaSpecG, Children who ceased to be looked after during the year ending 31 March 2014 - special guardianship order made to former foster carers
CLA_ceaSpecG2, Children who ceased to be looked after during the year ending 31 March 2014 - special guardianship order made to carers other than former foster carers
CLA_ceaIndLiv, Children who ceased to be looked after during the year ending 31 March 2014 - moved into independent living (with supportive accommodation)
CLA_ceaIndLiv2, Children who ceased to be looked after during the year ending 31 March 2014 - moved into independent living (with no formalised support)
CLA_cea_tran_res, Children who ceased to be looked after during the year ending 31 March 2014 - transferred to residential care funded by adult social services	
CLA_cea_sen_cust, Children who ceased to be looked after during the year ending 31 March 2014 - sentenced to custody
CLA_cea_OthRea, Children who ceased to be looked after during the year ending 31 March 2014 - care ceased for any other reason
CLA_cease, Children who ceased to be looked after during the year ending 31 March 2014
CLA_ceaAdop, Children who ceased to be looked after during the year ending 31 March 2014 due to adoption
All_aged1920&21, All children now aged 19 20 and 21 years old who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday.
Act_HE, Number whose activity is higher education i.e. studies beyond A level
Act_OE, Number whose activity is education other than higher education	
Act_TE, Number whose activity is training or employment
Act_NEET_ill, Number who are not in education training or employment due to illness or disability	
Act_NEET_oth, Number who are not in education training or employment due to other reasons
Act_NEET_preg, Number who are not in education training or employment due to pregnancy or parenting
LA_Noinf, Local authority does not have information
ChildDied, Number of children who died after leaving care
Acc_SUIT, Total number in suitable accommodation
Acc_P	, Number accommodated with parents or relatives 	
Acc_CH, Number accommodated in community homes 	
Acc_SITA, Number accommodated in semi-independent transitional accommodation 	
Acc_SL, Number accommodated in supported lodgings
Acc_GA, Number gone abroad
Acc_Dep, Number deported
Acc_OL, Number accommodated in ordinary lodgings
Acc_ResNK, Number with residence not known
Acc_NFA, Number with no fixed abode or homeless
Acc_F	, Number accommodated in foyers
Acc_IL, Number accommodated in independent living	
Acc_EA, Number accommodated in emergency accommodation 	
Acc_BB, Number in bed and breakfast accommodation
Acc_Cust, Number who are in custody
Acc_FFC, Number accommodated with former foster carers  	
Acc_OTH, Number in other accommodation
LA_Noinf, Local authority does not have information
ChildDied, Number of children who died after leaving care

"

varlist_2014 <- read_csv(varlist_2014, col_names = c("varname", "description"))


varlist_2015 <- "
New_geog_code, New geography code
geog_l, Geography level
geog_c, Geography code
geog_n, Geography name
CLA_Mar2015, Children looked after at 31 March 2015
CLA_2015, Children looked after during the year ending 31 March 2015
CLA_stp2015, Children looked after during the year ending 31 March 2015 who were only looked after under a series of short term placements
CLA_male, Male children looked after at 31 March 2015
CLA_female, Female children looked after at 31 March 2015
CLA_U1, Children looked after at 31 March 2015 aged under 1 year 	
CLA_1to4, Children looked after at 31 March 2015 aged 1 to 4 years
CLA_5to9, Children looked after at 31 March 2015 aged 5 to 9 years 
CLA_10to15, Children looked after at 31 March 2015 aged 10 to 15 years
CLA_16over, Children looked after at 31 March 2015 aged 16 years and over 
CLA_White, Children looked after at 31 March 2015 from a white ethnic background
CLA_Mixed, Children looked after at 31 March 2015 from a mixed ethnic background
CLA_Asian, Children looked after at 31 March 2015 from an Asian ethnic background
CLA_Black, Children looked after at 31 March 2015 from a Black ethnic background
CLA_EOTH, Children looked after at 31 March 2015 from any other ethnic background
CLA_Oth, Children looked after at 31 March 2015 where ethnic background is not known 
CLA_Fost, Children looked after at 31 March 2015 in foster placements
CLA_Adopt, Children looked after at 31 March 2015 placed for adoption 
CLA_Parent, Children looked after at 31 March 2015 placed with parents
CLA_Ocom, Children looked after at 31 March 2015 placed in other placements within the community 
CLA_Secure, Children looked after at 31 March 2015 placed in secure units children?s homes and hostels 
CLA_Ores, Children looked after at 31 March 2015 placed in other residential accommodation 
CLA_RSch, Children looked after at 31 March 2015 placed in residential schools
CLA_OthPl, Children looked after at 31 March 2015 in other placements 
CLA_ICO, Children looked after at 31 March 2015 under an interim care order 
CLA_FCO, Children looked after at 31 March 2015 under a full care order 
CLA_FrAd, Children looked after at 31 March 2015 who are freed for adoption 
CLA_PlaceO, Children looked after at 31 March 2015 who have a placement order 
CLA_S20, Children looked after at 31 March 2015 under section 20 
CLA_CPG, Children looked after at 31 March 2015 under child protection grounds 
CLA_YJLS, Children looked after at 31 March 2015 under a youth justice legal status 
CLA_UASC, Children looked after at 31 March 2015 who are unaccompanied asylum seekers 
CLA_OwnP, Children looked after at 31 March 2015 where placement is provided by the LAs own provision 
CLA_OthLA, Children looked after at 31 March 2015 under where placement is provided by another LA 
CLA_OthPP, Children looked after at 31 March 2015 where placement is provided by other public provision 
CLA_Priv, Children looked after at 31 March 2015 where placement is provided by private provision 
CLA_Vol, Children looked after at 31 March 2015 where placement is provided by voluntary provision 
CLA_Par, Children looked after at 31 March 2015 where placement is provided by parents 
CLA_Nrep, Children looked after at 31 March 2015 where placement provider is not reported 
CLA_InBound, Children looked after at 31 March 2015 placed within LA boundary 
CLA_Outbound, Children looked after at 31 March 2015 placed outside of the LA boundary 
CLA_ExtPl, Children looked after at 31 March 2015 in the LA's boundary by another LA 
CLA_NetGain, Children looked after at 31 March 2015 ? net gain of children by an LA
CLA_started2015, Children who started to be looked after during the year ending 31 March 2015
CLA_taken2015, Children who were taken into care during the year ending 31 March 2015 
SCLA_male, Male children who started to be looked after during the year ending 31 March 2015
SCLA_female, Female children who started to be looked after during the year ending 31 March 2015
SCLA_U1 , Children who started to be looked after during the year ending 31 March 2015 aged under 1 
SCLA_1to4, Children who started to be looked after during the year ending 31 March 2015 aged 1 to 4 years 
SCLA_5to9, Children who started to be looked after during the year ending 31 March 2015 aged 5 to 9 years 
SCLA_10to15, Children who started to be looked after during the year ending 31 March 2015 aged 0 to 15 years 
SCLA_16over, Children who started to be looked after during the year ending 31 March 2015 aged 16 years and over 
SCLA_AbNeg, Children who started to be looked after during the year ending 31 March 2015 due to abuse or neglect 
SCLA_Cdisab, Children who started to be looked after during the year ending 31 March 2015 due to the child?s disability 
SCLA_ParIll, Children who started to be looked after during the year ending 31 March 2015 due to parental illness or disability 
SCLA_FAcSt, Children who started to be looked after during the year ending 31 March 2015 due to family in acute stress
SCLA_FD, Children who started to be looked after during the year ending 31 March 2015 due to family dysfunction 
SCLA_SUB, Children who started to be looked after during the year ending 31 March 2015 due to socially unacceptable behaviour 
SCLA_LI, Children who started to be looked after during the year ending 31 March 2015 due to low income 
SCLA_AbsPar, Children who started to be looked after during the year ending 31 March 2015 due to absent parenting 
SCLA_ICO, Children who started to be looked after during the year ending 31 March 2015 under an interim care order 
SCLA_FCO, Children who started to be looked after during the year ending 31 March 2015 under a full care order 
SCLA_POG, Children who started to be looked after during the year ending 31 March 2015 where a placement order has been granted 
SCLA_S20, Children who started to be looked after during the year ending 31 March 2015 under section 20 
SCLA_UPP, Children who started to be looked after during the year ending 31 March 2015 under police protection 
SCLA_SEPO, Children who started to be looked after during the year ending 31 March 2015 subject to an emergency protection order 
SCLA_UCAO, Children who started to be looked after during the year ending 31 March 2015 under a child assessment order 
SCLA_ONCT, Children who started to be looked after during the year ending 31 March 2015 on remand or committed for trial 
SCLA_PACE, Children who started to be looked after during the year ending 31 March 2015 under the police and criminal evidence act 
SCLA_SORR, Children who started to be looked after during the year ending 31 March 2015 under a supervision order with residence requirement 
CLA_cease, Children who ceased to be looked after during the year ending 31 March 2015	
CLA_ceamal, Male children who ceased to be looked after during the year ending 31 March 2015
CLA_ceafe, Female children who ceased to be looked after during the year ending 31 March 2015	
CLA_cea1, Children who ceased to be looked after during the year ending 31 March 2015 aged under 1
CLA_cea14, Children who ceased to be looked after during the year ending 31 March 2015 aged 1 to 4 years 
CLA_cea59, Children who ceased to be looked after during the year ending 31 March 2015 aged 5 to 9 years 
CLA_cea1015, Children who ceased to be looked after during the year ending 31 March 2015 aged 10 to 15 years 	
CLA_cea16, Children who ceased to be looked after during the year ending 31 March 2015 aged 16 years and over 	
CLA_ceaAdop, Children who ceased to be looked after during the year ending 31 March 2015 due to adoption - application unopposed
CLA_ceaAdop2, Children who ceased to be looked after during the year ending 31 March 2015- consent dispensed with
CLA_ceaDied, Children who ceased to be looked after during the year ending 31 March 2015 as they have died 
CLA_ceataken, Children who ceased to be looked after during the year ending 31 March 2015 - care taken by another LA
CLA_ceaParPlan, Children who ceased to be looked after during the year ending 31 March 2015 - returned home to live with parents or relatives as part of care planning process
CLA_ceaParNPlan, Children who ceased to be looked after during the year ending 31 March 2015 - returned home to live with parents or relatives not as part of care planning process
CLA_ceaNoPar, Children who ceased to be looked after during the year ending 31 March 2015 - ceased to live with parents or relatives without parental responsibility
CLA_ceaROG, Children who ceased to be looked after during the year ending 31 March 2015 - residence order granted
CLA_ceaSpecG, Children who ceased to be looked after during the year ending 31 March 2015 - special guardianship order made to former foster carers
CLA_ceaSpecG2, Children who ceased to be looked after during the year ending 31 March 2015 - special guardianship order made to carers other than former foster carers
CLA_ceaIndLiv, Children who ceased to be looked after during the year ending 31 March 2015 - moved into independent living (with supportive accommodation)
CLA_ceaIndLiv2, Children who ceased to be looked after during the year ending 31 March 2015 - moved into independent living (with no formalised support)
CLA_cea_tran_res, Children who ceased to be looked after during the year ending 31 March 2015 - transferred to residential care funded by adult social services	
CLA_cea_sen_cust, Children who ceased to be looked after during the year ending 31 March 2015 - sentenced to custody
CLA_ceaRemEnd, Children who ceased to be looked after during the year ending 31 March 2015 - accommodation on remand ended
CLA_ceaAgeAssmt, Children who ceased to be looked after during the year ending 31 March 2015 - age assessment determined child was 18 or over
CLA_ceaAbroad, Children who ceased to be looked after during the year ending 31 March 2015 - child moved abroad
CLA_cea_OthRea, Children who ceased to be looked after during the year ending 31 March 2015 - care ceased for any other reason
CLA_cease, Children who ceased to be looked after during the year ending 31 March 2015
CLA_ceaAdop, Children who ceased to be looked after during the year ending 31 March 2015 due to adoption
All_aged1920&21, All children now aged 19 20 and 21 years old who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday.
Act_HE, Number whose activity is higher education i.e. studies beyond A level
Act_OE, Number whose activity is education other than higher education	
Act_TE, Number whose activity is training or employment
Act_NEET_ill, Number who are not in education training or employment due to illness or disability	
Act_NEET_oth, Number who are not in education training or employment due to other reasons
Act_NEET_preg, Number who are not in education training or employment due to pregnancy or parenting
LA_NoActinf, Local authority does not have activity information
Acc_SUIT, Total number in suitable accommodation
Acc_P, Number accommodated with parents or relatives 	
Acc_CH, Number accommodated in community homes 	
Acc_SITA, Number accommodated in semi-independent transitional accommodation 	
Acc_SL, Number accommodated in supported lodgings
Acc_GA, Number gone abroad
Acc_Dep, Number deported
Acc_OL, Number accommodated in ordinary lodgings
Acc_ResNK, Number with residence not known
Acc_NFA, Number with no fixed abode or homeless
Acc_F, Number accommodated in foyers
Acc_IL, Number accommodated in independent living	
Acc_EA, Number accommodated in emergency accommodation 	
Acc_BB, Number in bed and breakfast accommodation
Acc_Cust, Number who are in custody
Acc_FFC, Number accommodated with former foster carers  	
Acc_OTH, Number in other accommodation
LA_NoAccinf, Local authority does not have accommodation information
CLA_MissDuringYear, Children looked after who had a missing incident during the year
CLA_MissIncs, Number of missing incidents during the year
CLA_MissMoreOnce, Children looked after who went missing more than once during the year
CLA_Miss31Mar, Children looked after who were missing at 31 March 2015
CLA_AwayDuringYear, Children looked after who were away from placement without authorisation during the year
CLA_AwayIncs, Number of away from placements without authorisation incidents during the year
CLA_AwayMoreOnce, Children looked after who were away from placement without authorisation more than once during the year
CLA_Away31Mar, Children looked after who were away from placement without authorisation at 31 March 2015
CLA_12mths, Children looked after at 31 March 2015 for at least 12 months
CLA_10over, Children looked after at 31 March 2015 for at least 12 months aged 10 years and over
CLA_convicted, Children looked after at 31 March 2015 convicted or subject to a final warning or reprimand during the year
CLA_submisuse, Children looked after at 31 March 2015 identified as having a substance misuse problem
CLA_subint, Children looked after at 31 March 2015 received intervention for substance misuse
CLA_suboffint, Children looked after at 31 March 2015 offered intervention for substance misuse and refused
CLA_immunisation, Children looked after at 31 March 2015 whose immunisations were up to date
CLA_teethcheck, Children looked after at 31 March 2015 who had their teeth checked
CLA_healthassmt, Children looked after at 31 March 2015 who had their annual health assessment
CLA_5under, Children looked after at 31 March 2015 for at least 12 months aged 5 years and under
CLA_devassmt, Children looked after at 31 March 2015 whose development assessments were up to date
CLA_5to16, Children looked after at 31 March 2015 for at least 12 months aged 5 to 16 years
CLA_SDQ, Children looked after at 31 March 2015 with an SDQ score
CLA_SDQnormal, Children looked after at 31 March 2015 whose SDQ score was normal
CLA_SDQborderline, Children looked after at 31 March 2015 whose SDQ score was borderline
CLA_SDQconcern, Children looked after at 31 March 2015 whose SDQ score was a cause for concern
"  

varlist_2015 <- read_csv(varlist_2015, col_names = c("varname", "description"))


varlist_2016 <- "

New_geog_code, New geography code
geog_l, Geography level
geog_c, Geography code
geog_n, Geography name
CLA_Mar2016, Children looked after at 31 March 2016
CLA_2016, Children looked after during the year ending 31 March 2016
CLA_stp2016, Children looked after during the year ending 31 March 2016 who were only looked after under a series of short term placements
CLA_male, Male children looked after at 31 March 2016
CLA_female, Female children looked after at 31 March 2016
CLA_U1, Children looked after at 31 March 2016 aged under 1 year
CLA_1to4, Children looked after at 31 March 2016 aged 1 to 4 years
CLA_5to9, Children looked after at 31 March 2016 aged 5 to 9 years 
CLA_10to15, Children looked after at 31 March 2016 aged 10 to 15 years
CLA_16over, Children looked after at 31 March 2016 aged 16 years and over
CLA_White, Children looked after at 31 March 2016 from a white ethnic background
CLA_Mixed, Children looked after at 31 March 2016 from a mixed ethnic background
CLA_Asian, Children looked after at 31 March 2016 from an Asian ethnic background
CLA_Black, Children looked after at 31 March 2016 from a Black ethnic background
CLA_EOTH, Children looked after at 31 March 2016 from any other ethnic background
CLA_Oth, Children looked after at 31 March 2016 where ethnic background is not known
CLA_Fost, Children looked after at 31 March 2016 in foster placements
CLA_Adopt, Children looked after at 31 March 2016 placed for adoption
CLA_Parent, Children looked after at 31 March 2016 placed with parents
CLA_Ocom, Children looked after at 31 March 2016 placed in other placements within the community
CLA_Secure, Children looked after at 31 March 2016 placed in secure units children?s homes and hostels
CLA_Ores, Children looked after at 31 March 2016 placed in other residential accommodation
CLA_RSch, Children looked after at 31 March 2016 placed in residential schools
CLA_OthPl, Children looked after at 31 March 2016 in other placements
CLA_ICO, Children looked after at 31 March 2016 under an interim care order
CLA_FCO, Children looked after at 31 March 2016 under a full care order
CLA_FrAd, Children looked after at 31 March 2016 who are freed for adoption
CLA_PlaceO, Children looked after at 31 March 2016 who have a placement order
CLA_S20, Children looked after at 31 March 2016 under section 20
CLA_CPG, Children looked after at 31 March 2016 under child protection grounds
CLA_YJLS, Children looked after at 31 March 2016 under a youth justice legal status
CLA_UASC, Children looked after at 31 March 2016 who were unaccompanied asylum seekers during the year
CLA_OwnP, Children looked after at 31 March 2016 where placement is provided by the LAs own provision
CLA_OthLA, Children looked after at 31 March 2016 under where placement is provided by another LA
CLA_OthPP, Children looked after at 31 March 2016 where placement is provided by other public provision
CLA_Priv, Children looked after at 31 March 2016 where placement is provided by private provision
CLA_Vol, Children looked after at 31 March 2016 where placement is provided by voluntary provision
CLA_Par, Children looked after at 31 March 2016 where placement is provided by parents
CLA_Nrep, Children looked after at 31 March 2016 where placement provider is not reported
CLA_InBound, Children looked after at 31 March 2016 placed within LA boundary
CLA_Outbound, Children looked after at 31 March 2016 placed outside of the LA boundary
CLA_ExtPl, Children looked after at 31 March 2016 in the LA's boundary by another LA
CLA_NetGain, Children looked after at 31 March 2016 ? net gain of children by an LA
CLA_started2016, Children who started to be looked after during the year ending 31 March 2016
CLA_taken2016, Children who were taken into care during the year ending 31 March 2016
SCLA_male, Male children who started to be looked after during the year ending 31 March 2016
SCLA_female, Female children who started to be looked after during the year ending 31 March 2016
SCLA_U1 , Children who started to be looked after during the year ending 31 March 2016 aged under 1
SCLA_1to4, Children who started to be looked after during the year ending 31 March 2016 aged 1 to 4 years
SCLA_5to9, Children who started to be looked after during the year ending 31 March 2016 aged 5 to 9 years
SCLA_10to15, Children who started to be looked after during the year ending 31 March 2016 aged 10 to 15 years
SCLA_16over, Children who started to be looked after during the year ending 31 March 2016 aged 16 years and over
SCLA_AbNeg, Children who started to be looked after during the year ending 31 March 2016 due to abuse or neglect
SCLA_Cdisab, Children who started to be looked after during the year ending 31 March 2016 due to the child?s disability
SCLA_ParIll, Children who started to be looked after during the year ending 31 March 2016 due to parental illness or disability
SCLA_FAcSt, Children who started to be looked after during the year ending 31 March 2016 due to family in acute stress
SCLA_FD, Children who started to be looked after during the year ending 31 March 2016 due to family dysfunction
SCLA_SUB, Children who started to be looked after during the year ending 31 March 2016 due to socially unacceptable behaviour
SCLA_LI, Children who started to be looked after during the year ending 31 March 2016 due to low income
SCLA_AbsPar, Children who started to be looked after during the year ending 31 March 2016 due to absent parenting
SCLA_ICO, Children who started to be looked after during the year ending 31 March 2016 under an interim care order
SCLA_FCO, Children who started to be looked after during the year ending 31 March 2016 under a full care order
SCLA_POG, Children who started to be looked after during the year ending 31 March 2016 where a placement order has been granted
SCLA_S20, Children who started to be looked after during the year ending 31 March 2016 under section 20
SCLA_UPP, Children who started to be looked after during the year ending 31 March 2016 under police protection
SCLA_SEPO, Children who started to be looked after during the year ending 31 March 2016 subject to an emergency protection order
SCLA_UCAO, Children who started to be looked after during the year ending 31 March 2016 under a child assessment order
SCLA_ONCT, Children who started to be looked after during the year ending 31 March 2016 on remand or committed for trial
SCLA_PACE, Children who started to be looked after during the year ending 31 March 2016 under the police and criminal evidence act
SCLA_SORR, Children who started to be looked after during the year ending 31 March 2016 under a supervision order with residence requirement
CLA_cease, Children who ceased to be looked after during the year ending 31 March 2016
CLA_ceamal, Male children who ceased to be looked after during the year ending 31 March 2016
CLA_ceafe, Female children who ceased to be looked after during the year ending 31 March 2016
CLA_cea1, Children who ceased to be looked after during the year ending 31 March 2016 aged under 1
CLA_cea14, Children who ceased to be looked after during the year ending 31 March 2016 aged 1 to 4 years
CLA_cea59, Children who ceased to be looked after during the year ending 31 March 2016 aged 5 to 9 years
CLA_cea1015, Children who ceased to be looked after during the year ending 31 March 2016 aged 10 to 15 years
CLA_cea16, Children who ceased to be looked after during the year ending 31 March 2016 aged 16 years
CLA_cea17, Children who ceased to be looked after during the year ending 31 March 2016 aged 17 years
CLA_cea18, Children who ceased to be looked after during the year ending 31 March 2016 aged 18 years and over
CLA_ceaAdop1, Children who ceased to be looked after during the year ending 31 March 2016 due to adoption - application unopposed
CLA_ceaAdop2, Children who ceased to be looked after during the year ending 31 March 2016 - consent dispensed with
CLA_ceaDied, Children who ceased to be looked after during the year ending 31 March 2016 as they have died
CLA_ceataken, Children who ceased to be looked after during the year ending 31 March 2016 - care taken by another LA
CLA_ceaParPlan, Children who ceased to be looked after during the year ending 31 March 2016 - returned home to live with parents or relatives as part of care planning process
CLA_ceaParNPlan, Children who ceased to be looked after during the year ending 31 March 2016 - returned home to live with parents or relatives not as part of care planning process
CLA_ceaNoPar, Children who ceased to be looked after during the year ending 31 March 2016 - ceased to live with parents or relatives without parental responsibility
CLA_ceaROG, Children who ceased to be looked after during the year ending 31 March 2016 - residence order granted
CLA_ceaSpecG1, Children who ceased to be looked after during the year ending 31 March 2016 - special guardianship order made to former foster carers
CLA_ceaSpecG2, Children who ceased to be looked after during the year ending 31 March 2016 - special guardianship order made to carers other than former foster carers
CLA_ceaIndLiv1, Children who ceased to be looked after during the year ending 31 March 2016 - moved into independent living (with supportive accommodation)
CLA_ceaIndLiv2, Children who ceased to be looked after during the year ending 31 March 2016 - moved into independent living (with no formalised support)
CLA_cea_tran_res, Children who ceased to be looked after during the year ending 31 March 2016 - transferred to residential care funded by adult social services
CLA_cea_sen_cust, Children who ceased to be looked after during the year ending 31 March 2016 - sentenced to custody
CLA_ceaRemEnd, Children who ceased to be looked after during the year ending 31 March 2016 - accommodation on remand ended
CLA_ceaAgeAssmt, Children who ceased to be looked after during the year ending 31 March 2016 - age assessment determined child was 18 or over
CLA_ceaAbroad, Children who ceased to be looked after during the year ending 31 March 2016 - child moved abroad
CLA_cea_OthRea, Children who ceased to be looked after during the year ending 31 March 2016 - care ceased for any other reason
All_aged1920&21, All care leavers now aged 19 20 and 21 years old who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday
All_aged19, All care leavers now aged 19 years old who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday
All_aged20, All care leavers now aged 20 years old who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday
All_aged21, All care leavers now aged 21 years old who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday
All_aged17&18, All care leavers now aged 17 and 18 years old who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday
All_aged17, All care leavers now aged 17 years old who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday
All_aged18, All care leavers now aged 18 years old who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday
Act_HE1920&21, Number aged 19 20 and 21 years old whose activity is higher education i.e. studies beyond A level
Act_HE19, Number aged 19 years old whose activity is higher education i.e. studies beyond A level
Act_HE20, Number aged 20 years old whose activity is higher education i.e. studies beyond A level
Act_HE21, Number aged 21 years old whose activity is higher education i.e. studies beyond A level
Act_HE17&18, Number aged 17 and 18 years old whose activity is higher education i.e. studies beyond A level
Act_HE17, Number aged 17 years old whose activity is higher education i.e. studies beyond A level
Act_HE18, Number aged 18 years old whose activity is higher education i.e. studies beyond A level
Act_OE1920&21, Number aged 19 20 and 21 years old whose activity is education other than higher education
Act_OE19, Number aged 19 years old whose activity is education other than higher education
Act_OE20, Number aged 20 years old whose activity is education other than higher education
Act_OE21, Number aged 21 years old whose activity is education other than higher education
Act_OE17&18, Number aged 17 and 18 years old whose activity is education other than higher education
Act_OE17, Number aged 17 years old whose activity is education other than higher education
Act_OE18, Number aged 18 years old whose activity is education other than higher education
Act_TE1920&21, Number aged 19 20 and 21 years old whose activity is training or employment
Act_TE19, Number aged 19 years old whose activity is training or employment
Act_TE20, Number aged 20 years old whose activity is training or employment
Act_TE21, Number aged 21 years old whose activity is training or employment
Act_TE17&18, Number aged 17 and 18 years old whose activity is training or employment
Act_TE17, Number aged 17 years old whose activity is training or employment
Act_TE18, Number aged 18 years old whose activity is training or employment
Act_NEET_ill1920&21, Number aged 19 20 and 21 years old who are not in education training or employment due to illness or disability
Act_NEET_ill19, Number aged 19 years old who are not in education training or employment due to illness or disability
Act_NEET_ill20, Number aged 20 years old who are not in education training or employment due to illness or disability
Act_NEET_ill21, Number aged 21 years old who are not in education training or employment due to illness or disability
Act_NEET_ill17&18, Number aged 17 and 18 years old who are not in education training or employment due to illness or disability
Act_NEET_ill17, Number aged 17 years old who are not in education training or employment due to illness or disability
Act_NEET_ill18, Number aged 18 years old who are not in education training or employment due to illness or disability
Act_NEET_oth1920&21, Number aged 19 20 and 21 years old who are not in education training or employment due to other reasons
Act_NEET_oth19, Number aged 19 years old who are not in education training or employment due to other reasons
Act_NEET_oth20, Number aged 20 years old who are not in education training or employment due to other reasons
Act_NEET_oth21, Number aged 21 years old who are not in education training or employment due to other reasons
Act_NEET_oth17&18, Number aged 17 and 18 years old who are not in education training or employment due to other reasons
Act_NEET_oth17, Number aged 17 years old who are not in education training or employment due to other reasons
Act_NEET_oth18, Number aged 18 years old who are not in education training or employment due to other reasons
Act_NEET_preg1920&21, Number aged 19 20 and 21 years old who are not in education training or employment due to pregnancy or parenting
Act_NEET_preg19, Number aged 19 years old who are not in education training or employment due to pregnancy or parenting
Act_NEET_preg20, Number aged 20 years old who are not in education training or employment due to pregnancy or parenting
Act_NEET_preg21, Number aged 21 years old who are not in education training or employment due to pregnancy or parenting
Act_NEET_preg17&18, Number aged 17 and 18 years old who are not in education training or employment due to pregnancy or parenting
Act_NEET_preg17, Number aged 17 years old who are not in education training or employment due to pregnancy or parenting
Act_NEET_preg18, Number aged 18 years old who are not in education training or employment due to pregnancy or parenting
LA_NoActinf1920&21, Number aged 19 20 and 21 years old for whom local authority does not have activity information
LA_NoActinf19, Number aged 19 years old for whom local authority does not have activity information
LA_NoActinf20, Number aged 20 years old for whom local authority does not have activity information
LA_NoActinf21, Number aged 21 years old for whom local authority does not have activity information
LA_NoActinf17&18, Number aged 17 and 18 years old for whom local authority does not have activity information
LA_NoActinf17, Number aged 17 years old for whom local authority does not have activity information
LA_NoActinf18, Number aged 18 years old for whom local authority does not have activity information
Acc_SUIT1920&21, Total number aged 19 20 and 21 years old in suitable accommodation
Acc_SUIT19, Total number aged 19 years old in suitable accommodation
Acc_SUIT20, Total number aged 20 years old in suitable accommodation
Acc_SUIT21, Total number aged 21 years old in suitable accommodation
Acc_SUIT17&18, Total number aged 17 and 18 years old in suitable accommodation
Acc_SUIT17, Total number aged 17 years old in suitable accommodation
Acc_SUIT18, Total number aged 18 years old in suitable accommodation
Acc_P1920&21, Number aged 19 20 and 21 years old accommodated with parents or relatives
Acc_P19, Number aged 19 years old accommodated with parents or relatives
Acc_P20, Number aged 20 years old accommodated with parents or relatives
Acc_P21, Number aged 21 years old accommodated with parents or relatives
Acc_P17&18, Number aged 17 and 18 years old accommodated with parents or relatives
Acc_P17, Number aged 17 years old accommodated with parents or relatives
Acc_P18, Number aged 18 years old accommodated with parents or relatives
Acc_CH1920&21, Number aged 19 20 and 21 years old accommodated in community homes
Acc_CH19, Number aged 19 years old accommodated in community homes
Acc_CH20, Number aged 20 years old accommodated in community homes
Acc_CH21, Number aged 21 years old accommodated in community homes
Acc_CH17&18, Number aged 17 and 18 years old accommodated in community homes
Acc_CH17, Number aged 17 years old accommodated in community homes
Acc_CH18, Number aged 18 years old accommodated in community homes
Acc_SITA1920&21, Number aged 19 20 and 21 years old accommodated in semi-independent transitional accommodation
Acc_SITA19, Number aged 19 years old accommodated in semi-independent transitional accommodation
Acc_SITA20, Number aged 20 years old accommodated in semi-independent transitional accommodation
Acc_SITA21, Number aged 21 years old accommodated in semi-independent transitional accommodation
Acc_SITA17&18, Number aged 17 and 18 years old accommodated in semi-independent transitional accommodation
Acc_SITA17, Number aged 17 years old accommodated in semi-independent transitional accommodation
Acc_SITA18, Number aged 18 years old accommodated in semi-independent transitional accommodation
Acc_SL1920&21, Number aged 19 20 and 21 years old accommodated in supported lodgings
Acc_SL19, Number aged 19 years old accommodated in supported lodgings
Acc_SL20, Number aged 20 years old accommodated in supported lodgings
Acc_SL21, Number aged 21 years old accommodated in supported lodgings
Acc_SL17&18, Number aged 17 and 18 years old accommodated in supported lodgings
Acc_SL17, Number aged 17 years old accommodated in supported lodgings
Acc_SL18, Number aged 18 years old accommodated in supported lodgings
Acc_GA1920&21, Number aged 19 20 and 21 years old gone abroad
Acc_GA19, Number aged 19 years old gone abroad
Acc_GA20, Number aged 20 years old gone abroad
Acc_GA21, Number aged 21 years old gone abroad
Acc_GA17&18, Number aged 17 and 18 years old gone abroad
Acc_GA17, Number aged 17 years old gone abroad
Acc_GA18, Number aged 18 years old gone abroad
Acc_Dep1920&21, Number aged 19 20 and 21 years old deported
Acc_Dep19, Number aged 19 years old deported
Acc_Dep20, Number aged 20 years old deported
Acc_Dep21, Number aged 21 years old deported
Acc_Dep17&18, Number aged 17 and 18 years old deported
Acc_Dep17, Number aged 17 years old deported
Acc_Dep18, Number aged 18 years old deported
Acc_OL1920&21, Number aged 19 20 and 21 years old accommodated in ordinary lodgings
Acc_OL19, Number aged 19 years old accommodated in ordinary lodgings
Acc_OL20, Number aged 20 years old accommodated in ordinary lodgings
Acc_OL21, Number aged 21 years old accommodated in ordinary lodgings
Acc_OL17&18, Number aged 17 and 18 years old accommodated in ordinary lodgings
Acc_OL17, Number aged 17 years old accommodated in ordinary lodgings
Acc_OL18, Number aged 18 years old accommodated in ordinary lodgings
Acc_ResNK1920&21, Number aged 19 20 and 21 years old with residence not known
Acc_ResNK19, Number aged 19 years old with residence not known
Acc_ResNK20, Number aged 20 years old with residence not known
Acc_ResNK21, Number aged 21 years old with residence not known
Acc_ResNK17&18, Number aged 17 and 18 years old with residence not known
Acc_ResNK17, Number aged 17 years old with residence not known
Acc_ResNK18, Number aged 18 years old with residence not known
Acc_NFA1920&21, Number aged 19 20 and 21 years old with no fixed abode or homeless
Acc_NFA19, Number aged 19 years old with no fixed abode or homeless
Acc_NFA20, Number aged 20 years old with no fixed abode or homeless
Acc_NFA21, Number aged 21 years old with no fixed abode or homeless
Acc_NFA17&18, Number aged 17 and 18 years old with no fixed abode or homeless
Acc_NFA17, Number aged 17 years old with no fixed abode or homeless
Acc_NFA18, Number aged 18 years old with no fixed abode or homeless
Acc_F1920&21, Number aged 19 20 and 21 years old accommodated in foyers
Acc_F19, Number aged 19 years old accommodated in foyers
Acc_F20, Number aged 20 years old accommodated in foyers
Acc_F21, Number aged 21 years old accommodated in foyers
Acc_F17&18, Number aged 17 and 18 years old accommodated in foyers
Acc_F17, Number aged 17 years old accommodated in foyers
Acc_F18, Number aged 18 years old accommodated in foyers
Acc_IL1920&21, Number aged 19 20 and 21 years old accommodated in independent living
Acc_IL19, Number aged 19 years old accommodated in independent living
Acc_IL20, Number aged 20 years old accommodated in independent living
Acc_IL21, Number aged 21 years old accommodated in independent living
Acc_IL17&18, Number aged 17 and 18 years old accommodated in independent living
Acc_IL17, Number aged 17 years old accommodated in independent living
Acc_IL18, Number aged 18 years old accommodated in independent living
Acc_EA1920&21, Number aged 19 20 and 21 years old accommodated in emergency accommodation
Acc_EA19, Number aged 19 years old accommodated in emergency accommodation
Acc_EA20, Number aged 20 years old accommodated in emergency accommodation
Acc_EA21, Number aged 21 years old accommodated in emergency accommodation
Acc_EA17&18, Number aged 17 and 18 years old accommodated in emergency accommodation
Acc_EA17, Number aged 17 years old accommodated in emergency accommodation
Acc_EA18, Number aged 18 years old accommodated in emergency accommodation
Acc_BB1920&21, Number aged 19 20 and 21 years old in bed and breakfast accommodation
Acc_BB19, Number aged 19 years old in bed and breakfast accommodation
Acc_BB20, Number aged 20 years old in bed and breakfast accommodation
Acc_BB21, Number aged 21 years old in bed and breakfast accommodation
Acc_BB17&18, Number aged 17 and 18 years old in bed and breakfast accommodation
Acc_BB17, Number aged 17 years old in bed and breakfast accommodation
Acc_BB18, Number aged 18 years old in bed and breakfast accommodation
Acc_Cust1920&21, Number aged 19 20 and 21 years old who are in custody
Acc_Cust19, Number aged 19 years old who are in custody
Acc_Cust20, Number aged 20 years old who are in custody
Acc_Cust21, Number aged 21 years old who are in custody
Acc_Cust17&18, Number aged 17 and 18 years old who are in custody
Acc_Cust17, Number aged 17 years old who are in custody
Acc_Cust17, Number aged 18 years old who are in custody
Acc_FFC1920&21, Number aged 19 20 and 21 years old accommodated with former foster carers
Acc_FFC19, Number aged 19 years old accommodated with former foster carers
Acc_FFC20, Number aged 20 years old accommodated with former foster carers
Acc_FFC21, Number aged 21 years old accommodated with former foster carers
Acc_FFC17&18, Number aged 17 and 18 years old accommodated with former foster carers
Acc_FFC17, Number aged 17 years old accommodated with former foster carers
Acc_FFC18, Number aged 18 years old accommodated with former foster carers
Acc_OTH1920&21, Number aged 19 20 and 21 years old in other accommodation
Acc_OTH19, Number aged 19 years old in other accommodation
Acc_OTH20, Number aged 20 years old in other accommodation
Acc_OTH21, Number aged 21 years old in other accommodation
Acc_OTH17&18, Number aged 17 and 18 years old in other accommodation
Acc_OTH17, Number aged 17 years old in other accommodation
Acc_OTH18, Number aged 18 years old in other accommodation
LA_NoAccinf1920&21, Number aged 19 20 and 21 years old for whom local authority does not have accommodation information
LA_NoAccinf19, Number aged 19 years old for whom local authority does not have accommodation information
LA_NoAccinf20, Number aged 20 years old for whom local authority does not have accommodation information
LA_NoAccinf21, Number aged 21 years old for whom local authority does not have accommodation information
LA_NoAccinf17&18, Number aged 17 and 18 years old for whom local authority does not have accommodation information
LA_NoAccinf17, Number aged 17 years old for whom local authority does not have accommodation information
LA_NoAccinf18, Number aged 18 years old for whom local authority does not have accommodation information
CLA_MissDuringYear, Children looked after who had a missing incident during the year
CLA_MissIncs, Number of missing incidents during the year
CLA_MissMoreOnce, Children looked after who went missing more than once during the year
CLA_Miss31Mar, Children looked after who were missing at 31 March 2016
CLA_AwayDuringYear, Children looked after who were away from placement without authorisation during the year
CLA_AwayIncs, Number of away from placements without authorisation incidents during the year
CLA_AwayMoreOnce, Children looked after who were away from placement without authorisation more than once during the year
CLA_Away31Mar, Children looked after who were away from placement without authorisation at 31 March 2016
CLA_12mths, Children looked after at 31 March 2016 for at least 12 months
CLA_10over, Children looked after at 31 March 2016 for at least 12 months aged 10 years and over
CLA_convicted, Children looked after at 31 March 2016 convicted or subject to a final warning or reprimand during the year
CLA_submisuse, Children looked after at 31 March 2016 identified as having a substance misuse problem
CLA_subint, Children looked after at 31 March 2016 received intervention for substance misuse
CLA_suboffint, Children looked after at 31 March 2016 offered intervention for substance misuse and refused
CLA_immunisation, Children looked after at 31 March 2016 whose immunisations were up to date
CLA_teethcheck, Children looked after at 31 March 2016 who had their teeth checked
CLA_healthassmt, Children looked after at 31 March 2016 who had their annual health assessment
CLA_under5, Children looked after at 31 March 2016 for at least 12 months aged under 5 years
CLA_devassmt, Children looked after at 31 March 2016 whose development assessments were up to date
CLA_5to16, Children looked after at 31 March 2016 for at least 12 months aged 5 to 16 years
CLA_SDQ, Children looked after at 31 March 2016 with an SDQ score
CLA_SDQnormal, Children looked after at 31 March 2016 whose SDQ score was normal
CLA_SDQborderline, Children looked after at 31 March 2016 whose SDQ score was borderline
CLA_SDQconcern, Children looked after at 31 March 2016 whose SDQ score was a cause for concern

"

varlist_2016 <- read_csv(varlist_2016, col_names = c("varname", "description"))


varlist_2017 <- "

New_geog_code, New geography code
geog_l, Geography level
geog_c, Geography code
geog_n, Geography name
CLA_Mar2017, Children looked after at 31 March 2017
CLA_2017, Children looked after during the year ending 31 March 2017
CLA_stp2017, Children looked after during the year ending 31 March 2017 who were only looked after under a series of short term placements
CLA_male, Male children looked after at 31 March 2017
CLA_female, Female children looked after at 31 March 2017
CLA_U1, Children looked after at 31 March 2017 aged under 1
CLA_1to4, Children looked after at 31 March 2017 aged 1 to 4
CLA_5to9, Children looked after at 31 March 2017 aged 5 to 9
CLA_10to15, Children looked after at 31 March 2017 aged 10 to 15
CLA_16over, Children looked after at 31 March 2017 aged 16 and over
CLA_White, Children looked after at 31 March 2017 from a white ethnic background
CLA_Mixed, Children looked after at 31 March 2017 from a mixed ethnic background
CLA_Asian, Children looked after at 31 March 2017 from an Asian ethnic background
CLA_Black, Children looked after at 31 March 2017 from a Black ethnic background
CLA_EOTH, Children looked after at 31 March 2017 from any other ethnic background
CLA_Oth, Children looked after at 31 March 2017 where ethnic background is not known
CLA_Fost, Children looked after at 31 March 2017 in foster placements
CLA_Adopt, Children looked after at 31 March 2017 placed for adoption
CLA_Parent, Children looked after at 31 March 2017 placed with parents
CLA_Ocom, Children looked after at 31 March 2017 placed in other placements within the community
CLA_Secure, Children looked after at 31 March 2017 placed in secure units children?s homes and hostels
CLA_Ores, Children looked after at 31 March 2017 placed in other residential accommodation
CLA_RSch, Children looked after at 31 March 2017 placed in residential schools
CLA_OthPl, Children looked after at 31 March 2017 in other placements
CLA_ICO, Children looked after at 31 March 2017 under an interim care order
CLA_FCO, Children looked after at 31 March 2017 under a full care order
CLA_FrAd, Children looked after at 31 March 2017 who are freed for adoption
CLA_PlaceO, Children looked after at 31 March 2017 who have a placement order
CLA_S20, Children looked after at 31 March 2017 under section 20
CLA_CPG, Children looked after at 31 March 2017 under child protection grounds
CLA_YJLS, Children looked after at 31 March 2017 under a youth justice legal status
CLA_UASC, Children looked after at 31 March 2017 who were unaccompanied asylum seekers during the year
CLA_OwnP, Children looked after at 31 March 2017 where placement is provided by the local authority?s own provision
CLA_OthLA, Children looked after at 31 March 2017 under where placement is provided by another local authority
CLA_OthPP, Children looked after at 31 March 2017 where placement is provided by other public provision
CLA_Priv, Children looked after at 31 March 2017 where placement is provided by private provision
CLA_Vol, Children looked after at 31 March 2017 where placement is provided by voluntary provision
CLA_Par, Children looked after at 31 March 2017 where placement is provided by parents
CLA_Nrep, Children looked after at 31 March 2017 where placement provider is not reported
CLA_InBound, Children looked after at 31 March 2017 placed within the local authority boundary
CLA_OutBound, Children looked after at 31 March 2017 placed outside of the local authority boundary
CLA_InLA_LTE20, Children looked after at 31 March 2017 placed within the local authority boundary and within 20 miles of the child?s home
CLA_OutLA_LTE20, Children looked after at 31 March 2017 placed outside of the local authority boundary and within 20 miles of the child?s home
CLA_InLA_GT20, Children looked after at 31 March 2017 placed within the local authority boundary and over 20 miles from the child?s home
CLA_OutLA_GT20, Children looked after at 31 March 2017 placed outside of the local authority boundary and over 20 miles from the child?s home
CLA_InLA_NoInfo, Children looked after at 31 March 2017 placed within the local authority boundary where the distance from the child?s home is not known or not recorded
CLA_OutLA_NoInfo, Children looked after at 31 March 2017 placed outside of the local authority boundary where the distance from the child?s home is not known or not recorded
CLA_LAPl, Children looked after at 31 March 2017 who were the responsibility of all local authorities placed within the local authority boundary
CLA_IntPl, Children looked after at 31 March 2017 who were the responsibility of the internal local authority placed internally within the local authority boundary
CLA_ExtPl, Children looked after at 31 March 2017 who were the responsibility of an external local authority placed within the local authority boundary
CLA_NetGain, Children looked after at 31 March 2017 ? net gain of children by a local authority
CLA_RPC_All_Place, Placements that ended during the year ending 31 March 2017
CLA_RPC_CARPL, Placements that ended during the year ending 31 March 2017 due to a change to/implementation of care plan
CLA_RPC_CLOSE, Placements that ended during the year ending 31 March 2017 due to resignation or closure of provision
CLA_RPC_ALLEG, Placements that ended during the year ending 31 March 2017 due to allegation
CLA_RPC_STAND, Placements that ended during the year ending 31 March 2017 due to standards of care concern
CLA_RPC_APPRR, Placements that ended during the year ending 31 March 2017 due to removal of approval
CLA_RPC_CREQB, Placements that ended during the year ending 31 March 2017 due to the carer requesting the placement end because of the child?s behaviour
CLA_RPC_CREQO, Placements that ended during the year ending 31 March 2017 due to the carer requesting the placement end for other reasons
CLA_RPC_CHILD, Placements that ended during the year ending 31 March 2017 due to the child requesting the placement end
CLA_RPC_LAREQ, Placements that ended during the year ending 31 March 2017 due to the responsible area/authority requesting the placement end
CLA_RPC_PLACE, Placements that ended during the year ending 31 March 2017 due to the change in the status of a placement only
CLA_RPC_OTHER, Placements that ended during the year ending 31 March 2017 due to other reasons
CLA_started2017, Children who started to be looked after during the year ending 31 March 2017
CLA_taken2017, Children who were taken into care during the year ending 31 March 2017
SCLA_male, Male children who started to be looked after during the year ending 31 March 2017
SCLA_female, Female children who started to be looked after during the year ending 31 March 2017
SCLA_U1, Children who started to be looked after during the year ending 31 March 2017 aged under 1
SCLA_1to4, Children who started to be looked after during the year ending 31 March 2017 aged 1 to 4
SCLA_5to9, Children who started to be looked after during the year ending 31 March 2017 aged 5 to 9
SCLA_10to15, Children who started to be looked after during the year ending 31 March 2017 aged 10 to 15
SCLA_16over, Children who started to be looked after during the year ending 31 March 2017 aged 16 and over
SCLA_AbNeg, Children who started to be looked after during the year ending 31 March 2017 due to abuse or neglect
SCLA_Cdisab, Children who started to be looked after during the year ending 31 March 2017 due to the child?s disability
SCLA_ParIll, Children who started to be looked after during the year ending 31 March 2017 due to parental illness or disability
SCLA_FAcSt, Children who started to be looked after during the year ending 31 March 2017 due to family in acute stress
SCLA_FD, Children who started to be looked after during the year ending 31 March 2017 due to family dysfunction
SCLA_SUB, Children who started to be looked after during the year ending 31 March 2017 due to socially unacceptable behaviour
SCLA_LI, Children who started to be looked after during the year ending 31 March 2017 due to low income
SCLA_AbsPar, Children who started to be looked after during the year ending 31 March 2017 due to absent parenting
SCLA_ICO, Children who started to be looked after during the year ending 31 March 2017 under an interim care order
SCLA_FCO, Children who started to be looked after during the year ending 31 March 2017 under a full care order
SCLA_PlaceO, Children who started to be looked after during the year ending 31 March 2017 where a placement order has been granted
SCLA_S20, Children who started to be looked after during the year ending 31 March 2017 under section 20
SCLA_UPP, Children who started to be looked after during the year ending 31 March 2017 under police protection
SCLA_SEPO, Children who started to be looked after during the year ending 31 March 2017 subject to an emergency protection order
SCLA_UCAO, Children who started to be looked after during the year ending 31 March 2017 under a child assessment order
SCLA_ONCT, Children who started to be looked after during the year ending 31 March 2017 on remand or committed for trial
SCLA_PACE, Children who started to be looked after during the year ending 31 March 2017 under the police and criminal evidence act
SCLA_SORR, Children who started to be looked after during the year ending 31 March 2017 under a supervision order with residence requirement
CLA_cease2017, Children who ceased to be looked after during the year ending 31 March 2017
CEA_male, Male children who ceased to be looked after during the year ending 31 March 2017
CEA_female, Female children who ceased to be looked after during the year ending 31 March 2017
CEA_U1, Children who ceased to be looked after during the year ending 31 March 2017 aged under 1
CEA_1to4, Children who ceased to be looked after during the year ending 31 March 2017 aged 1 to 4
CEA_5to9, Children who ceased to be looked after during the year ending 31 March 2017 aged 5 to 9
CEA_10to15, Children who ceased to be looked after during the year ending 31 March 2017 aged 10 to 15
CEA_16, Children who ceased to be looked after during the year ending 31 March 2017 aged 16
CEA_17, Children who ceased to be looked after during the year ending 31 March 2017 aged 17
CEA_18over, Children who ceased to be looked after during the year ending 31 March 2017 aged 18 and over
CEA_Adop1, Children who ceased to be looked after during the year ending 31 March 2017 due to adoption - application unopposed
CEA_Adop2, Children who ceased to be looked after during the year ending 31 March 2017 due to adoption - consent dispensed with
CEA_Died, Children who ceased to be looked after during the year ending 31 March 2017 as they have died
CEA_Taken, Children who ceased to be looked after during the year ending 31 March 2017 - care taken by another LA
CEA_ParPlan, Children who ceased to be looked after during the year ending 31 March 2017 - returned home to live with parents or relatives as part of care planning process
CEA_ParNPlan, Children who ceased to be looked after during the year ending 31 March 2017 - returned home to live with parents or relatives not as part of care planning process
CEA_NoPar, Children who ceased to be looked after during the year ending 31 March 2017 - ceased to live with parents or relatives without parental responsibility
CEA_CAO, Children who ceased to be looked after during the year ending 31 March 2017 - child arrangement order granted
CEA_SGO1, Children who ceased to be looked after during the year ending 31 March 2017 - special guardianship order made to former foster carers
CEA_SGO2, Children who ceased to be looked after during the year ending 31 March 2017 - special guardianship order made to carers other than former foster carers
CEA_IndLiv1, Children who ceased to be looked after during the year ending 31 March 2017 - moved into independent living (with supportive accommodation)
CEA_IndLiv2, Children who ceased to be looked after during the year ending 31 March 2017 - moved into independent living (with no formalised support)
CEA_Residential, Children who ceased to be looked after during the year ending 31 March 2017 - transferred to residential care funded by adult social services
CEA_Custody, Children who ceased to be looked after during the year ending 31 March 2017 - sentenced to custody
CEA_RemEnd, Children who ceased to be looked after during the year ending 31 March 2017 - accommodation on remand ended
CEA_AgeAssmt, Children who ceased to be looked after during the year ending 31 March 2017 - age assessment determined child was 18 or over
CEA_Abroad, Children who ceased to be looked after during the year ending 31 March 2017 - child moved abroad
CEA_Other, Children who ceased to be looked after during the year ending 31 March 2017 - care ceased for any other reason
CL_All_17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday
CL_All_17, Care leavers in the year ending 31 March 2017 aged 17 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday
CL_All_18, Care leavers in the year ending 31 March 2017 aged 18 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday
CL_Act_HE17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 whose activity is higher education i.e. studies beyond A level
CL_Act_HE17, Care leavers in the year ending 31 March 2017 aged 17 whose activity is higher education i.e. studies beyond A level
CL_Act_HE18, Care leavers in the year ending 31 March 2017 aged 18 whose activity is higher education i.e. studies beyond A level
CL_Act_OE17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 whose activity is education other than higher education
CL_Act_OE17, Care leavers in the year ending 31 March 2017 aged 17 whose activity is education other than higher education
CL_Act_OE18, Care leavers in the year ending 31 March 2017 aged 18 whose activity is education other than higher education
CL_Act_TE17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 whose activity is training or employment
CL_Act_TE17, Care leavers in the year ending 31 March 2017 aged 17 whose activity is training or employment
CL_Act_TE18, Care leavers in the year ending 31 March 2017 aged 18 whose activity is training or employment
CL_Act_NEET_ill17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill17, Care leavers in the year ending 31 March 2017 aged 17 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill18, Care leavers in the year ending 31 March 2017 aged 18 who are not in education training or employment due to illness or disability
CL_Act_NEET_oth17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 who are not in education training or employment due to other reasons
CL_Act_NEET_oth17, Care leavers in the year ending 31 March 2017 aged 17 who are not in education training or employment due to other reasons
CL_Act_NEET_oth18, Care leavers in the year ending 31 March 2017 aged 18 who are not in education training or employment due to other reasons
CL_Act_NEET_preg17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg17, Care leavers in the year ending 31 March 2017 aged 17 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg18, Care leavers in the year ending 31 March 2017 aged 18 who are not in education training or employment due to pregnancy or parenting
CL_Act_NoInf17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 for whom local authority does not have activity information
CL_Act_NoInf17, Care leavers in the year ending 31 March 2017 aged 17 for whom local authority does not have activity information
CL_Act_NoInf18, Care leavers in the year ending 31 March 2017 aged 18 for whom local authority does not have activity information
CL_Acc_SUIT17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 in suitable accommodation
CL_Acc_SUIT17, Care leavers in the year ending 31 March 2017 aged 17 in suitable accommodation
CL_Acc_SUIT18, Care leavers in the year ending 31 March 2017 aged 18 in suitable accommodation
CL_Acc_NotSUIT17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 not in suitable accommodation
CL_Acc_NotSUIT17, Care leavers in the year ending 31 March 2017 aged 17 not in suitable accommodation
CL_Acc_NotSUIT18, Care leavers in the year ending 31 March 2017 aged 18 not in suitable accommodation
CL_Acc_NoSUITInfo17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 for whom suitability of accommodation is not available
CL_Acc_NoSUITInfo17, Care leavers in the year ending 31 March 2017 aged 17 for whom suitability of accommodation is not available
CL_Acc_NoSUITInfo18, Care leavers in the year ending 31 March 2017 aged 18 for whom suitability of accommodation is not available
CL_Acc_P17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 accommodated with parents or relatives
CL_Acc_P17, Care leavers in the year ending 31 March 2017 aged 17 accommodated with parents or relatives
CL_Acc_P18, Care leavers in the year ending 31 March 2017 aged 18 accommodated with parents or relatives
CL_Acc_CH17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 accommodated in community homes
CL_Acc_CH17, Care leavers in the year ending 31 March 2017 aged 17 accommodated in community homes
CL_Acc_CH18, Care leavers in the year ending 31 March 2017 aged 18 accommodated in community homes
CL_Acc_SITA17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 accommodated in semi-independent transitional accommodation
CL_Acc_SITA17, Care leavers in the year ending 31 March 2017 aged 17 accommodated in semi-independent transitional accommodation
CL_Acc_SITA18, Care leavers in the year ending 31 March 2017 aged 18 accommodated in semi-independent transitional accommodation
CL_Acc_SL17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 accommodated in supported lodgings
CL_Acc_SL17, Care leavers in the year ending 31 March 2017 aged 17 accommodated in supported lodgings
CL_Acc_SL18, Care leavers in the year ending 31 March 2017 aged 18 accommodated in supported lodgings
CL_Acc_GA17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 who have gone abroad
CL_Acc_GA17, Care leavers in the year ending 31 March 2017 aged 17 who have gone abroad
CL_Acc_GA18, Care leavers in the year ending 31 March 2017 aged 18 who have gone abroad
CL_Acc_Dep17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 who have been deported
CL_Acc_Dep17, Care leavers in the year ending 31 March 2017 aged 17 who have been deported
CL_Acc_Dep18, Care leavers in the year ending 31 March 2017 aged 18 who have been deported
CL_Acc_OL17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 accommodated in ordinary lodgings
CL_Acc_OL17, Care leavers in the year ending 31 March 2017 aged 17 accommodated in ordinary lodgings
CL_Acc_OL18, Care leavers in the year ending 31 March 2017 aged 18 accommodated in ordinary lodgings
CL_Acc_NK17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 with residence not known
CL_Acc_NK17, Care leavers in the year ending 31 March 2017 aged 17 with residence not known
CL_Acc_NK18, Care leavers in the year ending 31 March 2017 aged 18 with residence not known
CL_Acc_NFA17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 with no fixed abode or homeless
CL_Acc_NFA17, Care leavers in the year ending 31 March 2017 aged 17 with no fixed abode or homeless
CL_Acc_NFA18, Care leavers in the year ending 31 March 2017 aged 18 with no fixed abode or homeless
CL_Acc_F17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 accommodated in foyers
CL_Acc_F17, Care leavers in the year ending 31 March 2017 aged 17 accommodated in foyers
CL_Acc_F18, Care leavers in the year ending 31 March 2017 aged 18 accommodated in foyers
CL_Acc_IL17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 accommodated in independent living
CL_Acc_IL17, Care leavers in the year ending 31 March 2017 aged 17 accommodated in independent living
CL_Acc_IL18, Care leavers in the year ending 31 March 2017 aged 18 accommodated in independent living
CL_Acc_EA17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 accommodated in emergency accommodation
CL_Acc_EA17, Care leavers in the year ending 31 March 2017 aged 17 accommodated in emergency accommodation
CL_Acc_EA18, Care leavers in the year ending 31 March 2017 aged 18 accommodated in emergency accommodation
CL_Acc_BB17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 in bed and breakfast accommodation
CL_Acc_BB17, Care leavers in the year ending 31 March 2017 aged 17 in bed and breakfast accommodation
CL_Acc_BB18, Care leavers in the year ending 31 March 2017 aged 18 in bed and breakfast accommodation
CL_Acc_Cust17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 who are in custody
CL_Acc_Cust17, Care leavers in the year ending 31 March 2017 aged 17 who are in custody
CL_Acc_Cust18, Care leavers in the year ending 31 March 2017 aged 18 who are in custody
CL_Acc_FFC17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 accommodated with former foster carers
CL_Acc_FFC17, Care leavers in the year ending 31 March 2017 aged 17 accommodated with former foster carers
CL_Acc_FFC18, Care leavers in the year ending 31 March 2017 aged 18 accommodated with former foster carers
CL_Acc_OTH17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 in other accommodation
CL_Acc_OTH17, Care leavers in the year ending 31 March 2017 aged 17 in other accommodation
CL_Acc_OTH18, Care leavers in the year ending 31 March 2017 aged 18 in other accommodation
CL_Acc_NoInf17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 for whom local authority does not have accommodation information
CL_Acc_NoInf17, Care leavers in the year ending 31 March 2017 aged 17 for whom local authority does not have accommodation information
CL_Acc_NoInf18, Care leavers in the year ending 31 March 2017 aged 18 for whom local authority does not have accommodation information
CL_InTouch_IT17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 who are in touch with the local authority
CL_InTouch_IT17, Care leavers in the year ending 31 March 2017 aged 17 who are in touch with the local authority
CL_InTouch_IT18, Care leavers in the year ending 31 March 2017 aged 18 who are in touch with the local authority
CL_InTouch_Refu17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 who refuse contact with the local authority
CL_InTouch_Refu17, Care leavers in the year ending 31 March 2017 aged 17 who refuse contact with the local authority
CL_InTouch_Refu18, Care leavers in the year ending 31 March 2017 aged 18 who refuse contact with the local authority
CL_InTouch_NoServ17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 who no longer require services
CL_InTouch_NoServ17, Care leavers in the year ending 31 March 2017 aged 17 who no longer require services
CL_InTouch_NoServ18, Care leavers in the year ending 31 March 2017 aged 18 who no longer require services
CL_InTouch_Not17&18, Care leavers in the year ending 31 March 2017 aged 17 and 18 who are not in touch with the local authority
CL_InTouch_Not17, Care leavers in the year ending 31 March 2017 aged 17 who are not in touch with the local authority
CL_InTouch_Not18, Care leavers in the year ending 31 March 2017 aged 18 who are not in touch with the local authority
CL_StayPut_18, Care leavers in the year ending 31 March 2017 aged 18 who ceased to be looked after from a foster placement on their 18th birthday
CL_StayPut_FFC_18, Care leavers in the year ending 31 March 2017 aged 18 who ceased to be looked after from a foster placement on their 18th birthday and are living with the former foster carers
CL_All_19to21, Care leavers in the year ending 31 March 2017aged 19 to 21 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday
CL_All_19, Care leavers in the year ending 31 March 2017aged 19 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday
CL_All_20, Care leavers in the year ending 31 March 2017aged 20 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday
CL_All_21, Care leavers in the year ending 31 March 2017aged 21 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday
CL_Act_HE19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 whose activity is higher education i.e. studies beyond A level
CL_Act_HE19, Care leavers in the year ending 31 March 2017 aged 19 whose activity is higher education i.e. studies beyond A level
CL_Act_HE20, Care leavers in the year ending 31 March 2017 aged 20 whose activity is higher education i.e. studies beyond A level
CL_Act_HE21, Care leavers in the year ending 31 March 2017 aged 21 whose activity is higher education i.e. studies beyond A level
CL_Act_OE19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 whose activity is education other than higher education
CL_Act_OE19, Care leavers in the year ending 31 March 2017 aged 19 whose activity is education other than higher education
CL_Act_OE20, Care leavers in the year ending 31 March 2017 aged 20 whose activity is education other than higher education
CL_Act_OE21, Care leavers in the year ending 31 March 2017 aged 21 whose activity is education other than higher education
CL_Act_TE19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 whose activity is training or employment
CL_Act_TE19, Care leavers in the year ending 31 March 2017 aged 19 whose activity is training or employment
CL_Act_TE20, Care leavers in the year ending 31 March 2017 aged 20 whose activity is training or employment
CL_Act_TE21, Care leavers in the year ending 31 March 2017 aged 21 whose activity is training or employment
CL_Act_NEET_ill19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill19, Care leavers in the year ending 31 March 2017 aged 19 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill20, Care leavers in the year ending 31 March 2017 aged 20 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill21, Care leavers in the year ending 31 March 2017 aged 21 who are not in education training or employment due to illness or disability
CL_Act_NEET_oth19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 who are not in education training or employment due to other reasons
CL_Act_NEET_oth19, Care leavers in the year ending 31 March 2017 aged 19 who are not in education training or employment due to other reasons
CL_Act_NEET_oth20, Care leavers in the year ending 31 March 2017 aged 20 who are not in education training or employment due to other reasons
CL_Act_NEET_oth21, Care leavers in the year ending 31 March 2017 aged 21 who are not in education training or employment due to other reasons
CL_Act_NEET_preg19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg19, Care leavers in the year ending 31 March 2017 aged 19 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg20, Care leavers in the year ending 31 March 2017 aged 20 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg21, Care leavers in the year ending 31 March 2017 aged 21 who are not in education training or employment due to pregnancy or parenting
CL_Act_NoInf19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 for whom local authority does not have activity information
CL_Act_NoInf19, Care leavers in the year ending 31 March 2017 aged 19 for whom local authority does not have activity information
CL_Act_NoInf20, Care leavers in the year ending 31 March 2017 aged 20 for whom local authority does not have activity information
CL_Act_NoInf21, Care leavers in the year ending 31 March 2017 aged 21 for whom local authority does not have activity information
CL_Acc_SUIT19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 in suitable accommodation
CL_Acc_SUIT19, Care leavers in the year ending 31 March 2017 aged 19 in suitable accommodation
CL_Acc_SUIT20, Care leavers in the year ending 31 March 2017 aged 20 in suitable accommodation
CL_Acc_SUIT21, Care leavers in the year ending 31 March 2017 aged 21 in suitable accommodation
CL_Acc_NotSUIT19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 not in suitable accommodation
CL_Acc_NotSUIT19, Care leavers in the year ending 31 March 2017 aged 19 not in suitable accommodation
CL_Acc_NotSUIT20, Care leavers in the year ending 31 March 2017 aged 20 not in suitable accommodation
CL_Acc_NotSUIT21, Care leavers in the year ending 31 March 2017 aged 21 not in suitable accommodation
CL_Acc_NoSUITInfo19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 for whom suitability of accommodation is not available
CL_Acc_NoSUITInfo19, Care leavers in the year ending 31 March 2017 aged 19 for whom suitability of accommodation is not available
CL_Acc_NoSUITInfo20, Care leavers in the year ending 31 March 2017 aged 20 for whom suitability of accommodation is not available
CL_Acc_NoSUITInfo21, Care leavers in the year ending 31 March 2017 aged 21 for whom suitability of accommodation is not available
CL_Acc_P19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 years old accommodated with parents or relatives
CL_Acc_P19, Care leavers in the year ending 31 March 2017 aged 19 accommodated with parents or relatives
CL_Acc_P20, Care leavers in the year ending 31 March 2017 aged 20 accommodated with parents or relatives
CL_Acc_P21, Care leavers in the year ending 31 March 2017 aged 21 accommodated with parents or relatives
CL_Acc_CH19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 accommodated in community homes
CL_Acc_CH19, Care leavers in the year ending 31 March 2017 aged 19 accommodated in community homes
CL_Acc_CH20, Care leavers in the year ending 31 March 2017 aged 20 accommodated in community homes
CL_Acc_CH21, Care leavers in the year ending 31 March 2017 aged 21 accommodated in community homes
CL_Acc_SITA19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 accommodated in semi-independent transitional accommodation
CL_Acc_SITA19, Care leavers in the year ending 31 March 2017 aged 19 accommodated in semi-independent transitional accommodation
CL_Acc_SITA20, Care leavers in the year ending 31 March 2017 aged 20 accommodated in semi-independent transitional accommodation
CL_Acc_SITA21, Care leavers in the year ending 31 March 2017 aged 21 accommodated in semi-independent transitional accommodation
CL_Acc_SL19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 accommodated in supported lodgings
CL_Acc_SL19, Care leavers in the year ending 31 March 2017 aged 19 accommodated in supported lodgings
CL_Acc_SL20, Care leavers in the year ending 31 March 2017 aged 20 accommodated in supported lodgings
CL_Acc_SL21, Care leavers in the year ending 31 March 2017 aged 21 accommodated in supported lodgings
CL_Acc_GA19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 who have gone abroad
CL_Acc_GA19, Care leavers in the year ending 31 March 2017 aged 19 who have gone abroad
CL_Acc_GA20, Care leavers in the year ending 31 March 2017 aged 20 who have gone abroad
CL_Acc_GA21, Care leavers in the year ending 31 March 2017 aged 21 who have gone abroad
CL_Acc_Dep19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 who have been deported
CL_Acc_Dep19, Care leavers in the year ending 31 March 2017 aged 19 who have been deported
CL_Acc_Dep20, Care leavers in the year ending 31 March 2017 aged 20 who have been deported
CL_Acc_Dep21, Care leavers in the year ending 31 March 2017 aged 21 who have been deported
CL_Acc_OL19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 accommodated in ordinary lodgings
CL_Acc_OL19, Care leavers in the year ending 31 March 2017 aged 19 accommodated in ordinary lodgings
CL_Acc_OL20, Care leavers in the year ending 31 March 2017 aged 20 accommodated in ordinary lodgings
CL_Acc_OL21, Care leavers in the year ending 31 March 2017 aged 21 accommodated in ordinary lodgings
CL_Acc_NK19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 with residence not known
CL_Acc_NK19, Care leavers in the year ending 31 March 2017 aged 19 with residence not known
CL_Acc_NK20, Care leavers in the year ending 31 March 2017 aged 20 with residence not known
CL_Acc_NK21, Care leavers in the year ending 31 March 2017 aged 21 with residence not known
CL_Acc_NFA19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 with no fixed abode or homeless
CL_Acc_NFA19, Care leavers in the year ending 31 March 2017 aged 19 with no fixed abode or homeless
CL_Acc_NFA20, Care leavers in the year ending 31 March 2017 aged 20 with no fixed abode or homeless
CL_Acc_NFA21, Care leavers in the year ending 31 March 2017 aged 21 with no fixed abode or homeless
CL_Acc_F19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 accommodated in foyers
CL_Acc_F19, Care leavers in the year ending 31 March 2017 aged 19 accommodated in foyers
CL_Acc_F20, Care leavers in the year ending 31 March 2017 aged 20 accommodated in foyers
CL_Acc_F21, Care leavers in the year ending 31 March 2017 aged 21 accommodated in foyers
CL_Acc_IL19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 accommodated in independent living
CL_Acc_IL19, Care leavers in the year ending 31 March 2017 aged 19 accommodated in independent living
CL_Acc_IL20, Care leavers in the year ending 31 March 2017 aged 20 accommodated in independent living
CL_Acc_IL21, Care leavers in the year ending 31 March 2017 aged 21 accommodated in independent living
CL_Acc_EA19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 accommodated in emergency accommodation
CL_Acc_EA19, Care leavers in the year ending 31 March 2017 aged 19 accommodated in emergency accommodation
CL_Acc_EA20, Care leavers in the year ending 31 March 2017 aged 20 accommodated in emergency accommodation
CL_Acc_EA21, Care leavers in the year ending 31 March 2017 aged 21 accommodated in emergency accommodation
CL_Acc_BB19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 in bed and breakfast accommodation
CL_Acc_BB19, Care leavers in the year ending 31 March 2017 aged 19 in bed and breakfast accommodation
CL_Acc_BB20, Care leavers in the year ending 31 March 2017 aged 20 in bed and breakfast accommodation
CL_Acc_BB21, Care leavers in the year ending 31 March 2017 aged 21 in bed and breakfast accommodation
CL_Acc_Cust19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 who are in custody
CL_Acc_Cust19, Care leavers in the year ending 31 March 2017 aged 19 who are in custody
CL_Acc_Cust20, Care leavers in the year ending 31 March 2017 aged 20 who are in custody
CL_Acc_Cust21, Care leavers in the year ending 31 March 2017 aged 21 who are in custody
CL_Acc_FFC19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 accommodated with former foster carers
CL_Acc_FFC19, Care leavers in the year ending 31 March 2017 aged 19 accommodated with former foster carers
CL_Acc_FFC20, Care leavers in the year ending 31 March 2017 aged 20 accommodated with former foster carers
CL_Acc_FFC21, Care leavers in the year ending 31 March 2017 aged 21 accommodated with former foster carers
CL_Acc_OTH19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 in other accommodation
CL_Acc_OTH19, Care leavers in the year ending 31 March 2017 aged 19 in other accommodation
CL_Acc_OTH20, Care leavers in the year ending 31 March 2017 aged 20 in other accommodation
CL_Acc_OTH21, Care leavers in the year ending 31 March 2017 aged 21 in other accommodation
CL_Acc_NoInf19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 for whom local authority does not have accommodation information
CL_Acc_NoInf19, Care leavers in the year ending 31 March 2017 aged 19 for whom local authority does not have accommodation information
CL_Acc_NoInf20, Care leavers in the year ending 31 March 2017 aged 20 for whom local authority does not have accommodation information
CL_Acc_NoInf21, Care leavers in the year ending 31 March 2017 aged 21 for whom local authority does not have accommodation information
CL_InTouch_IT19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 who are in touch with the local authority
CL_InTouch_IT19, Care leavers in the year ending 31 March 2017 aged 19 who are in touch with the local authority
CL_InTouch_IT20, Care leavers in the year ending 31 March 2017 aged 20 who are in touch with the local authority
CL_InTouch_IT21, Care leavers in the year ending 31 March 2017 aged 21 who are in touch with the local authority
CL_InTouch_Refu19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 who refuse contact with the local authority
CL_InTouch_Refu19, Care leavers in the year ending 31 March 2017 aged 19 who refuse contact with the local authority
CL_InTouch_Refu20, Care leavers in the year ending 31 March 2017 aged 20 who refuse contact with the local authority
CL_InTouch_Refu21, Care leavers in the year ending 31 March 2017 aged 21 who refuse contact with the local authority
CL_InTouch_NoServ19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 who no longer require services
CL_InTouch_NoServ19, Care leavers in the year ending 31 March 2017 aged 19 who no longer require services
CL_InTouch_NoServ20, Care leavers in the year ending 31 March 2017 aged 20 who no longer require services
CL_InTouch_NoServ21, Care leavers in the year ending 31 March 2017 aged 21 who no longer require services
CL_InTouch_Not19to21, Care leavers in the year ending 31 March 2017 aged 19 to 21 who are not in touch with the local authority
CL_InTouch_Not19, Care leavers in the year ending 31 March 2017 aged 19 who are not in touch with the local authority
CL_InTouch_Not20, Care leavers in the year ending 31 March 2017 aged 20 who are not in touch with the local authority
CL_InTouch_Not21, Care leavers in the year ending 31 March 2017 aged 21 who are not in touch with the local authority
CL_StayPut_19&20, Care leavers in the year ending 31 March 2017 aged 19 and 20 who ceased to be looked after from a foster placement on their 18th birthday
CL_StayPut_19, Care leavers in the year ending 31 March 2017 aged 19 who ceased to be looked after from a foster placement on their 18th birthday
CL_StayPut_20, Care leavers in the year ending 31 March 2017 aged 20 who ceased to be looked after from a foster placement on their 18th birthday
CL_StayPut_FFC_19&20, Care leavers in the year ending 31 March 2017 aged 19 and 20 who ceased to be looked after from a foster placement on their 18th birthday and are living with the former foster carers
CL_StayPut_FFC_19, Care leavers in the year ending 31 March 2017 aged 19 who ceased to be looked after from a foster placement on their 18th birthday and are living with the former foster carers
CL_StayPut_FFC_20, Care leavers in the year ending 31 March 2017 aged 20 who ceased to be looked after from a foster placement on their 18th birthday and are living with the former foster carers
Miss_Miss_DuringYear, Children looked after during the year ending 31 March 2017 who had a missing incident during the year
Miss_Miss_Incs, Number of missing incidents during the year for children looked after during the year ending 31 March 2017
Miss_Miss_MoreOnce, Children looked after during the year ending 31 March 2017 who went missing more than once during the year
Miss_Miss_31Mar, Children looked after who were missing at 31 March 2017
Miss_Away_DuringYear, Children looked after during the year ending 31 March 2017 who were away from placement without authorisation during the year
Miss_Away_Incs, Number of away from placements without authorisation incidents during the year for children looked after during the year ending 31 March 2017
Miss_Away_MoreOnce, Children looked after during the year ending 31 March 2017 who were away from placement without authorisation more than once during the year
Miss_Away_31Mar, Children looked after who were away from placement without authorisation at 31 March 2017
OC2_12mths, Children looked after at 31 March 2017 for at least 12 months
OC2_10to17, Children looked after at 31 March 2017 for at least 12 months aged 10 to 17 years
OC2_convicted, Children looked after at 31 March 2017 convicted or subject to a final warning or reprimand during the year aged 10 to 17 years
OC2_submisuse, Children looked after at 31 March 2017 identified as having a substance misuse problem
OC2_subint, Children looked after at 31 March 2017 received intervention for substance misuse
OC2_suboffint, Children looked after at 31 March 2017 offered intervention for substance misuse and refused
OC2_immunisation, Children looked after at 31 March 2017 whose immunisations were up to date
OC2_teethcheck, Children looked after at 31 March 2017 who had their teeth checked
OC2_healthassmt, Children looked after at 31 March 2017 who had their annual health assessment
OC2_U5, Children looked after at 31 March 2017 for at least 12 months aged under 5 years
OC2_devassmt, Children looked after at 31 March 2017 whose development assessments were up to date
OC2_5to16, Children looked after at 31 March 2017 for at least 12 months aged 5 to 16
OC2_SDQ, Children looked after at 31 March 2017 with an SDQ score aged 5 to 16
OC2_SDQnormal, Children looked after at 31 March 2017 aged 5 to 16 whose SDQ score was normal
OC2_SDQborderline, Children looked after at 31 March 2017 aged 5 to 16 whose SDQ score was borderline
OC2_SDQconcern, Children looked after at 31 March 2017 aged 5 to 16 whose SDQ score was a cause for concern
OC2_SDQaverage, Children looked after at 31 March 2017 aged 5 to 16 with an SDQ score - average SDQ score

"


varlist_2017 <- read_csv(varlist_2017, col_names = c("varname", "description"))


varlist_2018 <- "

New_geog_code, New geography code
geog_l, Geography level
geog_c, Geography code
geog_n, Geography name
CLA_Mar2014, Children looked after at 31 March 2014
CLA_Mar2015, Children looked after at 31 March 2015
CLA_Mar2016, Children looked after at 31 March 2016
CLA_Mar2017, Children looked after at 31 March 2017
CLA_Mar2018, Children looked after at 31 March 2018
CLA_Pop2014, Mid-year population estimate of children in 2014
CLA_Pop2015, Mid-year population estimate of children in 2015
CLA_Pop2016, Mid-year population estimate of children in 2016
CLA_Pop2017, Mid-year population estimate of children in 2017
CLA_Pop2018, Mid-year population estimate of children in 2018
CLA_Rate2014, Rate of looked after children per 10000 at 31 March 2014 [1]
CLA_Rate2015, Rate of looked after children per 10000 at 31 March 2015 [1]
CLA_Rate2016, Rate of looked after children per 10000 at 31 March 2016 [1]
CLA_Rate2017, Rate of looked after children per 10000 at 31 March 2017 [1]
CLA_Rate2018, Rate of looked after children per 10000 at 31 March 2018 [1]
CLA_2014, Children looked after during the year ending 31 March 2014
CLA_2015, Children looked after during the year ending 31 March 2015
CLA_2016, Children looked after during the year ending 31 March 2016
CLA_2017, Children looked after during the year ending 31 March 2017
CLA_2018, Children looked after during the year ending 31 March 2018
CLA_stp2014, Children looked after during the year ending 31 March 2014 who were only looked after under a series of short term placements [2]
CLA_stp2015, Children looked after during the year ending 31 March 2015 who were only looked after under a series of short term placements [2]
CLA_stp2016, Children looked after during the year ending 31 March 2016 who were only looked after under a series of short term placements [2]
CLA_stp2017, Children looked after during the year ending 31 March 2017 who were only looked after under a series of short term placements [2]
CLA_stp2018, Children looked after during the year ending 31 March 2018 who were only looked after under a series of short term placements [2]
CLA_male, Male children looked after at 31 March 2018
CLA_female, Female children looked after at 31 March 2018
CLA_male_pc, Percentage of children looked after at 31 March 2018 who were male
CLA_female_pc, Percentage of children looked after at 31 March 2018 who were female
CLA_U1, Children looked after at 31 March 2018 aged under 1
CLA_1to4, Children looked after at 31 March 2018 aged 1 to 4
CLA_5to9, Children looked after at 31 March 2018 aged 5 to 9
CLA_10to15, Children looked after at 31 March 2018 aged 10 to 15
CLA_16over, Children looked after at 31 March 2018 aged 16 and over [3]
CLA_U1_pc, Percentage of children looked after at 31 March 2018 aged under 1
CLA_1to4_pc, Percentage of children looked after at 31 March 2018 aged 1 to 4
CLA_5to9_pc, Percentage of children looked after at 31 March 2018 aged 5 to 9
CLA_10to15_pc, Percentage of children looked after at 31 March 2018 aged 10 to 15
CLA_16over_pc, Percentage of children looked after at 31 March 2018 aged 16 and over [3]
CLA_White, Children looked after at 31 March 2018 from a white ethnic background
CLA_Mixed, Children looked after at 31 March 2018 from a mixed ethnic background
CLA_Asian, Children looked after at 31 March 2018 from an Asian ethnic background [3]
CLA_Black, Children looked after at 31 March 2018 from a black ethnic background
CLA_EOTH, Children looked after at 31 March 2018 from any other ethnic background [3]
CLA_Oth, Children looked after at 31 March 2018 where ethnic background is not known [4]
CLA_White_pc, Percentage of children looked after at 31 March 2018 from a white ethnic background
CLA_Mixed_pc, Percentage of children looked after at 31 March 2018 from a mixed ethnic background
CLA_Asian_pc, Percentage of children looked after at 31 March 2018 from an Asian ethnic background [3]
CLA_Black_pc, Percentage of children looked after at 31 March 2018 from a black ethnic background
CLA_EOTH_pc, Percentage of children looked after at 31 March 2018 from any other ethnic background [3]
CLA_Oth_pc, Percentage of children looked after at 31 March 2018 where ethnic background is not known [4]
CLA_Fost, Children looked after at 31 March 2018 in foster placements
CLA_Adopt, Children looked after at 31 March 2018 placed for adoption
CLA_Parent, Children looked after at 31 March 2018 placed with parents
CLA_Ocom, Children looked after at 31 March 2018 placed in other placements within the community
CLA_Secure, Children looked after at 31 March 2018 placed in secure units children?s homes and semi-independent living accommodation [5]
CLA_Ores, Children looked after at 31 March 2018 placed in other residential settings
CLA_RSch, Children looked after at 31 March 2018 placed in residential schools
CLA_OthPl, Children looked after at 31 March 2018 in other placements
CLA_Fost_pc, Percentage of children looked after at 31 March 2018 in foster placements
CLA_Adopt_pc, Percentage of children looked after at 31 March 2018 placed for adoption
CLA_Parent_pc, Percentage of children looked after at 31 March 2018 placed with parents
CLA_Ocom_pc, Percentage of children looked after at 31 March 2018 placed in other placements within the community
CLA_Secure_pc, Percentage of children looked after at 31 March 2018 placed in secure units children?s homes and semi-independent living accommodation [5]
CLA_Ores_pc, Percentage of children looked after at 31 March 2018 placed in other residential settings
CLA_RSch_pc, Percentage of children looked after at 31 March 2018 placed in residential schools
CLA_Oth_Pl_pc, Percentage of children looked after at 31 March 2018 placed in other placements
CLA_ICO, Children looked after at 31 March 2018 under an interim care order
CLA_FCO, Children looked after at 31 March 2018 under a full care order
CLA_FrAd, Children looked after at 31 March 2018 who are freed for adoption [6]
CLA_PlaceO, Children looked after at 31 March 2018 who have a placement order [7]
CLA_S20, Children looked after at 31 March 2018 accommodated under section 20 [3]
CLA_CPG, Children looked after at 31 March 2018 detained under child protection grounds [8]
CLA_YJLS, Children looked after at 31 March 2018 under a youth justice legal status [9]
CLA_ICO_pc, Percentage of children looked after at 31 March 2018 under an interim care order
CLA_FCO_pc, Percentage of children looked after at 31 March 2018 under a full care order
CLA_FrAd_pc, Percentage of children looked after at 31 March 2018 who are freed for adoption [6]
CLA_PlaceO_pc, Percentage of children looked after at 31 March 2018 who have a placement order [7]
CLA_S20_pc, Percentage of children looked after at 31 March 2018 accommodated under section 20 [3]
CLA_CPG_pc, Percentage of children looked after at 31 March 2018 detained under child protection grounds [8]
CLA_YJLS_pc, Percentage of children looked after at 31 March 2018 under a youth justice legal status [9]
CLA_UASC2014, Children looked after at 31 March 2014 who were unaccompanied asylum seekers during the year [3]
CLA_UASC2015, Children looked after at 31 March 2015 who were unaccompanied asylum seekers during the year [3]
CLA_UASC2016, Children looked after at 31 March 2016 who were unaccompanied asylum seekers during the year [3]
CLA_UASC2017, Children looked after at 31 March 2017 who were unaccompanied asylum seekers during the year [3]
CLA_UASC2018, Children looked after at 31 March 2018 who were unaccompanied asylum seekers during the year [3]
CLA_OwnP, Children looked after at 31 March 2018 where placement is provided by the local authority?s own provision
CLA_OthLA, Children looked after at 31 March 2018 under where placement is provided by another local authority
CLA_OthPP, Children looked after at 31 March 2018 where placement is provided by other public provision
CLA_Priv, Children looked after at 31 March 2018 where placement is provided by private provision
CLA_Vol, Children looked after at 31 March 2018 where placement is provided by voluntary provision
CLA_Par, Children looked after at 31 March 2018 where placement is provided by parents [10]
CLA_Nrep, Children looked after at 31 March 2018 where placement provider is not reported [11]
CLA_LTE20, Children looked after at 31 March 2018 placed within 20 miles of the child?s home
CLA_GT20, Children looked after at 31 March 2018 placed over 20 miles from the child?s home
CLA_NoInfo, Children looked after at 31 March 2018 where the distance from the child?s home is not known or not recorded [12]
CLA_InLA, Children looked after at 31 March 2018 placed within the local authority boundary [13]
CLA_OutLA, Children looked after at 31 March 2018 placed outside of the local authority boundary [13]
CLA_InLA_LTE20, Children looked after at 31 March 2018 placed within the local authority boundary and within 20 miles of the child?s home [13]
CLA_OutLA_LTE20, Children looked after at 31 March 2018 placed outside of the local authority boundary and within 20 miles of the child?s home [13]
CLA_InLA_GT20, Children looked after at 31 March 2018 placed within the local authority boundary and over 20 miles from the child?s home [13]
CLA_OutLA_GT20, Children looked after at 31 March 2018 placed outside of the local authority boundary and over 20 miles from the child?s home [13]
CLA_InLA_NoInfo, Children looked after at 31 March 2018 placed within the local authority boundary where the distance from the child?s home is not known or not recorded [12 13]
CLA_OutLA_NoInfo, Children looked after at 31 March 2018 placed outside of the local authority boundary where the distance from the child?s home is not known or not recorded [12 13]
CLA_LAPl, Children looked after at 31 March 2018 who were the responsibility of all local authorities placed within the local authority boundary
CLA_IntPl, Children looked after at 31 March 2018 who were the responsibility of the internal local authority placed internally within the local authority boundary [14]
CLA_ExtPl, Children looked after at 31 March 2018 who were the responsibility of an external local authority placed within the local authority boundary [14]
CLA_NetGain, Children looked after at 31 March 2018 ? net gain of children by a local authority [14]
CLA_LTE20_pc, Percentage of children looked after at 31 March 2018 placed within 20 miles of the child?s home
CLA_GT20_pc, Percentage of children looked after at 31 March 2018 placed over 20 miles from the child?s home
CLA_NoInfo_pc, Percentage of children looked after at 31 March 2018 where the distance from the child?s home is not known or not recorded [12]
CLA_InLA_pc, Percentage of children looked after at 31 March 2018 placed within the local authority boundary [13]
CLA_OutLA_pc, Percentage of children looked after at 31 March 2018 placed outside of the local authority boundary [13]
CLA_InLA_LTE20_pc, Percentage of children looked after at 31 March 2018 placed within the local authority boundary and within 20 miles of the child?s home [13]
CLA_OutLA_LTE20_pc, Percentage of children looked after at 31 March 2018 placed outside of the local authority boundary and within 20 miles of the child?s home [13]
CLA_InLA_GT20_pc, Percentage of children looked after at 31 March 2018 placed within the local authority boundary and over 20 miles from the child?s home [13]
CLA_OutLA_GT20_pc, Percentage of children looked after at 31 March 2018 placed outside of the local authority boundary and over 20 miles from the child?s home [13]
CLA_InLA_NoInfo_pc, Percentage of children looked after at 31 March 2018 placed within the local authority boundary where the distance from the child?s home is not known or not recorded [12 13]
CLA_OutLA_NoInfo_pc, Percentage of children looked after at 31 March 2018 placed outside of the local authority boundary where the distance from the child?s home is not known or not recorded [12 13]
CLA_IntPl_pc, Percentage of children placed within the local authority boundary at 31 March 2018 who were the responsibility of the internal local authority [14]
CLA_ExtPl_pc, Percentage of children placed within the local authority boundary at 31 March 2018 who were the responsibility of an external local authority [14]
CLA_3_Pl, Children looked after at 31 March 2018 with three or more placements during the year
CLA_3_Pl_pc, Percentage of children looked after at 31 March 2018 with three or more placements during the year
CLA_2andhalfyears, Children looked after continuously for at least 2.5 years at 31 March 2018 aged under 16
CLA_SamePl_2years, Children looked after at 31 March 2018 aged under 16 who had been looked after continuously for at least 2.5 years who were living in the same placement for at least 2 years or are placed for adoption and their adoption and their adoptive placement together with their previous placement last for at least 2 years
CLA_SamePl_2years_pc, Percentage of children looked after continuously for at least 2.5 years at 31 March aged under 16 who were living in the same placement for at least 2 years or are placed for adoption and their adoption and their adoptive placement together with their previous placement last for at least 2 years
CLA_NewPl_All_Place, Placements that started during the year ending 31 March 2018
CLA_NewPl_InLA_LTE20, Placements that started during the year ending 31 March 2018 within the local authority boundary and within 20 miles of the child?s home [13]
CLA_NewPl_OutLA_LTE20, Placements that started during the year ending 31 March 2018 placed outside of the local authority boundary and within 20 miles of the child?s home [13]
CLA_NewPl_InLA_GT20, Placements that started during the year ending 31 March 2018 placed within the local authority boundary and over 20 miles from the child?s home [13]
CLA_NewPl_OutLA_GT20, Placements that started during the year ending 31 March 2018 placed outside of the local authority boundary and over 20 miles from the child?s home [13]
CLA_NewPl_NoInfo, Placements that started during the year ending 31 March 2018 where the distance from the child?s home is not known or not recorded [12]
CLA_NewPl_InLA_LTE20_pc, Percentage of placements that started during the year ending 31 March 2018 placed within the local authority boundary and within 20 miles of the child?s home [13]
CLA_NewPl_OutLA_LTE20_pc, Percentage of placements that started during the year ending 31 March 2018 placed outside of the local authority boundary and within 20 miles of the child?s home [13]
CLA_NewPl_InLA_GT20_pc, Percentage of placements that started during the year ending 31 March 2018 placed within the local authority boundary and over 20 miles from the child?s home [13]
CLA_NewPl_OutLA_GT20_pc, Percentage of placements that started during the year ending 31 March 2018 placed outside of the local authority boundary and over 20 miles from the child?s home [13]
CLA_NewPl_NoInfo_pc, Percentage of placements that started during the year ending 31 March 2018 placed within the local authority boundary where the distance from the child?s home is not known or not recorded [12]
CLA_RPC_All_Place, Placements that ended during the year ending 31 March 2018
CLA_RPC_CarePlan, Placements that ended during the year ending 31 March 2018 due to a change to/implementation of care plan
CLA_RPC_Resign, Placements that ended during the year ending 31 March 2018 due to resignation or closure of provision
CLA_RPC_Alleg, Placements that ended during the year ending 31 March 2018 due to allegation
CLA_RPC_Standards, Placements that ended during the year ending 31 March 2018 due to standards of care concern
CLA_RPC_Approval, Placements that ended during the year ending 31 March 2018 due to removal of approval
CLA_RPC_CarerBehav, Placements that ended during the year ending 31 March 2018 due to the carer requesting the placement end because of the child?s behaviour
CLA_RPC_CarerOther, Placements that ended during the year ending 31 March 2018 due to the carer requesting the placement end for other reasons
CLA_RPC_Child, Placements that ended during the year ending 31 March 2018 due to the child requesting the placement end
CLA_RPC_Auth, Placements that ended during the year ending 31 March 2018 due to the responsible area/authority requesting the placement end
CLA_RPC_Status, Placements that ended during the year ending 31 March 2018 due to the change in the status of a placement only
CLA_RPC_Other, Placements that ended during the year ending 31 March 2018 due to other reasons
CLA_RPC_CarePlan_pc, Percentage of placements that ended during the year ending 31 March 2018 due to a change to/implementation of care plan
CLA_RPC_Resign_pc, Percentage of placements that ended during the year ending 31 March 2018 due to resignation or closure of provision
CLA_RPC_Alleg_pc, Percentage of placements that ended during the year ending 31 March 2018 due to allegation
CLA_RPC_Standards_pc, Percentage of placements that ended during the year ending 31 March 2018 due to standards of care concern
CLA_RPC_Approval_pc, Percentage of placements that ended during the year ending 31 March 2018 due to removal of approval
CLA_RPC_CarerBehav_pc, Percentage of placements that ended during the year ending 31 March 2018 due to the carer requesting the placement end because of the child?s behaviour
CLA_RPC_CarerOther_pc, Percentage of placements that ended during the year ending 31 March 2018 due to the carer requesting the placement end for other reasons
CLA_RPC_Child_pc, Percentage of placements that ended during the year ending 31 March 2018 due to the child requesting the placement end
CLA_RPC_Auth_pc, Percentage of placements that ended during the year ending 31 March 2018 due to the responsible area/authority requesting the placement end
CLA_RPC_Status_pc, Percentage of placements that ended during the year ending 31 March 2018 due to the change in the status of a placement only
CLA_RPC_Other_pc, Percentage of placements that ended during the year ending 31 March 2018 due to other reasons
CLA_started2014, Children who started to be looked after during the year ending 31 March 2014 [1]
CLA_started2015, Children who started to be looked after during the year ending 31 March 2015 [1]
CLA_started2016, Children who started to be looked after during the year ending 31 March 2016 [1]
CLA_started2017, Children who started to be looked after during the year ending 31 March 2017 [1]
CLA_started2018, Children who started to be looked after during the year ending 31 March 2018 [1]
CLA_taken2018, Children who were taken into care during the year ending 31 March 2018 [2]
CLA_taken2018_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 who were taken into care [2]
SCLA_male, Male children who started to be looked after during the year ending 31 March 2018
SCLA_female, Female children who started to be looked after during the year ending 31 March 2018
SCLA_male_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 who were male
SCLA_female_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 who were female
SCLA_U1, Children who started to be looked after during the year ending 31 March 2018 aged under 1
SCLA_1to4, Children who started to be looked after during the year ending 31 March 2018 aged 1 to 4
SCLA_5to9, Children who started to be looked after during the year ending 31 March 2018 aged 5 to 9
SCLA_10to15, Children who started to be looked after during the year ending 31 March 2018 aged 10 to 15
SCLA_16over, Children who started to be looked after during the year ending 31 March 2018 aged 16 and over [3]
SCLA_U1_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 aged under 1
SCLA_1to4_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 aged 1 to 4
SCLA_5to9_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 aged 5 to 9
SCLA_10to15_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 aged 10 to 15
SCLA_16over_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 aged 16 and over [3]
SCLA_AbNeg, Children who started to be looked after during the year ending 31 March 2018 due to abuse or neglect
SCLA_Cdisab, Children who started to be looked after during the year ending 31 March 2018 due to the child?s disability
SCLA_ParIll, Children who started to be looked after during the year ending 31 March 2018 due to parental illness or disability
SCLA_FAcSt, Children who started to be looked after during the year ending 31 March 2018 due to family in acute stress
SCLA_FD, Children who started to be looked after during the year ending 31 March 2018 due to family dysfunction
SCLA_SUB, Children who started to be looked after during the year ending 31 March 2018 due to socially unacceptable behaviour
SCLA_LI, Children who started to be looked after during the year ending 31 March 2018 due to low income
SCLA_AbsPar, Children who started to be looked after during the year ending 31 March 2018 due to absent parenting [3]
SCLA_AbNeg_pc, Children who started to be looked after during the year ending 31 March 2018 due to abuse or neglect
SCLA_Cdisab_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 due to the child?s disability
SCLA_ParIll_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 due to parental illness or disability
SCLA_FAcSt_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 due to family in acute stress
SCLA_FD_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 due to family dysfunction
SCLA_SUB_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 due to socially unacceptable behaviour
SCLA_LI_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 due to low income
SCLA_AbsPar_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 due to absent parenting [3]
SCLA_ICO, Children who started to be looked after during the year ending 31 March 2018 under an interim care order
SCLA_FCO, Children who started to be looked after during the year ending 31 March 2018 under a full care order
SCLA_PlaceO, Children who started to be looked after during the year ending 31 March 2018 where a placement order has been granted [4]
SCLA_S20, Children who started to be looked after during the year ending 31 March 2018 under section 20 [3]
SCLA_UPP, Children who started to be looked after during the year ending 31 March 2018 under police protection
SCLA_SEPO, Children who started to be looked after during the year ending 31 March 2018 subject to an emergency protection order
SCLA_UCAO, Children who started to be looked after during the year ending 31 March 2018 under a child assessment order
SCLA_ONCT, Children who started to be looked after during the year ending 31 March 2018 on remand or committed for trial
SCLA_PACE, Children who started to be looked after during the year ending 31 March 2018 under the police and criminal evidence act
SCLA_SORR, Children who started to be looked after during the year ending 31 March 2018 under a supervision order with residence requirement
SCLA_ICO_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 under an interim care order
SCLA_FCO_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 under a full care order
SCLA_PlaceO_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 where a placement order has been granted [4]
SCLA_S20_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 under section 20 [3]
SCLA_UPP_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 under police protection
SCLA_SEPO_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 subject to an emergency protection order
SCLA_UCAO_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 under a child assessment order
SCLA_ONCT_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 on remand or committed for trial
SCLA_PACE_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 under the police and criminal evidence act
SCLA_SORR_pc, Percentage of children who started to be looked after during the year ending 31 March 2018 under a supervision order with residence requirement
CLA_cease2014, Children who ceased to be looked after during the year ending 31 March 2014 [1]
CLA_cease2015, Children who ceased to be looked after during the year ending 31 March 2015 [1]
CLA_cease2016, Children who ceased to be looked after during the year ending 31 March 2016 [1]
CLA_cease2017, Children who ceased to be looked after during the year ending 31 March 2017 [1]
CLA_cease2018, Children who ceased to be looked after during the year ending 31 March 2018 [1]
CEA_male, Male children who ceased to be looked after during the year ending 31 March 2018
CEA_female, Female children who ceased to be looked after during the year ending 31 March 2018
CEA_male_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2018 who were male
CEA_female_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2018 who were female
CEA_U1, Children who ceased to be looked after during the year ending 31 March 2018 aged under 1
CEA_1to4, Children who ceased to be looked after during the year ending 31 March 2018 aged 1 to 4
CEA_5to9, Children who ceased to be looked after during the year ending 31 March 2018 aged 5 to 9
CEA_10to15, Children who ceased to be looked after during the year ending 31 March 2018 aged 10 to 15
CEA_16, Children who ceased to be looked after during the year ending 31 March 2018 aged 16
CEA_17, Children who ceased to be looked after during the year ending 31 March 2018 aged 17
CEA_18over, Children who ceased to be looked after during the year ending 31 March 2018 aged 18 and over [2]
CEA_U1_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2018 aged under 1
CEA_1to4_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2018 aged 1 to 4
CEA_5to9_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2018 aged 5 to 9
CEA_10to15_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2018 aged 10 to 15
CEA_16_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2018 aged 16
CEA_17_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2018 aged 17
CEA_18over_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2018 aged 18 and over [2]
CEA_Adop1, Children who ceased to be looked after during the year ending 31 March 2018 due to adoption - application unopposed
CEA_Adop2, Children who ceased to be looked after during the year ending 31 March 2018 due to adoption - consent dispensed with
CEA_Died, Children who ceased to be looked after during the year ending 31 March 2018 as they have died
CEA_Taken, Children who ceased to be looked after during the year ending 31 March 2018 - care taken by another LA [2]
CEA_ParPlan, Children who ceased to be looked after during the year ending 31 March 2018 - returned home to live with parents or relatives as part of care planning process
CEA_ParNPlan, Children who ceased to be looked after during the year ending 31 March 2018 - returned home to live with parents or relatives not as part of care planning process
CEA_NoPar, Children who ceased to be looked after during the year ending 31 March 2018 - ceased to live with parents or relatives without parental responsibility
CEA_CAO, Children who ceased to be looked after during the year ending 31 March 2018 - child arrangement order granted
CEA_SGO1, Children who ceased to be looked after during the year ending 31 March 2018 - special guardianship order made to former foster carers
CEA_SGO2, Children who ceased to be looked after during the year ending 31 March 2018 - special guardianship order made to carers other than former foster carers
CEA_IndLiv1, Children who ceased to be looked after during the year ending 31 March 2018 - moved into independent living (with supportive accommodation) [2]
CEA_IndLiv2, Children who ceased to be looked after during the year ending 31 March 2018 - moved into independent living (with no formalised support) [2]
CEA_Residential, Children who ceased to be looked after during the year ending 31 March 2018 - transferred to residential care funded by adult social services
CEA_Custody, Children who ceased to be looked after during the year ending 31 March 2018 - sentenced to custody
CEA_RemEnd, Children who ceased to be looked after during the year ending 31 March 2018 - accommodation on remand ended
CEA_AgeAssmt, Children who ceased to be looked after during the year ending 31 March 2018 - age assessment determined child was 18 or over [2]
CEA_Abroad, Children who ceased to be looked after during the year ending 31 March 2018 - child moved abroad
CEA_Other, Children who ceased to be looked after during the year ending 31 March 2018 - care ceased for any other reason [3]
CEA_Adop_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2018 due to adoption
CL_All_17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1 2]
CL_All_17, Care leavers in the year ending 31 March 2018 aged 17 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1 2]
CL_All_18, Care leavers in the year ending 31 March 2018 aged 18 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1 2]
CL_Act_EET17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 who are in education employment or training
CL_Act_EET17, Care leavers in the year ending 31 March 2018 aged 17 who are in education employment or training
CL_Act_EET18, Care leavers in the year ending 31 March 2018 aged 18 who are in education employment or training
CL_Act_HE17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 whose activity is higher education i.e. studies beyond A level
CL_Act_HE17, Care leavers in the year ending 31 March 2018 aged 17 whose activity is higher education i.e. studies beyond A level
CL_Act_HE18, Care leavers in the year ending 31 March 2018 aged 18 whose activity is higher education i.e. studies beyond A level
CL_Act_OE17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 whose activity is education other than higher education
CL_Act_OE17, Care leavers in the year ending 31 March 2018 aged 17 whose activity is education other than higher education
CL_Act_OE18, Care leavers in the year ending 31 March 2018 aged 18 whose activity is education other than higher education
CL_Act_TE17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 whose activity is training or employment
CL_Act_TE17, Care leavers in the year ending 31 March 2018 aged 17 whose activity is training or employment
CL_Act_TE18, Care leavers in the year ending 31 March 2018 aged 18 whose activity is training or employment
CL_Act_NEET17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 who are not in education employment or training
CL_Act_NEET17, Care leavers in the year ending 31 March 2018 aged 17 who are not in education employment or training
CL_Act_NEET18, Care leavers in the year ending 31 March 2018 aged 18 who are not in education employment or training
CL_Act_NEET_ill17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill17, Care leavers in the year ending 31 March 2018 aged 17 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill18, Care leavers in the year ending 31 March 2018 aged 18 who are not in education training or employment due to illness or disability
CL_Act_NEET_oth17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 who are not in education training or employment due to other reasons
CL_Act_NEET_oth17, Care leavers in the year ending 31 March 2018 aged 17 who are not in education training or employment due to other reasons
CL_Act_NEET_oth18, Care leavers in the year ending 31 March 2018 aged 18 who are not in education training or employment due to other reasons
CL_Act_NEET_preg17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg17, Care leavers in the year ending 31 March 2018 aged 17 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg18, Care leavers in the year ending 31 March 2018 aged 18 who are not in education training or employment due to pregnancy or parenting
CL_Act_NoInf17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 for whom local authority does not have activity information
CL_Act_NoInf17, Care leavers in the year ending 31 March 2018 aged 17 for whom local authority does not have activity information
CL_Act_NoInf18, Care leavers in the year ending 31 March 2018 aged 18 for whom local authority does not have activity information
CL_Act_EET17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 who are in education employment or training
CL_Act_EET17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 who are in education employment or training
CL_Act_EET18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 who are in education employment or training
CL_Act_HE17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 whose activity is higher education i.e. studies beyond A level
CL_Act_HE17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 whose activity is higher education i.e. studies beyond A level
CL_Act_HE18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 whose activity is higher education i.e. studies beyond A level
CL_Act_OE17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 whose activity is education other than higher education
CL_Act_OE17_pc, Ca Percentage of care re leavers in the year ending 31 March 2018 aged 17 whose activity is education other than higher education
CL_Act_OE18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 whose activity is education other than higher education
CL_Act_TE17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 whose activity is training or employment
CL_Act_TE17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 whose activity is training or employment
CL_Act_TE18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 whose activity is training or employment
CL_Act_NEET17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 who are not in education employment or training
CL_Act_NEET17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 who are not in education employment or training
CL_Act_NEET18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 who are not in education employment or training
CL_Act_NEET_ill17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 who are not in education training or employment due to illness or disability
CL_Act_NEET_oth17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 who are not in education training or employment due to other reasons
CL_Act_NEET_oth17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 who are not in education training or employment due to other reasons
CL_Act_NEET_oth18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 who are not in education training or employment due to other reasons
CL_Act_NEET_preg17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 who are not in education training or employment due to pregnancy or parenting
CL_Act_NoInf17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 for whom local authority does not have activity information
CL_Act_NoInf17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 for whom local authority does not have activity information
CL_Act_NoInf18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 for whom local authority does not have activity information
CL_Acc_Total_SUIT17&18, Care leavers in the year ending 31 March 2018 aged 17 to 18 in suitable accommodation calculations [3 4]
CL_Acc_Total_SUIT17, Care leavers in the year ending 31 March 2018 aged 17 in suitable accommodation calculations [3 4]
CL_Acc_Total_SUIT18, Care leavers in the year ending 31 March 2018 aged 18 in suitable accommodation calculations [3 4]
CL_Acc_SUIT17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 in suitable accommodation [3 4]
CL_Acc_SUIT17, Care leavers in the year ending 31 March 2018 aged 17 in suitable accommodation [3 4]
CL_Acc_SUIT18, Care leavers in the year ending 31 March 2018 aged 18 in suitable accommodation [3 4]
CL_Acc_NotSUIT17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 not in suitable accommodation [3 4]
CL_Acc_NotSUIT17, Care leavers in the year ending 31 March 2018 aged 17 not in suitable accommodation [3 4]
CL_Acc_NotSUIT18, Care leavers in the year ending 31 March 2018 aged 18 not in suitable accommodation [3 4]
CL_Acc_NoSUITInfo17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 for whom suitability of accommodation is not available [3 5]
CL_Acc_NoSUITInfo17, Care leavers in the year ending 31 March 2018 aged 17 for whom suitability of accommodation is not available [3 5]
CL_Acc_NoSUITInfo18, Care leavers in the year ending 31 March 2018 aged 18 for whom suitability of accommodation is not available [3 5]
CL_Acc_SUIT17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 in suitable accommodation [3 4]
CL_Acc_SUIT17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 in suitable accommodation [3 4]
CL_Acc_SUIT18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 in suitable accommodation [3 4]
CL_Acc_NotSUIT17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 not in suitable accommodation [3 4]
CL_Acc_NotSUIT17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 not in suitable accommodation [3 4]
CL_Acc_NotSUIT18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 not in suitable accommodation [3 4]
CL_Acc_NoSUITInfo17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 for whom suitability of accommodation is not available [3 5]
CL_Acc_NoSUITInfo17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 for whom suitability of accommodation is not available [3 5]
CL_Acc_NoSUITInfo18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 for whom suitability of accommodation is not available [3 5]
CL_Acc_P17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated with parents or relatives [6]
CL_Acc_P17, Care leavers in the year ending 31 March 2018 aged 17 accommodated with parents or relatives [6]
CL_Acc_P18, Care leavers in the year ending 31 March 2018 aged 18 accommodated with parents or relatives [6]
CL_Acc_CH17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated in community homes
CL_Acc_CH17, Care leavers in the year ending 31 March 2018 aged 17 accommodated in community homes
CL_Acc_CH18, Care leavers in the year ending 31 March 2018 aged 18 accommodated in community homes
CL_Acc_SITA17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated in semi-independent transitional accommodation
CL_Acc_SITA17, Care leavers in the year ending 31 March 2018 aged 17 accommodated in semi-independent transitional accommodation
CL_Acc_SITA18, Care leavers in the year ending 31 March 2018 aged 18 accommodated in semi-independent transitional accommodation
CL_Acc_SL17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated in supported lodgings
CL_Acc_SL17, Care leavers in the year ending 31 March 2018 aged 17 accommodated in supported lodgings
CL_Acc_SL18, Care leavers in the year ending 31 March 2018 aged 18 accommodated in supported lodgings
CL_Acc_GA17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 who have gone abroad
CL_Acc_GA17, Care leavers in the year ending 31 March 2018 aged 17 who have gone abroad
CL_Acc_GA18, Care leavers in the year ending 31 March 2018 aged 18 who have gone abroad
CL_Acc_Dep17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 who have been deported
CL_Acc_Dep17, Care leavers in the year ending 31 March 2018 aged 17 who have been deported
CL_Acc_Dep18, Care leavers in the year ending 31 March 2018 aged 18 who have been deported
CL_Acc_OL17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated in ordinary lodgings
CL_Acc_OL17, Care leavers in the year ending 31 March 2018 aged 17 accommodated in ordinary lodgings
CL_Acc_OL18, Care leavers in the year ending 31 March 2018 aged 18 accommodated in ordinary lodgings
CL_Acc_NK17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 with residence not known
CL_Acc_NK17, Care leavers in the year ending 31 March 2018 aged 17 with residence not known
CL_Acc_NK18, Care leavers in the year ending 31 March 2018 aged 18 with residence not known
CL_Acc_NFA17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 with no fixed abode or homeless
CL_Acc_NFA17, Care leavers in the year ending 31 March 2018 aged 17 with no fixed abode or homeless
CL_Acc_NFA18, Care leavers in the year ending 31 March 2018 aged 18 with no fixed abode or homeless
CL_Acc_F17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated in foyers
CL_Acc_F17, Care leavers in the year ending 31 March 2018 aged 17 accommodated in foyers
CL_Acc_F18, Care leavers in the year ending 31 March 2018 aged 18 accommodated in foyers
CL_Acc_IL17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated in independent living
CL_Acc_IL17, Care leavers in the year ending 31 March 2018 aged 17 accommodated in independent living
CL_Acc_IL18, Care leavers in the year ending 31 March 2018 aged 18 accommodated in independent living
CL_Acc_EA17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated in emergency accommodation
CL_Acc_EA17, Care leavers in the year ending 31 March 2018 aged 17 accommodated in emergency accommodation
CL_Acc_EA18, Care leavers in the year ending 31 March 2018 aged 18 accommodated in emergency accommodation
CL_Acc_BB17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 in bed and breakfast accommodation
CL_Acc_BB17, Care leavers in the year ending 31 March 2018 aged 17 in bed and breakfast accommodation
CL_Acc_BB18, Care leavers in the year ending 31 March 2018 aged 18 in bed and breakfast accommodation
CL_Acc_Cust17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 who are in custody
CL_Acc_Cust17, Care leavers in the year ending 31 March 2018 aged 17 who are in custody
CL_Acc_Cust18, Care leavers in the year ending 31 March 2018 aged 18 who are in custody
CL_Acc_FFC17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated with former foster carers
CL_Acc_FFC17, Care leavers in the year ending 31 March 2018 aged 17 accommodated with former foster carers
CL_Acc_FFC18, Care leavers in the year ending 31 March 2018 aged 18 accommodated with former foster carers
CL_Acc_OTH17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 in other accommodation
CL_Acc_OTH17, Care leavers in the year ending 31 March 2018 aged 17 in other accommodation
CL_Acc_OTH18, Care leavers in the year ending 31 March 2018 aged 18 in other accommodation
CL_Acc_NoInf17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 for whom local authority does not have accommodation information [7]
CL_Acc_NoInf17, Care leavers in the year ending 31 March 2018 aged 17 for whom local authority does not have accommodation information [7]
CL_Acc_NoInf18, Care leavers in the year ending 31 March 2018 aged 18 for whom local authority does not have accommodation information [7]
CL_Acc_P17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated with parents or relatives [6]
CL_Acc_P17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 accommodated with parents or relatives [6]
CL_Acc_P18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 accommodated with parents or relatives [6]
CL_Acc_CH17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated in community homes
CL_Acc_CH17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 accommodated in community homes
CL_Acc_CH18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 accommodated in community homes
CL_Acc_SITA17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated in semi-independent transitional accommodation
CL_Acc_SITA17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 accommodated in semi-independent transitional accommodation
CL_Acc_SITA18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 accommodated in semi-independent transitional accommodation
CL_Acc_SL17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated in supported lodgings
CL_Acc_SL17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 accommodated in supported lodgings
CL_Acc_SL18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 accommodated in supported lodgings
CL_Acc_GA17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 who have gone abroad
CL_Acc_GA17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 who have gone abroad
CL_Acc_GA18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 who have gone abroad
CL_Acc_Dep17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 who have been deported
CL_Acc_Dep17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 who have been deported
CL_Acc_Dep18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 who have been deported
CL_Acc_OL17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated in ordinary lodgings
CL_Acc_OL17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 accommodated in ordinary lodgings
CL_Acc_OL18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 accommodated in ordinary lodgings
CL_Acc_NK17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 with residence not known
CL_Acc_NK17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 with residence not known
CL_Acc_NK18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 with residence not known
CL_Acc_NFA17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 with no fixed abode or homeless
CL_Acc_NFA17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 with no fixed abode or homeless
CL_Acc_NFA18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 with no fixed abode or homeless
CL_Acc_F17&18_pc, C Percentage of care are leavers in the year ending 31 March 2018 aged 17 and 18 accommodated in foyers
CL_Acc_F17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 accommodated in foyers
CL_Acc_F18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 accommodated in foyers
CL_Acc_IL17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated in independent living
CL_Acc_IL17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 accommodated in independent living
CL_Acc_IL18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 accommodated in independent living
CL_Acc_EA17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated in emergency accommodation
CL_Acc_EA17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 accommodated in emergency accommodation
CL_Acc_EA18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 accommodated in emergency accommodation
CL_Acc_BB17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 in bed and breakfast accommodation
CL_Acc_BB17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 in bed and breakfast accommodation
CL_Acc_BB18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 in bed and breakfast accommodation
CL_Acc_Cust17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 who are in custody
CL_Acc_Cust17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 who are in custody
CL_Acc_Cust18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 who are in custody
CL_Acc_FFC17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 accommodated with former foster carers
CL_Acc_FFC17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 accommodated with former foster carers
CL_Acc_FFC18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 accommodated with former foster carers
CL_Acc_OTH17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 in other accommodation
CL_Acc_OTH17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 in other accommodation
CL_Acc_OTH18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 in other accommodation
CL_Acc_NoInf17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 for whom local authority does not have accommodation information [7]
CL_Acc_NoInf17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 for whom local authority does not have accommodation information [7]
CL_Acc_NoInf18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 for whom local authority does not have accommodation information [7]
CL_InTouch_IT17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 who are in touch with the local authority
CL_InTouch_IT17, Care leavers in the year ending 31 March 2018 aged 17 who are in touch with the local authority
CL_InTouch_IT18, Care leavers in the year ending 31 March 2018 aged 18 who are in touch with the local authority
CL_InTouch_Refu17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 who refuse contact with the local authority
CL_InTouch_Refu17, Care leavers in the year ending 31 March 2018 aged 17 who refuse contact with the local authority
CL_InTouch_Refu18, Care leavers in the year ending 31 March 2018 aged 18 who refuse contact with the local authority
CL_InTouch_NoServ17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 who no longer require services
CL_InTouch_NoServ17, Care leavers in the year ending 31 March 2018 aged 17 who no longer require services
CL_InTouch_NoServ18, Care leavers in the year ending 31 March 2018 aged 18 who no longer require services
CL_InTouch_Not17&18, Care leavers in the year ending 31 March 2018 aged 17 and 18 who are not in touch with the local authority
CL_InTouch_Not17, Care leavers in the year ending 31 March 2018 aged 17 who are not in touch with the local authority
CL_InTouch_Not18, Care leavers in the year ending 31 March 2018 aged 18 who are not in touch with the local authority
CL_InTouch_IT17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 who are in touch with the local authority
CL_InTouch_IT17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 who are in touch with the local authority
CL_InTouch_IT18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 who are in touch with the local authority
CL_InTouch_Refu17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 who refuse contact with the local authority
CL_InTouch_Refu17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 who refuse contact with the local authority
CL_InTouch_Refu18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 who refuse contact with the local authority
CL_InTouch_NoServ17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 who no longer require services
CL_InTouch_NoServ17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 who no longer require services
CL_InTouch_NoServ18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 who no longer require services
CL_InTouch_Not17&18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 and 18 who are not in touch with the local authority
CL_InTouch_Not17_pc, Percentage of care leavers in the year ending 31 March 2018 aged 17 who are not in touch with the local authority
CL_InTouch_Not18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 who are not in touch with the local authority
CL_StayPut_18, Care leavers in the year ending 31 March 2018 aged 18 who ceased to be looked after from a foster placement on their 18th birthday
CL_StayPut_FFC_18, Care leavers in the year ending 31 March 2018 aged 18 who ceased to be looked after from a foster placement on their 18th birthday and are living with the former foster carers
CL_StayPut_FFC_18_pc, Percentage of care leavers in the year ending 31 March 2018 aged 18 who ceased to be looked after from a foster placement on their 18th birthday who are living with the former foster carers
CL_All_19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_All_19, Care leavers in the year ending 31 March 2018 aged 19 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_All_20, Care leavers in the year ending 31 March 2018 aged 20 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_All_21, Care leavers in the year ending 31 March 2018 aged 21 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_Act_EET19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 who are in education employment or training
CL_Act_EET19, Care leavers in the year ending 31 March 2018 aged 19 who are in education employment or training
CL_Act_EET20, Care leavers in the year ending 31 March 2018 aged 20 who are in education employment or training
CL_Act_EET21, Care leavers in the year ending 31 March 2018 aged 21 who are in education employment or training
CL_Act_HE19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 whose activity is higher education i.e. studies beyond A level
CL_Act_HE19, Care leavers in the year ending 31 March 2018 aged 19 whose activity is higher education i.e. studies beyond A level
CL_Act_HE20, Care leavers in the year ending 31 March 2018 aged 20 whose activity is higher education i.e. studies beyond A level
CL_Act_HE21, Care leavers in the year ending 31 March 2018 aged 21 whose activity is higher education i.e. studies beyond A level
CL_Act_OE19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 whose activity is education other than higher education
CL_Act_OE19, Care leavers in the year ending 31 March 2018 aged 19 whose activity is education other than higher education
CL_Act_OE20, Care leavers in the year ending 31 March 2018 aged 20 whose activity is education other than higher education
CL_Act_OE21, Care leavers in the year ending 31 March 2018 aged 21 whose activity is education other than higher education
CL_Act_TE19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 whose activity is training or employment
CL_Act_TE19, Care leavers in the year ending 31 March 2018 aged 19 whose activity is training or employment
CL_Act_TE20, Care leavers in the year ending 31 March 2018 aged 20 whose activity is training or employment
CL_Act_TE21, Care leavers in the year ending 31 March 2018 aged 21 whose activity is training or employment
CL_Act_NEET19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 who are not in education employment or training
CL_Act_NEET19, Care leavers in the year ending 31 March 2018 aged 19 who are not in education employment or training
CL_Act_NEET20, Care leavers in the year ending 31 March 2018 aged 20 who are not in education employment or training
CL_Act_NEET21, Care leavers in the year ending 31 March 2018 aged 21 who are not in education employment or training
CL_Act_NEET_ill19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill19, Care leavers in the year ending 31 March 2018 aged 19 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill20, Care leavers in the year ending 31 March 2018 aged 20 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill21, Care leavers in the year ending 31 March 2018 aged 21 who are not in education training or employment due to illness or disability
CL_Act_NEET_oth19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 who are not in education training or employment due to other reasons
CL_Act_NEET_oth19, Care leavers in the year ending 31 March 2018 aged 19 who are not in education training or employment due to other reasons
CL_Act_NEET_oth20, Care leavers in the year ending 31 March 2018 aged 20 who are not in education training or employment due to other reasons
CL_Act_NEET_oth21, Care leavers in the year ending 31 March 2018 aged 21 who are not in education training or employment due to other reasons
CL_Act_NEET_preg19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg19, Care leavers in the year ending 31 March 2018 aged 19 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg20, Care leavers in the year ending 31 March 2018 aged 20 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg21, Care leavers in the year ending 31 March 2018 aged 21 who are not in education training or employment due to pregnancy or parenting
CL_Act_NoInf19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 for whom local authority does not have activity information
CL_Act_NoInf19, Care leavers in the year ending 31 March 2018 aged 19 for whom local authority does not have activity information
CL_Act_NoInf20, Care leavers in the year ending 31 March 2018 aged 20 for whom local authority does not have activity information
CL_Act_NoInf21, Care leavers in the year ending 31 March 2018 aged 21 for whom local authority does not have activity information
CL_Act_EET19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 who are in education employment or training
CL_Act_EET19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 who are in education employment or training
CL_Act_EET20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 who are in education employment or training
CL_Act_EET21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 who are in education employment or training
CL_Act_HE19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 whose activity is higher education i.e. studies beyond A level
CL_Act_HE19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 whose activity is higher education i.e. studies beyond A level
CL_Act_HE20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 whose activity is higher education i.e. studies beyond A level
CL_Act_HE21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 whose activity is higher education i.e. studies beyond A level
CL_Act_OE19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 whose activity is education other than higher education
CL_Act_OE19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 whose activity is education other than higher education
CL_Act_OE20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 whose activity is education other than higher education
CL_Act_OE21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 whose activity is education other than higher education
CL_Act_TE19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 whose activity is training or employment
CL_Act_TE19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 whose activity is training or employment
CL_Act_TE20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 whose activity is training or employment
CL_Act_TE21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 whose activity is training or employment
CL_Act_NEET19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 who are not in education employment or training
CL_Act_NEET19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 who are not in education employment or training
CL_Act_NEET20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 who are not in education employment or training
CL_Act_NEET21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 who are not in education employment or training
CL_Act_NEET_ill19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 who are not in education training or employment due to illness or disability
CL_Act_NEET_oth19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 who are not in education training or employment due to other reasons
CL_Act_NEET_oth19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 who are not in education training or employment due to other reasons
CL_Act_NEET_oth20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 who are not in education training or employment due to other reasons
CL_Act_NEET_oth21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 who are not in education training or employment due to other reasons
CL_Act_NEET_preg19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 who are not in education training or employment due to pregnancy or parenting
CL_Act_NoInf19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 for whom local authority does not have activity information
CL_Act_NoInf19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 for whom local authority does not have activity information
CL_Act_NoInf20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 for whom local authority does not have activity information
CL_Act_NoInf21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 for whom local authority does not have activity information
CL_InTouch_IT19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 who are in touch with the local authority
CL_InTouch_IT19, Care leavers in the year ending 31 March 2018 aged 19 who are in touch with the local authority
CL_InTouch_IT20, Care leavers in the year ending 31 March 2018 aged 20 who are in touch with the local authority
CL_InTouch_IT21, Care leavers in the year ending 31 March 2018 aged 21 who are in touch with the local authority
CL_InTouch_Refu19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 who refuse contact with the local authority
CL_InTouch_Refu19, Care leavers in the year ending 31 March 2018 aged 19 who refuse contact with the local authority
CL_InTouch_Refu20, Care leavers in the year ending 31 March 2018 aged 20 who refuse contact with the local authority
CL_InTouch_Refu21, Care leavers in the year ending 31 March 2018 aged 21 who refuse contact with the local authority
CL_InTouch_NoServ19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 who no longer require services
CL_InTouch_NoServ19, Care leavers in the year ending 31 March 2018 aged 19 who no longer require services
CL_InTouch_NoServ20, Care leavers in the year ending 31 March 2018 aged 20 who no longer require services
CL_InTouch_NoServ21, Care leavers in the year ending 31 March 2018 aged 21 who no longer require services
CL_InTouch_Not19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 who are not in touch with the local authority
CL_InTouch_Not19, Care leavers in the year ending 31 March 2018 aged 19 who are not in touch with the local authority
CL_InTouch_Not20, Care leavers in the year ending 31 March 2018 aged 20 who are not in touch with the local authority
CL_InTouch_Not21, Care leavers in the year ending 31 March 2018 aged 21 who are not in touch with the local authority
CL_InTouch_IT19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 who are in touch with the local authority
CL_InTouch_IT19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 who are in touch with the local authority
CL_InTouch_IT20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 who are in touch with the local authority
CL_InTouch_IT21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 who are in touch with the local authority
CL_InTouch_Refu19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 who refuse contact with the local authority
CL_InTouch_Refu19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 who refuse contact with the local authority
CL_InTouch_Refu20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 who refuse contact with the local authority
CL_InTouch_Refu21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 who refuse contact with the local authority
CL_InTouch_NoServ19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 who no longer require services
CL_InTouch_NoServ19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 who no longer require services
CL_InTouch_NoServ20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 who no longer require services
CL_InTouch_NoServ21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 who no longer require services
CL_InTouch_Not19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 who are not in touch with the local authority
CL_InTouch_Not19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 who are not in touch with the local authority
CL_InTouch_Not20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 who are not in touch with the local authority
CL_InTouch_Not21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 who are not in touch with the local authority
CL_All_19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_All_19, Care leavers in the year ending 31 March 2018 aged 19 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_All_20, Care leavers in the year ending 31 March 2018 aged 20 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_All_21, Care leavers in the year ending 31 March 2018 aged 21 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_Acc_Total_SUIT19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 in suitable accommodation calculations [2 3]
CL_Acc_Total_SUIT19, Care leavers in the year ending 31 March 2018 aged 19 in suitable accommodation calculations [2 3]
CL_Acc_Total_SUIT20, Care leavers in the year ending 31 March 2018 aged 20 in suitable accommodation calculations [2 3]
CL_Acc_Total_SUIT21, Care leavers in the year ending 31 March 2018 aged 21 in suitable accommodation calculations [2 3]
CL_Acc_SUIT19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 in suitable accommodation [2 3]
CL_Acc_SUIT19, Care leavers in the year ending 31 March 2018 aged 19 in suitable accommodation [2 3]
CL_Acc_SUIT20, Care leavers in the year ending 31 March 2018 aged 20 in suitable accommodation [2 3]
CL_Acc_SUIT21, Care leavers in the year ending 31 March 2018 aged 21 in suitable accommodation [2 3]
CL_Acc_NotSUIT19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 not in suitable accommodation [2 3]
CL_Acc_NotSUIT19, Care leavers in the year ending 31 March 2018 aged 19 not in suitable accommodation [2 3]
CL_Acc_NotSUIT20, Care leavers in the year ending 31 March 2018 aged 20 not in suitable accommodation [2 3]
CL_Acc_NotSUIT21, Care leavers in the year ending 31 March 2018 aged 21 not in suitable accommodation [2 3]
CL_Acc_NoSUITInfo19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 for whom suitability of accommodation is not available [2 4]
CL_Acc_NoSUITInfo19, Care leavers in the year ending 31 March 2018 aged 19 for whom suitability of accommodation is not available [2 4]
CL_Acc_NoSUITInfo20, Care leavers in the year ending 31 March 2018 aged 20 for whom suitability of accommodation is not available [2 4]
CL_Acc_NoSUITInfo21, Care leavers in the year ending 31 March 2018 aged 21 for whom suitability of accommodation is not available [2 4]
CL_Acc_SUIT19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 in suitable accommodation [2 3]
CL_Acc_SUIT19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 in suitable accommodation [2 3]
CL_Acc_SUIT20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 in suitable accommodation [2 3]
CL_Acc_SUIT21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 in suitable accommodation [2 3]
CL_Acc_NotSUIT19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 not in suitable accommodation [2 3]
CL_Acc_NotSUIT19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 not in suitable accommodation [2 3]
CL_Acc_NotSUIT20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 not in suitable accommodation [2 3]
CL_Acc_NotSUIT21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 not in suitable accommodation [2 3]
CL_Acc_NoSUITInfo19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 for whom suitability of accommodation is not available [2 4]
CL_Acc_NoSUITInfo19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 for whom suitability of accommodation is not available [2 4]
CL_Acc_NoSUITInfo20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 for whom suitability of accommodation is not available [2 4]
CL_Acc_NoSUITInfo21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 for whom suitability of accommodation is not available [2 4]
CL_Acc_P19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 years old accommodated with parents or relatives [5]
CL_Acc_P19, Care leavers in the year ending 31 March 2018 aged 19 accommodated with parents or relatives [5]
CL_Acc_P20, Care leavers in the year ending 31 March 2018 aged 20 accommodated with parents or relatives [5]
CL_Acc_P21, Care leavers in the year ending 31 March 2018 aged 21 accommodated with parents or relatives [5]
CL_Acc_CH19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated in community homes
CL_Acc_CH19, Care leavers in the year ending 31 March 2018 aged 19 accommodated in community homes
CL_Acc_CH20, Care leavers in the year ending 31 March 2018 aged 20 accommodated in community homes
CL_Acc_CH21, Care leavers in the year ending 31 March 2018 aged 21 accommodated in community homes
CL_Acc_SITA19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated in semi-independent transitional accommodation
CL_Acc_SITA19, Care leavers in the year ending 31 March 2018 aged 19 accommodated in semi-independent transitional accommodation
CL_Acc_SITA20, Care leavers in the year ending 31 March 2018 aged 20 accommodated in semi-independent transitional accommodation
CL_Acc_SITA21, Care leavers in the year ending 31 March 2018 aged 21 accommodated in semi-independent transitional accommodation
CL_Acc_SL19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated in supported lodgings
CL_Acc_SL19, Care leavers in the year ending 31 March 2018 aged 19 accommodated in supported lodgings
CL_Acc_SL20, Care leavers in the year ending 31 March 2018 aged 20 accommodated in supported lodgings
CL_Acc_SL21, Care leavers in the year ending 31 March 2018 aged 21 accommodated in supported lodgings
CL_Acc_GA19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 who have gone abroad
CL_Acc_GA19, Care leavers in the year ending 31 March 2018 aged 19 who have gone abroad
CL_Acc_GA20, Care leavers in the year ending 31 March 2018 aged 20 who have gone abroad
CL_Acc_GA21, Care leavers in the year ending 31 March 2018 aged 21 who have gone abroad
CL_Acc_Dep19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 who have been deported
CL_Acc_Dep19, Care leavers in the year ending 31 March 2018 aged 19 who have been deported
CL_Acc_Dep20, Care leavers in the year ending 31 March 2018 aged 20 who have been deported
CL_Acc_Dep21, Care leavers in the year ending 31 March 2018 aged 21 who have been deported
CL_Acc_OL19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated in ordinary lodgings
CL_Acc_OL19, Care leavers in the year ending 31 March 2018 aged 19 accommodated in ordinary lodgings
CL_Acc_OL20, Care leavers in the year ending 31 March 2018 aged 20 accommodated in ordinary lodgings
CL_Acc_OL21, Care leavers in the year ending 31 March 2018 aged 21 accommodated in ordinary lodgings
CL_Acc_NK19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 with residence not known
CL_Acc_NK19, Care leavers in the year ending 31 March 2018 aged 19 with residence not known
CL_Acc_NK20, Care leavers in the year ending 31 March 2018 aged 20 with residence not known
CL_Acc_NK21, Care leavers in the year ending 31 March 2018 aged 21 with residence not known
CL_Acc_NFA19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 with no fixed abode or homeless
CL_Acc_NFA19, Care leavers in the year ending 31 March 2018 aged 19 with no fixed abode or homeless
CL_Acc_NFA20, Care leavers in the year ending 31 March 2018 aged 20 with no fixed abode or homeless
CL_Acc_NFA21, Care leavers in the year ending 31 March 2018 aged 21 with no fixed abode or homeless
CL_Acc_F19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated in foyers
CL_Acc_F19, Care leavers in the year ending 31 March 2018 aged 19 accommodated in foyers
CL_Acc_F20, Care leavers in the year ending 31 March 2018 aged 20 accommodated in foyers
CL_Acc_F21, Care leavers in the year ending 31 March 2018 aged 21 accommodated in foyers
CL_Acc_IL19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated in independent living
CL_Acc_IL19, Care leavers in the year ending 31 March 2018 aged 19 accommodated in independent living
CL_Acc_IL20, Care leavers in the year ending 31 March 2018 aged 20 accommodated in independent living
CL_Acc_IL21, Care leavers in the year ending 31 March 2018 aged 21 accommodated in independent living
CL_Acc_EA19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated in emergency accommodation
CL_Acc_EA19, Care leavers in the year ending 31 March 2018 aged 19 accommodated in emergency accommodation
CL_Acc_EA20, Care leavers in the year ending 31 March 2018 aged 20 accommodated in emergency accommodation
CL_Acc_EA21, Care leavers in the year ending 31 March 2018 aged 21 accommodated in emergency accommodation
CL_Acc_BB19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 in bed and breakfast accommodation
CL_Acc_BB19, Care leavers in the year ending 31 March 2018 aged 19 in bed and breakfast accommodation
CL_Acc_BB20, Care leavers in the year ending 31 March 2018 aged 20 in bed and breakfast accommodation
CL_Acc_BB21, Care leavers in the year ending 31 March 2018 aged 21 in bed and breakfast accommodation
CL_Acc_Cust19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 who are in custody
CL_Acc_Cust19, Care leavers in the year ending 31 March 2018 aged 19 who are in custody
CL_Acc_Cust20, Care leavers in the year ending 31 March 2018 aged 20 who are in custody
CL_Acc_Cust21, Care leavers in the year ending 31 March 2018 aged 21 who are in custody
CL_Acc_FFC19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated with former foster carers
CL_Acc_FFC19, Care leavers in the year ending 31 March 2018 aged 19 accommodated with former foster carers
CL_Acc_FFC20, Care leavers in the year ending 31 March 2018 aged 20 accommodated with former foster carers
CL_Acc_FFC21, Care leavers in the year ending 31 March 2018 aged 21 accommodated with former foster carers
CL_Acc_OTH19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 in other accommodation
CL_Acc_OTH19, Care leavers in the year ending 31 March 2018 aged 19 in other accommodation
CL_Acc_OTH20, Care leavers in the year ending 31 March 2018 aged 20 in other accommodation
CL_Acc_OTH21, Care leavers in the year ending 31 March 2018 aged 21 in other accommodation
CL_Acc_NoInf19to21, Care leavers in the year ending 31 March 2018 aged 19 to 21 for whom local authority does not have accommodation information [6]
CL_Acc_NoInf19, Care leavers in the year ending 31 March 2018 aged 19 for whom local authority does not have accommodation information [6]
CL_Acc_NoInf20, Care leavers in the year ending 31 March 2018 aged 20 for whom local authority does not have accommodation information [6]
CL_Acc_NoInf21, Care leavers in the year ending 31 March 2018 aged 21 for whom local authority does not have accommodation information [6]
CL_Acc_P19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 years old accommodated with parents or relatives [5]
CL_Acc_P19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 accommodated with parents or relatives [5]
CL_Acc_P20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 accommodated with parents or relatives [5]
CL_Acc_P21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 accommodated with parents or relatives [5]
CL_Acc_CH19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated in community homes
CL_Acc_CH19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 accommodated in community homes
CL_Acc_CH20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 accommodated in community homes
CL_Acc_CH21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 accommodated in community homes
CL_Acc_SITA19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated in semi-independent transitional accommodation
CL_Acc_SITA19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 accommodated in semi-independent transitional accommodation
CL_Acc_SITA20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 accommodated in semi-independent transitional accommodation
CL_Acc_SITA21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 accommodated in semi-independent transitional accommodation
CL_Acc_SL19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated in supported lodgings
CL_Acc_SL19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 accommodated in supported lodgings
CL_Acc_SL20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 accommodated in supported lodgings
CL_Acc_SL21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 accommodated in supported lodgings
CL_Acc_GA19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 who have gone abroad
CL_Acc_GA19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 who have gone abroad
CL_Acc_GA20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 who have gone abroad
CL_Acc_GA21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 who have gone abroad
CL_Acc_Dep19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 who have been deported
CL_Acc_Dep19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 who have been deported
CL_Acc_Dep20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 who have been deported
CL_Acc_Dep21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 who have been deported
CL_Acc_OL19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated in ordinary lodgings
CL_Acc_OL19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 accommodated in ordinary lodgings
CL_Acc_OL20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 accommodated in ordinary lodgings
CL_Acc_OL21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 accommodated in ordinary lodgings
CL_Acc_NK19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 with residence not known
CL_Acc_NK19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 with residence not known
CL_Acc_NK20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 with residence not known
CL_Acc_NK21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 with residence not known
CL_Acc_NFA19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 with no fixed abode or homeless
CL_Acc_NFA19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 with no fixed abode or homeless
CL_Acc_NFA20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 with no fixed abode or homeless
CL_Acc_NFA21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 with no fixed abode or homeless
CL_Acc_F19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated in foyers
CL_Acc_F19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 accommodated in foyers
CL_Acc_F20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 accommodated in foyers
CL_Acc_F21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 accommodated in foyers
CL_Acc_IL19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated in independent living
CL_Acc_IL19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 accommodated in independent living
CL_Acc_IL20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 accommodated in independent living
CL_Acc_IL21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 accommodated in independent living
CL_Acc_EA19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated in emergency accommodation
CL_Acc_EA19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 accommodated in emergency accommodation
CL_Acc_EA20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 accommodated in emergency accommodation
CL_Acc_EA21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 accommodated in emergency accommodation
CL_Acc_BB19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 in bed and breakfast accommodation
CL_Acc_BB19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 in bed and breakfast accommodation
CL_Acc_BB20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 in bed and breakfast accommodation
CL_Acc_BB21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 in bed and breakfast accommodation
CL_Acc_Cust19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 who are in custody
CL_Acc_Cust19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 who are in custody
CL_Acc_Cust20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 who are in custody
CL_Acc_Cust21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 who are in custody
CL_Acc_FFC19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 accommodated with former foster carers
CL_Acc_FFC19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 accommodated with former foster carers
CL_Acc_FFC20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 accommodated with former foster carers
CL_Acc_FFC21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 accommodated with former foster carers
CL_Acc_OTH19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 in other accommodation
CL_Acc_OTH19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 in other accommodation
CL_Acc_OTH20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 in other accommodation
CL_Acc_OTH21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 in other accommodation
CL_Acc_NoInf19to21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 to 21 for whom local authority does not have accommodation information [6]
CL_Acc_NoInf19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 for whom local authority does not have accommodation information [6]
CL_Acc_NoInf20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 for whom local authority does not have accommodation information [6]
CL_Acc_NoInf21_pc, Percentage of care leavers in the year ending 31 March 2018 aged 21 for whom local authority does not have accommodation information [6]
CL_StayPut_19&20, Care leavers in the year ending 31 March 2018 aged 19 and 20 who ceased to be looked after from a foster placement on their 18th birthday
CL_StayPut_19, Care leavers in the year ending 31 March 2018 aged 19 who ceased to be looked after from a foster placement on their 18th birthday
CL_StayPut_20, Care leavers in the year ending 31 March 2018 aged 20 who ceased to be looked after from a foster placement on their 18th birthday
CL_StayPut_FFC_19&20, Care leavers in the year ending 31 March 2018 aged 19 and 20 who ceased to be looked after from a foster placement on their 18th birthday and are living with the former foster carers
CL_StayPut_FFC_19, Care leavers in the year ending 31 March 2018 aged 19 who ceased to be looked after from a foster placement on their 18th birthday and are living with the former foster carers
CL_StayPut_FFC_20, Care leavers in the year ending 31 March 2018 aged 20 who ceased to be looked after from a foster placement on their 18th birthday and are living with the former foster carers
CL_StayPut_FFC_19&20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 and 20 who ceased to be looked after from a foster placement on their 18th birthday who are living with the former foster carers
CL_StayPut_FFC_19_pc, Percentage of care leavers in the year ending 31 March 2018 aged 19 who ceased to be looked after from a foster placement on their 18th birthday who are living with the former foster carers
CL_StayPut_FFC_20_pc, Percentage of care leavers in the year ending 31 March 2018 aged 20 who ceased to be looked after from a foster placement on their 18th birthday who are living with the former foster carers
Miss_Miss_DuringYear, Children looked after during the year ending 31 March 2018 who had a missing incident during the year [1 2]
Miss_Miss_DuringYear_pc, Percentage of children looked after during the year ending 31 March 2018 who had a missing incident during the year [1 2]
Miss_Miss_Incs, Number of missing incidents during the year for children looked after during the year ending 31 March 2018 [3]
Miss_Miss_Incs_av, Average number of missing incidents during the year for children looked after during the year ending 31 March 2018 who went missing [3 4]
Miss_Miss_MoreOnce, Children looked after during the year ending 31 March 2018 who went missing more than once during the year [5]
Miss_Miss_31Mar, Children looked after who were missing at 31 March 2018
Miss_Away_DuringYear, Children looked after during the year ending 31 March 2018 who were away from placement without authorisation during the year [2 6 7]
Miss_Away_DuringYear_pc, Percentage of children looked after during the year ending 31 March 2018 who were away from placement without authorisation during the year [2 6 7]
Miss_Away_Incs, Number of away from placements without authorisation incidents during the year for children looked after during the year ending 31 March 2018 [3]
Miss_Away_Incs_av, Average number of away from placements without authorisation incidents during the year for children looked after during the year ending 31 March 2018 who went away [3 4]
Miss_Away_MoreOnce, Children looked after during the year ending 31 March 2018 who were away from placement without authorisation more than once during the year [5]
Miss_Away_31Mar, Children looked after who were away from placement without authorisation at 31 March 2018
OC2_12mths, Children looked after at 31 March 2018 for at least 12 months [1 2]
OC2_10to17, Children looked after at 31 March 2018 for at least 12 months aged 10 to 17 years [3]
OC2_convicted, Children looked after at 31 March 2018 aged 10 to 17 years who were convicted or subject to youth cautions or youth conditional cautions during the year [4]
OC2_convicted_pc, Percentage of children looked after at 31 March 2018 aged 10 to 17 years who were convicted or subject to youth cautions or youth conditional cautions during the year [4]
OC2_submisuse, Children looked after at 31 March 2018 identified as having a substance misuse problem [2 5]
OC2_subint, Children looked after at 31 March 2018 received intervention for substance misuse [6]
OC2_suboffint, Children looked after at 31 March 2018 offered intervention for substance misuse and refused [7]
OC2_submisuse_pc, Percentage of children looked after at 31 March 2018 identified as having a substance misuse problem [5]
OC2_subint_pc, Percentage of children looked after at 31 March 2018 identified as having a substance misuse problem who received an intervention [6]
OC2_suboffint_pc, Percentage of children looked after at 31 March 2018 identified as having a substance misuse problem who were offered an intervention but refused it [7]
OC2_immunisation, Children looked after at 31 March 2018 whose immunisations were up to date [8]
OC2_teethcheck, Children looked after at 31 March 2018 who had their teeth checked [9]
OC2_healthassmt, Children looked after at 31 March 2018 who had their annual health assessment [10]
OC2_immunisation_pc, Percentage of children looked after at 31 March 2018 whose immunisations were up to date [8]
OC2_teethcheck_pc, Percentage of children looked after at 31 March 2018 who had their teeth checked [9]
OC2_healthassmt_pc, Percentage of children looked after at 31 March 2018 who had their annual health assessment [10]
OC2_4andunder, Children looked after at 31 March 2018 for at least 12 months aged 4 and under [11]
OC2_devassmt, Children looked after at 31 March 2018 aged 4 and under whose development assessments were up to date [11]
OC2_devassmt_pc, Percentage of children looked after at 31 March 2018 aged 4 and under whose development assessments were up to date [11]
OC2_5to16, Children looked after at 31 March 2018 for at least 12 months aged 5 to 16
OC2_SDQ, Children looked after at 31 March 2018 aged 5 to 16 with an SDQ score [12]
OC2_SDQ_pc, Percentage of children looked after at 31 March 2018  aged 5 to 16 with an SDQ score [12]
OC2_SDQnormal, Children looked after at 31 March 2018 aged 5 to 16 whose SDQ score was normal [13]
OC2_SDQborderline, Children looked after at 31 March 2018 aged 5 to 16 whose SDQ score was borderline [13]
OC2_SDQconcern, Children looked after at 31 March 2018 aged 5 to 16 whose SDQ score was a cause for concern [13]
OC2_SDQnormal_pc, Percentage of children looked after at 31 March 2018 aged 5 to 16 whose SDQ score was normal [13]
OC2_SDQborderline_pc, Percentage of children looked after at 31 March 2018 aged 5 to 16 whose SDQ score was borderline [13]
OC2_SDQconcern_pc, Percentage of children looked after at 31 March 2018 aged 5 to 16 whose SDQ score was a cause for concern [13]
OC2_SDQaverage, Children looked after at 31 March 2018 aged 5 to 16 with an SDQ score - average SDQ score


"

varlist_2018 <- read_csv(varlist_2018, col_names = c("varname", "description"))


varlist_2019 <- "

New_geog_code, New geography code
geog_l, Geography level
geog_c, Geography code
geog_n, Geography name
CLA_Mar2015, Children looked after at 31 March 2015
CLA_Mar2016, Children looked after at 31 March 2016
CLA_Mar2017, Children looked after at 31 March 2017
CLA_Mar2018, Children looked after at 31 March 2018
CLA_Mar2019, Children looked after at 31 March 2019
CLA_Pop2014, Mid-year population estimate of children in 2014
CLA_Pop2015, Mid-year population estimate of children in 2015
CLA_Pop2016, Mid-year population estimate of children in 2016
CLA_Pop2017, Mid-year population estimate of children in 2017
CLA_Pop2018, Mid-year population estimate of children in 2018
CLA_Rate2015, Rate of looked after children per 10000 at 31 March 2015 [1]
CLA_Rate2016, Rate of looked after children per 10000 at 31 March 2016 [1]
CLA_Rate2017, Rate of looked after children per 10000 at 31 March 2017 [1]
CLA_Rate2018, Rate of looked after children per 10000 at 31 March 2018 [1]
CLA_Rate2019, Rate of looked after children per 10000 at 31 March 2019 [1]
CLA_2015, Children looked after during the year ending 31 March 2015
CLA_2016, Children looked after during the year ending 31 March 2016
CLA_2017, Children looked after during the year ending 31 March 2017
CLA_2018, Children looked after during the year ending 31 March 2018
CLA_2019, Children looked after during the year ending 31 March 2019
CLA_stp2015, Children looked after during the year ending 31 March 2015 who were only looked after under a series of short term placements [2]
CLA_stp2016, Children looked after during the year ending 31 March 2016 who were only looked after under a series of short term placements [2]
CLA_stp2017, Children looked after during the year ending 31 March 2017 who were only looked after under a series of short term placements [2]
CLA_stp2018, Children looked after during the year ending 31 March 2018 who were only looked after under a series of short term placements [2]
CLA_stp2019, Children looked after during the year ending 31 March 2019 who were only looked after under a series of short term placements [2]
CLA_male, Male children looked after at 31 March 2019
CLA_female, Female children looked after at 31 March 2019
CLA_male_pc, Percentage of children looked after at 31 March 2019 who were male
CLA_female_pc, Percentage of children looked after at 31 March 2019 who were female
CLA_U1, Children looked after at 31 March 2019 aged under 1
CLA_1to4, Children looked after at 31 March 2019 aged 1 to 4
CLA_5to9, Children looked after at 31 March 2019 aged 5 to 9
CLA_10to15, Children looked after at 31 March 2019 aged 10 to 15
CLA_16over, Children looked after at 31 March 2019 aged 16 and over [3]
CLA_U1_pc, Percentage of children looked after at 31 March 2019 aged under 1
CLA_1to4_pc, Percentage of children looked after at 31 March 2019 aged 1 to 4
CLA_5to9_pc, Percentage of children looked after at 31 March 2019 aged 5 to 9
CLA_10to15_pc, Percentage of children looked after at 31 March 2019 aged 10 to 15
CLA_16over_pc, Percentage of children looked after at 31 March 2019 aged 16 and over [3]
CLA_White, Children looked after at 31 March 2019 from a white ethnic background
CLA_Mixed, Children looked after at 31 March 2019 from a mixed ethnic background
CLA_Asian, Children looked after at 31 March 2019 from an Asian ethnic background [3]
CLA_Black, Children looked after at 31 March 2019 from a black ethnic background
CLA_EOTH, Children looked after at 31 March 2019 from any other ethnic background [3]
CLA_Oth, Children looked after at 31 March 2019 where ethnic background is not known [4]
CLA_White_pc, Percentage of children looked after at 31 March 2019 from a white ethnic background
CLA_Mixed_pc, Percentage of children looked after at 31 March 2019 from a mixed ethnic background
CLA_Asian_pc, Percentage of children looked after at 31 March 2019 from an Asian ethnic background [3]
CLA_Black_pc, Percentage of children looked after at 31 March 2019 from a black ethnic background
CLA_EOTH_pc, Percentage of children looked after at 31 March 2019 from any other ethnic background [3]
CLA_Oth_pc, Percentage of children looked after at 31 March 2019 where ethnic background is not known [4]
CLA_Fost, Children looked after at 31 March 2019 in foster placements
CLA_Adopt, Children looked after at 31 March 2019 placed for adoption
CLA_Parent, Children looked after at 31 March 2019 placed with parents
CLA_Ocom, Children looked after at 31 March 2019 placed in other placements within the community
CLA_Secure, Children looked after at 31 March 2019 placed in secure units children?s homes and semi-independent living accommodation [5]
CLA_Ores, Children looked after at 31 March 2019 placed in other residential settings
CLA_RSch, Children looked after at 31 March 2019 placed in residential schools
CLA_OthPl, Children looked after at 31 March 2019 in other placements
CLA_Fost_pc, Percentage of children looked after at 31 March 2019 in foster placements
CLA_Adopt_pc, Percentage of children looked after at 31 March 2019 placed for adoption
CLA_Parent_pc, Percentage of children looked after at 31 March 2019 placed with parents
CLA_Ocom_pc, Percentage of children looked after at 31 March 2019 placed in other placements within the community
CLA_Secure_pc, Percentage of children looked after at 31 March 2019 placed in secure units children?s homes and semi-independent living accommodation [5]
CLA_Ores_pc, Percentage of children looked after at 31 March 2019 placed in other residential settings
CLA_RSch_pc, Percentage of children looked after at 31 March 2019 placed in residential schools
CLA_OthPl_pc, Percentage of children looked after at 31 March 2019 placed in other placements
CLA_ICO, Children looked after at 31 March 2019 under an interim care order
CLA_FCO, Children looked after at 31 March 2019 under a full care order
CLA_FrAd, Children looked after at 31 March 2019 who are freed for adoption [6]
CLA_PlaceO, Children looked after at 31 March 2019 who have a placement order [7]
CLA_S20, Children looked after at 31 March 2019 accommodated under section 20 [3]
CLA_CPG, Children looked after at 31 March 2019 detained under child protection grounds [8]
CLA_YJLS, Children looked after at 31 March 2019 under a youth justice legal status [9]
CLA_ICO_pc, Percentage of children looked after at 31 March 2019 under an interim care order
CLA_FCO_pc, Percentage of children looked after at 31 March 2019 under a full care order
CLA_FrAd_pc, Percentage of children looked after at 31 March 2019 who are freed for adoption [6]
CLA_PlaceO_pc, Percentage of children looked after at 31 March 2019 who have a placement order [7]
CLA_S20_pc, Percentage of children looked after at 31 March 2019 accommodated under section 20 [3]
CLA_CPG_pc, Percentage of children looked after at 31 March 2019 detained under child protection grounds [8]
CLA_YJLS_pc, Percentage of children looked after at 31 March 2019 under a youth justice legal status [9]
CLA_UASC2015, Children looked after at 31 March 2015 who were unaccompanied asylum seekers during the year [3]
CLA_UASC2016, Children looked after at 31 March 2016 who were unaccompanied asylum seekers during the year [3]
CLA_UASC2017, Children looked after at 31 March 2017 who were unaccompanied asylum seekers during the year [3]
CLA_UASC2018, Children looked after at 31 March 2018 who were unaccompanied asylum seekers during the year [3]
CLA_UASC2019, Children looked after at 31 March 2019 who were unaccompanied asylum seekers during the year [3]
CLA_UASC2015_pc, Percentage of children looked after at 31 March 2015 who were unaccompanied asylum seekers during the year
CLA_UASC2016_pc, Percentage of children looked after at 31 March 2016 who were unaccompanied asylum seekers during the year
CLA_UASC2017_pc, Percentage of children looked after at 31 March 2017 who were unaccompanied asylum seekers during the year
CLA_UASC2018_pc, Percentage of children looked after at 31 March 2018 who were unaccompanied asylum seekers during the year
CLA_UASC2019_pc, Percentage of children looked after at 31 March 2019 who were unaccompanied asylum seekers during the year
CLA_OwnP, Children looked after at 31 March 2019 where placement is provided by the local authority?s own provision
CLA_OthLA, Children looked after at 31 March 2019 under where placement is provided by another local authority
CLA_OthPP, Children looked after at 31 March 2019 where placement is provided by other public provision
CLA_Priv, Children looked after at 31 March 2019 where placement is provided by private provision
CLA_Vol, Children looked after at 31 March 2019 where placement is provided by voluntary provision
CLA_Par, Children looked after at 31 March 2019 where placement is provided by parents [10]
CLA_Nrep, Children looked after at 31 March 2019 where placement provider is not reported [11]
CLA_OwnP_pc, Percentage of children looked after at 31 March 2019 where placement is provided by the local authority?s own provision
CLA_OthLA_pc, Percentage of children looked after at 31 March 2019 where placement is provided by another local authority
CLA_OthPP_pc, Percentage of children looked after at 31 March 2019 where placement is provided by other public provision
CLA_Priv_pc, Percentage of children looked after at 31 March 2019 where placement is provided by private provision
CLA_Vol_pc, Percentage of children looked after at 31 March 2019 where placement is provided by voluntary provision
CLA_Par_pc, Percentage of children looked after at 31 March 2019 where placement is provided by parents [10]
CLA_Nrep_pc, Percentage of children looked after at 31 March 2019 where placement provider is not reported [11]
CLA_LTE20, Children looked after at 31 March 2019 placed within 20 miles of the child?s home
CLA_GT20, Children looked after at 31 March 2019 placed over 20 miles from the child?s home
CLA_NoInfo, Children looked after at 31 March 2019 where the distance from the child?s home is not known or not recorded [12]
CLA_InLA, Children looked after at 31 March 2019 placed within the local authority boundary [13]
CLA_OutLA, Children looked after at 31 March 2019 placed outside of the local authority boundary [13]
CLA_InLA_LTE20, Children looked after at 31 March 2019 placed within the local authority boundary and within 20 miles of the child?s home [13]
CLA_OutLA_LTE20, Children looked after at 31 March 2019 placed outside of the local authority boundary and within 20 miles of the child?s home [13]
CLA_InLA_GT20, Children looked after at 31 March 2019 placed within the local authority boundary and over 20 miles from the child?s home [13]
CLA_OutLA_GT20, Children looked after at 31 March 2019 placed outside of the local authority boundary and over 20 miles from the child?s home [13]
CLA_InLA_NoInfo, Children looked after at 31 March 2019 placed within the local authority boundary where the distance from the child?s home is not known or not recorded [12 13]
CLA_OutLA_NoInfo, Children looked after at 31 March 2019 placed outside of the local authority boundary where the distance from the child?s home is not known or not recorded [12 13]
CLA_LAPl, Children looked after at 31 March 2019 who were the responsibility of all local authorities placed within the local authority boundary
CLA_IntPl, Children looked after at 31 March 2019 who were the responsibility of the internal local authority placed internally within the local authority boundary [14]
CLA_ExtPl, Children looked after at 31 March 2019 who were the responsibility of an external local authority placed within the local authority boundary [14]
CLA_NetGain, Children looked after at 31 March 2019 ? net gain of children by a local authority [14]
CLA_LTE20_pc, Percentage of children looked after at 31 March 2019 placed within 20 miles of the child?s home
CLA_GT20_pc, Percentage of children looked after at 31 March 2019 placed over 20 miles from the child?s home
CLA_NoInfo_pc, Percentage of children looked after at 31 March 2019 where the distance from the child?s home is not known or not recorded [12]
CLA_InLA_pc, Percentage of children looked after at 31 March 2019 placed within the local authority boundary [13]
CLA_OutLA_pc, Percentage of children looked after at 31 March 2019 placed outside of the local authority boundary [13]
CLA_InLA_LTE20_pc, Percentage of children looked after at 31 March 2019 placed within the local authority boundary and within 20 miles of the child?s home [13]
CLA_OutLA_LTE20_pc, Percentage of children looked after at 31 March 2019 placed outside of the local authority boundary and within 20 miles of the child?s home [13]
CLA_InLA_GT20_pc, Percentage of children looked after at 31 March 2019 placed within the local authority boundary and over 20 miles from the child?s home [13]
CLA_OutLA_GT20_pc, Percentage of children looked after at 31 March 2019 placed outside of the local authority boundary and over 20 miles from the child?s home [13]
CLA_InLA_NoInfo_pc, Percentage of children looked after at 31 March 2019 placed within the local authority boundary where the distance from the child?s home is not known or not recorded [12 13]
CLA_OutLA_NoInfo_pc, Percentage of children looked after at 31 March 2019 placed outside of the local authority boundary where the distance from the child?s home is not known or not recorded [12 13]
CLA_IntPl_pc, Percentage of children placed within the local authority boundary at 31 March 2019 who were the responsibility of the internal local authority [14]
CLA_ExtPl_pc, Percentage of children placed within the local authority boundary at 31 March 2019 who were the responsibility of an external local authority [14]
CLA_3_Pl, Children looked after at 31 March 2019 with three or more placements during the year
CLA_3_Pl_pc, Percentage of children looked after at 31 March 2019 with three or more placements during the year
CLA_2andhalfyears, Children looked after continuously for at least 2.5 years at 31 March 2019 aged under 16
CLA_SamePl_2years, Children looked after at 31 March 2019 aged under 16 who had been looked after continuously for at least 2.5 years who were living in the same placement for at least 2 years or are placed for adoption and their adoption and their adoptive placement together with their previous placement last for at least 2 years
CLA_SamePl_2years_pc, Percentage of children looked after continuously for at least 2.5 years at 31 March 2019 aged under 16 who were living in the same placement for at least 2 years or are placed for adoption and their adoption and their adoptive placement together with their previous placement last for at least 2 years
CLA_NewPl_All_Place, Placements that started during the year ending 31 March 2019
CLA_NewPl_InLA_LTE20, Placements that started during the year ending 31 March 2019 within the local authority boundary and within 20 miles of the child?s home [13]
CLA_NewPl_OutLA_LTE20, Placements that started during the year ending 31 March 2019 placed outside of the local authority boundary and within 20 miles of the child?s home [13]
CLA_NewPl_InLA_GT20, Placements that started during the year ending 31 March 2019 placed within the local authority boundary and over 20 miles from the child?s home [13]
CLA_NewPl_OutLA_GT20, Placements that started during the year ending 31 March 2019 placed outside of the local authority boundary and over 20 miles from the child?s home [13]
CLA_NewPl_NoInfo, Placements that started during the year ending 31 March 2019 where the distance from the child?s home is not known or not recorded [12]
CLA_NewPl_InLA_LTE20_pc, Percentage of placements that started during the year ending 31 March 2019 placed within the local authority boundary and within 20 miles of the child?s home [13]
CLA_NewPl_OutLA_LTE20_pc, Percentage of placements that started during the year ending 31 March 2019 placed outside of the local authority boundary and within 20 miles of the child?s home [13]
CLA_NewPl_InLA_GT20_pc, Percentage of placements that started during the year ending 31 March 2019 placed within the local authority boundary and over 20 miles from the child?s home [13]
CLA_NewPl_OutLA_GT20_pc, Percentage of placements that started during the year ending 31 March 2019 placed outside of the local authority boundary and over 20 miles from the child?s home [13]
CLA_NewPl_NoInfo_pc, Percentage of placements that started during the year ending 31 March 2019 placed within the local authority boundary where the distance from the child?s home is not known or not recorded [12]
CLA_RPC_All_Place, Placements that ended during the year ending 31 March 2019
CLA_RPC_CarePlan, Placements that ended during the year ending 31 March 2019 due to a change to/implementation of care plan
CLA_RPC_Resign, Placements that ended during the year ending 31 March 2019 due to resignation or closure of provision
CLA_RPC_Alleg, Placements that ended during the year ending 31 March 2019 due to allegation
CLA_RPC_Standards, Placements that ended during the year ending 31 March 2019 due to standards of care concern
CLA_RPC_Approval, Placements that ended during the year ending 31 March 2019 due to removal of approval
CLA_RPC_CarerBehav, Placements that ended during the year ending 31 March 2019 due to the carer requesting the placement end because of the child?s behaviour
CLA_RPC_CarerOther, Placements that ended during the year ending 31 March 2019 due to the carer requesting the placement end for other reasons
CLA_RPC_Custody, Placements that ended during the year ending 31 March 2019 because the child has been admitted into custody
CLA_RPC_Child, Placements that ended during the year ending 31 March 2019 due to the child requesting the placement end
CLA_RPC_Auth, Placements that ended during the year ending 31 March 2019 due to the responsible area/authority requesting the placement end
CLA_RPC_Status, Placements that ended during the year ending 31 March 2019 due to the change in the status of a placement only
CLA_RPC_Other, Placements that ended during the year ending 31 March 2019 due to other reasons
CLA_RPC_CarePlan_pc, Percentage of placements that ended during the year ending 31 March 2019 due to a change to/implementation of care plan
CLA_RPC_Resign_pc, Percentage of placements that ended during the year ending 31 March 2019 due to resignation or closure of provision
CLA_RPC_Alleg_pc, Percentage of placements that ended during the year ending 31 March 2019 due to allegation
CLA_RPC_Standards_pc, Percentage of placements that ended during the year ending 31 March 2019 due to standards of care concern
CLA_RPC_Approval_pc, Percentage of placements that ended during the year ending 31 March 2019 due to removal of approval
CLA_RPC_CarerBehav_pc, Percentage of placements that ended during the year ending 31 March 2019 due to the carer requesting the placement end because of the child?s behaviour
CLA_RPC_CarerOther_pc, Percentage of placements that ended during the year ending 31 March 2019 due to the carer requesting the placement end for other reasons
CLA_RPC_Custody_pc, Percentage of Placements that ended during the year ending 31 March 2019 because the child has been admitted into custody
CLA_RPC_Child_pc, Percentage of placements that ended during the year ending 31 March 2019 due to the child requesting the placement end
CLA_RPC_Auth_pc, Percentage of placements that ended during the year ending 31 March 2019 due to the responsible area/authority requesting the placement end
CLA_RPC_Status_pc, Percentage of placements that ended during the year ending 31 March 2019 due to the change in the status of a placement only
CLA_RPC_Other_pc, Percentage of placements that ended during the year ending 31 March 2019 due to other reasons
CLA_started2015, Children who started to be looked after during the year ending 31 March 2015 [1]
CLA_started2016, Children who started to be looked after during the year ending 31 March 2016 [1]
CLA_started2017, Children who started to be looked after during the year ending 31 March 2017 [1]
CLA_started2018, Children who started to be looked after during the year ending 31 March 2018 [1]
CLA_started2019, Children who started to be looked after during the year ending 31 March 2019 [1]
CLA_taken2019, Children who were taken into care during the year ending 31 March 2019 [2]
CLA_taken2019_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 who were taken into care [2]
SCLA_male, Male children who started to be looked after during the year ending 31 March 2019
SCLA_female, Female children who started to be looked after during the year ending 31 March 2019
SCLA_male_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 who were male
SCLA_female_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 who were female
SCLA_U1, Children who started to be looked after during the year ending 31 March 2019 aged under 1
SCLA_1to4, Children who started to be looked after during the year ending 31 March 2019 aged 1 to 4
SCLA_5to9, Children who started to be looked after during the year ending 31 March 2019 aged 5 to 9
SCLA_10to15, Children who started to be looked after during the year ending 31 March 2019 aged 10 to 15
SCLA_16over, Children who started to be looked after during the year ending 31 March 2019 aged 16 and over [3]
SCLA_U1_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 aged under 1
SCLA_1to4_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 aged 1 to 4
SCLA_5to9_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 aged 5 to 9
SCLA_10to15_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 aged 10 to 15
SCLA_16over_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 aged 16 and over [3]
SCLA_AbNeg, Children who started to be looked after during the year ending 31 March 2019 due to abuse or neglect
SCLA_Cdisab, Children who started to be looked after during the year ending 31 March 2019 due to the child?s disability
SCLA_ParIll, Children who started to be looked after during the year ending 31 March 2019 due to parental illness or disability
SCLA_FAcSt, Children who started to be looked after during the year ending 31 March 2019 due to family in acute stress
SCLA_FD, Children who started to be looked after during the year ending 31 March 2019 due to family dysfunction
SCLA_SUB, Children who started to be looked after during the year ending 31 March 2019 due to socially unacceptable behaviour
SCLA_LI, Children who started to be looked after during the year ending 31 March 2019 due to low income
SCLA_AbsPar, Children who started to be looked after during the year ending 31 March 2019 due to absent parenting [3]
SCLA_AbNeg_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 due to abuse or neglect
SCLA_Cdisab_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 due to the child?s disability
SCLA_ParIll_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 due to parental illness or disability
SCLA_FAcSt_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 due to family in acute stress
SCLA_FD_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 due to family dysfunction
SCLA_SUB_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 due to socially unacceptable behaviour
SCLA_LI_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 due to low income
SCLA_AbsPar_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 due to absent parenting [3]
SCLA_ICO, Children who started to be looked after during the year ending 31 March 2019 under an interim care order
SCLA_FCO, Children who started to be looked after during the year ending 31 March 2019 under a full care order
SCLA_PlaceO, Children who started to be looked after during the year ending 31 March 2019 where a placement order has been granted [4]
SCLA_S20, Children who started to be looked after during the year ending 31 March 2019 under section 20 [3]
SCLA_UPP, Children who started to be looked after during the year ending 31 March 2019 under police protection
SCLA_SEPO, Children who started to be looked after during the year ending 31 March 2019 subject to an emergency protection order
SCLA_UCAO, Children who started to be looked after during the year ending 31 March 2019 under a child assessment order
SCLA_ONCT, Children who started to be looked after during the year ending 31 March 2019 on remand or committed for trial
SCLA_PACE, Children who started to be looked after during the year ending 31 March 2019 under the police and criminal evidence act
SCLA_SORR, Children who started to be looked after during the year ending 31 March 2019 under a supervision order with residence requirement
SCLA_ICO_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 under an interim care order
SCLA_FCO_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 under a full care order
SCLA_PlaceO_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 where a placement order has been granted [4]
SCLA_S20_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 under section 20 [3]
SCLA_UPP_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 under police protection
SCLA_SEPO_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 subject to an emergency protection order
SCLA_UCAO_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 under a child assessment order
SCLA_ONCT_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 on remand or committed for trial
SCLA_PACE_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 under the police and criminal evidence act
SCLA_SORR_pc, Percentage of children who started to be looked after during the year ending 31 March 2019 under a supervision order with residence requirement
Started_CLA_Rate2015, Rate of children who started to be looked after per 10000 during the year ending 31 March 2015 [5]
Started_CLA_Rate2016, Rate of children who started to be looked after per 10000 during the year ending 31 March 2016 [5]
Started_CLA_Rate2017, Rate of children who started to be looked after per 10000 during the year ending 31 March 2017 [5]
Started_CLA_Rate2018, Rate of children who started to be looked after per 10000 during the year ending 31 March 2018 [5]
Started_CLA_Rate2019, Rate of children who started to be looked after per 10000 during the year ending 31 March 2019 [5]
CLA_cease2015, Children who ceased to be looked after during the year ending 31 March 2015 [1]
CLA_cease2016, Children who ceased to be looked after during the year ending 31 March 2016 [1]
CLA_cease2017, Children who ceased to be looked after during the year ending 31 March 2017 [1]
CLA_cease2018, Children who ceased to be looked after during the year ending 31 March 2018 [1]
CLA_cease2019, Children who ceased to be looked after during the year ending 31 March 2019 [1]
CEA_male, Male children who ceased to be looked after during the year ending 31 March 2019
CEA_female, Female children who ceased to be looked after during the year ending 31 March 2019
CEA_male_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 who were male
CEA_female_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 who were female
CEA_U1, Children who ceased to be looked after during the year ending 31 March 2019 aged under 1
CEA_1to4, Children who ceased to be looked after during the year ending 31 March 2019 aged 1 to 4
CEA_5to9, Children who ceased to be looked after during the year ending 31 March 2019 aged 5 to 9
CEA_10to15, Children who ceased to be looked after during the year ending 31 March 2019 aged 10 to 15
CEA_16, Children who ceased to be looked after during the year ending 31 March 2019 aged 16
CEA_17, Children who ceased to be looked after during the year ending 31 March 2019 aged 17
CEA_16over, Children who ceased to be looked after during the year ending 31 March 2019 aged 16 and over [2]
CEA_18over, Children who ceased to be looked after during the year ending 31 March 2019 aged 18 and over [2]
CEA_U1_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 aged under 1
CEA_1to4_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 aged 1 to 4
CEA_5to9_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 aged 5 to 9
CEA_10to15_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 aged 10 to 15
CEA_16_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 aged 16
CEA_17_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 aged 17
CEA_16over_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 aged 16 and over [2]
CEA_18over_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 aged 18 and over [2]
CEA_Adop1, Children who ceased to be looked after during the year ending 31 March 2019 due to adoption - application unopposed
CEA_Adop2, Children who ceased to be looked after during the year ending 31 March 2019 due to adoption - consent dispensed with
CEA_Died, Children who ceased to be looked after during the year ending 31 March 2019 as they have died
CEA_Taken, Children who ceased to be looked after during the year ending 31 March 2019 - care taken by another LA [2]
CEA_ParPlan, Children who ceased to be looked after during the year ending 31 March 2019 - returned home to live with parents or relatives as part of care planning process
CEA_ParNPlan, Children who ceased to be looked after during the year ending 31 March 2019 - returned home to live with parents or relatives not as part of care planning process
CEA_NoPar, Children who ceased to be looked after during the year ending 31 March 2019 - ceased to live with parents or relatives without parental responsibility
CEA_CAO, Children who ceased to be looked after during the year ending 31 March 2019 - child arrangement order granted
CEA_SGO_FFC_Rel, Children who ceased to be looked after during the year ending 31 March 2019 - special guardianship order made to former foster carer(s) who was/are a relative(s) or friend(s)
CEA_SGO_FFC_Oth, Children who ceased to be looked after during the year ending 31 March 2019 - special guardianship order made to former foster carer(s) other than relative(s) or friend(s)
CEA_SGO_Car_Rel, Children who ceased to be looked after during the year ending 31 March 2019 - special guardianship order made to carer(s) other than former foster carer(s) who was/are a relative(s) or friend(s)
CEA_SGO_Car_Oth, Children who ceased to be looked after during the year ending 31 March 2019 - special guardianship order made to carer(s) other than former foster carer(s) other than relative(s) or friend(s)
CEA_IndLiv1, Children who ceased to be looked after during the year ending 31 March 2019 - moved into independent living (with supportive accommodation) [2]
CEA_IndLiv2, Children who ceased to be looked after during the year ending 31 March 2019 - moved into independent living (with no formalised support) [2]
CEA_Residential, Children who ceased to be looked after during the year ending 31 March 2019 - transferred to residential care funded by adult social services
CEA_Custody, Children who ceased to be looked after during the year ending 31 March 2019 - sentenced to custody
CEA_RemEnd, Children who ceased to be looked after during the year ending 31 March 2019 - accommodation on remand ended
CEA_AgeAssmt, Children who ceased to be looked after during the year ending 31 March 2019 - age assessment determined child was 18 or over [2]
CEA_Abroad, Children who ceased to be looked after during the year ending 31 March 2019 - child moved abroad
CEA_Other, Children who ceased to be looked after during the year ending 31 March 2019 - care ceased for any other reason [3]
CEA_Adop_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 due to adoption
CEA_Died_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 as they have died 
CEA_Taken_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - care taken by another LA [2]
CEA_ParPlan_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - returned home to live with parents or relatives as part of care planning process
CEA_ParNPlan_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - returned home to live with parents or relatives not as part of care planning process
CEA_NoPar_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - ceased to live with parents or relatives without parental responsibility
CEA_CAO_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - child arrangement order granted
CEA_SGO_FFC_Rel_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - special guardianship order made to former foster carer(s) who was/are a relative(s) or friend(s)
CEA_SGO_FFC_Oth_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - special guardianship order made to former foster carer(s) other than relative(s) or friend(s)
CEA_SGO_Car_Rel_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - special guardianship order made to carer(s) other than former foster carer(s) who was/are a relative(s) or friend(s) 
CEA_SGO_Car_Oth_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - special guardianship order made to carer(s) other than former foster carer(s) other than relative(s) or friend(s)
CEA_IndLiv1_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - moved into independent living (with supportive accommodation) [2]
CEA_IndLiv2_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - moved into independent living (with no formalised support) [2]
CEA_Residential_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - transferred to residential care funded by adult social services
CEA_Custody_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - sentenced to custody
CEA_RemEnd_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - accommodation on remand ended 
CEA_AgeAssmt_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - age assessment determined child was 18 or over [2] 
CEA_Abroad_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - child moved abroad
CEA_Other_pc, Percentage of children who ceased to be looked after during the year ending 31 March 2019 - care ceased for any other reason [3]
Ceased_CLA_Rate2015, Rate of children who ceased to be looked after per 10000 during the year ending 31 March 2015 [4]  
Ceased_CLA_Rate2016,  Rate of children who ceased to be looked after per 10000 during the year ending 31 March 2016 [4]   
Ceased_CLA_Rate2017, Rate of children who ceased to be looked after per 10000 during the year ending 31 March 2017 [4]  
Ceased_CLA_Rate2018, Rate of children who ceased to be looked after per 10000 during the year ending 31 March 2018 [4]  
Ceased_CLA_Rate2019, Rate of children who ceased to be looked after per 10000 during the year ending 31 March 2019 [4]  
CL_All_17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1 2]
CL_All_17, Care leavers in the year ending 31 March 2019 aged 17 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1 2]
CL_All_18, Care leavers in the year ending 31 March 2019 aged 18 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1 2]
CL_Act_EET17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 who are in education employment or training
CL_Act_EET17, Care leavers in the year ending 31 March 2019 aged 17 who are in education employment or training
CL_Act_EET18, Care leavers in the year ending 31 March 2019 aged 18 who are in education employment or training
CL_Act_HE17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 whose activity is higher education i.e. studies beyond A level
CL_Act_HE17, Care leavers in the year ending 31 March 2019 aged 17 whose activity is higher education i.e. studies beyond A level
CL_Act_HE18, Care leavers in the year ending 31 March 2019 aged 18 whose activity is higher education i.e. studies beyond A level
CL_Act_OE17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 whose activity is education other than higher education
CL_Act_OE17, Care leavers in the year ending 31 March 2019 aged 17 whose activity is education other than higher education
CL_Act_OE18, Care leavers in the year ending 31 March 2019 aged 18 whose activity is education other than higher education
CL_Act_TE17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 whose activity is training or employment
CL_Act_TE17, Care leavers in the year ending 31 March 2019 aged 17 whose activity is training or employment
CL_Act_TE18, Care leavers in the year ending 31 March 2019 aged 18 whose activity is training or employment
CL_Act_NEET17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 who are not in education employment or training
CL_Act_NEET17, Care leavers in the year ending 31 March 2019 aged 17 who are not in education employment or training
CL_Act_NEET18, Care leavers in the year ending 31 March 2019 aged 18 who are not in education employment or training
CL_Act_NEET_ill17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill17, Care leavers in the year ending 31 March 2019 aged 17 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill18, Care leavers in the year ending 31 March 2019 aged 18 who are not in education training or employment due to illness or disability
CL_Act_NEET_oth17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 who are not in education training or employment due to other reasons
CL_Act_NEET_oth17, Care leavers in the year ending 31 March 2019 aged 17 who are not in education training or employment due to other reasons
CL_Act_NEET_oth18, Care leavers in the year ending 31 March 2019 aged 18 who are not in education training or employment due to other reasons
CL_Act_NEET_preg17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg17, Care leavers in the year ending 31 March 2019 aged 17 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg18, Care leavers in the year ending 31 March 2019 aged 18 who are not in education training or employment due to pregnancy or parenting
CL_Act_NoInf17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 for whom local authority does not have activity information
CL_Act_NoInf17, Care leavers in the year ending 31 March 2019 aged 17 for whom local authority does not have activity information
CL_Act_NoInf18, Care leavers in the year ending 31 March 2019 aged 18 for whom local authority does not have activity information
CL_Act_EET17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 who are in education employment or training
CL_Act_EET17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 who are in education employment or training
CL_Act_EET18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 who are in education employment or training
CL_Act_HE17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 whose activity is higher education i.e. studies beyond A level
CL_Act_HE17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 whose activity is higher education i.e. studies beyond A level
CL_Act_HE18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 whose activity is higher education i.e. studies beyond A level
CL_Act_OE17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 whose activity is education other than higher education
CL_Act_OE17_pc, Ca Percentage of care re leavers in the year ending 31 March 2019 aged 17 whose activity is education other than higher education
CL_Act_OE18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 whose activity is education other than higher education
CL_Act_TE17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 whose activity is training or employment
CL_Act_TE17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 whose activity is training or employment
CL_Act_TE18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 whose activity is training or employment
CL_Act_NEET17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 who are not in education employment or training
CL_Act_NEET17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 who are not in education employment or training
CL_Act_NEET18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 who are not in education employment or training
CL_Act_NEET_ill17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 who are not in education training or employment due to illness or disability
CL_Act_NEET_oth17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 who are not in education training or employment due to other reasons
CL_Act_NEET_oth17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 who are not in education training or employment due to other reasons
CL_Act_NEET_oth18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 who are not in education training or employment due to other reasons
CL_Act_NEET_preg17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 who are not in education training or employment due to pregnancy or parenting
CL_Act_NoInf17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 for whom local authority does not have activity information
CL_Act_NoInf17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 for whom local authority does not have activity information
CL_Act_NoInf18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 for whom local authority does not have activity information
CL_Acc_Total_SUIT17&18, Care leavers in the year ending 31 March 2019 aged 17 to 18 in suitable accommodation calculations [3 4]
CL_Acc_Total_SUIT17, Care leavers in the year ending 31 March 2019 aged 17 in suitable accommodation calculations [3 4]
CL_Acc_Total_SUIT18, Care leavers in the year ending 31 March 2019 aged 18 in suitable accommodation calculations [3 4]
CL_Acc_SUIT17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 in suitable accommodation [3 4]
CL_Acc_SUIT17, Care leavers in the year ending 31 March 2019 aged 17 in suitable accommodation [3 4]
CL_Acc_SUIT18, Care leavers in the year ending 31 March 2019 aged 18 in suitable accommodation [3 4]
CL_Acc_NotSUIT17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 not in suitable accommodation [3 4]
CL_Acc_NotSUIT17, Care leavers in the year ending 31 March 2019 aged 17 not in suitable accommodation [3 4]
CL_Acc_NotSUIT18, Care leavers in the year ending 31 March 2019 aged 18 not in suitable accommodation [3 4]
CL_Acc_NoSUITInfo17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 for whom suitability of accommodation is not available [3 5]
CL_Acc_NoSUITInfo17, Care leavers in the year ending 31 March 2019 aged 17 for whom suitability of accommodation is not available [3 5]
CL_Acc_NoSUITInfo18, Care leavers in the year ending 31 March 2019 aged 18 for whom suitability of accommodation is not available [3 5]
CL_Acc_SUIT17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 in suitable accommodation [3 4]
CL_Acc_SUIT17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 in suitable accommodation [3 4]
CL_Acc_SUIT18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 in suitable accommodation [3 4]
CL_Acc_NotSUIT17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 not in suitable accommodation [3 4]
CL_Acc_NotSUIT17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 not in suitable accommodation [3 4]
CL_Acc_NotSUIT18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 not in suitable accommodation [3 4]
CL_Acc_NoSUITInfo17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 for whom suitability of accommodation is not available [3 5]
CL_Acc_NoSUITInfo17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 for whom suitability of accommodation is not available [3 5]
CL_Acc_NoSUITInfo18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 for whom suitability of accommodation is not available [3 5]
CL_Acc_P17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated with parents or relatives [6]
CL_Acc_P17, Care leavers in the year ending 31 March 2019 aged 17 accommodated with parents or relatives [6]
CL_Acc_P18, Care leavers in the year ending 31 March 2019 aged 18 accommodated with parents or relatives [6]
CL_Acc_CH17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated in community homes
CL_Acc_CH17, Care leavers in the year ending 31 March 2019 aged 17 accommodated in community homes
CL_Acc_CH18, Care leavers in the year ending 31 March 2019 aged 18 accommodated in community homes
CL_Acc_SITA17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated in semi-independent transitional accommodation
CL_Acc_SITA17, Care leavers in the year ending 31 March 2019 aged 17 accommodated in semi-independent transitional accommodation
CL_Acc_SITA18, Care leavers in the year ending 31 March 2019 aged 18 accommodated in semi-independent transitional accommodation
CL_Acc_SL17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated in supported lodgings
CL_Acc_SL17, Care leavers in the year ending 31 March 2019 aged 17 accommodated in supported lodgings
CL_Acc_SL18, Care leavers in the year ending 31 March 2019 aged 18 accommodated in supported lodgings
CL_Acc_GA17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 who have gone abroad
CL_Acc_GA17, Care leavers in the year ending 31 March 2019 aged 17 who have gone abroad
CL_Acc_GA18, Care leavers in the year ending 31 March 2019 aged 18 who have gone abroad
CL_Acc_Dep17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 who have been deported
CL_Acc_Dep17, Care leavers in the year ending 31 March 2019 aged 17 who have been deported
CL_Acc_Dep18, Care leavers in the year ending 31 March 2019 aged 18 who have been deported
CL_Acc_OL17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated in ordinary lodgings
CL_Acc_OL17, Care leavers in the year ending 31 March 2019 aged 17 accommodated in ordinary lodgings
CL_Acc_OL18, Care leavers in the year ending 31 March 2019 aged 18 accommodated in ordinary lodgings
CL_Acc_NK17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 with residence not known
CL_Acc_NK17, Care leavers in the year ending 31 March 2019 aged 17 with residence not known
CL_Acc_NK18, Care leavers in the year ending 31 March 2019 aged 18 with residence not known
CL_Acc_NFA17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 with no fixed abode or homeless
CL_Acc_NFA17, Care leavers in the year ending 31 March 2019 aged 17 with no fixed abode or homeless
CL_Acc_NFA18, Care leavers in the year ending 31 March 2019 aged 18 with no fixed abode or homeless
CL_Acc_F17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated in foyers
CL_Acc_F17, Care leavers in the year ending 31 March 2019 aged 17 accommodated in foyers
CL_Acc_F18, Care leavers in the year ending 31 March 2019 aged 18 accommodated in foyers
CL_Acc_IL17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated in independent living
CL_Acc_IL17, Care leavers in the year ending 31 March 2019 aged 17 accommodated in independent living
CL_Acc_IL18, Care leavers in the year ending 31 March 2019 aged 18 accommodated in independent living
CL_Acc_EA17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated in emergency accommodation
CL_Acc_EA17, Care leavers in the year ending 31 March 2019 aged 17 accommodated in emergency accommodation
CL_Acc_EA18, Care leavers in the year ending 31 March 2019 aged 18 accommodated in emergency accommodation
CL_Acc_BB17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 in bed and breakfast accommodation
CL_Acc_BB17, Care leavers in the year ending 31 March 2019 aged 17 in bed and breakfast accommodation
CL_Acc_BB18, Care leavers in the year ending 31 March 2019 aged 18 in bed and breakfast accommodation
CL_Acc_Cust17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 who are in custody
CL_Acc_Cust17, Care leavers in the year ending 31 March 2019 aged 17 who are in custody
CL_Acc_Cust18, Care leavers in the year ending 31 March 2019 aged 18 who are in custody
CL_Acc_FFC17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated with former foster carers
CL_Acc_FFC17, Care leavers in the year ending 31 March 2019 aged 17 accommodated with former foster carers
CL_Acc_FFC18, Care leavers in the year ending 31 March 2019 aged 18 accommodated with former foster carers
CL_Acc_OTH17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 in other accommodation
CL_Acc_OTH17, Care leavers in the year ending 31 March 2019 aged 17 in other accommodation
CL_Acc_OTH18, Care leavers in the year ending 31 March 2019 aged 18 in other accommodation
CL_Acc_NoInf17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 for whom local authority does not have accommodation information [7]
CL_Acc_NoInf17, Care leavers in the year ending 31 March 2019 aged 17 for whom local authority does not have accommodation information [7]
CL_Acc_NoInf18, Care leavers in the year ending 31 March 2019 aged 18 for whom local authority does not have accommodation information [7]
CL_Acc_P17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated with parents or relatives [6]
CL_Acc_P17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 accommodated with parents or relatives [6]
CL_Acc_P18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 accommodated with parents or relatives [6]
CL_Acc_CH17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated in community homes
CL_Acc_CH17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 accommodated in community homes
CL_Acc_CH18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 accommodated in community homes
CL_Acc_SITA17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated in semi-independent transitional accommodation
CL_Acc_SITA17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 accommodated in semi-independent transitional accommodation
CL_Acc_SITA18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 accommodated in semi-independent transitional accommodation
CL_Acc_SL17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated in supported lodgings
CL_Acc_SL17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 accommodated in supported lodgings
CL_Acc_SL18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 accommodated in supported lodgings
CL_Acc_GA17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 who have gone abroad
CL_Acc_GA17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 who have gone abroad
CL_Acc_GA18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 who have gone abroad
CL_Acc_Dep17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 who have been deported
CL_Acc_Dep17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 who have been deported
CL_Acc_Dep18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 who have been deported
CL_Acc_OL17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated in ordinary lodgings
CL_Acc_OL17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 accommodated in ordinary lodgings
CL_Acc_OL18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 accommodated in ordinary lodgings
CL_Acc_NK17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 with residence not known
CL_Acc_NK17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 with residence not known
CL_Acc_NK18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 with residence not known
CL_Acc_NFA17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 with no fixed abode or homeless
CL_Acc_NFA17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 with no fixed abode or homeless
CL_Acc_NFA18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 with no fixed abode or homeless
CL_Acc_F17&18_pc, C Percentage of care are leavers in the year ending 31 March 2019 aged 17 and 18 accommodated in foyers
CL_Acc_F17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 accommodated in foyers
CL_Acc_F18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 accommodated in foyers
CL_Acc_IL17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated in independent living
CL_Acc_IL17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 accommodated in independent living
CL_Acc_IL18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 accommodated in independent living
CL_Acc_EA17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated in emergency accommodation
CL_Acc_EA17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 accommodated in emergency accommodation
CL_Acc_EA18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 accommodated in emergency accommodation
CL_Acc_BB17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 in bed and breakfast accommodation
CL_Acc_BB17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 in bed and breakfast accommodation
CL_Acc_BB18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 in bed and breakfast accommodation
CL_Acc_Cust17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 who are in custody
CL_Acc_Cust17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 who are in custody
CL_Acc_Cust18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 who are in custody
CL_Acc_FFC17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 accommodated with former foster carers
CL_Acc_FFC17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 accommodated with former foster carers
CL_Acc_FFC18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 accommodated with former foster carers
CL_Acc_OTH17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 in other accommodation
CL_Acc_OTH17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 in other accommodation
CL_Acc_OTH18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 in other accommodation
CL_Acc_NoInf17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 for whom local authority does not have accommodation information [7]
CL_Acc_NoInf17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 for whom local authority does not have accommodation information [7]
CL_Acc_NoInf18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 for whom local authority does not have accommodation information [7]
CL_InTouch_IT17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 who are in touch with the local authority
CL_InTouch_IT17, Care leavers in the year ending 31 March 2019 aged 17 who are in touch with the local authority
CL_InTouch_IT18, Care leavers in the year ending 31 March 2019 aged 18 who are in touch with the local authority
CL_InTouch_Refu17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 who refuse contact with the local authority
CL_InTouch_Refu17, Care leavers in the year ending 31 March 2019 aged 17 who refuse contact with the local authority
CL_InTouch_Refu18, Care leavers in the year ending 31 March 2019 aged 18 who refuse contact with the local authority
CL_InTouch_NoServ17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 who no longer require services
CL_InTouch_NoServ17, Care leavers in the year ending 31 March 2019 aged 17 who no longer require services
CL_InTouch_NoServ18, Care leavers in the year ending 31 March 2019 aged 18 who no longer require services
CL_InTouch_Not17&18, Care leavers in the year ending 31 March 2019 aged 17 and 18 who are not in touch with the local authority
CL_InTouch_Not17, Care leavers in the year ending 31 March 2019 aged 17 who are not in touch with the local authority
CL_InTouch_Not18, Care leavers in the year ending 31 March 2019 aged 18 who are not in touch with the local authority
CL_InTouch_IT17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 who are in touch with the local authority
CL_InTouch_IT17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 who are in touch with the local authority
CL_InTouch_IT18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 who are in touch with the local authority
CL_InTouch_Refu17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 who refuse contact with the local authority
CL_InTouch_Refu17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 who refuse contact with the local authority
CL_InTouch_Refu18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 who refuse contact with the local authority
CL_InTouch_NoServ17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 who no longer require services
CL_InTouch_NoServ17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 who no longer require services
CL_InTouch_NoServ18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 who no longer require services
CL_InTouch_Not17&18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 and 18 who are not in touch with the local authority
CL_InTouch_Not17_pc, Percentage of care leavers in the year ending 31 March 2019 aged 17 who are not in touch with the local authority
CL_InTouch_Not18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 who are not in touch with the local authority
CL_StayPut_18, Care leavers in the year ending 31 March 2019 aged 18 who ceased to be looked after from a foster placement on their 18th birthday
CL_StayPut_FFC_18, Care leavers in the year ending 31 March 2019 aged 18 who ceased to be looked after from a foster placement on their 18th birthday and are living with the former foster carers
CL_StayPut_FFC_18_pc, Percentage of care leavers in the year ending 31 March 2019 aged 18 who ceased to be looked after from a foster placement on their 18th birthday who are living with the former foster carers
CL_All_19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_All_19, Care leavers in the year ending 31 March 2019 aged 19 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_All_20, Care leavers in the year ending 31 March 2019 aged 20 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_All_21, Care leavers in the year ending 31 March 2019 aged 21 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_Act_EET19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 who are in education employment or training
CL_Act_EET19, Care leavers in the year ending 31 March 2019 aged 19 who are in education employment or training
CL_Act_EET20, Care leavers in the year ending 31 March 2019 aged 20 who are in education employment or training
CL_Act_EET21, Care leavers in the year ending 31 March 2019 aged 21 who are in education employment or training
CL_Act_HE19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 whose activity is higher education i.e. studies beyond A level
CL_Act_HE19, Care leavers in the year ending 31 March 2019 aged 19 whose activity is higher education i.e. studies beyond A level
CL_Act_HE20, Care leavers in the year ending 31 March 2019 aged 20 whose activity is higher education i.e. studies beyond A level
CL_Act_HE21, Care leavers in the year ending 31 March 2019 aged 21 whose activity is higher education i.e. studies beyond A level
CL_Act_OE19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 whose activity is education other than higher education
CL_Act_OE19, Care leavers in the year ending 31 March 2019 aged 19 whose activity is education other than higher education
CL_Act_OE20, Care leavers in the year ending 31 March 2019 aged 20 whose activity is education other than higher education
CL_Act_OE21, Care leavers in the year ending 31 March 2019 aged 21 whose activity is education other than higher education
CL_Act_TE19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 whose activity is training or employment
CL_Act_TE19, Care leavers in the year ending 31 March 2019 aged 19 whose activity is training or employment
CL_Act_TE20, Care leavers in the year ending 31 March 2019 aged 20 whose activity is training or employment
CL_Act_TE21, Care leavers in the year ending 31 March 2019 aged 21 whose activity is training or employment
CL_Act_NEET19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 who are not in education employment or training
CL_Act_NEET19, Care leavers in the year ending 31 March 2019 aged 19 who are not in education employment or training
CL_Act_NEET20, Care leavers in the year ending 31 March 2019 aged 20 who are not in education employment or training
CL_Act_NEET21, Care leavers in the year ending 31 March 2019 aged 21 who are not in education employment or training
CL_Act_NEET_ill19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill19, Care leavers in the year ending 31 March 2019 aged 19 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill20, Care leavers in the year ending 31 March 2019 aged 20 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill21, Care leavers in the year ending 31 March 2019 aged 21 who are not in education training or employment due to illness or disability
CL_Act_NEET_oth19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 who are not in education training or employment due to other reasons
CL_Act_NEET_oth19, Care leavers in the year ending 31 March 2019 aged 19 who are not in education training or employment due to other reasons
CL_Act_NEET_oth20, Care leavers in the year ending 31 March 2019 aged 20 who are not in education training or employment due to other reasons
CL_Act_NEET_oth21, Care leavers in the year ending 31 March 2019 aged 21 who are not in education training or employment due to other reasons
CL_Act_NEET_preg19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg19, Care leavers in the year ending 31 March 2019 aged 19 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg20, Care leavers in the year ending 31 March 2019 aged 20 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg21, Care leavers in the year ending 31 March 2019 aged 21 who are not in education training or employment due to pregnancy or parenting
CL_Act_NoInf19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 for whom local authority does not have activity information
CL_Act_NoInf19, Care leavers in the year ending 31 March 2019 aged 19 for whom local authority does not have activity information
CL_Act_NoInf20, Care leavers in the year ending 31 March 2019 aged 20 for whom local authority does not have activity information
CL_Act_NoInf21, Care leavers in the year ending 31 March 2019 aged 21 for whom local authority does not have activity information
CL_Act_EET19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 who are in education employment or training
CL_Act_EET19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 who are in education employment or training
CL_Act_EET20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 who are in education employment or training
CL_Act_EET21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 who are in education employment or training
CL_Act_HE19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 whose activity is higher education i.e. studies beyond A level
CL_Act_HE19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 whose activity is higher education i.e. studies beyond A level
CL_Act_HE20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 whose activity is higher education i.e. studies beyond A level
CL_Act_HE21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 whose activity is higher education i.e. studies beyond A level
CL_Act_OE19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 whose activity is education other than higher education
CL_Act_OE19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 whose activity is education other than higher education
CL_Act_OE20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 whose activity is education other than higher education
CL_Act_OE21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 whose activity is education other than higher education
CL_Act_TE19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 whose activity is training or employment
CL_Act_TE19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 whose activity is training or employment
CL_Act_TE20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 whose activity is training or employment
CL_Act_TE21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 whose activity is training or employment
CL_Act_NEET19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 who are not in education employment or training
CL_Act_NEET19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 who are not in education employment or training
CL_Act_NEET20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 who are not in education employment or training
CL_Act_NEET21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 who are not in education employment or training
CL_Act_NEET_ill19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 who are not in education training or employment due to illness or disability
CL_Act_NEET_ill21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 who are not in education training or employment due to illness or disability
CL_Act_NEET_oth19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 who are not in education training or employment due to other reasons
CL_Act_NEET_oth19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 who are not in education training or employment due to other reasons
CL_Act_NEET_oth20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 who are not in education training or employment due to other reasons
CL_Act_NEET_oth21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 who are not in education training or employment due to other reasons
CL_Act_NEET_preg19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 who are not in education training or employment due to pregnancy or parenting
CL_Act_NEET_preg21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 who are not in education training or employment due to pregnancy or parenting
CL_Act_NoInf19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 for whom local authority does not have activity information
CL_Act_NoInf19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 for whom local authority does not have activity information
CL_Act_NoInf20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 for whom local authority does not have activity information
CL_Act_NoInf21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 for whom local authority does not have activity information
CL_InTouch_IT19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 who are in touch with the local authority
CL_InTouch_IT19, Care leavers in the year ending 31 March 2019 aged 19 who are in touch with the local authority
CL_InTouch_IT20, Care leavers in the year ending 31 March 2019 aged 20 who are in touch with the local authority
CL_InTouch_IT21, Care leavers in the year ending 31 March 2019 aged 21 who are in touch with the local authority
CL_InTouch_Refu19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 who refuse contact with the local authority
CL_InTouch_Refu19, Care leavers in the year ending 31 March 2019 aged 19 who refuse contact with the local authority
CL_InTouch_Refu20, Care leavers in the year ending 31 March 2019 aged 20 who refuse contact with the local authority
CL_InTouch_Refu21, Care leavers in the year ending 31 March 2019 aged 21 who refuse contact with the local authority
CL_InTouch_NoServ19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 who no longer require services
CL_InTouch_NoServ19, Care leavers in the year ending 31 March 2019 aged 19 who no longer require services
CL_InTouch_NoServ20, Care leavers in the year ending 31 March 2019 aged 20 who no longer require services
CL_InTouch_NoServ21, Care leavers in the year ending 31 March 2019 aged 21 who no longer require services
CL_InTouch_Not19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 who are not in touch with the local authority
CL_InTouch_Not19, Care leavers in the year ending 31 March 2019 aged 19 who are not in touch with the local authority
CL_InTouch_Not20, Care leavers in the year ending 31 March 2019 aged 20 who are not in touch with the local authority
CL_InTouch_Not21, Care leavers in the year ending 31 March 2019 aged 21 who are not in touch with the local authority
CL_InTouch_IT19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 who are in touch with the local authority
CL_InTouch_IT19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 who are in touch with the local authority
CL_InTouch_IT20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 who are in touch with the local authority
CL_InTouch_IT21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 who are in touch with the local authority
CL_InTouch_Refu19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 who refuse contact with the local authority
CL_InTouch_Refu19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 who refuse contact with the local authority
CL_InTouch_Refu20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 who refuse contact with the local authority
CL_InTouch_Refu21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 who refuse contact with the local authority
CL_InTouch_NoServ19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 who no longer require services
CL_InTouch_NoServ19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 who no longer require services
CL_InTouch_NoServ20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 who no longer require services
CL_InTouch_NoServ21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 who no longer require services
CL_InTouch_Not19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 who are not in touch with the local authority
CL_InTouch_Not19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 who are not in touch with the local authority
CL_InTouch_Not20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 who are not in touch with the local authority
CL_InTouch_Not21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 who are not in touch with the local authority
CL_All_19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_All_19, Care leavers in the year ending 31 March 2019 aged 19 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_All_20, Care leavers in the year ending 31 March 2019 aged 20 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_All_21, Care leavers in the year ending 31 March 2019 aged 21 who were looked after for a total of at least 13 weeks after their 14th birthday including some time after their 16th birthday [1]
CL_Acc_Total_SUIT19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 in suitable accommodation calculations [2 3]
CL_Acc_Total_SUIT19, Care leavers in the year ending 31 March 2019 aged 19 in suitable accommodation calculations [2 3]
CL_Acc_Total_SUIT20, Care leavers in the year ending 31 March 2019 aged 20 in suitable accommodation calculations [2 3]
CL_Acc_Total_SUIT21, Care leavers in the year ending 31 March 2019 aged 21 in suitable accommodation calculations [2 3]
CL_Acc_SUIT19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 in suitable accommodation [2 3]
CL_Acc_SUIT19, Care leavers in the year ending 31 March 2019 aged 19 in suitable accommodation [2 3]
CL_Acc_SUIT20, Care leavers in the year ending 31 March 2019 aged 20 in suitable accommodation [2 3]
CL_Acc_SUIT21, Care leavers in the year ending 31 March 2019 aged 21 in suitable accommodation [2 3]
CL_Acc_NotSUIT19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 not in suitable accommodation [2 3]
CL_Acc_NotSUIT19, Care leavers in the year ending 31 March 2019 aged 19 not in suitable accommodation [2 3]
CL_Acc_NotSUIT20, Care leavers in the year ending 31 March 2019 aged 20 not in suitable accommodation [2 3]
CL_Acc_NotSUIT21, Care leavers in the year ending 31 March 2019 aged 21 not in suitable accommodation [2 3]
CL_Acc_NoSUITInfo19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 for whom suitability of accommodation is not available [2 4]
CL_Acc_NoSUITInfo19, Care leavers in the year ending 31 March 2019 aged 19 for whom suitability of accommodation is not available [2 4]
CL_Acc_NoSUITInfo20, Care leavers in the year ending 31 March 2019 aged 20 for whom suitability of accommodation is not available [2 4]
CL_Acc_NoSUITInfo21, Care leavers in the year ending 31 March 2019 aged 21 for whom suitability of accommodation is not available [2 4]
CL_Acc_SUIT19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 in suitable accommodation [2 3]
CL_Acc_SUIT19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 in suitable accommodation [2 3]
CL_Acc_SUIT20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 in suitable accommodation [2 3]
CL_Acc_SUIT21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 in suitable accommodation [2 3]
CL_Acc_NotSUIT19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 not in suitable accommodation [2 3]
CL_Acc_NotSUIT19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 not in suitable accommodation [2 3]
CL_Acc_NotSUIT20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 not in suitable accommodation [2 3]
CL_Acc_NotSUIT21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 not in suitable accommodation [2 3]
CL_Acc_NoSUITInfo19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 for whom suitability of accommodation is not available [2 4]
CL_Acc_NoSUITInfo19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 for whom suitability of accommodation is not available [2 4]
CL_Acc_NoSUITInfo20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 for whom suitability of accommodation is not available [2 4]
CL_Acc_NoSUITInfo21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 for whom suitability of accommodation is not available [2 4]
CL_Acc_P19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 years old accommodated with parents or relatives [5]
CL_Acc_P19, Care leavers in the year ending 31 March 2019 aged 19 accommodated with parents or relatives [5]
CL_Acc_P20, Care leavers in the year ending 31 March 2019 aged 20 accommodated with parents or relatives [5]
CL_Acc_P21, Care leavers in the year ending 31 March 2019 aged 21 accommodated with parents or relatives [5]
CL_Acc_CH19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated in community homes
CL_Acc_CH19, Care leavers in the year ending 31 March 2019 aged 19 accommodated in community homes
CL_Acc_CH20, Care leavers in the year ending 31 March 2019 aged 20 accommodated in community homes
CL_Acc_CH21, Care leavers in the year ending 31 March 2019 aged 21 accommodated in community homes
CL_Acc_SITA19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated in semi-independent transitional accommodation
CL_Acc_SITA19, Care leavers in the year ending 31 March 2019 aged 19 accommodated in semi-independent transitional accommodation
CL_Acc_SITA20, Care leavers in the year ending 31 March 2019 aged 20 accommodated in semi-independent transitional accommodation
CL_Acc_SITA21, Care leavers in the year ending 31 March 2019 aged 21 accommodated in semi-independent transitional accommodation
CL_Acc_SL19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated in supported lodgings
CL_Acc_SL19, Care leavers in the year ending 31 March 2019 aged 19 accommodated in supported lodgings
CL_Acc_SL20, Care leavers in the year ending 31 March 2019 aged 20 accommodated in supported lodgings
CL_Acc_SL21, Care leavers in the year ending 31 March 2019 aged 21 accommodated in supported lodgings
CL_Acc_GA19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 who have gone abroad
CL_Acc_GA19, Care leavers in the year ending 31 March 2019 aged 19 who have gone abroad
CL_Acc_GA20, Care leavers in the year ending 31 March 2019 aged 20 who have gone abroad
CL_Acc_GA21, Care leavers in the year ending 31 March 2019 aged 21 who have gone abroad
CL_Acc_Dep19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 who have been deported
CL_Acc_Dep19, Care leavers in the year ending 31 March 2019 aged 19 who have been deported
CL_Acc_Dep20, Care leavers in the year ending 31 March 2019 aged 20 who have been deported
CL_Acc_Dep21, Care leavers in the year ending 31 March 2019 aged 21 who have been deported
CL_Acc_OL19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated in ordinary lodgings
CL_Acc_OL19, Care leavers in the year ending 31 March 2019 aged 19 accommodated in ordinary lodgings
CL_Acc_OL20, Care leavers in the year ending 31 March 2019 aged 20 accommodated in ordinary lodgings
CL_Acc_OL21, Care leavers in the year ending 31 March 2019 aged 21 accommodated in ordinary lodgings
CL_Acc_NK19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 with residence not known
CL_Acc_NK19, Care leavers in the year ending 31 March 2019 aged 19 with residence not known
CL_Acc_NK20, Care leavers in the year ending 31 March 2019 aged 20 with residence not known
CL_Acc_NK21, Care leavers in the year ending 31 March 2019 aged 21 with residence not known
CL_Acc_NFA19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 with no fixed abode or homeless
CL_Acc_NFA19, Care leavers in the year ending 31 March 2019 aged 19 with no fixed abode or homeless
CL_Acc_NFA20, Care leavers in the year ending 31 March 2019 aged 20 with no fixed abode or homeless
CL_Acc_NFA21, Care leavers in the year ending 31 March 2019 aged 21 with no fixed abode or homeless
CL_Acc_F19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated in foyers
CL_Acc_F19, Care leavers in the year ending 31 March 2019 aged 19 accommodated in foyers
CL_Acc_F20, Care leavers in the year ending 31 March 2019 aged 20 accommodated in foyers
CL_Acc_F21, Care leavers in the year ending 31 March 2019 aged 21 accommodated in foyers
CL_Acc_IL19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated in independent living
CL_Acc_IL19, Care leavers in the year ending 31 March 2019 aged 19 accommodated in independent living
CL_Acc_IL20, Care leavers in the year ending 31 March 2019 aged 20 accommodated in independent living
CL_Acc_IL21, Care leavers in the year ending 31 March 2019 aged 21 accommodated in independent living
CL_Acc_EA19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated in emergency accommodation
CL_Acc_EA19, Care leavers in the year ending 31 March 2019 aged 19 accommodated in emergency accommodation
CL_Acc_EA20, Care leavers in the year ending 31 March 2019 aged 20 accommodated in emergency accommodation
CL_Acc_EA21, Care leavers in the year ending 31 March 2019 aged 21 accommodated in emergency accommodation
CL_Acc_BB19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 in bed and breakfast accommodation
CL_Acc_BB19, Care leavers in the year ending 31 March 2019 aged 19 in bed and breakfast accommodation
CL_Acc_BB20, Care leavers in the year ending 31 March 2019 aged 20 in bed and breakfast accommodation
CL_Acc_BB21, Care leavers in the year ending 31 March 2019 aged 21 in bed and breakfast accommodation
CL_Acc_Cust19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 who are in custody
CL_Acc_Cust19, Care leavers in the year ending 31 March 2019 aged 19 who are in custody
CL_Acc_Cust20, Care leavers in the year ending 31 March 2019 aged 20 who are in custody
CL_Acc_Cust21, Care leavers in the year ending 31 March 2019 aged 21 who are in custody
CL_Acc_FFC19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated with former foster carers
CL_Acc_FFC19, Care leavers in the year ending 31 March 2019 aged 19 accommodated with former foster carers
CL_Acc_FFC20, Care leavers in the year ending 31 March 2019 aged 20 accommodated with former foster carers
CL_Acc_FFC21, Care leavers in the year ending 31 March 2019 aged 21 accommodated with former foster carers
CL_Acc_OTH19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 in other accommodation
CL_Acc_OTH19, Care leavers in the year ending 31 March 2019 aged 19 in other accommodation
CL_Acc_OTH20, Care leavers in the year ending 31 March 2019 aged 20 in other accommodation
CL_Acc_OTH21, Care leavers in the year ending 31 March 2019 aged 21 in other accommodation
CL_Acc_NoInf19to21, Care leavers in the year ending 31 March 2019 aged 19 to 21 for whom local authority does not have accommodation information [6]
CL_Acc_NoInf19, Care leavers in the year ending 31 March 2019 aged 19 for whom local authority does not have accommodation information [6]
CL_Acc_NoInf20, Care leavers in the year ending 31 March 2019 aged 20 for whom local authority does not have accommodation information [6]
CL_Acc_NoInf21, Care leavers in the year ending 31 March 2019 aged 21 for whom local authority does not have accommodation information [6]
CL_Acc_P19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 years old accommodated with parents or relatives [5]
CL_Acc_P19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 accommodated with parents or relatives [5]
CL_Acc_P20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 accommodated with parents or relatives [5]
CL_Acc_P21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 accommodated with parents or relatives [5]
CL_Acc_CH19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated in community homes
CL_Acc_CH19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 accommodated in community homes
CL_Acc_CH20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 accommodated in community homes
CL_Acc_CH21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 accommodated in community homes
CL_Acc_SITA19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated in semi-independent transitional accommodation
CL_Acc_SITA19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 accommodated in semi-independent transitional accommodation
CL_Acc_SITA20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 accommodated in semi-independent transitional accommodation
CL_Acc_SITA21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 accommodated in semi-independent transitional accommodation
CL_Acc_SL19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated in supported lodgings
CL_Acc_SL19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 accommodated in supported lodgings
CL_Acc_SL20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 accommodated in supported lodgings
CL_Acc_SL21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 accommodated in supported lodgings
CL_Acc_GA19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 who have gone abroad
CL_Acc_GA19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 who have gone abroad
CL_Acc_GA20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 who have gone abroad
CL_Acc_GA21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 who have gone abroad
CL_Acc_Dep19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 who have been deported
CL_Acc_Dep19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 who have been deported
CL_Acc_Dep20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 who have been deported
CL_Acc_Dep21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 who have been deported
CL_Acc_OL19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated in ordinary lodgings
CL_Acc_OL19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 accommodated in ordinary lodgings
CL_Acc_OL20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 accommodated in ordinary lodgings
CL_Acc_OL21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 accommodated in ordinary lodgings
CL_Acc_NK19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 with residence not known
CL_Acc_NK19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 with residence not known
CL_Acc_NK20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 with residence not known
CL_Acc_NK21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 with residence not known
CL_Acc_NFA19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 with no fixed abode or homeless
CL_Acc_NFA19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 with no fixed abode or homeless
CL_Acc_NFA20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 with no fixed abode or homeless
CL_Acc_NFA21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 with no fixed abode or homeless
CL_Acc_F19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated in foyers
CL_Acc_F19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 accommodated in foyers
CL_Acc_F20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 accommodated in foyers
CL_Acc_F21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 accommodated in foyers
CL_Acc_IL19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated in independent living
CL_Acc_IL19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 accommodated in independent living
CL_Acc_IL20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 accommodated in independent living
CL_Acc_IL21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 accommodated in independent living
CL_Acc_EA19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated in emergency accommodation
CL_Acc_EA19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 accommodated in emergency accommodation
CL_Acc_EA20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 accommodated in emergency accommodation
CL_Acc_EA21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 accommodated in emergency accommodation
CL_Acc_BB19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 in bed and breakfast accommodation
CL_Acc_BB19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 in bed and breakfast accommodation
CL_Acc_BB20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 in bed and breakfast accommodation
CL_Acc_BB21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 in bed and breakfast accommodation
CL_Acc_Cust19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 who are in custody
CL_Acc_Cust19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 who are in custody
CL_Acc_Cust20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 who are in custody
CL_Acc_Cust21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 who are in custody
CL_Acc_FFC19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 accommodated with former foster carers
CL_Acc_FFC19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 accommodated with former foster carers
CL_Acc_FFC20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 accommodated with former foster carers
CL_Acc_FFC21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 accommodated with former foster carers
CL_Acc_OTH19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 in other accommodation
CL_Acc_OTH19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 in other accommodation
CL_Acc_OTH20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 in other accommodation
CL_Acc_OTH21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 in other accommodation
CL_Acc_NoInf19to21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 to 21 for whom local authority does not have accommodation information [6]
CL_Acc_NoInf19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 for whom local authority does not have accommodation information [6]
CL_Acc_NoInf20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 for whom local authority does not have accommodation information [6]
CL_Acc_NoInf21_pc, Percentage of care leavers in the year ending 31 March 2019 aged 21 for whom local authority does not have accommodation information [6]
CL_StayPut_19&20, Care leavers in the year ending 31 March 2019 aged 19 and 20 who ceased to be looked after from a foster placement on their 18th birthday
CL_StayPut_19, Care leavers in the year ending 31 March 2019 aged 19 who ceased to be looked after from a foster placement on their 18th birthday
CL_StayPut_20, Care leavers in the year ending 31 March 2019 aged 20 who ceased to be looked after from a foster placement on their 18th birthday
CL_StayPut_FFC_19&20, Care leavers in the year ending 31 March 2019 aged 19 and 20 who ceased to be looked after from a foster placement on their 18th birthday and are living with the former foster carers
CL_StayPut_FFC_19, Care leavers in the year ending 31 March 2019 aged 19 who ceased to be looked after from a foster placement on their 18th birthday and are living with the former foster carers
CL_StayPut_FFC_20, Care leavers in the year ending 31 March 2019 aged 20 who ceased to be looked after from a foster placement on their 18th birthday and are living with the former foster carers
CL_StayPut_FFC_19&20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 and 20 who ceased to be looked after from a foster placement on their 18th birthday who are living with the former foster carers
CL_StayPut_FFC_19_pc, Percentage of care leavers in the year ending 31 March 2019 aged 19 who ceased to be looked after from a foster placement on their 18th birthday who are living with the former foster carers
CL_StayPut_FFC_20_pc, Percentage of care leavers in the year ending 31 March 2019 aged 20 who ceased to be looked after from a foster placement on their 18th birthday who are living with the former foster carers
Miss_Miss_DuringYear, Children looked after during the year ending 31 March 2019 who had a missing incident during the year [1 2]
Miss_Miss_DuringYear_pc, Percentage of children looked after during the year ending 31 March 2019 who had a missing incident during the year [1 2]
Miss_Miss_Incs, Number of missing incidents during the year for children looked after during the year ending 31 March 2019 [3]
Miss_Miss_Incs_av, Average number of missing incidents during the year for children looked after during the year ending 31 March 2019 who went missing [3 4]
Miss_Miss_MoreOnce, Children looked after during the year ending 31 March 2019 who went missing more than once during the year [5]
Miss_Miss_31Mar, Children looked after who were missing at 31 March 2019
Miss_Away_DuringYear, Children looked after during the year ending 31 March 2019 who were away from placement without authorisation during the year [2 6]
Miss_Away_DuringYear_pc, Percentage of children looked after during the year ending 31 March 2019 who were away from placement without authorisation during the year [2 6]
Miss_Away_Incs, Number of away from placements without authorisation incidents during the year for children looked after during the year ending 31 March 2019 [3]
Miss_Away_Incs_av, Average number of away from placements without authorisation incidents during the year for children looked after during the year ending 31 March 2019 who went away [3 4]
Miss_Away_MoreOnce, Children looked after during the year ending 31 March 2019 who were away from placement without authorisation more than once during the year [5]
Miss_Away_31Mar, Children looked after who were away from placement without authorisation at 31 March 2019
OC2_12mths, Children looked after at 31 March 2019 for at least 12 months [1 2]
OC2_10to17, Children looked after at 31 March 2019 for at least 12 months aged 10 to 17 years [3]
OC2_convicted, Children looked after at 31 March 2019 aged 10 to 17 years who were convicted or subject to youth cautions or youth conditional cautions during the year [4]
OC2_convicted_pc, Percentage of children looked after at 31 March 2019 aged 10 to 17 years who were convicted or subject to youth cautions or youth conditional cautions during the year [4]
OC2_submisuse, Children looked after at 31 March 2019 identified as having a substance misuse problem [2 5]
OC2_subint, Children looked after at 31 March 2019 received intervention for substance misuse [6]
OC2_suboffint, Children looked after at 31 March 2019 offered intervention for substance misuse and refused [7]
OC2_submisuse_pc, Percentage of children looked after at 31 March 2019 identified as having a substance misuse problem [5]
OC2_subint_pc, Percentage of children looked after at 31 March 2019 identified as having a substance misuse problem who received an intervention [6]
OC2_suboffint_pc, Percentage of children looked after at 31 March 2019 identified as having a substance misuse problem who were offered an intervention but refused it [7]
OC2_immunisation, Children looked after at 31 March 2019 whose immunisations were up to date [8]
OC2_teethcheck, Children looked after at 31 March 2019 who had their teeth checked [9]
OC2_healthassmt, Children looked after at 31 March 2019 who had their annual health assessment [10]
OC2_immunisation_pc, Percentage of children looked after at 31 March 2019 whose immunisations were up to date [8]
OC2_teethcheck_pc, Percentage of children looked after at 31 March 2019 who had their teeth checked [9]
OC2_healthassmt_pc, Percentage of children looked after at 31 March 2019 who had their annual health assessment [10]
OC2_4andunder, Children looked after at 31 March 2019 for at least 12 months aged 4 and under [11]
OC2_devassmt, Children looked after at 31 March 2019 aged 4 and under whose development assessments were up to date [11]
OC2_devassmt_pc, Percentage of children looked after at 31 March 2019 aged 4 and under whose development assessments were up to date [11]
OC2_5to16, Children looked after at 31 March 2019 for at least 12 months aged 5 to 16
OC2_SDQ, Children looked after at 31 March 2019 aged 5 to 16 with an SDQ score [12]
OC2_SDQ_pc, Percentage of children looked after at 31 March 2019 aged 5 to 16 with an SDQ score [12]
OC2_SDQnormal, Children looked after at 31 March 2019 aged 5 to 16 whose SDQ score was normal [13]
OC2_SDQborderline, Children looked after at 31 March 2019 aged 5 to 16 whose SDQ score was borderline [13]
OC2_SDQconcern, Children looked after at 31 March 2019 aged 5 to 16 whose SDQ score was a cause for concern [13]
OC2_SDQnormal_pc, Percentage of children looked after at 31 March 2019 aged 5 to 16 whose SDQ score was normal [13]
OC2_SDQborderline_pc, Percentage of children looked after at 31 March 2019 aged 5 to 16 whose SDQ score was borderline [13]
OC2_SDQconcern_pc, Percentage of children looked after at 31 March 2019 aged 5 to 16 whose SDQ score was a cause for concern [13]
OC2_SDQaverage, Children looked after at 31 March 2019 aged 5 to 16 with an SDQ score - average SDQ score

"

varlist_2019 <- read_csv(varlist_2019, col_names = c("varname", "description"))


  
  
# Select all varnames that are present in all years - 65
complete_varlist <- filter(varlist_2011, 
                           varname == "CLA_Mar2011" |
                             varname == "CLA_2011" |
                             varname == "CLA_started2011" |
           varname %in% varlist_2012$varname & 
           varname %in% varlist_2013$varname &
           varname %in% varlist_2014$varname &
           varname %in% varlist_2015$varname &
           varname %in% varlist_2016$varname &
           varname %in% varlist_2017$varname &
           varname %in% varlist_2018$varname &
           varname %in% varlist_2019$varname) %>%
  mutate(varname = case_when(varname == "CLA_Mar2011" ~ "CLA_Mar",
                             varname == "CLA_2011" ~ "CLA_", 
                             varname == "CLA_started2011" ~ "CLA_started",
                             TRUE ~ varname),
         description = str_remove_all(description, "2011 ")) %>%
  filter(!varname %in% c("CLA_OthPl")) # CLA Oth Pl missing from 2016 data
  
# All vars between 2011 and 2015 - 106
  filter(varlist_2011, 
         varname %in% varlist_2012$varname & 
           varname %in% varlist_2013$varname &
           varname %in% varlist_2014$varname &
           varname %in% varlist_2015$varname) 
  
# All vars between 2017 and 2017 - 361
  filter(varlist_2017, 
           varname %in% varlist_2018$varname &
           varname %in% varlist_2019$varname) 
  

# Just make one set for all years for now

  
#varlist_2011 %>% View(.)
# To be added manually
# CLA_Mar2011 - CLA at March 31st - denominator for CLA_male:CLA_NetGain
# CLA_2011 - CLA throughout year 
# CLA_started2011 - CLA who started in year - denominator for	SCLA_male:SCLA_SORR


complete_varlist %>% view(.)

tidycla_2011 <- read_csv("data/cla_data/cla_2011/SFR21_CLA.csv", na = c("x", "c")) %>%
  rename(CLA_Mar = CLA_Mar2011, CLA_ = CLA_2011) %>%
  left_join(., read_csv("data/cla_data/cla_2011/SFE21_ADM.csv", na = c("x", "c")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2011) %>%
  select(complete_varlist$varname) %>%
  mutate_at(vars(CLA_male:CLA_NetGain), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  mutate_at(vars(SCLA_male:SCLA_SORR), list(pc = ~round((./CLA_started)*100, 2))) %>%
  # pivot_longer(CLA_Mar:SCLA_SORR_pc, names_to = "description", values_to = "value") %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2011, .before = geog_c)

  
tidycla_2012 <- read_csv("data/cla_data/cla_2012/SFR20_CLA2012.csv", na = c("x", "c")) %>%
  rename(CLA_Mar = CLA_Mar2012, CLA_ = CLA_2012) %>%
  left_join(., read_csv("data/cla_data/cla_2012/SFR20_ADM2012.csv", na = c("x", "c")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2012) %>%
  select(complete_varlist$varname) %>%
  mutate_at(vars(CLA_male:CLA_NetGain), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  mutate_at(vars(SCLA_male:SCLA_SORR), list(pc = ~round((./CLA_started)*100, 2))) %>%
  # pivot_longer(CLA_Mar:SCLA_SORR_pc, names_to = "description", values_to = "value") %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2012, .before = geog_c)

tidycla_2013 <- read_csv("data/cla_data/cla_2013/SFR36_CLA2013.csv", na = c("x", "c", "#VALUE!")) %>%
  rename(CLA_Mar = CLA_Mar2013, CLA_ = CLA_2013) %>%
  left_join(., read_csv("data/cla_data/cla_2013/SFR36_ADM2013.csv", na = c("x", "c", "#VALUE!")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2013) %>%
  select(complete_varlist$varname) %>%
  mutate_at(vars(CLA_male:CLA_NetGain), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  mutate_at(vars(SCLA_male:SCLA_SORR), list(pc = ~round((./CLA_started)*100, 2))) %>%
  # pivot_longer(CLA_Mar:SCLA_SORR_pc, names_to = "description", values_to = "value") %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2013, .before = geog_c)

tidycla_2014 <- read_csv("data/cla_data/cla_2014/SFR36_CLA2014.csv", na = c("x", "c", "#VALUE!", "..")) %>%
  rename(CLA_Mar = CLA_Mar2014, CLA_ = CLA_2014) %>%
  left_join(., read_csv("data/cla_data/cla_2014/SFR36_ADM2014.csv", na = c("x", "c", "#VALUE!", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2014) %>%
  select(complete_varlist$varname) %>%
  mutate_at(vars(CLA_male:CLA_NetGain), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  mutate_at(vars(SCLA_male:SCLA_SORR), list(pc = ~round((./CLA_started)*100, 2))) %>%
  # pivot_longer(CLA_Mar:SCLA_SORR_pc, names_to = "description", values_to = "value") %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2014, .before = geog_c)


tidycla_2015 <- read_csv("data/cla_data/cla_2015/SFR34_CLA2015.csv", na = c("x", "c", "#VALUE!", "..")) %>%
  rename(CLA_Mar = CLA_Mar2015, CLA_ = CLA_2015) %>%
  left_join(., read_csv("data/cla_data/cla_2015/SFR34_ADM2015.csv", na = c("x", "c", "#VALUE!", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2014) %>% # Weird error here in the returns? Started 2014? Just a simple name change mistake?
  select(complete_varlist$varname) %>%
  mutate_at(vars(CLA_male:CLA_NetGain), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  mutate_at(vars(SCLA_male:SCLA_SORR), list(pc = ~round((./CLA_started)*100, 2))) %>%
  # pivot_longer(CLA_Mar:SCLA_SORR_pc, names_to = "description", values_to = "value") %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2015, .before = geog_c)

tidycla_2016 <- read_csv("data/cla_data/cla_2016/SFR41_CLA2016.csv", na = c("x", "c", "#VALUE!", "..")) %>%
  rename(CLA_Mar = CLA_Mar2016, CLA_ = CLA_2016) %>%
  left_join(., read_csv("data/cla_data/cla_2016/SFR41_ADM2016.csv", na = c("x", "c", "#VALUE!", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2016) %>% # Weird error here in the returns? Started 2014? Just a simple name change mistake?
  select(complete_varlist$varname) %>%
  mutate_at(vars(CLA_male:CLA_NetGain), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  mutate_at(vars(SCLA_male:SCLA_SORR), list(pc = ~round((./CLA_started)*100, 2))) %>%
  # pivot_longer(CLA_Mar:SCLA_SORR_pc, names_to = "description", values_to = "value") %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2016, .before = geog_c)

tidycla_2017 <- read_csv("data/cla_data/cla_2017/SFR50_CLA2017.csv", na = c("x", "c", "#VALUE!", "..")) %>%
  rename(CLA_Mar = CLA_Mar2017, CLA_ = CLA_2017) %>%
  left_join(., read_csv("data/cla_data/cla_2017/SFR50_ADM2017.csv", na = c("x", "c", "#VALUE!", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2017) %>% # Weird error here in the returns? Started 2014? Just a simple name change mistake?
  select(complete_varlist$varname) %>%
  mutate_at(vars(CLA_male:CLA_NetGain), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  mutate_at(vars(SCLA_male:SCLA_SORR), list(pc = ~round((./CLA_started)*100, 2))) %>%
  # pivot_longer(CLA_Mar:SCLA_SORR_pc, names_to = "description", values_to = "value") %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2017, .before = geog_c)

tidycla_2018 <- read_csv("data/cla_data/cla_2018/CLA2018.csv", na = c("x", "c", "#VALUE!", "..")) %>%
  rename(CLA_Mar = CLA_Mar2018, CLA_ = CLA_2018) %>%
  left_join(., read_csv("data/cla_data/cla_2018/ADM2018_amended.csv", na = c("x", "c", "#VALUE!", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2018) %>% 
  select(complete_varlist$varname) %>%
  mutate_at(vars(CLA_male:CLA_NetGain), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  mutate_at(vars(SCLA_male:SCLA_SORR), list(pc = ~round((./CLA_started)*100, 2))) %>%
  # pivot_longer(CLA_Mar:SCLA_SORR_pc, names_to = "description", values_to = "value") %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2018, .before = geog_c)

tidycla_2019 <- read_csv("data/cla_data/cla_2019/CLA2019.csv", na = c("x", "c", "#VALUE!", "..")) %>%
  rename(CLA_Mar = CLA_Mar2019, CLA_ = CLA_2019) %>%
  left_join(., read_csv("data/cla_data/cla_2019/ADM2019.csv", na = c("x", "c", "#VALUE!", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2019) %>% 
  select(complete_varlist$varname) %>%
  mutate_at(vars(CLA_male:CLA_NetGain), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  mutate_at(vars(SCLA_male:SCLA_SORR), list(pc = ~round((./CLA_started)*100, 2))) %>%
  # pivot_longer(CLA_Mar:SCLA_SORR_pc, names_to = "description", values_to = "value") %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2019, .before = geog_c) 


combined_longt <- bind_rows(tidycla_2011, tidycla_2012, tidycla_2013, tidycla_2014, tidycla_2015, tidycla_2016, tidycla_2017, tidycla_2018, tidycla_2019)

# Create a lookup for variable names and descriptions
complete_varlist <- complete_varlist %>%
  mutate(description = ifelse(row_number() < 5, description, paste0(description, " (Total Number)"))) %>%
  bind_rows(tibble(varname = paste0(complete_varlist$varname[7:42], "_pc"), description = paste0(complete_varlist$description[7:42], " (Percentage of all CLA on 31st March)"))) %>%
  bind_rows(tibble(varname = paste0(complete_varlist$varname[44:67], "_pc"), description = paste0(complete_varlist$description[44:67], " (Percentage of all CLA Starting in the year)")))

# Change variable names to descriptions, and replace column names to be contingent with csc_data
cla_long_final <- combined_longt %>%
  pivot_longer(cols = CLA_Mar:SCLA_SORR_pc, names_to = "description", values_to = "value") %>%
  left_join(., complete_varlist, by = c("description" = "varname")) %>%
  select(new_la_code = New_geog_code, la_name = geog_n, year, description = description.y, value) %>%
  mutate(description = str_remove_all(description, "20[0-9][0-9] "))


csc_data <- read_rds("data/csc_data.RDS")


csc_data_v2 <- csc_data %>%
  bind_rows(cla_long_final)

write_rds(csc_data_v2, path = "data/csc_data_v2.RDS")



# Cathy data - FRG --------------------------------------------------------------

sgo_orders <- read_csv("data/cla_data/cla_2019/CEA2019.csv", na = c("x", "c", "#VALUE!", "..", ".")) %>%
  select(geog_n, geog_l, CEA_SGO_FFC_Rel_pc, CEA_SGO_Car_Rel_pc)


sgo_orders_region1 <- sgo_orders %>%
  filter(geog_l == "REGION", geog_n != "LONDON") %>%
  arrange(CEA_SGO_FFC_Rel_pc) %>% 
  mutate(
    geog_n = as.factor(tools::toTitleCase(tolower(geog_n)))
  ) %>%
  mutate(
    id = seq(1, 10, 1),
    geog_n = fct_reorder(geog_n, CEA_SGO_FFC_Rel_pc)
  ) 

number_of_bar <- nrow(sgo_orders_region1)
angle <-  90 - 360 * (sgo_orders_region1$id-0.5) / number_of_bar 
sgo_orders_region1$hjust<-ifelse(angle < -90, 1, 0)
sgo_orders_region1$angle<-ifelse(angle < -90, angle+180, angle)

sgo_orders_region1 %>%
  ggplot() +
  geom_col(aes(x = id, y = CEA_SGO_FFC_Rel_pc), fill = "#15126F", col = "white") +
  geom_text(aes(x = id, y = CEA_SGO_FFC_Rel_pc + 0.2 * CEA_SGO_FFC_Rel_pc, 
                label = geog_n, angle = angle, hjust = hjust), size = 5, col = "#15126F",
            position = "dodge") +
  geom_text(aes(x = id, y = CEA_SGO_FFC_Rel_pc -2.3, label = paste0(CEA_SGO_FFC_Rel_pc, "%")), size = 5, col = "#19AAED") +
  annotate("text", x = 1, y = -5, label = paste0("England\n", sgo_orders$CEA_SGO_FFC_Rel_pc[sgo_orders$geog_n == "ENGLAND"],"%"), size = 5, col = "#15126F") +
  theme_minimal() +
  ggeasy::easy_remove_axes() +
  ylim(c(-5, 22)) +
  coord_polar() +
  ggtitle(str_wrap("On average, 8% of children ceasing to be 'looked after' in 2018/19 ended their time in care through a special guardianship order made to former foster carer(s) who were relatives or friends. This use of SGOs varied a lot across regions.\n\n Percentage of children ceasing to be looked after through an SGO made to former foster carer(s) who were relatives or friends:", 80)) +
  theme(
    plot.title = element_text(hjust = 0, size = 14, color = "#15126F"), panel.grid = element_blank()) &
  plot_annotation(caption = "Data visualisation by Calum Webb @cjrwebb. Data from Department for Education Looked-after children statistics 2019.")


sgo_orders$CEA_SGO_Car_Rel_pc

sgo_orders_region2 <- sgo_orders %>%
  filter(geog_l == "REGION", geog_n != "LONDON") %>%
  arrange(CEA_SGO_Car_Rel_pc) %>% 
  mutate(
    geog_n = as.factor(tools::toTitleCase(tolower(geog_n)))
  ) %>%
  mutate(
    id = seq(1, 10, 1),
    geog_n = fct_reorder(geog_n, CEA_SGO_Car_Rel_pc)
  ) 

number_of_bar <- nrow(sgo_orders_region2)
angle <-  90 - 360 * (sgo_orders_region2$id-0.5) / number_of_bar 
sgo_orders_region2$hjust<-ifelse(angle < -90, 1, 0)
sgo_orders_region2$angle<-ifelse(angle < -90, angle+180, angle)

sgo_orders_region2 %>%
  ggplot() +
  geom_col(aes(x = id, y = CEA_SGO_Car_Rel_pc), fill = "#15126F", col = "white") +
  geom_text(aes(x = id, y = CEA_SGO_Car_Rel_pc + 0.2 * CEA_SGO_Car_Rel_pc, 
                label = geog_n, angle = angle, hjust = hjust), size = 5, col = "#15126F",
            position = "dodge") +
  geom_text(aes(x = id, y = 0.6*CEA_SGO_Car_Rel_pc, label = paste0(CEA_SGO_Car_Rel_pc, "%")), size = 5, col = "#19AAED") +
  annotate("text", x = 1, y = -3, label = paste0("England\n", sgo_orders$CEA_SGO_Car_Rel_pc[sgo_orders$geog_n == "ENGLAND"],"%"), size = 5, col = "#15126F") +
  theme_minimal() +
  ggeasy::easy_remove_axes() +
  ylim(c(-3, 8)) +
  coord_polar() +
  ggtitle(str_wrap("A further 4 per cent of children in England ceasing to be 'looked after' in 2018/19 ended their time in care through an SGO made to carer(s) other than former foster carer(s) who were or are relatives or friends. Percentage of children ceasing to be looked after through this kind of SGO by region:", 80)) +
  theme(
    plot.title = element_text(hjust = 0, size = 14, color = "#15126F"), panel.grid = element_blank()) &
  plot_annotation(caption = "Data visualisation by Calum Webb @cjrwebb. Data from Department for Education Looked-after children statistics 2019.")



sgo_orders_la1 <- sgo_orders %>%
  filter(geog_l == "LA" & !is.na(CEA_SGO_FFC_Rel_pc)) %>%
  arrange(CEA_SGO_FFC_Rel_pc) %>% 
  mutate(
    geog_n = as.factor(tools::toTitleCase(tolower(geog_n)))
  ) %>%
  mutate(
    id = seq(1, nrow(.), 1),
    geog_n = fct_reorder(geog_n, CEA_SGO_FFC_Rel_pc)
  ) 

number_of_bar <- nrow(sgo_orders_la1)
angle <-  90 - 360 * (sgo_orders_la1$id-0.5) / number_of_bar 
sgo_orders_la1$hjust<-ifelse(angle < -90, 1, 0)
sgo_orders_la1$angle<-ifelse(angle < -90, angle+180, angle)

sgo_orders_la1 %>%
  ggplot() +
  geom_col(aes(x = id, y = CEA_SGO_FFC_Rel_pc), fill = "#15126F", col = "white") +
  geom_text(aes(x = id, y = CEA_SGO_FFC_Rel_pc + 0.2, 
                label = paste0(geog_n, " ", CEA_SGO_FFC_Rel_pc, "%")), size = 1.8, col = "#15126F",
            position = "dodge", hjust = 0) +
  annotate("text", x = 70, y = -3, label = paste0("LA Avg.\n", round(mean(sgo_orders_la1$CEA_SGO_FFC_Rel_pc, na.rm = TRUE), 1),"%"), size = 5, col = "#15126F") +
  theme_minimal() +
  ggeasy::easy_remove_axes() +
  ylim(c(-5, 30)) +
  coord_flip() +
  ggtitle(str_wrap("Many LAs had missing or censored data for the number of exits from care due to SGOs being made to former foster carer(s) who were relatives or friends, but for those with data, the average rate for an LA was 9.1 per cent of children that were no longer 'looked after' in 2018/19. These SGOs made up between 0 and 23 per cent of ceased looked after care depending on the local authority:", 95)) +
  theme(
    plot.title = element_text(hjust = 0, size = 14, color = "#15126F"), panel.grid = element_blank()) &
  plot_annotation(caption = "Data visualisation by Calum Webb @cjrwebb. Data from Department for Education Looked-after children statistics 2019.")




sgo_orders_la2 <- sgo_orders %>%
  filter(geog_l == "LA" & !is.na(CEA_SGO_Car_Rel_pc)) %>%
  arrange(CEA_SGO_Car_Rel_pc) %>% 
  mutate(
    geog_n = as.factor(tools::toTitleCase(tolower(geog_n)))
  ) %>%
  mutate(
    id = seq(1, nrow(.), 1),
    geog_n = fct_reorder(geog_n, CEA_SGO_Car_Rel_pc)
  ) 

number_of_bar <- nrow(sgo_orders_la2)
angle <-  90 - 360 * (sgo_orders_la2$id-0.5) / number_of_bar 
sgo_orders_la2$hjust<-ifelse(angle < -90, 1, 0)
sgo_orders_la2$angle<-ifelse(angle < -90, angle+180, angle)

sgo_orders_la2 %>%
  ggplot() +
  geom_col(aes(x = id, y = CEA_SGO_Car_Rel_pc), fill = "#15126F", col = "white") +
  geom_text(aes(x = id, y = CEA_SGO_Car_Rel_pc + 0.2, 
                label = paste0(geog_n, " ", CEA_SGO_Car_Rel_pc, "%")), size = 1.8, col = "#15126F",
            position = "dodge", hjust = 0) +
  annotate("text", x = 70, y = -3, label = paste0("LA Avg.\n", round(mean(sgo_orders_la2$CEA_SGO_Car_Rel_pc, na.rm = TRUE), 1),"%"), size = 5, col = "#15126F") +
  theme_minimal() +
  ggeasy::easy_remove_axes() +
  ylim(c(-5, 30)) +
  coord_flip() +
  ggtitle(str_wrap("Again, many LAs had missing or censored data about the proportion of children looked after who had ceased to be 'looked after' because of an SGO made to a carer other than a foster carer who was a relative or friend in 2018/19. The average across local authorities was 4.9 per cent, but this ranged from 17 per cent in Middlesbrough to 0 per cent in 20 local authorities:", 95)) +
  theme(
    plot.title = element_text(hjust = 0, size = 14, color = "#15126F"), panel.grid = element_blank()) &
  plot_annotation(caption = "Data visualisation by Calum Webb @cjrwebb. Data from Department for Education Looked-after children statistics 2019.")

# Longitudinal data

# 2015 - 2019


foster_data <- tibble::tribble(
  ~type, ~V2,    ~year2010,    ~year2011,    ~year2012,    ~year2013,    ~year2014,
  "Foster placement inside Council boundary (not with relative or friend)",  NA,  24520,  25400,  26650,  26770,  27160,
  "Foster placement inside Council boundary with relative or friend",  NA,   5330,   5380,   5300,   5150,   5200
) %>% bind_rows(
  tibble::tribble(
    ~type, ~V2,    ~year2010,    ~year2011,    ~year2012,    ~year2013,    ~year2014,
    "Foster placement outside Council boundary (not with relative or friend)",  NA,  14940,  15280,  15950,  16580,  16790,
    "Foster placement outside Council boundary with relative or friend",  NA,   2090,   2110,   2130,   2090,   2100
  )
) %>%
  select(-V2) %>%
  pivot_longer(cols = year2010:year2014, values_to = "number", names_to = "year") %>%
  mutate(year = as.double(str_remove_all(year, "year"))) %>%
bind_rows(
tibble::tribble(
  ~type, ~V2,    ~year2015,    ~year2016,    ~year2017,    ~year2018,    ~year2019,
  "Foster placement inside Council boundary (not with relative or friend)",  NA,  32300,  31920,  32980,  33860,  34330,
  "Foster placement inside Council boundary with relative or friend",  NA,   5610,   5740,   6310,   6800,   7260
) %>%
  bind_rows(
    tibble::tribble(
      ~type, ~V2,    ~year2015,    ~year2016,    ~year2017,    ~year2018,    ~year2019,
      "Foster placement outside Council boundary (not with relative or friend)",  NA,  19270,  19510,  20030,  20880,  21820,
      "Foster placement outside Council boundary with relative or friend",  NA,   2310,   2380,   2530,   2930,   3190
    )
  ) %>%
  select(-V2) %>%
  pivot_longer(cols = year2015:year2019, values_to = "number", names_to = "year") %>%
  mutate(year = as.double(str_remove_all(year, "year")))
)


foster_data <- foster_data %>%
  group_by(year) %>%
  mutate(total_fosters_year = sum(number)) %>%
  ungroup() %>%
  mutate(
    percent_foster = round((number / total_fosters_year) * 100, 1)
  ) %>%
  arrange(type, year)


foster_data %>%
  mutate(within_outside = ifelse(str_detect(type, "outside"), "Outside Council Area", "Inside Council Area"),
         placed_with_frrel = ifelse(str_detect(type, "not with"), "Not with friends/relatives", "With friends/relatives")) %>%
  ggplot() +
  geom_line(aes(group = placed_with_frrel, col = placed_with_frrel, x = year, y = percent_foster), size = 1.5) +
  geom_label(aes(group = placed_with_frrel, col = placed_with_frrel, x = year, y = percent_foster, label = percent_foster), size = 5) +
  annotate("text", x = 2014.5, y = 85, label = "Not placed with relatives/friends", col = "#15126F", size = 5) +
  annotate("text", x = 2014.5, y = 70, label = "Placed with relatives/friends", col = "#297FB9", size = 5) +
  facet_wrap(within_outside ~., ncol = 1) +
  scale_x_continuous(breaks = seq(2010, 2019, 1)) +
  scale_color_manual(values = c("#15126F", "#297FB9")) +
  ylim(c(0,100)) +
  ylab("Percentage of foster placements") +
  xlab("Year ending") +
  ggeasy::easy_remove_legend() +
  theme_minimal() +
  theme(
    legend.position = "none", strip.text = element_text(colour = "white"),
    strip.background = element_rect(fill = "#1F9DB9"),
    text = element_text(color = "#15126F", size = 16),
    
  ) &
  plot_annotation(caption = "Data visualisation by Calum Webb @cjrwebb. Data from Department for Education 'Looked-after children statistics'.")


# end of quick FRG analysis -----------------------------------------------


# try and match across CEA for years using fuzzy matching --------------------------------------

# consistent variables in 2010-2017 that are not in 2017-2019

# 106 variables
complete_2010_2016 <- filter(varlist_2011, 
       varname %in% varlist_2012$varname & 
         varname %in% varlist_2013$varname &
         varname %in% varlist_2014$varname &
         varname %in% varlist_2015$varname &
         varname %in% varlist_2016$varname) 

# Which are not in complete 2010-2019 varlist - 42 vars including ceased
complete_2010_2016_unmatched <- complete_2010_2016 %>% filter(!varname %in% complete_varlist$varname)

# 361 variables between 2017 and 2019
complete_2017_2019 <- filter(varlist_2017, 
       varname %in% varlist_2018$varname & 
         varname %in% varlist_2019$varname) 

# 297 not in including ceased
complete_2017_2019_unmatched <- complete_2017_2019 %>% filter(!varname %in% complete_varlist$varname)

# Explore fuzzy matching descriptions 

# Match by description variable (calculate distance), group by varname
# and then return the highest matching distance
fuzzy_matches_2010_2019 <- stringdist_join(complete_2010_2016_unmatched %>% 
                                             mutate(full_string = paste(varname, description)), 
                                           complete_2017_2019_unmatched %>% 
                                             mutate(full_string = paste(varname, description)),
                                            by = "full_string",
                                            mode = "left",
                                            ignore_case = TRUE,
                                            method = "jw",
                                            max_dist = 99,
                                            distance_col = "dist") %>%
                                            group_by(varname.x) %>%
                                            select(-full_string.x, -full_string.y) %>%
                                            top_n(1, -dist) %>%
                                            arrange(dist)

# Elbow plot to see if any distances seem very high
fuzzy_matches_2010_2019 %>%
  ggplot() +
  geom_line(aes(x = rank(dist), y = dist)) +
  geom_point(aes(x = rank(dist), y = dist)) +
  geom_text_repel(aes(x = rank(dist), y = dist, 
                      label = paste(varname.x, "-", varname.y)), size = 2)


# Manual checking of correct matches
fuzzy_matches_2010_2019 <- fuzzy_matches_2010_2019 %>% add_column(valid = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))

#view(fuzzy_matches_2010_2019)

# Add a few manually:
# CLA_cea16 = Over 16 CLA ceased = CLA_16over
# CLA_ceaROG = CLA ceased residence order = can't find equivalent
# CLA_Outbound = CLA placed outside LA boundary 31st Mar = needs to combine CLA_OutLA_LTE20 and CLA_OutLA_GT20
# CLA_InBound = CLA placed inside LA boundary 31st March = can only be calculated manually by difference between (CLA_OutLA_LTE20 + CLA_OutLA_GT20) - CLA total 31st march

fuzzy_matches_2010_2019_final <- fuzzy_matches_2010_2019 %>% 
  filter(valid == TRUE) %>%
  ungroup() %>%
  add_row(varname.x = "CLA_cea16", 
          description.x = "Children looked after at 31 March aged 16 and over", 
          varname.y = "CEA_16", 
          description.y = "Children looked after at 31 March aged 16",
          valid = TRUE) %>%
  add_row(varname.x = "CLA_cea16", 
          description.x = "Children looked after at 31 March aged 16 and over", 
          varname.y = "CEA_17", 
          description.y = "Children looked after at 31 March aged 17",
          valid = TRUE) %>%
  add_row(varname.x = "CLA_cea16", 
          description.x = "Children looked after at 31 March aged 16 and over", 
          varname.y = "CEA_18over", 
          description.y = "Children looked after at 31 March aged 18 and over",
          valid = TRUE) %>%
  add_row(varname.x = c("CLA_Outbound", "CLA_InBound"), 
          description.x = c("Children looked after at 31 March 2011 placed outside of the LA boundary", "Children looked after at 31 March 2011 placed within LA boundary"), 
          varname.y = c("CLA_Outbound", "CLA_InBound"), 
          description.y = c("Children looked after at 31 March 2011 placed outside of the LA boundary", "Children looked after at 31 March 2011 placed within LA boundary"),
          valid = c(TRUE, TRUE))

# Create inbound and outbound variables by combining variables CLA_OutLA_LTE20 and CLA_OutLA_GT20 
# in tidycla_2017, tidycla_2018 and tidycla_2019

cla_inoutbound_2017 <- read_csv("data/cla_data/cla_2017/SFR50_CLA2017.csv", na = c("x", "c", "#VALUE!", "..")) %>%
  rename(CLA_Mar = CLA_Mar2017, CLA_ = CLA_2017) %>%
  left_join(., read_csv("data/cla_data/cla_2017/SFR50_ADM2017.csv", na = c("x", "c", "#VALUE!", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2017) %>%
  select(New_geog_code, geog_l, geog_c, geog_n, CLA_Mar, CLA_OutLA_LTE20, CLA_OutLA_GT20) %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(
    CLA_Outbound = CLA_OutLA_LTE20 + CLA_OutLA_GT20,
    CLA_InBound = CLA_Mar - CLA_Outbound,
    year = 2017
  ) %>%
  select(-CLA_OutLA_LTE20:-CLA_OutLA_GT20) %>%
  mutate_at(vars(CLA_Outbound:CLA_InBound), list(pc = ~(./CLA_Mar)*100)) %>%
  select(New_geog_code, geog_c, year, geog_n, everything()) %>%
  select(-CLA_Mar) 


cla_inoutbound_2018 <- read_csv("data/cla_data/cla_2018/CLA2018.csv", na = c("x", "c", "#VALUE!", "..")) %>%
  rename(CLA_Mar = CLA_Mar2018, CLA_ = CLA_2018) %>%
  left_join(., read_csv("data/cla_data/cla_2018/ADM2018_amended.csv", na = c("x", "c", "#VALUE!", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2018) %>%
  select(New_geog_code, geog_l, geog_c, geog_n, CLA_Mar, CLA_OutLA, CLA_InLA) %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(
    CLA_Outbound = CLA_OutLA,
    CLA_InBound = CLA_InLA,
    year = 2018
  ) %>%
  select(-CLA_OutLA:-CLA_InLA) %>%
  mutate_at(vars(CLA_Outbound:CLA_InBound), list(pc = ~(./CLA_Mar)*100)) %>%
  select(New_geog_code, geog_c, year, geog_n, everything()) %>%
  select(-CLA_Mar) 

cla_inoutbound_2019 <- read_csv("data/cla_data/cla_2019/CLA2019.csv", na = c("x", "c", "#VALUE!", "..")) %>%
  rename(CLA_Mar = CLA_Mar2019, CLA_ = CLA_2019) %>%
  left_join(., read_csv("data/cla_data/cla_2019/ADM2019.csv", na = c("x", "c", "#VALUE!", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2019) %>%
  select(New_geog_code, geog_l, geog_c, geog_n, CLA_Mar, CLA_OutLA, CLA_InLA) %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(
    CLA_Outbound = CLA_OutLA,
    CLA_InBound = CLA_InLA,
    year = 2019
  ) %>%
  select(-CLA_OutLA:-CLA_InLA) %>%
  mutate_at(vars(CLA_Outbound:CLA_InBound), list(pc = ~(./CLA_Mar)*100)) %>%
  select(New_geog_code, geog_c, year, geog_n, everything()) %>%
  select(-CLA_Mar) 

cla_inoutbound_2017_2019 <- bind_rows(cla_inoutbound_2017, cla_inoutbound_2018, cla_inoutbound_2019)

# read in and select variables with unmatched names, then add names to match

fuzzy_matches_2010_2019_final


fuzzyextras_2011 <- read_csv("data/cla_data/cla_2011/SFR21_CLA.csv", na = c("x", "c", "..")) %>%
  rename(CLA_Mar = CLA_Mar2011, CLA_ = CLA_2011) %>%
  select(-CLA_Adopt) %>%
  left_join(., read_csv("data/cla_data/cla_2011/SFE21_ADM.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  left_join(., read_csv("data/cla_data/cla_2011/SFR21_CEA.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2011) %>%
  select(contains(c("New_geog_code", "geog_l", "geog_c", "geog_n", "CLA_Mar", "CLA_started", "Adop", fuzzy_matches_2010_2019_final$varname.x))) %>%
  mutate(CLA_InBound = CLA_Mar - CLA_Outbound) %>%
  rowwise() %>%
  mutate_at(vars(CLA_OthPl, CLA_Outbound, CLA_InBound), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
    mutate(
      CLA_cea_adopt_all = ifelse(is.na(CLA_ceaAdop) & is.na(CLA_ceaAdop2), NA, sum(CLA_ceaAdop, CLA_ceaAdop2, na.rm = TRUE)), # All Ceasing through adoption
      CLA_cea_unopposed_pc = (CLA_ceaAdop / CLA_cea_adopt_all) * 100, # % ceasing through adoption that was unopposed
      CLA_cea_dispensedconsent_pc = (CLA_ceaAdop2 / CLA_cea_adopt_all) * 100
    ) %>%
  ungroup() %>%
  mutate_at(vars(CLA_cease16:CLA_ceataken, CLA_cea_sen_cust, CLA_ceaAdop, CLA_ceaAdop2, CLA_cea_adopt_all), list(pc = ~round((./CLA_cease)*100, 2))) %>%
  mutate_at(vars(SCLA_POG), list(SCLA_POG_pc = ~round((./CLA_started)*100, 2))) %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2011, .before = geog_c) %>%
  select(everything(), sort(names(.)[7:48])) %>%
  select(New_geog_code, year, geog_c, geog_n, CLA_OthPl, CLA_OthPl_pc, SCLA_POG, SCLA_POG_pc, CLA_cease, CLA_cea1, CLA_cea14, CLA_cea59, CLA_cea1015, CLA_cea16, CLA_cea1_pc, CLA_cea14_pc, CLA_cea59_pc, CLA_cea1015_pc, CLA_cea16_pc, everything()) %>%
  select(-CLA_Mar, -CLA_started) %>%
  select(-CLA_cease16, -CLA_cease16_pc)


fuzzyextras_2012 <- read_csv("data/cla_data/cla_2012/SFR20_CLA2012.csv", na = c("x", "c", "..")) %>%
  rename(CLA_Mar = CLA_Mar2012, CLA_ = CLA_2012) %>%
  select(-CLA_Adopt) %>%
  left_join(., read_csv("data/cla_data/cla_2012/SFR20_ADM2012.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  left_join(., read_csv("data/cla_data/cla_2012/SFR_CEA2012.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2012) %>%
  select(contains(c("New_geog_code", "geog_l", "geog_c", "geog_n", "CLA_Mar", "CLA_started", "Adop", fuzzy_matches_2010_2019_final$varname.x))) %>%
  mutate(CLA_InBound = CLA_Mar - CLA_Outbound) %>%
  mutate_at(vars(CLA_OthPl, CLA_Outbound, CLA_InBound), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  rowwise() %>%
  mutate(
    CLA_cea_adopt_all = ifelse(is.na(CLA_ceaAdop) & is.na(CLA_ceaAdop2), NA, sum(CLA_ceaAdop, CLA_ceaAdop2, na.rm = TRUE)), # All Ceasing through adoption
    CLA_cea_unopposed_pc = (CLA_ceaAdop / CLA_cea_adopt_all) * 100, # % ceasing through adoption that was unopposed
    CLA_cea_dispensedconsent_pc = (CLA_ceaAdop2 / CLA_cea_adopt_all) * 100
  ) %>%
  ungroup() %>%
  mutate_at(vars(CLA_cease16:CLA_ceataken, CLA_cea_sen_cust, CLA_ceaAdop, CLA_ceaAdop2, CLA_cea_adopt_all), list(pc = ~round((./CLA_cease)*100, 2))) %>%
  mutate_at(vars(SCLA_POG), list(SCLA_POG_pc = ~round((./CLA_started)*100, 2))) %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2012, .before = geog_c) %>%
  select(everything(), sort(names(.)[7:48])) %>%
  select(New_geog_code, year, geog_c, geog_n, CLA_OthPl, CLA_OthPl_pc, SCLA_POG, SCLA_POG_pc, CLA_cease, CLA_cea1, CLA_cea14, CLA_cea59, CLA_cea1015, CLA_cea16, CLA_cea1_pc, CLA_cea14_pc, CLA_cea59_pc, CLA_cea1015_pc, CLA_cea16_pc, everything()) %>%
  select(-CLA_Mar, -CLA_started) %>%
  select(-CLA_cease16, -CLA_cease16_pc)

fuzzyextras_2013 <- read_csv("data/cla_data/cla_2013/SFR36_CLA2013.csv", na = c("x", "c", "..")) %>%
  rename(CLA_Mar = CLA_Mar2013, CLA_ = CLA_2013) %>%
  select(-CLA_Adopt) %>%
  left_join(., read_csv("data/cla_data/cla_2013/SFR36_ADM2013.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  left_join(., read_csv("data/cla_data/cla_2013/SFR36_CEA2013.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2013) %>%
  select(contains(c("New_geog_code", "geog_l", "geog_c", "geog_n", "CLA_Mar", "CLA_started", "Adop", fuzzy_matches_2010_2019_final$varname.x))) %>%
  mutate(CLA_InBound = CLA_Mar - CLA_Outbound) %>%
  mutate_at(vars(CLA_OthPl, CLA_Outbound, CLA_InBound), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  rowwise() %>%
  mutate(
    CLA_cea_adopt_all = ifelse(is.na(CLA_ceaAdop) & is.na(CLA_ceaAdop2), NA, sum(CLA_ceaAdop, CLA_ceaAdop2, na.rm = TRUE)), # All Ceasing through adoption
    CLA_cea_unopposed_pc = (CLA_ceaAdop / CLA_cea_adopt_all) * 100, # % ceasing through adoption that was unopposed
    CLA_cea_dispensedconsent_pc = (CLA_ceaAdop2 / CLA_cea_adopt_all) * 100
  ) %>%
  ungroup() %>%
  mutate_at(vars(CLA_cease16:CLA_ceataken, CLA_cea_sen_cust, CLA_ceaAdop, CLA_ceaAdop2, CLA_cea_adopt_all), list(pc = ~round((./CLA_cease)*100, 2))) %>%
  mutate_at(vars(SCLA_POG), list(SCLA_POG_pc = ~round((./CLA_started)*100, 2))) %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2013, .before = geog_c) %>%
  select(everything(), sort(names(.)[7:48])) %>%
  select(New_geog_code, year, geog_c, geog_n, CLA_OthPl, CLA_OthPl_pc, SCLA_POG, SCLA_POG_pc, CLA_cease, CLA_cea1, CLA_cea14, CLA_cea59, CLA_cea1015, CLA_cea16, CLA_cea1_pc, CLA_cea14_pc, CLA_cea59_pc, CLA_cea1015_pc, CLA_cea16_pc, everything()) %>%
  select(-CLA_Mar, -CLA_started) %>%
  select(-CLA_cease16, -CLA_cease16_pc)

fuzzyextras_2014 <- read_csv("data/cla_data/cla_2014/SFR36_CLA2014.csv", na = c("x", "c", "..")) %>%
  rename(CLA_Mar = CLA_Mar2014, CLA_ = CLA_2014) %>%
  select(-CLA_Adopt) %>% # gets rid of CLA_adopt in favour of CEA Adopt
  left_join(., read_csv("data/cla_data/cla_2014/SFR36_ADM2014.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  left_join(., read_csv("data/cla_data/cla_2014/SFR36_CEA2014.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2014) %>%
  select(contains(c("New_geog_code", "geog_l", "geog_c", "geog_n", "CLA_Mar", "CLA_started", "Adop", fuzzy_matches_2010_2019_final$varname.x))) %>%
  mutate(CLA_InBound = CLA_Mar - CLA_Outbound) %>%
  mutate_at(vars(CLA_OthPl, CLA_Outbound, CLA_InBound), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  rowwise() %>%
  mutate(
    CLA_cea_adopt_all = ifelse(is.na(CLA_ceaAdop) & is.na(CLA_ceaAdop2), NA, sum(CLA_ceaAdop, CLA_ceaAdop2, na.rm = TRUE)), # All Ceasing through adoption
    CLA_cea_unopposed_pc = (CLA_ceaAdop / CLA_cea_adopt_all) * 100, # % ceasing through adoption that was unopposed
    CLA_cea_dispensedconsent_pc = (CLA_ceaAdop2 / CLA_cea_adopt_all) * 100
  ) %>%
  ungroup() %>%
  mutate_at(vars(CLA_cease16:CLA_ceataken, CLA_cea_sen_cust, CLA_ceaAdop, CLA_ceaAdop2, CLA_cea_adopt_all), list(pc = ~round((./CLA_cease)*100, 2))) %>%
  mutate_at(vars(SCLA_POG), list(SCLA_POG_pc = ~round((./CLA_started)*100, 2))) %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2014, .before = geog_c) %>%
  select(everything(), sort(names(.)[7:48])) %>%
  select(New_geog_code, year, geog_c, geog_n, CLA_OthPl, CLA_OthPl_pc, SCLA_POG, SCLA_POG_pc, CLA_cease, CLA_cea1, CLA_cea14, CLA_cea59, CLA_cea1015, CLA_cea16, CLA_cea1_pc, CLA_cea14_pc, CLA_cea59_pc, CLA_cea1015_pc, CLA_cea16_pc, everything()) %>%
  select(-CLA_Mar, -CLA_started) %>%
  select(-CLA_cease16, -CLA_cease16_pc)


fuzzyextras_2015 <- read_csv("data/cla_data/cla_2015/SFR34_CLA2015.csv", na = c("x", "c", "..")) %>%
  rename(CLA_Mar = CLA_Mar2015, CLA_ = CLA_2015) %>%
  select(-CLA_Adopt) %>%
  left_join(., read_csv("data/cla_data/cla_2015/SFR34_ADM2015.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  left_join(., read_csv("data/cla_data/cla_2015/SFR34_CEA2015.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2014) %>% # Weird error again that exists for 2015 data
  select(contains(c("New_geog_code", "geog_l", "geog_c", "geog_n", "CLA_Mar", "CLA_started", "Adop", fuzzy_matches_2010_2019_final$varname.x))) %>%
  mutate(CLA_InBound = CLA_Mar - CLA_Outbound) %>%
  mutate_at(vars(CLA_OthPl, CLA_Outbound, CLA_InBound), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  rowwise() %>%
  mutate(
    CLA_cea_adopt_all = ifelse(is.na(CLA_ceaAdop) & is.na(CLA_ceaAdop2), NA, sum(CLA_ceaAdop, CLA_ceaAdop2, na.rm = TRUE)), # All Ceasing through adoption
    CLA_cea_unopposed_pc = (CLA_ceaAdop / CLA_cea_adopt_all) * 100, # % ceasing through adoption that was unopposed
    CLA_cea_dispensedconsent_pc = (CLA_ceaAdop2 / CLA_cea_adopt_all) * 100, 
  ) %>%
  ungroup() %>%
  mutate_at(vars(CLA_cea14:CLA_ceataken, CLA_cea_sen_cust, CLA_ceaAdop, CLA_ceaAdop2, CLA_cea_adopt_all), list(pc = ~round((./CLA_cease)*100, 2))) %>%
  mutate_at(vars(SCLA_POG), list(SCLA_POG_pc = ~round((./CLA_started)*100, 2))) %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2015, .before = geog_c) %>%
  select(everything(), sort(names(.)[7:48])) %>%
  select(New_geog_code, year, geog_c, geog_n, CLA_OthPl, CLA_OthPl_pc, SCLA_POG, SCLA_POG_pc, CLA_cease, CLA_cea1, CLA_cea14, CLA_cea59, CLA_cea1015, CLA_cea16, CLA_cea1_pc, CLA_cea14_pc, CLA_cea59_pc, CLA_cea1015_pc, CLA_cea16_pc, everything()) %>%
  select(-CLA_Mar, -CLA_started) 

# from 2016 - need to combine CLA cease age 16 17 and 18 to get 16+
# Other placement not actually in this file despite being in meta data
fuzzyextras_2016 <- read_csv("data/cla_data/cla_2016/SFR41_CLA2016.csv", na = c("x", "c", "..")) %>%
  rename(CLA_Mar = CLA_Mar2016, CLA_ = CLA_2016) %>%
  select(-CLA_Adopt) %>%
  left_join(., read_csv("data/cla_data/cla_2016/SFR41_ADM2016.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  left_join(., read_csv("data/cla_data/cla_2016/SFR41_CEA2016.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2016) %>% 
  select(contains(c("New_geog_code", "geog_l", "geog_c", "geog_n", "CLA_Mar", "CLA_started", "Adop", fuzzy_matches_2010_2019_final$varname.x))) %>%
  mutate(CLA_InBound = CLA_Mar - CLA_Outbound) %>%
  mutate_at(vars(CLA_Outbound, CLA_InBound), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  rowwise() %>%
  mutate(
    CLA_cea_adopt_all = ifelse(is.na(CLA_ceaAdop1) & is.na(CLA_ceaAdop2), NA, sum(CLA_ceaAdop1, CLA_ceaAdop2, na.rm = TRUE)), # All Ceasing through adoption
    CLA_cea_unopposed_pc = (CLA_ceaAdop1 / CLA_cea_adopt_all) * 100, # % ceasing through adoption that was unopposed
    CLA_cea_dispensedconsent_pc = (CLA_ceaAdop2 / CLA_cea_adopt_all) * 100, 
    CLA_cea16 = sum(CLA_cea16, CLA_cea17, CLA_cea18, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(-CLA_cea17, -CLA_cea18) %>%
  mutate_at(vars(CLA_cea14:CLA_ceataken, CLA_cea_sen_cust, CLA_ceaAdop1, CLA_ceaAdop2, CLA_cea_adopt_all), list(pc = ~round((./CLA_cease)*100, 2))) %>%
  mutate_at(vars(SCLA_POG), list(SCLA_POG_pc = ~round((./CLA_started)*100, 2))) %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2016, .before = geog_c) %>%
  select(everything(), sort(names(.)[6:48])) %>%
  select(New_geog_code, year, geog_c, geog_n, SCLA_POG, SCLA_POG_pc, CLA_cease, CLA_cea1, CLA_cea14, CLA_cea59, CLA_cea1015, CLA_cea16, CLA_cea1_pc, CLA_cea14_pc, CLA_cea59_pc, CLA_cea1015_pc, CLA_cea16_pc, everything()) %>%
  select(-CLA_Mar, -CLA_started) 


fuzzyextras_2017 <- read_csv("data/cla_data/cla_2017/SFR50_CLA2017.csv", na = c("x", "c", "..")) %>%
  rename(CLA_Mar = CLA_Mar2017, CLA_ = CLA_2017) %>%
  select(-CLA_Adopt) %>%
  left_join(., read_csv("data/cla_data/cla_2017/SFR50_ADM2017.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  left_join(., read_csv("data/cla_data/cla_2017/SFR50_CEA2017.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2017, CLA_cease = CLA_cease2017) %>% 
  select(contains(c("New_geog_code", "geog_l", "geog_c", "geog_n", "CLA_Mar", "CLA_started", "Adop", "CLA_cease", fuzzy_matches_2010_2019_final$varname.y))) %>%
  mutate(CLA_InBound = CLA_Mar - CLA_Outbound) %>%
  mutate_at(vars(CLA_Outbound, CLA_InBound), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  rowwise() %>%
  mutate(
    CLA_cea_adopt_all = ifelse(is.na(CEA_Adop1) & is.na(CEA_Adop2), NA, sum(CEA_Adop1, CEA_Adop2, na.rm = TRUE)), # All Ceasing through adoption
    CLA_cea_unopposed_pc = (CEA_Adop1 / CLA_cea_adopt_all) * 100, # % ceasing through adoption that was unopposed
    CLA_cea_dispensedconsent_pc = (CEA_Adop2 / CLA_cea_adopt_all) * 100, 
    CEA_16over = sum(CEA_16, CEA_17, CEA_18over, na.rm = TRUE)
  )  %>%
  select(-CEA_16, -CEA_17, -CEA_18over) %>%
  ungroup() %>%
  mutate_at(vars(CEA_1to4:CEA_Taken, CEA_16over, CEA_Custody, CEA_Adop1, CEA_Adop2, CLA_cea_adopt_all), list(pc = ~round((./CLA_cease)*100, 2))) %>%
  mutate_at(vars(SCLA_PlaceO), list(SCLA_PlaceO_pc = ~round((./CLA_started)*100, 2))) %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2017, .before = geog_c) %>%
  select(everything(), sort(names(.)[6:45])) %>%
  select(New_geog_code, year, geog_c, geog_n, SCLA_PlaceO, SCLA_PlaceO_pc, CLA_cease, CEA_U1, CEA_1to4, CEA_5to9, CEA_10to15, CEA_16over, CEA_U1_pc, CEA_1to4_pc, CEA_5to9_pc, CEA_10to15_pc, CEA_16over_pc, everything()) %>%
  select(-CLA_Mar, -CLA_started) 

fuzzyextras_2018 <- read_csv("data/cla_data/cla_2018/CLA2018.csv", na = c("x", "c", "..")) %>%
  rename(CLA_Mar = CLA_Mar2018, CLA_ = CLA_2018) %>%
  select(-CLA_Adopt, -CLA_Adopt_pc) %>%
  left_join(., read_csv("data/cla_data/cla_2018/ADM2018_amended.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  left_join(., read_csv("data/cla_data/cla_2018/CEA2018.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2018, CLA_cease = CLA_cease2018) %>% 
  select(contains(c("New_geog_code", "geog_l", "geog_c", "geog_n", "CLA_Mar", "CLA_started", "Adop", "CLA_cease", "CLA_InLA", "CLA_OutLA", fuzzy_matches_2010_2019_final$varname.y))) %>%
  mutate(CLA_InBound = CLA_InLA,
         CLA_Outbound = CLA_OutLA) %>%
  select(-CLA_InLA, -CLA_OutLA, -CLA_Mar2014:-CLA_Mar2017, -CLA_started2014:-CLA_started2017, -CLA_cease2014:-CLA_cease2017, -CLA_InLA_LTE20:-CLA_OutLA_NoInfo_pc) %>% # remove waste vars
  rowwise() %>%
  select(-contains("_pc")) %>% # remove DfE % calculations for consistency
  mutate(
    CLA_cea_adopt_all = ifelse(is.na(CEA_Adop1) & is.na(CEA_Adop2), NA, sum(CEA_Adop1, CEA_Adop2, na.rm = TRUE)), # All Ceasing through adoption
    CLA_cea_unopposed_pc = (CEA_Adop1 / CLA_cea_adopt_all) * 100, # % ceasing through adoption that was unopposed
    CLA_cea_dispensedconsent_pc = (CEA_Adop2 / CLA_cea_adopt_all) * 100, 
    CEA_16over = sum(CEA_16, CEA_17, CEA_18over, na.rm = TRUE)
  )  %>%
  select(-CEA_16, -CEA_17, -CEA_18over) %>%
  ungroup() %>%
  mutate_at(vars(CEA_1to4:CEA_Taken, CEA_16over, CEA_Custody, CEA_Adop1, CEA_Adop2, CLA_cea_adopt_all), list(pc = ~round((./CLA_cease)*100, 2))) %>%
  mutate_at(vars(SCLA_PlaceO), list(SCLA_PlaceO_pc = ~round((./CLA_started)*100, 2))) %>%
  mutate_at(vars(CLA_Outbound, CLA_InBound), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2018, .before = geog_c) %>%
  select(everything(), sort(names(.)[6:41])) %>%
  select(New_geog_code, year, geog_c, geog_n, SCLA_PlaceO, SCLA_PlaceO_pc, CLA_cease, CEA_U1, CEA_1to4, CEA_5to9, CEA_10to15, CEA_16over, CEA_U1_pc, CEA_1to4_pc, CEA_5to9_pc, CEA_10to15_pc, CEA_16over_pc, everything()) %>%
  select(-CLA_Mar, -CLA_started) 

fuzzyextras_2019 <- read_csv("data/cla_data/cla_2019/CLA2019.csv", na = c("x", "c", "..")) %>%
  rename(CLA_Mar = CLA_Mar2019, CLA_ = CLA_2019) %>%
  select(-CLA_Adopt, -CLA_Adopt_pc) %>%
  left_join(., read_csv("data/cla_data/cla_2019/ADM2019.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  left_join(., read_csv("data/cla_data/cla_2019/CEA2019.csv", na = c("x", "c", "..")), 
            by = c("New_geog_code", "geog_l", "geog_c", "geog_n")) %>%
  rename(CLA_started = CLA_started2019, CLA_cease = CLA_cease2019) %>% 
  select(contains(c("New_geog_code", "geog_l", "geog_c", "geog_n", "CLA_Mar", "CLA_started", "Adop", "CLA_cease", "CLA_InLA", "CLA_OutLA", fuzzy_matches_2010_2019_final$varname.y))) %>%
  mutate(CLA_InBound = CLA_InLA,
         CLA_Outbound = CLA_OutLA) %>%
  select(-CLA_InLA, -CLA_OutLA, -CLA_Mar2015:-CLA_Mar2018, -CLA_started2015:-CLA_started2018, -CLA_cease2015:-CLA_cease2018, -CLA_InLA_LTE20:-CLA_OutLA_NoInfo_pc) %>% # remove waste vars
  select(-contains("_pc")) %>% # remove DfE % calculations for consistency
  rowwise() %>%
  mutate(
    CLA_cea_adopt_all = ifelse(is.na(CEA_Adop1) & is.na(CEA_Adop2), NA, sum(CEA_Adop1, CEA_Adop2, na.rm = TRUE)), # All Ceasing through adoption
    CLA_cea_unopposed_pc = (CEA_Adop1 / CLA_cea_adopt_all) * 100, # % ceasing through adoption that was unopposed
    CLA_cea_dispensedconsent_pc = (CEA_Adop2 / CLA_cea_adopt_all) * 100, 
    CEA_16over = sum(CEA_16, CEA_17, CEA_18over, na.rm = TRUE)
  )  %>%
  select(-CEA_16, -CEA_17, -CEA_18over) %>%
  ungroup() %>%
  mutate_at(vars(CEA_1to4:CEA_Taken, CEA_16over, CEA_Custody, CEA_Adop1, CEA_Adop2, CLA_cea_adopt_all), list(pc = ~round((./CLA_cease)*100, 2))) %>%
  mutate_at(vars(SCLA_PlaceO), list(SCLA_PlaceO_pc = ~round((./CLA_started)*100, 2))) %>%
  mutate_at(vars(CLA_Outbound, CLA_InBound), list(pc = ~round((./CLA_Mar)*100, 2))) %>%
  filter(geog_l == "LA") %>%
  select(-geog_l) %>%
  mutate(year = 2019, .before = geog_c) %>%
  select(everything(), sort(names(.)[6:41])) %>%
  select(New_geog_code, year, geog_c, geog_n, SCLA_PlaceO, SCLA_PlaceO_pc, CLA_cease, CEA_U1, CEA_1to4, CEA_5to9, CEA_10to15, CEA_16over, CEA_U1_pc, CEA_1to4_pc, CEA_5to9_pc, CEA_10to15_pc, CEA_16over_pc, everything()) %>%
  select(-CLA_Mar, -CLA_started) 


# Make names equal
# Add percentages
fuzzy_matches_2010_2019_final <- fuzzy_matches_2010_2019_final %>%
  mutate(description.x = str_remove_all(description.x, "20[0-9][0-9] "), description.y = str_remove_all(description.y, "20[0-9][0-9] "))

fuzzy_matches_2010_2019_final$description.x
fuzzy_matches_2010_2019_final$description.x[1:3] # unneeded
fuzzy_matches_2010_2019_final$description.x[4:15] # descriptions for ceased
fuzzy_matches_2010_2019_final$description.x[16:18] # unneeded
fuzzy_matches_2010_2019_final$description.x[19:20] # descriptions for in and out of boundaries


pc_varnames.x <- paste0(fuzzy_matches_2010_2019_final$varname.x, "_pc")

pc_descriptions <- c(fuzzy_matches_2010_2019_final$description.x[1:3], # unneeded
paste0(fuzzy_matches_2010_2019_final$description.x[4:15], " (% of all CLA ceased during year)"), # descriptions for ceased
fuzzy_matches_2010_2019_final$description.x[16:18], # unneeded
paste0(fuzzy_matches_2010_2019_final$description.x[19:20], " (% of all CLA at March 31st)")) # descriptions for in and out of boundaries

pc_varnames.y <- paste0(fuzzy_matches_2010_2019_final$varname.y, "_pc")

pc_addons <- tibble(varname.x = pc_varnames.x, description.x = pc_descriptions, varname.y = pc_varnames.y, description.y = pc_descriptions, dist = NA, valid = NA)

fuzzy_matches_2010_2019_final <- bind_rows(fuzzy_matches_2010_2019_final, pc_addons)

# Check and add any missing names

all_unique_names <- unique(c(names(fuzzyextras_2011), names(fuzzyextras_2012), names(fuzzyextras_2013), names(fuzzyextras_2014), names(fuzzyextras_2015), names(fuzzyextras_2016), names(fuzzyextras_2017), names(fuzzyextras_2018), names(fuzzyextras_2019)))

setdiff(all_unique_names, c(fuzzy_matches_2010_2019_final$varname.x, fuzzy_matches_2010_2019_final$varname.y))

fuzzy_matches_2010_2019_final <- fuzzy_matches_2010_2019_final %>%
  add_row(varname.x = "CLA_cea_adopt_all", description.x = "Number of children who ceased to be looked after because they were adopted (total)", varname.y = "CLA_cea_adopt_all", description.y = "Number of children who ceased to be looked after because they were adopted (total)") %>%
  add_row(varname.x = "CLA_cea_adopt_all_pc", description.x = "Number of children who ceased to be looked after because they were adopted, total (% of all CLA ceased during year)", varname.y = "CLA_cea_adopt_all_pc", description.y = "Number of children who ceased to be looked after because they were adopted, total (% of all CLA ceased during year)") %>%
  add_row(varname.x = "CLA_cea_unopposed_pc", description.x = "Percentage of children who ceased to be looked after because of adoption where the adoption application was unopposed", varname.y = "CLA_cea_unopposed_pc", description.y = "Percentage of children who ceased to be looked after because of adoption where the adoption application was unopposed") %>%
  add_row(varname.x = "CLA_cea_dispensedconsent_pc", description.x = "Percentage of children who ceased to be looked after because of adoption where consent was dispensed with by the court", varname.y = "CLA_cea_dispensedconsent_pc", description.y = "Percentage of children who ceased to be looked after because of adoption where consent was dispensed with by the court") %>%
  add_row(varname.x = "CLA_ceaAdop", description.x = "Children who ceased to be looked after during the year ending 31 March 2011 due to adoption - application unopposed", varname.y = "CEA_Adop1", description.y = "Children who ceased to be looked after during the year ending 31 March 2011 due to adoption - application unopposed") %>%
  add_row(varname.x = "CLA_ceaAdop_pc", description.x = "Children who ceased to be looked after during the year ending 31 March 2011 due to adoption - application unopposed (% of all CLA ceased during year)", varname.y = "CEA_Adop1_pc", description.y = "Children who ceased to be looked after during the year ending 31 March 2011 due to adoption - application unopposed (% of all CLA ceased during year)") %>%
  add_row(varname.x = "CLA_ceaAdop", description.x = "Children who ceased to be looked after during the year ending 31 March 2011 due to adoption - application unopposed", varname.y = "CLA_ceaAdop1", description.y = "Children who ceased to be looked after during the year ending 31 March 2011 due to adoption - application unopposed") %>%
  add_row(varname.x = "CLA_ceaAdop_pc", description.x = "Children who ceased to be looked after during the year ending 31 March 2011 due to adoption - application unopposed (% of all CLA ceased during year)", varname.y = "CLA_ceaAdop1_pc", description.y = "Children who ceased to be looked after during the year ending 31 March 2011 due to adoption - application unopposed (% of all CLA ceased during year)") %>%
  add_row(varname.x = "CLA_ceaAdop2", description.x = "Children who ceased to be looked after during the year ending 31 March 2011- consent dispensed with", varname.y = "CEA_Adop2", description.y = "Children who ceased to be looked after during the year ending 31 March 2011- consent dispensed with") %>%
  add_row(varname.x = "CLA_ceaAdop2_pc", description.x = "Children who ceased to be looked after during the year ending 31 March 2011- consent dispensed with (% of all CLA ceased during year)", varname.y = "CEA_Adop2_pc", description.y = "Children who ceased to be looked after during the year ending 31 March 2011- consent dispensed with (% of all CLA ceased during year)") %>%
  add_row(varname.x = "CEA_16over", description.x = "Children who ceased to be looked after during the year ending 31 March 2019 aged 16 and over", varname.y = "CEA_16over", description.y = "Children who ceased to be looked after during the year ending 31 March 2019 aged 16 and over") %>%
  add_row(varname.x = "CEA_16over_pc", description.x = "Children who ceased to be looked after during the year ending 31 March 2019 aged 16 and over (% of all CLA ceased during year)", varname.y = "CEA_16over_pc", description.y = "Children who ceased to be looked after during the year ending 31 March 2019 aged 16 and over (% of all CLA ceased during year)") %>%
  add_row(varname.x = "CLA_OthPl_pc", description.x = "Children looked after at 31 March in other placements (% of all CLA on March 31st) - Uses fuzzy matching between 2011-2016 and 2017-2019", varname.y = "CLA_OthPl_pc", description.y = "Children looked after at 31 March in other placements (% of all CLA on March 31st) - Uses fuzzy matching between 2011-2016 and 2017-2019")

setdiff(all_unique_names, c(fuzzy_matches_2010_2019_final$varname.x, fuzzy_matches_2010_2019_final$varname.y))

#fuzzy_matches_2010_2019_final %>% view(.)

matches_lookup <- tibble(
  varname = c(fuzzy_matches_2010_2019_final$varname.x, fuzzy_matches_2010_2019_final$varname.y),
  description = paste(c(fuzzy_matches_2010_2019_final$description.x, fuzzy_matches_2010_2019_final$description.x))
) %>%
  mutate(description = str_remove_all(description, "2011-")) %>%
  mutate(description = str_remove_all(description, "2011 ")) %>%
  mutate(description = str_replace_all(description, "consent dispensed with", "adoption - consent dispensed with")) %>%
  mutate(description = paste(description,  "- Uses fuzzy matching between 2011-2016 and 2017-2019"))




# Convert to long format and merge years

fuzzyextras_2011_long <- fuzzyextras_2011 %>% pivot_longer(CLA_OthPl:CLA_cea_adopt_all_pc, names_to = "varname", values_to = "value")
fuzzyextras_2012_long <- fuzzyextras_2012 %>% pivot_longer(CLA_OthPl:CLA_cea_adopt_all_pc, names_to = "varname", values_to = "value")
fuzzyextras_2013_long <- fuzzyextras_2013 %>% pivot_longer(CLA_OthPl:CLA_cea_adopt_all_pc, names_to = "varname", values_to = "value")
fuzzyextras_2014_long <- fuzzyextras_2014 %>% pivot_longer(CLA_OthPl:CLA_cea_adopt_all_pc, names_to = "varname", values_to = "value")
fuzzyextras_2015_long <- fuzzyextras_2015 %>% pivot_longer(CLA_OthPl:CLA_cea_adopt_all_pc, names_to = "varname", values_to = "value")
fuzzyextras_2016_long <- fuzzyextras_2016 %>% pivot_longer(SCLA_POG:CLA_cea_adopt_all_pc, names_to = "varname", values_to = "value")
fuzzyextras_2017_long <- fuzzyextras_2017 %>% pivot_longer(SCLA_PlaceO:CLA_cea_adopt_all_pc, names_to = "varname", values_to = "value")
fuzzyextras_2018_long <- fuzzyextras_2018 %>% pivot_longer(SCLA_PlaceO:CLA_cea_adopt_all_pc, names_to = "varname", values_to = "value")
fuzzyextras_2019_long <- fuzzyextras_2019 %>% pivot_longer(SCLA_PlaceO:CLA_cea_adopt_all_pc, names_to = "varname", values_to = "value")

fuzzyextras_full <- bind_rows(fuzzyextras_2011_long, fuzzyextras_2012_long, fuzzyextras_2013_long, fuzzyextras_2014_long, fuzzyextras_2015_long, fuzzyextras_2016_long, fuzzyextras_2017_long, fuzzyextras_2018_long, fuzzyextras_2019_long)

# Add 2017-2019 inbound outbound
cla_inoutbound_2018_long <- cla_inoutbound_2018 %>% pivot_longer(CLA_Outbound:CLA_InBound_pc, names_to = "varname", values_to = "value")
cla_inoutbound_2019_long <- cla_inoutbound_2019 %>% pivot_longer(CLA_Outbound:CLA_InBound_pc, names_to = "varname", values_to = "value")

fuzzyextras_full <- bind_rows(fuzzyextras_full, cla_inoutbound_2018_long, cla_inoutbound_2019_long)



# Add merged descriptions to disparate varnames (where descriptions are description.x)

# Check visually and see if this looks right
left_join(fuzzyextras_full, matches_lookup, by = "varname") %>%
  .$description %>%
  unique(.)

# final corrections
matches_lookup <- matches_lookup %>%
  mutate(description = ifelse(varname == "CLA_OthPl_pc", "Children looked after at 31 March in other placements (% of all CLA on March 31st) - Uses fuzzy matching between 2011-2016 and 2017-2019", description)) %>%
  mutate(description = ifelse(varname == "CLA_cea16_pc", "Children who ceased to be looked after during the year ending 31 March 2019 aged 16 and over (% of all CLA ceased during year) - Uses fuzzy matching between 2011-2016 and 2017-2019", description)) %>%
  mutate(description = ifelse(varname == "CLA_cea16", "Children who ceased to be looked after during the year ending 31 March 2019 aged 16 and over - Uses fuzzy matching between 2011-2016 and 2017-2019", description))

# remove duplicates
matches_lookup <- matches_lookup %>%
  distinct(varname, .keep_all = TRUE)



# View(matches_lookup)

fuzzyextras_full <- left_join(fuzzyextras_full, matches_lookup, by = "varname") 
#View(fuzzyextras_full)

# remove NaNs/Infs
fuzzyextras_full <- fuzzyextras_full %>%
  mutate(value = ifelse(is.nan(value), NA, value))


# Looks okay. Add to data

# rename cols to match for binding
fuzzyextras_full <- fuzzyextras_full %>%
  select(new_la_code = New_geog_code, la_name = geog_n, year = year, description = description, value = value)


csc_data <- read_rds("data/csc_data_v2.RDS")

csc_data

csc_data_v3 <- bind_rows(csc_data, fuzzyextras_full)

write_rds(csc_data_v3, path = "data/csc_data_v3.RDS")

csc_data <- read_rds(path = "data/csc_data_v3.RDS")

# This fixes DfE typo for 10-15 in 2011
csc_data %>%
  mutate(
    description = ifelse(str_detect(description, "aged 0 to 15"), str_replace(description, "aged 0 to 15", "aged 10 to 15"), description)
  ) %>%
  filter(str_detect(description, "0-17")) %>%
  .$description %>%
  unique(.)

csc_data <- csc_data %>%
  mutate(
    description = ifelse(str_detect(description, "aged 0 to 15"), str_replace(description, "aged 0 to 15", "aged 10 to 15"), description)
  ) 

write_rds(csc_data, path = "data/csc_data_v3.RDS")

# issues with total adoptions in 2018 in London - a very large number of zeros? Not sure
# if this is a true outlier or something related to the data 
# Some comparisons are messed up by the data censoring
# Add NA + NA = NA condition to sums, so sum NA + NA, na.rm != 0 but equals NA
# Helps a bit, but still gives some percentages that add up to >100 


# Add detailed 2017-2019 data from new UD ---------------------------------

varlist_2017_2019 <-   filter(varlist_2017, 
                              varname %in% varlist_2018$varname &
                                varname %in% varlist_2019$varname) 

# Filter out ones already included
varlist_2017_2019 <- varlist_2017_2019 %>%
  filter(!varname %in% complete_varlist$varname & !varname %in% matches_lookup$varname)


