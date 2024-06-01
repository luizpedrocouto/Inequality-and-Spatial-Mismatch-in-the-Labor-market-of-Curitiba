library('haven')
#library('OasisR')
library('tidyverse')
library('sf')
library('mapview')
library('geobr')
library('aopdata')
library('fabricatr')
library('foreign')
library('RColorBrewer')
library('sp')
library('plyr')
library('areal')
library('AER')
#library('riverdist')
library('rgdal')
library('nngeo')

##### READING DATA
#### SITE ACCESS: https://ippuc.org.br/pesquisas?backTo=to%255Bname%255D%3DPagina%26to%255Bparams%255D%255Bslug%255D%3Dpesquisa-e-informacoes%26page%3D%25C3%25A0%2520Pesquisa%2520e%2520Informa%25C3%25A7%25C3%25B5es

base_usada_na_dissertacao <- haven::read_dta(
  "D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/Pesquisa Oridem Destino Curitiba - IPPUC/Instrumentos/Instrumento 5 Rios/BaseAvancos.dta")

base_usada_na_dissertacao_ensaio_1 <- readxl::read_xlsx(
  "D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/base_dissimilaridade.xlsx")

limites_cwb <- st_read("D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/SHPsMaps/cwb_mun_lim.gpkg")

CWB_Censo_2010df <- readxl::read_excel("D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/Censo 2010/PR/Base informaçoes setores2010 universo PR/EXCEL/Base_informações_setores2010_sinopse_PR.xls") %>% 
     mutate(
    Cod_setor=as.numeric(Cod_setor),V017=as.numeric(V017))

CWB_Censo_2010df <- readxl::read_excel("D:/Economia/Estudos/Impacto da Linha Verde no uso do solo/Dados/Censos/PR/Base informaçoes setores2010 universo PR/EXCEL/Basico_PR.xls") %>% 
  mutate_all(funs(as.numeric(.)))

CWB_Censo_pessoa <- readxl::read_excel(
  "D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/Censo 2010/PR/Base informaçoes setores2010 universo PR/EXCEL/Pessoa03_PR.xls")


Rais_2017_trabalhador <- haven::read_dta(
  "D:/Economia/Estudos/Impacto da Linha Verde no uso do solo/Dados/RAIS/2017/pr2017_limpo.dta") 

Rais_2017_trabalhador_cwb <- filter(Rais_2017_trabalhador,
 municipio%in%str_sub(mun_mac_geobr,start = 1,end = 6))

cwb_cbd <- st_read("D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/SHPsMaps/CBD.gpkg") %>% 
  st_as_sf(coords=c("lon","lat"),crs=4326)

cwb_cbd <- st_transform(cwb_cbd,4674)

##### BOIJOSLY DATA ----

Boijosly_pt <- st_read("D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/Fishnet_centroides.gpkg")

Boijosly_shp <- st_read("D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/Fishnet_shp2D.gpkg")

Boijosly_csv <- readxl::read_xlsx("D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/PROJETO OPORTUNIDADES/Dados disponibilizados/Base GTFS.xlsx") 

##### MAPS ----
myprojection_utm <- CRS(
  "+proj=utm +zone=22 +ellps=GRS80 +units=m +no_defs")
#myprojection_utm <- CRS(
#  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs=WGS84=0,0,0")
SHP_MAC <- st_read(
  "D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/SHP_MAC_crs.gpkg") %>% 
  st_as_sf(coords=geom,crs=4326) %>% st_transform(crs=4674)

#%>% 
#  st_set_crs(4326) %>% st_transform(4674)

mapview(SHP_MAC)

setorescensitarios_ORIGEM <- df_od_cwb %>% 
  select(SETOR.IBGE.ORIGEM,ZONA_ORIGEM) %>% 
  distinct(SETOR.IBGE.ORIGEM,.keep_all = TRUE)
colnames(setorescensitarios_ORIGEM) <- c("Setor","Zona")

setorescensitarios_DESTINO <- df_od_cwb %>% select(IBGE_DESTINO,ZONA_DESTINO) %>% 
  distinct(IBGE_DESTINO,.keep_all = TRUE)
colnames(setorescensitarios_DESTINO) <- c("Setor","Zona")
  
setorescensitarios <- rbind(
  setorescensitarios_DESTINO,setorescensitarios_ORIGEM) %>% 
  distinct(Setor,.keep_all = TRUE) 

setorescensitarios <- as.numeric(setorescensitarios$Setor)
  
RM_CWB_SETORES <- geobr::read_census_tract(
  code_tract = "PR",year = 2010, simplified = FALSE) %>% 
  filter(code_tract%in%setorescensitarios)

mun_mac_geobr <- RM_CWB_SETORES %>% select(code_muni) %>% distinct(code_muni)

mun_mac_geobr <- as.numeric(mun_mac_geobr$code_muni)

RM_CWB_SETORES <- geobr::read_census_tract(
  code_tract = "PR",year = 2010, simplified = FALSE) %>% 
  filter(code_muni%in%mun_mac_geobr)

mapview(RM_CWB_SETORES,alpha=0.3) #+ (Boijosly_shp)

#####
base_usada_na_dissertacao_ensaio_1

fun_base_fig_2 <- function(quartil_baixo,quartil_alto){
  a <- base_usada_na_dissertacao_ensaio_1 %>% 
    #mutate(grupo_renda=split_quantile(
    #x=as.numeric(base_usada_na_dissertacao_ensaio_1$RENDA),type = 4)) %>% 
    filter(RENDA>quartil_baixo&RENDA<quartil_alto&TIPO_MEIO=="COLETIVO") %>% 
    group_by(ZONA_DE_TRABALHO) %>% dplyr::summarise(tot_emp=sum(`peso final`)) %>% 
    mutate(percent_empregos = tot_emp/sum(tot_emp)*100)
  b <- SHP_MAC %>% 
    left_join(a, by = c("ZONA"="ZONA_DE_TRABALHO"))
 print(
mapview(b,
zcol= "percent_empregos",col.regions = brewer.pal(5,"Reds"),alpha=0.1))#+ (limites_cwb))
 return(b)
}

mapa_1_quartil_1 <- fun_base_fig_2(quartil_baixo = 0, quartil_alto = 1100)
mapa_1_quartil_2 <- fun_base_fig_2(quartil_baixo = 1101, quartil_alto=1500)
mapa_1_quartil_3 <- fun_base_fig_2(quartil_baixo = 1501,quartil_alto= 2500)
mapa_1_quartil_4 <- fun_base_fig_2(quartil_baixo = 2501,quartil_alto =  40000)

##### TRAVEL MATRIX TIME

#Boijosly_trav_matrix <- as.data.frame(xtabs(
#  formula = `tempo em minutos sistema BRT`~`J de destino`+`I de Origem`,
#  data = Boijosly_csv)) %>% 
#  mutate(`Time in Minutes`=Freq,Freq=NULL,
#  J.de.destino=as.numeric(J.de.destino))

Boijosly_trav_matrix <- as.data.frame(xtabs(
  formula = TEMPO.MINUTOS~Origem+Destino,
  data = Boijosly_results)) %>% 
  mutate(TEMPO.MINUTOS=Freq,Freq=NULL,
        Origem=as.numeric(Origem))


is.de.destino <- Boijosly_results %>% group_by(Destino) %>% 
  summarise(Tempo=mean(TEMPO.MINUTOS,na.rm=T))

##### RAIS
naturezajuridica <- c(1015,1023,1031,1040,1058,1066,1074,1082,
    1104,1112,1120,1139,147,1155,1163,1171,1180,1198,1201,1210,2011,2038)

rais_ready <- function(raisyear_geoc){
  a <- readxl::read_excel(raisyear_geoc) %>% mutate(
    lon=as.numeric(lon),lat=as.numeric(lat)) %>% drop_na(lon|lat) %>% 
    dplyr::filter(!(naturjur %in% c(1015,1023,1031,1040,1058,1066,1074,1082,
  1104,1112,1120,1147,1155,1163,1171,1180,1198,2038))) %>% 
    dplyr::filter(!(ibgesubsetor == "ADM PUBLICA")) %>% 
  filter(!(naturjur %in% naturezajuridica)) %>% 
    st_as_sf(coords=c("lon","lat"),
    crs=4326)
  
  b <- st_transform(a,crs=4674) #%>% mutate()
  
  return(b)
}

shp_rais_sf_17 <- rais_ready(
 "D:/Economia/Estudos/Impacto da Linha Verde no uso do solo/Dados/RAIS/2017/Rais_2017_geocodificada.xlsx") %>% 
  mutate(estcltout=as.numeric(estcltout))

RM_CWB_censo_Merg <- RM_CWB_SETORES %>% 
  mutate(code_tract=as.numeric(code_tract)) %>% 
  left_join(CWB_Censo_2010df, by=c("code_tract"="Cod_setor"))

RM_CWB_EMPREGOS <- st_join(RM_CWB_censo_Merg,shp_rais_sf_17)

RM_CWB_EMPREGOS <- RM_CWB_EMPREGOS %>%
  group_by(code_tract,geom) %>% 
 dplyr::summarise(`Total de emprego`=sum(estcltout)) 
    
RM_CWB_EMPREGOS$area <- st_area(RM_CWB_EMPREGOS)/1000

RM_CWB_EMPREGOS <- RM_CWB_EMPREGOS %>% mutate(
  area=units::drop_units(area)) %>% mutate(
    `Densidade de emprego`=`Total de emprego`/area)

mapview(
  RM_CWB_EMPREGOS,zcol="Densidade de emprego",
  col.regions=brewer.pal(7,"Reds"),alpha=0.1)

##### EMPREGOS ----

industrial <- c(1112,	1120,	1139,	1147,	1155,	1198,	1210,	1228,	1317,	1325,	
                1333,	1341,	1392,	1414,	1422,	1430,	1449,	1457,	1465,	1503,	
                1619,	1627,	1708,	2119,	2127,	2135,	5118,	5126,	10006,	11100,	
                11207,	13102,	13218,	13226,	13234,	13242,	13250,	13293,	
                14109,	14214,	14222,	14290,	15113,	15121,	15130,	15148,	
                15210,	15229,	15237,	15318,	15326,	15334,	15415,	15423,	
                15431,	15512,	15520,	15539,	15547,	15555,	15563,	15598,	
                15610,	15628,	15717,	15725,	15814,	15822,	15830,	15849,	
                15857,	15865,	15890,	15911,	15920,	15938,	15946,	15954,	
                16004,	17116,	17191,	17213,	17221,	17230,	17248,	17310,	
                17329,	17337,	17418,	17493,	17507,	17612,	17620,	17639,	
                17647,	17698,	17710,	17728,	17795,	18112,	18120,	18139,	
                18210,	18228,	19100,	19216,	19291,	19313,	19321,	19330,	
                19399,	20109,	20214,	20222,	20230,	20290,	21105,	21210,	
                21229,	21318,	21326,	21415,	21423,	21490,	22110,	22128,	
                22136,	22144,	22152,	22160,	22179,	22187,	22195,	22217,	
                22225,	22292,	22314,	22322,	22330,	22349,	23108,	23205,	
                23213,	23299,	23302,	23400,	24112,	24120,	24139,	24147,	
                24198,	24210,	24228,	24295,	24317,	24325,	24333,	24414,	
                24422,	24511,	24520,	24538,	24546,	24619,	24627,	24635,	
                24694,	24716,	24724,	24732,	24813,	24821,	24830,	24910,	
                24929,	24937,	24945,	24953,	24961,	24996,	25119,	25127,	
                25194,	25216,	25224,	25291,	26115,	26123,	26190,	26204,	
                26301,	26417,	26425,	26492,	26913,	26921,	26999,	27111,	
                27120,	27138,	27146,	27219,	27227,	27235,	27243,	27251,	
                27260,	27294,	27316,	27391,	27413,	27421,	27499,	27510,	
                27529,	28118,	28126,	28134,	28215,	28223,	28312,	28320,	
                28339,	28347,	28398,	28410,	28428,	28436,	28819,	28827,	
                28916,	28924,	28932,	28991,	29114,	29122,	29130,	29149,	
                29157,	29211,	29220,	29238,	29246,	29254,	29297,	29319,	
                29327,	29408,	29513,	29521,	29530,	29548,	29610,	29629,	
                29637,	29645,	29653,	29696,	29718,	29726,	29815,	29890,	
                29912,	29920,	29939,	29947,	29955,	29963,	30112,	30120,	
                30210,	30228,	31119,	31127,	31135,	31216,	31224,	31305,	
                31410,	31429,	31518,	31526,	31607,	31810,	31828,	31895,	
                31917,	31925,	31992,	32107,	32212,	32220,	32301,	32905,	
                33103,	33200,	33308,	33405,	33502,	33910,	33928,	33936,	
                33944,	34100,	34207,	34312,	34320,	34398,	34410,	34428,	
                34436,	34444,	34495,	34509,	35114,	35122,	35211,	35220,	
                35238,	35319,	35327,	35912,	35920,	35998,	36110,	36129,	
                36137,	36145,	36919,	36927,	36935,	36943,	36951,	36960,	
                36978,	36994,	37109,	37206,	40100,	40118,	40126,	40134,	
                40142,	40207,	40304,	41009,	45110)

servico <- c(52698,	52710,	52728,	52795,	55115,	55123,	55131,	55190,	
             55212,	55220,	55239,	55247,	55298,	60100,	60216,	60224,	
             60232,	60240,	60259,	60267,	60275,	60283,	60291,	60305,	
             61115,	61123,	61212,	61220,	61239,	62103,	62200,	62308,	
             63118,	63126,	63215,	63223,	63231,	63304,	63401,	64114,	
             64122,	64203,	65102,	65218,	65226,	65234,	65242,	65315,	
             65323,	65331,	65340,	65358,	65404,	65510,	65595,	65919,	
             65927,	65935,	65994,	66117,	66125,	66133,	66214,	66222,	
             66303,	67113,	67121,	67199,	67202,	70106,	70203,	70319,	
             70327,	70408,	71102,	71218,	71226,	71234,	71315,	71323,	
             71331,	71390,	71404,	72109,	72206,	72214,	72290,	72303,	
             72400,	72508,	72907,	73105,	73202,	74110,	74128,	74136,	
             74144,	74152,	74160,	74209,	74306,	74403,	74500,	74608,	
             74705,	74918,	74926,	74993,	75116,	75124,	75132,	75140,	
             75213,	75221,	75230,	75248,	75256,	75302,	80110,	80128,	
             80136,	80144,	80152,	80209,	80217,	80225,	80306,	80314,	
             80322,	80330,	80918,	80926,	80934,	80942,	80950,	80969,	
             80977,	80993,	85111,	85120,	85138,	85146,	85154,	85162,	
             85200,	85316,	85324,	90000,	91111,	91120,	91200,	91910,	
             91928,	91995,	92118,	92126,	92134,	92215,	92223,	92312,	
             92320,	92398,	92401,	92517,	92525,	92533,	92614,	92622,	
             93017,	93025,	93033,	93041,	93092,	95001,	99007,	50202,	
             50423,	45110,	45128,	45136,	45217,	45225,	45233,	45241,	
             45250,	45292,	45314,	45322,	45330,	45349,	45411,	45420,	
             45438,	45497,	45500,	45519,	45527,	45594)

comercio <- c(45608,	50105,	50202,	50300,	50415,	50423,	50504,	51110,	
              51128,	51136,	51144,	51152,	51160,	51179,	51187,	51195,	
              51217,	51225,	51314,	51322,	51330,	51349,	51357,	51365,	
              51373,	51390,	51411,	51420,	51438,	51446,	51454,	51462,	
              51470,	51497,	51519,	51527,	51535,	51543,	51551,	51594,	
              51616,	51624,	51632,	51640,	51659,	51691,	51918,	51926,	
              52116,	52124,	52132,	52140,	52159,	52213,	52221,	52230,	
              52248,	52299,	52310,	52329,	52337,	52418,	52426,	52434,	
              52442,	52450,	52469,	52477,	52493,	52507,	52612,	52620,	
              52698)

Rais_2017_trabalhador_cwb <- Rais_2017_trabalhador_cwb %>% mutate(
  salario=str_replace(remmedr,",",".")) %>% mutate(salario=as.numeric(salario))

####

rais_17_empregos <- shp_rais_sf_17 %>% 
  mutate(emp_indust=if_else(clascnae95%in%industrial,estcltout,0),
         emp_servico=if_else(clascnae95%in%servico,estcltout,0),
         emp_comercio=if_else(clascnae95%in%comercio,estcltout,0)) %>% 
  left_join(Rais_2017_trabalhador_cwb,filter(salario>0),by="identificad_m") %>% 
  dplyr::group_by(identificad_m) %>% dplyr::summarise(
    salario=sum(salario,na.rm=T),
    quant_empregos=sum(estcltout),
    emp_indust=sum(emp_indust),
    emp_servico=sum(emp_servico),
    emp_comercio=sum(emp_comercio)) 

rais_17_empregos_sal <- rais_17_empregos %>% left_join(Rais_2017_trabalhador_cwb,by="identificad_m")

##### QUARTILES

Rais_2017_trabalhador_cwb_sal <- Rais_2017_trabalhador_cwb_sf %>% 
  #distinct(cpf_m,.keep_all=TRUE) %>% 
  mutate(salario=str_replace(remmedr,",",".")) %>% mutate(salario=as.numeric(salario)) %>% 
  mutate(quartile=as.numeric(split_quantile(salario,type = 4)))

Rais_2017_trabalhador_cwb_sal_sf <- Rais_2017_trabalhador_cwb_sal %>% 
  left_join(rais_17_empregos,by="identificad_m") %>% 
  distinct(cpf_m,.keep_all=TRUE)

RM_CWB_EMPREGOS_por_trabalhadorq4 <- st_join(
  RM_CWB_censo_Merg,select(
    st_as_sf(Rais_2017_trabalhador_cwb_sal_sf_quart4),geom,salario,quartile)) %>% 
  group_by(code_tract) %>% 
  dplyr::summarise(Total.emp.quart1=sum(quartile,na.rm=T))

RM_CWB_EMPREGOS_por_trabalhadorq4 <- st_join(
  RM_CWB_censo_Merg,select(
    st_as_sf(Rais_2017_trabalhador_cwb_sal_sf_quart4),geom,salario,quartile)) %>% 
  group_by(code_tract) %>% 
  dplyr::summarise(Total.emp.quart4=sum(quartile,na.rm=T))

RM_CWB_EMPREGOS_por_trabalhadorq3e4 <- st_join(
  RM_CWB_censo_Merg,select(
    st_as_sf(Rais_2017_trabalhador_cwb_sal_sf_quart3e4),geom,salario,quartile)) %>% 
  group_by(code_tract) %>% 
  dplyr::summarise(Total.emp.quart3e4=sum(quartile,na.rm=T))

##### TOTAL

RM_CWB_EMPREGOS <- st_join(RM_CWB_censo_Merg,rais_17_empregos)

RM_CWB_EMP <- RM_CWB_EMPREGOS %>%  
  group_by(code_tract,geom) %>% 
  dplyr::summarise(#`Rendimento Médio`=mean(salario,na.rm = T),
            Total.emp=sum((emp_comercio+emp_indust+emp_comercio),na.rm = T),
            Total.emp_ind=sum(emp_indust,na.rm = T),
            Total.emp_ser=sum(emp_servico,na.rm = T),
            Total.emp_com=sum(emp_comercio,na.rm = T))

RM_CWB_EMP <- RM_CWB_EMP %>% 
  mutate(km2=units::drop_units(st_area(RM_CWB_EMP)/1000000)) %>% mutate(
  `Employment density`=log(1+(`Total.emp`/km2)))

mapview(RM_CWB_EMP,zcol="Employment density",
  col.regions=brewer.pal(7,"Spectral"),alpha=0) + (munseat) + (limites_cwb)

###### ACESSIBILIDADES ----
#### CUMULATIVA

Boijosly_results <- read.csv(
  "D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/PROJETO OPORTUNIDADES/Dados disponibilizados/resultsCuritiba.csv",
  sep=";") %>% mutate_all(funs(as.numeric(.)))

Boijosly_shp <- Boijosly_shp %>% st_as_sf(coordinates=geom,crs=4326) %>% 
  st_transform(crs=31982)

RM_CWB_EMP_planar_quart2 <- RM_CWB_EMPREGOS_por_trabalhadorq2 %>% 
  st_as_sf(coords=geom,crs=4326) %>% 
 st_transform(crs=31982)


RM_CWB_EMP_planar_quart3e4 <- RM_CWB_EMPREGOS_por_trabalhadorq3e4 %>% 
  st_as_sf(coords=geom,crs=4326) %>% 
  st_transform(crs=31982)


#### GARBAGE ----

#RM_CWB_EMP_planar <- RM_CWB_EMP %>% st_as_sf(coords=geom,crs=4326) %>% 
 # st_transform(crs=31982)

#Boijosly_empregos <- st_join(Boijosly_empregos,shp_rais_sf_17) %>% 
#  group_by(TARGET_FID,geom) %>% dplyr::summarise(empregos=sum(estcltout))

#interpolacao <- areal::aw_interpolate(RM_CWB_EMP,tid = code_tract,
#  source=Boijosly_empregos, sid = empregos,weight = "sum",output = "tibble",
#  extensive = "empregos")

#Boijosly_fromcensus <- st_join(Boijosly_shp,RM_CWB_EMP_planar) %>% 
#  distinct(TARGET_FID,.keep_all = TRUE)


#RM_CWB_EMP_planar <- RM_CWB_EMP_planar %>% mutate(qtj=`Total.emp`)

######## garbage
rais_17_empregos_transformed <- st_transform(rais_17_empregos,crs=31982)
st_crs(rais_17_empregos_transformed)

boijoslyfromrais <- st_join(Boijosly_shp,rais_17_empregos_transformed) %>% 
  group_by(TARGET_FID) %>% 
  dplyr::summarise(empregos=sum(estcltout))

ar_validate(source = RM_CWB_EMP_planar_quart2, target = Boijosly_shp,
            varList = "qtj", method = "aw",
            verbose = TRUE)


Boijosly_fromcensus_interp <- aw_interpolate(
    Boijosly_shp,
    source = st_buffer(RM_CWB_EMP_planar,0),
    sid = Total.emp, tid = TARGET_FID,
    weight = "sum", output = "tibble", 
    extensive = c("Total.emp"))

Boijosly_fromcensus_interp_2 <- aw_interpolate(
  Boijosly_shp,
  source = st_buffer(RM_CWB_EMP_planar_quart2,0),
  sid =Total.emp.quart2, tid = TARGET_FID,
  weight = "sum", output = "tibble", 
  extensive = c("Total.emp.quart2"))

Boijosly_fromcensus_interp_3e4 <- aw_interpolate(
  Boijosly_shp,
  source = st_buffer(RM_CWB_EMP_planar_quart3e4,0),
  sid = Total.emp.quart3e4, tid = TARGET_FID,
  weight = "sum", output = "tibble", 
  extensive = c("Total.emp.quart3e4"))

##### TEM QUE TRAZER OS EMPREGOS INDUSTRIA,SERVICO E COMERCIO POR INTERPOLACAO + $

Boijosly_fromcensus_interp$geom <- Boijosly_shp$geom

Boijosly_fromcensus_interp <- st_as_sf(Boijosly_fromcensus_interp)

###########

mapview(Boijosly_empregos,
  zcol="empregos",col.regions=brewer.pal(7,"Reds"),alpha=0.1)

mapview(RM_CWB_EMP,
        zcol="Quantity of Jobs",col.regions=brewer.pal("4","Spectral"),alpha=0)


######
Acess_cum <- Boijosly_fromcensus_interp %>% 
  left_join(Boijosly_results,by=c("TARGET_FID"="Destino")) %>% drop_na(qtj) %>% 
  mutate(cumulativo=if_else(TEMPO.MIN<60,qtj,0)) %>% #,
  #cumulativo_ind=if_else(TEMPO.MIN<=45,Total.emp_ind,0),
  #cumulativo_com=if_else(TEMPO.MIN<=45,Total.emp_com,0),
  #cumulativo_ser=if_else(TEMPO.MIN<=45,Total.emp_ser,0)) %>% 
  group_by(ORIG_FID) %>% 
  dplyr::summarise(cumulativo=sum(cumulativo,na.rm=T)) %>% #,
  #cumulativo_ind=sum(cumulativo_ind,na.rm = T),
  #cumulativo_com=sum(cumulativo_com,na.rm = T),
  #cumulativo_ser=sum(cumulativo_ser,na.rm = T),) %>% 
  mutate(`Percent cumulativo`=cumulativo/sum(cumulativo)*100,
         #`Percent cumulativo_ind`=cumulativo_ind/sum(cumulativo_ind)*100,
         #`Percent cumulativo_com`=cumulativo_com/sum(cumulativo_com)*100,
         #`Percent cumulativo_ser`=cumulativo_ser/sum(cumulativo_ser)*100,
         #TARGET_FID=as.numeric(TARGET_FID))
         ORIG_FID=as.numeric(ORIG_FID))
           
Acess_cum_2 <- Boijosly_fromcensus_interp_2 %>% 
  left_join(Boijosly_results,by=c("TARGET_FID"="Destino")) %>% 
  mutate(cumulativo15=if_else(TEMPO.MIN<15,Total.emp.quart2,0),
         cumulativo30=if_else(TEMPO.MIN<30,Total.emp.quart2,0),
        cumulativo45=if_else(TEMPO.MIN<45,Total.emp.quart2,0),
        cumulativo60=if_else(TEMPO.MIN<60,Total.emp.quart2,0),
        cumulativomil=if_else(TEMPO.MIN<1000,Total.emp.quart2,0),
        cumulativomax=if_else(TEMPO.MIN<8735,Total.emp.quart2,0)) %>% 
  group_by(ORIG_FID) %>% 
  dplyr::summarise(cumulativo15=sum(cumulativo15,na.rm=T),
                   cumulativo30=sum(cumulativo30,na.rm = T),
                   cumulativo45=sum(cumulativo45,na.rm = T),
                   cumulativo60=sum(cumulativo60,na.rm = T),
                   cumulativomil=sum(cumulativomil,na.rm = T),
                   cumulativomax=sum(cumulativomax,na.rm = T)) %>% 
  mutate(`Percent cumulativo15`=cumulativo15/sum(cumulativo15,na.rm = T)*100,
         `Percent cumulativo30`=cumulativo30/sum(cumulativo30,na.rm = T)*100,
         `Percent cumulativo45`=cumulativo45/sum(cumulativo45,na.rm = T)*100,
         `Percent cumulativo60`=cumulativo60/sum(cumulativo60,na.rm = T)*100,
         `Percent cumulativomil`=cumulativomil/sum(cumulativomil,na.rm = T)*100,
         `Percent cumulativomax`=cumulativomil/sum(cumulativomax,na.rm = T)*100,
         ORIG_FID=as.numeric(ORIG_FID))

Acess_cum_2$geom <- Acess_grav_shp$geom

Acess_cum_2 <- st_as_sf(Acess_cum_2)

Acess_cum_shp_1 <- st_join(Boijosly_shp,st_as_sf(Acess_cum_1)) %>% 
  distinct(ORIG_FID.x, .keep_all = TRUE)

mapview(Acess_cum_shp,
  zcol="Percent cumulativo",col.regions=brewer.pal(7,"Spectral"), alpha=0) +
  (munseat) + (limites_cwb)

st_write(st_join(RM_CWB_EMP_planar_quart1,Acess_cum_shp_1),"Acess_cum_census_QUART1.gpkg")

###### GRAVITACIONAL ----

Boijosly_results <- read.csv(
  "D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/PROJETO OPORTUNIDADES/Dados disponibilizados/resultsCuritiba.csv",
  sep=";") %>% mutate_all(funs(as.numeric(.))) %>% 
  mutate(if_else(TEMPO.MIN==0,1,TEMPO.MIN)) 

Boijosly_shp <- Boijosly_shp %>% st_transform(4674)

Acess_grav <- Boijosly_fromcensus_interp %>% 
  left_join(Boijosly_results,by=c("TARGET_FID"="Destino")) %>% 
  mutate(gravitational=qtj/(exp(0.01154*TEMPO.MIN)),
         gravitational.ind=Total.emp_ind/(exp(0.01154*TEMPO.MIN)),
         gravitational.com=Total.emp_com/(exp(0.01154*TEMPO.MIN)),
         gravitational.ser=Total.emp_ser/(exp(0.01154*TEMPO.MIN))) %>% 
  group_by(TARGET_FID) %>% 
  dplyr::summarise(gravitational=sum(gravitational,na.rm=T),
                   gravitational.ind=sum(gravitational.ind,na.rm=T),
                   gravitational.com=sum(gravitational.com,na.rm=T),
                   gravitational.ser=sum(gravitational.ser,na.rm=T)) %>% 
  mutate(TARGET_FID=as.numeric(TARGET_FID))

Acess_grav_shp <- st_join(Boijosly_shp,Acess_grav) %>% 
  distinct(TARGET_FID.x, .keep_all = TRUE) %>% 
  mutate(loggrav=log(1+gravitational),
         loggrav.ind=log(1+gravitational.ind),
         loggrav.com=log(1+gravitational.com),
         loggrav.ser=log(1+gravitational.ser))

mapview(Acess_grav_shp,
  zcol="loggrav",col.regions=brewer.pal(7,"Spectral"), alpha=0) + 
  (limites_cwb)


RM_CWB_Grav$geom <- RM_CWB_EMP$geom 

RM_CWB_Grav$loggrav.ind <- RM_CWB_Grav.ind$loggrav.ind

RM_CWB_Grav$loggrav.com <- RM_CWB_Grav.com$loggrav.com

RM_CWB_Grav$loggrav.ser <- RM_CWB_Grav.ser$loggrav.ser


RM_CWB_Grav <- st_as_sf(RM_CWB_Grav)

mapview(RM_CWB_Grav,zcol="loggrav.ind",
  col.regions=brewer.pal(7,"Spectral"),alpha=0) + (limites_cwb) + 
  (munseat) + (Boijosly_fromcensus_interp)


setwd("D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/SHPsMaps/")
st_write(RM_CWB_Grav,"RM.cwb.gravsectors.gpkg")

base_usada_na_dissertacao <- base_usada_na_dissertacao %>% 
  mutate(setor.trabalho=if_else(COD_DESTINO%in%c(1:3),
  IBGE_DESTINO,SETORIBGEORIGEM),
  setor.residencia=if_else(COD_DESTINO%in%c(9),IBGE_DESTINO,SETORIBGEORIGEM))

df_reg_grav <- base_usada_na_dissertacao %>% 
  left_join(RM_CWB_Grav,by=c("setor.residencia"="code_tract")) %>%  
  mutate(quartile=as.numeric(split_quantile(RENDA,type = 4)),
         zeros=0) %>% 
  mutate(origemxdestino=paste0(ZONA_ORIGEM,zeros,ZONA_DESTINO))

cor(df_reg_grav$lnDistRios2,df_reg_grav$gravitational,"complete.obs")

## saving in dta
write_dta(df_reg_grav,file.path(getwd(),"D","Economia","MESTRADO",
                                "dfreggravit.dta"))

xlsx::write.xlsx(df_reg_grav,"df_reg_grav.res.sectors.xlsx",sheetName = "sheetreg",
                 col.names = TRUE)

##### DTRANSIT

pop_above_age <- c('V050','V051','V052','V053','V054','V055','V056','V057','V058','V059','V060','V061','V062','V063')

func_dtransit <- function(grupo_renda,output.adress,output.name,shape_cum,cumoutput){
 
  a <- readxl::read_excel(
  "D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/Censo 2010/PR/Base informaçoes setores2010 universo PR/EXCEL/Basico_PR.xls") %>% 
    mutate_at(.vars = vars(
      'V001','V002','V003','V004','V005','V006','V007','V008','V009'),#,'V050',
      #'V051','V052','V053','V054','V055','V056','V057','V058','V059','V060',
      #'V061','V062','V063',
      #'V010','V011','V012',
      funs(as.numeric(.))) %>% 
    drop_na(V011) %>% mutate(quartil=split_quantile(V011,type=4)) 
  
  b <- RM_CWB_SETORES %>% 
  mutate(code_tract=as.numeric(code_tract)) %>% 
  left_join(a, by=c("code_tract"="Cod_setor")) %>% filter(quartil==grupo_renda)
  c1 <- shape_cum %>% st_as_sf(coords=geom,crs=4326) %>% 
    st_transform(crs=4674)
  c <- st_join(b,c1) %>% distinct(code_tract,.keep_all = TRUE) 
  #c <- aw_interpolate(b, source = Acess_cum_shp, 
  #                    tid = code_tract, sid=cumulativo,
  #                    extensive = "cumulativo",weight = "sum", 
  #                    output = "tibble")
  
  c$area  <- units::drop_units(st_area(c)/1000000)
  
  c <- c %>% drop_na(ORIG_FID) %>% distinct(code_tract,.keep_all = TRUE) %>% 
    mutate(
      wi=(V003/sum(V003,na.rm=T)),
      ca=(cumoutput/sum(cumoutput,na.rm=T))) %>% 
    mutate(Dtransit=0.5*sum(abs(wi-ca),na.rm= T),
    pop.density=V003/area)
  
  #### visualizacoes e outputs
  print(min(c$Dtransit))
  print(c(summary(c$V011,na.rm=T),sd(c$V011,na.rm = T)))
  print(mapview(
  c,zcol="pop.density",col.regions=brewer.pal(7,
  "Spectral"),alpha=0.1)+(limites_cwb))
  #setwd(output.adress)
  #return(st_write(c,output.name))
  print("Total households number")
  print(as.numeric(sum(c$V003,na.rm = T)))
  return(min(c$Dtransit))
}

#outputdtransittotal <- func_dtransit(grupo_renda = c("1","2","3","4"),
 #    output.adress="D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/SHPsMaps/",
#                           output.name="cwb_rm_dtransit_renda_4.gpkg")


outputdtransit1 <- func_dtransit(grupo_renda = "1",
    output.adress="D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/SHPsMaps/",
    output.name="cwb_rm_dtransit_renda_4.gpkg",Acess_cum_1,cumulativo15)

outputdtransit2 <- func_dtransit(grupo_renda = "2",
   output.adress="D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/SHPsMaps/",
  output.name="cwb_rm_dtransit_renda_2.gpkg")

outputdtransit3 <- func_dtransit(grupo_renda = "3",
   output.adress="D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/SHPsMaps/",
   output.name="cwb_rm_dtransit_renda_3.gpkg")

outputdtransit4 <- func_dtransit(grupo_renda = "4",
   output.adress="D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/SHPsMaps/",
   output.name="cwb_rm_dtransit_renda_4.gpkg")


##### GRAFICOS ----
##### DTRANSIT


Dtransit.graph <- as.data.frame(cbind(c("Total","Poorest","Middle-poorest",
  "Middle-richest","Richest"),c(Dtransit_total,Dtransit_1,Dtransit_2,
  Dtransit_3,Dtransit_4)))
colnames(Dtransit.graph) <- c("Household income","DTransit")

order <- c("Poorest","Middle-poorest","Middle-richest","Richest","Total")

Dtransit.graph$categories <- factor(Dtransit.graph$`Household income`, levels= order)

# Change the colors manually
ggplot(data=Dtransit.graph, 
  aes(x=categories, y=substr(DTransit,start = 0,stop = 4), 
      fill=`Household income`)) + ylab("Dtransit") + xlab("Income group") +
  geom_bar(stat="identity",color="black",position=position_dodge(),width = 0.4)+
  theme_minimal() + 
# Use custom colors
  scale_fill_manual(values=c('#999999','#E69F00',"#56B4E9","#e956e7")) + 
# Use brewer color palettes
 scale_fill_brewer(palette="Greens")

##### LINES

RM_CWB_SETORES_Cent <- st_centroid(RM_CWB_SETORES) 

RM_CWB_SETORES_Cent$Dist_cbd <- units::drop_units(
  st_distance(RM_CWB_SETORES_Cent,cwb_cbd))

RM_CWB_SETORES_Cent <- as.data.frame(RM_CWB_SETORES_Cent) %>% 
  mutate(code_tract=as.numeric(code_tract)) 

CWB_Censo_2010df <- CWB_Censo_2010df %>% drop_na(V011)

RM_CWB_SETORES_Cent <- RM_CWB_SETORES_Cent %>% 
  mutate(code_tract=as.numeric(code_tract)) %>%  
  left_join(CWB_Censo_2010df,by=c("code_tract"="Cod_setor")) %>% 
  drop_na(V011) %>% mutate(quartil=split_quantile(V011,type = 4))

RM_CWB_SETORES_Cents <- RM_CWB_SETORES_Cent %>% filter(quartil==2) %>% 
  transform(Dist_cbd=cut(Dist_cbd,40,ordered_result=TRUE)) 

RM_CWB_SETORES_Cents2 <- ddply(RM_CWB_SETORES_Cents,
 "Dist_cbd", summarize,totVal = sum(V011,na.rm = T)) %>% mutate(
 `Distance to CBD`=as.numeric(sprintf("%4.f",Dist_cbd)),
 `Distancia ate o CBD`=as.numeric(Dist_cbd)*1000,
 percentage2=(totVal/sum(totVal,na.rm = T))*100)

RM_CWB_SETORES_Centsbinded <- RM_CWB_SETORES_Cents1 %>% left_join(
  select(RM_CWB_SETORES_Cents2,`Distance to CBD`,percentage2),by="Distance to CBD")

ggplot(RM_CWB_SETORES_Centsbinded,aes(x=`Distance to CBD`)) +
  geom_line(aes(y=percentage1,color="blue"),position = position_dodge(0.2)) +
  geom_line(aes(y=percentage2,color="darkred",linetype="twodash"))

#####
dfggplo2variables <- RM_CWB_SETORES_Centsbinded %>% 
  select(`Distance to CBD`,percentage1,percentage2) %>% 
  gather(key = "variable","value", -`Distance to CBD`)

ggplot(dfggplo2variables,aes(x=`Distance to CBD`,y=value)) +
  geom_line(aes(color=variable,linetype=variable)) +
  scale_color_manual(values=c("darkred","steelblue")) +
  ylab("Percentage") 

###### EMPREGOS
RM_CWB_EMP <- st_join(RM_CWB_EMPREGOS,shp_rais_sf_17) #%>% 
#distinct(code_tract, .keep_all = TRUE)

RM_CWB_EMP <- RM_CWB_EMP %>% group_by(code_tract) %>% 
  dplyr::summarise(Wages=mean(`Rendimento Médio`,na.rm=T),
                   `Quantity of Jobs`=sum(estcltout,na.rm = T))


func_graf_dist_cbd <- function(){
  
  a <- st_centroid(RM_CWB_SETORES) 
  
  a$Dist_cbd <- units::drop_units(st_distance(a,cwb_cbd))
  
  b <- as.data.frame(a) %>% mutate(code_tract=as.numeric(code_tract))  
  
  c <- CWB_Censo_2010df %>% drop_na(V011)
  
  d <- b %>% mutate(code_tract=as.numeric(code_tract)) %>%  
    left_join(c,by=c("code_tract"="Cod_setor")) %>% drop_na(V011) %>% 
    mutate(quartile=split_quantile(V011,type = 4))
  
  e <- d %>% mutate(
    `holseholds quartile 1`=if_else(quartile==1,V001,0),
    `holseholds quartile 2`=if_else(quartile==2,V001,0),
    `holseholds quartile 3`=if_else(quartile==3,V001,0),
    `holseholds quartile 4`=if_else(quartile==4,V001,0)) %>% 
    transform(Dist_cbd=cut(Dist_cbd,40,ordered_result=TRUE)) 
  
  f <- e %>% left_join(RM_CWB_EMP,by="code_tract") %>% drop_na(Wages) %>% 
    mutate(quartilewages=split_quantile(Wages,type = 4)) %>%  
   mutate(jobs.1=if_else(quartilewages==1,`Quantity of Jobs`,0),
           jobs.2=if_else(quartilewages==2,`Quantity of Jobs`,0),
           jobs.3=if_else(quartilewages==3,`Quantity of Jobs`,0),
           jobs.4=if_else(quartilewages==4,`Quantity of Jobs`,0))

  g <- ddply(f,"Dist_cbd", summarize,
  tot.residents.1 = sum(holseholds.quartile.1,na.rm = T),
  tot.residents.2 = sum(holseholds.quartile.2,na.rm = T),
  tot.residents.3 = sum(holseholds.quartile.3,na.rm = T),
  tot.residents.4 = sum(holseholds.quartile.4,na.rm = T),
  #tot.jobs = `Quantity of Jobs`/sum(`Quantity of Jobs`,na.rm = T),
  jobs.1 = sum(jobs.1, na.rm = T),
  jobs.2 = sum(jobs.2, na.rm = T),
  jobs.3 = sum(jobs.3, na.rm = T),
  jobs.4 = sum(jobs.4, na.rm = T)) %>% mutate(
  `Distance to CBD`=as.numeric(sprintf("%4.f",Dist_cbd)),
  `% jobs of wage quartile 1`=(jobs.1/sum(jobs.1,na.rm = T))*100,
  `% jobs of wage quartile 2`=(jobs.2/sum(jobs.2,na.rm = T))*100,
  `% jobs of wage quartile 3`=(jobs.3/sum(jobs.3,na.rm = T))*100,
  `% jobs of wage quartile 4`=(jobs.4/sum(jobs.4,na.rm = T))*100,
  `% of households income quartile 1`=(tot.residents.1/sum(tot.residents.1,na.rm = T))*100,
  `% of households income quartile 2`=(tot.residents.2/sum(tot.residents.2,na.rm = T))*100,
  `% of households income quartile 3`=(tot.residents.3/sum(tot.residents.3,na.rm = T))*100,
  `% of households income quartile 4`=(tot.residents.4/sum(tot.residents.4,na.rm = T))*100)
  
  return(g)
}

dfggplot_1.2 <- g %>% 
  select(`Distance to CBD`,`% of households income quartile 1`,
         `% of households income quartile 2`,
         `% jobs of wage quartile 1`,`% jobs of wage quartile 2`) %>% 
  gather(key = "variable","value", -`Distance to CBD`)

dfggplot_3.4 <- g %>% 
  select(`Distance to CBD`,`% of households income quartile 3`,
         `% of households income quartile 4`,
  `% jobs of wage quartile 3`,`% jobs of wage quartile 4`) %>% 
  gather(key = "variable","value", -`Distance to CBD`)

ggplot(dfggplot_3.4,aes(x=`Distance to CBD`,y=value)) +
  geom_line(aes(color=variable),linetype="dashed",size=0.8) + 
  geom_point(aes(color=variable)) +
  scale_color_manual(values=c("red","black","blue","yellow")) + 
  ylim(0,32) + xlim(0,25) + 
  ylab("Percentage") + xlab("Linear Distance to the CBD of Curitiba") +
  theme(panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "black"),
    legend.position=c(0.75,0.75)) 
  

######
RM_CWB_SETORES$code_tract <- as.numeric(RM_CWB_SETORES$code_tract)

RM_CWB_cbds <- as.data.frame(RM_CWB_EMP) %>% 
  left_join(RM_CWB_SETORES, by = "code_tract") 

RM_CWB_cbds <- RM_CWB_cbds %>% group_by(name_muni) %>% 
  dplyr::summarise(`CBD de emprego`=max(`Quantity of Jobs`))

RM_CWB_cbds <- RM_CWB_cbds %>% left_join(RM_CWB_EMP,
    by=c("CBD de emprego"="Quantity of Jobs")) %>% 
  distinct(name_muni,.keep_all = TRUE) %>% st_as_sf() %>% st_centroid()

mapview(RM_CWB_cbds)


munseat <- read_municipal_seat(year=2010) %>% 
  filter(code_muni%in%c(mun_mac_geobr))

mapview(munseat)

setwd("D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/SHPsMaps/")
st_write(munseat,"cbds.municipios.gpkg")

##### ANALISE OUTPLUT
quartis.od <- df_reg_grav %>% 
  mutate(quartile=split_quantile(RENDA, type = 4)) %>% group_by(quartile) %>% 
  dplyr::summarise(mean=weighted.mean(RENDA,pesofinal),
                   standdev=sd(RENDA),
                   minimun=min(RENDA),
                   maximum=max(RENDA))


##### MODE SHARE FUNCTION

func_mode_share <- function(grupo_renda){
  
  a <- readxl::read_excel(
    "D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/Censo 2010/PR/Base informaçoes setores2010 universo PR/EXCEL/Basico_PR.xls") %>% 
    mutate_at(.vars = vars(
      'V001','V002','V003','V004','V005','V006','V007','V008','V009',
      'V010','V011','V012'),
      funs(as.numeric(.))) %>% 
    drop_na(V011) %>% mutate(quartil=split_quantile(V011,type=4)) 
  
  b <- RM_CWB_SETORES %>% 
    mutate(code_tract=as.numeric(code_tract)) %>% 
    left_join(a, by=c("code_tract"="Cod_setor")) %>% filter(quartil%in%grupo_renda)
  
  c <- df_od_peso %>% filter(COD_ORIGEM%in%c(1,2,3)|COD_.DESTINO%in%c(1,2,3)) %>% mutate(
  ZONA_RESIDENCIA=if_else(COD_ORIGEM==9,ZONA_ORIGEM,ZONA_DESTINO),
  COD_CENS_RESIDENCIA=if_else(COD_ORIGEM==9,SETOR.IBGE.ORIGEM,IBGE_DESTINO)) %>% 
    mutate(Mode_share_TP=if_else(COD_MEIO%in%c(1,2,3,9,10,11),pesofinal,0),
           Mode_share_private=if_else(COD_MEIO%in%c(6:8,12),pesofinal,0))  
  
  d <- c %>% group_by(ZONA_RESIDENCIA) %>% 
    dplyr::summarise(
 `Mode Share Public Transport`=sum(Mode_share_TP)/(sum(Mode_share_private)+sum(Mode_share_TP)),
 `Mode Share Private Transport`=sum(Mode_share_private)/(sum(Mode_share_private)+sum(Mode_share_TP)))
  
  e <- c %>% left_join(d,by = "ZONA_RESIDENCIA") %>%    
  distinct(COD_PESSOA, .keep_all = TRUE)
  
  f <- b %>% left_join(e,by =c("code_tract"="COD_CENS_RESIDENCIA")) %>%    
    distinct(COD_PESSOA, .keep_all = TRUE)
  
  g <- group_by(f) %>% summarise(media_share=mean(`Mode Share Public Transport`,na.rm=T))
  
  print(head(g))
  #print(mean(f$`Mode Share Public Transport`,na.rm = T))

}

func_mode_share(grupo_renda = 1)
func_mode_share(grupo_renda = 2)
func_mode_share(grupo_renda = 3)
func_mode_share(grupo_renda = 4)

##### INSTRUMENT ----

projeto_reboucas <- st_read(
  "D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/Instrumento historico/projeto_reboucas.gpkg"
)

ferrovias <- st_read(
  "D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/SHPsMaps/ferrovias_1892.gpkg") %>% 
  st_transform(crs=4674)

od_points <- st_read("D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/SHPsMaps/centroides_od.gpkg") %>% 
  st_transform(crs=4674)

st_crs(od_points)

mapview(ferrovias) + (od_points)

distance_ferros <- st_nearest_feature(ferrovias,od_points)

ferro_points <- st_cast(st_geometry(ferrovias), "POINT") 
mapview(ferro_points)

st_write(ferro_points,"ferro_points1982.gpkg")

st_crs(ferro_points)
st_crs(od_points)

###### worked

zonas_od <- base_dissertacao %>% distinct(ZONA_ORIGEM) 

pts_dist <- st_connect(od_points, ferro_points)

min_distance <- as.data.frame(cbind(od_points,st_length(pts_dist))) %>% 
  mutate(dist.linear=ceiling(units::drop_units(st_length.pts_dist.)),
         logdist.linear=log(ceiling(units::drop_units(st_length.pts_dist.))))

df_instrument_ferr <- df_reg_grav %>% 
  left_join(min_distance, by=c("ZONA_ORIGEM"="ZONA")) %>% 
  mutate(geom.x=NULL,geom.y=NULL)

setwd("D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/Outputs/")

write.csv(df_instrument_ferr,"df_intrument_ferr.csv")

###### GRAVIT ROBUSTO ----

Census_centroids <- st_read(
  "D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/Instrumento historico/centroides_setores.gpkg") %>% 
  st_transform(crs=31982)

census_join_boijosly <- Census_centroids %>% st_join(Acess_grav_shp) %>% 
  mutate(code_tract=as.character(code_tract))

census_join_gravrobust <- as.data.frame(RM_CWB_SETORES) %>% 
  left_join(census_join_boijosly,by="code_tract") %>% 
  st_as_sf()

mapview(census_join_gravrobust,
        zcol="loggrav.y",col.regions=brewer.pal(7,"Spectral"), alpha=0) + 
  (limites_cwb)

##### GRAVIT ROBUSTO COM SALARIO

Rais_2017_trab_assalariados <- cbind(Rais_2017_trabalhador_cwb_sf,Rais_2017_trabalhador_cwb[,93]) %>% 
  mutate(salario=as.numeric(salario)) %>% 
  filter(salario>0) %>% group_by(identificad_m) %>% 
  dplyr::summarise(salario=mean(salario,na.rm=T)) 

rais_17_empregos_com_salarios <- rais_17_empregos %>% 
  left_join(Rais_2017_trab_assalariados,by='identificad_m') %>% 
  filter(salario>0) %>% 
 mutate(quartil=split_quantile(salario,type = 4))

Rais_2017_trabalhador_cwb_sal_sf_quart1 <- filter(
  Rais_2017_trabalhador_cwb_sal_sf,quartile==1) %>% drop_na(radiccnpj_m.y) %>% 
  mutate(geom=geometry)

st_write(select(Rais_2017_trabalhador_cwb_sal_sf_quart1,salario,quartile,geom),"Rais_2017_trabalhador_cwb_sal_qiartile1.gpkg")

Rais_2017_trabalhador_cwb_sal_sf_quart2 <- filter(
  Rais_2017_trabalhador_cwb_sal_sf,quartile==2) %>% drop_na(radiccnpj_m.y) %>% 
  mutate(geom=geometry)

st_write(select(Rais_2017_trabalhador_cwb_sal_sf_quart2,salario,quartile,geom),"Rais_2017_trabalhador_cwb_sal_qiartile2.gpkg")

Rais_2017_trabalhador_cwb_sal_sf_quart3 <- filter(
  Rais_2017_trabalhador_cwb_sal_sf,quartile==3) %>% drop_na(radiccnpj_m.y) %>% 
  mutate(geom=geometry) %>% st_as_sf()

st_write(select(Rais_2017_trabalhador_cwb_sal_sf_quart3,salario,quartile,geom),"Rais_2017_trabalhador_cwb_sal_qiartile3.gpkg")

Rais_2017_trabalhador_cwb_sal_sf_quart4 <- filter(
  Rais_2017_trabalhador_cwb_sal_sf,quartile==4) %>% drop_na(radiccnpj_m.y) %>% 
  mutate(geom=geometry)

st_write(select(Rais_2017_trabalhador_cwb_sal_sf_quart4,salario,quartile,geom),"Rais_2017_trabalhador_cwb_sal_qiartile4.gpkg")

######
Rais_2017_trabalhador_cwb_sal_sf_quart1e2 <- filter(
  Rais_2017_trabalhador_cwb_sal_sf,quartile<3) %>% drop_na(radiccnpj_m.y) %>% 
  mutate(geom=geometry)

Rais_2017_trabalhador_cwb_sal_sf_quart3e4 <- filter(
  Rais_2017_trabalhador_cwb_sal_sf,quartile>=3) %>% drop_na(radiccnpj_m.y) %>% 
  mutate(geom=geometry)

####### INDICES

Boijosly_fromcensus_interp_3e4$geom <- Boijosly_shp$geom

Acess_grav_quart3e4 <- st_as_sf(Boijosly_fromcensus_interp_3e4) %>% 
  left_join(Boijosly_results,by=c("TARGET_FID"="Destino")) %>% 
  mutate(gravitational=Total.emp.quart3e4/(exp(0.01154*TEMPO.MIN))) %>% 
  group_by(TARGET_FID) %>% 
  dplyr::summarise(gravitational=sum(gravitational,na.rm=T)) %>% 
  mutate(TARGET_FID=as.numeric(TARGET_FID)) %>% st_as_sf()

Acess_grav_quart3e4$geom <- Boijosly_shp$geom

Acess_grav_quart3e4 <- st_as_sf(Acess_grav_quart3e4)

Acess_grav_q3e4_shp <- st_join(Boijosly_shp,Acess_grav_quart3e4) %>% 
  distinct(TARGET_FID.x, .keep_all = TRUE) %>% 
  mutate(loggrav=log(1+gravitational))

census_join_boijosly_quartile3e4 <- Census_centroids %>% st_join(Acess_grav_q3e4_shp)

setwd("D:/Economia/MESTRADO/DISSERTACAO/Insumos/Dados/R/Outputs/")

write.csv(census_join_boijosly_quartile3e4,"Acess_grav_quart3e4.csv")
