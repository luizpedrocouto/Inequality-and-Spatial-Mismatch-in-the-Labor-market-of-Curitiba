
cd "D:\Economia\MESTRADO\DISSERTACAO\Insumos\Dados\R\Outputs"

## VARIAVEL INSTRUMENTAL
ivregress 2sls LNRENDA DummySexo lnIdade lnIdadeaoquad D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_comercio D_Industria (loggravitationaly = lninst_rios_5km) if D_Individual==0 & D_SITSETOR_RES==1 [fweight = pesofinal]

estat firststage


ivregress 2sls LNRENDA DummySexo lnIdade lnIdadeaoquad D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_comercio D_Industria (loggravindr = lninst_rios_5km) if D_Individual==0 & D_Industria==1 & D_SITSETOR_RES==1 [fweight = pesofinal]
estat firststage

ivregress 2sls LNRENDA DummySexo lnIdade lnIdadeaoquad D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_comercio D_Industria (loggravcomr = lninst_rios_5km) if D_Individual==0 & D_comercio==1 & D_SITSETOR_RES==1 [fweight = pesofinal]
estat firststage

ivregress 2sls LNRENDA DummySexo lnIdade lnIdadeaoquad D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_comercio D_Industria (loggravserr = lninst_rios_5km) if D_Individual==0 & D_servico==1 & D_SITSETOR_RES==1 [fweight = pesofinal]
estat firststage


summarize loggravitationaly loggravindr loggravcomr loggravserr gravitattionalyr

######
gen D_SIT_RES_TRAB = (D_SITSETOR_RES*D_SITSETOR_TRAB)

reg LNRENDA lnDistCBD DummySexo lnIdade lnIdadeaoquad D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_comercio D_Industria D_SITSETOR_RES D_SITSETOR_TRAB D_SIT_RES_TRAB if D_Individual==0 [fweight = pesofinal] ,vce(robust)

reg LNRENDA loggravitational DummySexo lnIdade lnIdadeaoquad D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_comercio D_Industria D_SITSETOR_RES D_SITSETOR_TRAB D_SIT_RES_TRAB if D_Individual==0 [fweight = pesofinal] ,vce(robust)

ivregress 2sls LNRENDA DummySexo lnIdade lnIdadeaoquad D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_comercio D_Industria D_SITSETOR_RES D_SITSETOR_TRAB D_SIT_RES_TRAB (loggravitational = lnDistRios2) if D_Individual==0 [fweight = pesofinal]
estat firststage

#### TABELAS 

sum LNRENDA loggravitational lnDistCBD lnDistRios2 DummySexo lnIdade lnIdadeaoquad D_EducSeminst D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_servico D_comercio D_Industria D_SITSETOR_RES D_SITSETOR_TRAB D_SIT_RES_TRAB if D_Individual==0 [fweight = pesofinal] 

keep if D_Individual==0
outreg2 using results, word replace sum(log) keep(LNRENDA loggravitational lnDistCBD lnDistRios2 DummySexo lnIdade lnIdadeaoquad D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_comercio D_Industria D_SITSETOR_RES D_SITSETOR_TRAB D_SIT_RES_TRAB)

reg LNRENDA lnDistCBD DummySexo lnIdade lnIdadeaoquad D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_comercio D_Industria D_SITSETOR_RES D_SITSETOR_TRAB D_SIT_RES_TRAB if D_Individual==0 [fweight = pesofinal] ,vce(robust)
outreg2 using myreg.doc, replace ctitle(OLS)

reg LNRENDA loggravitational DummySexo lnIdade lnIdadeaoquad D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_comercio D_Industria D_SITSETOR_RES D_SITSETOR_TRAB D_SIT_RES_TRAB if D_Individual==0 [fweight = pesofinal] ,vce(robust)
outreg2 using myreg.doc, append ctitle(OLS)

ivregress 2sls LNRENDA DummySexo lnIdade lnIdadeaoquad D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_comercio D_Industria D_SITSETOR_RES D_SITSETOR_TRAB D_SIT_RES_TRAB (loggravitational = lnDistRios2) if D_Individual==0 [fweight = pesofinal]
outreg2 using myreg.doc, append ctitle(IV INSTRUMENT) addtext(log of linear instrument using rivers)

estat firststage





#### COM DUMMY RESIDE E TRABALHA
ivregress 2sls LNRENDA DummySexo lnIdade lnIdadeaoquad D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_comercio D_Industria TRABRESID (loggravitational = lnDistRios2) if D_Individual==0 [fweight = pesofinal]

estat firststage
### COM EFEITOS FIXOS A NIVEL DE ZONA

set matsize 2000
ivregress 2sls LNRENDA DummySexo lnIdade lnIdadeaoquad D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_comercio D_Industria TRABRESID i.ZONA_TRABALHO(loggravitational = lnDistRios2) if D_Individual==0 [fweight = pesofinal], vce(jackknife)
estat firststage

ivregress 2sls LNRENDA DummySexo lnIdade lnIdadeaoquad D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_comercio D_Industria i.ZONA_TRABALHO (loggravitational = lnDistRios2) if D_Individual==0 [fweight = pesofinal], vce(jackknife)
estat firststage

ivregress 2sls LNRENDA DummySexo lnIdade lnIdadeaoquad D_Educ_1 D_Educ_2 D_Educ_3 D_Educ_4 D_Educ_5 D_Educ_6 D_Educ_7 D_comercio D_Industria i.cod_municipio (loggravitational = lnDistRios2) if D_Individual==0 [fweight = pesofinal], vce(jackknife)
estat firststage
