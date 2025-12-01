/***************ETAPE 1: CREATION DES TABLES POUR LE SUIVI DES MOBILITES*****************/

data IDE_1215;
set echange.ctrad_FRE1215IDF;
WHERE CATBEN notin ('99');
KEEP NORDALLC NUMCAF NUMORGCD CATBEN MATRICUL SEXE NIRMON NIRMME QUALNIRE QUALNIRN VALNIRMO VALNIRMM PRESFRES DTNAIRES DTNAICON DTINSCDO NATIFAM DDMOSIDO SITDOS MOTISITD NUMCOMDO ID15;
ID15 = _N_;
run; 

data IDE_1216;
set echange.ctrad_FR6_1216;
WHERE CATBEN notin ('99');
KEEP NORDALLC NUMCAF NUMORGCD CATBEN MATRICUL SEXE NIRMON NIRMME QUALNIRE QUALNIRN VALNIRMO VALNIRMM PRESFRES DTNAIRES DTNAICON DTINSCDO NATIFAM DDMOSIDO SITDOS MOTISITD NUMCOMDO ID16;
ID16 = _N_;
run; 

data IDE_1217;
set echange.ctrad_FR6_1217;
WHERE CATBEN notin ('99');
KEEP NORDALLC NUMCAF NUMORGCD CATBEN MATRICUL SEXE NIRMON NIRMME QUALNIRE QUALNIRN VALNIRMO VALNIRMM PRESFRES DTNAIRES DTNAICON DTINSCDO NATIFAM DDMOSIDO SITDOS MOTISITD NUMCOMDO ID17;
ID17 = _N_;
run;

data IDE_1218;
set echange.ctrad_FR6_1218;
WHERE CATBEN notin ('99');
KEEP NORDALLC NUMCAF NUMORGCD CATBEN MATRICUL SEXE NIRMON NIRMME QUALNIRE QUALNIRN VALNIRMO VALNIRMM PRESFRES DTNAIRES DTNAICON DTINSCDO NATIFAM DDMOSIDO SITDOS MOTISITD NUMCOMDO ID18;
ID18 = _N_;
run;

data IDE_1219;
set echange.ctrad_FR6_1219;
WHERE CATBEN notin ('99');
KEEP NORDALLC NUMCAF NUMORGCD CATBEN MATRICUL SEXE NIRMON NIRMME QUALNIRE QUALNIRN VALNIRMO VALNIRMM PRESFRES DTNAIRES DTNAICON DTINSCDO NATIFAM DDMOSIDO SITDOS MOTISITD NUMCOMDO ID19;
ID19 = _N_;
run;

data ADR_1215;
set echange.ctrad_FRE1215IDF;
WHERE CATBEN notin ('99');
KEEP ETATADR LILI2ADR LILI3ADR LILI4ADR LILI5ADR LILI6ADR LILI7ADR NBMCHADR NORDALLC ID15;
ID15 = _N_;
run; 

data ADR_1216;
set echange.ctrad_FR6_1216;
WHERE CATBEN notin ('99');
KEEP ETATADR LILI2ADR LILI3ADR LILI4ADR LILI5ADR LILI6ADR LILI7ADR NBMCHADR NORDALLC ID16;
ID16 = _N_;
run; 

data ADR_1217;
set echange.ctrad_FR6_1217;
WHERE CATBEN notin ('99');
KEEP ETATADR LILI2ADR LILI3ADR LILI4ADR LILI5ADR LILI6ADR LILI7ADR NBMCHADR NORDALLC ID17;
ID17 = _N_;
run;

data ADR_1218;
set echange.ctrad_FR6_1218;
WHERE CATBEN notin ('99');
KEEP ETATADR LILI2ADR LILI3ADR LILI4ADR LILI5ADR LILI6ADR LILI7ADR NBMCHADR NORDALLC ID18;
ID18 = _N_;
run;

data ADR_1219;
set echange.ctrad_FR6_1219;
WHERE CATBEN notin ('99');
KEEP ETATADR LILI2ADR LILI3ADR LILI4ADR LILI5ADR LILI6ADR LILI7ADR NBMCHADR NORDALLC ID19;
ID19 = _N_;
run;

data MENA_1215;
set echange.ctrad_FRE1215IDF;
WHERE CATBEN notin ('99');
KEEP NBNAIMOI TOPGRO ABANEURE NBENLEFA PERSCOUV PRESCONJ SITFAM NATIFAM NBMSIFAM NORDALLC ID15;
ID15 = _N_;
run;

data MENA_1216;
set echange.ctrad_FR6_1216;
WHERE CATBEN notin ('99');
KEEP NBNAIMOI TOPGRO ABANEURE NBENLEFA PERSCOUV PRESCONJ SITFAM NATIFAM NBMSIFAM NORDALLC ID16;
ID16 = _N_;
run; 

data MENA_1217;
set echange.ctrad_FR6_1217;
WHERE CATBEN notin ('99');
KEEP NBNAIMOI TOPGRO ABANEURE NBENLEFA PERSCOUV PRESCONJ SITFAM NATIFAM NBMSIFAM NORDALLC ID17;
ID17 = _N_;
run;

data MENA_1218;
set echange.ctrad_FR6_1218;
WHERE CATBEN notin ('99');
KEEP NBNAIMOI TOPGRO ABANEURE NBENLEFA PERSCOUV PRESCONJ SITFAM NATIFAM NBMSIFAM NORDALLC ID18;
ID18 = _N_;
run;

data MENA_1219;
set echange.ctrad_FR6_1219;
WHERE CATBEN notin ('99');
KEEP NBNAIMOI TOPGRO ABANEURE NBENLEFA PERSCOUV PRESCONJ SITFAM NATIFAM NBMSIFAM NORDALLC ID19;
ID19 = _N_;
run;

data REV_15;
set echange.ctrad_FRE1215IDF;
WHERE CATBEN notin ('99');
KEEP MTREAPAT MTPRERUC RUCDERRE TPPOPRUC NBUC ID15;
ID15 = _N_;
run; 

data REV_16;
set echange.ctrad_FR6_1216;
WHERE CATBEN notin ('99');
KEEP MTREAPAT MTPRERUC RUCDERRE TPPOPRUC NBUC NORDALLC ID16;
ID16 = _N_;
run; 

data REV_17;
set echange.ctrad_FR6_1217;
WHERE CATBEN notin ('99');
KEEP MTREAPAT MTPRERUC RUCDERRE TPPOPRUC NBUC NORDALLC ID17;
ID17 = _N_;
run; 

data REV_18;
set echange.ctrad_FR6_1218;
WHERE CATBEN notin ('99');
KEEP MTREAPAT MTPRERUC RUCDERRE TPPOPRUC NBUC NORDALLC ID18;
ID18 = _N_;
run; 

data REV_19;
set echange.ctrad_FR6_1219;
WHERE CATBEN notin ('99');
KEEP MTREAPAT MTPRERUC RUCDERRE TPPOPRUC NBUC NORDALLC ID19;
ID19 = _N_;
run; 

data LOG_15;
set echange.ctrad_FRE1215IDF;
WHERE CATBEN notin ('99');
KEEP ALFVERS ALSVERS APLVERS MTLOGCAL ETATIMPA NBCOH PPRPPU OCCLOG NORDALLC ID15;
ID15 = _N_;
run;

data EXTRA_LOG_15;
set echange.ctrad_FIC1215IDF;
WHERE CATBEN notin ('99');
KEEP PARCAL PARCAPL NORDALLC NUMCAF NUMCOMDO;
ID15 = _N_;
run;

data LOG_16;
set echange.ctrad_FR6_1216;
WHERE CATBEN notin ('99');
KEEP ALFVERS ALSVERS APLVERS PARCAL PARCAPL MTLOGCAL ETATIMPA SURFTOT NBCOH PPRPPU OCCLOG NORDALLC ID16;
ID16 = _N_;
run;

data LOG_17;
set echange.ctrad_FR6_1217;
WHERE CATBEN notin ('99');
KEEP ALFVERS ALSVERS APLVERS PARCAL PARCAPL MTLOGCAL ETATIMPA SURFTOT NBCOH PPRPPU OCCLOG NORDALLC ID17;
ID17 = _N_;
run;

data LOG_18;
set echange.ctrad_FR6_1218;
WHERE CATBEN notin ('99');
KEEP ALFVERS ALSVERS APLVERS PARCAL PARCAPL MTLOGCAL ETATIMPA SURFTOT NBCOH PPRPPU OCCLOG NORDALLC ID18;
ID18 = _N_;
run;

data LOG_19;
set echange.ctrad_FR6_1219;
WHERE CATBEN notin ('99');
KEEP ALFVERS ALSVERS APLVERS PARCAL PARCAPL MTLOGCAL ETATIMPA SURFTOT NBCOH PPRPPU OCCLOG NORDALLC ID19;
ID19 = _N_;
run;

data ENF_15;
set echange.ctrad_FRE1215IDF;
WHERE CATBEN notin ('99');
KEEP ANNNEN1 ANNNEN2 ANNNEN3 ANNNEN4 ANNNEN5 ANNNEN6 ANNNEN7 ANNNEN8 ANNNEN9 ANNNEN10 ANNNEN11 ANNNEN12 ID15;
ID15 = _N_;
run;

data ENF_16;
set echange.ctrad_FR6_1216;
WHERE CATBEN notin ('99');
KEEP ANNNEN1 ANNNEN2 ANNNEN3 ANNNEN4 ANNNEN5 ANNNEN6 ANNNEN7 ANNNEN8 ANNNEN9 ANNNEN10 ANNNEN11 ANNNEN12 ID16;
ID16 = _N_;
run;

data ENF_17;
set echange.ctrad_FR6_1217;
WHERE CATBEN notin ('99');
KEEP ANNNEN1 ANNNEN2 ANNNEN3 ANNNEN4 ANNNEN5 ANNNEN6 ANNNEN7 ANNNEN8 ANNNEN9 ANNNEN10 ANNNEN11 ANNNEN12 ID17;
ID17 = _N_;
run;

data ENF_18;
set echange.ctrad_FR6_1218;
WHERE CATBEN notin ('99');
KEEP ANNNEN1 ANNNEN2 ANNNEN3 ANNNEN4 ANNNEN5 ANNNEN6 ANNNEN7 ANNNEN8 ANNNEN9 ANNNEN10 ANNNEN11 ANNNEN12 ID18;
ID18 = _N_;
run;

data ENF_19;
set echange.ctrad_FR6_1219;
WHERE CATBEN notin ('99');
KEEP ANNNEN1 ANNNEN2 ANNNEN3 ANNNEN4 ANNNEN5 ANNNEN6 ANNNEN7 ANNNEN8 ANNNEN9 ANNNEN10 ANNNEN11 ANNNEN12 ID19;
ID19 = _N_;
run;

data ACT_15;
set echange.ctrad_FRE1215IDF;
WHERE CATBEN notin ('99');
KEEP STATUETU ACTRESPD ACTCONJ ID15;
ID15 = _N_;
run;

data ACT_16;
set echange.ctrad_FR6_1216;
WHERE CATBEN notin ('99');
KEEP STATUETU ACTRESPD ACTCONJ ID16;
ID16 = _N_;
run;

data ACT_17;
set echange.ctrad_FR6_1217;
WHERE CATBEN notin ('99');
KEEP STATUETU ACTRESPD ACTCONJ ID17;
ID17 = _N_;
run;

data ACT_18;
set echange.ctrad_FR6_1218;
WHERE CATBEN notin ('99');
KEEP STATUETU ACTRESPD ACTCONJ ID18;
ID18 = _N_;
run;

data ACT_19;
set echange.ctrad_FR6_1219;
WHERE CATBEN notin ('99');
KEEP STATUETU ACTRESPD ACTCONJ ID19;
ID19 = _N_;
run;

data GEO_15;
set echange.ctrad_GEO15_IDF;
WHERE CATBEN notin ('99');
KEEP NORDALLC NUMCAF NUMCOMDO GEOXL2E GEOYL2E QUALXY NUMCOMDO;
run;

data GEO_16;
set echange.ctrad_GEO16_IDF;
WHERE CATBEN notin ('99');
KEEP NORDALLC NUMCAF NUMCOMDO GEOX93 GEOY93 QUALXY NUMCOMDO;
run;

data GEO_17;
set echange.ctrad_GEO17_IDF;
WHERE CATBEN notin ('99');
KEEP NORDALLC NUMCAF PRESFRES GEOX93 GEOY93 QUALXY NUMCOMDO;
run;

data GEO_18;
set echange.ctrad_GEO18_IDF;
WHERE CATBEN notin ('99');
KEEP NORDALLC NUMCAF PRESFRES GEOX93 GEOY93 QUALXY NUMCOMDO;
run;

data GEO_19;
set echange.ctrad_GEO19_IDF;
WHERE CATBEN notin ('99');
KEEP NORDALLC NUMCAF PRESFRES GEOX93 GEOY93 QUALXY NUMCOMDO;
run;


/***************** ETAPE 2: CREATION NIR UNIQUE ET JOINTURE IDE ******************/
/** AppuyŽ sur le programme "BASEMOBILITE5, CTRAD - Bruno Fayard - mai 2014"  **/
/*RQ: FR6 Noyau dur uniquement pour l'annŽe 1, et pas pour annŽe 2 : en pŽriode 2 on garde tout le monde pour retrouver ceux qui ont quittŽ l'IDF */

/* Variables */ 

%let per=1218;
%let aa=%sysfunc(substr(&per.,3,2));
%let mm=%sysfunc(substr(&per.,1,2));
%let per2=1219;
%let aa2=%sysfunc(substr(&per2.,3,2));

/*** A. NIRALLOC ***/
data WORK.IDE_&per.;
set WORK.IDE_&per.;
if sexe ="1" then NIRALLOC_&aa. = NIRMON;
if sexe ="2" then NIRALLOC_&aa.= NIRMME;
if sexe ="1" then VALNIRALLOC_&aa. = VALNIRMO;
if sexe ="2" then VALNIRALLOC_&aa. = VALNIRMM;
WHERE PRESFRES IN (2,4,5,6,9);
run; 

data WORK.IDE_&per2.;
set WORK.IDE_&per2.;
if sexe ="1" then NIRALLOC_&aa2.= NIRMON;
if sexe ="2" then NIRALLOC_&aa2. = NIRMME;
if sexe ="1" then VALNIRALLOC_&aa2. = VALNIRMO;
if sexe ="2" then VALNIRALLOC_&aa2. = VALNIRMM;
run; 

/*** B. Nettoyage des donnŽes pŽriode 1 ***/

/* Identification des allocataires ayant un nir certifiŽ en pŽriode 1 */

DATA WORK.IDE_&per.(drop= valniralloc_&aa.) ;
SET WORK.IDE_&per.;
WHERE VALNIRALLOC_&aa. = "2" ; RUN;

/* Suppression des doublons en pŽriode 1 (une seule Žtape car seulement les NOYAU-DUR) */
proc sort data=WORK.IDE_&per. out= IDE_&per. nodupkey dupout=doublons_ide&aa.;
   by NIRALLOC_&aa.;
run;

/*** C. Nettoyage des donnŽes pŽriode 2  ***/

/* C.1 Identification des allocataires ayant un nir certifiŽ en pŽriode 2 */

DATA WORK.IDE_&per2.(drop= valniralloc_&aa2.) ;
SET WORK.IDE_&per2.;
WHERE VALNIRALLOC_&aa2. = "2" ; RUN;

/*C.2 Doublons en pŽriode 2 */ 

/* C.2.1: on supprime le doublon dont le numcomdo est différent du numcaf */

PROC SQL;    /*reperage des doublons par comptage des NIR*/
   CREATE TABLE WORK.doublon AS 
   SELECT (N(t1.MATRICUL)) AS cptdoublon,
          t1.NIRALLOC_&aa2.
      FROM WORK.IDE_&per2. t1
      GROUP BY t1.NIRALLOC_&aa2.;
QUIT;  


PROC SQL;     /*crŽe une table de NIR en doublons*/
   CREATE TABLE WORK.DOUBLON2 AS 
   SELECT t1.cptdoublon, 
          t1.NIRALLOC_&aa2.
      FROM WORK.DOUBLON t1
      WHERE t1.cptdoublon NOT = 1;
QUIT;


PROC SQL;   /*croise la table des NIR doublons avec la table complte*/
   CREATE TABLE WORK.doublon3 AS 
   SELECT t1.*, 
          t2.cptdoublon
	FROM WORK.IDE_&per2. t1 left JOIN WORK.DOUBLON2 t2 ON (t1.NIRALLOC_&aa2. = t2.NIRALLOC_&aa2.)
	order by cptdoublon desc, NIRALLOC_&aa2. asc;
QUIT;


data doublon4;   /*repere un des doublons sur le critre numcaf numcomdo diffŽrents en P2*/
set doublon3;
if cptdoublon > 1 and substr(Numcomdo,1,2) ne substr(NUMCAF,1,2)and 
substr(Numcomdo,1,2) in ("75","77","78","91","92","93","94","95") then d2 = 1 ;
run;
data doublon5;  /*élimine le doublon selon le critère choisi*/
set doublon4;
where d2 ne 1;
run; 


/* C.2.1: on supprime le doublon pour lequel l'allocataire est radiŽ*/

PROC SQL;    /*deuxime reperage des doublons par comptage des NIR*/
   CREATE TABLE doublon6 AS 
   SELECT (N(t1.MATRICUL)) AS cptdoublonbis, 
          t1.NIRALLOC_&aa2.
      FROM doublon5 t1
      GROUP BY t1.NIRALLOC_&aa2.;
QUIT;  


PROC SQL;     /*crŽe une table de NIR en doublons*/
   CREATE TABLE DOUBLON7 AS 
   SELECT t1.cptdoublonbis, 
          t1.NIRALLOC_&aa2.
      FROM DOUBLON6 t1
      WHERE t1.cptdoublonbis NOT = 1;
QUIT; 


PROC SQL;   /*croise la table des NIR doublons avec la table complte*/
   CREATE TABLE doublon8 AS 
   SELECT t1.*, 
          t2.cptdoublonbis
	FROM doublon5 t1 left JOIN DOUBLON7 t2 ON (t1.NIRALLOC_&aa2. = t2.NIRALLOC_&aa2.)
	order by cptdoublonbis desc, NIRALLOC_&aa2. asc;
QUIT;


DATA WORK.doublon9;/*repere un des doublons sur le critre compte radiŽ en P2*/
SET WORK.doublon8;
if cptdoublonbis > 1 and catben ='21' and 
substr(Numcomdo,1,2) in("75","77","78","91","92","93","94","95") then d2 = 1;
run;


data IDE_&per2.;  /*Žlimine le doublon selon le critre choisi*/
set doublon9;
where d2 ne 1;
run; 

/*** D. Jointure des tables IDE ***/
PROC SQL;
CREATE TABLE TAB_&aa.&aa2. AS
SELECT t1.NUMCAF as NUMCAF_&aa., t1.NUMORGCD as NUMORGCD_&aa., t1.NORDALLC as NORDALLC_&aa.,t1.MATRICUL as MATRICUL_&aa., t1.CATBEN as CATBEN_&aa., t1.NUMCOMDO as NUMCOMDO_&aa., t1.PRESFRES as PRESFRES_&aa.,
t1.SEXE, t1.NATIFAM as NATIFAM_&aa., t1.DTNAIRES as DTNAIRES_&aa., t1.DTNAICON as DTNAICON_&aa., t1.DTINSCDO as DTINSCDO_&aa.,t1.SITDOS as SITDOS_&aa., t1.MOTISITD as MOTISITD_&aa., t1.ID&aa.,
t1.NIRALLOC_&aa.,t2.NUMCAF as NUMCAF_&aa2., t2.NUMORGCD as NUMORGCD_&aa2., t2.NORDALLC as NORDALLC_&aa2.,t2.MATRICUL as MATRICUL_&aa2.,t2.CATBEN as CATBEN_&aa2., t2.NUMCOMDO as NUMCOMDO_&aa2., t2.PRESFRES as PRESFRES_&aa2.,
t2.NATIFAM as NATIFAM_&aa2., t2.DTNAIRES as DTNAIRES_&aa2., t2.DTNAICON as DTNAICON_&aa2., t2.DTINSCDO as DTINSCDO_&aa2., t2.SITDOS as SITDOS_&aa2., t2.MOTISITD as MOTISITD_&aa2., t2.ID&aa2.,
t2.NIRALLOC_&aa2.
 FROM WORK.IDE_&per. t1 full JOIN WORK.IDE_&per2. t2 ON (t1.NIRALLOC_&aa. = t2.NIRALLOC_&aa2.);
QUIT;


/* crŽation VARIABLES compteur ET NIRALLOC unique ET DŽpartement prenant ET age respd ET NATIONALITE FAMILLE */
data TAB_&aa.&aa2.(drop= NIRALLOC_&aa. NIRALLOC_&aa2. dtnaires_&aa. dtnaires_&aa2.) ;
SET TAB_&aa.&aa2.;
NIRALLOC = NIRALLOC_&aa.;
IF NIRALLOC_&aa. = "" then NIRALLOC = NIRALLOC_&aa2. ;
attrib DTNAIRESP format= NLDATE18. INFORMAT=DDMMYY10. ;
DTNAIRESP = DTNAIRES_&aa.;IF DTNAIRES_&aa. = . then DTNAIRESP = DTNAIRES_&aa2. ;
attrib NATIOF LABEL="Nationalité famille" ;
NATIOF = NATIFAM_&aa.;IF NATIFAM_&aa. = " " then NATIOF = NATIFAM_&aa2. ;
attrib CPT LABEL="Compteur";
CPT= 1;
DEPTPR = substr(Numcomdo_&aa2.,1,2);
if substr(Numcomdo_&aa2.,1,2) in ("98","99","00") then DEPTPR = "ETR";
if substr(Numcomdo_&aa2.,1,3) in ("971","972","973","974","976") 
		then DEPTPR = substr(Numcomdo_&aa2.,1,3);
run;


/*** E. DOUBLONS SUR LA TABLE ISSUE DE LA JOINTURE ***/ 

/* E.1: on supprime le doublon pour lequel l'allocataire absent en 2018 et HND en 2019*/

/* Comptage des NIR */ 
PROC SQL;    
   CREATE TABLE doublon11 AS 
   SELECT (N(t1.cpt)) AS cptdoublonter, 
          t1.NIRALLOC
      FROM TAB_&aa.&aa2. t1
      GROUP BY t1.NIRALLOC;
QUIT;   


/*Crée une table de NIR en doublons*/
PROC SQL;     
   CREATE TABLE DOUBLON12 AS 
   SELECT t1.cptdoublonter,
          t1.NIRALLOC
      FROM DOUBLON11 t1
      WHERE t1.cptdoublonter NOT = 1;
QUIT; 


/*Croise la table des NIR doublons avec la table complète*/
PROC SQL;   
   CREATE TABLE doublon13 AS 
   SELECT t1.*, 
          t2.cptdoublonter
	FROM TAB_&aa.&aa2. t1 left JOIN DOUBLON12 t2 ON (t1.NIRALLOC = t2.NIRALLOC)
	order by cptdoublonter desc, NIRALLOC asc;
QUIT;


/*Repere un des doublons sur le critre absent en p1 et hors noyau dur en p2*/
DATA doublon14;
SET doublon13;
if cptdoublonter > 1 and matricul_&aa. =. and 
PRESFRES_&aa2. notIN (2,4,5,6,9) then d2 = 1;
run;

/*Elimine le doublon selon le critère choisi*/
data doublon15;  
set doublon14;
where d2 ne 1;
run; 


/* E.2: on supprime le doublon pour lequel l'allocataire absent en 2018 et RM en 2019*/

/*Comptage des NIR*/
PROC SQL;    
   CREATE TABLE doublon16 AS 
   SELECT (N(t1.cpt)) AS cptdoublonquater, 
          t1.NIRALLOC
      FROM doublon15 t1
      GROUP BY t1.NIRALLOC;
QUIT;   


/*CrŽe une table de NIR en doublons*/
PROC SQL;     
   CREATE TABLE DOUBLON17 AS 
   SELECT t1.cptdoublonquater,
          t1.NIRALLOC
      FROM DOUBLON16 t1
      WHERE t1.cptdoublonquater NOT = 1;
QUIT; 
-
/*Croise la table des NIR doublons avec la table complte*/
PROC SQL;   
   CREATE TABLE doublon18 AS 
   SELECT t1.*, 
          t2.cptdoublonquater
	FROM doublon15 t1 left JOIN DOUBLON17 t2 ON (t1.NIRALLOC = t2.NIRALLOC)
	order by cptdoublonquater desc, NIRALLOC asc;
QUIT;


/*Repere un des doublons sur le critre absent en 2017 et radiŽ/mutation en 2018*/
DATA doublon19;
SET doublon18;
if cptdoublonquater > 1 and motisitd_&aa2. = 'RM' then d2 = 1;
run;


/*Žlimine le doublon selon le critère choisi*/
data TABD_&aa.&aa2.;  
set doublon19;
where d2 ne 1;
run; 


/*** F.Verification qu'il ne reste plus de doublons ***/ 

proc sort data=TABD_&aa.&aa2. out=TAB_&aa.&aa2. nodupkey dupout=doublo3;
   by NIRALLOC ;
 run;


data TAB_FINAL_&aa.&aa2. (drop= cpt cptdoublonter d2 cptdoublonquater);
set TAB_&aa.&aa2.;
/*where NORDALLC_&aa. ne .;*/ /* pour ne garder que les prŽsents en P1 et P2 */ 
run;

/* Sauvegarde dans ECHANGE */ 
data ECHANGE.LUGUI_IDE_&aa.&aa2.;
set TAB_FINAL_&aa.&aa2.;
run;

