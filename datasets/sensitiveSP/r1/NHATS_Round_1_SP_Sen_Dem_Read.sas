/* NHATS_Round_1_SP_Sensitive_Demographic_Read.sas  */

/* Section #1 - Creating the Formats */
PROC FORMAT ;

    VALUE W000005W
    -7 = '-7 RF'
    -8 = '-8 DK'
    -1 = '-1 Inapplicable'
    -9 = '-9 Missing'
     1 = " 1 YES"
     2 = " 2 NO"    ;
    VALUE W000073W
     -7 = '-7 RF'
     -8 = '-8 DK'
     -1 = '-1 Inapplicable'
     -9 = '-9 Missing'
      1 = " 1 JANUARY"
      2 = " 2 FEBRUARY"
      3 = " 3 MARCH"
      4 = " 4 APRIL"
      5 = " 5 MAY"
      6 = " 6 JUNE"
      7 = " 7 JULY"
      8 = " 8 AUGUST"
      9 = " 9 SEPTEMBER"
     10 = "10 OCTOBER"
     11 = "11 NOVEMBER"
     12 = "12 DECEMBER"    ;
    VALUE W000101W
     -7 = '-7 RF'
     -8 = '-8 DK'
     -1 = '-1 Inapplicable'
     -9 = '-9 Missing'
      1 = " 1 {WHITE}"
      2 = " 2 {BLACK}"
      3 = " 3 {AMERICAN INDIAN}"
      4 = " 4 {ALASKAN NATIVE}"
      5 = " 5 {ASIAN}"
      6 = " 6 {NATIVE HAWAIIAN}"
      7 = " 7 {PACIFIC ISLANDER}"
      8 = " 8 {OTHER}"    ;
    VALUE RFDK_F
    -7 = '-7 RF'
    -8 = '-8 DK'
    -1 = '-1 Inapplicable'
    -9 = '-9 Missing';

RUN;

/* SP DATA */
/* Section #2 - Input statement with variable name and location on the .txt file  */

Data NHATS_R1_OP_Sensitive_Demo_Data;
    INFILE  "[PATH]\NHATS_Round_1_SP_Sensitive_Demographic_File.txt" 
	LRECL=368 ;
INPUT @1 spid 8.
@9 r1dbirthmth 8.
@17 r1dbirthyr 8.
@25 r1dintvwrage 8.
@33 hc1cancerty2 8.
@41 hc1cancerty3 8.
@49 hc1cancerty4 8.
@57 hc1cancerty5 8.
@65 hc1cancerty6 8.
@73 hc1cancerty7 8.
@81 hc1cancerty8 8.
@89 hh1mthendmar 8.
@97 hh1modob 8.
@105 hh1yrdob 8.
@113 hh1dspousage 8.
@121 el1yrcametus 8.
@129 el1agecomlus 8.
@137 rl1yourrace1 8.
@145 rl1yourrace2 8.
@153 rl1yourrace3 8.
@161 rl1yourrace4 8.
@169 rl1yourrace5 8.
@177 rl1yourrace6 8.
@185 rl1yourrace7 8.
@193 rl1yourrace8 8.
@201 rl1primarace 8.
@209 rl1hisplatno 8.
@217 rl1mexpurcu1 8.
@225 rl1mexpurcu2 8.
@233 rl1mexpurcu3 8.
@241 rl1mexpurcu4 8.
@249 rl1mexpurcu5 8.
@257 rl1mexpurcu6 8.
@265 rl1mexpurcu7 8.
@273 rl1mexpurcu8 8.
@281 va1whnserva1 8.
@289 va1whnserva2 8.
@297 va1whnserva3 8.
@305 va1whnserva4 8.
@313 va1whnserva5 8.
@321 va1whnserva6 8.
@329 va1whnserva7 8.
@337 va1whnserva8 8.
@345 va1whnserva9 8.
@353 ia1msrtrecss 8.
@361 ia1yrstressp 8. ;

/* Section #3 - format assignment statement   */
format  r1dbirthmth hh1mthendmar ia1msrtrecss hh1modob w000073W.
			hc1cancerty4 hc1cancerty5 hc1cancerty6 hc1cancerty7 hc1cancerty8 w000005w.
			hh1yrdob hh1dspousage r1dbirthyr r1dintvwrage el1yrcametus el1agecomlus  RFDK_F.;

	/* Section #4 - Label assignment statement   */
Label	spid= "NHATS SAMPLED PERSON ID"
r1dbirthmth= "R1 D BIRTH MONTH OF SP"
r1dbirthyr= "R1 D BIRTH YEAR OF SP"
r1dintvwrage= "R1 D AGE OF SP AT INTERVIEW"
hc1cancerty2= "R1 HC3 SP HAD BREAST CANCER"
hc1cancerty3= "R1 HC3 SP HAD PROSTATE CANCER"
hc1cancerty4= "R1 HC3 SP HAD BLADDER CANCER"
hc1cancerty5= "R1 HC3 SP HAD CRV OVRN UTRN CNCR "
hc1cancerty6= "R1 HC3 SP HAD COLON CANCER"
hc1cancerty7= "R1 HC3 SP HAD KIDNEY CANCER"
hc1cancerty8= "R1 HC3 SP HAD OTHER CANCER"
hh1mthendmar= "R1 HH2A MTH MARR END OR SPS PASS"
rl1yourrace1= "R1 RL1 RACE OF SP WHITE"
rl1yourrace2= "R1 RL1 RACE OF SP AFRICN AMERICN"
rl1yourrace3= "R1 RL1 RACE OF SP AMERICN INDIAN"
rl1yourrace4= "R1 RL1 RACE OF SP ALASKA NATIVE"
rl1yourrace5= "R1 RL1 RACE OF SP ASIAN"
rl1yourrace6= "R1 RL1 RACE OF SP NATIVE HWAIIAN"
rl1yourrace7= "R1 RL1 RACE OF SP PACIFIC ISLNDR"
rl1yourrace8= "R1 RL1 RACE OF SP OTHER SPECIFY"
rl1primarace= "R1 RL2 RACE CONSID YRSLF PRIMRLY"
rl1hisplatno= "R1 RL3 CNSDR YRSF HSPAN OR LATNO"
rl1mexpurcu1= "R1 RL4 HISP ETHNCTY MEXICAN AMER"
rl1mexpurcu2= "R1 RL4 HISP ETHNCTY PUERO RICAN"
rl1mexpurcu3= "R1 RL4 HISP ETHNCTY CUBAN AMER"
rl1mexpurcu4= "R1 RL4 HISP ETHNCTY OTHR SPECIFY"
rl1mexpurcu5= "R1 RL4 HISP ETHNCTY DOMINICAN"
rl1mexpurcu6= "R1 RL4 HISP ETHNCTY CENTRAL AMER"
rl1mexpurcu7= "R1 RL4 HISP ETHNCTY SOUTH AMER"
rl1mexpurcu8= "R1 RL4 HISP ETHNCTY SPANISH"
va1whnserva1= "R1 VA2 WHEN SERVD BEFRE NOV 1941"
va1whnserva2= "R1 VA2 WHN SERVD WWII"
va1whnserva3= "R1 VA2 WHN SERVD 1947 TO 1950"
va1whnserva4= "R1 VA2 WHN SERVD KOREAN WAR"
va1whnserva5= "R1 VA2 WHN SERVD 1955 TO 1964 "
va1whnserva6= "R1 VA2 WHN SERVD VIETNAM ERA"
va1whnserva7= "R1 VA2 WHN SERVD 1975 TO 1990"
va1whnserva8= "R1 VA2 WHN SERVD 1990 TO 2001"
va1whnserva9= "R1 VA2 WHN SERVD 2001 OR LATER"
ia1msrtrecss= "R1 IA3A MTH STRD REC SOC SEC PAY"
ia1yrstressp= "R1 IA3B YR STRTD REC SOC SEC PAY"
hh1modob= "R1 HH6A MONTH DOB OF SPOUSE"
hh1yrdob= "R1 HH6C YEAR DOB OF SPOUSE"
hh1dspousage= "R1 D SPOUSE AGE";
run;
