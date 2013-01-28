C Copyright(c) 1998, Space Science and Engineering Center, UW-Madison
C Refer to "McIDAS Software Acquisition and Distribution Policies"
C in the file  mcidas/data/license.txt

C *** $Id: kbxvisr.dlm,v 1.22 1998/10/13 20:25:45 dglo Exp $ ***

      INTEGER FUNCTION KBXINI(CIN,COUT,IOPT)
      IMPLICIT NONE
      CHARACTER*4 CIN
      CHARACTER*4 COUT
      INTEGER IOPT(*)

      ! symbolic constants & shared data

      INCLUDE 'areaparm.inc'

      INTEGER JTYPE
      INTEGER ISOU
      INTEGER IDES
      INTEGER JOPT(NUMAREAOPTIONS)
      COMMON/VISRXX/JTYPE,ISOU,IDES,JOPT

      ! external functions

      ! local variables

      CALL MOVW(NUMAREAOPTIONS,IOPT,JOPT)
      JTYPE=0
      ISOU=IOPT(1)
      IDES=IOPT(2)
      IF(CIN.EQ.'BRIT'.AND.COUT.EQ.'TEMP') JTYPE=1
      IF(CIN.EQ.'BRIT'.AND.COUT.EQ.'MODB') JTYPE=2
      IF(CIN.EQ.'BRIT'.AND.COUT.EQ.'RAW ') JTYPE=3
      IF(JTYPE.EQ.0) GO TO 900

      KBXINI=0
      RETURN
900   CONTINUE
      KBXINI=-1
      RETURN
      END

      INTEGER FUNCTION KBXCAL(CALB,IDIR,NVAL,IBAND,IBUF)
      IMPLICIT NONE
      INTEGER CALB(*)
      INTEGER IDIR(*)
      INTEGER NVAL
      INTEGER IBAND
      INTEGER IBUF(*)

      ! symbolic constants & shared data

      INCLUDE 'areaparm.inc'

      INTEGER JTYPE
      INTEGER ISOU
      INTEGER IDES
      INTEGER JOPT(NUMAREAOPTIONS)
      COMMON/VISRXX/JTYPE,ISOU,IDES,JOPT

      CHARACTER*4 CALTYP
      COMMON/BRKPNT/ CALTYP

      ! external functions

      CHARACTER*4 CLIT
      INTEGER BRKVAL
      LOGICAL ISIR
      integer mciydtocyd

      ! local variables

      INTEGER I
      INTEGER IDAY
      integer idir4
      integer iret
      INTEGER ITAB(256)

      ! initialized variables

      INTEGER LASDAY
      INTEGER LASTYP

      DATA LASDAY / -1 /
      DATA LASTYP / -1 /

c  make sure we work with idir4 as ccyyddd
      if(idir(4).gt.1900000)  then
         idir4=idir(4)
      else
         iret=mciydtocyd(idir(4),idir4)
      endif

      IF (.NOT.ISIR(IDIR(3),CLIT(IDIR(57)),IBAND)) THEN
C     IF(IAND(IDIR(3),1).EQ.0.AND.IDIR(3).NE.6) THEN
         IF((CALTYP.EQ.'BRIT'.OR.CALTYP.EQ.'RAW ')
     &       .AND.JTYPE.EQ.2) THEN
            IF(LASDAY.NE.idir4.OR.LASTYP.NE.JTYPE) THEN
               DO 100 I=0,255
100            ITAB(I+1)=BRKVAL(REAL(I))
               LASDAY = idir4
               LASTYP = JTYPE
            ENDIF
            CALL MPIXTB(NVAL,ISOU,IDES,IBUF,ITAB)
            RETURN
         ELSE
            CALL MPIXEL(NVAL,ISOU,IDES,IBUF)
            RETURN
         ENDIF
      ENDIF
      IDAY=IDIR4
      IF(LASDAY.NE.IDIR4.OR.LASTYP.NE.JTYPE) THEN
         IF(JTYPE.EQ.1) THEN
                CALL TEMPTB(IDAY,ITAB)
         ELSE IF(JTYPE.EQ.3) THEN
                DO 105 I=0,255
105             ITAB(I+1)=I
         ELSE IF(CALTYP.EQ.'BRIT'.OR.CALTYP.EQ.'RAW') THEN
                DO 110 I=0,255
110             ITAB(I+1)=BRKVAL(REAL(I))
         ELSE IF(CALTYP.EQ.'TEMP') THEN
                CALL TEMPTB(IDAY,ITAB)
                DO 200 I=0,255
                   ITAB(I+1) = BRKVAL(REAL(ITAB(I+1))/10.)
200             CONTINUE
         ENDIF
         LASDAY = IDIR4
         LASTYP = JTYPE
      ENDIF
      CALL MPIXTB(NVAL,ISOU,IDES,IBUF,ITAB)
      KBXCAL=0
      RETURN
      END

C--- ERROR CODE
C       -1 = INVALID OPTION CFUNC
C       -2 = INVALID BAND ('INFO')
C       -3 = ERROR IN BREAKPOINT TABLE (OR MISSING) ('KEYS', 'BRKP')

      INTEGER FUNCTION KBXOPT(CFUNC,IIN,IOUT)
      IMPLICIT NONE
      CHARACTER*4 CFUNC
      INTEGER IIN(*)
      INTEGER IOUT(*)

      ! symbolic constants & shared data

      INCLUDE 'areaparm.inc'

      INTEGER JTYPE
      INTEGER ISOU
      INTEGER IDES
      INTEGER JOPT(NUMAREAOPTIONS)
      COMMON/VISRXX/JTYPE,ISOU,IDES,JOPT

      CHARACTER*4 CALTYP
      COMMON/BRKPNT/ CALTYP

      ! external functions

      CHARACTER*4 CLIT
      INTEGER BRKSET
      INTEGER ISCHAR
      INTEGER LIT

      ! local variables

      CHARACTER*8 CFILE
      LOGICAL ISIR

C--- CHECK FOR VALID CALIBRATION TYPES
      IF(CFUNC.EQ.'KEYS') THEN
         IF (.NOT.ISIR(IIN(1),CLIT(IIN(37)),IIN(4))) THEN
C        IF(IAND(IIN(1),1).EQ.0.AND.IIN(1).NE.6) THEN
            IOUT(1)=2
            IOUT(2)=LIT('RAW ')
            IOUT(3)=LIT('BRIT')
         ELSE
            IOUT(1)=3
            IOUT(2)=LIT('RAW ')
            IOUT(3)=LIT('TEMP')
            IOUT(4)=LIT('BRIT')
         ENDIF
         IF(ISCHAR(IIN(38)).EQ.1) THEN
            CALL MOVCW(IIN(38),CFILE)
            IF(BRKSET(CFILE,CALTYP).NE.0) THEN
                KBXOPT = -3
                RETURN
            ENDIF
         ENDIF
         KBXOPT=0

C--- BREAKPOINT TABLE OPTION
      ELSE IF(CFUNC.EQ.'BRKP') THEN
         CALL MOVCW(IIN(1),CFILE)
         IF(BRKSET(CFILE,CALTYP).NE.0) THEN
                KBXOPT = -3
                RETURN
         ENDIF
         KBXOPT = 0

C--- BAND INFO OPTION
      ELSE IF(CFUNC.EQ.'INFO') THEN
         IF (.NOT.ISIR(IIN(2),CLIT(IIN(3)),IIN(1))) THEN
C        IF(IAND(IIN(1),1).EQ.0.AND.IIN(1).NE.6) THEN
            IOUT(1)=2
            IOUT(2)=LIT('RAW ')
            IOUT(3)=LIT('BRIT')
            IOUT(4)=LIT('    ')
            IOUT(5)=LIT('    ')
            IOUT(6)=1
            IOUT(7)=1
         ELSE
            IOUT(1)=3
            IOUT(2)=LIT('RAW ')
            IOUT(3)=LIT('TEMP')
            IOUT(4)=LIT('BRIT')
            IOUT(5)=LIT('    ')
            IOUT(6)=LIT('K   ')
            IOUT(7)=LIT('    ')
            IOUT(8)=1
            IOUT(9)=10
            IOUT(10)=1
         ENDIF
         KBXOPT = 0

C--- INVALID OPTION
      ELSE
            KBXOPT = -1

      ENDIF
      RETURN
      END

C TEMPTB   MOSHER 1074 WINLIB  SMS IR BRIGHTNESS TO DEGREES KELVIN*10
C $ SUBROUTINE TEMPTB(IDAY, IRAD, ITEMP)  (FM)
C $ CONVERTS FROM INFRARED COUNT TO TEMPERATURE*10
C $ IDAY = (I) INPUT  DAY (YYDDD)
C $ IRAD = (I) INPUT  IR BRIGHTNESS
C $ ITEM = (I) OUTPUT  TEMPERATURE*10
C $$ TEMP = CONVERT
c  iday is now ccyyddd, so the IRDAY array has to be changed
c  to ccyyddd
C

      SUBROUTINE TEMPTB(IDAY,ITEM)
      IMPLICIT NONE
      INTEGER IDAY
      INTEGER ITEM(*)

      ! external functions
      character*12 cfi

      ! local variables

      INTEGER I
      INTEGER IR
      INTEGER IRAD
      INTEGER J
      INTEGER JTEMP
      REAL A
      REAL D
      REAL DT
      REAL T

      ! initialized variables

      INTEGER IRTOT
      INTEGER IRDAY(2)
      INTEGER ICNT(9)
      INTEGER ITGAT(9)
      INTEGER IRTEM1(256)

      DATA IRTOT    / 2 /
      DATA IRDAY    / 1974200,1974304 /
      DATA ICNT     /  41,  60,  90, 120, 150, 180, 210, 230, 235/
      DATA ITGAT    /3297,3216,3079,2926,2751,2539,2223,1944,1819/
      DATA IRTEM1/3300,3295,3290,3285,3280,3275,3270,3265,3260,3255,
     >3250,3245,3240,3235,3230,3225,3220,3215,3210,3205,3200,3195,3190,
     >3185,3180,3175,3170,3165,3160,3155,3150,3145,3140,3135,3130,3125,
     >3120,3115,3110,3105,3100,3095,3090,3085,3080,3075,3070,3065,3060,
     >3055,3050,3045,3040,3035,3030,3025,3020,3015,3010,3005,3000,2995,
     >2990,2985,2980,2975,2970,2965,2960,2955,2950,2945,2940,2935,2930,
     >2925,2920,2915,2910,2905,2900,2895,2890,2885,2880,2875,2870,2865,
     >2860,2855,2850,2845,2840,2835,2830,2825,2820,2815,2810,2805,2800,
     >2795,2790,2785,2780,2775,2770,2765,2760,2755,2750,2745,2740,2735,
     >2730,2725,2720,2715,2710,2705,2700,2695,2690,2685,2680,2675,2670,
     >2665,2660,2655,2650,2645,2640,2635,2630,2625,2620,2615,2610,2605,
     >2600,2595,2590,2585,2580,2575,2570,2565,2560,2555,2550,2545,2540,
     >2535,2530,2525,2520,2515,2510,2505,2500,2495,2490,2485,2480,2475,
     >2470,2465,2460,2455,2450,2445,2440,2435,2430,2425,2420,2410,2400,
     >2390,2380,2370,2360,2350,2340,2330,2320,2310,2300,2290,2280,2270,
     >2260,2250,2240,2230,2220,2210,2200,2190,2180,2170,2160,2150,2140,
     >2130,2120,2110,2100,2090,2080,2070,2060,2050,2040,2030,2020,2010,
     >2000,1990,1980,1970,1960,1950,1940,1930,1920,1910,1900,1890,1880,
     >1870,1860,1850,1840,1830,1820,1810,1800,1790,1780,1770,1760,1750,
     >1740,1730,1720,1710,1700,1690,1680,1670,1660,1650,1640,1630/
C
C  FIND WHICH CALIBRATION PERIOD TO USE
C
      call mctrace(1,'AGETSERV',' in temptb')
      call mctrace(1,'AGETSERV',' date is'//cfi(iday))
      DO 300 IRAD=0,255
      DO 1 I=1,IRTOT
      J=I
      IF(IDAY.LT.IRDAY(I)) GO TO 2
1     CONTINUE
      J=3
2     GO TO (10,20,30),J
C
C CALIBRATION FOR 1ST GATE PERIOD 27 JUNE-16 JULY
C
10    DO 11 I=2,9
       J=I-1
11     IF(IRAD.LE.ICNT(I)) GO TO 12
      J=8
12    JTEMP=((ITGAT(J+1)-ITGAT(J))*(IRAD-ICNT(J))/(ICNT(J+1)-ICNT(J)))
     > +ITGAT (J)
      ITEM(IRAD+1)=JTEMP
      GO TO 300
C
C CALIBRATION FOR PERIOD 16 JULY - 30 OCT. 74
20    IR=IRAD+1
      T=FLOAT(IRTEM1(IR))/10.
       D=FLOAT(IDAY-1974000)
C
C COMPUTE CORRECTION FACTOR
C
       A=-31.97+D*(.3385+D*(-1.195E-3+1.443E-6*D))
      DT=A*(3.8+T*(-.031-2.0E-5*T))
      ITEM(IR)=IFIX((T+DT)*10.)
      GO TO 300
C
C DATA AFTER 30 OCT. 74
C
30    IR=IRAD+1
      ITEM(IR)=IRTEM1(IR)

300   CONTINUE
      RETURN
      END
