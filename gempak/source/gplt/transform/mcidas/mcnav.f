C********************************************************************
C*
C*  Auxiliary MCIDAS NAVIGATION routines
C*
C********************************************************************
C*
      FUNCTION IROUND(X)
C *** McIDAS Revision History ***
C 1 IROUND.FOR 16-Mar-90,16:38:50,`SSEC' PC-McIDAS ver 5.00
C 2 IROUND.FOR 25-Sep-90,7:34:30,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C $ FUNCTION IROUND(X)  (JMB)
C $ ROUNDS A FLOATING POINT VALUE
C $ X = (R) INPUT  FLOATING POINT VALUE
C $$ IROUND = REAL
C
      IROUND=NINT(X)
      RETURN
      END
      SUBROUTINE SOLARP(JDAY,JTIME,GHA,DEC,XLAT,XLON)
C *** McIDAS Revision History ***
C 1 SOLARP.FOR 23-Mar-90,12:34:48,`SSEC' PC-McIDAS ver 5.00
C 2 SOLARP.FOR 25-Sep-90,7:36:38,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C SOLARP MOSHER 1074 WINLIB  Z HOUR ANGLE AND SOLAR DECL FOR DAY-TIME
C $ SUBROUTINE SOLARP(JDAY, JTIME, GHA, DEC, XLAT, XLON)  (DAS)
C $ COMPUTES GREENWICH HOUR ANGLE AND DECLINATION OF SUN
C $ JDAY = (I) INPUT  SATELLITE/YEAR/DAY
C $ JTIME = (I) INPUT  HOUR/MINUTE/SECOND
C $ GHA = (R) OUTPUT  GREENWICH HOUR ANGLE
C $ DEC = (R) OUTPUT  DECLINATION
C $ XLAT = (R) OUTPUT  LATITUDE OF SUN POSITION
C $ XLON = (R) OUTPUT  LONGITUDE OF SUN POSITION
C $$ SOLARP = COMPUTATION, NAVIGATION
C
C     ORBITAL CONSTANTS
C
C     IEPYD = EPOCH YEAR-DAY
C     IEPHMS = EPOCH HOUR-MINUTE-SECOND
C     OECCEN = ECCENTRICITY OF EARTH ORBIT
C     OINCLI = INCLINATION TO CELESTIAL EQUATOR
C     PERHEL = PERIHELION
C     ASNODE = ASCENDING NODE
C     XMANOM = MEAN ANOMOLY
C     XMMC = MEAN MOTION CONSTANT
C     SHA = CELESTIAL HOUR ANGLE
C     IRAYD  =  YYDDD WHEN CELESTIAL COOR. SYS. COINCIDES WITH EARTH COO
C     IRAHMS = HHMMSS WHEN CELESTIAL COOR. SYS. COINCIDES WITH EARTH COO
C
      REAL*8 DIFTIM,ECANM1,ECANOM,RAHA,TIMDIF,XHA,XMANOM
C     REAL*8 DABS,DMOD,DSQRT,DSIN,DCOS,DATAN2
      DATA INIT/0/
C
C
      IF(INIT.NE.0)GO TO 1
      INIT=1
      PI=3.14159265
      RDPDG=PI/180.0
      SOLSID=1.00273791
      IEPYD=74004
      IEPHMS=0
      OECCEN=0.016722
      OINCLI=RDPDG*FLALO(232700)
      PERHEL=RDPDG*FLALO(1011311)+PI
      ASNODE=RDPDG*FLALO(0)
      XMMC=.01720209895/1440.0
      SHA=100.26467
      IRAYD=74001
      IRAHMS=0
      SINC=SIN(OINCLI)
      CINC=COS(OINCLI)
      SPER=SIN(PERHEL)
      CPER=COS(PERHEL)
      SAND=SIN(ASNODE)
      CAND=COS(ASNODE)
      PX=CPER*CAND-SPER*SAND*CINC
      PY=CPER*SAND+SPER*CAND*CINC
      PZ=SPER*SINC
      QX=(-SPER)*CAND-CPER*SAND*CINC
      QY=(-SPER)*SAND+CPER*CAND*CINC
      QZ=CPER*SINC
 1    IDAY=MOD(JDAY,100000)
      PTIME=FTIME(JTIME)
      DIFTIM=TIMDIF(IEPYD,IEPHMS,IDAY,JTIME)
      XMANOM=XMMC*DIFTIM
      ECANM1=XMANOM
      EPSILN=1.0E-8
      DO 2 I=1,20
      ECANOM=XMANOM+OECCEN*SIN(ECANM1)
      IF(ABS(ECANOM-ECANM1).LT.EPSILN)GO TO 3
 2    ECANM1=ECANOM
 3    XOMEGA=DCOS(ECANOM)-OECCEN
      YOMEGA=SQRT(1.0-OECCEN**2)*DSIN(ECANOM)
      XFACT=1.0/SQRT(XOMEGA**2+YOMEGA**2)
      XOMEGA=XOMEGA*XFACT
      YOMEGA=YOMEGA*XFACT
      XS=XOMEGA*PX+YOMEGA*QX
      YS=XOMEGA*PY+YOMEGA*QY
      ZS=XOMEGA*PZ+YOMEGA*QZ
      SLRA=ATAN2(YS,XS)/RDPDG
      RAHA=TIMDIF(IRAYD,IRAHMS,IDAY,JTIME)*SOLSID/4.0
      GHA=PTIME*15.0
      XHA=360.0-SHA-RAHA+SLRA+GHA
      GHA=DMOD(XHA,360.0D0)
      GHA=360.0-GHA-2.0
      DEC=ATAN2(ZS,SQRT(XS**2+YS**2))/RDPDG
      XLAT=GEOLAT(DEC*RDPDG,1)/RDPDG
      XLON=-GHA-PTIME*15.0+720.0
      XLON=AMOD(XLON,360.0)
      RETURN
      END
      FUNCTION TIMDIF(IYRDA1,IHMS1,IYRDA2,IHMS2)
C *** McIDAS Revision History ***
C 1 TIMDIF.FOR 23-Mar-90,12:38:46,`SSEC' PC-McIDAS ver 5.00
C 2 TIMDIF.FOR 25-Sep-90,7:28:46,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C $ TIMDIF(IYRDA1, IHMS1, IYRDA2, IHMS2)  (JI)
C $ TIME DIFFERENCE IN MINUTES
C $ IYRDA1 = (I) INPUT  FIRST YEAR AND JULIAN DAY (YYDDD)
C $ IHMS1 = (I) INPUT  FIRST TIME (HHMMSS)
C $ IYRDA2 = (I) INPUT  SECOND YEAR AND DAY (YYDDD)
C $ IHMS2 = (I) INPUT  SECOND TIME (HHMMSS)
C $$ TIMDIF = TIME
C
C FUNC VAL (REAL*8) IS TIME DIFFERENCE IN MINUTES (POSITIVE IF
C FIRST DAY/TIME IS THE EARLIER OF THE TWO).
C
C --- Fixed for Y2K - D. Kidwell/NCEP   5/99
C --- Fixed for Y2K21 - B. Hebbard/NCEP 1/21
C
      DOUBLE PRECISION TIMDIF,D1,D2,T1,T2
      IY1=MOD(IYRDA1/1000,100)
      IF ( iy1 .le. 40 ) iy1 = iy1 + 100
      ID1=MOD(IYRDA1,1000)
      IFAC1=(IY1-1)/4+1
      D1=365*(IY1-1)+IFAC1+ID1-1
      IY2=MOD(IYRDA2/1000,100)
      IF ( iy2 .le. 40 ) iy2 = iy2 + 100
      ID2=MOD(IYRDA2,1000)
      IFAC2=(IY2-1)/4+1
      D2=365*(IY2-1)+IFAC2+ID2-1
      T1=1440.D0*D1+60.D0*FLALO(IHMS1)
      T2=1440.D0*D2+60.D0*FLALO(IHMS2)
      TIMDIF=T2-T1
      RETURN
      END
      FUNCTION GEOLAT(XLAT,IDIR)
C *** McIDAS Revision History ***
C 1 GEOLAT.FOR 23-Mar-90,12:40:32,`SSEC' PC-McIDAS ver 5.00
C 2 GEOLAT.FOR 25-Sep-90,7:28:48,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C $ FUNCTION GEOLAT(XLAT, IDIR)  (DAS)
C $ GEOCENTRIC/GEODETIC LATITUDE CONVERSION.  FN VAL IN RADIANS.
C $ XLAT = (R) INPUT  LATITUDE (RADIANS)
C $ IDIR = (I) 1 FOR GEODEDIC TO GEOCENTRIC CONVERSION, 2 FOR GEOCENTRIC
C $   TO GEODEDIC CONVERSION
C $$ GEOLAT = CONVERT, LATITUDE, NAVIGATION
C
C-----XLAT, FN VALUE EXPRESSED IN RADIANS AS PER HARRIS SYSTEM
C
      DATA A/6378.388/,B/6356.912/
      ASQ=A**2
      BSQ=B**2
      CX=COS(XLAT)
      SX=SIN(XLAT)
      IF(IDIR.EQ.2)GOTO 1
      GEOLAT=ATAN2(BSQ*SX,ASQ*CX)
      RETURN
    1 GEOLAT=ATAN2(ASQ*SX,BSQ*CX)
      RETURN
      END
      SUBROUTINE LLCART(XLAT,XLON,X,Y,Z)                                
C *** McIDAS Revision History ***
C 1 LLCART.FOR 19-Mar-90,21:50:54,`SSEC' PC-McIDAS ver 5.00
C 2 LLCART.FOR 25-Sep-90,7:33:58,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C $ SUBROUTINE LLCART(XLAT, XLON, X,Y,Z)  (DAS)                         
C $ CONVERT LAT, LON TO CARTESIAN CENTERED COORDS  (X, Y, Z).           
C $ XLAT = (R) INPUT  LATITUDE IN DEGREES, NORTH +                      
C $ XLON = (R) INPUT  LONGITUDE IN DEGREES, WEST +                      
C $ X, Y, Z = (R) OUTPUT  COORDS IN SYSTEM WITH ORIGIN AT CENTER OF     
C $  PLANET. POS X-AXIS PIERCES EQUATOR AT LON 0 DEG, POS Y-AXIS PIERCES
C $  EQUATOR AT LON 90 DEG, & POS Z-AXIS INTERSECTS THE NORTH POLE.     
C $  (IN KM).                                                           
C $$ LLCART = COORDINATES,LATITUDE,LONGITUDE,NAVIGATION                 
      IMPLICIT REAL*8 (D)                                               
      REAL*8 ASQ,BSQ,AB,RDPDG,ECC,ECCSQR
      DATA ASQ/40683833.48/,BSQ/40410330.18/,AB/40546851.22/            
      DATA ECC/.081992D0/,ECCSQR/6.72265D-3/                            
      DATA RDPDG/1.74532925199D-02/
      DATA KWEST/-1/,KCORD/0/                                           
      GO TO 55                                                          
      ENTRY LLOPT(DRAD,DECC,IWEST,ICORD)                                
C $ SUBROUTINE LLOPT(DRAD,DECC,IWEST,ICORD)                             
C $ LLOPT IS USED TO INPUT RADIUS & ECCENTRICITY OTHER THAN EARTH       
C $ AND DETERMINE + LONGITUDE CONVETION & PLANETOCENTRIC/DETIC          
C $ DRAD = (R*8) INPUT  EQUATORIAL RADIUS                               
C $ DECC = (R*8) INPUT  ECCENTRICITY                                    
C $ IWEST = (I) INPUT  >= 0    WEST POSITIVE, < 0 WEST NEGATIVE         
C $ ICORD = (I) INPUT  >= 0 PLANETODETIC, < 0 PLANETOCENTRIC            
C $$ LLOPT = COORDINATES,LATITUDE,LONGITUDE,NAVIGATION                  
      ASQ=DRAD*DRAD                                                     
      ECC=DECC                                                          
      ECCSQR=ECC*ECC                                                    
      DPOLE=SQRT(ASQ*(1.D0-ECCSQR))                                     
      BSQ=DPOLE*DPOLE                                                   
      AB=DRAD*DPOLE                                                     
      IF(IWEST.LT.0) KWEST=1                                            
      IF(ICORD.LT.0) KCORD=-1                                           
      RETURN                                                            
      ENTRY LLOBL(TLAT,RAD)                                             
C $ SUBROUTINE LLOBL(TLAT,RAD) (DAS)                                    
C $ CALCULATE RADIUS AT LATITUDE TLAT                                   
C $ TLAT = (R) INPUT  LATITUDE                                          
C $ RAD = (R) OUTPUT  RADIUS IN KM                                      
C $$ LLOBL = NAVIGATION,COORDINATES                                     
      TCOS=COS(TLAT*RDPDG)                                              
      DDRAD=((1.D0-ECCSQR)*ASQ)/(1.D0-ECCSQR*TCOS*TCOS)                 
      RAD=SQRT(DDRAD)                                                   
      RETURN                                                            
55    CONTINUE                                                          
      YLAT=RDPDG*XLAT                                                   
C-----CONVERT TO GEOCENTRIC (SPHERICAL) LATITUDE IF KCORD >= 0          
CCC     YLAT=GEOLAT(YLAT,1)                                             
      IF(KCORD.GE.0) YLAT=ATAN2(BSQ*SIN(YLAT),ASQ*COS(YLAT))            
      YLON=KWEST*RDPDG*XLON                                             
      SNLT=SIN(YLAT)                                                    
      CSLT=COS(YLAT)                                                    
      CSLN=COS(YLON)                                                    
      SNLN=SIN(YLON)                                                    
      TNLT=(SNLT/CSLT)**2                                               
      R=AB*SQRT((1.0+TNLT)/(BSQ+ASQ*TNLT))                              
      X=R*CSLT*CSLN                                                     
      Y=R*CSLT*SNLN                                                     
      Z=R*SNLT                                                          
      RETURN                                                            
      ENTRY CARTLL(X,Y,Z,XLAT,XLON)                                     
C $ SUBROUTINE CARTLL(X, Y, Z, XLAT, XLON)  (DALY 1978)                 
C $ CONVERT CARTESIAN CENTERED COORD (X, Y, Z) TO LAT, LON.             
C $ X, Y, Z = (R) OUTPUT  COORDS IN SYSTEM WITH ORIGIN AT CENTER OF     
C $  PLANET. POS X-AXIS PIERCES EQUATOR AT LON 0 DEG, POS Y-AXIS PIERCES
C $  EQUATOR AT LON 90 DEG, & POS Z-AXIS INTERSECTS THE NORTH POLE.     
C $  IN KM.                                                             
C $ XLAT = (R) INPUT  LATITUDE IN DEGREES, NORTH +                      
C $ XLON = (R) INPUT  LONGITUDE IN DEGREES, WEST +                      
C $$ CARTLL = COORDINATES,LATITUDE,LONGITUDE,NAVIGATION                 
C                                                                       
C                                                                       
      XLAT=100.0                                                        
      XLON=0.0                                                        
      IF(X.EQ.0..AND.Y.EQ.0..AND.Z.EQ.0.) GO TO 90                      
      A=ATAN(Z/SQRT(X*X+Y*Y))                                           
C-----CONVERT TO GEODETIC LATITUDE IF KCORD > 0                         
CCC     XLAT=GEOLAT(ATAN(Z/SQRT(X*X+Y*Y)),2)/RDPDG                      
      IF(KCORD.GE.0) THEN                                               
         XLAT=ATAN2(ASQ*SIN(A),BSQ*COS(A))/RDPDG                        
      ELSE                                                              
         XLAT=A/RDPDG                                                   
      ENDIF                                                             
      XLON=KWEST*ATAN2(Y,X)/RDPDG                                       
   90 RETURN                                                            
      END                                                               
       FUNCTION LIT(C)
C *** McIDAS Revision History ***
C 1 LIT.FOR 27-Feb-90,9:04:00,`SSEC' PC-McIDAS ver 5.00
C 2 LIT.FOR 24-Sep-90,18:19:26,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C $    FUNCTION LIT(CC)   (JMB)
C $    LIT  --  CHANGE CHARACTER*4 INTO INTEGER*4
C $    INPUT:
C $        CC  (C)  CHARACTER STRING
C $    FUNCTION VALUE:
C $        THE SAME BYTES, CHANGED TO TYPE INTEGER
       CHARACTER*(*) C
       CHARACTER*4 C1
       C1=C
C       CALL MOVCW(C1,L1)
       read ( c1, '(A4)') L1
       LIT=L1
       RETURN
       END
       CHARACTER*4 FUNCTION CLIT(L)
C *** McIDAS Revision History ***
C 1 CLIT.FOR 27-Feb-90,8:52:06,`SSEC' PC-McIDAS ver 5.00
C 2 CLIT.FOR 24-Sep-90,18:19:28,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
       CHARACTER*4 C
C       CALL MOVWC(L,C)
       write ( c, '(A4)') l
       CLIT=C
       RETURN
       END
      FUNCTION ITIMC(X)
C *** McIDAS Revision History ***
C 1 ITIME.FOR 23-Mar-90,12:32:54,`SSEC' PC-McIDAS ver 5.00
C 2 ITIME.FOR 25-Sep-90,7:28:54,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C $ FUNCTION ITIMC(X)  (JMB)
C $ FLOATING POINT TIME TO PACKED INTEGER ( SIGN HH MM SS )
C $ X = (R) INPUT  FLOATING POINT TIME
C $$ ITIMC = TIME, CONVERT, INTEGER, REAL
C
      IF(X.LT.0.0)GO TO 1
      Y=X
      I=1
      GO TO 2
 1    Y=-X
      I=-1
 2    J=3600.0*Y+0.5
      ITIMC=10000*(J/3600)+100*MOD(J/60,60)+MOD(J,60)
      ITIMC=I*ITIMC
      RETURN
      END
      FUNCTION FLALO(M)
C *** McIDAS Revision History ***
C 1 FLALO.FOR 19-Mar-90,21:49:24,`SSEC' PC-McIDAS ver 5.00
C 2 FLALO.FOR 25-Sep-90,7:33:56,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C $ FUNCTION FLALO(M)  (BL)
C $ CONVERT PACKED INTEGER (SIGN DDD MM SS) LATITUDE-LONGITUDE TO REAL*4
C $ M = (I) INPUT  PACKED INTEGER (SIGN DDD MM SS) LATITUDE-LONGITUDE
C $$ FLALO = CONVERT, INTEGER, LATITUDE, LONGITUDE, REAL
C
      IF(M.LT.0)GO TO 1
      N=M
      X=1.0
      GO TO 2
 1    N=-M
      X=-1.0
 2    FLALO=FLOAT(N/10000)+FLOAT(MOD(N/100,100))/60.0+FLOAT(MOD(N,100))/
     13600.0
      FLALO=X*FLALO
      RETURN
      END
       CHARACTER*12 FUNCTION CFZ(L)
C *** McIDAS Revision History ***
C 1 CFZ.FOR 27-Feb-90,21:47:28,`SSEC' PC-McIDAS ver 5.00
C 2 CFZ.FOR 25-Sep-90,7:35:58,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
       IMPLICIT CHARACTER*12 (C)
       WRITE(C,1)L
 1     FORMAT(4X,Z8)
       CFZ=C
       RETURN
       END
      FUNCTION IFTOK(CTOK)
C *** McIDAS Revision History ***
C 1 IFTOK.FOR 27-Feb-90,9:34:44,`SSEC' PC-McIDAS ver 5.00
C 2 IFTOK.FOR 24-Sep-90,18:19:20,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C
C* J. Cowie/COMET	 1/95	Changed to deal only with integer or float
C*				numeric respresentaions
C*
C*******************************************************************************
C $ FUNCTION IFTOK(CTOK)  (TMW)
C $ CONVERT NUMERIC CHARACTER TOKEN TO INTERNAL FORM. FN VAL IS INTEGER
C $   (ROUNDED) PART OF NUMERIC  VALUE.
C $ CTOK = (C) INPUT  CHARACTER TOKEN TO CONVERT
C $$ IFTOK = CONVERT, UTILITY, SCANNER
C
C-----CONTAINS ENTRY POINTS IFTOK,DFTOK
C-----CONVERT NUMERIC TOKEN TO BINARY
C-----TOKEN TO CONVERT IS IN CTOK
C-----FUNCTION VALUE:
C        IFTOK: INTEGER (ROUNDED) PART OF NUMERIC VALUE
C        DFTOK: REAL*8
C-----FUNCTION COMPUTED BY CALCULATING IVAL & ISCAL, WHERE TRUE
C        VALUE = IVAL * (10**ISCAL). ISCAL IS ARBITRARILY RESTRICTED TO
C        BETWEEN -30 AND 30.
C-----IF SYNTAX ERROR OR OUT-OF-RANGE CONDITION DETECTED, 0 IS
C        RETURNED
C
C-----NUMBER IN CTOK CAN APPEAR AS: HEX (E.G. $7FFF) DECIMAL INTEGER
C        (E.G. -123) FLOATING POINT (E.G. -123. , -123.E-6 , -123.D15)
C        BABYLONIAN (E.G. 12:30 , -179:59:59)
C        DATE (E.G. 83/1/31 , 2883/1/31 (28 IS SS) , 28/// )
C
C
C
      PARAMETER (IMAXL=214748364,IMAXR=7)
      IMPLICIT CHARACTER*12 (C)
      IMPLICIT REAL*8 (D)
      CHARACTER CTOK*(*)
      CHARACTER*33 CSTR
      CHARACTER*2 C2
      CHARACTER*1 C
      INTEGER *2 ITYPE,IZERO,ICHA,J,ISIGN,IDIG,K
      INTEGER *2 JJ
C      INTEGER MAXVAL(3)
C      DATA MAXVAL/9999,59,59/
C
C
C
C----(FUNCTION IFTOK(CTOK))
         ITYPE=1
         GOTO 1
      ENTRY DFTOK(CTOK)
C $ ENTRY DFTOK(CTOK)  (TMW)
C $ CONVERT NUMERIC CHARACTER TOKEN TO INTERNAL FORM. FN VAL IS REAL*8.
C $ CTOK = (C) INPUT  CHARACTER TOKEN TO CONVERT
C $$ DFTOK = CONVERT, UTILITY, SCANNER
         ITYPE=2
         GOTO 1
C
C
C
 1    IZERO=ICHAR('0')
       ICHA=ICHAR('A')
      LENC=MIN0(32,LEN(CTOK))
C-----LOCATE FIRST NON-BLANK
      DO 2 J=1,LENC
         C=CTOK(J:J)
         IF (C.NE.' ') GOTO 3
 2    CONTINUE
      GOTO 89
 3    CSTR=CTOK(J:LENC)
      IVAL=0
      ISCAL=0
      ISIGN=1
      C=CSTR(1:1)
      K=1
      IF (C.EQ.'+') THEN
         K=2
      ELSE IF (C.EQ.'-') THEN
         K=2
         ISIGN=-1
      ENDIF
      C2=CSTR(1:2)
C
C
      IF (C.EQ.'$') THEN
C
C-----CONVERT HEX NUMBER
*     IF (C2.EQ.'$ '.OR.CSTR(10:10).NE.' ') GOTO 89
*       DO 5 J=2,9
*         C=CSTR(J:J)
*         IF (C.EQ.' ') GOTO 90
*         ICH=ICHAR(C)
*         IDIG=ICH-IZERO
*         IF (IDIG.GE. 0 .AND.IDIG.LE. 9 ) GOTO 6
*         IDIG=ICH-ICHA+ 10
*         IF (IDIG.GE. 10 .AND.IDIG.LE. 15 ) GOTO 6
*         GOTO 89
* 6       CONTINUE
*         IF (J.LT. 9 ) THEN
*            IVAL=IVAL*16+IDIG
*         ELSE
*            K=IC(IVAL,0)
*            CALL STC(0,IVAL,0)
*            IVAL=IVAL*16+IDIG
*            CALL STC(K*16+IC(IVAL,0),IVAL,0)
*         ENDIF
* 5    CONTINUE
C
C
      ELSE IF (INDEX(CSTR(1:16),':').GT.0) THEN
C
C
C-----CONVERT BABYLONIAN TOKEN (E.G. TIME-- 12:45:00) TO INTERNAL
C        BINARY (SCALED INTEGER) FORM. IVAL IS HHDDDDD, WHERE HH IS
C        HOURS AND DDDDD IS FRACTIONS OF AN HOUR
C
C
*       IVAL=0
*       ISCAL=-5
*       IF (C2.EQ.': ') THEN
*          CALL GETTIM(L)
*          LSECS=MOD(L,10000)/100*60+MOD(L,100)
*          IVAL=L/10000*100000+(LSECS*1000+18)/36
*          GOTO 90
*       ENDIF
*      IPART(1)=0
*      IPART(2)=0
*      IPART(3)=0
C-----NP COUNTS # OF ':' SEEN (PLUS 1)
*      NP=1
C
C-----GET NEXT CHARACTER
C
*      DO 22 J=K,16
*        C=CSTR(J:J)
*        IF (C.EQ.' ') THEN
*           L=((IPART(2)*60+IPART(3))*1000+18)/36
*           IVAL=IPART(1)*100000+L
*           IVAL=IVAL*ISIGN
*           GOTO 90
*        ELSE IF (C.EQ.':') THEN
*           NP=NP+ 1
*           IF (NP.GT. 3 ) GOTO 89
*        ELSE IF (C.GE.'0'.AND.C.LE.'9') THEN
*           IPART(NP)=IPART(NP)*10+ICHAR(C)-IZERO
*           IF (IPART(NP).GT.MAXVAL(NP)) GOTO 89
*        ELSE
*           GOTO 89
*        ENDIF
*22   CONTINUE
C
C
      ELSE IF (INDEX(CSTR(1:16),'/').GT.0) THEN
C
C
C-----CONVERT DATE IN (GENERAL) FORM SS/YY/MM/DD OR SSYY/MM/DD
*     CALL GETDAY(IYYDDD)
*     DO 31 J=1,4
* 31   IPART(J)=0
*     NP=1
*     DO 35 J=K,16
*        C=CSTR(J:J)
*        IF (C.EQ.' ') THEN
C-----      REVERSE IPART ARRAY SO THAT DAY IS IN IPART(1), MON
C           IN IPART(2), ETC.
*           I=IPART(1)
*           IPART(1)=IPART(NP)
*           IPART(NP)=I
*           IF (NP.GT. 3 ) THEN
*              I=IPART(2)
*              IPART(2)=IPART(3)
*              IPART(3)=I
*           ENDIF
*           IF (IPART(3).EQ.0) IPART(3)=IYYDDD/1000
*           IF (IPART(1).EQ.0.AND.IPART(2).EQ.0) THEN
*              IVAL=MOD(IYYDDD,1000)+IPART(3)*1000
*           ELSE IF (IPART(1).EQ.0.OR.IPART(2).EQ.0) THEN
*              GOTO 89
*           ELSE
*              IF (IPART(1).GT.31) GOTO 89
*              IF (IPART(2).GT.12) GOTO 89
*              IVAL=IDMYYD(IPART(1),IPART(2),IPART(3))
*           ENDIF
*           IF (IPART(4).GT.9999) GOTO 89
*           IVAL=IVAL+IPART(4)*100000
*           IVAL=ISIGN*IVAL
*           GOTO 90
*        ELSE IF (C.EQ.'/') THEN
*           NP=NP+ 1
*           IF (NP.GT. 4 ) GOTO 89
*        ELSE IF (C.GE.'0'.AND.C.LE.'9') THEN
*           IPART(NP)=IPART(NP)*10+ICHAR(C)-IZERO
*        ELSE
*           GOTO 89
*        ENDIF
*35   CONTINUE
C
C
      ELSE
C
C
C-----CONVERT NUMERIC (INTEGER OR FLOATING POINT) VALUE
C-----SAMPLE FORMS: 0, +12, 12 , 12., -12.3, +12.3@09, .004@-11
C
C
      IF (C2.EQ.'. '.OR.C2.EQ.'+ '.OR.C2.EQ.'- ') GOTO 89
C-----NDEC COUNTS # OF PLACES AFTER DECIMAL POINT
      NDEC=-1
C-----IEXPSN IS SIGN OF EXPONENT
      IEXPSN=1
C
C-----NEXT-CHARACTER LOOP
C
      DO 42 J=K,16
         C=CSTR(J:J)
         II=ICHAR(C)
         IF (C.EQ.' ') GOTO 45
         IF (C.EQ.'.') THEN
            IF (NDEC.GE.0) GOTO 89
            NDEC=0
         ELSE IF (C.GE.'0'.AND.C.LE.'9') THEN
            IDIG=ICHAR(C)-IZERO
           IF (IVAL.GT.IMAXL .OR. (IVAL.EQ.IMAXL.AND.IDIG.GT.IMAXR))
     +							GOTO 89
            IVAL=IVAL*10+IDIG
            IF (NDEC.GE.0) NDEC=NDEC+ 1
         ELSE IF (C.EQ.'E'.OR.C.EQ.'D') THEN
            JJ=J+ 1
            C=CSTR(JJ:JJ)
            JJ=JJ+ 1
            IF (C.EQ.'+') THEN
               C=CSTR(JJ:JJ)
               JJ=JJ+ 1
            ELSE IF (C.EQ.'-') THEN
               IEXPSN=-1
               C=CSTR(JJ:JJ)
               JJ=JJ+ 1
            ENDIF
            IF (C.LT.'0'.OR.C.GT.'9') GOTO 89
            ISCAL=ICHAR(C)-IZERO
            C=CSTR(JJ:JJ)
            IF (C.EQ.' ') GOTO 45
            IF (C.LT.'0'.OR.C.GT.'9') GOTO 89
            ISCAL=ISCAL*10+ICHAR(C)-IZERO
            GOTO 45
         ELSE
            GOTO 89
         ENDIF
 42   CONTINUE
 45   CONTINUE
      IVAL=IVAL*ISIGN
      ISCAL=IEXPSN*ISCAL-MAX0(0,NDEC)
      IF (ISCAL.LT.-30.OR.ISCAL.GT.30) GOTO 89
      GOTO 90
C
C
      ENDIF
C
C-----DONE
C
C-----ERROR CONDITION COMES HERE
 89   CONTINUE
      IVAL=0
      ISCAL=0
C-----ALL ORDINARY RETURNS COME HERE
 90   CONTINUE
C-----CONVERT TO CORRECT OUTPUT TYPE
      IF (ITYPE.EQ. 1 ) THEN
C-----   INTEGER TYPE. IF IABS(INTEGER) .GE. 1D10, 0 IS RETURNED
         IF (IVAL.EQ.0.OR.ISCAL.EQ.0) THEN
            IFTOK=IVAL
            RETURN
         ELSE
            D=DFLOAT(IABS(IVAL))*(10.D0**ISCAL)
            IF (D.LE.1.D9) THEN
               IFTOK=IDINT(D+.5D0)
               IF (IVAL.LT.0) IFTOK=-IFTOK
            ELSE
               IFTOK=0
            ENDIF
         ENDIF
      ELSE
         DFTOK=DFLOAT(IVAL)*(10.D0**ISCAL)
      ENDIF
      RETURN
      END
      FUNCTION FTIME(M)
C *** McIDAS Revision History ***
C 1 FTIME.FOR 23-Mar-90,12:38:58,`SSEC' PC-McIDAS ver 5.00
C 2 FTIME.FOR 25-Sep-90,7:28:44,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C $ FUNCTION FTIME(M)  (BL)
C $ CONVERT PACKED INTEGER (SIGN HH MM SS) TIME TO REAL*4
C $ M = (I) INPUT  PACKED INTEGER (SIGN HH MM SS) TIME
C $$ FTIME = CONVERT, INTEGER, TIME, REAL
C
      IF(M.LT.0)GO TO 1
      N=M
      X=1.0
      GO TO 2
 1    N=-M
      X=-1.0
 2    FTIME=FLOAT(N/10000)+FLOAT(MOD(N/100,100))/60.0+FLOAT(MOD(N,100))/
     13600.0
      FTIME=X*FTIME
      RETURN
      END
