C Copyright(c) 1998, Space Science and Engineering Center, UW-Madison
C Refer to "McIDAS Software Acquisition and Distribution Policies"
C in the file  mcidas/data/license.txt

C *** $Id: kbxaaa.dlm,v 1.24 1998/10/13 20:25:31 dglo Exp $ ***

      INTEGER FUNCTION KBXINI(CIN,COUT,IOPT)
      IMPLICIT NONE
      CHARACTER*4 CIN
      CHARACTER*4 COUT
      INTEGER IOPT(*)

      ! symbolic constants & shared data

      INCLUDE 'areaparm.inc'

      INTEGER ITYPE
      INTEGER JTYPE
      INTEGER JOPT(NUMAREAOPTIONS)
      COMMON/AAAXX/ITYPE,JTYPE,JOPT

      CALL MOVW(NUMAREAOPTIONS,IOPT,JOPT)
      ITYPE=0
      IF(CIN.EQ.'RAW') ITYPE=1
      IF(CIN.EQ.'RAD') ITYPE=2
      IF(CIN.EQ.'TEMP') ITYPE=3
      IF(CIN.EQ.'BRIT') ITYPE=4
      IF(ITYPE.EQ.0) GO TO 900
      JTYPE=0
      IF(COUT.EQ.'RAD') THEN
         JTYPE=2
         IF(JOPT(2).LT.4) GOTO 900
      ENDIF
      IF(COUT.EQ.'TEMP') THEN
         JTYPE=3
         IF(JOPT(2).LT.2) GOTO 900
      ENDIF
      IF(COUT.EQ.'BRIT') JTYPE=4
      IF(COUT.EQ.'MODB') JTYPE=4
      IF(JTYPE.EQ.0) GOTO 900
      KBXINI=0
      RETURN
900   CONTINUE
      KBXINI=-1
      RETURN
      END

      INTEGER FUNCTION KBXCAL(PFX,IDIR,NVAL,IBAND,IBUF)
      IMPLICIT NONE
      INTEGER*2 PFX(*)
      INTEGER IDIR(*)
      INTEGER NVAL
      INTEGER IBAND
      INTEGER IBUF(*)

      ! symbolic constants & shared data

      INTEGER    NCH
      PARAMETER (NCH=2)

      INCLUDE 'areaparm.inc'

      INTEGER ITYPE
      INTEGER JTYPE
      INTEGER JOPT(NUMAREAOPTIONS)
      COMMON/AAAXX/ITYPE,JTYPE,JOPT

      INTEGER IARR(128)
      COMMON/CALBXX/IARR

      ! local variables

      INTEGER NBANDS
      INTEGER NDOC
      INTEGER NCAL
      INTEGER NCOD
      INTEGER IA
      INTEGER CALPOS
      INTEGER CHANL
      INTEGER GTCHAN
      INTEGER IDES
      INTEGER ISOU
      INTEGER LSTUNT
      INTEGER NLEV
      INTEGER TABLE(4096,2)
      INTEGER VISTAB(4096)

      ! initialized variables

      INTEGER LSTCHL(NCH)
      INTEGER LSTARA
      INTEGER LSTVIS
      INTEGER INDCHL

      DATA LSTCHL / NCH*-1 /
      DATA LSTARA / -1 /
      DATA LSTVIS / -1 /
      DATA INDCHL / NCH /

      IA=IDIR(33)
      ISOU=JOPT(1)
      IDES=JOPT(2)
      IF(IA.NE.LSTARA) THEN
            CALL ARAGET(IA,IDIR(63),512,IARR)
C
C--- First word in calibration block is the SS number. Usually this
C--- is 30 (GOES-6) or 32 (GOES-7), but it may be 31 or 33 depending
C--- on the ingestor used. Always set to 30 or 32.
C
            IARR(1) = IARR(1)/2 * 2 

            NBANDS=IDIR(14)
            NCOD=0
            IF(IDIR(36).NE.0) NCOD=4
            NDOC=IDIR(49)
            NCAL=IDIR(50)
            NLEV=IDIR(51)
            CALPOS=NCOD+NDOC+12
            LSTARA=IA

      ENDIF

C  Handle visible (band 14) before we get to IR section

      IF(IBAND .EQ. 14) THEN
         IF(LSTVIS .EQ. -1) THEN
            CALL VISCON(VISTAB) 
            LSTVIS = IA
         ENDIF
         IF( JTYPE .EQ. 4) 
     *      CALL MPIXTB(NVAL, ISOU, IDES, IBUF, VISTAB)
         GO TO 100
      ENDIF

      CHANL=GTCHAN(IBAND,PFX,CALPOS)
      IF(CHANL.LE.0) THEN
         CALL ZEROS(IBUF,NVAL*IDES)
         KBXCAL=-1
         RETURN
      ENDIF
      GOTO (10,20,30,40) ITYPE
10    GOTO (100,11,12,13) JTYPE
11    IF(CHANL.NE.LSTCHL(INDCHL).OR.JTYPE.NE.LSTUNT) THEN
         INDCHL=MOD(INDCHL,NCH)+1
         IF(CHANL.NE.LSTCHL(INDCHL).OR.JTYPE.NE.LSTUNT)
     >   CALL IRCON(0,IBAND,CHANL,TABLE(1,INDCHL))
         LSTCHL(INDCHL)=CHANL
         LSTUNT=JTYPE
      ENDIF
      GOTO 50
12    IF(CHANL.NE.LSTCHL(INDCHL).OR.JTYPE.NE.LSTUNT) THEN
         INDCHL=MOD(INDCHL,NCH)+1
         IF(CHANL.NE.LSTCHL(INDCHL).OR.JTYPE.NE.LSTUNT)
     >   CALL IRCON(1,IBAND,CHANL,TABLE(1,INDCHL))
         LSTCHL(INDCHL)=CHANL
         LSTUNT=JTYPE
      ENDIF
      GOTO 50
13    IF(CHANL.NE.LSTCHL(INDCHL).OR.JTYPE.NE.LSTUNT) THEN
         INDCHL=MOD(INDCHL,NCH)+1
         IF(CHANL.NE.LSTCHL(INDCHL).OR.JTYPE.NE.LSTUNT)
     >   CALL IRCON(2,IBAND,CHANL,TABLE(1,INDCHL))
         LSTCHL(INDCHL)=CHANL
         LSTUNT=JTYPE
      ENDIF
      GOTO 50
20    GOTO (100,100,100,100) JTYPE
30    GOTO (100,100,100,100) JTYPE
40    GOTO (100,100,100,100) JTYPE
50    CONTINUE
      CALL MAAATB(NVAL,ISOU,IDES,IBUF,TABLE(1,INDCHL))
100   CONTINUE
      KBXCAL=0
      RETURN
      END

C
C--- ERROR CODES: -1 INVALID OPTION CFUNC
C                 -2 INVALID BAND ('INFO')
C                 -3 ERROR IN BREAKPOINT TABLE (OR MISSING) ('KEYS','BRKP')
      INTEGER FUNCTION KBXOPT(CFUNC,IIN,IOUT)
      IMPLICIT NONE
      CHARACTER*4 CFUNC
      INTEGER IIN(*)
      INTEGER IOUT(*)

      ! symbolic constants & shared data

      INCLUDE 'areaparm.inc'

      INTEGER ITYPE
      INTEGER JTYPE
      INTEGER JOPT(NUMAREAOPTIONS)
      COMMON/AAAXX/ITYPE,JTYPE,JOPT

      CHARACTER*4 CALTYP
      COMMON/BRKPNT/CALTYP

      ! external functions

      INTEGER BRKSET
      INTEGER ISCHAR
      INTEGER LIT

      ! local variables

      CHARACTER*8 CFILE

      IF(CFUNC.EQ.'KEYS') THEN
C--      IIN CONTAINS FRAME DIR

c--- Check for visible band

         if( iin(4) .eq. 14) then
           iout(1) = 2
           iout(2) = lit('RAW ')
           iout(3) = lit('BRIT')
         else
           IOUT(1)=4
           IOUT(2)=LIT('RAW ')
           IOUT(3)=LIT('RAD ')
           IOUT(4)=LIT('TEMP')
           IOUT(5)=LIT('BRIT')
        endif
C
C--- CHECK FRAME DIRECTORY FOR BREAKPOINT TABLE
C
         IF(ISCHAR(IIN(38)).EQ.1) THEN
            CALL MOVWC(IIN(38),CFILE)
            IF(BRKSET(CFILE,CALTYP).NE.0) THEN
               KBXOPT=-3
               RETURN
            ENDIF
         ENDIF
         KBXOPT=0

      ELSE IF(CFUNC.EQ.'BRKP') THEN
         CALL MOVWC(IIN(1),CFILE)
         IF(BRKSET(CFILE,CALTYP).NE.0) THEN
            KBXOPT=-3
            RETURN
         ENDIF
         KBXOPT=0

      ELSE IF(CFUNC.EQ.'INFO') THEN
C
C--- CHECK FOR VALID BAND
C
         IF(IIN(1).LT.1.OR.IIN(1).GT.14) THEN
            KBXOPT = -2
            RETURN
         ENDIF

         if( iin(1) .eq. 14) then
           iout(1) = 2
           iout(2) = lit('RAW ')
           iout(3) = lit('BRIT')
           iout(4) = lit('    ')
           iout(5) = lit('    ')
           iout(6) = 1
           iout(7) = 1
        else
           IOUT(1)=4
           IOUT(2)=LIT('RAW ')
           IOUT(3)=LIT('RAD ')
           IOUT(4)=LIT('TEMP')
           IOUT(5)=LIT('BRIT')
           IOUT(6)=LIT('    ')
           IOUT(7)=LIT('MW**')
           IOUT(8)=LIT('K   ')
           IOUT(9)=LIT('    ')
           IOUT(10)=1
           IOUT(11)=1000
           IOUT(12)=10
           IOUT(13)=1
        endif
        KBXOPT=0
      ELSE
         KBXOPT = -1
      ENDIF
      RETURN
      END

      INTEGER FUNCTION GTCHAN(IBAND,PFX,CALPOS)
      IMPLICIT NONE
      INTEGER IBAND
      INTEGER*2 PFX(*)
      INTEGER CALPOS

      ! local variables

      INTEGER CHANL
      INTEGER I
      INTEGER IBEG
      INTEGER IEND
      INTEGER IPOS
      INTEGER ISTEP
      INTEGER KBAND

      ! initialized variables

      INTEGER MAP(38)

      DATA    MAP     /1,2,3,4,5,6,7,8,9,10,11,12,
     >                 1,2,3,4,5,6,7,8,9,10,11,12,
     >                     3,4,5,  7,8,9,10,
     >                     3,4,5,  7,8,9,10/


      IPOS=CALPOS/2+1
C
C -- store IBAND in temp variable cause may have to change it
C
      KBAND = IBAND
      IBEG = 1
      IEND = 13
      ISTEP = 1
C
C -- band 13 is really band 8 during dwell, relabeled to avoid conflicts
C -- with band 8 during step mode.  insure we find band 8 channel here...
C
      IF (IBAND .EQ. 13) THEN
        KBAND = 8
        IBEG = 13
        IEND = 1
        ISTEP = -1
      ENDIF
C
      DO 100 I= IBEG,IEND,ISTEP
      CHANL = PFX(IPOS)
      IF(CHANL.LT.1.OR.CHANL.GT.38) GO TO 100
      IF(MAP(CHANL) .EQ. KBAND) THEN
         GTCHAN=CHANL
         RETURN
      ENDIF
      IPOS=IPOS+4
 100  CONTINUE
      GTCHAN=-1
      RETURN
      END

      SUBROUTINE VISCON(VISTAB)
      IMPLICIT NONE

      INTEGER VISTAB(*)

      ! symbolic constants & shared data

      INTEGER IVAL
      REAL XVAL
      INTEGER I

      ! external functions

      INTEGER BRKVAL
      REAL REAL
      REAL SQRT
      REAL NINT

      CHARACTER*4 CALTYP
      COMMON/BRKPNT/CALTYP

      IF( CALTYP .EQ. 'RAW ') THEN                                 
                                                                       
          DO 5 I=0,4095                                                
              VISTAB(I+1) = BRKVAL(REAL(I))                             
5         CONTINUE                                                      
                                                                       
       ELSE IF( CALTYP .EQ. 'BRIT') THEN             
                                                               
          DO 6 I=0,4095                                    
             XVAL = SQRT(REAL(I))*4.                    
             VISTAB(I+1) = BRKVAL(XVAL)           
6         CONTINUE                  
                                    
       ELSE                             
                                       
          DO 7 I=0,4095                             
             IVAL = NINT(SQRT(REAL(I))*4.)              
             VISTAB(I+1) = MIN0(IVAL, 255)                
7         CONTINUE                              
                                                           
       ENDIF                                          

      RETURN
      END

      SUBROUTINE IRCON(TYPE,BAND,CHANL,TABLE)
C
C--- Generate lookup TABLE to convert IR raw counts to:
C                 Unit	TYPE
C                 ----  ----
C		  RAD    0
C                 TEMP   1
C                 BRIT   2
      IMPLICIT NONE
      INTEGER TYPE
      INTEGER BAND
      INTEGER CHANL
      INTEGER TABLE(*)

      ! symbolic constants & shared data

      INTEGER ISSS
      INTEGER IDAY
      INTEGER ITIME
      INTEGER IAB(2,38)
      INTEGER IFAB(38)
      INTEGER IDUM(11)
      COMMON/CALBXX/ISSS,IDAY,ITIME,IAB,IFAB,IDUM

      CHARACTER*4 CALTYP
      COMMON/BRKPNT/CALTYP

      ! external functions

      INTEGER BRKVAL
      INTEGER GRYSCL
      REAL VASTBB

      ! local variables

      INTEGER FAB
      INTEGER I
      REAL AB0
      REAL AB1
      REAL RAD(4096)
      REAL RADVAL
      REAL TEMP(4096)
      REAL XBRIT
      REAL XFAB

      AB0=IAB(1,CHANL)
      AB1=IAB(2,CHANL)
      FAB=IFAB(CHANL)
      XFAB=2.**(15-FAB)

      IF(CALTYP.EQ.'RAW '.AND.TYPE.EQ.2) THEN
         DO 50  I=2,4096
         TABLE(I)=BRKVAL(REAL((I-1)*8))
 50      CONTINUE
         TABLE(1)=0
         RETURN
      ENDIF

C
C CALCULATE RADIANCE
C
      IF(CALTYP.EQ.'RAD '.AND.TYPE.EQ.2) THEN
         DO 100 I=2,4096
         RADVAL=AMAX1(0.0,(AB1*(I-1)/4.-AB0)/XFAB)
         TABLE(I)=BRKVAL(RADVAL)
100      CONTINUE
         TABLE(1)=0
         RETURN
      ELSE
         DO 110 I=2,4096
         RAD(I)=AMAX1(0.0,(AB1*(I-1)/4.-AB0)/XFAB)
         TABLE(I)=NINT(RAD(I)*1000.)
110      CONTINUE
         TABLE(1)=0
      ENDIF
      IF(TYPE.EQ.0) RETURN
C
C CALCULATE TEMPERATURE
C
      IF(CALTYP.EQ.'TEMP'.AND.TYPE.EQ.2) THEN
         DO 200 I=2,4096
         TABLE(I)=BRKVAL(VASTBB(RAD(I), ISSS, BAND))
200      CONTINUE
         TABLE(1)=0
         RETURN
      ELSE
         DO 210 I=2,4096
         TEMP(I)=VASTBB(RAD(I), ISSS, BAND)
         TABLE(I)=NINT(10.*TEMP(I))
210      CONTINUE
         TABLE(1)=0
      ENDIF
      IF(TYPE.EQ.1) RETURN
C
C CALCULATE GREYSCALE
C
      IF(CALTYP.EQ.'BRIT'.AND.TYPE.EQ.2) THEN
         DO 300 I=2,4096
         XBRIT=GRYSCL(TEMP(I))
         TABLE(I)=BRKVAL(XBRIT)
300      CONTINUE
         TABLE(1)=0
         RETURN
      ELSE
         DO 310 I=2,4096
         TABLE(I)=GRYSCL(TEMP(I))
310      CONTINUE
         TABLE(1)=0
      ENDIF

      RETURN
      END

C $ FUNCTION VASTBB(RAD, ISSS, KCH)  (RCD)
C $ CONVERT RADIANCE TO TEMPERATURE (K)
C $  RAD = (F) INPUT  CALIBRATED RADIANCE (FROM RADENC)
C $  ISSS = SSEC SATELLITE# 30=GOES6 32=GOES7
C $  KCH = (I) INPUT  BAND #
C $  VASTBB = (F) OUTPUT BRIGHTNESS TEMPERATURE
C $$ VASTBB = COMPUTATION,SATELLITE,SOUNDER,IO

      REAL FUNCTION VASTBB(RAD, ISSS, KCH)
      IMPLICIT NONE
      REAL RAD
      INTEGER KCH, ISSS

      ! local variables

      INTEGER KKCH
      REAL EXPN
      REAL TT

      ! initialized variables
      REAL G6FK1(12)
      REAL G6FK2(12)
      REAL G6TC(2,12)
      REAL G7FK1(12)
      REAL G7FK2(12)
      REAL G7TC(2,12)

C---     GOES-6 Coefficients       ---
      DATA G6FK1    / 0.371600E+04, 0.393600E+04, 0.408900E+04,
     >                0.433600E+04, 0.501900E+04, 0.128000E+06,
     >                0.585500E+04, 0.843800E+04, 0.310300E+05,
     >                0.387900E+05, 0.136100E+06, 0.195000E+06/

      DATA G6FK2    / 0.975800E+03, 0.994800E+03, 0.100700E+04,
     >                0.102700E+04, 0.107900E+04, 0.317500E+04,
     >                0.113600E+04, 0.128300E+04, 0.198000E+04,
     >                0.213300E+04, 0.324100E+04, 0.365300E+04/

      DATA G6TC     /-0.285900E-03, 0.100000E+01, 0.247200E-02,
     >                0.100000E+01, 0.233400E-02, 0.100000E+01,
     >                0.394800E-02, 0.999900E+00, 0.403100E-02,
     >                0.999900E+00, 0.685700E-01, 0.999800E+00,
     >                0.425600E-02, 0.999900E+00, 0.326300E+00,
     >                0.997300E+00, 0.682200E-01, 0.999700E+00,
     >                0.736000E+00, 0.997300E+00, 0.578700E-01,
     >                0.999900E+00, 0.421300E+00, 0.999100E+00/


C---     GOES-7 Coefficients       ---
      DATA G7FK1    / 0.376700E+04, 0.394000E+04, 0.416700E+04,
     >                0.436400E+04, 0.502500E+04, 0.129100E+06,
     >                0.585100E+04, 0.852500E+04, 0.310700E+05,
     >                0.392000E+05, 0.132500E+06, 0.193200E+06/

      DATA G7FK2    / 0.980300E+03, 0.995100E+03, 0.101400E+04,
     >                0.103000E+04, 0.107900E+04, 0.318500E+04,
     >                0.113500E+04, 0.128700E+04, 0.198100E+04,
     >                0.214000E+04, 0.321200E+04, 0.364200E+04/

      DATA G7TC     /-0.126700E-02, 0.100000E+01, 0.318000E-02,
     >                0.999900E+00, 0.238300E-02, 0.100000E+01,
     >                0.273900E-02, 0.999900E+00, 0.536000E-02,
     >                0.999900E+00, 0.735300E-01, 0.999800E+00,
     >                0.444700E-02, 0.999900E+00, 0.340800E+00,
     >                0.997300E+00, 0.647800E-01, 0.999800E+00,
     >                0.644800E+00, 0.997700E+00, 0.583000E-01,
     >                0.999900E+00, 0.415000E+00, 0.999100E+00/


      KKCH=KCH
C
C BECAUSE 13 IS REALLY BAND 8
C
      IF(KKCH.EQ.13) KKCH=8


      VASTBB=0.0
      IF(RAD.EQ.0.0) RETURN
      
c--- GOES 6 
      if( isss .eq. 30 ) then
         EXPN=G6FK1(KKCH)/RAD+1.
         TT=G6FK2(KKCH)/ALOG(EXPN)
         VASTBB=(TT-G6TC(1,KKCH))/G6TC(2,KKCH)

c--- GOES 7
      else if( isss .eq. 32 ) then
         EXPN=G7FK1(KKCH)/RAD+1.
         TT=G7FK2(KKCH)/ALOG(EXPN)
         VASTBB=(TT-G7TC(1,KKCH))/G7TC(2,KKCH)
	
      endif

      RETURN
      END

C $ FUNCTION GRYSCL(TEMPK)  (RCD)
C $ CONVERT A BRIGHTNESS TEMPERATURE TO A GREY SCALE
C $ TEMPK = (R) INPUT  TEMPERATURE IN DEGRESS KELVIN
C $$ GRYSCL = CONVERT

      INTEGER FUNCTION GRYSCL (TEMPK)
      IMPLICIT NONE
      REAL TEMPK

      ! initialized variables

      INTEGER CON1
      INTEGER CON2
      REAL TLIM

      DATA CON1 / 418 /
      DATA CON2 / 660 /
      DATA TLIM / 242.0 /

C CONVERT A BRIGHTNESS TEMPERATURE TO A GREY SCALE
C     TEMPK---TEMPERATURE IN DEGREES KELVIN
      IF (TEMPK .GE. TLIM) GOTO 100
         GRYSCL = MIN0 (CON1 - IFIX (TEMPK), 255)
         GOTO 200
100   CONTINUE
         GRYSCL = MAX0 (CON2 - IFIX (2 * TEMPK), 0)
200   CONTINUE
      RETURN
      END
