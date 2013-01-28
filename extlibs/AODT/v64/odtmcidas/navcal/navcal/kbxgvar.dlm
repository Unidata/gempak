C Copyright(c) 1998, Space Science and Engineering Center, UW-Madison
C Refer to "McIDAS Software Acquisition and Distribution Policies"
C in the file  mcidas/data/license.txt

C *** $Id: kbxgvar.dlm,v 1.32 2003/01/31 20:11:54 daves Exp $ ***

      FUNCTION KBXINI(CIN,COUT,IOPT)

C--- Calibration module for GVAR data


      include 'areaparm.inc'

      CHARACTER*4 CIN,COUT
      integer calflg, calarr
      DIMENSION IOPT(*)
      COMMON/GVARXX/JTYPE,ISOU,IDES,JOPT(NUMAREAOPTIONS)
      common/gvrcal/calflg,calarr(128)

      CHARACTER*4 CALTYP
      COMMON/BRKPNT/ CALTYP

      CALL MOVW(NUMAREAOPTIONS,IOPT,JOPT)
      JTYPE=0
      calflg = 0
      ISOU=IOPT(1)
      IDES=IOPT(2)
      IF(CIN.EQ.'RAW '.AND.COUT.EQ.'RAD ') JTYPE=1
      IF(CIN.EQ.'RAW '.AND.COUT.EQ.'ALB ') JTYPE=1
      IF(CIN.EQ.'RAW '.AND.COUT.EQ.'TEMP') JTYPE=2
      IF(CIN.EQ.'RAW '.AND.COUT.EQ.'BRIT') JTYPE=3
      IF(CIN.EQ.'RAW '.AND.COUT.EQ.'MODB') JTYPE=4

      IF(JTYPE.EQ.0) GO TO 900
      KBXINI=0
      RETURN

900   CONTINUE
      KBXINI=-1

      RETURN
      END

      FUNCTION KBXCAL(CALB,IDIR,NVAL,JBAND,IBUF)

      include 'areaparm.inc'

      PARAMETER (NTABLE = 2)
      PARAMETER (NUMFLG = 100)	! Threshold to determine when to 
				! generate the entire lookup table
				! vs just the region enclosing the data.

      COMMON/GVARXX/JTYPE,ISOU,IDES,JOPT(NUMAREAOPTIONS)

      CHARACTER*4 CALTYP
      COMMON/BRKPNT/ CALTYP

      integer calflg, calarr
      common/gvrcal/calflg,calarr(128)

      INTEGER IBAND,JBAND
      INTEGER CHKCOD
      INTEGER BRKVAL
      INTEGER GRYSCL
      INTEGER CALB(*)
      INTEGER IDIR(*)
      INTEGER IOFF
      INTEGER ISAT
      INTEGER TBLHI
      INTEGER TBLLO
      INTEGER*2 IBUF(*)
      INTEGER IARR(128),ITAB(32768, NTABLE)
      INTEGER HVCOD
      INTEGER INTAB(256)
      INTEGER LIST(3, NTABLE)
      INTEGER AREA, TYPE, BAND
      INTEGER SVGOFF, SVBOFF, SVAOFF, SVG2OFF
      PARAMETER (AREA = 1, TYPE = 2, BAND = 3)
      PARAMETER (NITEMS = 3 * NTABLE)

      REAL*4 TEMP, F, G2TERM
      REAL*4 GAIN, BIAS
      REAL*4 RAD, ALB, NEWARR(128)
      REAL*4 VG2AVG, VGAVG, VBAVG, VALB, SCALE

      LOGICAL BRKTST

      DATA LASTIM/-1/, INDX/0/, LIST/NITEMS * -1/, JSHORT/0/

C  OFFSETS INTO THE ARRAY RETURNED BY CHKIR() AND CHKVIS()
      DATA IVGOFF/9/, IVBOFF/1/, IVAOFF/25/, IVG2OFF/17/
      DATA SVGOFF/5/, SVBOFF/1/, SVAOFF/13/, SVG2OFF/10/

C  MCIDAS 'D' KEY TAKES A LONG TIME WHEN USING A LOOKUP
C  TABLE OF SIZE 32768.  IF WE ARE ONLY CALIBRATING NUMLFG PIXELS OR 
C  FEWER SET THE TBLLO AND TBLHI BOUNDS TO BE THE RAW VALUES IN IBUF.
C  MGVATB RIGHT SHIFTS 1-BIT, SO WE ALSO NEED TO DIVIDE BY 2
C  SO WE ARE USING THE CORRECT INDEX IN THE LOOKUP TABLE.

       ISAT = IDIR(3)

C If this is imager and band 6, change to band 5 for index purposes

       IF(MOD(ISAT,2).EQ.0 .AND. JBAND.EQ.6) THEN
           IBAND=5
       ELSE
           IBAND=JBAND
       ENDIF

       IF(NVAL .LE. NUMFLG .AND. JSHORT .EQ. 0) THEN
C        If first call and NVAL .le. 100, we assume the pixel values are
C        similar and occupy a limited range, so we generate an incomplete
C        lookup table covering only that range.
         TBLLO = 32767
         TBLHI = 0
         JSHORT = 1
         DO 5 I = 1, NVAL
           IF(IBUF(I) .LT. 0) THEN
              ITEMP = IBUF(I)
              ITEMP = (IAND(ITEMP, 65535) / 2) + 1
           ELSE
              ITEMP = (IBUF(I) / 2) + 1
           ENDIF
           TBLLO = MIN0(TBLLO, ITEMP)
           TBLHI = MAX0(TBLHI, ITEMP)
 5       CONTINUE

       ELSE IF (JSHORT .EQ. 1) THEN
C        This is the second call, after generating an incomplete lookup table,
C        so we assume there may be more calls and take the time now to reset
C        the indices and generate a complete first table.

         JSHORT = -1
         TBLLO = 1
         TBLHI = 32768
         LIST(AREA,1) = -1

       ELSE

C        Is this table already generated for the area, band, and calibration
C        data type?  Then we make sure we don't waste time generating a short
C        table again if NVAL should drop below NUMFLG on subsequent calls.

         JSHORT = -1

         DO 10 I = 1, NTABLE
            IF( IDIR(33) .EQ. LIST(AREA, I) .AND.
     *           IBAND    .EQ. LIST(BAND, I) .AND.
     *           JTYPE    .EQ. LIST(TYPE, I)) THEN

               INDX = I
               GO TO 100
            ENDIF
10       CONTINUE

         TBLLO = 1
         TBLHI = 32768

       ENDIF


C Read in calibration block, if not read in

       IF(IDIR(33) .NE. LASTIM) THEN
         LASTIM=IDIR(33)

C check calflg to see if cal block passed in via kbxopt
C
         if (calflg .ne. 0) then
           CALL MOVW(128, calarr, IARR)
           hvcod = 1

C check to see if cal block present in area
C
         else IF(IDIR(63) .EQ. 0) THEN
           HVCOD = 0
         ELSE
	   IOFF = IDIR(63)
	   CALL ARAGET(IDIR(33), IOFF, 512, IARR)
           hvcod = 1
         ENDIF

         IF(HVCOD .EQ. 1) THEN
           HVCOD = CHKCOD(IARR)
         ENDIF

       ENDIF

       DO 15 I = 1, NTABLE
         IF(LIST(AREA, I) .EQ. -1) THEN
            INDX = I
            GO TO 16
         ENDIF
15     CONTINUE

       INDX = 1

16     CONTINUE

	 LIST(AREA, INDX) = IDIR(33)
	 LIST(BAND, INDX) = IBAND
	 LIST(TYPE, INDX) = JTYPE
C Check if stretch table to be applied to RAW counts.
C The index is multiplied by 2 before passing to BRKVAL to compensate for the
C divide by 2 in MGVATB.

         BRKTST = (JTYPE .EQ. 4 .AND. CALTYP .EQ. 'RAW ')

         IF (BRKTST) THEN

           DO 36 I=TBLLO, TBLHI
              ITAB(I, INDX) = BRKVAL(REAL(I-1)*2.)
 36        CONTINUE
	
           GO TO 100

         ENDIF

C
C--- VISIBLE ONLY -- IMAGER  or SOUNDER
C
         IF( (IBAND .EQ. 1 .AND. MOD(ISAT,2) .EQ. 0)  .OR.
     *       (IBAND .EQ. 19.AND. MOD(ISAT,2) .EQ. 1) ) THEN

C          CHECK THE VISIBLE COEFFICIENTS IN CODICIL AGAINST
C          FACTORY.  COEFFICIENTS ARE RETURNED IN NEWARR.

           CALL CHKVIS(ISAT, IARR, NEWARR, HVCOD)

C          CALL DMPARR(NEWARR, 1, 25)
           VG2AVG = 0.0
           VGAVG = 0.0
           VBAVG = 0.0

C          COMPUTE THE AVERAGE OF THE VISIBLE COUNT TO RADIANCE
C          COEFFICIENTS

C Imager:
           IF( IBAND .EQ. 1) THEN
              DO 101 ITEMP=1,8
                 VG2AVG = VG2AVG + NEWARR(ITEMP+IVG2OFF-1)
                 VGAVG = VGAVG + NEWARR(ITEMP+IVGOFF-1)
                 VBAVG = VBAVG + NEWARR(ITEMP+IVBOFF-1)
101           CONTINUE

              VG2AVG = VG2AVG / 8.0
              VGAVG = VGAVG / 8.0
              VBAVG = VBAVG / 8.0
              VALB = NEWARR(IVAOFF)
              SCALE = 16.

C Sounder:
	    ELSE

               DO 102 ITEMP=1,4
                  VG2AVG = VG2AVG + NEWARR(ITEMP+SVG2OFF-1)
                  VGAVG = VGAVG + NEWARR(ITEMP+SVGOFF-1)
                  VBAVG = VBAVG + NEWARR(ITEMP+SVBOFF-1)
102            CONTINUE

               VG2AVG = VG2AVG / 4.0
               VGAVG = VGAVG / 4.0
               VBAVG = VBAVG / 4.0
               VALB = NEWARR(SVAOFF)
               SCALE = 4.

            ENDIF

C  DO VISIBLE ALBEDO CALCULATIONS

C Check if stretch table to be applied to ALB.

	   BRKTST = (JTYPE .EQ. 4 .AND. CALTYP .EQ. 'ALB ')

	   G2TERM = 1.0


C If converting to BRIT, work problem backward and fill in table

           IF(JTYPE .EQ. 3 .AND. ABS(VG2AVG) .LT. 0.0001) THEN

              DO 38 I= 0, 255
                 TEMP = (I + .5)/25.5
                 ALBEDO = TEMP * TEMP/100.
                 IRAW = NINT(SCALE*(ALBEDO/VALB - VBAVG - 1.)/VGAVG)
                 IRAW = MIN0(IRAW, 32767)
                 IRAW = MAX0(IRAW, 0)
                 INTAB(I+1) = IRAW
38            CONTINUE
              CALL EXPTAB(INTAB, ITAB(1, INDX), 0)
              GO TO 100
           ENDIF

           IF( NVAL .GT. NUMFLG) THEN

              IRAW = NINT(SCALE*(0.0/VALB - VBAVG - 1.)/VGAVG)
              TBLLO = MAX0(1, IRAW + 1)

              IRAW = NINT(SCALE*(1.0/VALB - VBAVG - 1.)/VGAVG)
              TBLHI = MIN0(32768, IRAW + 1)
           ENDIF

C Convert to ALB or output as MODB using ALB

	   IF( JTYPE .EQ. 1 .OR. BRKTST) THEN
	
              DO 40 I=TBLLO, TBLHI

C ---        VISIBLE IMAGER DATA IS 10-BIT  SHIFTED LEFT 5 BITS
C ---        TO OCCUPY 15 BITS IN MCIDAS AREA.  MGVATB ROUTINE
C ---        RIGHT SHIFTS ONE BIT BEFORE USING LOOKUP TABLE.
C ---        SO WE MUST DIVIDE BY SIXTEEN TO RIGHT SHIFT 4 BITS

                F = (I  - 1)/SCALE

C ---        Set the 2nd order visible gain term if the power
C ----       is some number other than zero.

                IF(ABS(VG2AVG) .GT. 0.0001) THEN
                  G2TERM = F**VG2AVG
                ENDIF

                ALB = (G2TERM + (F * VGAVG) + VBAVG) * VALB
                IF(ALB .LT. 0.0) ALB = 0.0


        	IF (BRKTST) THEN

C Scale the Albedo by 100 for stretch table (user inputs in percent)

                   ITAB(I, INDX) = BRKVAL(REAL(ALB * 100.0))
                ELSE

C Scale Albedo by 1000  (output in 10ths of percent)

                   ITAB(I, INDX) = NINT(ALB * 1000. )

                ENDIF

40            CONTINUE

           ELSE IF( JTYPE .GE. 3 ) THEN

C Convert to BRIT or output as MODB using BRIT

C Check if stretch table to be applied to BRIT.

	      BRKTST = (JTYPE .EQ. 4 .AND. CALTYP .EQ. 'BRIT')

             DO 45 I=TBLLO, TBLHI

                F = (I  - 1)/SCALE

                IF(ABS(VG2AVG) .GT. 0.0001) THEN
                  G2TERM = F**VG2AVG
                ENDIF

                ALB = (G2TERM + (F * VGAVG) + VBAVG) * VALB
                IF(ALB .LT. 0.0) ALB = 0.0

	       IF(BRKTST) THEN
                  ITAB(I, INDX) = BRKVAL(SQRT(100.0*ALB)*25.5)
	       ELSE
                  ITAB(I, INDX) = NINT(SQRT(100.0*ALB)*25.5)
	       ENDIF

45           CONTINUE
           ENDIF
C
C --- IR FOR IMAGER AND SOUNDER
C
           ELSE

C
C--- DO RADIANCE CALCULATION
C
               CALL CHKIR(ISAT, IBAND, IARR, GAIN, BIAS, HVCOD)

C Imager:
	       IF( MOD(ISAT,2) .EQ. 0) THEN
                  SCALE = 16.
                  IGVBAND = IBAND + 20
C                 DETERMINE WHICH DETECTOR CONFIGURATION IS IN USE
                  IGVDET = CALB(14)/65536
                  IF( IGVDET.EQ.0 .AND. IDIR(49).NE.0 ) THEN
                     IGVDET=1
                  ELSE IF( IGVDET.EQ.1023 .AND. IDIR(49).NE.0 ) THEN
                     IGVDET=2
                  ELSE IF( IDIR(49).EQ.0 ) THEN    ! NO DOC SECTION
                     IGVDET=0
                  ELSE
                     IGVDET=0                   ! UNDEFINED CONFIGURATION
                  END IF

C Sounder:
	       ELSE IF( MOD(ISAT,2) .EQ. 1) THEN
                  SCALE = 0.5
                  IGVBAND = IBAND
               ENDIF

C Check if stretch table to be applied to RAD.

               BRKTST = (JTYPE .EQ. 4 .AND. CALTYP .EQ. 'RAD ')

C If converting to BRIT, work problem backward and fill in table

               IF( JTYPE .EQ. 3) THEN
                  DO 801 I = 0, 255
                  TEMP = TMPSCL(I)
                  RAD = GVATBB(TEMP, IGVBAND, IGVDET, ISAT, 1)
                  RAW = RAD * GAIN + BIAS
                  IRAW = NINT(RAW * SCALE + 1.)
		  IRAW = MIN0( IRAW, 32767)
                  IRAW = MAX0( IRAW, 0)
                  INTAB(I+1) = IRAW
801               CONTINUE

                  CALL EXPTAB(INTAB, ITAB(1, INDX), 1)
                  GO TO 100
              ENDIF

	      IF (NVAL .GT. NUMFLG) THEN

                  RAD = GVATBB(0.0, IGVBAND, IGVDET, ISAT, 1)
                  RAW = RAD * GAIN + BIAS
                  IRAW = NINT(RAW * SCALE + 1.)
                  TBLLO = MAX0( IRAW + 1, 1)

                  RAD = GVATBB(345.0, IGVBAND, IGVDET, ISAT, 1)
                  RAW = RAD * GAIN + BIAS
                  IRAW = NINT(RAW * SCALE + 1.)
		  TBLHI = MIN0( IRAW + 1, 32768)
              ENDIF

              IF( JTYPE .EQ. 1 .OR. (JTYPE.EQ.4.AND.BRKTST) ) THEN
                 DO 6 I=TBLLO, TBLHI
                    F = I
                    TEMP = (F-1.0) /  SCALE
                    RAD = (TEMP - BIAS)/ GAIN

		    IF (BRKTST) THEN
                       ITAB(I, INDX) = BRKVAL(RAD)
		    ELSE
                       ITAB(I, INDX) = NINT(RAD * 1000.)
		    ENDIF

6                CONTINUE

C
C--- DO TEMPERATURE CALCULATION
C
               ELSE IF(JTYPE .EQ. 2 .OR. JTYPE .EQ. 4) THEN

C Check if stretch table to be applied to TEMP.

                 BRKTST = (JTYPE .EQ. 4 .AND. CALTYP .EQ. 'TEMP')

                 DO 7 I = TBLLO, TBLHI

                    F = I
                    TEMP = (F-1.0) /  SCALE
                    RAD = (TEMP - BIAS)/ GAIN

		    TEMP = GVATBB(RAD, IGVBAND, IGVDET, ISAT, 0)

 		    IF (BRKTST) THEN
                       ITAB(I, INDX) = BRKVAL(TEMP)
		    ELSE
                       ITAB(I, INDX) = NINT(TEMP * 10.)
		    ENDIF

7                CONTINUE

C
C--- IMAGER GRAY SCALE CALC
C
               ELSE IF( JTYPE .GT. 2) THEN

C Check if stretch table to be applied to TEMP.

                  BRKTST = (JTYPE .EQ. 4 .AND. CALTYP .EQ. 'BRIT')

                  DO 8 I = TBLLO, TBLHI

                    F = I
                    TEMP = (F-1.0) /  SCALE
                    RAD = (TEMP - BIAS)/ GAIN

		    TEMP = GVATBB(RAD, IGVBAND, IGVDET, ISAT, 0)
                    ITEMP = GRYSCL(TEMP)
                    IF(ITEMP .LT. 0) ITEMP=0
                    IF(ITEMP.GT. 255) ITEMP=255

		    IF (BRKTST) THEN
                       ITAB(I, INDX) = BRKVAL(REAL(ITEMP))
		    ELSE
                       ITAB(I, INDX) = ITEMP
		    ENDIF

8                 CONTINUE

               ENDIF
       ENDIF

 100  CALL MGVATB(NVAL,ISOU,IDES,IBUF,ITAB(1, INDX))

      KBXCAL=0
      RETURN
      END

C ----------------------------------------------------------
      FUNCTION KBXOPT(CFUNC,IIN,IOUT)

      include 'areaparm.inc'

      CHARACTER*4 CFUNC
      CHARACTER*12 CFILE

      integer calflg, calarr
      common/gvrcal/calflg,calarr(128)

      INTEGER BRKSET
      COMMON/GVARXX/JTYPE,ISOU,IDES,JOPT(NUMAREAOPTIONS)

      CHARACTER*4 CALTYP
      COMMON/BRKPNT/ CALTYP

      INTEGER IIN(*),IOUT(*),ISAT
      IBAND = IIN(4)
      ISAT = IIN(1)

      IF(CFUNC.EQ.'KEYS') THEN

C --- VISIBLE ---
         IF((IBAND .EQ. 1 .AND. MOD(ISAT,2) .EQ. 0) .OR.
     *      (IBAND .EQ. 19.AND. MOD(ISAT,2) .EQ. 1)) THEN
            IOUT(1)=3
            IOUT(2)=LIT('RAW ')
            IOUT(3)=LIT('ALB ')
            IOUT(4)=LIT('BRIT')
C
C--- check to see if a stretch table has been applied
C
            IF(ISCHAR(IIN(38)).EQ.1) THEN
               CALL MOVCW(IIN(38),CFILE)
               IF(BRKSET(CFILE,CALTYP).NE.0) THEN
                   KBXOPT = -3
                   RETURN
               ENDIF
            ENDIF

         ELSE

C --- IR ---
            IOUT(1)=4
            IOUT(2)=LIT('RAW ')
            IOUT(3)=LIT('RAD ')
            IOUT(4)=LIT('TEMP')
            IOUT(5)=LIT('BRIT')
C
C--- check to see if a stretch table has been applied
C
            IF(ISCHAR(IIN(38)).EQ.1) THEN
               CALL MOVCW(IIN(38),CFILE)
               IF(BRKSET(CFILE,CALTYP).NE.0) THEN
                   KBXOPT = -3
                   RETURN
               ENDIF
            ENDIF

         ENDIF

C
C--- BREAKPOINT TABLE OPTION
C
      ELSE IF(CFUNC.EQ.'BRKP') THEN
         CALL MOVCW(IIN(1),CFILE)
         IF(BRKSET(CFILE,CALTYP).NE.0) THEN
                KBXOPT = -3
                RETURN
         ENDIF
         KBXOPT = 0

      ELSE IF(CFUNC.EQ.'INFO') THEN

C --- VISIBLE ---
         IF((IIN(1) .EQ. 1 .AND. MOD(IIN(2),2) .EQ. 0) .OR.
     *      (IIN(1) .EQ. 19.AND. MOD(IIN(2),2) .EQ. 1)) THEN
            IOUT(1)=3
            IOUT(2)=LIT('RAW ')
            IOUT(3)=LIT('ALB ')
            IOUT(4)=LIT('BRIT')
	    IOUT(5)=LIT('    ')
	    IOUT(6)=LIT(' %  ')
	    IOUT(7)=LIT('    ')
	    IOUT(8)=1
	    IOUT(9)=10
	    IOUT(10)=1
         ELSE

C --- IR ---
            IOUT(1)=4
            IOUT(2)=LIT('RAW ')
            IOUT(3)=LIT('RAD ')
            IOUT(4)=LIT('TEMP')
            IOUT(5)=LIT('BRIT')
	    IOUT(6)=LIT('    ')
	    IOUT(7)=LIT('MW**')
	    IOUT(8)=LIT(' K  ')
	    IOUT(9)=LIT('    ')
	    IOUT(10)=1
	    IOUT(11)=1000
	    IOUT(12)=10
	    IOUT(13)=1
         ENDIF

C cal block being passed in rather than extracted from area on disk

      ELSE IF (CFUNC .EQ. 'CALB') THEN
        CALFLG = 1
        CALL MOVW(128, IIN, calarr)
      ENDIF

      KBXOPT=0
      RETURN
      END



C ----------------------------------------------------------

      REAL FUNCTION GVATBB(VAL,KCH, KDET, ISAT, SWITCH)
C $ FUNCTION GVATBB(VAL, KCH, KDET, ISAT, SWITCH)
C $ CONVERT RADIANCE TO TEMPERATURE (K) OR VICE-VERSA
C $  VAL = (R) INPUT  CALIBRATED RADIANCE OR TEMP (DEPENDING ON SWITCH)
C $  KCH = (I) INPUT  BAND #
C $  KDET = (I) INPUT  DETECTOR #
C $  ISAT = (I) INPUT  SAT #
C $  SWITCH = (I) INPUT  0:  RAD-> TEMP   1: TEMP-> RAD
C $  GVATBB = (R) OUTPUT BRIGHTNESS TEMPERATURE OR RADIANCE
C $$ GVATBB = COMPUTATION,SATELLITE,SOUNDER,IO

      PARAMETER (NSAT=5)

      DIMENSION FK1(25,3*NSAT),FK2(25,3*NSAT),TC(2,25,3*NSAT)

      INTEGER SWITCH, ISAT, SATIDX


C THE FOLLOWING ARE CONSTANTS TO CALCULATE TEMPERATURE FROM
C CALIBRATED RADIANCE.  CONSTANTS ARE ORDERED IN THE ARRAY BY
C BAND WITH SOUNDER BEING INDEXES 1 - 18.  19 - 21 ARE UNUSED
C AND IMAGER IS HANDLED BY ADDING 20 TO THE BAND.  IMAGER
C CONSTANTS ARE INDEXES 22 - 25.

C THE FIRST 3 SETS ARE AVERAGED CONSTANTS (DETECTOR INDEPENDENT), THE
C NEXT 3 ARE DETECTOR 1 DEPENDENCIES, THE NEXT THREE DETECTOR 2.  NOTE
C THAT IF THE WRONG DETECTOR IS CHOSEN, THE ERROR WILL DOUBLE.  IF THE
C DETECTOR IS NOT SPECIFIED (VALUE OF KDET=0), THE AVERAGED CONSTANTS
C ARE USED.  THE DETECTOR IS NORMALLY IDENTIFIED FROM THE LINE PREFIX,
C SO IF A LINE IS COPIED WITHOUT THE PREFIX, THE DETECTOR INFORMATION
C WILL BE LOST.  THE AVERAGED CONSTANTS ARE FROM HAL WOOLF (PREVIOUS
C VERSION OF KBXGVAR), WHILE THE DETECTOR SPECIFIC CONSTANTS ARE FROM
C MIKE WEINREB MEMO OF 5/29/98 TO TIM SCHMIT.  THE GOES-11 CONSTANTS ARE
C FROM TIM SCHMIT VIA DEE WADE IN A MEMO DATED 3/27/00, AND ARE NOT
C DETECTOR SPECIFIC.

C GOES-8 FK1'S (DETECTOR AVERAGE)


      DATA (FK1(I,1),I=1,25)/
     *           0.3756810E+04, 0.4011100E+04, 0.4296870E+04,
     *           0.4681130E+04, 0.4975250E+04, 0.5881410E+04,
     *           0.6787440E+04, 0.8873710E+04, 0.1299794E+05,
     *           0.2862932E+05, 0.3424830E+05, 0.4311430E+05,
     *           0.1242353E+06, 0.1281235E+06, 0.1351482E+06,
     *           0.1691671E+06, 0.1882350E+06, 0.2257944E+06,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.1999862E+06, 0.3879239E+05, 0.9737930E+04,
     *           0.6944640E+04/

C GOES-9 FK1'S (DETECTOR AVERAGE)

      DATA (FK1(I,2),I=1,25)/
     *           0.3765120E+04, 0.3981160E+04, 0.4281880E+04,
     *           0.4678910E+04, 0.4962590E+04, 0.5860420E+04,
     *           0.6770320E+04, 0.8958910E+04, 0.1296593E+05,
     *           0.2839828E+05, 0.3420134E+05, 0.4252514E+05,
     *           0.1240574E+06, 0.1280114E+06, 0.1348497E+06,
     *           0.1678142E+06, 0.1888012E+06, 0.2258565E+06,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.1988078E+06, 0.3873241E+05, 0.9717210E+04,
     *           0.6899470E+04/

C GOES-10 FK1'S (DETECTOR AVERAGE)

      DATA (FK1(I,3),I=1,25)/
     *           0.37305E+04,   0.40039E+04,   0.43124E+04,
     *           0.46616E+04,   0.49734E+04,   0.58698E+04,
     *           0.68161E+04,   0.89404E+04,   0.12973E+05,
     *           0.28708E+05,   0.34401E+05,   0.43086E+05,
     *           0.12468E+06,   0.12882E+06,   0.13532E+06,
     *           0.16853E+06,   0.18862E+06,   0.22487E+06,
     *           0.43370E+08,   0.00000E+00,   0.43370E+08,
     *           0.19841E+06,   0.39086E+05,   0.97744E+04,
     *           0.68286E+04/

C GOES-11 FK1'S (DETECTOR AVERAGE)

      DATA (FK1(I,4),I=1,25)/
     *           003765.61,     003992.81,     004303.85,
     *           004680.65,     004956.33,     005858.85,
     *           006866.75,     008939.46,     013026.87,
     *           028611.34,     034527.00,     043236.68,
     *           124987.94,     128845.45,     134487.50,
     *           169502.47,     188726.92,     225757.77,
     *           000000.00,     000000.00,     000000.00,
     *           200178.17,     038788.66,     009653.43,
     *           006877.84/

C GOES-12 FK1s (DETECTOR AVERAGE)

      DATA (FK1(I,5),I=1,25)/
     *           0.37778E+04, 0.40086E+04, 0.43085E+04,
     *           0.47041E+04, 0.50134E+04, 0.58645E+04,
     *           0.69071E+04, 0.90388E+04, 0.12972E+05,
     *           0.28931E+05, 0.34531E+05, 0.43340E+05,
     *           0.12492E+06, 0.12822E+06, 0.13535E+06,
     *           0.16981E+06, 0.18954E+06, 0.22538E+06,
     *           0.00000E+00, 0.00000E+00, 0.00000E+00,
     *           0.20096E+06, 0.43702E+05, 0.96859E+04,
     *           0.50471E+04/

C GOES-8 FK1'S (DETECTOR 1)

      DATA (FK1(I,NSAT+1),I=1,25)/
     *           0.3756810E+04, 0.4011100E+04, 0.4296870E+04,
     *           0.4681130E+04, 0.4975250E+04, 0.5881410E+04,
     *           0.6787440E+04, 0.8873710E+04, 0.1299794E+05,
     *           0.2862932E+05, 0.3424830E+05, 0.4311430E+05,
     *           0.1242353E+06, 0.1281235E+06, 0.1351482E+06,
     *           0.1691671E+06, 0.1882350E+06, 0.2257944E+06,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.1990583E+06, 0.3876157E+05, 0.9713928E+04,
     *           0.6985630E+04/

C GOES-9 FK1'S (DETECTOR 1)

      DATA (FK1(I,NSAT+2),I=1,25)/
     *           0.3765120E+04, 0.3981160E+04, 0.4281880E+04,
     *           0.4678910E+04, 0.4962590E+04, 0.5860420E+04,
     *           0.6770320E+04, 0.8958910E+04, 0.1296593E+05,
     *           0.2839828E+05, 0.3420134E+05, 0.4252514E+05,
     *           0.1240574E+06, 0.1280114E+06, 0.1348497E+06,
     *           0.1678142E+06, 0.1888012E+06, 0.2258565E+06,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.1987011E+06, 0.3875450E+05, 0.9722976E+04,
     *           0.6909796E+04/

C GOES-10 FK1'S (DETECTOR 1)

      DATA (FK1(I,NSAT+3),I=1,25)/
     *           0.37305E+04,   0.40039E+04,   0.43124E+04,
     *           0.46616E+04,   0.49734E+04,   0.58698E+04,
     *           0.68161E+04,   0.89404E+04,   0.12973E+05,
     *           0.28708E+05,   0.34401E+05,   0.43086E+05,
     *           0.12468E+06,   0.12882E+06,   0.13532E+06,
     *           0.16853E+06,   0.18862E+06,   0.22487E+06,
     *           0.43370E+08,   0.00000E+00,   0.43370E+08,
     *           0.1981894E+06, 0.3910085E+05, 0.9770261E+04,
     *           0.6832162E+04/

C GOES-11 FK1'S (DETECTOR 1)

      DATA (FK1(I,NSAT+4),I=1,25)/
     *           003765.61,     003992.81,     004303.85,
     *           004680.65,     004956.33,     005858.85,
     *           006866.75,     008939.46,     013026.87,
     *           028611.34,     034527.00,     043236.68,
     *           124987.94,     128845.45,     134487.50,
     *           169502.47,     188726.92,     225757.77,
     *           000000.00,     000000.00,     000000.00,
     *           200178.17,     038788.66,     009653.43,
     *           006877.84/

C GOES-12 FK1s (DETECTOR 1)

      DATA (FK1(I,NSAT+5),I=1,25)/
     *           0.37778E+04, 0.40086E+04, 0.43085E+04,
     *           0.47041E+04, 0.50134E+04, 0.58645E+04,
     *           0.69071E+04, 0.90388E+04, 0.12972E+05,
     *           0.28931E+05, 0.34531E+05, 0.43340E+05,
     *           0.12492E+06, 0.12822E+06, 0.13535E+06,
     *           0.16981E+06, 0.18954E+06, 0.22538E+06,
     *           0.00000E+00, 0.00000E+00, 0.00000E+00,
     *           0.20096E+06, 0.43699E+05, 0.96859E+04,
     *           0.50485E+04/

C GOES-8 FK1'S (DETECTOR 2)

      DATA (FK1(I,2*NSAT+1),I=1,25)/
     *           0.3756810E+04, 0.4011100E+04, 0.4296870E+04,
     *           0.4681130E+04, 0.4975250E+04, 0.5881410E+04,
     *           0.6787440E+04, 0.8873710E+04, 0.1299794E+05,
     *           0.2862932E+05, 0.3424830E+05, 0.4311430E+05,
     *           0.1242353E+06, 0.1281235E+06, 0.1351482E+06,
     *           0.1691671E+06, 0.1882350E+06, 0.2257944E+06,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.1995047E+06, 0.3879239E+05, 0.9747653E+04,
     *           0.6984128E+04/

C GOES-9 FK1'S (DETECTOR 2)

      DATA (FK1(I,2*NSAT+2),I=1,25)/
     *           0.3765120E+04, 0.3981160E+04, 0.4281880E+04,
     *           0.4678910E+04, 0.4962590E+04, 0.5860420E+04,
     *           0.6770320E+04, 0.8958910E+04, 0.1296593E+05,
     *           0.2839828E+05, 0.3420134E+05, 0.4252514E+05,
     *           0.1240574E+06, 0.1280114E+06, 0.1348497E+06,
     *           0.1678142E+06, 0.1888012E+06, 0.2258565E+06,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.1987011E+06, 0.3873241E+05, 0.9713304E+04,
     *           0.6911536E+04/

C GOES-10 FK1'S (DETECTOR 2)

      DATA (FK1(I,2*NSAT+3),I=1,25)/
     *           0.37305E+04,   0.40039E+04,   0.43124E+04,
     *           0.46616E+04,   0.49734E+04,   0.58698E+04,
     *           0.68161E+04,   0.89404E+04,   0.12973E+05,
     *           0.28708E+05,   0.34401E+05,   0.43086E+05,
     *           0.12468E+06,   0.12882E+06,   0.13532E+06,
     *           0.16853E+06,   0.18862E+06,   0.22487E+06,
     *           0.43370E+08,   0.00000E+00,   0.43370E+08,
     *           0.1981894E+06, 0.39086E+05,   0.976673E+04,
     *           0.6832463E+04/

C GOES-11 FK1'S (DETECTOR 2)

      DATA (FK1(I,2*NSAT+4),I=1,25)/
     *           003765.61,     003992.81,     004303.85,
     *           004680.65,     004956.33,     005858.85,
     *           006866.75,     008939.46,     013026.87,
     *           028611.34,     034527.00,     043236.68,
     *           124987.94,     128845.45,     134487.50,
     *           169502.47,     188726.92,     225757.77,
     *           000000.00,     000000.00,     000000.00,
     *           200178.17,     038788.66,     009653.43,
     *           006877.84/

C GOES-12 FK1s (DETECTOR 2)

      DATA (FK1(I,2*NSAT+5),I=1,25)/
     *           0.37778E+04, 0.40086E+04, 0.43085E+04,
     *           0.47041E+04, 0.50134E+04, 0.58645E+04,
     *           0.69071E+04, 0.90388E+04, 0.12972E+05,
     *           0.28931E+05, 0.34531E+05, 0.43340E+05,
     *           0.12492E+06, 0.12822E+06, 0.13535E+06,
     *           0.16981E+06, 0.18954E+06, 0.22538E+06,
     *           0.00000E+00, 0.00000E+00, 0.00000E+00,
     *           0.20096E+06, 0.43731E+05, 0.96859E+04,
     *           0.50485E+04/

C GOES-8 FK2'S (DETECTOR AVERAGE)

      DATA (FK2(I,1),I=1,25)/
     *           0.9794000E+03, 0.1001010E+04, 0.1024240E+04,
     *           0.1053910E+04, 0.1075530E+04, 0.1137220E+04,
     *           0.1192850E+04, 0.1304330E+04, 0.1481300E+04,
     *           0.1927330E+04, 0.2045960E+04, 0.2209150E+04,
     *           0.3143640E+04, 0.3176100E+04, 0.3233120E+04,
     *           0.3484360E+04, 0.3610650E+04, 0.3836390E+04,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.3684270E+04, 0.2132720E+04, 0.1345370E+04,
     *           0.1201990E+04/

C GOES-9 FK2'S (DETECTOR AVERAGE)

      DATA (FK2(I,2),I=1,25)/
     *           0.9801200E+03, 0.9985200E+03, 0.1023050E+04,
     *           0.1053740E+04, 0.1074620E+04, 0.1135870E+04,
     *           0.1191850E+04, 0.1308490E+04, 0.1480080E+04,
     *           0.1922130E+04, 0.2045030E+04, 0.2199040E+04,
     *           0.3142140E+04, 0.3175180E+04, 0.3230740E+04,
     *           0.3475050E+04, 0.3614260E+04, 0.3836740E+04,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.3677020E+04, 0.2131620E+04, 0.1344410E+04,
     *           0.1199380E+04/

C GOES-10 FK2'S (DETECTOR AVERAGE)

      DATA (FK2(I,3),I=1,25)/
     *           0.97710E+03,   0.10004E+04,   0.10255E+04,
     *           0.10524E+04,   0.10754E+04,   0.11365E+04,
     *           0.11945E+04,   0.13076E+04,   0.14804E+04,
     *           0.19291E+04,   0.20490E+04,   0.22087E+04,
     *           0.31474E+04,   0.31818E+04,   0.32345E+04,
     *           0.34800E+04,   0.36131E+04,   0.38311E+04,
     *           0.22135E+05,   0.00000E+00,   0.22135E+05,
     *           0.36745E+04,   0.21381E+04,   0.13470E+04,
     *           0.11953E+04/

C GOES-11 FK2'S (DETECTOR AVERAGE)

      DATA (FK2(I,4),I=1,25)/
     *           000980.16,     000999.49,     001024.80,
     *           001053.87,     001074.17,     001135.77,
     *           001197.48,     001307.54,     001482.40,
     *           001926.92,     002051.50,     002211.24,
     *           003149.98,     003182.06,     003227.84,
     *           003486.66,     003613.79,     003836.18,
     *           000000.00,     000000.00,     000000.00,
     *           003685.45,     002132.65,     001341.46,
     *           001198.13/

C GOES-12 FK2 (DETECTOR AVERAGE)

      DATA (FK2(I,5),I=1,25)/
     *           0.98121E+03,   0.10008E+04,   0.10252E+04,
     *           0.10556E+04,   0.10783E+04,   0.11361E+04,
     *           0.11998E+04,   0.13124E+04,   0.14803E+04,
     *           0.19340E+04,   0.20516E+04,   0.22130E+04,
     *           0.31494E+04,   0.31769E+04,   0.32347E+04,
     *           0.34888E+04,   0.36189E+04,   0.38340E+04,
     *           0.00000E+00,   0.00000E+00,   0.00000E+00,
     *           0.36902E+04,   0.22191E+04,   0.13430E+04,
     *           0.10807E+04/

C GOES-8 FK2'S (DETECTOR 1)

      DATA (FK2(I,NSAT+1),I=1,25)/
     *           0.9794000E+03, 0.1001010E+04, 0.1024240E+04,
     *           0.1053910E+04, 0.1075530E+04, 0.1137220E+04,
     *           0.1192850E+04, 0.1304330E+04, 0.1481300E+04,
     *           0.1927330E+04, 0.2045960E+04, 0.2209150E+04,
     *           0.3143640E+04, 0.3176100E+04, 0.3233120E+04,
     *           0.3484360E+04, 0.3610650E+04, 0.3836390E+04,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.3678679E+04, 0.2132221E+04, 0.1344302E+04,
     *           0.1204390E+04/

C GOES-9 FK2'S (DETECTOR 1)

      DATA (FK2(I,NSAT+2),I=1,25)/
     *           0.9801200E+03, 0.9985200E+03, 0.1023050E+04,
     *           0.1053740E+04, 0.1074620E+04, 0.1135870E+04,
     *           0.1191850E+04, 0.1308490E+04, 0.1480080E+04,
     *           0.1922130E+04, 0.2045030E+04, 0.2199040E+04,
     *           0.3142140E+04, 0.3175180E+04, 0.3230740E+04,
     *           0.3475050E+04, 0.3614260E+04, 0.3836740E+04,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.3676477E+04, 0.2132092E+04, 0.1344719E+04,
     *           0.1200015E+04/

C GOES-10 FK2'S (DETECTOR 1)

      DATA (FK2(I,NSAT+3),I=1,25)/
     *           0.97710E+03,   0.10004E+04,   0.10255E+04,
     *           0.10524E+04,   0.10754E+04,   0.11365E+04,
     *           0.11945E+04,   0.13076E+04,   0.14804E+04,
     *           0.19291E+04,   0.20490E+04,   0.22087E+04,
     *           0.31474E+04,   0.31818E+04,   0.32345E+04,
     *           0.34800E+04,   0.36131E+04,   0.38311E+04,
     *           0.22135E+05,   0.00000E+00,   0.22135E+05,
     *           0.3673318E+04, 0.2138424E+04, 0.1346895E+04,
     *           0.1195504E+04/

C GOES-11 FK2'S (DETECTOR 1)

      DATA (FK2(I,NSAT+4),I=1,25)/
     *           000980.16,     000999.49,     001024.80,
     *           001053.87,     001074.17,     001135.77,
     *           001197.48,     001307.54,     001482.40,
     *           001926.92,     002051.50,     002211.24,
     *           003149.98,     003182.06,     003227.84,
     *           003486.66,     003613.79,     003836.18,
     *           000000.00,     000000.00,     000000.00,
     *           003685.45,     002132.65,     001341.46,
     *           001198.13/

C GOES-12 FK2 (DETECTOR 1)

      DATA (FK2(I,NSAT+5),I=1,25)/
     *           0.98121E+03,   0.10008E+04,   0.10252E+04,
     *           0.10556E+04,   0.10783E+04,   0.11361E+04,
     *           0.11998E+04,   0.13124E+04,   0.14803E+04,
     *           0.19340E+04,   0.20516E+04,   0.22130E+04,
     *           0.31494E+04,   0.31769E+04,   0.32347E+04,
     *           0.34888E+04,   0.36189E+04,   0.38340E+04,
     *           0.00000E+00,   0.00000E+00,   0.00000E+00,
     *           0.36902E+04,   0.22191E+04,   0.13430E+04,
     *           0.10808E+04/

C GOES-8 FK2'S (DETECTOR 2)

      DATA (FK2(I,2*NSAT+1),I=1,25)/
     *           0.9794000E+03, 0.1001010E+04, 0.1024240E+04,
     *           0.1053910E+04, 0.1075530E+04, 0.1137220E+04,
     *           0.1192850E+04, 0.1304330E+04, 0.1481300E+04,
     *           0.1927330E+04, 0.2045960E+04, 0.2209150E+04,
     *           0.3143640E+04, 0.3176100E+04, 0.3233120E+04,
     *           0.3484360E+04, 0.3610650E+04, 0.3836390E+04,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.3681427E+04, 0.2132720E+04, 0.1345856E+04,
     *           0.1204303E+04/

C GOES-9 FK2'S (DETECTOR 2)

      DATA (FK2(I,2*NSAT+2),I=1,25)/
     *           0.9801200E+03, 0.9985200E+03, 0.1023050E+04,
     *           0.1053740E+04, 0.1074620E+04, 0.1135870E+04,
     *           0.1191850E+04, 0.1308490E+04, 0.1480080E+04,
     *           0.1922130E+04, 0.2045030E+04, 0.2199040E+04,
     *           0.3142140E+04, 0.3175180E+04, 0.3230740E+04,
     *           0.3475050E+04, 0.3614260E+04, 0.3836740E+04,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.3676477E+04, 0.2131620E+04, 0.1344273E+04,
     *           0.1200116E+04/

C GOES-10 FK2'S (DETECTOR 2)

      DATA (FK2(I,2*NSAT+3),I=1,25)/
     *           0.97710E+03,   0.10004E+04,   0.10255E+04,
     *           0.10524E+04,   0.10754E+04,   0.11365E+04,
     *           0.11945E+04,   0.13076E+04,   0.14804E+04,
     *           0.19291E+04,   0.20490E+04,   0.22087E+04,
     *           0.31474E+04,   0.31818E+04,   0.32345E+04,
     *           0.34800E+04,   0.36131E+04,   0.38311E+04,
     *           0.22135E+05,   0.00000E+00,   0.22135E+05,
     *           0.3673318E+04, 0.21381E+04,   0.1346733E+04,
     *           0.1195522E+04/

C GOES-11 FK2'S (DETECTOR 2)

      DATA (FK2(I,2*NSAT+4),I=1,25)/
     *           000980.16,     000999.49,     001024.80,
     *           001053.87,     001074.17,     001135.77,
     *           001197.48,     001307.54,     001482.40,
     *           001926.92,     002051.50,     002211.24,
     *           003149.98,     003182.06,     003227.84,
     *           003486.66,     003613.79,     003836.18,
     *           000000.00,     000000.00,     000000.00,
     *           003685.45,     002132.65,     001341.46,
     *           001198.13/


C GOES-12 FK2 (DETECTOR 2)

      DATA (FK2(I,2*NSAT+5),I=1,25)/
     *           0.98121E+03,   0.10008E+04,   0.10252E+04,
     *           0.10556E+04,   0.10783E+04,   0.11361E+04,
     *           0.11998E+04,   0.13124E+04,   0.14803E+04,
     *           0.19340E+04,   0.20516E+04,   0.22130E+04,
     *           0.31494E+04,   0.31769E+04,   0.32347E+04,
     *           0.34888E+04,   0.36189E+04,   0.38340E+04,
     *           0.00000E+00,   0.00000E+00,   0.00000E+00,
     *           0.36902E+04,   0.22196E+04,   0.13430E+04,
     *           0.10808E+04/

C GOES-8 TC'S (DETECTOR AVERAGE)

      DATA ((TC(I,J,1),I=1,2),J=1,25)/
     *           0.1230000E-01, 0.9999000E+00, 0.1330000E-01,
     *           0.9999000E+00, 0.1860000E-01, 0.9999000E+00,
     *           0.1500000E-01, 0.9999000E+00, 0.1650000E-01,
     *           0.9999000E+00, 0.4740000E-01, 0.9998000E+00,
     *           0.1318000E+00, 0.9995000E+00, 0.1200000E+00,
     *           0.9996000E+00, 0.4260000E-01, 0.9999000E+00,
     *           0.1505000E+00, 0.9996000E+00, 0.2743000E+00,
     *           0.9993000E+00, 0.1447000E+00, 0.9997000E+00,
     *           0.2240000E-01, 0.1000000E+01, 0.2200000E-01,
     *           0.1000000E+01, 0.2170000E-01, 0.1000000E+01,
     *           0.5790000E-01, 0.9999000E+00, 0.6230000E-01,
     *           0.9999000E+00, 0.3675000E+00, 0.9995000E+00,
     *           0.0000000E+01, 0.0000000E+00, 0.0000000E+00,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.6357000E+00, 0.9991000E+00, 0.6060000E+00,
     *           0.9986000E+00, 0.3735000E+00, 0.9987000E+00,
     *           0.2217000E+00, 0.9992000E+00/

C GOES-9 TC'S (DETECTOR AVERAGE)

      DATA ((TC(I,J,2),I=1,2),J=1,25)/
     *           0.9900000E-02, 0.1000000E+01, 0.1190000E-01,
     *           0.9999000E+00, 0.1220000E-01, 0.9999000E+00,
     *           0.1190000E-01, 0.9999000E+00, 0.1350000E-01,
     *           0.9999000E+00, 0.4400000E-01, 0.9998000E+00,
     *           0.1345000E+00, 0.9995000E+00, 0.1193000E+00,
     *           0.9996000E+00, 0.4070000E-01, 0.9999000E+00,
     *           0.1438000E+00, 0.9996000E+00, 0.2762000E+00,
     *           0.9993000E+00, 0.1370000E+00, 0.9997000E+00,
     *           0.1890000E-01, 0.1000000E+01, 0.1980000E-01,
     *           0.1000000E+01, 0.1910000E-01, 0.1000000E+01,
     *           0.5310000E-01, 0.9999000E+00, 0.6120000E-01,
     *           0.9999000E+00, 0.3042000E+00, 0.9996000E+00,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.5864000E+00, 0.9992000E+00, 0.4841000E+00,
     *           0.9989000E+00, 0.3622000E+00, 0.9988000E+00,
     *           0.2014000E+00, 0.9992000E+00/

C GOES-10 TC'S (DETECTOR AVERAGE)

      DATA ((TC(I,J,3),I=1,2),J=1,25)/
     *           0.00988,       0.99995,       0.01196,
     *           0.99994,       0.01245,       0.99994,
     *           0.01245,       0.99995,       0.01366,
     *           0.99994,       0.04311,       0.99983,
     *           0.13973,       0.99947,       0.11707,
     *           0.99959,       0.03979,       0.99988,
     *           0.14968,       0.99962,       0.27603,
     *           0.99933,       0.13049,       0.99970,
     *           0.02008,       0.99997,       0.01834,
     *           0.99997,       0.02017,       0.99997,
     *           0.05292,       0.99992,       0.05330,
     *           0.99992,       0.28683,       0.99961,
     *           0.00000,       1.00000,       0.00000,
     *           0.00000,       0.00000,       1.00000,
     *           0.62226,       0.99912,       0.61438,
     *           0.99857,       0.27791,       0.99905,
     *           0.21145,       0.99919/

C GOES-11 TC'S (DETECTOR AVERAGE)

      DATA((TC(I,J,4),I=1,2),J=1,25)/
     *           0.0096000E+00, 1.0000000E+00, 0.0121000E+00,
     *           0.9999000E+00, 0.0123000E+00, 0.9999000E+00,
     *           0.0119000E+00, 0.9999000E+00, 0.0132000E+00,
     *           0.9999000E+00, 0.0449000E+00, 0.9998000E+00,
     *           0.1299000E+00, 0.9995000E+00, 0.1180000E+00,
     *           0.9996000E+00, 0.0386000E+00, 0.9999000E+00,
     *           0.1509000E+00, 0.9996000E+00, 0.2738000E+00,
     *           0.9993000E+00, 0.1345000E+00, 0.9997000E+00,
     *           0.0190000E+00, 1.0000000E+00, 0.0194000E+00,
     *           1.0000000E+00, 0.0201000E+00, 1.0000000E+00,
     *           0.0488000E+00, 0.9999000E+00, 0.0542000E+00,
     *           0.9999000E+00, 0.2916000E+00, 0.9996000E+00,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.6286000E+00, 0.9991000E+00, 0.5934000E+00,
     *           0.9986000E+00, 0.3828000E+00, 0.9987000E+00,
     *           0.2026000E+00, 0.9992000E+00/

C GOES-12 TC (DETECTOR AVERAGE)

      DATA((TC(I,J,5),I=1,2),J=1,25)/
     x		.01010,		.99995,
     x		.01252,		.99994,
     x		.01229,		.99994,
     x		.01189,		.99995,
     x		.01264,		.99995,
     x		.04189,		.99983,
     x		.13474,		.99949,
     x		.12341,		.99957,
     x		.03844,		.99988,
     x		.15764,		.99960,
     x		.27420,		.99934,
     x		.13683,		.99969,
     x		.02124,		.99996,
     x		.01780,		.99997,
     x		.02037,		.99997,
     x		.04933,		.99993,
     x		.05386,		.99992,
     x		.28872,		.99961,
     x		0.,		0.,
     x		0.,		0.,
     x		0.,		0.,
     x		 .69703,	.99902,
     x		5.08315,	.98872,
     x		 .37554,	.99872,
     x		 .09537,	.99960/


C GOES-8 TC'S (DETECTOR 1)

      DATA ((TC(I,J,NSAT+1),I=1,2),J=1,25)/
     *           0.1230000E-01, 0.9999000E+00, 0.1330000E-01,
     *           0.9999000E+00, 0.1860000E-01, 0.9999000E+00,
     *           0.1500000E-01, 0.9999000E+00, 0.1650000E-01,
     *           0.9999000E+00, 0.4740000E-01, 0.9998000E+00,
     *           0.1318000E+00, 0.9995000E+00, 0.1200000E+00,
     *           0.9996000E+00, 0.4260000E-01, 0.9999000E+00,
     *           0.1505000E+00, 0.9996000E+00, 0.2743000E+00,
     *           0.9993000E+00, 0.1447000E+00, 0.9997000E+00,
     *           0.2240000E-01, 0.1000000E+01, 0.2200000E-01,
     *           0.1000000E+01, 0.2170000E-01, 0.1000000E+01,
     *           0.5790000E-01, 0.9999000E+00, 0.6230000E-01,
     *           0.9999000E+00, 0.3675000E+00, 0.9995000E+00,
     *           0.0000000E+01, 0.0000000E+00, 0.0000000E+00,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.5776530E+00, 0.9984900E+00, 0.5930620E+00,
     *           0.9985840E+00, 0.3221760E+00, 0.9987310E+00,
     *           0.4220770E+00, 0.9988310E+00/

C GOES-9 TC'S (DETECTOR 1)

      DATA ((TC(I,J,NSAT+2),I=1,2),J=1,25)/
     *           0.9900000E-02, 0.1000000E+01, 0.1190000E-01,
     *           0.9999000E+00, 0.1220000E-01, 0.9999000E+00,
     *           0.1190000E-01, 0.9999000E+00, 0.1350000E-01,
     *           0.9999000E+00, 0.4400000E-01, 0.9998000E+00,
     *           0.1345000E+00, 0.9995000E+00, 0.1193000E+00,
     *           0.9996000E+00, 0.4070000E-01, 0.9999000E+00,
     *           0.1438000E+00, 0.9996000E+00, 0.2762000E+00,
     *           0.9993000E+00, 0.1370000E+00, 0.9997000E+00,
     *           0.1890000E-01, 0.1000000E+01, 0.1980000E-01,
     *           0.1000000E+01, 0.1910000E-01, 0.1000000E+01,
     *           0.5310000E-01, 0.9999000E+00, 0.6120000E-01,
     *           0.9999000E+00, 0.3042000E+00, 0.9996000E+00,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.5793620E+00, 0.9990590E+00, 0.4924860E+00,
     *           0.9989250E+00, 0.3843010E+00, 0.9987090E+00,
     *           0.3027100E+00, 0.9990600E+00/

C GOES-10 TC'S (DETECTOR 1)

      DATA ((TC(I,J,NSAT+3),I=1,2),J=1,25)/
     *           0.00988,       0.99995,       0.01196,
     *           0.99994,       0.01245,       0.99994,
     *           0.01245,       0.99995,       0.01366,
     *           0.99994,       0.04311,       0.99983,
     *           0.13973,       0.99947,       0.11707,
     *           0.99959,       0.03979,       0.99988,
     *           0.14968,       0.99962,       0.27603,
     *           0.99933,       0.13049,       0.99970,
     *           0.02008,       0.99997,       0.01834,
     *           0.99997,       0.02017,       0.99997,
     *           0.05292,       0.99992,       0.05330,
     *           0.99992,       0.28683,       0.99961,
     *           0.00000,       1.00000,       0.00000,
     *           0.00000,       0.00000,       1.00000,
     *           0.605178,      0.99890,       0.615675,
     *           0.998601,      0.271027,      0.999034,
     *           0.264813,      0.999092/

C GOES-11 TC'S (DETECTOR 1)

      DATA((TC(I,J,NSAT+4),I=1,2),J=1,25)/
     *           0.0096000E+00, 1.0000000E+00, 0.0121000E+00,
     *           0.9999000E+00, 0.0123000E+00, 0.9999000E+00,
     *           0.0119000E+00, 0.9999000E+00, 0.0132000E+00,
     *           0.9999000E+00, 0.0449000E+00, 0.9998000E+00,
     *           0.1299000E+00, 0.9995000E+00, 0.1180000E+00,
     *           0.9996000E+00, 0.0386000E+00, 0.9999000E+00,
     *           0.1509000E+00, 0.9996000E+00, 0.2738000E+00,
     *           0.9993000E+00, 0.1345000E+00, 0.9997000E+00,
     *           0.0190000E+00, 1.0000000E+00, 0.0194000E+00,
     *           1.0000000E+00, 0.0201000E+00, 1.0000000E+00,
     *           0.0488000E+00, 0.9999000E+00, 0.0542000E+00,
     *           0.9999000E+00, 0.2916000E+00, 0.9996000E+00,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.6286000E+00, 0.9991000E+00, 0.5934000E+00,
     *           0.9986000E+00, 0.3828000E+00, 0.9987000E+00,
     *           0.2026000E+00, 0.9992000E+00/


C GOES-12 TC (DETECTOR 1)

      DATA((TC(I,J,NSAT+5),I=1,2),J=1,25)/
     x          .01010,         .99995,
     x          .01252,         .99994,
     x          .01229,         .99994,
     x          .01189,         .99995,
     x          .01264,         .99995,
     x          .04189,         .99983,
     x          .13474,         .99949,
     x          .12341,         .99957,
     x          .03844,         .99988,
     x          .15764,         .99960,
     x          .27420,         .99934,
     x          .13683,         .99969,
     x          .02124,         .99996,
     x          .01780,         .99997,
     x          .02037,         .99997,
     x          .04933,         .99993,
     x          .05386,         .99992,
     x          .28872,         .99961,
     x		0.,		0.,
     x		0.,		0.,
     x		0.,		0.,
     x		 .69707,	.99902,
     x		5.08462,	.98871,
     x		 .37566,	.99872,
     x		 .08876,	.99962/


C GOES-8 TC'S (DETECTOR 2)

      DATA ((TC(I,J,2*NSAT+1),I=1,2),J=1,25)/
     *           0.1230000E-01, 0.9999000E+00, 0.1330000E-01,
     *           0.9999000E+00, 0.1860000E-01, 0.9999000E+00,
     *           0.1500000E-01, 0.9999000E+00, 0.1650000E-01,
     *           0.9999000E+00, 0.4740000E-01, 0.9998000E+00,
     *           0.1318000E+00, 0.9995000E+00, 0.1200000E+00,
     *           0.9996000E+00, 0.4260000E-01, 0.9999000E+00,
     *           0.1505000E+00, 0.9996000E+00, 0.2743000E+00,
     *           0.9993000E+00, 0.1447000E+00, 0.9997000E+00,
     *           0.2240000E-01, 0.1000000E+01, 0.2200000E-01,
     *           0.1000000E+01, 0.2170000E-01, 0.1000000E+01,
     *           0.5790000E-01, 0.9999000E+00, 0.6230000E-01,
     *           0.9999000E+00, 0.3675000E+00, 0.9995000E+00,
     *           0.0000000E+01, 0.0000000E+00, 0.0000000E+00,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.5809630E+00, 0.9984700E+00, 0.6060000E+00,
     *           0.9986000E+00, 0.3514350E+00, 0.9987090E+00,
     *           0.4663680E+00, 0.9987450E+00/

C GOES-9 TC'S (DETECTOR 2)

      DATA ((TC(I,J,2*NSAT+2),I=1,2),J=1,25)/
     *           0.9900000E-02, 0.1000000E+01, 0.1190000E-01,
     *           0.9999000E+00, 0.1220000E-01, 0.9999000E+00,
     *           0.1190000E-01, 0.9999000E+00, 0.1350000E-01,
     *           0.9999000E+00, 0.4400000E-01, 0.9998000E+00,
     *           0.1345000E+00, 0.9995000E+00, 0.1193000E+00,
     *           0.9996000E+00, 0.4070000E-01, 0.9999000E+00,
     *           0.1438000E+00, 0.9996000E+00, 0.2762000E+00,
     *           0.9993000E+00, 0.1370000E+00, 0.9997000E+00,
     *           0.1890000E-01, 0.1000000E+01, 0.1980000E-01,
     *           0.1000000E+01, 0.1910000E-01, 0.1000000E+01,
     *           0.5310000E-01, 0.9999000E+00, 0.6120000E-01,
     *           0.9999000E+00, 0.3042000E+00, 0.9996000E+00,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.5793620E+00, 0.9990590E+00, 0.4841000E+00,
     *           0.9989000E+00, 0.3632410E+00, 0.9987300E+00,
     *           0.3065470E+00, 0.9990530E+00/

C GOES-10 TC'S (DETECTOR 2)

      DATA ((TC(I,J,2*NSAT+3),I=1,2),J=1,25)/
     *           0.00988,       0.99995,       0.01196,
     *           0.99994,       0.01245,       0.99994,
     *           0.01245,       0.99995,       0.01366,
     *           0.99994,       0.04311,       0.99983,
     *           0.13973,       0.99947,       0.11707,
     *           0.99959,       0.03979,       0.99988,
     *           0.14968,       0.99962,       0.27603,
     *           0.99933,       0.13049,       0.99970,
     *           0.02008,       0.99997,       0.01834,
     *           0.99997,       0.02017,       0.99997,
     *           0.05292,       0.99992,       0.05330,
     *           0.99992,       0.28683,       0.99961,
     *           0.00000,       1.00000,       0.00000,
     *           0.00000,       0.00000,       1.00000,
     *           0.605178,      0.99890,       0.61438,
     *           0.99857,       0.270378,      0.999032,
     *           0.260331,      0.999105/

C GOES-11 TC'S (DETECTOR 2)

      DATA((TC(I,J,2*NSAT+4),I=1,2),J=1,25)/
     *           0.0096000E+00, 1.0000000E+00, 0.0121000E+00,
     *           0.9999000E+00, 0.0123000E+00, 0.9999000E+00,
     *           0.0119000E+00, 0.9999000E+00, 0.0132000E+00,
     *           0.9999000E+00, 0.0449000E+00, 0.9998000E+00,
     *           0.1299000E+00, 0.9995000E+00, 0.1180000E+00,
     *           0.9996000E+00, 0.0386000E+00, 0.9999000E+00,
     *           0.1509000E+00, 0.9996000E+00, 0.2738000E+00,
     *           0.9993000E+00, 0.1345000E+00, 0.9997000E+00,
     *           0.0190000E+00, 1.0000000E+00, 0.0194000E+00,
     *           1.0000000E+00, 0.0201000E+00, 1.0000000E+00,
     *           0.0488000E+00, 0.9999000E+00, 0.0542000E+00,
     *           0.9999000E+00, 0.2916000E+00, 0.9996000E+00,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.0000000E+00, 0.0000000E+00, 0.0000000E+00,
     *           0.6286000E+00, 0.9991000E+00, 0.5934000E+00,
     *           0.9986000E+00, 0.3828000E+00, 0.9987000E+00,
     *           0.2026000E+00, 0.9992000E+00/

C GOES-12 TC (DETECTOR 2)

      DATA((TC(I,J,2*NSAT+5),I=1,2),J=1,25)/
     x          .01010,         .99995,
     x          .01252,         .99994,
     x          .01229,         .99994,
     x          .01189,         .99995,
     x          .01264,         .99995,
     x          .04189,         .99983,
     x          .13474,         .99949,
     x          .12341,         .99957,
     x          .03844,         .99988,
     x          .15764,         .99960,
     x          .27420,         .99934,
     x          .13683,         .99969,
     x          .02124,         .99996,
     x          .01780,         .99997,
     x          .02037,         .99997,
     x          .04933,         .99993,
     x          .05386,         .99992,
     x          .28872,         .99961,
     x		0.,		0.,
     x		0.,		0.,
     x		0.,		0.,
     x		 .69707,	.99902,
     x		5.08801,	.98870,
     x		 .37566,	.99872,
     x		 .08876,	.99962/


      KKCH=KCH

      IF(ISAT.LT.72) THEN
         SATIDX=1               ! goes-8
      ELSE IF(ISAT.LT.74) THEN
         SATIDX=2               ! goes-9
      ELSE IF(ISAT.LT.76) THEN
         SATIDX=3               ! goes-10
      ELSE IF(ISAT.LT.78) THEN
         SATIDX=4               ! goes-11
      ELSE IF(ISAT.LT.80) THEN
         SATIDX=5               ! goes-12
      ELSE
         SATIDX=6               ! goes-13
      ENDIF

      IF(SATIDX.GT.5) SATIDX=5   ! no goes-13 constants yet

C     CHANGE SATIDX TO ACCOMMODATE THE DETECTOR ID (IF SUPPLIED)
C       (IF DETECTOR IS MISIDENTIFIED OR NOT SUPPLIED,
C                                    WE USE THE AVERAGED CONSTANTS)
      IF(KDET.LT.0.OR.KDET.GT.2) KDET=0
      SATIDX=SATIDX+(NSAT*KDET)
C     SAVE SATIDX, SO THERE IS SOME RECORD OF WHICH CONSTANTS WERE USED
C       (THE CONSTANTS ARE SO SIMILAR, THE DIFFERENCES ARE BARELY
C        DETECTABLE IN THE CALIBRATION OUTPUT, EXCEPT FOR GOES-8,
C        WHERE THE DETECTORS WERE NOT AS WELL MATCHED.  ONE WOULD
C        EXPECT 0.1K OR LESS DIFFERENCE FOR GOES-9 & -10, AND ABOUT
C        0.3K OR LESS FOR GOES-8.)


      GVATBB=0.0
      IF(VAL.LE.0.0) RETURN

C   RAD -> TEMP

      IF(SWITCH .EQ. 0) THEN
         EXPN=FK1(KKCH,SATIDX)/VAL+1.
         TT=FK2(KKCH,SATIDX)/ALOG(EXPN)
         GVATBB=(TT-TC(1,KKCH,SATIDX))/TC(2,KKCH,SATIDX)

C    TEMP -> RAD

      ELSE
         TT = TC(1,KKCH,SATIDX) + TC(2,KKCH,SATIDX) * VAL
         EXPN = EXP(FK2(KKCH,SATIDX)/TT) - 1.
         GVATBB = FK1(KKCH,SATIDX)/EXPN
      ENDIF
      RETURN
      END



C ----------------------------------------------------------
C [                   FUNCTION GRYSCL                       [
C ----------------------------------------------------------

      INTEGER FUNCTION GRYSCL (TEMPK)

C $ FUNCTION GRYSCL(TEMPK)
C $ CONVERT A BRIGHTNESS TEMPERATURE TO A GREY SCALE
C $ TEMPK = (R) INPUT  TEMPERATURE IN DEGRESS KELVIN
C $$ GRYSCL = CONVERT
      REAL TEMPK
      REAL TLIM
      INTEGER CON1, CON2
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



C ----------------------------------------------------------
C [                   FUNCTION EXPTAB                       [
C ----------------------------------------------------------

      REAL FUNCTION TMPSCL (IBRIT)

C $ REAL FUNCTION TMPSCL(IBRIT)
C $ CONVERT A GRAY SCALE VALUE TO A BRIGHTNESS TEMPERATURE
C $ IBRIT = (I) INPUT  GRAY SCALE VALUE
C $$ TMPSCL = CONVERT
      REAL CON1, CON2
      DATA CON1 / 418. /
      DATA CON2 / 660. /
      DATA ILIM / 176 /
      IF (IBRIT .GT. ILIM) THEN
         TMPSCL = CON1 - IBRIT
      ELSE
         TMPSCL = (CON2 - IBRIT)/2.
      ENDIF
      RETURN
      END



C ----------------------------------------------------------
C [ FUNCTION TO CONVERT FROM GVAR 4-BYTE REAL VALUES       [
C [ FORMATTED FOR THE GOULD/SEL COMPUTER INTO IBM REAL     [
C [ VALUES.                                                [
C [ ALL FLOATING POINT NUMBERS USED IN GVAR ARE 32 BITS IN [
C [ LENGTH, TRANSPORT VIA FOUR SEQUENTIAL 8-BIT WORDS.     [
C [ THIS FORMAT EMPLOYS A SIGN BIT, A 7-BIT EXPONENT, AND  [
C [ A 24 BIT FRACTIONAL MANTISSA DEFINED AS FOLLOWS:       [
C [                                                        [
C [ WORD     1           2          3          4           [
C [ BITS  1      8   1      8   1      8   1      8        [
C [       --------   --------   --------   --------        [
C [       SEEEEEEE   MMMMMMMM   MMMMMMMM   MMMMMMMM        [
C [                 MSB --- 24 BIT MANTISSA --- LSB        [
C [                                                        [
C [ FOR FURTHER INFORMATION CONSULT PP. 170-171 IN GVAR    [
C [ TRANSMISSION MANUAL 'OPERATIONS GROUND EQUIPMENT INTER-[
C [ FACE SPECIFICATION' DRL 504-02-1                       [
C [                                                        [
C [ RECEIVES: INTEGER*4 VALUE - 4-BYTE INTEGER VALUE HOLD- [
C [                             ING VALUE TO BE CONVERTED  [
C [ RETURNS:  REAL FRMT - FLOATING POINT VALUE             [
C ----------------------------------------------------------

       REAL FUNCTION FRMT(VALUE)

       INTEGER CONVRT, VALUE, TEMP
       INTEGER IEOR
       REAL             MEH

       EQUIVALENCE (MEH,TEMP)
       DATA CONVRT / Z'7FFFFFFF' /
       TEMP = VALUE
       IF (TEMP .LT. 0) TEMP = IEOR(TEMP, CONVRT) + 1

       CALL SWBYT4(MEH, 1)
       CALL FLTCON(MEH, 1)

       FRMT = MEH

       RETURN
       END



C ----------------------------------------------------------
C [ COMPARES THE FACTORY VISIBLE COEFFICIENTS TO THOSE OF  [
C [ THE CODICIL SENT WITH THIS IMAGE.  IF THEY ARE DIFFERNT[
C [ THEN A WARNING MESSAGE IS DISPLAYED AND THE NEW COEFF  [
C [ ARE RETURNED IN THE ARRAY VISARR.                      [
C [                                                        [
C [ RECEIVES: INTEGER ISAT - SATELLITE NUMBER              [
C [           INTEGER CODARR - CODICIL ARRAY               [
C [ RETURNS:  REAL VISARR - ARRAY OF VISIBLE CALIBRATION   [
C ----------------------------------------------------------

       SUBROUTINE CHKVIS(ISAT, CODARR, VISARR, HVCOD)

       IMPLICIT INTEGER(A-Z)
       INTEGER ISAT, CODARR(128), NEWPOS, CODPOS, IFLAG
       INTEGER HVCOD
       REAL VISARR(128), FRMT, MXDF, PCTDIF
       REAL IVBIAS(8), IVGAIN(8), IV2GAIN(8), IVALB
       REAL SVBIAS(4), SVGAIN(4), SV2GAIN(4), SVALB


C ---  THE FOLLOWING DATA STATEMENTS ARE PREDETERMINED SCALING
C ---  FACTORS USED TO UNSCALE THE SCALED RADIANCE SENT IN
C ---  THE GVAR TRANSMISSION.

       DATA IVBIAS/-0.154116E+02, -0.153044E+02, -0.153890E+02,
     *             -0.152684E+02, -0.153111E+02, -0.152730E+02,
     *             -0.153534E+02, -0.153300E+02/
       DATA IVGAIN/ 0.552808E+00,  0.550187E+00,  0.553975E+00,
     *              0.550833E+00,  0.550946E+00,  0.552190E+00,
     *              0.550459E+00,  0.550728E+00/
       DATA IV2GAIN/0.000000E+00,  0.000000E+00,  0.000000E+00,
     *              0.000000E+00,  0.000000E+00,  0.000000E+00,
     *              0.000000E+00,  0.000000E+00/
       DATA IVALB/  0.192979E-02/
       DATA SVBIAS/-0.603300E+02, -0.601600E+02, -0.617700E+02,
     *             -0.615000E+02/
       DATA SVGAIN/ 0.648253E-01,  0.652221E-01,  0.656024E-01,
     *              0.664202E-01/
       DATA SV2GAIN/0.000000E+00,  0.000000E+00,  0.000000E+00,
     *              0.000000E+00/

       DATA SVALB/  0.220062E-02/
       DATA MXDF/   1.000000E-03/

       IFLAG = 0
       NEWPOS = 1
       CODPOS = 1

C  CHECK THE IMAGER VIS COEFFICIENTS
       IF(MOD(ISAT,2) .EQ. 0) THEN
C          CHECK IMAGER BIAS TERMS (1 PER DETECTOR)
           DO 10 ITEMP=1,8
C            --------------------------------------------------
C            GET THE PERCENT DIFFERENCE BETWEEN THE FACTORY
C            VALUE AND THAT OF THE CODICIL VALUE.  IF IT IS
C            GREATER THAN MXDF, THEN RETURN THE NEW COEFFICIENT
C            AND SET THE WARNING MESSAGE FLAG TO 1
C            --------------------------------------------------
             IF(PCTDIF(IVBIAS(ITEMP), FRMT(CODARR(CODPOS)))
     *            .GT. MXDF .AND. HVCOD .EQ. 1) THEN
               VISARR(NEWPOS) = FRMT(CODARR(CODPOS))
               IFLAG = 1
             ELSE
               VISARR(NEWPOS) = IVBIAS(ITEMP)
             ENDIF
             NEWPOS = NEWPOS + 1
             CODPOS = CODPOS + 1
10         CONTINUE

C-----     CHECK IMAGER FIRST ORDER GAIN TERMS (1 PER DETECTOR)
           DO 20 ITEMP=1,8
             IF(PCTDIF(IVGAIN(ITEMP), FRMT(CODARR(CODPOS)))
     *            .GT. MXDF .AND. HVCOD .EQ. 1) THEN
               VISARR(NEWPOS) = FRMT(CODARR(CODPOS))
               IFLAG = 1
             ELSE
               VISARR(NEWPOS) = IVGAIN(ITEMP)
             ENDIF
             NEWPOS = NEWPOS + 1
             CODPOS = CODPOS + 1
C            CALL DMPARR(VISARR, 1, 25)
20         CONTINUE

C-----     CHECK IMAGER SECOND ORDER GAIN TERMS (1 PER DETECTOR)
           DO 30 ITEMP=1,8
             IF(PCTDIF(IV2GAIN(ITEMP), FRMT(CODARR(CODPOS)))
     *       .GT. MXDF .AND. HVCOD .EQ. 1) THEN
               VISARR(NEWPOS) = FRMT(CODARR(CODPOS))
               IFLAG = 1
             ELSE
               VISARR(NEWPOS) = IV2GAIN(ITEMP)
             ENDIF
             NEWPOS = NEWPOS + 1
             CODPOS = CODPOS + 1
30         CONTINUE

C-----     CHECK IMAGER ALBEDO FACTOR TERM (1 FOR ALL DETECTORS)
           IF(PCTDIF(IVALB, FRMT(CODARR(CODPOS))) .GT. MXDF
     *     .AND. HVCOD .EQ. 1) THEN
             VISARR(NEWPOS) = FRMT(CODARR(CODPOS))
             IFLAG = 1
           ELSE
             VISARR(NEWPOS) = IVALB
           ENDIF
       ENDIF

C
C ---CHECK SOUNDER VIS--------------------
C

       IF(MOD(ISAT,2) .EQ. 1) THEN
C---     CHECK SOUNDER BIAS TERMS (1 PER DETECTOR)
         DO 40 ITEMP=1,4
           IF(PCTDIF(SVBIAS(ITEMP), FRMT(CODARR(CODPOS)))
     *             .GT. MXDF .AND. HVCOD .EQ. 1) THEN
             VISARR(NEWPOS) = FRMT(CODARR(CODPOS))
             IFLAG = 1
           ELSE
             VISARR(NEWPOS) = SVBIAS(ITEMP)
           ENDIF
           NEWPOS = NEWPOS + 1
           CODPOS = CODPOS + 1
40       CONTINUE

C-----   CHECK SOUNDER FIRST ORDER GAIN TERMS (1 PER DETECTOR)
         DO 50 ITEMP=1,4
           IF(PCTDIF(SVGAIN(ITEMP), FRMT(CODARR(CODPOS)))
     *             .GT. MXDF .AND. HVCOD .EQ. 1) THEN
             VISARR(NEWPOS) = FRMT(CODARR(CODPOS))
             IFLAG = 1
           ELSE
             VISARR(NEWPOS) = SVGAIN(ITEMP)
           ENDIF
           NEWPOS = NEWPOS + 1
           CODPOS = CODPOS + 1
50       CONTINUE

C-----   CHECK SOUNDER SECOND ORDER GAIN TERMS (1 PER DETECTOR)
         DO 60 ITEMP=1,4
           IF(PCTDIF(SV2GAIN(ITEMP), FRMT(CODARR(CODPOS)))
     *             .GT. MXDF .AND. HVCOD .EQ. 1) THEN
             VISARR(NEWPOS) = FRMT(CODARR(CODPOS))
             IFLAG = 1
           ELSE
             VISARR(NEWPOS) = SV2GAIN(ITEMP)
           ENDIF
           NEWPOS = NEWPOS + 1
           CODPOS = CODPOS + 1
60       CONTINUE

C-----   CHECK SOUNDER ALBEDO TERM (1 FOR ALL DETECTORS)
         IF(PCTDIF(SVALB, FRMT(CODARR(CODPOS))) .GT. MXDF
     *   .AND. HVCOD .EQ. 1) THEN
           VISARR(NEWPOS) = FRMT(CODARR(CODPOS))
           IFLAG = IFLAG + 1
         ELSE
           VISARR(NEWPOS) = SVALB
         ENDIF
       ENDIF

C      CHECK IF FLAG HAS BEEN SET
C      IF(IFLAG .GT. 0 .AND. HVCOD .EQ. 1) THEN
C        CALL DDEST('VISIBLE CALIBRATION COEF CHANGED: USING NEW', 0)
C      ENDIF
C      IF(HVCOD .EQ. 0) THEN
C        CALL DDEST('NO CALIBRATION CODICIL: USING DEFAULT', 0)
C      ENDIF
       RETURN
       END



C ----------------------------------------------------------
C [ CHECKS THE IR COEFFICIENTS IN THE CODICIL WITH THOSE   [
C [ SET AT THE FACTORY TO SEE IF THEY HAVE CHANGED.  IF ANY[
C [ HAVE CHANGED, A DDEST MESSAGE IS DISPLAYED SAYING SO.  [
C [ IF COEFF HAVE CHANGED THEN NEW COEFF ARE RETURNED FOR  [
C [ USE.  ELSE FACTORY VALUES ARE RETURNED                 [
C [ RECEIVES: ISAT - SATELLITE NUMBER                      [
C [           IBAND - BAND NUMBER                          [
C [           CODARR - CODICIL ARRAY                       [
C [ RETURNS:  GAIN - GAIN SCALING FACTOR FOR THIS BAND     [
C [           BIAS - BIAS SCALING FACTOR FOR THIS BAND     [
C ----------------------------------------------------------

       SUBROUTINE CHKIR(ISAT, IBAND, CODARR, GAIN, BIAS, HVCOD)
       INTEGER ISAT, IBAND, CODARR(128), IFLAG, HVCOD
       INTEGER SBIASOFF, SGAINOFF
       REAL GAIN, BIAS, MXDF, FRMT
       REAL IIRGAIN(4), IIRBIAS(4), SIRGAIN(18), SIRBIAS(18)
       integer loc2,loc3,loc4,loc5

C       CHARACTER*12 CFG

C  OFFSETS TO THE BIAS AND GAIN VALUES IN IMAGER CODICIL
       DATA IBIASOFF /25/, IGAINOFF /33/

C  OFFSETS TO THE BIAS AND GAIN VALUES IN SOUNDER CODICIL
       DATA SBIASOFF /13/, SGAINOFF /31/

C  FACTORY GAIN AND BIAS SCALING FACTORS FOR THE IMAGER
       DATA IIRGAIN/  0.522850E+01,  0.502714E+01,  0.227389E+03,
     *                0.388383E+02/
       DATA IIRBIAS/  0.156854E+02,  0.153332E+02,  0.682167E+02,
     *                0.291287E+02/

C  FACTORY GAIN AND BIAS SCALING FACTORS FOR THE SOUNDER
       DATA SIRGAIN/  0.528977E+03,  0.540005E+03,  0.485624E+03,
     *                0.394575E+03,  0.357802E+03,  0.334175E+03,
     *                0.311523E+03,  0.314603E+03,  0.434352E+03,
     *                0.112622E+04,  0.189956E+04,  0.287434E+04,
     *                0.964275E+04,  0.141054E+05,  0.262213E+05,
     *                0.107206E+05,  0.121361E+05,  0.193581E+05/
       DATA SIRBIAS/  0.174563E+04,  0.156601E+04,  0.131119E+04,
     *                0.887794E+03,  0.787164E+03,  0.417719E+03,
     *                0.249218E+03,  0.251683E+03,  0.716680E+03,
     *                0.900979E+03,  0.113974E+04,  0.215576E+04,
     *                0.626779E+03,  0.916850E+03,  0.170439E+04,
     *                0.428824E+03,  0.497581E+03,  0.348446E+03/

C      MAXIMUM PERCENT DIFFERENCE THRESHOLD
       DATA MXDF /1.000000E-03/

       IFLAG = 0

C ----- CHECK IMAGER IR COEFFICIENTS -------------
       IF(MOD(ISAT,2) .EQ. 0) THEN

       if(isat.lt.78) then
          loc2=3
          loc3=4
          loc4=1
          loc5=2
       else
          loc2=1
          loc3=2
          loc4=3
          loc5=4
       endif

C-----   CHECK CODICIL GAIN AND BIAS AGAINST FACTORY FOR BAND 2
         IF(IBAND .EQ. 2) THEN
           BIAS = IIRBIAS(3)
           GAIN = IIRGAIN(3)
           IF(PCTDIF(BIAS, FRMT(CODARR(IBIASOFF + loc2))) .GT. MXDF
     *       .AND. HVCOD .EQ. 1) THEN
             IFLAG = 1
             BIAS = FRMT(CODARR(IBIASOFF + loc2))
           ENDIF
           IF(PCTDIF(GAIN, FRMT(CODARR(IGAINOFF + loc2))) .GT. MXDF
     *       .AND. HVCOD .EQ. 1) THEN
             IFLAG = 1
             GAIN = FRMT(CODARR(IGAINOFF + loc2))
           ENDIF
         ENDIF

C-----   CHECK CODICIL GAIN AND BIAS AGAINST FACTORY FOR BAND 3
         IF(IBAND .EQ. 3) THEN
           BIAS = IIRBIAS(4)
           GAIN = IIRGAIN(4)
           IF(PCTDIF(BIAS, FRMT(CODARR(IBIASOFF + loc3))) .GT. MXDF
     *       .AND. HVCOD .EQ. 1) THEN
             IFLAG = 1
             BIAS = FRMT(CODARR(IBIASOFF + loc3))
           ENDIF
           IF(PCTDIF(GAIN, FRMT(CODARR(IGAINOFF + loc3))) .GT. MXDF
     *       .AND. HVCOD .EQ. 1) THEN
             IFLAG = 1
             GAIN = FRMT(CODARR(IGAINOFF + loc3))
           ENDIF
         ENDIF

C-----   CHECK CODICIL GAIN AND BIAS AGAINST FACTORY FOR BAND 4
         IF(IBAND .EQ. 4) THEN
           BIAS = IIRBIAS(1)
           GAIN = IIRGAIN(1)
           IF(PCTDIF(BIAS, FRMT(CODARR(IBIASOFF + loc4))) .GT. MXDF
     *       .AND. HVCOD .EQ. 1) THEN
             IFLAG = 1
             BIAS = FRMT(CODARR(IBIASOFF + loc4))
           ENDIF
           IF(PCTDIF(GAIN, FRMT(CODARR(IGAINOFF + loc4))) .GT. MXDF
     *       .AND. HVCOD .EQ. 1) THEN
             IFLAG = 1
             GAIN = FRMT(CODARR(IGAINOFF + loc4))
           ENDIF
C            CALL DDEST('BIAS = '//CFG(BIAS),0)
C            CALL DDEST('GAIN = '//CFG(GAIN),0)
         ENDIF

C-----   CHECK CODICIL GAIN AND BIAS AGAINST FACTORY FOR BAND 5
         IF(IBAND .EQ. 5) THEN
           BIAS = IIRBIAS(2)
           GAIN = IIRGAIN(2)
           IF(PCTDIF(BIAS, FRMT(CODARR(IBIASOFF + loc5))) .GT. MXDF
     *       .AND. HVCOD .EQ. 1) THEN
             IFLAG = 1
             BIAS = FRMT(CODARR(IBIASOFF + loc5))
           ENDIF
           IF(PCTDIF(GAIN, FRMT(CODARR(IGAINOFF + loc5))) .GT. MXDF
     *       .AND. HVCOD .EQ. 1) THEN
             IFLAG = 1
             GAIN = FRMT(CODARR(IGAINOFF + loc5))
           ENDIF
         ENDIF
       ENDIF

C ---- CHECK SOUNDER IR COEFFICIENTS -------------
       IF(MOD(ISAT,2) .EQ. 1) THEN

C ---    CHECK SOUNDER IR GAIN FACTORS
         IF(PCTDIF(SIRGAIN(IBAND), FRMT(CODARR(SGAINOFF + IBAND)))
     *             .GT. MXDF .AND. HVCOD .EQ. 1) THEN
           IFLAG = 1
           GAIN = FRMT(CODARR(SGAINOFF + IBAND))
         ELSE
           GAIN = SIRGAIN(IBAND)
         ENDIF

C ---    CHECK SOUNDER IR BIAS FACTORS
         IF(PCTDIF(SIRBIAS(IBAND), FRMT(CODARR(SBIASOFF + IBAND)))
     *             .GT. MXDF .AND. HVCOD .EQ. 1) THEN
           IFLAG = 1
           BIAS = FRMT(CODARR(SBIASOFF + IBAND))
         ELSE
           BIAS = SIRBIAS(IBAND)
         ENDIF
       ENDIF

C ---  CHECK IF ERROR FLAG HAS BEEN SET

C      IF(IFLAG .GT. 0 .AND. HVCOD .EQ. 1) THEN
C        CALL DDEST('IR CALIBRATION COEF CHANGED: USING NEW', 0)
C      ENDIF
C      IF(HVCOD .EQ. 0) THEN
C        CALL DDEST('NO CALIBRATION CODICIL: USING DEFAULT', 0)
C      ENDIF
       RETURN
       END



C ----------------------------------------------------------
C [ PRINT A 5 COLUMN LISTING OF AN ARRAY IN CFG() FORMAT   [
C [ RECEIVES: REAL ARRAY(*)                                [
C [           INTEGER START:  STARTING NUMBER OF ARRAY     [
C [           INTEGER END: ENDING NUMBER OF ARRAY          [
C ----------------------------------------------------------

       SUBROUTINE DMPARR(ARRAY, START, END)
       REAL ARRAY(*)
       INTEGER START, END
C       INTEGER ITEMP
C       CHARACTER*12 CFG

C      DO 850 ITEMP=START,END,5
C        CALL DDEST(CFG(ARRAY(ITEMP))//CFG(ARRAY(ITEMP+1))//
C    *     CFG(ARRAY(ITEMP+2))//CFG(ARRAY(ITEMP+3))//CFG(ARRAY(ITEMP+4))
C    *     ,0)
C50    CONTINUE
       RETURN
       END

C ----------------------------------------------------------
C [ COMPUTES THE ABSOLUTE PERCENT DIFFERENCE BETWEEN TWO   [
C [ REAL NUMBERS.                                          [
C [                                                        [
C [ RECEIVES: REAL VALUE1 - FIRST NUMBER                   [
C [           REAL VALUE2 - SECOND NUMBER                  [
C [ RETURNS:  REAL PCTDIF - ABSOLUTE PERCENT DIFFERENCE    [
C ----------------------------------------------------------
       REAL FUNCTION PCTDIF(VALUE1, VALUE2)

       REAL VALUE1, VALUE2

       IF(VALUE1 .NE. 0.0) THEN
         PCTDIF = ABS((VALUE1 - VALUE2) / VALUE1)
       ELSE
         IF(VALUE2 .NE. 0.0) THEN
           PCTDIF = ABS((VALUE1 - VALUE2) / VALUE2)
         ELSE
             PCTDIF = 0.
         ENDIF
       ENDIF
       RETURN
       END

C ---------------------------------------------------------
C [ LOOKS AT THE FIRST 20 WORDS OF THE CODICIL TO DETER-  [
C [ MINE IF THE CODICIL IS VALID.                         [
C [                                                       [
C [ RECEIVES: INTEGER IARR - Array of 128 real words      [
C [ RETURNS:  INTEGER CHKCOD - 0 if invalid               [
C [                            1 if valid                 [
C ---------------------------------------------------------
       INTEGER FUNCTION CHKCOD(IARR)

       INTEGER IARR(128), NUMINT, ICOUNT

       NUMINT = 0
       DO 54 ICOUNT=1,20
         IF(IARR(ICOUNT) .LT. 65536 .AND.
     *      IARR(ICOUNT) .GT. 0) THEN
           NUMINT = NUMINT + 1
         ENDIF
54     CONTINUE
       IF(NUMINT .GT. 10) THEN
         CHKCOD = 0
       ELSE
         CHKCOD = 1
       ENDIF
       RETURN
       END



C ----------------------------------------------------------
C [                SUBROUTINE EXPTAB                       [
C ----------------------------------------------------------

        SUBROUTINE EXPTAB(INTAB, ITAB, SWITCH)
C
C Expand INTAB (an array of 255 pointers) to ITAB
C SWITCH is 0 for both increasing, 1 for INTAB increases as ITAB
C        decreases
C
        INTEGER INTAB(*), ITAB(*)
	INTEGER SWITCH

C Visible:  INTAB & ITAB both increasing

	IF(SWITCH .EQ. 0) THEN
	   LBEG = 1
           LEND = 254
           LSTEP = 1
	   OFFSET = -1
	   FSTVAL = 0
	   LSTVAL = 255
	ELSE IF(SWITCH .EQ. 1) THEN

C IR:  INTAB increase opposite of ITAB

	   LBEG = 255
           LEND = 1
           LSTEP = -1
	   OFFSET = 0
	   FSTVAL = 255
	   LSTVAL = 0
	ENDIF

        IND1 = 2
	IND2 = INTAB(LBEG) - 1
C
C Fill in the first part of the table
C
	ITAB(1) = 0
	DO 25 J=IND1, IND2
	   ITAB(J) = FSTVAL
 25     CONTINUE

        IND1 = IND2 + 1
C
C Readjust the beginning brightness value by one step
C since we have already filled in the first part of the table
C
	LBEG = LBEG + LSTEP

C The 100 loop, is the BRIT count (0 -> 255)

        DO 100 IBRIT = LBEG, LEND, LSTEP

           IND2 = INTAB(IBRIT) - 1

C INTAB contains the range of raw counts corresponding to BRIT values

           DO 50 J = IND1, IND2
              ITAB(J) = IBRIT + OFFSET
50      CONTINUE

        IND1 = IND2 + 1

100     CONTINUE

C Fill in the rest of the table; either 255 or 0

        DO 150 J = IND1, 32768
           ITAB(J) = LSTVAL
150     CONTINUE

        RETURN
	END
