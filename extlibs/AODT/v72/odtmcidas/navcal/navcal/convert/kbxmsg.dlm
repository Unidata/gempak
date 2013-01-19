C Copyright(c) 2001, Space Science and Engineering Center, UW-Madison
C Refer to "McIDAS Software Acquisition and Distribution Policies"
C in the file  mcidas/data/license.txt

C *** $Id: kbxmsg.dlm,v 1.10 2004/07/22 17:11:41 russd Rel $ ***

C   Calibration for Meteosat second generation (MSH)

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
      INTEGER CALFLG
      INTEGER CALARR(313)
      COMMON/MSGCOM/ITYPE,JTYPE,JOPT,CALFLG,CALARR

      ! external functions

      ! local variables

C
      CALL MOVW(NUMAREAOPTIONS,IOPT,JOPT)
      ITYPE=0
      CALFLG = 0
      if(CIN.eq.'RAW'.and. COUT.eq.'BRIT') ITYPE=1
      if(CIN.eq.'RAW'.and. COUT.eq.'RAD ') ITYPE=2
      if(CIN.eq.'RAW'.and. COUT.eq.'REFL') ITYPE=3
      if(CIN.eq.'RAW'.and. COUT.eq.'TEMP') ITYPE=4
      if(ITYPE.eq.0) GOTO 900
      KBXINI=0
      RETURN
900   CONTINUE
      KBXINI=-1
      RETURN
      END
C
C--------------------------------------------------------------------

      INTEGER FUNCTION KBXCAL(PFX,IDIR,NVAL,BAND,IBUF)
      IMPLICIT NONE
      INTEGER PFX(*)
      INTEGER IDIR(*)
      INTEGER NVAL
      INTEGER BAND
      INTEGER*2 IBUF(*)

      ! symbolic constants & shared data

      INCLUDE 'areaparm.inc'

      INTEGER ITYPE
      INTEGER JTYPE
      INTEGER JOPT(NUMAREAOPTIONS)
      INTEGER CALFLG
      INTEGER CALARR(313)
      COMMON/MSGCOM/ITYPE,JTYPE,JOPT,CALFLG,CALARR

      ! external functions

      CHARACTER*12 	CFI
      CHARACTER*12	CFE
      CHARACTER*12	CFF

      ! local variables

      CHARACTER*104     COUT
      CHARACTER*1252 	CBUF
      INTEGER 		I
      INTEGER 		IDES
      INTEGER 		ISOU
      INTEGER 		ITEMP
      INTEGER 		THIS
      INTEGER           BANDOFFSET
      INTEGER 		BUF(313)
      INTEGER 		IBRIT
      REAL    		XTEMP
      REAL    		REFL
      DOUBLE PRECISION 	C1W3
      DOUBLE PRECISION	C2W
      DOUBLE PRECISION	ALPHA
      DOUBLE PRECISION	BETA
      DOUBLE PRECISION	GAIN
      DOUBLE PRECISION	OFFSET
      REAL 		FACTOR(12)

      EQUIVALENCE(CBUF,BUF)

      DATA THIS/-9999/

C --- ADDED FOR EUMETSAT COMPATIBILITY
      DATA 	C1W3/0.0D0/
      DATA      C2W/0.0D0/
      DATA      ALPHA/0.0D0/
      DATA      BETA/0.0D0/
      DATA      GAIN/0.0D0/
      DATA      OFFSET/0.0D0/

      DATA FACTOR/ 
     &             21.21,
     &             23.24,
     &             19.77,
     &              0.00,
     &              0.00,
     &              0.00,
     &              0.00, 
     &              0.00,
     &              0.00,
     &              0.00,
     &              0.00,
     &             22.39
     &           /


C --- get radiometric constants from CAL header
      IF( THIS.NE.IDIR(33) ) THEN

         THIS = IDIR(33)
         COUT = ' '

C ------ Check to see if calibration block passed in through kbxopt
         IF(CALFLG .NE. 0) THEN
            CALL MOVW(51, CALARR, BUF)
         ELSE
            CALL ARAGET(IDIR(33),IDIR(63),104,BUF)
         ENDIF

C ------ NEW FORMAT: ALL BANDS IN BLOCK
         IF( CBUF(1:4).EQ.'MSGT' ) THEN

            IF(CALFLG .NE. 0) THEN
               CALL MOVW(313, CALARR, BUF)
            ELSE
               CALL ARAGET(IDIR(33),IDIR(63),1252,BUF)
            ENDIF

            BANDOFFSET = (BAND-1)*104+5
            COUT(1:104) = CBUF(BANDOFFSET:BANDOFFSET+103)

         ELSE 

            COUT = CBUF(1:104)

         ENDIF
         CALL M0SXTRCE('KBXMSG: CAL='//COUT)



1        FORMAT(6E17.10)
         READ(COUT,1,ERR=999) C1W3,C2W,ALPHA,BETA,GAIN,OFFSET
          
         CALL M0SXTRCE('KBXMSG: GAIN='//CFF( GAIN, 4 ))
         CALL M0SXTRCE('KBXMSG: OFFSET='//CFF( OFFSET, 4 ))

         ISOU=JOPT(1)
         IDES=JOPT(2)

      ENDIF


      DO I = 1, NVAL
      ITEMP = IBUF(I)

C --- Visible and near-visible (VIS006, VIS008, IR016, HRV)
      IF( BAND.LT.4 .OR. BAND.EQ.12) THEN

C ------ RAW->TEMP, can't do temperature for this chanel
         IF( ITYPE.EQ.4 ) THEN
            IBUF(I)=0

         ELSE
            XTEMP = ( REAL(ITEMP) * GAIN ) + OFFSET
            IF( XTEMP.LE.0.0 ) XTEMP = 0.0      

C ----------radiance
            IF( ITYPE.EQ.2 ) THEN
               IBUF(I) = NINT( XTEMP*100.0 )

C --------- reflectance
            ELSEIF( ITYPE.EQ.3 ) then
               REFL = (XTEMP/FACTOR(BAND))*100
               IF( REFL.LT.  0.00 ) REFL =   0.00
               IF( REFL.GT.100.00 ) REFL = 100.00
               IBUF(I) = NINT(REFL*100)

C --------- brightness
            ELSE
               REFL = (XTEMP/FACTOR(BAND))*100
               IF( REFL.LT.  0.00 ) REFL =   0.00
               IF( REFL.GT.100.00 ) REFL = 100.00
               IBUF(I) = NINT(SQRT(REFL)*25.5)
            ENDIF

         ENDIF

C --- IR channel
      ELSE
         XTEMP = (GAIN * ITEMP) + OFFSET
         IF( XTEMP.LT.0.0 ) XTEMP = 0.0

C ------ radiance
         IF( ITYPE.EQ.2) THEN
            IBUF(I) = NINT( XTEMP*100.0 )

C ------ reflectance, can't do reflectance for these channels
         ELSEIF( ITYPE.EQ.3 ) THEN
            IBUF(I)=0

C ------ temperature
         ELSEIF( ITYPE.EQ.4) THEN

            IF( XTEMP.gt.0.0 ) THEN
               XTEMP = (C2W/LOG(1.0+C1W3/XTEMP)-BETA)/ALPHA
               IBUF(I)=NINT( XTEMP*100.0 )
            ELSE
               IBUF(I) = 0
            ENDIF

C ------ brightness
         ELSE
            IF( XTEMP.gt.0.0 ) THEN
               XTEMP = (C2W/LOG(1.0+C1W3/XTEMP)-BETA)/ALPHA
               CALL GRYSCL(XTEMP, IBRIT)
               IBUF(I) = IBRIT
            ELSE
               IBUF(I) = 255
            ENDIF

         ENDIF

      ENDIF
      ENDDO

c --- pack the array
      CALL MPIXEL(NVAL,ISOU,IDES,IBUF)

      KBXCAL=0
      RETURN

C --- READ ERROR
999   CALL M0SXTRCE('KBXMSG: CAN NOT READ CAL HEADER')
      KBXCAL=-1
      RETURN

      END
C
C--------------------------------------------------------------------

      INTEGER FUNCTION KBXOPT(CFUNC,IIN,IOUT)
      IMPLICIT NONE
      CHARACTER*4 CFUNC
      INTEGER IIN(*)
      INTEGER IOUT(*)

      ! external functions

      INTEGER LIT

      INCLUDE 'areaparm.inc'

      INTEGER ITYPE
      INTEGER JTYPE
      INTEGER JOPT(NUMAREAOPTIONS)
      INTEGER CALFLG
      INTEGER CALARR(313)
      COMMON/MSGCOM/ITYPE,JTYPE,JOPT,CALFLG,CALARR
C
      KBXOPT=0

      IF(CFUNC(1:4).EQ.'KEYS') THEN
         IF( IIN(4).LE.3 .OR. IIN(4).EQ.12 ) THEN
             IOUT(1)=4
             IOUT(2)=LIT('RAW ')
             IOUT(3)=LIT('RAD ')
             IOUT(4)=LIT('REFL')
             IOUT(5)=LIT('BRIT')
         ELSE
             IOUT(1)=4
             IOUT(2)=LIT('RAW ')
             IOUT(3)=LIT('RAD ')
             IOUT(4)=LIT('TEMP')
             IOUT(5)=LIT('BRIT')
         ENDIF
      ELSEIF(CFUNC(1:4).EQ.'INFO') THEN
         IF( IIN(1).LE.3 .OR. IIN(1).EQ.12 ) THEN
             IOUT(1)=4
             IOUT(2)=LIT('RAW ')
             IOUT(3)=LIT('RAD ')
             IOUT(4)=LIT('REFL')
             IOUT(5)=LIT('BRIT')
             IOUT(6)=LIT('    ')
             IOUT(7)=LIT('MW**')
             IOUT(8)=LIT('%   ')
             IOUT(9)=LIT('    ')
             IOUT(10)=1
             IOUT(11)=100
             IOUT(12)=100
             IOUT(13)=1
         ELSE
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
             IOUT(11)=100
             IOUT(12)=100
             IOUT(13)=1
         ENDIF
      ELSEIF(CFUNC(1:4).EQ.'CALB') THEN
         CALFLG = 1
         CALL MOVW(313,IIN,CALARR)
      ELSE
         KBXOPT=-1
      ENDIF

      RETURN
      END
