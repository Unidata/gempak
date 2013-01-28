C Copyright(c) 1998, Space Science and Engineering Center, UW-Madison
C Refer to "McIDAS Software Acquisition and Distribution Policies"
C in the file  mcidas/data/license.txt

C *** $Id: kbxgms.dlm,v 1.1 2000/07/12 13:12:25 gad Exp $ ***

C$ Name:
C$      KBXINI - Calibration module for GMS data, initializes conversion type
C$
C$ Interface:
C$      integer function
C$      KBXINI(CHARACTER*4 CIN, CHARACTER*4 COUT, INTEGER ARRAY IOPT(*))
C$
C$ Input:
C$      CIN - Input pixel type ('RAW', 'BRIT')
C$      COUT - Output pixel type ('TEMP', 'ALB', 'RAD', 'RAW', 'BRIT')
C$      IOPT - Input calibration parameters
C$               IOPT(1) - Source pixel size (1)
C$               IOPT(2) - Destination pixel size (1,2,4)
C$
C$ Output:
C$      Error messages if return value is not 0.  (To be backward compatible
C$      with the existing API, the return value does not indicate which
C$      error was encountered.)
C$
C$ Return values:
C$        0     - success
C$       -1     - error
C$                   default GMS tables not present in system file 'GMSCAL'
C$                   or invalid byte sizes were specified for input or output,
C$                   or cannot convert to requested output units
C$
C$ Remarks:
C$      See remarks at subroutine RD_CAL for details of GMS-5 Calibration Block.
C$      Note also that for ADDE we need to comment out all SDEST and DDEST
C$      calls, since messages cannot be sent to "stdout" pipe mixed with data.
C$
C$      EDEST messages will be sent to "stderr" and can be left in, but will
C$      not be returned to a remote client.
C$
C$
C$ Categories: 
C$      image 
C$      display 
C$      calibration 
C$      met/science 


      INTEGER FUNCTION KBXINI(CIN,COUT,IOPT)

C     This function initializes the common block /GMSCAL/ and returns
C        an error if the GMSCAL file is not present.

      IMPLICIT NONE          !  All variables must be declared

C     Input Parameters:
      CHARACTER*4 CIN        !  Input pixel type (RAW, BRIT)
      CHARACTER*4 COUT       !  Output pixel type (TEMP, ALB, RAD, RAW, BRIT)
      INTEGER IOPT(*)        !  Input calibration parameters



C     Symbolic constants & global shared data in common

      INCLUDE 'areaparm.inc'         !  Global declarations for McIDAS areas
                                     !     (defines NUMAREAOPTIONS)

      COMMON/GMSXX/JTYPE,ISOU,IDES,KOPT,JOPT
      INTEGER JTYPE                  !  Flag identifying conversion code
      INTEGER ISOU                   !  Source pixel size in bytes
      INTEGER IDES                   !  Destination pixel size in bytes
      INTEGER KOPT                   !  Flag specifying how to construct ITAB
      INTEGER JOPT(NUMAREAOPTIONS)   !  Calibration parameters for conversion

      COMMON/DEBUG/IVAL,XTAB,ITAB,JTAB
      INTEGER ITAB(1024)
      INTEGER JTAB(1024)
      INTEGER IVAL
      REAL XTAB(1024)


C     External functions used (subroutines are not declared)

      INTEGER LWFILE                 !  Tests for LW file presence



C     Local variables

      INTEGER IERROR                 !  Error flag for byte size, conversion
                                     !  type, and calibration file test


C     Move the IOPT array from user's input (area) to common
      CALL MOVW(NUMAREAOPTIONS,IOPT,JOPT)
      IERROR = 0
      JTYPE = 0
      ISOU = IOPT(1)
      IDES = IOPT(2)
      IF(ISOU.NE.1.OR.(IDES.NE.1.AND.IDES.NE.2.AND.IDES.NE.4)) THEN
         IERROR = 1
         CALL EDEST('KBX_INI: Invalid byte sizes specified.',0)
         CALL EDEST('Source (must be 1)=',ISOU)
         CALL EDEST('Destination (must be 1,2, or 4)=',IDES)
      END IF

C     Identify the type of conversion that needs to be done...
C        Input units are:  RAW  |  One or two byte DN units, direct from
C                                     the spacecraft sensors 
C                          BRIT |  One byte brightness units with no
C                                     dimensions, used to display an
C                                     image on 8-bit frames (this data
C                                     may have already been run through
C                                     a transfer function, or be same as
C                                     RAW prior to 25-Aug 92)
C
C        Output units are: TEMP |  Temperature in degrees K.
C                          ALB  |  Albedo in rounded integer per cent 0-100
C                          RAD  |  Radiance milliWatts/meter**2/steradian/cm**-1
C                          BRIT |  Grayscale output for visual display (DF)
C                          MODB |  Modified brightness units, resulting
C                                     from an SU produced transfer function
C                                     table applied during a frame load
C                          RAW  |  No change to input data


C     (NOTE:  Prior to 25-Aug 1992, the RAW data was called BRIT, so we
C             have to accept both types as input to be backward compatible)

      IF((CIN.EQ.'BRIT'.OR.CIN.EQ.'RAW').AND.COUT.EQ.'TEMP') JTYPE=1
      IF((CIN.EQ.'BRIT'.OR.CIN.EQ.'RAW').AND.COUT.EQ.'ALB' ) JTYPE=2
      IF((CIN.EQ.'BRIT'.OR.CIN.EQ.'RAW').AND.COUT.EQ.'RAD' ) JTYPE=3
      IF((CIN.EQ.'BRIT'                ).AND.COUT.EQ.'MODB') JTYPE=4
      IF((CIN.EQ.'BRIT'.OR.CIN.EQ.'RAW').AND.COUT.EQ.'RAW' ) JTYPE=5
      IF((CIN.EQ.'BRIT'.OR.CIN.EQ.'RAW').AND.COUT.EQ.'BRIT') JTYPE=6
      IF((CIN.EQ.'RAW'                 ).AND.COUT.EQ.'MODB') JTYPE=7
C     Type of conversion is unrecognized
      IF(JTYPE.EQ.0) THEN
         CALL EDEST('KBX_CAL data conversion unrecognized:',0)
         CALL EDEST('          Input  type-->'//CIN//'<--',0)
         CALL EDEST('          Output type-->'//COUT//'<--',0)
         IERROR =1 
      END IF

C     Is the LWfile 'GMSCAL' present? (Contains default GMS-4 & -5 tables.)
      IF( LWFILE('GMSCAL') .EQ. 0) THEN
         CALL EDEST('KBX_INI error, LW file GMSCAL not found',0)
C        The default tables are required as part of the system
         IERROR = 1
      ENDIF
C     Is the LWfile 'GMSCALU' present? (Contains user's optional GMS tables.)
      IF( LWFILE('GMSCALU') .EQ. 0) THEN
C         CALL DDEST('KBX_INI: LW file GMSCALU not found',0)
      ENDIF
      IF(IERROR.NE.0) GO TO 900


C     Calibration can continue, initialize KOPT & return success value (0)

      KOPT=0     !  Initialize calibration table generation to try all methods
C                !
C                !  There are still some glitches associated with the long 
C                !  calibration block as of Jan 96:
C                !
C                !      1) The 2 power series coefficients for converting to
C                !         a radiance are inaccurate (we get garbage output
C                !         or radiances clearly too large or negative in
C                !         some channels when using the power series).
C                !
C                !      2) Either the first or last value in the real time
C                !         temperature or albedo tables may be garbage,
C                !         so we are replacing them with their neighboring
C                !         values, which appear to be OK.
C                !
C                !  It is suggested that the real-time radiances not be used
C                !  as defaults until we know the problem is corrected.
C                !  The user can set SYSVAL 151 to the default value of 7
C                !  to avoid the problem entirely.  Alternatively, the module
C                !  RDCAL will check for grossly unreasonable radiances
C                !  and substitute method 4 for method 8 if bits 4 and 8
C                !  are both set in KOPT, or if the default value KOPT=0
C                !  (set above, which tries all options in turn) is used.
C                !  If only method 8 is forced by "SYSVAL CHANGE 151 8",
C                !  the calibration function will complain via EDEST if the
C                !  radiances are grossly untrustworthy, and will return -1.
C                !  SYSVAL 152 will contain the bit value of the method
C                !  successfully used by RDCAL upon return.

      KBXINI=0
      GO TO 999


C     Error exit - cannot initialize calibration, return value (-1)
  900 CONTINUE
      KBXINI=-1


  999 CONTINUE
      RETURN

      END



C$ Name:
C$      KBXCAL - Converts input DN to output of correct byte size & dimensions
C$
C$ Interface:
C$      integer function
C$      KBXCAL(INTEGER CALB(*), INTEGER IDIR(*), INTEGER NVAL,
C$                                    INTEGER IBAND, INTEGER IBUF(*) )
C$
C$ Input:
C$      CALB - Line header associated with the IBUF data
C$      IDIR - Area directory associated with the IBUF data
C$      NVAL - Number of input pixels in the IBUF data
C$      IBAND - Band number for the IBUF data in the area
C$      IBUF - Buffer containing the pixel data to be converted
C$                 (The size of the input and output pixels in IBUF is
C$                  determined by the first two IOPT values in the call
C$                  to KBXINI.)
C$
C$
C$ Output:
C$      IBUF - Buffer containing the pixel data to be converted
C$                 (The size of the input and output pixels in IBUF is
C$                  determined by the first two IOPT values in the call
C$                  to KBXINI.)
C$
C$ Return values:
C$       0      - success
C$      -1      - failure to build a calibration lookup table
C$                 (output will always be returned in IBUF, even with an
C$                  invalid or unbuilt table) 
C$
C$ Remarks:
C$      KBXINI determines if the conversion is permitted, by putting
C$      constraints on the conversion types.  Here, the type is assumed
C$      already validated, and the only decision is whether to build a
C$      table for a new valid type or use the table already existing.
C$      Since data is always returned, it is the user's responsibility to
C$      test return value KBXCAL for 0 to guarantee returned data is valid.
C$
C$      The common block /DEBUG/ can be used to look at the calibration
C$      tables generated.  IVAL contains the ID of the method of generation,
C$      ITAB the real-time or locally generated albedo or temperature tables,
C$      XTAB the internal physical values (albedo, temperature, or radiance),
C$      and JTAB the final lookup table containing the scaled integer values
C$      which are output by KBX_CAL.  This information is useful for those
C$      wishing to generate and test GMSCALU files or plot calibration output.
C$
C$ Categories: 
C$      image 
C$      display 
C$      calibration 
C$      met/science 


      INTEGER FUNCTION KBXCAL(CALB,IDIR,NVAL,IBAND,IBUF)

C     This function uses the initialized common block to perform the
C        conversion from input pixel data (usually from an area, via a
C        call to REDARA) to output units of the correct byte size and
C        physical dimensions.

C        If a lookup table for the conversion hasn't been built, the
C        routine will build one, using information in the area directory
C        IDIR, the line prefix CALB, and the band number IBAND.

      IMPLICIT NONE             !  All variables must be declared

C     Input Parameters:
      INTEGER CALB(*)           !  Input array of calibration constants
                                !     from each line header for IBUF data
      INTEGER IDIR(*)           !  Area directory buffer (this is mandatory
                                !     for GMS, since the area calibration
                                !     block must be accessed locally)
      INTEGER NVAL              !  Number of pixels to process from input
      INTEGER IBAND             !  Band number (not needed for GMS-4)

C     Output Parameters:
      INTEGER IBUF(*)           !  I/O array containing pixels to be modified
                                !     (will contain converted values at end)


C     Symbolic constants & global shared data in common

      INCLUDE 'areaparm.inc'         !  Global declarations for McIDAS areas
                                     !     (defines NUMAREAOPTIONS)

      COMMON/GMSXX/JTYPE,ISOU,IDES,KOPT,JOPT
      INTEGER JTYPE                  !  Flag identifying conversion code
      INTEGER ISOU                   !  Source pixel size in bytes
      INTEGER IDES                   !  Destination pixel size in bytes
      INTEGER KOPT                   !  Flag specifying how to construct ITAB
      INTEGER JOPT(NUMAREAOPTIONS)   !  Calibration parameters for conversion

      COMMON/BRKPNT/CALTYP           !  Common block for BRKSET table type
      CHARACTER*4 CALTYP             !  Calibration type for breakpoint table

      COMMON/DEBUG/IVAL,XTAB,ITABR,JTAB
      INTEGER IVAL                   !  Identifies method used by RD_CAL
                                     !    to generate the ITAB/ITABR array
      INTEGER ITABR(1024)            !  stage 1 conversion table
                                     !    produced by RD_CAL function
                                     !    and returned as ITAB.  It is
                                     !    renamed ITABR here to avoid doubly
                                     !    defining the array.
      REAL XTAB(1024)                !  Real physical quantity corresponding
                                     !    to the input DN value: either
                                     !    albedo, temperature, or radiance
                                     !    (this quantity is converted to a
                                     !    scaled integer appearing in JTAB)
      INTEGER JTAB(1024)             !  stage 2 conversion table
                                     !    produced by KBX_CAL here
                                     !    (generates conversion transfer
                                     !     function table from albedo or
                                     !     temperature to scaled integer
                                     !     output, using BRKVAL and MPIXTB)
      INTEGER JTABV(256,4)           !  JTAB array for visible channel where
                                     !     visible sensors are identified
                                     !     by IVISN=1,2,3,4.  For IR bands,
                                     !     the value of IVISN=1 always, since
                                     !     only 1/4 of the table is used
      EQUIVALENCE (JTAB,JTABV)


C     External functions used

      INTEGER BRKVAL                 !  Provides values from breakpoint table
      REAL VPLANC                    !  Planck function
      INTEGER RDCAL                  !  Sets up the ITAB lookup table



C     Local variables and arrays

      INTEGER I                      !  index variable
      INTEGER K                      !  index variable
      INTEGER IBRIT                  !  Output of GRYSCAL TEMP-->BRIT conversion
                                     !    (high BRIT on screen means low TEMP)
      INTEGER ICHAN                  !  visible or IR channel designator
                                     !     *** may have to increase to  0-3
                                     !         for GMS-5 ***
      INTEGER IVISN                  !  Visible sensor designator 0-4
                                     !     (0 means unknown or undefined)
      REAL XALB                      !  true albedo 0.000-1.000
                                     !    (returned ALB output is
                                     !     in %, i.e. albedo*100.)
      REAL XTEMP                     !  true absolute temperature (Kelvins)
      REAL XRAD                      !  Radiance (mW/m**2/ster/cm**-1)
      INTEGER IRTAB                  !  Identifies current channel as IR
      INTEGER ITAB(1024)             !  stage 1 conversion table
                                     !    produced by RD_CAL function
                                     !    (generates a calibrated transfer
                                     !     function table from the physical
                                     !     conversion formulas -- input
                                     !     index is a function of DN value
                                     !     and IVISN, output is either an
                                     !     albedo or is in degrees Kelvin) 
      INTEGER ITABV(256,4)           !  ITAB array for visible channel where
                                     !     visible sensors are identified
                                     !     by IVISN=1,2,3,4 
      EQUIVALENCE (ITAB,ITABV)



C     Initialized variables

      INTEGER IVFLG                  !  if set to 1 means visible data 
      INTEGER LASTYP                 !  JTYPE for which tables are current
      INTEGER LASCHAN                !  ICHAN for which tables are current
      INTEGER LASBAND                !  IBAND for which tables are current

      DATA IVFLG    /  0 /
      DATA LASTYP   / -1 /
      DATA LASCHAN  / -1 /
      DATA LASBAND  / -1 /



C     Determine channel and table ID (vis or IR) to control branching logic.
C     The SS & band number are used in RD_CAL to get the actual tables.
C     Note we include the old band 8 for IR window data only for historical
C     reasons, to be backward compatible with archived data
      IF(IDIR(3).EQ.12) THEN
         ICHAN=0
         IRTAB=0
      ELSE IF(IDIR(3).EQ.13) THEN
         ICHAN=1
         IRTAB=1
      ELSE IF(IDIR(3).EQ.82.AND.IBAND.EQ.1) THEN
         ICHAN=0
         IRTAB=0
      ELSE IF(IDIR(3).EQ.82.AND.IBAND.EQ.2) THEN
         ICHAN=1
         IRTAB=1
      ELSE IF(IDIR(3).EQ.82.AND.IBAND.EQ.8) THEN
         ICHAN=1
         IRTAB=1
      ELSE IF(IDIR(3).EQ.83.AND.IBAND.EQ.1) THEN
         ICHAN=0
         IRTAB=0
      ELSE IF(IDIR(3).EQ.83.AND.IBAND.EQ.2) THEN
         ICHAN=1
         IRTAB=1
      ELSE IF(IDIR(3).EQ.83.AND.IBAND.EQ.3) THEN
         ICHAN=1
         IRTAB=1
      ELSE IF(IDIR(3).EQ.83.AND.IBAND.EQ.4) THEN
         ICHAN=1
         IRTAB=1
      ELSE IF(IDIR(3).EQ.83.AND.IBAND.EQ.8) THEN
         ICHAN=1
         IRTAB=1
      ELSE
         CALL EDEST('KBX_CAL: Unrecognized data band or SS',0)
         KBXCAL = -1
      END IF
C     Identify the visible band sensor, if possible, from line header info
      IF(IBAND.EQ.1) THEN
         IF(CALB(2).EQ.Z'6C6C0000') THEN
            IVISN=1
         ELSE IF(CALB(2).EQ.Z'B4B40000') THEN
            IVISN=2
         ELSE IF(CALB(2).EQ.Z'D8D80000') THEN
            IVISN=3
         ELSE IF(CALB(2).EQ.Z'FCFC0000') THEN
            IVISN=4
         ELSE
            IVISN=1
         END IF
      ELSE
C        All of the IR bands only use the first 1/4 of the tables
         IVISN=1
      END IF       

C     First we test JTYPE against LASTYPE to see if we need to generate
C        (or regenerate) the lookup table.  The table can be produced
C        from:
C           1) a SU generated breakpoint table (MODB),
C           2) the RAW index values (no change from input to output),
C           3) a table, ITAB, generated from a variety of possible data
C                sources examined in routine RD_CAL
C           4) no new table is needed


C     1)  Is the breakpoint table stretch to be applied?
C            (CALTYP is set to one of the two valid inputs, RAW or BRIT,
C             and MODB is the desired output type)
      IF ( (CALTYP.EQ.'BRIT' .AND. JTYPE.EQ.4 .AND. LASTYP.NE.JTYPE)
     &.OR. (CALTYP.EQ.'RAW'  .AND. JTYPE.EQ.7 .AND. LASTYP.NE.JTYPE) )
     &THEN
         DO I=1,256
            JTAB(I) = BRKVAL(REAL(I-1))
         END DO
         LASTYP = JTYPE
         KBXCAL = 0


C     2)  Is output value in units of RAW?  Then the table merely returns the
C        input value.  There is no need to generate a special transfer function.
      ELSE IF (JTYPE.EQ.5 .AND. LASTYP.NE.JTYPE) THEN
         DO I=1,256
            JTAB(I) = I-1
         END DO
         LASTYP = JTYPE
         KBXCAL = 0


C     3)  Is the input visible or IR?  Are we initializing, or changing JTYPE,
C         or changing ICHAN, and are we using the default tables in GMSCAL?
C         Or has IBAND changed?

      ELSE IF (LASTYP.NE.JTYPE .OR. LASCHAN.NE.ICHAN .OR.
     &         LASBAND .NE. IBAND) THEN

C        Is the input visible data?

         IF(ICHAN.EQ.0 .AND. (IVFLG.EQ.0 .OR. LASTYP.NE.JTYPE .OR.
     &      LASBAND .NE. IBAND) ) THEN

C           Yes... Input data is visible channel data

            LASCHAN = ICHAN
            LASBAND = IBAND
            IF(RDCAL(CALB,IDIR,IBAND,ITAB).NE.0) THEN
               CALL EDEST('KBX_CAL: RD_CAL call failed.',0)
               KBXCAL = -1
            END IF
            IVFLG=1

C           Convert visible data to albedo, ALB
            IF(JTYPE.EQ.2 .OR. JTYPE.EQ.4 .OR. JTYPE.EQ.7) THEN
C              We need to do the four interpolated tables in ITAB (0-255 each)
               DO I=1,1024
                  XALB = REAL(ITAB(I)/1000.)
                  XTAB(I) = XALB
                  IF (CALTYP.EQ.'ALB'.AND.JTYPE.NE.2) THEN       ! MODB
                     JTAB(I) = BRKVAL((XALB+0.5)/10.)
                  ELSE IF(JTYPE .EQ. 2) THEN                     ! ALB
                     JTAB(I) = INT(XALB+0.5)
                  END IF
               END DO
               LASTYP = JTYPE
            ENDIF

C           Convert visible to BRIT (usually for the DF command)
C           Note that we display albedo now to permit visible sensor variations
C           to be removed from images by using transmitted S-VISSR tables
            IF(JTYPE.EQ.6 .OR. JTYPE.EQ.7) THEN
               IF(CALTYP.EQ.'BRIT' .AND. JTYPE.NE.6) THEN     ! MODB
                  DO I=1,256
C                    There are only 256 BRKVAL levels, so we can't do the
C                      visible sensor decalibration properly, and may get
C                      line-to-line variations ("garden rake effect")
                     K = BRKVAL(REAL(ITAB(I))/4000.)
                     JTAB(I) = K
                     JTAB(I+256) = K
                     JTAB(I+512) = K
                     JTAB(I+768) = K
                  END DO
               ELSE IF(JTYPE.EQ.6) THEN                       ! BRIT
                  DO I=1,1024
C                    This makes the BRIT values in JTAB directly proportional
C                       to the albedo:  0.0 --> 0,   &   1.0 --> 250.
                     JTAB(I) = ITAB(I)/4000.
C                    We need slightly reduced contrast so users can stretch
C                       portions of the image to their own desires with EB,
C                       (especially the low albedo regions of the image) and
C                       accomodate variations between sensors & gain states.
C                       so:              0 --> 35,   &    250 --> 235.
                     JTAB(I) = 35. + (200./250.)*JTAB(I)
                  END DO
               END IF
C           Old (RAW --> BRIT) code for comparison...
C               (assumed DN values 0-255 were returned instead of albedo*1000)
C               IF(CALTYP.EQ.'BRIT' .AND. JTYPE.NE.6) THEN     ! MODB
C                     JTAB(I) = BRKVAL(REAL(I-1))
C               ELSE IF(JTYPE.EQ.6) THEN                       ! BRIT
C                     JTAB(I) = I-1
C               END IF
               IVISN=1
               LASTYP = JTYPE
            ENDIF


C        Is the input IR data?
         ELSE IF(ICHAN.EQ.1 .AND. LASTYP.NE.JTYPE .OR. 
     &           LASBAND .NE. IBAND) THEN

C           Yes... Input data is IR data

            LASCHAN = ICHAN
            LASBAND = IBAND
            IF(RDCAL(CALB,IDIR,IBAND,ITAB).NE.0) THEN
               CALL EDEST('KBX_CAL: RD_CAL call failed.',0)
               KBXCAL = -1
            END IF
            IVFLG=1

C           Convert to IR emission temperature (TEMP) in degrees Kelvin
            IF(JTYPE.EQ.1 .OR. JTYPE.EQ.4 .OR. JTYPE.EQ.7) THEN
               DO I=1,256
                  IF (CALTYP.EQ.'TEMP' .AND. JTYPE.NE.1) THEN    ! MODB
                     JTAB(I) = BRKVAL(ITAB(I)/1000.)
                  ELSE IF (JTYPE.EQ.1) THEN                      ! TEMP
                     JTAB(I) = INT(ITAB(I)/100.+0.5)
                  END IF
                  XTEMP = FLOAT(ITAB(I))/1000.
                  XTAB(I) = XTEMP
               END DO
               IVISN=1
            END IF

C           Convert to (spectral) radiance (milliWatts/meter**2/ster/cm**-1)
            IF(JTYPE.EQ.3 .OR. JTYPE.EQ.4 .OR. JTYPE.EQ.7) THEN
               DO I=1,256
                  IF (CALTYP.EQ.'RAD ' .AND. JTYPE.NE.3) THEN    ! MODB
                     JTAB(I) = BRKVAL(VPLANC(ITAB(I)/1000.,I,IDIR(3),
     &                  IBAND)*10)
                  ELSE IF (JTYPE.EQ.3) THEN                      ! RAD
                     JTAB(I) = NINT(VPLANC(ITAB(I)/1000.,I,IDIR(3),
     &                  IBAND)*100.)
                  ENDIF
                  XTEMP = FLOAT(ITAB(I))/1000.
                  XRAD = VPLANC(XTEMP,I,IDIR(3),IBAND)
                  XTAB(I) = XRAD
               END DO
               IVISN=1
            ENDIF


C           Convert temperature to gray scale using GRYSCL function
            IF(JTYPE .EQ. 6 .OR. JTYPE .EQ. 7) THEN
               LASTYP = JTYPE
               DO I=1,256
                  XTEMP = FLOAT(ITAB(I))/1000.
                  CALL GRYSCL(XTEMP,IBRIT)
                  XTAB(I)=REAL(IBRIT)
                  IF(CALTYP.EQ.'BRIT' .AND. JTYPE.NE.6) THEN     ! MODB
                     JTAB(I) = BRKVAL(REAL(IBRIT))
                  ELSE IF(JTYPE .EQ. 6) THEN                     ! BRIT
                     JTAB(I) = IBRIT
                  ENDIF
               END DO
               IVISN=1
            END IF
            LASTYP=JTYPE

         ENDIF
         KBXCAL = 0



C     4)  We don't have to change, so use the existing table and IVISN value
      ELSE
         KBXCAL =  0
      ENDIF

C     For all but 3/4 of the GMS-5 visible channels with ALB output, IVISN=1
C     The table should be correct now, so we do the lookup
C     for the physical scaled integer value pixels in IBUF

      CALL MPIXTB(NVAL,ISOU,IDES,IBUF,JTABV(1,IVISN))


      RETURN

      END




C$ Name:
C$      KBX_OPT - Returns auxiliary parameters for setup or sets internal state
C$
C$ Interface:
C$      integer function
C$      KBX_OPT(CHARACTER*4 CFUNC, INTEGER IIN(*), INTEGER IOUT(*) )
C$
C$ Input:
C$      CFUNC   - Option or function to be executed
C$                             ('KEYS', 'BRKP', 'INFO', 'METH')
C$      IIN     - Array of input parameters
C$      IOUT    - Array of output parameters
C$                  (The number of input and output parameters is determined
C$                   by the option or function executed.  The calling routine
C$                   is responsible for having arrays large enough to send
C$                   and receive output for the option or function requested.)
C$
C$ Output:
C$      IOUT    - Array of output parameters
C$                  (The number of input and output parameters is determined
C$                   by the option or function executed.  The calling routine
C$                   is responsible for having arrays large enough to send
C$                   and receive output for the option or function requested.)
C$
C$ Return values:
C$       0      - success
C$      -1      - Invalid function requested (doesn't exist)
C$      -2      - Invalid band for the spacecraft or instrument (doesn't exist)
C$      -3      - Error in breakpoint table (table cannot be set up)
C$      -4      - Lookup table generation method is invalid (doesn't exist)
C$
C$ Remarks:
C$      Check the d.pgm code to see how the various functions are used.
C$
C$ Categories: 
C$      image 
C$      display 
C$      calibration 
C$      met/science 



      INTEGER FUNCTION KBXOPT(CFUNC,IIN,IOUT)
C     Sets internal flags or states, returns parameter or state data

      IMPLICIT NONE             !  All variables must be declared

C     Input parameters:
      CHARACTER*4 CFUNC         !  Option or function descriptor
      INTEGER IIN(*)            !  Input parameters

C     Output Parameters:
      INTEGER IOUT(*)           !  Output parameters

C     ERROR CODES -1 INVALID OPTION CFUNC
C                 -2 INVALID BAND ('INFO')
C                 -3 ERROR IN BREAKPOINT TABLE OR MISSING TABLE KEYS='BRKP'
C                 -4 ERROR IN DEFINING TABLE GENERATION METHOD ('METH')


C     Symbolic constants & global shared data in common

      INCLUDE 'areaparm.inc'         !  Global declarations for McIDAS areas
                                     !     (defines NUMAREAOPTIONS)

      COMMON/GMSXX/JTYPE,ISOU,IDES,KOPT,JOPT
      INTEGER JTYPE                  !  Flag identifying conversion code
      INTEGER ISOU                   !  Source pixel size in bytes
      INTEGER IDES                   !  Destination pixel size in bytes
      INTEGER KOPT                   !  Flag specifying how to construct ITAB
      INTEGER JOPT(NUMAREAOPTIONS)   !  Calibration parameters for conversion

      COMMON/BRKPNT/CALTYP           !  Common block for BRKSET table type
      CHARACTER*4 CALTYP             !  Calibration type for breakpoint table

      COMMON/DEBUG/IVAL,XTAB,ITAB,JTAB
      INTEGER ITAB(1024)
      INTEGER JTAB(1024)
      INTEGER IVAL
      REAL XTAB(1024)


C     External functions

      INTEGER BRKSET                 !  Sets breakpoint table values
      INTEGER ISCHAR                 !  Numeric ASCII value of character byte
      INTEGER LIT                    !  Four byte integer representing CHAR*4
      INTEGER IAND                   !  Logical AND function



C     Local variables

      CHARACTER*8 CFILE              !  Breakpoint table name for SU
                                     !    (stored in frame dir words 38-39)
      INTEGER ITEST                  !  Validity test variable



C     1)  "KEYS"  option:
      IF(CFUNC.EQ.'KEYS') THEN
C        IIN contains the frame directory.  Check: is the frame visible or IR?
         IF(IIN(1).EQ.12.OR.(IIN(1).GE.82.AND.IIN(4).EQ.1) ) THEN
C           looking at visible data
            IOUT(1)=3
            IOUT(2)=LIT('RAW ')
            IOUT(3)=LIT('ALB ')
            IOUT(4)=LIT('BRIT')
         ELSE IF(IIN(1).EQ.13.OR.(IIN(1).GE.82.AND.IIN(4).GT.1) ) THEN
C           looking at IR data
            IOUT(1)=4
            IOUT(2)=LIT('RAW ')
            IOUT(3)=LIT('RAD ')
            IOUT(4)=LIT('TEMP')
            IOUT(5)=LIT('BRIT')
         ELSE
            CALL EDEST('KBX_OPT: Cannot identify vis or IR for KEYS opti
     &on.',0)
         ENDIF

C        Look for the breakpoint table name in the frame directory
         KBXOPT = 0
         IF (ISCHAR(IIN(38)).EQ. 1) THEN
            CALL MOVWC(IIN(38),CFILE)
            IF (BRKSET(CFILE,CALTYP).NE.0) THEN
               KBXOPT = -3
            ENDIF
         ENDIF


C     2)  "BRKP"  option:
      ELSE IF (CFUNC.EQ.'BRKP') THEN
         CALL MOVWC(IIN(1), CFILE)
         KBXOPT = 0
         IF (BRKSET(CFILE,CALTYP).NE.0) THEN
            KBXOPT = -3
         ENDIF


C     3)  "INFO"  option:
      ELSE  IF (CFUNC .EQ. 'INFO') THEN

C        IIN contains the band number and SS.  This option returns a list
C           of output data types from cal module, dimensions, and scale factors
         IF(IIN(2).EQ.12.OR.(IIN(2).GE.82.AND.IIN(1).EQ.1) ) THEN
C           looking at visible data
            IOUT(1) = 3
            IOUT(2) = LIT('RAW ')
            IOUT(3) = LIT('ALB ')
            IOUT(4) = LIT('BRIT')
            IOUT(5) = LIT('    ')
            IOUT(6) = LIT('  % ')
            IOUT(7) = LIT('    ')
            IOUT(8) = 1
            IOUT(9) = 10
            IOUT(10) = 1
         ELSE IF(IIN(2).EQ.13.OR.(IIN(2).GE.82.AND.IIN(1).GT.1) ) THEN
C           looking at IR data
            IOUT(1) = 4
            IOUT(2) = LIT('RAW ')
            IOUT(3) = LIT('RAD ')
            IOUT(4) = LIT('TEMP')
            IOUT(5) = LIT('BRIT')
            IOUT(6) = LIT('    ')
            IOUT(7) = LIT('MW**')
            IOUT(8) = LIT('  K ')
            IOUT(9) = LIT('    ')
            IOUT(10)= 1
            IOUT(11)= 100
            IOUT(12)= 10
            IOUT(13)= 1
         ELSE
            CALL EDEST('KBX_OPT: Cannot identify vis or IR for INFO opti
     &on.',0)
         ENDIF
         KBXOPT = 0


C     4)  "METH"  (method) option:
      ELSE  IF (CFUNC .EQ. 'METH') THEN

C        IIN contains the area, band number, and SS.  This option sets
C        the KOPT flag in common block /GMSXX/ which provides RD_CAL
C        with the user's choice of how to set up the internal calibration
C        table ITAB.  See RD_CAL Remarks for details.
C
C        Note that SYSVAL can also set KOPT by user command:
C                     "SYSVAL CHANGE 151 n"
C        where n=KOPT.  Valid KOPT bits are 8, 4, 2, & 1.

C        Test input value for invalid bits and return -4 if found
         ITEST=IAND(IIN(1),Z'FFFFFFF0')
         IF(ITEST.NE.0) THEN
            KBXOPT = -4
            KOPT = 0
            CALL EDEST('KBXOPT: Invalid calibration table method!  ',
     &         IIN(1))
            CALL EDEST('        Must be 1-15',0)
         ELSE
            KBXOPT = 0
            KOPT=IIN(1)
         END IF

C     5)  Unknown option -- error exit
      ELSE
         CALL EDEST('Unknown KBX_OPT function-->'//CFUNC//'<--',0)
         KBXOPT = -1
      END IF

      RETURN

      END



C$ Name:
C$      V_PLANC - Converts absolute temperature to radiance
C$
C$ Interface:
C$      real function
C$      V_PLANC(REAL T, INTEGER INDEX, INTEGER ISS, INTEGER IBAND)
C$
C$ Input:
C$      T       - absolute temperature
C$      INDEX   - table index (DN number)
C$      ISS     - spacecraft/instrument ID
C$      IBAND   - instrument spectral band
C$
C$ Return values:
C$
C$       V_PLANC - radiance
C$                  [units of (milliWatts/meter**2/steradian/(cm**-1))*100.0]
C$
C$
C$ Remarks:
C$      The keyin GMSCONST, was written by DAS on the mainframe (as MBGMSCAL),
C$   using formulas from an unknown source.  The operations appear reasonable,
C$   using a normalized spectral response curve and wavelength intervals of
C$   0.2 microns to do numerical integration.  The exact physical dimensions
C$   resulting from putting the constants into the formula below are not
C$   entirely clear, however.  We believe that for the IR window channel, we
C$   should be getting about 100 milliwats per meter squared per steradian
C$   per wave number, and the current output from the calibration modules
C$   is returning about 1000!  We should be doing some comparison testing
C$   with GOES-7 calibration on the same targets, so we can rely on something
C$   other than simple order-of-magnitude estimates.
C$      Note that GMS-4 uses only temperature as input, with constants which
C$   are calculated from the spectral response in the 10.5-12.5 micron window
C$   channel.  GMS-5 on the other hand, uses the index or DN value in the
C$   channel to directly convert to radiance by means of the real time power
C$   series constants transmitted in the stretched VISSR downlink data.  The
C$   spacecraft id permits tables for different spacecraft, while the band
C$   number is required to identify the correct sensor table.
C$
C$ Categories: 
C$      calibration 


      REAL FUNCTION VPLANC(T,INDEX,ISS,IBAND)

      IMPLICIT NONE
      REAL T                    ! Absolute temperature
      INTEGER INDEX             ! Table index (DN value)
      INTEGER ISS               ! Spacecraft or instrument ID
      INTEGER IBAND             ! Spectral Band for instrument

      COMMON/RAD/KTAB
      INTEGER KTAB(256)         ! Radiance table for GMS-5 from RD_CAL
                                !  (and GMP_FCO/GMT_RAD if not a power series)

C     Local variables

      REAL EXPN
      REAL TT



C     Initialized variables

      REAL FK1
      REAL FK2
      REAL TC1
      REAL TC2

C     THESE CONSTANTS FROM MBGMSCAL FOR GMS-4 ONLY
      DATA FK1 / .81021E+04 /
      DATA FK2 / .12654E+04 /
      DATA TC1 / .62070     /
      DATA TC2 / .99528     /



C     GMS-4 Uses a Planck function with the constants from above
      IF(     ISS.EQ.13
     &   .OR.(ISS.EQ.82.AND.IBAND.EQ.2)
     &   .OR.(ISS.EQ.82.AND.IBAND.EQ.8)) THEN
         TT=TC1+TC2*T
         EXPN=EXP(FK2/TT) - 1.
         VPLANC=FK1/EXPN
C     GMS-5 and following use a table generated from a power series
      ELSE IF(ISS.GE.83.AND.ISS.LE.86) THEN
         VPLANC=KTAB(INDEX)/1000.
      ELSE
         CALL EDEST('V_PLANC:  Unrecognized SS or BAND',0)
         CALL EDEST('              ISS=',ISS)
         CALL EDEST('            IBAND=',IBAND)
         VPLANC=0.0
      END IF

      RETURN

      END



C$ Name:
C$      GMT_RAD - Planck function for GMS-5
C$
C$ Interface:
C$      REAL FUNCTION
C$      GMT_RAD(REAL T,  INTEGER IBAND)
C$
C$ Input:
C$      T - Temperature (degrees K)
C$
C$ Output:
C$      IBAND - IR band number (2=10u window, 3=11u window, 4=H2O band)
C$
C$ Return values:
C$      GMT_RAD - Radiance (milliWatts/meter**2/ster/cm**-1)
C$
C$ Remarks:
C$      Call GMP_FCO to set constants in common /GMS_PFC/ before using 
C$
C$ Categories: 
C$      calibration 

      REAL FUNCTION GMTRAD(T,IBAND)

      IMPLICIT NONE
      REAL T                   !  Temperature in degrees Kelvin
      INTEGER IBAND            !  IR band number

      INTEGER NK               !  Number of bands
      INTEGER NT               !  Number of TC constants
      PARAMETER (NK=3,NT=2)

      INTEGER K                !  IR channel array index
      REAL EXPN
      REAL TT

      COMMON/GMSPFC/CWN(NK),FK1(NK),FK2(NK),TC(NT,NK)
      REAL CWN
      REAL FK1
      REAL FK2
      REAL TC

      K=IBAND-1
      TT=TC(1,K)+TC(2,K)*T
      EXPN=EXP(FK2(K)/TT) - 1.
      GMTRAD=FK1(K)/EXPN

      RETURN

      END



C$ Name:
C$      GMP_FCO - Sets up cal constants in common, given band and sensor set
C$
C$ Interface:
C$      SUBROUTINE
C$      GMP_FCO(CHARACTER*1 CDET,  INTEGER IOK)
C$
C$ Input:
C$      CDET = Detector set, A (primary) or B (redundant)
C$
C$ Output:
C$      IOK = 1 for SUCCESS, 0 for FAILURE
C$
C$ Remarks:
C$      Use before calling GMT_RAD
C$
C$ Categories: 
C$      calibration 

      SUBROUTINE GMPFCO(CDET,IOK)

      IMPLICIT NONE

      CHARACTER*1 CDET
      CHARACTER*1 CA
      CHARACTER*1 CB
      INTEGER IOK

      INTEGER NK               !  Number of bands
      INTEGER NT               !  Number of TC constants
      PARAMETER (NK=3,NT=2)
      INTEGER J                !  Index
      INTEGER K                !  Index

      COMMON/GMSPFC/CWN(NK),FK1(NK),FK2(NK),TCC(NT,NK)
      REAL CWN
      REAL FK1
      REAL FK2
      REAL TCC

      REAL AWN(NK)
      REAL AK1(NK)
      REAL AK2(NK)
      REAL ACC(NT,NK)
      REAL BWN(NK)
      REAL BK1(NK)
      REAL BK2(NK)
      REAL BCC(NT,NK)

      DATA CA/'A'/,CB/'B'/

      DATA AWN/925.374,869.613,1443.508/
      DATA AK1/.94381E+04,.78327E+04,.35825E+05/
      DATA AK2/.13314E+04,.12512E+04,.20769E+04/
      DATA ACC/.52162,.99819, .45456,.99833, .45842,.99893/

      DATA BWN/929.438,869.225,1444.768/
      DATA BK1/.95630E+04,.78222E+04,.35919E+05/
      DATA BK2/.13373E+04,.12506E+04,.20787E+04/
      DATA BCC/.53652,.99815, .51422,.99811, .48990,.99886/


      IOK=1
      IF(CDET.EQ.CA) THEN
         DO K=1,NK
            CWN(K)=AWN(K)
            FK1(K)=AK1(K)
            FK2(K)=AK2(K)
            DO J=1,NT
               TCC(J,K)=ACC(J,K)
            ENDDO
         ENDDO
      ELSE IF(CDET.EQ.CB) THEN
         DO K=1,NK
            CWN(K)=BWN(K)
            FK1(K)=BK1(K)
            FK2(K)=BK2(K)
            DO J=1,NT
               TCC(J,K)=BCC(J,K)
            ENDDO
         ENDDO
      ELSE
         IOK=0
      ENDIF

      RETURN

      END



C$ Name:
C$      RD_CAL - Generates lookup tables converting DN to physical units
C$
C$ Interface:
C$      integer function
C$      RD_CAL( INTEGER CALB(*), INTEGER IDIR(*), INTEGER IBAND,
C$                                             INTEGER ITAB(*) )
C$
C$ Input:
C$      CALB - Line header calibration data
C$      IDIR - Area directory
C$      IBAND - Band number of data in area
C$
C$ Output:
C$      ITAB - Lookup table for the band, line, and area specified
C$
C$ Return values:
C$       0      - success
C$      -1      - Lookup table not generated
C$
C$ Remarks:
C$
C$               Calibration Block for McIDAS GMS-5 Areas
C$          ==================================================
C$
C$
C$  INGESTOR:
C$
C$     The GMS ingestor contains a logical switch which identifies whether to
C$  fill only sub-block 1 or not.  Some SSEC data movers (especially on the
C$  mainframe) are sensitive to calibration blocks longer than 128 words, so
C$  the ingestor needs to fill only 128 words as an option to be backward
C$  compatible with existing software at SSEC.  Other installations may choose
C$  to use the long calibration block option, which allows use of the real-
C$  time conversion tables for level to albedo and level to temperature.  The
C$  new calibration block format (both long and short forms) is defined below.
C$
C$     In addition, the line prefix for visible GMS-5 data must contain an
C$  identifier for the visible sensor used on that line: VIS1, VIS2, VIS3, or
C$  VIS4.  If not present, or if the prefix doesn't exist, the default GMSCAL
C$  albedo table will be used (which makes no distinction between visible
C$  sensors).  This visible sensor identifier in the line prefix, in
C$  conjunction with the subcommuted GMS-5 albedo tables, will get rid of the
C$  "garden rake" effect seen in GOES-7 visible images.
C$
C$
C$
C$  CALIBRATION MODULE:
C$
C$    The GMS calibration module will accept either a long or a short
C$  calibration block, and will adjust its output accordingly (returning an
C$  error flag if any specifically requested data needed for translation
C$  table generation is not present in the calibration block, and/or the
C$  requested method or methods cannot be used).
C$
C$     Even if an error is returned, the calibration module will in most cases
C$  provide a default table, so some reasonable image data can be returned.
C$  The assumption is that if applications are smart enough to test for
C$  errors, they know what to do with them.  If no errors are tested for, we
C$  give the application something to look at anyway.  The exception concerns
C$  use of a new KBXOPT option, "METH", which specifically defines the method
C$  used to generate each calibration table.  This call uses the following
C$  code:
C$
C$         METHOD= a value from 1-15, each bit identifying one of four differ-
C$                   ent methods to be used if the corresponding bit is set
C$         IIN(1)=METHOD
C$         ISTAT=KBXOPT('METH',IIN,IOUT)     ! IOUT is unused
C$         IF(ISTAT.NE.0) THEN
C$            CALL SDEST('GMS Cal option ERROR: ',ISTAT)
C$            ! Do something
C$         END IF
C$
C$  and returns -4 (a new error value) if the method selected is invalid.
C$
C$
C$      Method(s) Used (METH)
C$      ---------------------
C$
C$              8             Use the appropriate table in the calibration
C$                               block for the indicated area, band number,
C$                               and sensor id, as ingested from the S-VISSR
C$                               real-time data.  Generate radiances by
C$                               using the power series coefficients sent
C$                               from the spacecraft.
C$
C$              4             Use the appropriate table in the calibration
C$                               block for the indicated area, band number,
C$                               and sensor id, as ingested from the S-VISSR
C$                               real-time data.  Convert temp to radiance
C$                               by using a s/c & band specific Planck function.
C$                              (This option is useful if the power series
C$                               coefficients have been corrupted, but the
C$                               temperature tables in the calibration block
C$                               are OK.)  Option 8 will automatically default
C$                               to option 4 if both bits are set and the
C$                               power series radiances are obviously bad.
C$
C$              2             Use the table in the user provided file
C$                               "GMSCALU".
C$                              (This file has the same format as GMSCAL, and
C$                               provides the user a chance to use a different
C$                               sensor DN to temperature conversion formula
C$                               or sensor DN to albedo formula than either 
C$                               the one provided by the ground station
C$                               or the one used in the default tables.
C$                               The user is responsible for copying GMSCALU
C$                               and modifying it as desired.  This file has
C$                               GMS-5 tables in it as of May 30, 1995, copied
C$                               from "Revision of GMS Stretched-VISSR Data
C$                               Format", from Japan Meteorological Agency,
C$                               dated October 1993.  It was copied to file
C$                               'gmstable.dat', and processed through the
C$                               utility keyin 'GMSCALT', not a part of the
C$                               core McIDAS distribution.
C$
C$              1             Use the "GMSCAL" file distributed with McIDAS.
C$                              (This method will always succeed unless the
C$                               GMSCAL file is missing.  It uses default
C$                               tables for both albedo and temperature, and
C$                               makes no distinction between the different
C$                               visible sensors.
C$
C$              0             Use a series of defaults as follows: 8,4,2,1
C$                               until a successful method is achieved, or
C$                               the error -1 is returned.  IF KBX_OPT is
C$                               used with the "METH" (method) option or 
C$                               SYSVAL is set, one or more bits may be
C$                               turned off, but "on" bits will still be used
C$                               in the same order, i.e. 8,1 or 4,2,1.  The
C$                               RD_CAL function will return the error -1
C$                               only in the highly unlikely event that no
C$                               table can be generated.
C$
C$
C$  ==========================================================================
C$
C$
C$
C$                             McIDAS Calibration Block
C$                             ========================
C$
C$     ( The McIDAS Calibration Block defined here is the block addressed in
C$       word 62 of the area directory.  It lies just ahead of the image data
C$       pointed to by word 33 of the area directory. This block, as defined
C$       here, is currently 7168 bytes long.  A short form of the block is
C$       128 words long and contains only the directory and sub-block 1.     )
C$
C$
C$  Word_Address  Contents
C$  ------------  ------------------------------------------------------------
C$
C$                - DIRECTORY BLOCK -
C$      0-    0   "GMS5"      Four byte ASCII ID of Calibration Block Format
C$      1-    1   0x58        Directory Block length in bytes  (Currently 88)
C$      2-    2   Sub-Block 1 Four Byte ASCII Identifier "COEF" (coefficients)
C$      3-    3   Sub-Block 1 Starting Byte Offset from word 0  (Currently 88)
C$
C$      ***** FOLLOWING SUB-BLOCKS ARE NOT PRESENT FOR 128 WORD OPTION *****
C$
C$      4-    4   Sub-Block 2 Four byte Identifier "4VIS" or "5VIS" (visible)
C$      5-    5   Sub-Block 2 Starting Byte Offset from word 0 above (1024)
C$      6-    6   Sub-Block 3 Four byte Identifier "4IR "  (10.5-12.5 microns)
C$                                              or "5IR1"  (10.5-11.5 microns)
C$      7-    7   Sub-Block 3 Starting Byte Offset from word 0 above (2048)
C$      8-    8   Sub-Block 4 Four byte Identifier "5IR2"  (11.5-12.5 microns)
C$      9-    9   Sub-Block 4 Starting Byte Offset from word 0 above (3072)
C$     10-   10   Sub-Block 5 Four byte Identifier "5IR3"  (6.5-7.0 microns)
C$     11-   11   Sub-Block 5 Starting Byte Offset from word 0 above (4096)
C$     12-   12   Sub-Block 6 Four Byte Identifier "SPAR"  (spare words)
C$     13-   13   Sub-Block 6 Starting Byte Offset from word 0 above (5120)
C$                                          (Sub-Block 6 is currently unused)
C$     14-   21   Spares
C$
C$                NOTE:  Directory or Sub-Block entries which are not filled
C$                       by the ingestor will contain zeros.  All character
C$                       strings are ASCII.  A GMS word is 8-bits in length,
C$                       McIDAS words are 32-bits!)
C$
C$  ==========================================================================
C$
C$                SUB-BLOCK 1:  - COEFFICIENT BLOCK -
C$                ------------------------------------------------------------
C$     22-   85   Words 1-256 of GMS-5 Calibration Data Block
C$     86-  127   Reserved for Future Parameterized Conversion Tables
C$
C$                   ***** END OF 128 WORD CALIBRATION BLOCK *****
C$
C$    128-  255   Unused (not present if the 128 word calibration block
C$                                                ingestor option is chosen)
C$
C$
C$                SUB-BLOCK 2:  - VISIBLE LEVEL TO ALBEDO CONVERSION TABLES -
C$                ------------------------------------------------------------
C$    256-  511   Words 257-1280 of GMS-5 Calibration Data Block
C$                   (Four 64-level tables for VIS1, VIS2, VIS3, VIS4)
C$
C$
C$                SUB-BLOCK 3:  - IR1 LEVEL TO TEMPERATURE CONVERSION TABLE -
C$                ------------------------------------------------------------
C$    512-  767   Words 1281-2304 of GMS-5 Calibration Data Block
C$
C$
C$                SUB-BLOCK 4:  - IR2 LEVEL TO TEMPERATURE CONVERSION TABLE -
C$                ------------------------------------------------------------
C$    768- 1023   Words 2305-3328 of GMS-5 Calibration Data Block
C$
C$
C$                SUB-BLOCK 5:  - IR3 LEVEL TO TEMPERATURE CONVERSION TABLE -
C$                ------------------------------------------------------------
C$   1024- 1279   Words 3329-4352 of GMS-5 Calibration Data Block
C$
C$
C$                SUB-BLOCK 6:  - SPARES -
C$                ------------------------------------------------------------
C$   1280- 1791   Words 4353-6400 of GMS-5 Calibration Data Block
C$
C$
C$  ==========================================================================
C$
C$
C$ Categories: 
C$      image 
C$      display 
C$      calibration 
C$      met/science 


      INTEGER FUNCTION RDCAL(CALB,IDIR,IBAND,ITAB)
C     Generates a lookup table converting GMS DN values to albedo
C        or absolute temperature

      IMPLICIT NONE             !  All variables must be declared

C     Input Parameters:
      INTEGER CALB(*)           !  Input array of calibration constants
                                !     from each line header for IBUF data
      INTEGER IDIR(*)           !  Area directory buffer (this is mandatory
                                !     for GMS, since the area calibration
                                !     block must be accessed locally)
      INTEGER IBAND             !  Band number of data in area

C     Output Parameters:
      INTEGER ITAB(*)           !  Lookup table containing albedo or
                                !     absolute radiation temperature

      INCLUDE 'areaparm.inc'    !  Global declarations for McIDAS areas
                                !     (defines NUMAREAOPTIONS)

      COMMON/GMSXX/JTYPE,ISOU,IDES,KOPT,JOPT
      INTEGER JTYPE             !  Flag identifying conversion code
      INTEGER ISOU              !  Source pixel size in bytes
      INTEGER IDES              !  Destination pixel size in bytes
      INTEGER KOPT              !  Flag specifying how to construct ITAB
      INTEGER JOPT(NUMAREAOPTIONS)   !  Calibration parameters for conversion

      COMMON/RAD/KTAB
      INTEGER KTAB(256)         !  Radiance table for GMS-5
      
      COMMON/DEBUG/IVAL,XTAB,ITABR,JTAB  !  We use only IVAL to pass back
      INTEGER ITABR(1024)                !   the calibration option used
      INTEGER JTAB(1024)                 !   and rename ITAB because its
      INTEGER IVAL                       !   address is passed as an argument
      REAL XTAB(1024)                    !   too, and might be susceptable to
                                         !   platform or compiler dependencies

C     Internal Parameters:
      INTEGER LTAB              !  ID number of calibration table
      INTEGER IOFF              !  byte offset into GMSCAL file(s)


C     External functions used

      INTEGER IAND                   !  Logical .AND. function
      INTEGER KSYS                   !  Acquire SYSVAL value
      INTEGER LWI                    !  LW file read
      INTEGER LIT                    !  Converts CHAR*4 to INTEGER
      REAL AMOD                      !  Real MOD function
      REAL GMTRAD                    !  Converts GMS-5 TEMP to RAD



C     Local variables and arrays

      INTEGER I                      !  index variable
      INTEGER J                      !  index variable
      INTEGER K                      !  index variable
      INTEGER NLEN                   !  Length of arbitrary block in bytes
      INTEGER KEND                   !  End word count of a block
      INTEGER LBSTART                !  Input block starting byte count
      INTEGER NBSTART                !  Output block starting byte count
      INTEGER IV                     !  Input index value
      INTEGER IXV                    !  Interpolated data output index value
      INTEGER V1                     !  First table value
      INTEGER V2                     !  Second table value
      INTEGER IAREA                  !  area number from IDIR array
      INTEGER ISS                    !  spacecraft/sensor ID from IDIR array
      INTEGER ISTAT                  !  LWI status returned
      INTEGER IYR                    !  Year
      INTEGER IMO                    !  Month
      INTEGER IDA                    !  Day of month
      INTEGER IHR                    !  Hour of day (GMT)
      INTEGER IMN                    !  Minute of hour
      INTEGER NTABS                  !  number of tables in the GMSCAL file
                                     !    (from first word in file)
      INTEGER DIR(512)               !  Default GMSCAL table directory
      INTEGER ICALBL(1792)           !  GMS-5 McIDAS Calibration block
      INTEGER IDCAL                  !  Calibration ID from data block
      INTEGER IDATE(2)               !  Six-byte date data block was generated
                                     !    (YY,YY,MM,DD,HH,mm)
      INTEGER IDSEN                  !  Sensor selector byte
                                     !    (1=primary,   2=redundant)
      INTEGER IOK                    !  bad radiance value counter
                                     !    (significant if non-zero)
      INTEGER JOK                    !  error flag for GMP_FCO function
                                     !    (significant if non-zero)
      INTEGER KBYT                   !  Byte address of CNAME block
      INTEGER IBETA(7,3)             !  Calibration constants for the three
                                     !    IR bands for converting DN to
                                     !    sensor output voltage (scaled ints)
      INTEGER IFACT(3)               !  Number of calibration constants in
                                     !    the series expansion for each band
      INTEGER IG(3)                  !  G constant for each IR band
      INTEGER IV0(3)                 !  V0 constant for each IR band
      INTEGER IC0(3)                 !  C0 constant for each IR band
      INTEGER ISPARE(3)              !  Unused spare location in table
      INTEGER IFAC                   !  Number of calibration constants in
                                     !    the series expansion to be used

      REAL RIV                       !  Real input index value IV
      REAL RREM                      !  Remainder mod 1.0
      REAL BETA(7,3)                 !  Calibration constants for the three
                                     !    IR bands for converting DN to
                                     !    sensor output voltage
      REAL G                         !  G constant to be used
                                     !     (volts/watt/cm**2/sr)
      REAL V0                        !  V0 constant to be used
                                     !     (zero level voltage)
      REAL C0                        !  C0 constant to be used
      REAL C                         !  Intermediate "DN" value used in the
                                     !    series expansion
      REAL V                         !  Output voltage from series expansion
      REAL R                         !  Radiance from voltage and G constant
                                     !     (watts/cm**2/sr)
      REAL WNFCTR(3)                 !  Wave number factor to convert from
                                     !     W/cm**2/sr to W/etc/cm**-1
                                     !     for the GMS-5 IR bands
      CHARACTER*12 CFILE             !  Input file containing default
                                     !     calibration tables
      CHARACTER*1 CALBL(7168)        !  GMS-5 McIDAS Calibration block
      CHARACTER*4 CNAME              !  Table Name in calibration data block

      DATA WNFCTR /.7903, .7874, .8676/


C     Check that we have necessary information and that requested band exists
      ISS=IDIR(3)
      IAREA=IDIR(33)


C     Determine the number of the default calibration table in the file to use
C        File structure is as follows:
C            Word       Contents
C         -----------   ---------------------------------------------
C              1        Number of tables in the file
C            2-512      ID number (1-511) of the table (if present)
C          513-768      1) GMS-4 Visible Channel
C          769-1024     2) Unused
C         1025-1280     3) GMS-4 IR Channel
C         1281-1536     4) GMS-5 Visible Channel
C         1537-1792     5) GMS-5 IR1 Channel
C         1793-2048     6) GMS-5 IR2 Channel
C         2049-2304     7) GMS-5 IR3 Channel

      LTAB=0
      IF(IDIR(3).EQ.12) THEN
         LTAB=1
      ELSE IF(IDIR(3).EQ.13) THEN
         LTAB=3
      ELSE IF(IDIR(3).EQ.82.AND.IBAND.EQ.1) THEN
         LTAB=1
      ELSE IF(IDIR(3).EQ.82.AND.IBAND.EQ.2) THEN
         LTAB=3
      ELSE IF(IDIR(3).EQ.82.AND.IBAND.EQ.8) THEN
         LTAB=3
      ELSE IF(IDIR(3).EQ.83.AND.IBAND.EQ.1) THEN
         LTAB=4
      ELSE IF(IDIR(3).EQ.83.AND.IBAND.EQ.2) THEN
         LTAB=5
      ELSE IF(IDIR(3).EQ.83.AND.IBAND.EQ.3) THEN
         LTAB=6
      ELSE IF(IDIR(3).EQ.83.AND.IBAND.EQ.4) THEN
         LTAB=7
      ELSE IF(IDIR(3).EQ.83.AND.IBAND.EQ.8) THEN
         LTAB=5
      ELSE
         CALL EDEST('RD_CAL: Unrecognized data band or SS',0)
         RDCAL=-1
         GO TO 500
      END IF

C     KOPT is forced by KBX_OPT, or can be acquired or changed
C     from SYSVAL 151, or it defaults to 15
C     If KOPT is valid (between 1-15), it is assumed already set in KBX_OPT 
C     If not set, we look first in SYSVAL for a valid value      
      IF(KOPT.LE.0.OR.KOPT.GT.15) THEN
         KOPT=KSYS(151)
      END IF
C     If after SYSVAL KOPT is still unspecified, or invalid, then try all
C     the options for generating the table
      IF(KOPT.LE.0.OR.KOPT.GT.15) THEN
C         KOPT=15
C     We currently default to option 7 because we don't trust the WNFCTR
C     constants
         KOPT=7
C     We must use option=3 default for GMS-4 and earlier, however
         IF(IDIR(3).LE.82) KOPT=3
      END IF
C     Inform user if KOPT was preset in error
      IF(IDIR(3).LE.82.AND.KOPT.GT.3) THEN
         CALL EDEST('Invalid calibration option for SS=',IDIR(3))
         CALL EDEST('     ...was ',KOPT)
         KOPT=3
         CALL EDEST('     ...reset to ',KOPT)
         CALL EDEST('Options > 3 valid for GMS-5 and later only!',0)
      END IF
      
C     Return the option which was successful to the calling program or
C     debugging routine in IVAL, in case the information is desired.
C     SYSKEY word 152 also will contain IVAL, so the user can check which
C     calibration method was used.
C     Note that KOPT only identifies the options to be tested, not which
C     one was finally successful!
      IVAL=0

      IDSEN=0


C     Now do the requested options in order of precedence
C     If options 8 or 4 are chosen, we need the real-time calibration data
      IF(IAND(KOPT,12).NE.0) THEN
      
C        These two options read the real-time calibration from the area
         CALL ARAGET(IAREA,IDIR(63),7168,ICALBL)

         IF(ICALBL(1).NE.LIT('GMS5') .OR. ICALBL(2).GT.90) THEN
            CALL EDEST('Calibration block is not GMS-5 format.',0)
            CALL EDEST('It cannot be used by the GMS calibration module.
     &',0)
            CALL EDEST('Will attempt to use GMSCAL file tables instead.'
     &,0)
            GO TO 200
         END IF

         NLEN=ICALBL(2)


C        Get the coefficient block and unpack it
         IF(ICALBL(3).EQ.LIT('COEF')) THEN
            CALL MOVB(256,ICALBL(1),CALBL,0)
            DO I=1,60,3
               DO J=I,I+2
                  DO K=-3,0
                     IF(ICHAR(CALBL(J*4+K)).LT.32.OR.
     &                  ICHAR(CALBL(J*4+K)).GT.126)
     &                        CALBL(J*4+K)='*'
                  END DO
               END DO
            END DO

C           Start unpacking the block contents
            CALL MOVB(256,ICALBL(ICALBL(4)/4+1),CALBL,0)
            CALL MOVB(4,CALBL(1),IDCAL,0)
            CALL MOVB(6,CALBL(5),IDATE,0)
            IYR=IDATE(1)/65536
            IMO=(IDATE(1)-IYR*65536)/256
            IDA=IDATE(1)-IYR*65536-IMO*256
            IHR=(IDATE(2)/65536)/256
            IMN=IDATE(2)/65536-IHR*256
            CALL MOVB(1,CALBL(11),IDSEN,0)
            IDSEN=IDSEN/16777216              ! shift right 3 bytes
            IFACT(1)=ICHAR(CALBL(12))
            CALL MOVB(4,CALBL(13),IBETA(1,1),0)
            CALL MOVB(4,CALBL(17),IBETA(2,1),0)
            CALL MOVB(4,CALBL(21),IBETA(3,1),0)
            CALL MOVB(4,CALBL(25),IBETA(4,1),0)
            CALL MOVB(4,CALBL(29),IBETA(5,1),0)
            CALL MOVB(4,CALBL(33),IBETA(6,1),0)
            CALL MOVB(4,CALBL(37),IBETA(7,1),0)
            CALL MOVB(4,CALBL(41),IG(1),0)
            CALL MOVB(4,CALBL(45),IV0(1),0)
            CALL MOVB(4,CALBL(49),IC0(1),0)
            CALL MOVB(4,CALBL(53),ISPARE(1),0)
            IFACT(2)=ICHAR(CALBL(57))
            CALL MOVB(4,CALBL(58),IBETA(1,2),0)
            CALL MOVB(4,CALBL(62),IBETA(2,2),0)
            CALL MOVB(4,CALBL(66),IBETA(3,2),0)
            CALL MOVB(4,CALBL(70),IBETA(4,2),0)
            CALL MOVB(4,CALBL(74),IBETA(5,2),0)
            CALL MOVB(4,CALBL(78),IBETA(6,2),0)
            CALL MOVB(4,CALBL(82),IBETA(7,2),0)
            CALL MOVB(4,CALBL(86),IG(2),0)
            CALL MOVB(4,CALBL(90),IV0(2),0)
            CALL MOVB(4,CALBL(94),IC0(2),0)
            CALL MOVB(4,CALBL(98),ISPARE(2),0)
            IFACT(3)=ICHAR(CALBL(102))
            CALL MOVB(4,CALBL(103),IBETA(1,3),0)
            CALL MOVB(4,CALBL(107),IBETA(2,3),0)
            CALL MOVB(4,CALBL(111),IBETA(3,3),0)
            CALL MOVB(4,CALBL(115),IBETA(4,3),0)
            CALL MOVB(4,CALBL(119),IBETA(5,3),0)
            CALL MOVB(4,CALBL(123),IBETA(6,3),0)
            CALL MOVB(4,CALBL(127),IBETA(7,3),0)
            CALL MOVB(4,CALBL(131),IG(3),0)
            CALL MOVB(4,CALBL(135),IV0(3),0)
            CALL MOVB(4,CALBL(139),IC0(3),0)
            CALL MOVB(4,CALBL(143),ISPARE(3),0)

C        Calculate the RAD tables for VPLANC in W/cm**2/sr and scale by 1000
C        NOTE:  The formulas in Table A-7 don't make sense unless the series
C               expansion converts DN levels to a voltage, V, not C

C           First convert the scaled integers to reals for the requested band
            IF(IBAND.GT.1.AND.IBAND.LE.4) THEN

C              Mask out sign bit and apply scaling factor
C              Apply R4.6 scaling factors (4 byte ints scaled by 10**-6)
               G= IAND( IG(IBAND-1),Z'7FFFFFFF')*1.0E-6
               V0=IAND(IV0(IBAND-1),Z'7FFFFFFF')*1.0E-6
               C0=IAND(IC0(IBAND-1),Z'7FFFFFFF')*1.0E-6

C              Add sign bit back to the scaled numbers
               G=SIGN(G,REAL(IG(IBAND-1)))
               V0=SIGN(V0,REAL(IV0(IBAND-1)))
               C0=SIGN(C0,REAL(IC0(IBAND-1)))

C              The number of coefficients runs from 1-7 (presumably)   
               IFAC=IFACT(IBAND-1)+1

C              The number of power series coefficients may not be
C              passed correctly in the real time data, so we default
C              to a valid number to avoid excessive looping.  The
C              assumption is that non-valid coefficients will still
C              be set to 0.  Negative IFAC simply can't be used.
C              We filter out grossly bad radiances later on (IOK).
               IF(IFAC.LT.1.OR.IFAC.GT.7) THEN
                  IFAC=7
               END IF

               DO I=1,3
                  DO J=1,IFAC
                     BETA(J,I)=IAND(IBETA(J,I),Z'7FFFFFFF')*1.0E-6
                     BETA(J,I)=SIGN(BETA(J,I),REAL(IBETA(J,I)))
                  END DO
               END DO

               DO J=1,255                       !  index on S-VISSR DN level
                  C=255.-J+C0                   !  convert to instrument DN
                  V=0.0
                  DO K=1,IFAC                   !  do the series expansion
                     V=V+BETA(K,IBAND-1)*C**(K-1)
                  END DO
                  R=(V-V0)/G                    !  convert to radiance W/etc.
                  R=0.5*R/WNFCTR(IBAND-1)       !  convert to mW/etc./cm**-1
                  KTAB(J)=INT(R*1000.0)         !  scale by 1000 and put into
                                                !     the KTAB table as mW/etc.
               END DO

C              Test that radiance falls into expected range 0-190 mW/etc.
               IOK=0
               DO J=2,255
                  IF(KTAB(J).LT.0.OR.KTAB(J).GT.190000) IOK=IOK+1
               END DO

            END IF

         ELSE

            GO TO 100

         END IF


C        Choose the table name needed
         IF(LTAB.LE.3) THEN
            GO TO 100
         END IF
         IF(LTAB.EQ.4) CNAME='5VIS'
         IF(LTAB.EQ.5) CNAME='5IR1'
         IF(LTAB.EQ.6) CNAME='5IR2'
         IF(LTAB.EQ.7) CNAME='5IR3'
         IF(LTAB.GE.8) THEN
            GO TO 100
         END IF

C        Locate the table, if it exists in the cal block, by searching the
C          cal block directory
         KBYT=0
         KEND=NLEN/4

C        The KEND test triggers only if the directory contains garbage, which
C        shouldn't happen if it is properly identified and written, but it
C        is easy to corrupt the data if moved between big endian and little
C        endian machines with mixed ADDE and ftp.
         IF(KEND.GT.22) THEN
            CALL EDEST('RD_CAL:  Bad directory structure. KEND=',KEND)
            KEND=22
         END IF
         DO J=5,KEND
            IF(ICALBL(J).EQ.LIT(CNAME)) THEN
               KBYT=ICALBL(J+1)
            END IF
         END DO
         IF(KBYT.NE.0) THEN
            CALL MOVB(1024,ICALBL(KBYT/4+1),ITAB,0)

C           We need to extend the table at each end, since the first or last
C           values are sometimes corrupted!!! (Hopefully this will be
C           eventually fixed)
            ITAB(1)=ITAB(2)
            ITAB(256)=ITAB(255)

         ELSE

            CALL EDEST('KBYT cannot be set -- no table exists',0)
            GO TO 100

         END IF


C        The visible tables need more work, otherwise we can quit this band
         IF(IBAND.NE.1) GO TO 80


C        We expand the visible table in place by interpolation from
C        64 to 256 levels per sensor, building from the back of the
C        array so we don't overwrite anything we need
         DO I=4,1,-1
            LBSTART = 1 + (I-1)*64
            NBSTART = (I-1)*256
            DO IXV=256,1,-1
               RIV=(FLOAT(IXV-1)/252.)*63.
               IV=RIV
               V1=ITAB(LBSTART+IV)
               V2=ITAB(LBSTART+IV+1)
C              Make sure the table doesn't go higher than 1.00 for vals > 252
               IF(IV.EQ.63) V2=1000000
               RREM=AMOD(RIV,1.)
               ITAB(NBSTART+IXV)=(V1+(V2-V1)*RREM+0.5)
            END DO
         END DO

C        Temp or albedo table successfully generated, so we can quit (unless
C        the power series is garbage and we need to convert the radiance
C        tables to GMTRAD output)
   80    CONTINUE
         IVAL=8
         IF(IOK.GT.5.AND.IAND(KOPT,15).EQ.8) THEN
            CALL EDEST('Too many of the real-time power series radiances
     & are',0)
            CALL EDEST('outside expected limits and are likely to be in 
     &error.',0)
            GO TO 100
         END IF

C        If we want only the real time temperature tables and still need
C        to recalculate the radiance tables, use GMPFCO in method 4 below
         IF(IAND(KOPT,4).EQ.0) GO TO 400
      END IF


C     If we get to here, we must try the next method of generating a table
C     ( either KOPT < 8 or IOK > 5 )
  100 CONTINUE

      IF(IAND(KOPT,4).NE.0) THEN

C        This option makes the tables from a GMS-5 Planck function

C        Set up table of radiances for GMS-5
         IF(ISS.EQ.83) THEN
            IF(IDSEN.NE.2) CALL GMPFCO('A',JOK)
            IF(IDSEN.EQ.2) CALL GMPFCO('B',JOK)
            IF(JOK.EQ.0) THEN
               GO TO 300
            END IF
            DO J=1,256
               KTAB(J)=GMTRAD(ITAB(J)*0.001,IBAND)*1000.
            END DO
         ELSE
            GO TO 200
         END IF

C        Table successfully generated, so we can quit
         IVAL=4
         GO TO 400

      END IF


C     If we get to here, we must try the next method of generating a table
  200 CONTINUE

      IF(IAND(KOPT,2).NE.0) THEN

C        This option reads from user file GMSCALU
         CFILE='GMSCALU'

C        LTAB is the number of the calibration table for the GMS channel

C        Read the directory first to get the number of tables
         ISTAT=LWI(CFILE,0,512,DIR)
C        Return words to native byte order
         CALL SWBYT4 (DIR, 512)
         IF(ISTAT.NE.0.AND.IAND(KOPT,1).EQ.0) THEN
            CALL EDEST('ERROR reading calibration tables from file '//
     &         CFILE,0)
            GO TO 300
         END IF 
         NTABS=DIR(1)

C        Next, read appropriate table, as determined by the SS and BAND number
         IOFF = 512+(LTAB-1)*256
         IF(LTAB.GT.NTABS) THEN
            GO TO 300
         END IF
         ISTAT = LWI(CFILE,IOFF,256,ITAB)
C        Return words to native byte order
         CALL SWBYT4 (ITAB, 256)

C        The visible tables need to be duplicated for the four visible channels
         DO J=1,256
            ITAB(J+256)=ITAB(J)
            ITAB(J+512)=ITAB(J)
            ITAB(J+768)=ITAB(J)
         END DO

         IF(ISTAT.NE.0) THEN
            GO TO 300
         END IF

C        Set up table of radiances for GMS-5 using calibration constants
         IF(ISS.EQ.83) THEN
            IF(IDSEN.NE.2) CALL GMPFCO('A',JOK)
            IF(IDSEN.EQ.2) CALL GMPFCO('B',JOK)
            IF(JOK.EQ.0) THEN
               GO TO 300
            END IF
            DO J=1,256
               KTAB(J)=GMTRAD(ITAB(J)*0.001,IBAND)*1000.
            END DO
         END IF

C        Table successfully generated, so we can quit
         IVAL=2
         GO TO 400

      END IF

C     If we get to here, we must try the next method of generating a table
  300 CONTINUE


      IF(IAND(KOPT,1).NE.0) THEN

C        This option reads from user file GMSCAL
         CFILE='GMSCAL'

C        LTAB is the number of the calibration table for the GMS channel

C        Read the directory first to get the number of tables
         ISTAT=LWI(CFILE,0,512,DIR)
         CALL SWBYT4 (DIR, 512)
         IF(ISTAT.NE.0) THEN
            CALL EDEST('ERROR reading calibration tables from file '//
     &         CFILE,0)
            RDCAL=-1
            GO TO 500
         END IF 
         NTABS=DIR(1)

C        Next, read appropriate table, as determined by the SS and BAND number
         IOFF = 512+(LTAB-1)*256
         IF(LTAB.GT.NTABS) THEN
            CALL EDEST('RD_CAL ERROR -- Unrecognized Table ID=',LTAB)
            CALL EDEST('The table index is greater than number of tables in
     & in the file '//CFILE,0)
            CALL EDEST('Check SS and band number for consistency with the t
     &e table ID',0)
            CALL EDEST('                                   SS=',IDIR(3))
            CALL EDEST('                                 Band=',IBAND)
            RDCAL=-1
            GO TO 500
         END IF
         ISTAT = LWI(CFILE,IOFF,256,ITAB)
         CALL SWBYT4 (ITAB, 256)

C        The visible tables need to be duplicated for the four visible channels
         DO J=1,256
            ITAB(J+256)=ITAB(J)
            ITAB(J+512)=ITAB(J)
            ITAB(J+768)=ITAB(J)
         END DO

         IF(ISTAT.NE.0) THEN
            CALL EDEST('File status error reading file'//CFILE,ISTAT)
            RDCAL=-1
            GO TO 500
         END IF

C        Set up table of radiances for GMS-5 using calibration constants
         IF(ISS.EQ.83) THEN
            IF(IDSEN.NE.2) CALL GMPFCO('A',JOK)
            IF(IDSEN.EQ.2) CALL GMPFCO('B',JOK)
            IF(JOK.EQ.0) THEN
               RDCAL=-1
               GO TO 500
            END IF
            DO J=1,256
               KTAB(J)=GMTRAD(ITAB(J)*0.001,IBAND)*1000.0
            END DO
         END IF

C        Table successfully generated, so we can quit
         IVAL=1
         GO TO 400

      END IF


C     Table not successfully generated, so we must return an error
      RDCAL=-1
      GO TO 500

C     We now have the default table here or we should have returned an error
C     or not have set IVAL
  400 CONTINUE
      IF(IVAL.EQ.0) THEN
         RDCAL=-1
      ELSE
         RDCAL=0
      END IF


  500 CONTINUE
C     Pass the method used back out for the user to check with SYSVAL
      CALL SYSIN(152,IVAL)

C     Move the ITAB table to common for debugging
      DO I=1,1024
         ITABR(I)=ITAB(I)
      END DO
      RETURN

      END

