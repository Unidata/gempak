        SUBROUTINE  LS_AAXX (  iaaxx, istarr, iret )
C************************************************************************
C* LS_AAXX                                                              *
C*                                                                      *
C* This subroutine decodes the YYGGi(w) group data which is stored in   *
C* the iaaxx array.  The YYGGi(w) group follows the AAXX group.  A	*
C* bulletin can have more than one set of these groups.			*
C*                                                                      *
C* LS_AAXX ( IAAXX, ISTARR, IRET )					*
C*                                                                      *
C* Input parameters:                                                    *
C*      IAAXX          CHAR*  	    YYGGi(w) group in AAXX line         *
C*      ISTARR(*)      INTEGER	    System time - YYYY,MM,DD,HH,MM      *
C*					                                *
C* Output parameters:                                                   *
C*	IUWIND         INTEGER 	    Indicator for source and units of	*
C*			 	    of wind speed (WMO Code  Table 1855)*
C*      IHOUR          INTEGER	    Hour of observation of report       *
C*      IRPTDT(*)      INTEGER	    Report date-time (YYYY,MM,DD,HH,MM) *
C*      IRET           INTEGER	    Return code                         *
C*                             	      0 = normal return                 *
C*                             	      1 = problems                      *
C**                                                                     *
C* Log:                                                                 *
C* R. Hollern/NCEP       6/02                                           *
C* D. Kidwell/NCEP       7/02 	Modified for GEMPAK                     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE         'lscmn.cmn'
C*
        CHARACTER*(*)   iaaxx
        INTEGER         istarr (*)
C*
        CHARACTER       fld2*2,  fld1*1
C------------------------------------------------------------------------
        iret = 0
C
C*      Get day of month of observation.
C
        fld2 = iaaxx ( 1:2 )     
        CALL  ST_INTG ( fld2, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            IF ( ival .gt. 0 .and. ival .lt. 32 ) THEN
                imnday = ival
              ELSE
                iret = 1
            END IF
          ELSE
            iret = 1
        END IF
C
C*      Get hour of observation.
C
        fld2 = iaaxx ( 3:4 )     
        CALL  ST_INTG ( fld2, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            IF ( ival .ge. 0 .and. ival .lt. 24 ) THEN
                ihour = ival
              ELSE
                iret = 1
            END IF
          ELSE
            iret = 1
        END IF
C
	IF ( iret .eq. 1 ) RETURN
C
C*      Set minutes of obs hour to zero.
C
        imins = 0
C
C*      Combine the run times and obs times into a report date-time.
C
        ndays = 10
        CALL DC_RTIM ( istarr, imnday, ihour, imins, ndays, irptdt, ier)
C
        IF ( ier .ne. 0 ) THEN
            iret = 1
            RETURN
        END IF
C
C*      Save date/time data interface array.
C
        rivals ( iryear ) = FLOAT ( irptdt (1) )
        rivals ( irmnth ) = FLOAT ( irptdt (2) )
        rivals ( irdays ) = FLOAT ( irptdt (3) )
        rivals ( irhour ) = FLOAT ( irptdt (4) )
        rivals ( irminu ) = FLOAT ( irptdt (5) )
C
C*      Get indicator for source and units of wind speed.
C
        fld1 = iaaxx ( 5:5 )   
C
        IF ( fld1 .eq. '0' .or. fld1 .eq. '1'  .or.
     +       fld1 .eq. '3' .or. fld1 .eq. '4' ) THEN
C
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) THEN
		iuwind = FLOAT ( ival )
            END IF
        END IF
C*
	RETURN
	END
