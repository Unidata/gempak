	SUBROUTINE SC_IFPT ( lenr, report, ihhmm, rimnem, cimnem, iret )
C************************************************************************
C* SC_IFPT								*
C*									*
C* This subroutine prints the interface values arrays to the decoder	*
C* log if the verbosity level is 4 or higher.				*
C*									*
C* SC_IFPT  ( LENR, REPORT, IHHMM, RIMNEM, CIMNEM, IRET )		*
C*									*
C* Input parameters:                                                    *
C*	LENR		INTEGER		Length of report in bytes       *
C*	REPORT		CHAR*		Report array			*
C*      IHHMM           INTEGER         Report time                     *
C*	RIMNEM (*)	CHAR*		Interface mnemonics for reals   *
C*	CIMNEM (*)	CHAR*		Interface mnemonics for chars   *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = Normal return 		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		12/97	Adapted from MA_IFPT			*
C* S. Jacobs/NCEP	 4/98	Fixed typo in a WRITE statement		*
C* A. Hardy/GSC		 5/98	Call DC_IFPC, DC_IFPR, 'LS' -> 'DC' in  *
C*                              WLOG calls                              *
C* D. Kidwell/NCEP	 6/98   Fixed bug to print weather types 2 and 3*
C* D. Kidwell/NCEP	10/98   Added intf mnemonics to calling sequence*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sccmn.cmn'
C*
	CHARACTER*(*)	report, rimnem (*), cimnem (*)
C*
	CHARACTER       logmsg*80, logtim*25 
C*
C------------------------------------------------------------------------
	iret   = 0
C
        logmsg = '<-----BEGIN INTERFACE OUTPUT----->' 
        CALL DC_WLOG ( 4, 'DC', 2, logmsg (1:50), ierwlg )
C
C*	Print the raw report.
C
	CALL DC_WLOG ( 4, 'DC', 2, 'RAW REPORT:', ierwlg )
	DO i = 1, lenr, 50
	    length = MIN ( lenr, i+49 )
	    CALL DC_WLOG ( 4, 'DC', 2, report ( i:length ), ierwlg )
	END DO
C
C*	Print the report time.
C
	WRITE ( UNIT = logtim, FMT = '( 6I5 )' ) ihhmm
	logmsg = 'REPORT TIME:' // logtim
        CALL DC_WLOG ( 4, 'DC', 2, logmsg (1:50), ierwlg )
C
C*	Print all of the single-level character interface values
C*	that are not missing.
C
	CALL DC_IFPC ( 4, cimnem, civals, 1, ierf )
C
C*	Print all of the single-level real interface values
C*	that are not missing.
C
	CALL DC_IFPR ( 4, rimnem, rivals, NRSIMN, ierf )
C
C*	Print all of the multi-level interface values.
C*	Print the three weather types.
C
	IF  ( rivals ( irnpwx ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 2A10 )' )
     +		    rimnem ( irnpwx ), cimnem ( icwcod (1) )
	    CALL DC_WLOG  ( 4, 'DC', 2, logmsg (1:20), ierwlg )
	    WRITE ( UNIT = logmsg, FMT = '( F10.2, A10 )' )
     +		    rivals ( irnpwx ), civals ( icwcod (1) )
	    CALL DC_WLOG  ( 4, 'DC', 2, logmsg (1:20), ierwlg )
C
	    IF  ( rivals ( irnpwx ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irnpwx )
		    WRITE ( UNIT = logmsg, FMT = '( 10X, A10 )' )
     +		            civals ( icwcod ( jj ) )
		    CALL DC_WLOG ( 4, 'DC', 2, logmsg (1:20), ierwlg )
		END DO
	    END IF
C
	END IF
C
        logmsg = '<-----END INTERFACE OUTPUT----->'
        CALL DC_WLOG ( 4, 'DC', 2, logmsg (1:50), ierwlg )
C*
	RETURN
	END
