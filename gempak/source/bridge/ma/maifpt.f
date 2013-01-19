	SUBROUTINE MA_IFPT  ( mszrpt, marrpt, rimnem, cimnem, iret )
C************************************************************************
C* MA_IFPT								*
C*									*
C* This subroutine prints the interface values arrays to the decoder	*
C* log if the verbosity level is 3 or higher.				*
C*									*
C* MA_IFPT  ( MSZRPT, MARRPT, RIMNEM, CIMNEM, IRET )			*
C*									*
C* Input parameters:                                                    *
C*	MSZRPT		INTEGER		Length of report in bytes       *
C*	MARRPT		CHAR*		Report array			*
C*	RIMNEM (*)	CHAR*		Interface mnemonics for reals   *
C*	CIMNEM (*)	CHAR*		Interface mnemonics for chars   *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = Normal return 		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/97	Adapted from AF_IFPT			*
C* D. Kidwell/NCEP	 5/98	Call DC_IFPC, DC_IFPR, 'MA' -> 'DC' in  *
C*                              WLOG calls                              *
C* D. Kidwell/NCEP	10/98	Added intf mnemonics to calling sequence*
C* R. Hollern/NCEP       7/99   Set loglvl to 3; report time from intf  *
C* C. Caruso Magee/NCEP	 4/01	Called ST_UNPR for Coast Guard report	*
C************************************************************************
	INCLUDE		'macmn.cmn'
C*
	CHARACTER*(*)	marrpt, rimnem (*), cimnem (*)
C*
C------------------------------------------------------------------------
	iret = 0
        loglvl = 3
C
        logmsg = '<-----BEGIN INTERFACE OUTPUT----->' 
        CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (1:50), ierwlg )
C
C*	Print the raw report.
C
	CALL DC_WLOG ( loglvl, 'DC', 2, 'RAW REPORT:', ierwlg )
	IF ( ibrtyp .eq. 4 ) THEN
C
C*	    Eliminate substrings of unprintable characters for
C*	    Coast Guard report
C
	    CALL ST_UNPR ( marrpt, mszrpt, marrpt, mszrpto, ier )
	    mszrpt = mszrpto
	END IF
	DO i = 1, mszrpt, 50
	    msz = MIN ( mszrpt, i+49 )
	    CALL DC_WLOG ( loglvl, 'DC', 2, marrpt ( i:msz ), ierwlg )
	END DO
C
C*	Print all of the single-level character interface values
C*	that are not missing.
C
	CALL DC_IFPC ( loglvl, cimnem, civals, NCIMN, ierf )
C
C*	Print all of the single-level real interface values
C*	that are not missing.
C
	CALL DC_IFPR ( loglvl, rimnem, rivals, NRSLMN, ierf )
C
C*	Print all of the multi-level interface values.
C*	Print depth, temperature, and salinity.
C
	IF  ( rivals ( irndts ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 4A10 )' )
     +		rimnem ( irndts ),
     +		rimnem ( irdbss (1) ),
     +		rimnem ( irstmp (1) ), rimnem ( irsaln (1) )
	    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:50), ierwlg )
	    WRITE ( UNIT = logmsg, FMT = '( 4F10.2 )' )
     +		rivals ( irndts ),
     +		rivals ( irdbss (1) ),
     +		rivals ( irstmp (1) ), rivals ( irsaln (1) )
	    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:50), ierwlg )
C
	    IF  ( rivals ( irndts ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irndts )
		    WRITE ( UNIT = logmsg,
     +			    FMT = '( 10X, 3F10.2 )' )
     +		     rivals ( irdbss ( jj ) ),
     +		     rivals ( irstmp ( jj ) ), rivals ( irsaln ( jj ) )
		    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:50), 
     +				    ierwlg )
		END DO
	    END IF
C
	END IF
C
C*	Print section one cloud data.
C
	IF  ( rivals ( irnclo ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 5A10 )' )
     +		rimnem ( irnclo ),
     +		rimnem ( irvsso (1) ), rimnem ( irclam (1) ),
     +		rimnem ( ircltp (1) ), rimnem ( irhocb (1) )
	    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:50), ierwlg )
	    WRITE ( UNIT = logmsg, FMT = '( 5F10.2 )' )
     +		rivals ( irnclo ),
     +		rivals ( irvsso (1) ), rivals ( irclam (1) ),
     +		rivals ( ircltp (1) ), rivals ( irhocb (1) )
	    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:50), ierwlg )
C
	    IF  ( rivals ( irnclo ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irnclo )
		    WRITE ( UNIT = logmsg,
     +			    FMT = '( 10X, 4F10.2 )' )
     +		     rivals ( irvsso ( jj ) ), rivals ( irclam ( jj ) ),
     +		     rivals ( ircltp ( jj ) ), rivals ( irhocb ( jj ) )
		    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:50), 
     +				    ierwlg )
		END DO
	    END IF
C
	END IF
C
C*	Print section three cloud data.
C
	IF  ( rivals ( irncl3 ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 5A10 )' )
     +		rimnem ( irncl3 ),
     +		rimnem ( irvss3 (1) ), rimnem ( ircla3 (1) ),
     +		rimnem ( irclt3 (1) ), rimnem ( irhcb3 (1) )
	    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:50), ierwlg )
	    WRITE ( UNIT = logmsg, FMT = '( 5F10.2 )' )
     +		rivals ( irncl3 ),
     +		rivals ( irvss3 (1) ), rivals ( ircla3 (1) ),
     +		rivals ( irclt3 (1) ), rivals ( irhcb3 (1) )
	    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:50), ierwlg )
C
	    IF  ( rivals ( irncl3 ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irncl3 )
		    WRITE ( UNIT = logmsg,
     +			    FMT = '( 10X, 4F10.2 )' )
     +		     rivals ( irvss3 ( jj ) ), rivals ( ircla3 ( jj ) ),
     +		     rivals ( irclt3 ( jj ) ), rivals ( irhcb3 ( jj ) )
		    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:50), 
     +				    ierwlg )
		END DO
	    END IF
C
	END IF
C
C*	Print continuous wind data.
C
	IF  ( rivals ( irncwd ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 4A10 )' )
     +		rimnem ( irncwd ),
     +		rimnem ( irtpmi (1) ),
     +		rimnem ( irwdrc (1) ), rimnem ( irwdsc (1) )
	    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:50), ierwlg )
	    WRITE ( UNIT = logmsg, FMT = '( 4F10.2 )' )
     +		rivals ( irncwd ),
     +		rivals ( irtpmi (1) ),
     +		rivals ( irwdrc (1) ), rivals ( irwdsc (1) )
	    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:50), ierwlg )
C
	    IF  ( rivals ( irncwd ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irncwd )
		    WRITE ( UNIT = logmsg,
     +			    FMT = '( 10X, 3F10.2 )' )
     +		     rivals ( irtpmi ( jj ) ),
     +		     rivals ( irwdrc ( jj ) ), rivals ( irwdsc ( jj ) )
		    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:50), 
     +				    ierwlg )
		END DO
	    END IF
C
	END IF
C
C*	Print wave data.
C
	IF  ( rivals ( irnswv ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 4A10 )' )
     +		rimnem ( irnswv ),
     +		rimnem ( irdosw (1) ),
     +		rimnem ( irposw (1) ), rimnem ( irhosw (1) )
	    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:50), ierwlg )
	    WRITE ( UNIT = logmsg, FMT = '( 4F10.2 )' )
     +		rivals ( irnswv ),
     +		rivals ( irdosw (1) ),
     +		rivals ( irposw (1) ), rivals ( irhosw (1) )
	    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:50), ierwlg )
C
	    IF  ( rivals ( irnswv ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irnswv )
		    WRITE ( UNIT = logmsg,
     +			    FMT = '( 10X, 3F10.2 )' )
     +		     rivals ( irdosw ( jj ) ),
     +		     rivals ( irposw ( jj ) ), rivals ( irhosw ( jj ) )
		    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:50), 
     +				    ierwlg )
		END DO
	    END IF
C
	END IF
C
        logmsg = '<-----END INTERFACE OUTPUT----->'
        CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (1:50), ierwlg )
C*
	RETURN
	END
