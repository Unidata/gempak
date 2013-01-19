	SUBROUTINE LS_IFPT  ( lszrpt, lsfrpt, rimnem, cimnem, iret )
C************************************************************************
C* LS_IFPT								*
C*									*
C* This subroutine prints the interface values arrays to the decoder	*
C* log if the verbosity level is 3 or higher.				*
C*									*
C* LS_IFPT  ( LSZRPT, LSFRPT, RIMNEM, CIMNEM, IRET )                    *
C*									*
C* Input parameters:                                                    *
C*	LSZRPT		INTEGER		Length of report in bytes       *
C*	LSFRPT		CHAR*		Report array			*
C*      RIMNEM (*)      CHAR*           Interface mnemonics for reals	*
C*      CIMNEM (*)      CHAR*           Interface mnemonics for chars  *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = Normal return 		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/97	Adapted from AF_IFPT			*
C* R. Hollern/NCEP	 1/98	Changes based on MA_IFPT                *
C* D. Kidwell/NCEP	 5/98	Call DC_IFPC, DC_IFPR, 'LS' -> 'DC' in  *
C*                              WLOG calls                              *
C* D. Kidwell/NCEP	10/98	Added intf mnemonics to calling sequence*
C* R. Hollern/NCEP	 8/99   Got report obs time from interface      *
C*                              array and added code to print sec4 data *
C* F. J Yen/NCEP	 8/99	Removed unused include statement
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'lscmn.cmn'
C*
	CHARACTER*(*)   lsfrpt, rimnem (*), cimnem (*)
C*
	CHARACTER	logtim*25
C*
C------------------------------------------------------------------------
	iret   = 0
        loglvl = 3
C
        logmsg = '<-----BEGIN INTERFACE OUTPUT----->' 
        CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (1:50), ierw )
C
C*	Print the raw report.
C
	CALL DC_WLOG ( loglvl, 'DC', 2, 'RAW REPORT:', ierw )
	DO i = 1, lszrpt, 50
	    msz = MIN ( lszrpt, i+49 )
	    CALL DC_WLOG ( loglvl, 'DC', 2, lsfrpt ( i:msz ), ierw )
	END DO
C
C*	Print the report time.
C
	WRITE ( UNIT = logtim, FMT = '( 5I5 )' ) irptdt
	logmsg = 'REPORT TIME:' // logtim
	CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (1:50), ierw )
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
C*	Print section 1 cloud data.
C
	IF  ( rivals ( irnclo ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 5A10 )' )
     +		rimnem ( irnclo ),
     +		rimnem ( irvsso (1) ), rimnem ( irclam (1) ),
     +		rimnem ( ircltp (1) ), rimnem ( irhocb (1) )
	    CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (1:50), ierw )
	    WRITE ( UNIT = logmsg, FMT = '( 5F10.2 )' )
     +		rivals ( irnclo ),
     +		rivals ( irvsso (1) ), rivals ( irclam (1) ),
     +		rivals ( ircltp (1) ), rivals ( irhocb (1) )
	    CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (1:50), ierw )
C
	    IF  ( rivals ( irnclo ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irnclo )
		    WRITE ( UNIT = logmsg,
     +			    FMT = '( 10X, 4F10.2 )' )
     +		     rivals ( irvsso ( jj ) ), rivals ( irclam ( jj ) ),
     +		     rivals ( ircltp ( jj ) ), rivals ( irhocb ( jj ) )
		    CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (:50), ierw )
		END DO
	    END IF
C
	END IF
C
C*	Print section 3 cloud data.
C
	IF  ( rivals ( irncl3 ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 5A10 )' )
     +		rimnem ( irncl3 ),
     +		rimnem ( irvss3 (1) ), rimnem ( ircla3 (1) ),
     +		rimnem ( irclt3 (1) ), rimnem ( irhcb3 (1) )
	    CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (1:50), ierw )
	    WRITE ( UNIT = logmsg, FMT = '( 5F10.2 )' )
     +		rivals ( irncl3 ),
     +		rivals ( irvss3 (1) ), rivals ( ircla3 (1) ),
     +		rivals ( irclt3 (1) ), rivals ( irhcb3 (1) )
	    CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (1:50), ierw )
C
	    IF  ( rivals ( irncl3 ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irncl3 )
		    WRITE ( UNIT = logmsg,
     +			    FMT = '( 10X, 4F10.2 )' )
     +		     rivals ( irvss3 ( jj ) ), rivals ( ircla3 ( jj ) ),
     +		     rivals ( irclt3 ( jj ) ), rivals ( irhcb3 ( jj ) )
		    CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (:50), ierw )
		END DO
	    END IF
C
	END IF
C
C*	Print section 4 cloud data.
C
	IF  ( rivals ( irncl4 ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 6A10 )' )
     +		rimnem ( irncl4 ),     rimnem ( irvss4 (1) ),
     +		rimnem ( ircla4 (1) ), rimnem ( irclt4 (1) ),
     +		rimnem ( irhct4 (1) ), rimnem ( ircldt (1) )
	    CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (1:60), ierw )
	    WRITE ( UNIT = logmsg, FMT = '( 6F10.2 )' )
     +		rivals ( irncl4 ),       rivals ( irvss4 ( 1 ) ), 
     +	        rivals ( ircla4 ( 1 ) ), rivals ( irclt4 ( 1 ) ),
     +	        rivals ( irhct4 ( 1 ) ), rivals ( ircldt ( 1 ) )
	    CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (1:60), ierw )
C
	    IF  ( rivals ( irncl4 ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irncl4 )
		    WRITE ( UNIT = logmsg, FMT = '( 10X, 5F10.2 )' )
     +		    rivals ( irvss4 ( jj ) ), rivals ( ircla4 ( jj ) ),
     +		    rivals ( irclt4 ( jj ) ), rivals ( irhct4 ( jj ) ), 
     +		    rivals ( ircldt ( jj ) )
		    CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (1:60), ierw )
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
	    CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (1:50), ierw )
	    WRITE ( UNIT = logmsg, FMT = '( 4F10.2 )' )
     +		rivals ( irnswv ),
     +		rivals ( irdosw (1) ),
     +		rivals ( irposw (1) ), rivals ( irhosw (1) )
	    CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (1:50), ierw )
C
	    IF  ( rivals ( irnswv ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irnswv )
		    WRITE ( UNIT = logmsg,
     +			    FMT = '( 10X, 3F10.2 )' )
     +		     rivals ( irdosw ( jj ) ),
     +		     rivals ( irposw ( jj ) ), rivals ( irhosw ( jj ) )
		    CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (:50), ierw )
		END DO
	    END IF
C
	END IF
C
        logmsg = '<-----END INTERFACE OUTPUT----->'
        CALL DC_WLOG ( loglvl, 'DC', 2, logmsg (1:50), ierw )
C*
	RETURN
	END
