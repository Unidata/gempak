	SUBROUTINE MT_IFPT  ( mtrlen, mtrrpt, rimnem, cimnem, iret )
C************************************************************************
C* MT_IFPT								*
C*									*
C* This subroutine prints the interface values arrays to the decoder	*
C* log if the verbosity level is 4 or higher.				*
C*									*
C* MT_IFPT  ( MTRLEN, MTRRPT, RIMNEM, CIMNEM, IRET )			*
C*									*
C* Input parameters:                                                    *
C*	MTRLEN		INTEGER		Length of report in bytes       *
C*	MTRRPT		CHAR*		Report array			*
C*	RIMNEM (*)	CHAR*		Interface mnemonics for reals   *
C*	CIMNEM (*)	CHAR*		Interface mnemonics for chars   *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = Normal return 		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP        4/98	Adapted from LS_IFPT - new interface  *
C* D. Kidwell/NCEP       10/98	Added intf mnemonics to calling sequence*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'mtcmn.cmn'
C*
	CHARACTER*(*)	mtrrpt, rimnem (*), cimnem (*) 
C*
	CHARACTER	logtim*25
C------------------------------------------------------------------------
	iret   = 0
C
        logmsg = '<-----BEGIN INTERFACE OUTPUT----->' 
        CALL DC_WLOG ( 4, 'DC', 2, logmsg (1:50), ierw )
C
C*	Print the raw report.
C
	CALL DC_WLOG ( 4, 'DC', 2, 'RAW REPORT:', ierw )
	DO i = 1, mtrlen, 50
	    msz = MIN ( mtrlen, i+49 )
	    CALL DC_WLOG ( 4, 'DC', 2, mtrrpt ( i:msz ), ierw )
	END DO
C
C*	Print the report time.
C
	WRITE ( UNIT = logtim, FMT = '( 5I5 )' ) irtarr
	logmsg = 'REPORT TIME:' // logtim
        CALL DC_WLOG ( 4, 'DC', 2, logmsg (1:50), ierw )
C
C*	Print all of the single-level character interface values
C*	that are not missing.
C
	CALL DC_IFPC ( 4, cimnem, civals, 1, ierf )
C
C*	Print all of the single-level real interface values
C*	that are not missing.
C
	CALL DC_IFPR ( 4, rimnem, rivals, NRSLMN, ierf )
C
C*	Print all of the multi-level interface values.
C*	Print visibility data.
C
	IF  ( rivals ( irnvsb ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 4A10 )' )
     +		rimnem ( irnvsb ),
     +		rimnem ( irvsby (1) ), rimnem ( irvsbk (1) ),
     +		rimnem ( irvsfl (1) )
	    CALL DC_WLOG ( 4, 'DC', 2, logmsg (1:50), ierw )
	    WRITE ( UNIT = logmsg, FMT = '( 4F10.2 )' )
     +		rivals ( irnvsb ),
     +		rivals ( irvsby (1) ), rivals ( irvsbk (1) ),
     +		rivals ( irvsfl (1) )
	    CALL DC_WLOG ( 4, 'DC', 2, logmsg (1:50), ierw )
C
	    IF  ( rivals ( irnvsb ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irnvsb )
		    WRITE ( UNIT = logmsg,
     +			    FMT = '( 10X, 3F10.2 )' )
     +		     rivals ( irvsby ( jj ) ), rivals ( irvsbk ( jj ) ),
     +		     rivals ( irvsfl ( jj ) )
		    CALL DC_WLOG ( 4, 'DC', 2, logmsg(:50), ierw )
		END DO
	    END IF
C
	END IF
C
C*	Print runway visual range data.
C
	IF  ( rivals ( irnrwy ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 7A10 )' )
     +		rimnem ( irnrwy ),
     +		cimnem ( icrwid (1) ), rimnem ( irv1ri (1) ),
     +		rimnem ( irv2ri (1) ), rimnem ( irv1rf (1) ),
     +		rimnem ( irv2rf (1) ), rimnem ( irrwyt (1) )
	    CALL DC_WLOG ( 4, 'DC', 2, logmsg (1:70), ierw )
	    WRITE ( UNIT = logmsg, FMT = '( F10.2, A10, 5F10.2 )' )
     +		rivals ( irnrwy ),
     +		civals ( icrwid (1) ), rivals ( irv1ri (1) ),
     +		rivals ( irv2ri (1) ), rivals ( irv1rf (1) ),
     +		rivals ( irv2rf (1) ), rivals ( irrwyt (1) )
	    CALL DC_WLOG ( 4, 'DC', 2, logmsg (1:70), ierw )
C
	    IF  ( rivals ( irnrwy ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irnrwy )
		    WRITE ( UNIT = logmsg,
     +			    FMT = '( 10X, A10, 5F10.2 )' )
     +		     civals ( icrwid (jj) ), rivals ( irv1ri (jj) ),
     +		     rivals ( irv2ri (jj) ), rivals ( irv1rf (jj) ),
     +		     rivals ( irv2rf (jj) ), rivals ( irrwyt (jj) )
		    CALL DC_WLOG ( 4, 'DC', 2, logmsg(:70), ierw )
		END DO
	    END IF
C
	END IF
C
C*	Print  weather phenomena data.
C
	IF  ( rivals ( irnpwx ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 2A10 )' )
     +		rimnem ( irnpwx ), cimnem ( icwcod (1) )
	    CALL DC_WLOG ( 4, 'DC', 2, logmsg (1:20), ierw )
	    WRITE ( UNIT = logmsg, FMT = '( F10.2, A10 )' )
     +		rivals ( irnpwx ), civals ( icwcod (1) )
	    CALL DC_WLOG ( 4, 'DC', 2, logmsg (1:20), ierw )
C
	    IF  ( rivals ( irnpwx ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irnpwx )
		    WRITE ( UNIT = logmsg, FMT = '( 10X, A10 )' )
     +		            civals ( icwcod ( jj ) )
		    CALL DC_WLOG ( 4, 'DC', 2, logmsg(:20), ierw )
		END DO
	    END IF
C
	END IF
C
C*	Print sky condition data.
C
	IF  ( rivals ( irnsky ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 2A10 )' )
     +		rimnem ( irnsky ), rimnem ( ircmtn (1) )
	    CALL DC_WLOG ( 4, 'DC', 2, logmsg (1:20), ierw )
	    WRITE ( UNIT = logmsg, FMT = '( 2F10.2 )' )
     +		rivals ( irnsky ), rivals ( ircmtn (1) )
	    CALL DC_WLOG ( 4, 'DC', 2, logmsg (1:20), ierw )
C
	    IF  ( rivals ( irnsky ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irnsky )
		    WRITE ( UNIT = logmsg, FMT = '( 10X, F10.2 )' )
     +		     rivals ( ircmtn ( jj ) )
		    CALL DC_WLOG ( 4, 'DC', 2, logmsg(:20), ierw )
		END DO
	    END IF
C
	END IF
C
        logmsg = '<-----END INTERFACE OUTPUT----->'
        CALL DC_WLOG ( 4, 'DC', 2, logmsg (1:50), ierw )
C*
	RETURN
	END
