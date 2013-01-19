	SUBROUTINE AF_IFPT  ( rimnem, cimnem, iret )
C************************************************************************
C* AF_IFPT								*
C*									*
C* This subroutine prints the interface values arrays to the decoder	*
C* log if the verbosity level is 3 or higher.				*
C*									*
C* AF_IFPT  ( RIMNEM, CIMNEM, IRET )					*
C*									*
C* Input parameters:							*
C*	RIMNEM (*)	CHAR*		Interface mnemonics for reals   *
C*	CIMNEM (*)	CHAR*		Interface mnemonics for chars   *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		08/97						*
C* J. Ator/NCEP		03/98	Use DC_IFPC and DC_IFPR			*
C* D. Kidwell/NCEP	10/98	Added intf mnemonics to calling sequence*
C* D. Kidwell/NCEP	 6/99	Added more icing, turb, sky cover parms *
C* D. Kidwell/NCEP	 8/99	(irhocb,irhoct) -> (irhcbf,irhctf)      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	rimnem (*), cimnem (*)
C-----------------------------------------------------------------------
	iret = 0
C
C*	Print out all of the single-level character interface values
C*	that are not "missing".
C
	ncsimn = ( icwcod (1) ) - 1
	CALL DC_IFPC  ( 3, cimnem, civals, ncsimn, ierfpc )
C
C*	Print out all of the single-level real interface values
C*	that are not "missing".
C
	CALL DC_IFPR  ( 3, rimnem, rivals, NRSIMN, ierfpr )
C
C*	Print out all of the multi-level interface values.
C
C*	Turbulence.
C
	IF  ( rivals ( irntrb ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 6A10 )' )
     +		rimnem ( irntrb ),
     +		rimnem ( irdgot (1) ),
     +		rimnem ( irhbot (1) ), rimnem ( irhtot (1) ),
     +		rimnem ( irfqot (1) ), rimnem ( irtpot (1) )
	    CALL DC_WLOG  ( 3, 'AF', 1, logmsg, ierwlg )
	    WRITE ( UNIT = logmsg, FMT = '( 6F10.2 )' )
     +		rivals ( irntrb ),
     +		rivals ( irdgot (1) ),
     +		rivals ( irhbot (1) ), rivals ( irhtot (1) ),
     +		rivals ( irfqot (1) ), rivals ( irtpot (1) )
	    CALL DC_WLOG  ( 3, 'AF', 1, logmsg, ierwlg )
C
	    IF  ( rivals ( irntrb ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irntrb )
		    WRITE ( UNIT = logmsg,
     +			    FMT = '( 10X, 5F10.2 )' )
     +		     rivals ( irdgot ( jj ) ),
     +		     rivals ( irhbot ( jj ) ), rivals ( irhtot ( jj ) ),
     +		     rivals ( irfqot ( jj ) ), rivals ( irtpot ( jj ) )
		    CALL DC_WLOG  ( 3, 'AF', 1, logmsg, ierwlg )
		END DO
	    END IF
C
	END IF
C
C*	Airframe icing.
C
	IF  ( rivals ( irnicg ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 5A10 )' )
     +		rimnem ( irnicg ),
     +		rimnem ( irafic (1) ),
     +		rimnem ( irhboi (1) ), rimnem ( irhtoi (1) ),
     +		rimnem ( irtpoi (1) )
	    CALL DC_WLOG  ( 3, 'AF', 1, logmsg, ierwlg )
	    WRITE ( UNIT = logmsg, FMT = '( 5F10.2 )' )
     +		rivals ( irnicg ),
     +		rivals ( irafic (1) ),
     +		rivals ( irhboi (1) ), rivals ( irhtoi (1) ),
     +		rivals ( irtpoi (1) )
	    CALL DC_WLOG  ( 3, 'AF', 1, logmsg, ierwlg )
C
	    IF  ( rivals ( irnicg ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irnicg )
		    WRITE ( UNIT = logmsg,
     +			    FMT = '( 10X, 4F10.2 )' )
     +		     rivals ( irafic ( jj ) ),
     +		     rivals ( irhboi ( jj ) ), rivals ( irhtoi ( jj ) ),
     +		     rivals ( irtpoi ( jj ) )
		    CALL DC_WLOG  ( 3, 'AF', 1, logmsg, ierwlg )
		END DO
	    END IF
C
	END IF
C
C*	Present weather.
C
	IF  ( rivals ( irnpwx ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 4A10 )' )
     +		rimnem ( irnpwx ),
     +		cimnem ( icwcod (1) ),
     +		rimnem ( irhbwx (1) ), rimnem ( irhtwx (1) )
	    CALL DC_WLOG  ( 3, 'AF', 1, logmsg, ierwlg )
	    WRITE ( UNIT = logmsg, FMT = '( F10.2, A10, 2F10.2 )' )
     +		rivals ( irnpwx ),
     +		civals ( icwcod (1) ),
     +		rivals ( irhbwx (1) ), rivals ( irhtwx (1) )
	    CALL DC_WLOG  ( 3, 'AF', 1, logmsg, ierwlg )
C
	    IF  ( rivals ( irnpwx ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irnpwx )
		    WRITE ( UNIT = logmsg,
     +			    FMT = '( 10X, A10, 2F10.2 )' )
     +		     civals ( icwcod ( jj ) ),
     +		     rivals ( irhbwx ( jj ) ), rivals ( irhtwx ( jj ) )
		    CALL DC_WLOG  ( 3, 'AF', 1, logmsg, ierwlg )
		END DO
	    END IF
C
	END IF
C
C*	Cloud data.
C
	IF  ( rivals ( irncld ) .gt. 0 )  THEN
C
	    WRITE ( UNIT = logmsg, FMT = '( 5A10 )' )
     +		rimnem ( irncld ),
     +		rimnem ( irclam (1) ), rimnem ( ircltp (1) ),
     +		rimnem ( irhcbf (1) ), rimnem ( irhctf (1) )
	    CALL DC_WLOG  ( 3, 'AF', 1, logmsg, ierwlg )
	    WRITE ( UNIT = logmsg, FMT = '( 5F10.2 )' )
     +		rivals ( irncld ),
     +		rivals ( irclam (1) ), rivals ( ircltp (1) ),
     +		rivals ( irhcbf (1) ), rivals ( irhctf (1) )
	    CALL DC_WLOG  ( 3, 'AF', 1, logmsg, ierwlg )
C
	    IF  ( rivals ( irncld ) .gt. 1 )  THEN
		DO jj = 2, rivals ( irncld )
		    WRITE ( UNIT = logmsg,
     +			    FMT = '( 10X, 4F10.2 )' )
     +		     rivals ( irclam ( jj ) ), rivals ( ircltp ( jj ) ),
     +		     rivals ( irhcbf ( jj ) ), rivals ( irhctf ( jj ) )
		    CALL DC_WLOG  ( 3, 'AF', 1, logmsg, ierwlg )
		END DO
	    END IF
C
	END IF
C*
	RETURN
	END
