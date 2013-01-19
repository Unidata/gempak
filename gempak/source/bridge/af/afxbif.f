	SUBROUTINE AF_XBIF  ( bullx, lenbx, ibxptr, iahday, iret )
C************************************************************************
C* AF_XBIF								*
C*									*
C* This subroutine decodes any extra information located between the 	*
C* WMO header line and the start of the first report within a bulletin.	*
C* Upon exit, IBXPTR points to the start of the first report within	*
C* the bulletin.							*
C*									*
C* AF_XBIF  ( BULLX, LENBX, IBXPTR, IAHDAY, IRET )			*
C*									*
C* Input parameters:							*
C*	BULLX		CHAR*		Text portion of bulletin 	*
C*	LENBX		INTEGER		Length of BULLX 		*
C*									*
C* Input and output parameters:						*
C*	IBXPTR		INTEGER		Pointer within BULLX 		*
C*									*
C* Output parameters:							*
C*	IAHDAY		INTEGER		AMDAR header day		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = critical error in bulletin*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		08/97						*
C* J. Ator/NCEP		12/97	AF_CRID -> DC_CRID			*
C* J. Ator/NCEP		03/98	Keep decoding RECCO bulletins even if	*
C*				get bad return from DC_CRID		*
C* J. Ator/NCEP		12/98	Initialize btids, lbtids via DATA stmts,*
C*				don't decode RECCO ID from 2nd hdr line	*
C* J. Ator/NCEP		11/99	Declare field variable locally		*
C* J. Ator/NCEP		10/00	Decode AMDAR header day			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	bullx
C*
	PARAMETER	( NBTIDS = 2 )
C*
	CHARACTER	btids ( NBTIDS )*6, field*(MXLENF)
C*
	INTEGER		lbtids ( NBTIDS )
C*
	DATA		btids
     +		    / 'AIREPS', 'AIREP' /
	DATA		lbtids
     +		    / 6, 5 /
C-----------------------------------------------------------------------
	iret = -1
C
	IF  ( bultyp .eq. AIREP )  THEN
C
C*	    Check if there is an 'AIREPS' or 'AIREP' indicator to
C*	    denote the start of the bulletin.  If so, then position
C*	    the bulletin pointer directly after the indicator.
C
	    iebyte = MIN0 ( ( ibxptr + 15 ), lenbx )
	    CALL ST_NXTS  ( bullx, ibxptr, iebyte,
     +			    btids, lbtids, NBTIDS, ipt1,
     +			    ibtid, iernxt )
	    IF  ( iernxt .eq. 0 )  THEN
	       ibxptr = ibxptr + ipt1 + ( lbtids ( ibtid ) ) - 1
	    END IF
	ELSE IF  ( bultyp .eq. AMDAR )  THEN
C
C*	    Locate the 'AMDAR' indicator.
C
	    iebyte = MIN0 ( ( ibxptr + 20 ), lenbx )
	    ipt1 = INDEX ( bullx ( ibxptr : iebyte ), 'AMDAR' )
	    IF  ( ipt1 .eq. 0 )  THEN
		RETURN
	    END IF
C
C*	    The next group should be a 4-digit day/hour group.
C*	    Decode the day value.
C
	    ibxptr = ibxptr + ipt1 + 4
	    CALL AF_GFLD  ( bullx, lenbx, ibxptr, field, lenf, ier )
	    IF  ( ( ier .ne. 0 ) .or. ( lenf .ne. 4 ) )  THEN
		RETURN
	    END IF
	    CALL ST_INTG  ( field (1:2), iahday, ier )
	    IF  ( ier .ne. 0 )  THEN
		RETURN
	    ELSE IF  ( ( iahday .lt. 0 ) .or. ( iahday .gt. 31 ) )  THEN
		RETURN
	    END IF
	END IF
C
	iret = 0
C*
	RETURN
	END
