	SUBROUTINE AF_OBDH  ( bulldt, ibday, ibhr, ibmin, iret )
C************************************************************************
C* AF_OBDH								*
C*									*
C* This subroutine locates and decodes the bulletin day, bulletin hour,	*
C* and bulletin minute from within the header of an AIREP, PIREP, AMDAR,*
C* or RECCO bulletin.							*
C*									*
C* AF_OBDH ( BULLDT, IBDAY, IBHR, IBMIN, IRET )				*
C*									*
C* Input parameters:							*
C*	BULLDT		CHAR*		Bulletin date-time 		*
C*									*
C* Output parameters:							*
C*	IBDAY		INTEGER		Bulletin day 			*
C*	IBHR		INTEGER		Bulletin hour 			*
C*	IBMIN		INTEGER		Bulletin minute			*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C*					 -1 = error during decoding	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		11/96	Added capability for AMDAR and RECCO 	*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NCEP		10/00	Decode bulletin minute			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	bulldt
C-----------------------------------------------------------------------
	iret = -1
C
C*	Check for the correct length of the bulletin header
C*	date-time. 
C
	CALL ST_LSTR  ( bulldt, lbdt, ierlst )
	IF  ( lbdt .ne. 6 )  THEN
	    RETURN
	END IF
C
C*	Decode the bulletin day from the bulletin header date-time.
C
	CALL ST_INTG  ( bulldt (1:2), ibday, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( ( ibday .lt. 0 ) .or. ( ibday .gt. 31 ) )  THEN
	    RETURN
	END IF
C
C*	Decode the bulletin hour from the bulletin header date-time.
C
	CALL ST_INTG  ( bulldt (3:4), ibhr, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( ( ibhr .lt. 0 ) .or. ( ibhr .gt. 23 ) )  THEN
	    RETURN
	END IF
C
C*	Decode the bulletin minute from the bulletin header date-time.
C
	CALL ST_INTG  ( bulldt (5:6), ibmin, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	ELSE IF  ( ( ibmin .lt. 0 ) .or. ( ibmin .gt. 59 ) )  THEN
	    RETURN
	END IF
C
	iret = 0
C*
	RETURN
	END
