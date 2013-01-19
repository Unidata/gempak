	SUBROUTINE SNSWWE  ( winuni, vcord, iret )
C************************************************************************
C* SNSWWE								*
C*									*
C* This subroutine checks to see if wind data exist.			*
C*									*
C* SNSWWE  ( WINUNI, VCORD, IRET )					*
C*									*
C* Input parameters:							*
C*	WINUNI		CHAR*		Wind units			*
C*	VCORD		CHAR*		Vertical coordinate		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-18 = wind data not in file	*
C**									*
C* Log:									*
C* J. Whistler/SSAI	 2/91						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	winuni, vcord
C*
	CHARACTER	parm (3)*4 
	LOGICAL		chrflg (3), cmpflg (3)
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Load parameter names into array for PC package.
C
	parm (1) = vcord
	parm (2) = 'DRCT'
	IF  ( winuni .eq. 'K' )  THEN
	    parm (3) = 'SKNT'
	  ELSE
	    parm (3) = 'SPED'
	END IF
C
C*	Set PC package for winds.
C
	CALL PC_DFLV  ( 3, parm, chrflg, cmpflg, npm, ier )
	IF  ( ( .not. cmpflg (1) ) .or. ( .not. cmpflg (2) ) .or.
     +	      ( .not. cmpflg (3) ) )  THEN
	    iret = -18
	    CALL ER_WMSG  ( 'SNCROSS', iret, ' ', ier )
	END IF
C*
	RETURN
C*
	END
