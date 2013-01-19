	FUNCTION PR_HCDM  ( hh )
C************************************************************************
C* PR_HCDM								*
C*									*
C* This function returns the height in meters which corresponds to a	*
C* given code figure from WMO Code Table 1677.  If the given code	*
C* figure is found to be invalid, then RMISSD is returned.		*
C*									*
C* REAL PR_HCDM  ( HH )							*
C*									*
C* Input parameters:							*
C*	HH		CHAR* 		Code figure from WMO table 1677 *
C*									*
C* Output parameters:							*
C*	PR_HCDM		REAL		Height in meters  		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NCEP		01/02						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	hh
C-----------------------------------------------------------------------
C
C*	Note that code figures 90 through 99 will also be mapped
C*	to RMISSD, since there is no clear way to set a single output 
C*	value for the ranges represented by such code figures.
C
	PR_HCDM = RMISSD
C
	CALL ST_INTG  ( hh (1:2), ihh, ier )
	IF  ( ier .eq. 0 )  THEN
	    IF  ( ( ihh .ge. 0 ) .and. ( ihh .le. 50 ) )  THEN
		PR_HCDM = FLOAT ( ihh ) * 30.
	    ELSE IF  ( ( ihh .ge. 56 ) .and. ( ihh .le. 80 ) )  THEN
		PR_HCDM = FLOAT ( ihh - 50 ) * 300.
	    ELSE IF  ( ( ihh .ge. 81 ) .and. ( ihh .le. 89 ) )  THEN
		PR_HCDM = ( FLOAT ( ihh - 80 ) * 1500. ) + 9000.
	    END IF
	END IF
C*
	RETURN
	END
