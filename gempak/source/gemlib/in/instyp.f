	SUBROUTINE IN_STYP  ( stntyp, rep, miss, unlst, list, iret )
C************************************************************************
C* IN_STYP								*
C*									*
C* This subroutine processes the STNTYP parameter.			*
C*									*
C*	If stntyp = 'A', all stations will be listed.			*
C*	If stntyp = 'R', all reporting stations ( i.e., stations	*
C*	   that report at least once ) will be listed.			*
C*	If stntyp = 'M', all missing (non-reporting) stations will be	*
C*	   listed.							*
C*	If stntyp = 'U', all stations not in the station table will be	*
C*	   listed.							*
C*	If stntyp = 'L', all stations in the station table will be	*
C*	   listed.							*
C*									*
C* A combination of station types may be listed.  E.g., STNTYP = RU	*
C* will list only those stations which are reporting AND unlisted.	*
C*									*
C* The default is to list all stations.					*
C*									*
C* IN_STYP ( STNTYP, REP, MISS, UNLST, LIST, IRET )			*
C*									*
C* Input parameters:							*
C*	STNTYP		CHAR*		STNTYP variable			*
C*									*
C* Output parameters:							*
C*	REP		LOGICAL		Reporting station flag		*
C*	MISS		LOGICAL		Missing station flag		*
C*	UNLST		LOGICAL		Unlisted station flag		*
C*	LIST		LOGICAL		Listed station flag		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-12 = invalid input		*
C**									*
C* Log:									*
C* K. Tyle/GSC		 4/97						*
C* K. Tyle/GSC		 4/97	Changed return code; changed "non-	*
C*				missing" (type N) to "reporting"	*
C*				(type R)				*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stntyp
C*
	LOGICAL		rep, miss, unlst, list
C-----------------------------------------------------------------------
	iret  = 0
	rep   = .true.
	miss  = .true.
	unlst = .true.
	list  = .true.
C
C*	Convert to upper-case.
C
	CALL ST_LCUC  ( stntyp, stntyp, ier )
	CALL ST_LSTR  ( stntyp, lens, ier )
	IF ( lens .ne. 0 ) THEN
C
C*	    Search for valid types within the string.
C
	    indxa  = INDEX ( stntyp, 'A' )
	    indxr  = INDEX ( stntyp, 'R' )
	    indxm  = INDEX ( stntyp, 'M' )
	    indxu  = INDEX ( stntyp, 'U' )
	    indxl  = INDEX ( stntyp, 'L' )
C
	    IF ( indxa .eq. 0 ) THEN
		IF ( indxr .eq. 0 ) rep   = .false.
		IF ( indxm .eq. 0 ) miss  = .false.
		IF ( indxu .eq. 0 ) unlst = .false.
		IF ( indxl .eq. 0 ) list  = .false.
		IF ( ( .not. rep   ) .and. ( .not. miss ) .and.
     +		     ( .not. unlst ) .and. ( .not. list ) ) iret = -12
	    END IF
	END IF
C*
	RETURN
	END
