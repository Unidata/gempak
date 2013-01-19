	SUBROUTINE GD_GLEVW ( iacss, gdattm1, gdattm2, ivcord, maxlev,
     +        	              levarr1, levarr2, nlev, iret )
C************************************************************************
C* GD_GLEVW								*
C*									*
C* This subroutine returns all the levels present in a grid file for	*
C* a given date and vertical coordinate.  The levels returned are	*
C* not sorted.								*
C*									*
C* GD_GLEVW ( iacss, gdattm1, gdattm2, ivcord, maxlev, levarr1, levarr2,*
C*            nlev, iret )						*
C*									*
C* Input parameters:							*
C*	IACSS		INTEGER		Grid access number		*
C*	GDATTM1		CHAR*		GEMPAK times			*
C*	GDATTM2		CHAR*		GEMPAK times			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*	MAXLEV		INTEGER		Maximum number of levels	*
C*									*
C* Output parameters:							*
C*	LEVARR1 (NLEV)	INTEGER		Levels found			*
C*	LEVARR2	(NLEV)	INTEGER		Levels found			*
C*	NLEV		INTEGER		Number of levels found		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = read/write error		*
C**									*
C* Log:									*
C* R. Tian/SAIC		 3/06						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdattm1, gdattm2
	INTEGER		levarr1(*), levarr2(*)
C*
	CHARACTER	gdattm (2)*20
	INTEGER		levarr ( 2, LLMXLV )
C-----------------------------------------------------------------------
	gdattm (1) = gdattm1
	gdattm (2) = gdattm2
	CALL GD_GLEV ( iacss, gdattm, ivcord, maxlev, levarr,
     +							nlev, iret )

	DO i = 1, nlev
	    levarr1 (i) = levarr ( 1, i )
	    levarr2 (i) = levarr ( 2, i )
	END DO

	RETURN
	END
