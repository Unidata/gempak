	SUBROUTINE ND_GANL  ( anlyss, rnvblk, anlblk, iret )
C************************************************************************
C* ND_GANL								*
C*									*
C* This subroutine takes the user input for ANLYSS (the analysis 	*
C* information) and makes a grid analysis block.  The grid navigation	*
C* is first set in GR_SNAV.						*
C*									*
C* ND_GANL  ( ANLYSS, RNVBLK, ANLBLK, IRET )				*
C*									*
C* Input parameters:							*
C*	ANLYSS		CHAR*		User input for ANLYSS		*
C*      RNVBLK (*)      REAL		Grid navigation block		*
C*									*
C* Output parameters:							*
C*	ANLBLK (18)	REAL		Grid analysis block		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*                                       -4 = invalid navigation	*
C*					-10 = invalid extend region	*
C**									*
C* Log:									*
C* S. Jacobs/EAI	 7/93		Copied from GDCANL		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	anlyss
	REAL		rnvblk (*), anlblk (*)
C*
	REAL		dbnds  (4)
	INTEGER		iebnds (4)
	CHARACTER       stusr (2)*48
C------------------------------------------------------------------------
	iret = 0
C
C*      Separate user input into two substrings.
C
	CALL ST_CLST ( anlyss, '/', ' ', 2, stusr, n, ier )
C
C*	Extract DELTAN from the first substring.
C
	CALL ST_CRNM ( stusr (1), deltan, ier )
C
C*	Extract extend region from the second substring.
C
	CALL ST_ILST ( stusr (2), ';', 2, 4, iebnds, n, ier )
C
C*	Set default data area to missing.
C
	DO  i = 1, 4
	    dbnds (i) = RMISSD
	END DO
C
C*	Make an analysis block.
C
	CALL GR_MBN2  ( deltan, iebnds, dbnds, rnvblk, anlblk, ier )
C*
	RETURN
	END
