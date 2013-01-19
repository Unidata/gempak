	SUBROUTINE SI_GEMP  ( lunf, ihhmm, parms, nparm, drct, dtnm, 
     +                        iret )
C************************************************************************
C* SI_GEMP                                                            	*
C*                                                                      *
C* This subroutine writes a decoded SI report to an existing GEMPAK 	*
C* file.								*
C* 								        *
C* SI_GEMP  ( LUNF, IHHMM, CPRMS, PARMS, NPARM, NUMPRM, IMNEM, IRET )   *
C*								        *
C* Input parameters:						        *
C*	LUNF		INTEGER  	Surface file number             *
C*	IHHMM		INTEGER		Verification time (HHMM)        *
C*	PARMS (*)       CHAR*		Parameter list                  *
C*	NPARM		INTEGER		Number of parameters            *
C*	DRCT		REAL		Direction			*
C*	DTNM		REAL		Distance in nautical miles	*
C*								        *
C* Output parameters:						        *
C*	IRET		INTEGER		Return code                     *
C*					  0 = normal return             *
C**								        *
C* Log:							        	*
C* T. Lee/SAIC         8/02						*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	parms (*)
C*
	REAL		rdata (2)
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize the GEMPAK array first.
C
	DO ii = 1, nparm
	    rdata ( ii ) = RMISSD
	END DO
C
	IF  ( parms ( 1 ) .eq. 'DRCT' )  THEN
	    rdata ( 1 ) = drct
	    rdata ( 2 ) = dtnm
	  ELSE IF ( parms ( 1 ) .eq. 'DTNM' )  THEN
	    rdata ( 1 ) = dtnm
	    rdata ( 2 ) = drct
	  ELSE
	    CALL DC_WLOG ( 2, 'DCIDFT', -3, parms (1), ier )
	END IF
C
C*	Write the data to a surface data file.
C
	CALL SF_WDAT ( lunf, ihhmm, rdata, iret )
	IF ( iret .ne. 0 ) THEN
	    CALL DC_WLOG ( 2, 'SF', iret, ' ', ier )
	END IF
C*
	RETURN
	END
