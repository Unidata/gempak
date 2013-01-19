	SUBROUTINE LW_INPT ( lunf, iword, numwds, iarray, iret ) 
C************************************************************************
C* LW_INPT								*
C* 									*
C* This subroutine reads from a lw-format file.  The number of words to *
C* read and the starting location in the file (where the first word is  *
C* assumed to be at location 0) must be given.                          *
C* 									*
C* LW_INPT  ( LUNF, IWORD, NUMWDS, IARRAY, IRET )			*
C* 									*
C* Input parameters:							*
C*	LUNF		INTEGER		Logical unit number		*
C*	IWORD		INTEGER		Record number (counting from 0)	*
C*	NUMWDS		INTEGER		Number of words to read         *
C* 									*
C* Output parameters:							*
C*	IARRAY (NUMWDS)	INTEGER		Data record			*
C*	IRET		INTEGER		Return code			*
C*				  	  0 = normal return		*
C*				  	 -4 = cannot read file		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	12/99	                      			*
C* D. Kidwell/NCEP	 2/00	Corrected missing value parameter 	*
C* S. Jacobs/NCEP	 2/01	Added check for LINUX machine type	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		iarray (*)
C*
	PARAMETER	( IMSG = -2000000 )
C-----------------------------------------------------------------------
	iret = 0
C
	DO ii = 1, numwds
	    CALL FL_READ ( lunf, iword + ii, 1, iarray (ii), iret ) 
	END DO
C
C*	Swap bytes if necessary.
C
	IF ( ( MTMACH .eq. MTULTX ) .or. ( MTMACH .eq. MTALPH ) .or.
     +	     ( MTMACH .eq. MTLNUX ) ) THEN
	    ier = MV_SWP4 ( numwds, iarray, iarray )
	END IF
C    
	DO ii = 1, numwds
	    IF ( iarray ( ii ) .lt. IMSG )  iarray ( ii ) = IMISSD
	END DO
C*
	RETURN
	END
