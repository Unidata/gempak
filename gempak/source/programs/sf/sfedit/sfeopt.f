	SUBROUTINE SFEOPT  ( sfefil, sffile, parms, nparm, 
     +			     iret )
C************************************************************************
C* SFEOPT								*
C*									*
C* This subroutine allows the user to exit from SFEDIT.			*
C*									*
C* SFEOPT  ( SFEFIL, SFFILE, PARMS, NPARM, IRET )			*
C*									*
C* Input parameters:							*
C*	SFEFIL		CHAR*		Edit file name			*
C*	SFFILE		CHAR*		Output surface file name	*
C*	PARMS  (NPARM)	CHAR*		Parameters in edit file		*
C*	NPARM		INTEGER		Number of parameters		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C** Log:								*
C* M. desJardins/GSFC	 6/88						*
C* S. Schotz/GSC	 6/90	Removed respnd flag			*
C************************************************************************
	CHARACTER*(*)	sfefil, sffile, parms (*)
C------------------------------------------------------------------------
	iret = 0
C*
	WRITE  ( 6, 1000 )  sfefil, sffile
1000	FORMAT ( / ' SFEDIT PARAMETERS: ' //
     +             ' Edit file:           ', A, /
     +             ' Output surface file: ', A, / )
C*
	WRITE  ( 6, 1001 ) ( parms (i), i = 1, nparm )
1001	FORMAT (   ' Parameters to be added to file:', //
     +             15 ( 1X, A ) )
C*
	CALL TM_ACCP  ( ier )
	IF  ( ier .eq. 2 )  THEN
	    iret = 2
	END IF
C*
	RETURN
	END
