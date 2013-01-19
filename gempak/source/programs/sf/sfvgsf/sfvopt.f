	SUBROUTINE SFVOPT  ( vgfile, sffile, dattim, parms, icolr,
     +			     nparm, iret )
C************************************************************************
C* SFVOPT								*
C*									*
C* This subroutine allows the user to exit from SFVGSF.			*
C*									*
C* SFVOPT  ( VGFILE, SFFILE, DATTIM, PARMS, ICOLR, NPARM, IRET )	*
C*									*
C* Input parameters:							*
C*	VGFILE		CHAR*		Vector graphics file name	*
C*	SFFILE		CHAR*		Output surface file name	*
C*	DATTIM		CHAR*		Date/time for the data		*
C*	PARMS  (NPARM)	CHAR*		Parameters list			*
C*	ICOLR  (NPARM)	INTEGER		Color list			*
C*	NPARM		INTEGER		Number of parameters		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C** Log:								*
C* S. Jacobs/NCEP	 2/99						*
C************************************************************************
	CHARACTER*(*)	vgfile, sffile, dattim, parms (*)
	INTEGER		icolr (*)
C------------------------------------------------------------------------
	iret = 0
C*
	WRITE  ( 6, 1000 )  vgfile, sffile, dattim
1000	FORMAT ( / ' SFVGSF PARAMETERS: ' //
     +             ' Vector graphics file: ', A, /
     +             ' Output surface file:  ', A, /
     +             ' Date/time:            ', A, /
     +             ' Parameters to be added to file (Parm color):' )
C*
	WRITE  ( 6, 1001 ) ( parms (i), icolr(i), i = 1, nparm )
1001	FORMAT ( 10X, A, 4X, '(', I3, ')' )
C*
	CALL TM_ACCP  ( ier )
	IF  ( ier .eq. 2 )  THEN
	    iret = 2
	END IF
C*
	RETURN
	END
