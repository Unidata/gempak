	SUBROUTINE SFCOPT  ( sffile, sfoutf, area, dattim, datout,
     +			     ntimes, sfparm, iret )
C************************************************************************
C* SFCOPT								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* SFCOPT  ( SFFILE, SFOUTF, AREA, DATTIM, DATOUT, NTIMES, SFPARM,      *
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	SFFILE		CHAR*		Input file name			*
C*	SFOUTF		CHAR*		Output file name		*
C*	AREA		CHAR*		Area name			*
C*	DATTIM (*)	CHAR*		Input date/times   		*
C*	DATOUT (*)	CHAR*		Output date/times		*
C*	NTIMES		INTEGER		Number of times                 *
C*	SFPARM		CHAR*		Output parameters		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = exit			*
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C* S. Schotz/GSC	 5/90	Get respnd locally from IP_RESP		*
C* A. Hardy/GSC		 1/99   Added output date and time              *
C* D. Kidwell/NCEP	 3/99   Added requested output parameters;      *
C*				changed time display                    *
C************************************************************************
	CHARACTER*(*)	sffile, sfoutf, area, dattim (*), datout (*),
     +		        sfparm
	LOGICAL		respnd
C------------------------------------------------------------------------
	iret = 0
C*
	WRITE  ( 6, 1000 ) sffile, sfoutf, area, sfparm,
     +			   ( dattim (i), datout (i), i = 1, ntimes )
1000	FORMAT ( / ' SFMOD PARAMETERS: ', // ,
     +             ' Input file:        ', A, /
     +             ' Output file:       ', A, /
     +             ' Area:              ', A, /
     +             ' Output parameters: ', A, /
     +             ' Input date/time:        Output date/time:  ', /
     +             ( 1X, A, 4X, A ) )
C
C*	Wait for user input.
C
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = -1
	END IF
C*
	RETURN
	END
