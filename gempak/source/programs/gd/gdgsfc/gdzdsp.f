	SUBROUTINE GDZDSP  ( gdfile, gdatim, gfunc, ivcord, level, 
     +			     iscale, sffile, sfparm, dattim, iret )
C************************************************************************
C* GDZDSP								*
C*									*
C* This subroutine displays the variables to be used in GDGSFC.  It	*
C* also allows the users to accept the parameters or exit the program.	*
C*									*
C* GDZDSP  ( GDFILE, GDATIM, GFUNC, IVCORD, LEVEL, ISCALE, SFFILE,	*
C*	     SFPARM, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file			*
C*	GDATIM(2)	CHAR*		Grid time			*
C*	GFUNC		CHAR*		Grid function			*
C*	IVCORD		INTEGER		Grid coordinate			*
C*	LEVEL(2)	INTEGER		Grid level			*
C*	ISCALE		INTEGER		Scaling factor			*
C*	SFFILE		CHAR*		Surface file			*
C*	SFPARM		CHAR*		Surface parameter		*
C*	DATTIM		CHAR*		Data time			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -1 = user typed EXIT		*
C**									*
C* Log:									*
C* T. Lee/GSC		11/99						*
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim (2), gfunc, sffile, sfparm, 
     +			dattim
	INTEGER		level (2)
	LOGICAL		respnd
C------------------------------------------------------------------------
	iret = 0
C
C*      Write out the grid file name.
C
	WRITE  ( 6, 1000 ) gdfile
1000	FORMAT ( / ' Grid file: ', A )
C
C*	Write out the grid identifier.
C
	WRITE  ( 6, 2000 )
2000	FORMAT ( ' GRID IDENTIFIER: ' )
	CALL GR_WTRM  ( 6, .true., 0, gdatim, level, ivcord, gfunc,
     +			ier )
C
C*	Write out the surface file and parameter names.
C
	WRITE ( 6, 3000) iscale
3000	FORMAT ( /, 50X, ' SCALE:', I3 )
	CALL ST_LCUC  ( sffile, sffile, ier )
	WRITE  ( 6, 4000 ) sffile, dattim, sfparm
4000	FORMAT ( / ' File:              ', A,/
     +             ' Time:              ', A,//
     +             ' Parameter:         ', A )
C
C*	Prompt user.
C
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = -1
	END IF
C*
	RETURN
	END
