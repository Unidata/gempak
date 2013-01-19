	SUBROUTINE DSDATT ( iunit, filnam, itype, xsize, ysize,
     +			    ncurwn, iret )
C************************************************************************
C* DSDATT								*
C* 									*
C* This subroutine defines the device attributes.			*
C* 									*
C* DSDATT ( IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN, IRET )		*
C*									*
C* Input parameters:							*
C*	IUNIT		INTEGER		Type of output device		*
C*					  For XW:			*
C*					    1 = GEMPAK window		*
C*					    2 = Motif window		*
C*	FILNAM		CHAR*		Name of output window or file	*
C*	ITYPE		INTEGER		Device type (color,bw,etc)	*
C*	XSIZE		REAL		X size in inches or pixels	*
C*	YSIZE		REAL		Y size in inches or pixels	*
C*									*
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Chang/EAI		12/93						*
C* S. Jacobs/NMC	 3/94	Renamed DSHCAT to DSDATT		*
C* S. Jacobs/NMC	 6/94	Added call to HSDATT			*
C* S. Jacobs/NCEP	 3/96	Added filnam to the calling sequence	*
C* S. Jacobs/NCEP	 4/96	Added iunit to the calling sequence	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	filnam
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Call driver to set the device attributes.
C
	CALL HSDATT ( iunit, filnam, itype, xsize, ysize, ncurwn, iret )
C*
	RETURN
	END
