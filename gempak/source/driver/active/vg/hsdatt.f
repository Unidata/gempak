	SUBROUTINE HSDATT  ( iunit, filnam, itype, xsize, ysize,
     +			     ncurwn, iret )
C************************************************************************
C* HSDATT - VG 								*
C* 									*
C* This subroutine defines the device attributes.			*
C* 									*
C* HSDATT  ( IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN, IRET )		*
C*									*
C* Input parameters:							*
C*	IUNIT		INTEGER		Type of output device		*
C*					  For XW:			*
C*					    1 = GEMPAK window		*
C*					    2 = Motif window		*
C*	FILNAM		CHAR*		Name of file to use as output	*
C*      ITYPE           INTEGER         Device type (color,bw,etc)      *
C*      XSIZE           REAL            X size in inches or pixels      *
C*      YSIZE           REAL            Y size in inches or pixels      *
C*                                                                      *
C* Output parameters:							*
C*	NCURWN		INTEGER		Always 0			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/97						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* T. Piper/SAIC	02/04	Calling sequence change for vsdatt	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER*(*)	filnam
C*
	CHARACTER	fnm*132
C------------------------------------------------------------------------
	iret   = NORMAL
	ncurwn = 0
C*
	CALL ST_NULL ( filnam, fnm, lenf, ier )
	CALL VSDATT ( iunit, fnm, itype, iret )
C*
	RETURN
	END
