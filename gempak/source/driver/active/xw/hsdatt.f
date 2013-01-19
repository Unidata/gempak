	SUBROUTINE HSDATT ( iunit, filnam, itype, xsize, ysize,
     +			    ncurwn, iret )
C************************************************************************
C* HSDATT - XW								*
C* 									*
C* This subroutine defines the device attributes.			*
C* 									*
C* HSDATT ( IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN, IRET )		*
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
C* S. Jacobs/NCEP	 3/96	Added filnam to the calling sequence	*
C* S. Jacobs/NCEP	 4/96	Added iunit to the calling sequence	*
C* C. Lin/EAI	         6/97	Added 'S' coordinates			*
C*				Changed calling sequence to XSDATT      *
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	filnam
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Set the device attributes.
C
	CALL ST_LSTR ( filnam, lenf, ier )
	CALL XSDATT  ( iunit, filnam, lenf, itype, xsize, ysize,
     +		       ixsize, iysize, isxsiz, isysiz, ixoff, iyoff, 
     +		       ncurwn, iret )
C
C*	Udate device attributes in /DEVCHR/.
C
	iright = ixsize
	ibot   = iysize
        iswdth = isxsiz
        ishght = isysiz
        isxoff = ixoff
        isyoff = iyoff

C
C*	Flush the graphics to the window and pop the window.
C
	CALL XXFLSH ( .true., ier )
C*
	RETURN
	END
