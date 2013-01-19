	SUBROUTINE HSDATT ( iunit, filnam, itype, xsize, ysize,
     +			    ncurwn, iret )
C************************************************************************
C* HSDATT - GF								*
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
C* T. Piper/SAIC	02/08	New for GF				*
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	filnam
C------------------------------------------------------------------------
	iret = NORMAL

C
C*  Check the requested device.
C
	CALL ST_LSTR ( filnam, lenf, ier )

C
C*  Set the GF device attributes.
C
	CALL GFDATT ( iunit, filnam, lenf, itype, xsize, ysize,
     +                    ixsize, iysize, isxsiz, isysiz, ixoff,
     +                    iyoff, ncurwn, iret )

C
C*  Flush the GF graphics to the window.
C
	CALL GFFLSH ( .true., ier )

C
C*  Update device attributes in /DEVCHR/.
C
	iright = ixsize
	ibot   = iysize
	iswdth = isxsiz
	ishght = isysiz
	isxoff = ixoff
	isyoff = iyoff
C
	RETURN
	END
