	SUBROUTINE HSDATT  ( iunit, filnam, itype, xsize, ysize,
     +			     ncurwn, iret )
C************************************************************************
C* HSDATT - NC								*
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
C*	FILNAM		CHAR*		Name of output window or file	*
C*      ITYPE           INTEGER         Device type (color,bw,etc)      *
C*      XSIZE           REAL            X size in inches or pixels      *
C*      YSIZE           REAL            Y size in inches or pixels      *
C*                                                                      *
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 2/96	Added filnam to the calling sequence	*
C* S. Jacobs/NCEP	 4/96	Added iunit to the calling sequence	*
C* S. Jacobs/NCEP	 5/96	Added iunit to MSDATT call		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* R. Tian/SAIC          4/02   Added init of 'S' coord                 *
C************************************************************************
	INCLUDE		'ERROR.PRM'
        INCLUDE         'DEVCHR.CMN'
C*
	CHARACTER*(*)	filnam

C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Set the device attributes.
C
	CALL ST_LSTR ( filnam, lenf, ier )
	CALL MSDATT ( iunit, filnam, lenf, itype, xsize, ysize,
     +                ileft, ibot, iright, itop, iret )
C
C*      isxoff = x offset of the 'S' coord relative to 'D' coord
C*      isyoff = y offset of the 'S' coord relative to 'D' coord
C*      iswdht = width of the 'S' coord system
C*      ishght = height of the 'S' coord system
C
        isxoff  = 0
        isyoff  = 0
        iswdth  = iright - ileft
        ishght  = itop - ibot
C*
	RETURN
	END
