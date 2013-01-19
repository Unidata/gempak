	SUBROUTINE HSDATT  ( iunit, filnam, itype, xsize, ysize,
     +			     ncurwn, iret )
C************************************************************************
C* HSDATT - PS								*
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
C* E. Wehner/EAi	 3/96	Modified to call "C" version, and pass 	*
C*				more parameters down			*
C* S. Jacobs/NCEP	 4/96	Added ileft,ibot,iright,itop,nncolr to	*
C*				call to PSDATT				*
C* S. Jacobs/NCEP	 4/96	Added iunit to calling sequence		*
C* S. Jacobs/NCEP	 5/96	Added iunit to PSDATT call		*
C* C. Lin/EAI           6/97    Set 'S' coordinates 			*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER*(*)	filnam
C------------------------------------------------------------------------
	iret   = NORMAL
	ncurwn = 0
C*
	CALL ST_LSTR ( filnam, lenf, ier )
	CALL PSDATT ( iunit, filnam, lenf, itype, xsize, ysize,
     +		      ileft, ibot, iright, itop, nncolr, iret )
        isxoff  = 0
        isyoff  = 0
        iswdth  = ABS ( iright - ileft )
        ishght  = ABS ( itop - ibot )
C
C*	Reset the color compute flag.
C
	colcmp = .true.
	IF  ( nncolr .eq. 1 )  colcmp = .false.
C*
	RETURN
	END
