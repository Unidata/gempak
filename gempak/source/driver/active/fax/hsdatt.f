	SUBROUTINE HSDATT  ( iunit, filnam, itype, xsize, ysize, 
     +			     ncurwn, iret )
C************************************************************************
C* HSDATT - FAX								*
C* 									*
C* This subroutine defines the device attributes.			*
C* 									*
C* HSDATT  ( IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN, IRET )		*
C*									*
C* Input parameters:							*
C*	IUNIT		INTEGER		Unit id				*
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
C* E. Wehner/EAi	 7/96	Adopted to call raster C routine	*
C* E. Wehner/EAi	 1/97	Added iunit				*
C* C. Lin/EAI            6/97	Set 'S' coordinates for FAX      	*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 7/97	Changed ST_LSTR to ST_NULL		*
C* T. Piper/SAIC	02/04	Calling sequence change to rsdatt	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER*(*)	filnam
C------------------------------------------------------------------------
	iret   = NORMAL
	ncurwn = 0
C*
	CALL ST_NULL ( filnam, filnam, lenf, ier )
	CALL RSDATT ( filnam, xsize, ysize, ileft, ibot, 
     +			iright, itop, nncolr, iret )
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
