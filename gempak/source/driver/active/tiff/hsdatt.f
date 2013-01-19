	SUBROUTINE HSDATT  ( iunit, filnam, itype, xsize, ysize, 
     +			     ncurwn, iret )
C************************************************************************
C* HSDATT - TIFF							*
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
C* S. Jacobs/NCEP	12/98						*
C* T. Piper/SAIC	02/04	Calling sequence change for tsdatt	*
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
	CALL TSDATT ( filnam, xsize, ysize, ileft,
     +		      ibot, iright, itop, nncolr, iret )
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
C*
C*      Do not change anything below this line.
C------------------------------------------------------------------------
	ispanx = ISIGN ( 1, ( iright - ileft ) )
	ispany = ISIGN ( 1, ( itop - ibot ) )
C
	CALL TSPAN ( ispanx, ispany, ier)
C*
	RETURN
	END
