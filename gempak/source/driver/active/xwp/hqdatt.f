	SUBROUTINE HQDATT ( iunit, filnam, itype, xsize, ysize,
     +			    ncurwn, iret )
C************************************************************************
C* HQDATT - XWP								*
C* 									*
C* This subroutine queries the device attributes.			*
C* 									*
C* HQDATT ( IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN, IRET )		*
C*									*
C* Output parameters:							*
C*	IUNIT		INTEGER		Type of output device		*
C*					  For XW:			*
C*					    1 = GEMPAK window		*
C*					    2 = Motif window		*
C*	FILNAM		CHAR*		Name of output window or file	*
C*	ITYPE		INTEGER		Device type (color,bw,etc)	*
C*	XSIZE		REAL		X size in inches or pixels	*
C*	YSIZE		REAL		Y size in inches or pixels	*
C*	NCURWN		INTEGER		Current window number		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 5/96	Copied from HSDATT			*
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	filnam
C*
	CHARACTER	fname*72
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C*	Query the device attributes.
C
	filnam = ' '
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
	    CALL XQDATT ( iunit, fname, lenf, itype, xsize, ysize,
     +			  ncurwn, iret )
	  ELSE IF  ( ddev .eq. 'PS' )  THEN
	    CALL PQDATT ( iunit, fname, lenf, itype, xsize, ysize,
     +			  ncurwn, iret )
	END IF
	filnam = fname (1:lenf)
C*
	RETURN
	END
