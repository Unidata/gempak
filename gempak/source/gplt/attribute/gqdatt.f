	SUBROUTINE GQDATT ( iunit, filnam, itype, xsize, ysize,
     +			    ncurwn, iret )
C************************************************************************
C* GQDATT								*
C* 									*
C* This subroutine queries the device attributes.			*
C* 									*
C* GQDATT ( IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN, IRET )		*
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
C* S. Jacobs/NCEP	 5/96	Copied from GSDATT			*
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	filnam
C------------------------------------------------------------------------
C*	Check that device has been set.
C
	IF  ( ddev .ne. ' ' )  THEN
	    iret = NORMAL
	  ELSE
	    iret = NDVICE
	    RETURN
	END IF
C
C*	Query the device for the attribute information.
C
	CALL DQDATT ( iunit, filnam, itype, xsize, ysize, ncurwn, iret )
	ncurwn = ncurwn + 1
C*
	RETURN
	END
