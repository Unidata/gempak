	SUBROUTINE HQDATT  ( iunit, filnam, itype, xsize, ysize,
     +			     ncurwn, iret )
C************************************************************************
C* HQDATT - UTF								*
C* 									*
C* This subroutine queries the device attributes.			*
C* 									*
C* HQDATT  ( IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN, IRET )		*
C*									*
C* Output parameters:							*
C*	IUNIT		INTEGER		Type of output device		*
C*	FILNAM		CHAR*		Name of file to use as output	*
C*      ITYPE           INTEGER         Output file format		*
C*					     2 = NAFOS format		*
C*					   <>2 = AFOS format		*
C*      XSIZE           REAL            X size in pixels		*
C*      YSIZE           REAL            Y size in pixels		*
C*	NCURWN		INTEGER		Current window number		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Safford/GSC	11/96	Initial coding       		 	*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 8/97	Increased fname from 72 to 160 chars	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER*(*)	filnam
C*
	CHARACTER	fname*160
C------------------------------------------------------------------------
	iret   = NORMAL
C
C*	Query the device attributes.
C
	CALL UQDATT ( iunit, fname, lenf, itype, xsize, ysize,
     +		      ncurwn, iret )
	filnam = fname (1:lenf)
C*
	RETURN
	END
