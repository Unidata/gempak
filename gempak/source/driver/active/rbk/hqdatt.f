	SUBROUTINE HQDATT  ( iunit, filnam, itype, xsize, ysize,
     +			     ncurwn, iret )
C************************************************************************
C* HQDATT - RBK 						        *
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
C* A. Hardy/GSC		9/98		Modified from utf's HQDATT      *
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
	CALL AQDATT ( iunit, fname, lenf, itype, xsize, ysize,
     +		      ncurwn, iret )
	filnam = fname (1:lenf)
C*
	RETURN
	END
