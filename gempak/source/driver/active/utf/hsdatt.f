	SUBROUTINE HSDATT  ( iunit, filnam, itype, xsize, ysize,
     +			     ncurwn, iret )
C************************************************************************
C* HSDATT - UTF								*
C* 									*
C* This subroutine defines the device attributes.			*
C* 									*
C* HSDATT  ( IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN, IRET )		*
C*									*
C* Input parameters:							*
C*	IUNIT		INTEGER		Type of output device		*
C*					  (not used for UTF)		*
C*	FILNAM		CHAR*		Name of file to use as output	*
C*      ITYPE           INTEGER         Output file format		*
C*					     2 = NAFOS format		*
C*					   <>2 = AFOS format		*
C*      XSIZE           REAL            X size in pixels		*
C*      YSIZE           REAL            Y size in pixels		*
C*                                                                      *
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C*					  (not used for UTF)		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Safford/GSC	11/96	Initial coding				*
C* C. Lin/EAI            6/97   Set 'S' coordinates 			*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 8/97	Changed ST_LSTR to ST_NULL		*
C* S. Jacobs/NCEP	 8/97	Added base time to USDATT call		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER*(*)	filnam
C*
	CHARACTER	carr(2)*160, fname*160
C------------------------------------------------------------------------
	iret   = NORMAL
	ncurwn = 0
C*
	CALL ST_CLST ( filnam, ';', ' ', 2, carr, num, ierr )
	CALL ST_NULL ( carr(1), fname, lenf, ier )
	CALL ST_NUMB ( carr(2), ibase, ier )
	CALL USDATT ( iunit, fname, lenf, itype, ibase, xsize, ysize,
     +		      ileft, ibot, iright, itop, nncolr, iret )
        isxoff  = 0
        isyoff  = 0
        iswdth  = ABS ( iright - ileft )
        ishght  = ABS ( itop - ibot )
C*
	RETURN
	END
