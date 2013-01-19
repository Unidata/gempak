	SUBROUTINE DSDATT ( iunit, filnam, itype, xsize, ysize,
     +			    ncurwn, iret )
C************************************************************************
C* DSDATT								*
C* 									*
C* This subroutine defines the size of the output device.		*
C*									*
C* DSDATT ( IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN, IRET )		*
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
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Chang/EAI	 	12/93						*
C* S. Jacobs/NMC	 3/94	Renamed DSHCAT to DSDATT		*
C* S. Jacobs/NCEP	 2/96	Added filnam to the calling sequence	*
C* S. Jacobs/NCEP	 4/96	Added iunit to the calling sequence	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	filnam
C
	CHARACTER	fil*72
	INTEGER		isend (22)
	REAL		rsend (2)
C------------------------------------------------------------------------
C
C*	Convert text strings into integer arrays.
C
	fil = filnam (1:72)
	CALL ST_STOI ( fil, 72, nv, isend (4), ier )
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend ( 1) = 24
	isend ( 2) = CSDATT
	isend ( 3) = iunit
	isend (22) = itype
C
	CALL GPUT ( isend, 22, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	rsend (1) = xsize
	rsend (2) = ysize
C
	CALL GPUTR ( rsend, 2, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) THEN
	    iret = ier
	    RETURN
	END IF
C
	CALL GGET ( ncurwn, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C*
	RETURN
	END
