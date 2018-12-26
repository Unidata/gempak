	SUBROUTINE GSATMG4 ( imgnam, area, nav, ixlef, iytop, ixrit,
     +			    iybot, iret )
C************************************************************************
C* GSATMG4								*
C*									*
C* This subroutine defines unremapped GOES satellite navigation         *
C* given in a MCIDAS format.  The image file specified contains	        *
C* the required navigation information.					*
C*									*
C* GSATMG4 ( IMGNAM, AREA, NAV, IXLEF, IYTOP, IXRIT, IYBOT, IRET )	*
C*									*
C* Input parameters:							*
C*	IMGNAM		CHAR*256	Satellite image name		*
C*	AREA (64)	INTEGER		MCIDAS AREA header		*
C*	NAV (640)	INTEGER		MCIDAS NAV header		*
C*	IXLEF		INTEGER		Left image coordinate		*
C*	IYTOP		INTEGER		Top image coordinate		*
C*	IXRIT		INTEGER		Right image coordinate		*
C*	IYBOT		INTEGER		Bottom image coordinate		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	 8/94	Clone of GSATMC				*
C* J. Cowie/COMET	 2/95	Increased nav array for GVAR nav	*
C* J. Cowie/COMET	 5/95	Add image bounds to calling sequence	*
C* M. Linda/GSC		 3/96	Added check for GPLT buffer overflow	*
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C* S. Jacobs/NCEP	 6/00	Increased image name from 80 to 256 char*
C* S. Guan/NCEP          2/18   Modified to Himawari netcdf4            *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*) 	imgnam
	INTEGER		area (64), nav (640)
C
	INTEGER		isend (4), inam (64)
	CHARACTER	name*256
C------------------------------------------------------------------------
C
C*	Check if GPLT buffer will overflow.
C
	isnd = 2 + ( 64 + 64 + 640 + 4 )
	ircv = 1 + ( 1 )
	IF ( ( isnd + ircv ) .gt. IGBSIZ ) THEN
	    iret = NOBUFF
	    RETURN
	END IF
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = isnd
	isend (2) = FSATMG
C
	CALL GPUT ( isend, 2, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	name = imgnam
	CALL ST_STOI ( name, 256, nv, inam, iret )
	CALL GPUT ( inam, 64, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( area, 64, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( nav, 640, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	isend (1) = ixlef
	isend (2) = iytop
	isend (3) = ixrit
	isend (4) = iybot
C
	CALL GPUT ( isend, 4, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ierr )
	IF ( ierr .ne. NORMAL ) iret = ierr
C*
	RETURN
	END
