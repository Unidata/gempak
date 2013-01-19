	SUBROUTINE GQSATN ( navtyp, imgnam, iret )
C************************************************************************
C* GQSATN								*
C*									*
C* This subroutine returns satellite navigation information.		*
C* The navigation type may be up to 8 characters long.  The name may	*
C* be up to 256 characters in length.					*
C*									*
C* GQSATN ( NAVTYP, IMGNAM, IRET )					*
C*									*
C* Output parameters:							*
C*	NAVTYP		CHAR*8		Satellite navigation type	*
C*	IMGNAM		CHAR*256	Satellite image name		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/85						*
C* S. Jacobs/NMC	 7/94	Removed AOI projection			*
C* M. Linda/GSC		12/95	Removed NPGS satellite navigation	*
C* S. Jacobs/NCEP	 6/00	Increased imgnam from 80 to 256 chars	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'SATDEF.CMN'
	INCLUDE		'XYDEF.CMN'
C*
	CHARACTER*(*) 	imgnam, navtyp
C------------------------------------------------------------------------
C*	Retrieve values from common.
C
	IF  ( igmode .ne. 1 ) THEN
	    iret = NIMODE
C
	ELSE IF ( .not. mset .or. ( mtype .ne. 3 ) ) THEN
	    iret = NSATNV
C
	ELSE
	    navtyp = nvtypa
	    imgnam = kmgnam
	    iret   = NORMAL
C
	END IF
C*
	RETURN
	END
