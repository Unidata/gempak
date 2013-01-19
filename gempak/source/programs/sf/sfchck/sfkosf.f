	SUBROUTINE SFKOSF  ( sffile, sffcur, iflno, newfil, iret )
C************************************************************************
C* SFKOSF								*
C*									*
C* This subroutine opens the surface file for SFCHCK.			*
C*									*
C* SFKOSF  ( SFFILE, SFFCUR, IFLNO, NEWFIL, IRET )			*
C*									*
C* Input parameters:							*
C*	SFFILE		CHAR*		Surface file name		*
C*									*
C* Input and output parameters:						*
C*	SFFCUR		CHAR*		Current surface file 		*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	NEWFIL		LOGICAL		New file flag			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -1 = file not open		*
C**									*
C* Log:									*
C* K. Tyle/GSC		 4/97	Based on SFLFIL				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sffile, sffcur
	CHARACTER*4	prmdst (MMPARM)
	LOGICAL		newfil
C----------------------------------------------------------------------
	iret = 0
C
C*	Check to see if file requested is current file.
C
	IF  ( ( sffile .eq. sffcur )  .and. ( sffile .ne. ' ' ) )  THEN
	    newfil = .false.
	    RETURN
	END IF
C
C*	Close already opened file and initialize parameters.
C
	IF  ( iflno .gt. 0 )   CALL SF_CLOS  ( iflno, ier )
	iflno  = 0
	sffcur = ' '
C
C*	Open new file.
C
	CALL SF_OPNF  ( sffile, .false., iflno, iflsrc, npmdst, prmdst,
     +			ier )
C
C*	Check for error.
C
	IF  ( ier .ne. 0 )  THEN
	    iret   = -1
	  ELSE
	    sffcur = sffile
	    newfil = .true.
	END IF
C*
	RETURN
	END
