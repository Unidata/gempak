	SUBROUTINE SFXFIL  ( sffile, sffcur, iflno, newfil, iret )
C************************************************************************
C* SFXFIL								*
C*									*
C* This subroutine opens the surface file for SFGRAM.			*
C*									*
C* SFXFIL  ( SFFILE, SFFCUR, IFLNO, NEWFIL, IRET )			*
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
C*					-14 = file not open		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 6/89						*
C* M. desJardins/GSFC	 4/90	GEMPAK 5				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sffile, sffcur
	LOGICAL		newfil
C*
	CHARACTER	prmdst (MMPARM)*4
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
	CALL SF_OPNF  ( sffile, .false., iflno, isrc, npmdst, prmdst,
     +			ier )
C
C*	Check for error.
C
	IF  ( ier .ne. 0 )  THEN
	    iret   = -14
	  ELSE
	    sffcur = sffile
	    newfil = .true.
C
C*	    Set PC package.
C
	    CALL PC_INIT  ( 0, npmdst, prmdst, ier )
	END IF
C*
	RETURN
	END
