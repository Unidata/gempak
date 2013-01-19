	SUBROUTINE DM_FKEY ( iflno, keynam, type, loc, iret )
C************************************************************************
C* DM_FKEY								*
C*									*
C* This subroutine finds the type and location of a row or column	*
C* key.  If the key is not found, the location is set to 0.		*
C*									*
C* DM_FKEY  ( IFLNO, KEYNAM, TYPE, LOC, IRET )				*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	KEYNAM		CHAR*4		Key name			*
C*									*
C* Output parameters:							*
C*	TYPE		CHAR*		Type : ROW or COL		*
C*	LOC		INTEGER		Key location			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file is not open		*
C*					-14 = invalid key name		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	keynam, type
C------------------------------------------------------------------------
	type = ' '
	loc  = 0
C
C*	Check that the file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
	iret = -14
C
C*	Check row keys for key name.
C
	DO  i = 1, krkeys (iflno)
	    IF ( keynam .eq. kkrow (i,iflno) ) THEN
		type = 'ROW'
		loc  = i
		iret = 0
		RETURN
	    END IF
	END DO
C
C*	Check column keys.
C
	DO  i = 1, kckeys (iflno)
	    IF ( keynam .eq. kkcol (i,iflno) ) THEN
		type = 'COL'
		loc  = i
		iret = 0
		RETURN
	    END IF
	END DO
C*
	RETURN
	END
