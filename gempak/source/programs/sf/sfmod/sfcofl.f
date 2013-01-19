	SUBROUTINE SFCOFL  ( sffile, sffcur, iflno, ntime, times, 
     +			     iret )
C************************************************************************
C* SFCOFL								*
C*									*
C* This subroutine processes the original surface file name.		*
C*									*
C* SFCOFL  ( SFFILE, SFFCUR, IFLNO, NTIME, TIMES, IRET )		*
C*									*
C* Input parameters:							*
C*	SFFILE		CHAR*		Input surface file name		*
C*									*
C* Input and output parameters:						*
C*	SFFCUR		CHAR*		Current surface file name	*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	NTIME		INTEGER		Number of times in data set	*
C*	TIMES (NTIME)	CHAR*		Data set times			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -1 = file open error		*
C**									*
C* Log:									*
C* I. Graffman/RDS    10/85						*
C* M. desJardins/GSFC	10/86	Added GEMPAK parameter names		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sffcur, sffile, times(*)
C*
	CHARACTER	prms (MMPARM)*4
C----------------------------------------------------------------------
	iret   = 0
C
C*	Check whether this file name has changed.
C
	IF  ( ( sffcur .eq. sffile ) .and. ( sffile .ne. ' ' ) )  THEN
	    RETURN
	END IF
C
C*	Close old file if there was one.
C
	IF  ( iflno .ne. 0 )  THEN
	    CALL SF_CLOS  ( iflno, ier )
	END IF
	iflno  = 0
	sffcur = ' '
C
C*	Open file and check for errors.
C
	CALL SF_OPNF  ( sffile, .false., iflno, isrc, nprm, prms, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Get times from the file.
C
	CALL SF_GTIM  ( iflno, LLMXTM, ntime, times, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'SF',  ier, sffile, ierr )
	    CALL SF_CLOS  ( iflno, ierr )
	    iret  = -1
	    iflno = 0
	    RETURN
	END IF
C
C*	Set up the PC subroutine library.
C
	CALL PC_INIT  ( 0, nprm, prms, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'PC',  ier, ' ', ierr )
	    CALL SF_CLOS  ( iflno, ierr )
	    iret  = -1
	    iflno = 0
	    RETURN
	END IF
C
C*	Save the file name as the current file.
C
	sffcur = sffile
C*
	RETURN
	END
