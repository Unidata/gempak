	SUBROUTINE SFMFIL  ( sffile, sffcur, iflno, newfil, parms, 
     +			     nparm,  iret )
C************************************************************************
C* SFMFIL								*
C*									*
C* This subroutine processes a surface file name.			*
C*									*
C* SFMFIL  ( SFFILE, SFFCUR, IFLNO, NEWFIL, PARMS, NPARM, IRET )	*
C*									*
C* Input parameters:							*
C*	SFFILE		CHAR*		Input surface file name		*
C*									*
C* Input and output parameters:						*
C*	SFFCUR		CHAR*		Current surface file name	*
C*	IFLNO		INTEGER		Surfact file number		*
C*									*
C* Output parameters:							*
C*	NEWFIL		LOGICAL		New file flag			*
C*	PARMS (NPARM)	CHAR*		Data set parameters		*
C*	NPARM		INTEGER		Number of parameters		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -6 = file open error		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87						*
C* M. desJardins/GSFC	 6/88	Reorganized				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)    sffile, sffcur, parms (*)
	LOGICAL          newfil
C----------------------------------------------------------------------
	iret = 0
C
C*	Check to see if file requested is current file.
C
	IF  ( ( sffcur .eq. sffile ) .and. ( sffile .ne. ' ' ) )  THEN
	    newfil = .false.
	    RETURN
	END IF
C
C*	Close already opened file.
C
	IF  ( iflno .gt. 0 )  CALL SF_CLOS  ( iflno, ier )
C
C*	Open new file and check for error.
C
	CALL SF_OPNF ( sffile, .false., iflno, isrc, nparm, parms, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret   = -6
	    sffcur = ' '
	  ELSE
	    sffcur = sffile
	    newfil = .true.
C
C*	    Set PC package.
C
	    CALL PC_INIT  ( 0, nparm, parms, ier )
	    IF  ( ier .ne. 0 )  THEN
                CALL ER_WMSG  ( 'PC',  ier, ' ', ierr )
	        iret = -6
	    END IF
	END IF
C*
	RETURN
	END
