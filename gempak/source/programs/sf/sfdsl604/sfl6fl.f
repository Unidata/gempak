	SUBROUTINE SFL6FL  ( file, sffile, iflno, newfil, iret )
C************************************************************************
C* SFL6FL								*
C*									*
C* This subroutine processes a surface file name.			*
C*									*
C* SFL6FL  ( FILE, SFFILE, IFLNO, NEWFIL, IRET )			*
C*									*
C* Input parameters:							*
C*	FILE		CHAR*		Input surface file 		*
C*									*
C* Input and output parameters:						*
C*	SFFILE		CHAR*		Current surface file 		*
C*	IFLNO		INTEGER		File number 			*
C*									*
C* Output parameters:							*
C*	NEWFIL		LOGICAL		New file flag			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = file open error		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	 6/88	Changed calling sequence		*
C* M. desJardins/GSFC	11/89	Documentation				*
C* K. Tyle/GSC		 1/97	Call ER_LMSG				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*) 	sffile, file
	LOGICAL		newfil
C*
	CHARACTER	prms (MMPARM)*4
C------------------------------------------------------------------------
	iret = 0
C
C*	Check to see if file requested is current file.
C
	IF  ( ( file .eq. sffile ) .and. ( file .ne. ' ' ) )  THEN
	    newfil = .false.
	    RETURN
	END IF
C
C*	Close already opened file.
C
	IF  ( iflno .gt. 0 )  CALL SF_CLOS ( iflno, ier )
C
C*	Open new file.
C
	CALL SF_OPNF  ( file, .false., iflno, isrc, nprm, prms, ier )
C
C*	Check for error.
C
	IF  ( ier .ne. 0 )  THEN
	    iret   = -1
	    sffile = ' '
	  ELSE
	    sffile = file
	    newfil = .true.
C
C*	    Set PC package.
C
	    CALL PC_INIT  ( 0, nprm, prms, ier )
	    IF  ( ier .ne. 0 )  THEN
                CALL ER_LMSG  ( 0, 'PC',  ier, ' ', ierr )
	        iret = -1
	    END IF
	END IF
C*
	RETURN
	END
