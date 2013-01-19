	SUBROUTINE SFL6TM  ( ddd, newfil, dattim, iflno, ntime, 
     +			     times, iret )
C************************************************************************
C* SFL6TM								*
C*									*
C* This subroutine processes the date/time entered by the user.		*
C*									*
C* SFL6TM  ( DDD, NEWFIL, DATTIM, IFLNO, NTIME, TIMES, IRET )		*
C*									*
C* Input parameters:							*
C*	DDD		CHAR*		Input date/time			*
C*	NEWFIL		LOGICAL		New file flag			*
C*									*
C* Input and output parameters:						*
C*	DATTIM		CHAR*		Current date/time		*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	NTIME		INTEGER		Number of times 		*
C*	TIMES (*)	CHAR*		Times to process		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -1 = error getting time	*
C**									*
C* Log:									*
C* I. Graffman/RDS	GEMPAK4						*
C* S. Schotz/GSC	 5/90	Removed respnd flag			*
C* K. Tyle/GSC		 1/97	Call ER_LMSG				*
C* T. Piper/SAIC	 4/02	Fixed UMR; set size timdst & tout to 20	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)   dattim, ddd, times (*)
	LOGICAL         newfil
	CHARACTER	timdst (LLMXTM)*20, tout*20
C-------------------------------------------------------------------------
	IF  ( ( ddd .ne. dattim ) .or. ( ddd .eq. ' ' ) .or.
     +	      ( ddd .eq. 'LIST' ) .or. ( newfil ) )  THEN
C
C*	    Get times from file.
C
	    CALL SF_GTIM  ( iflno, LLMXTM, ntimf, timdst, iret )
	    IF  ( iret .ne. 0 )  THEN
		CALL ER_LMSG  ( 0, 'SF', iret, ' ', ier )
		iret = -1
		RETURN
	    END IF
C
C*	    Find the time.
C
	    CALL TI_FIND  ( ddd, ntimf, timdst, tout, ntime, 
     +			    times, ier )
	    IF  ( ier .eq. 0 ) THEN
		dattim = ddd
	      ELSE 
		iret = -1
		dattim = ' '
	    END IF
	END IF
C
	RETURN
	END
