	SUBROUTINE SFLFIL  ( sffile, sffcur, iflno, newfil, iflsrc,
     +			     prmdst, npmdst, iret )
C************************************************************************
C* SFLFIL								*
C*									*
C* This subroutine opens the surface file for SFLIST.			*
C*									*
C* SFLFIL  ( SFFILE, SFFCUR, IFLNO, NEWFIL, IFLSRC, PRMDST, NPMDST, 	*
C*	     IRET )							*
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
C*	IFLSRC		INTEGER		Data file source		*
C*	PRMDST (NPMDST)	CHAR*		Data set parameters		*
C*	NPMDST		INTEGER		Number of parameters 		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -1 = file not open		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	 6/88	Rewritten				*
C* D. Keiser/GSC	 4/96	Added IFLSRC to calling sequence	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sffile, sffcur, prmdst (*)
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
C
C*	    Set PC package.
C
	    CALL PC_INIT  ( 0, npmdst, prmdst, ier )
	    IF  ( ier .ne. 0 )  THEN
                CALL ER_WMSG  ( 'PC',  ier, ' ', ierr )
		iret   = -1
		CALL SF_CLOS  ( iflno, ier )
		iflno  = 0
		sffcur = ' '
	    END IF
	END IF
C*
	RETURN
	END
