	SUBROUTINE SNHFIL  ( snfile, snfcur, isnfln, newfil, iret )
C************************************************************************
C* SNHFIL								*
C*									*
C* This subroutine opens the sounding file for SNHODO.			*
C*									*
C* SNHFIL  ( SNFILE, SNFCUR, ISNFLN, NEWFIL, IRET )			*
C*									*
C* Input parameters:							*
C*	SNFILE		CHAR*		Sounding file name		*
C*									*
C* Input and output parameters:						*
C*	SNFCUR		CHAR*		Current sounding file		*
C*	ISNFLN		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	NEWFIL		LOGICAL		New file flag			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = file not open		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 8/89	Copy of SNPFIL				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snfile, snfcur
	LOGICAL		newfil
C*
	CHARACTER	prmdst (MMPARM)*4
	LOGICAL		mrgdat
C----------------------------------------------------------------------
	iret = 0
C
C*	Check to see if file requested is current file.
C
	IF  ( ( snfile .eq. snfcur )  .and. ( snfile .ne. ' ' ) )  THEN
	    newfil = .false.
	    RETURN
	END IF
C
C*	Close already opened file and initialize parameters.
C
	IF  ( isnfln .gt. 0 )   CALL SN_CLOS  ( isnfln, ier )
	isnfln  = 0
	snfcur  = ' '
C
C*	Open new file.
C
	CALL SN_OPNF  ( snfile, .false., isnfln, isrc, npmdst, prmdst,
     +			ivert, mrgdat, ier )
C
C*	Check for error.
C
	IF  ( ier .ne. 0 )  THEN
	    iret   = -1
	  ELSE
	    snfcur = snfile
	    newfil = .true.
C
C*	    Set PC package.
C
	    CALL PC_INIT  ( ivert, npmdst, prmdst, ier )
	    IF  ( ier .ne. 0 )  THEN
                CALL ER_WMSG  ( 'PC',  ier, ' ', ierr )
		iret   = -1
		CALL SN_CLOS  ( isnfln, ier )
		isnfln  = 0
		snfcur = ' '
	    END IF
	END IF
C*
	RETURN
	END
