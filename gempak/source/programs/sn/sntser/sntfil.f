	SUBROUTINE SNTFIL  ( snfile, snfcur, isnfln, newfil, prmdst,
     +			     npmdst, ivert,  iret )
C************************************************************************
C* SNTFIL								*
C*									*
C* This subroutine opens the sounding file for SNTSER.			*
C*									*
C* SNTFIL  ( SNFILE, SNFCUR, ISNFLN, NEWFIL, PRMDST, NPMDST, IVERT,	*
C*           IRET )							*
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
C*	PRMDST (NPMDST)	CHAR*		Data set parameters		*
C*	NPMDST		INTEGER		Number of parameters 		*
C*	IVERT		INTEGER		Vertical coordinate		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					-14 = file not open		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 5/89	Adapted from SNLFIL			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snfile, snfcur, prmdst (*)
	LOGICAL		newfil, mrgflg
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
     +			ivert, mrgflg, ier )
C
C*	Check for error.
C
	IF  ( ier .ne. 0 )  THEN
	    iret   = -14
	  ELSE
	    snfcur = snfile
	    newfil = .true.
C
C*	    Set PC package.
C
	    CALL PC_INIT  ( ivert, npmdst, prmdst, ier )
	    IF  ( ier .ne. 0 )  THEN
                CALL ER_WMSG  ( 'PC',  ier, ' ', ierr )
		iret   = -14
		CALL SN_CLOS  ( isnfln, ier )
		isnfln  = 0
		snfcur = ' '
	    END IF
	END IF
C*
	RETURN
	END
