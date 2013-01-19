	SUBROUTINE SNOINF  ( snfile, snfcur, isnfln, newfil, prmdst,
     +			     npmdst, ivert, mrgdat, iret )
C************************************************************************
C* SNOINF								*
C*									*
C* This subroutine opens the sounding file for SNMOD.			*
C*									*
C* SNOINF  ( SNFILE, SNFCUR, ISNFLN, NEWFIL, PRMDST, NPMDST, IVERT,	*
C*           MRGDAT, IRET )						*
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
C*      MRGDAT		LOGICAL         Merge flag			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -1 = file not open		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	10/88	Rewritten				*
C* S. Schotz/GSC	12/89   Pass back merge flag			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snfile, snfcur, prmdst (*)
	LOGICAL		newfil
C*
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
