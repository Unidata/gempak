	SUBROUTINE SNPFIL  ( snfile, snfcur, isnfln, newfil, npmdst,
     +                       prmdst, iret )
C************************************************************************
C* SNPFIL								*
C*									*
C* This subroutine opens the sounding file for SNPROF.			*
C*									*
C* SNPFIL  ( SNFILE, SNFCUR, ISNFLN, NEWFIL, IRET )			*
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
C*      NPMDST		INTEGER		Number of params		*
C*	CPARMS		CHAR*           Params(MMPARM)*4		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = file not open		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	10/88	Rewritten				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snfile, snfcur
	LOGICAL		newfil
C*
	CHARACTER	prmdst (MMPARM)*4
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
	IF  ( isnfln .gt. 0 )   CALL SF_CLOS  ( isnfln, ier )
	isnfln  = 0
	snfcur  = ' '
C
C*	Open new file.
C
	CALL SF_OPNF  ( snfile, .false., isnfln, isrc, npmdst, prmdst,
     +			ier )
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
            CALL PC_INIT(1,npmdst, prmdst,ier)
C
	END IF
C*
	RETURN
	END
