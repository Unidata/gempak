	SUBROUTINE SNMFIL  ( snfile, snfcur, iflno, newfil, parms, 
     +			     nparm, ivert, iret )
C************************************************************************
C* SNMFIL								*
C*									*
C* This subroutine processes a sounding file name.			*
C*									*
C* SNMFIL  ( SNFILE, SNFCUR, IFLNO, NEWFIL, PARMS, NPARM, IVERT, 	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	SNFILE		CHAR*		Input sounding file name	*
C*									*
C* Input and output parameters:						*
C*	SNFCUR		CHAR*		Current sounding file name	*
C*	IFLNO		INTEGER		Sounding file number		*
C*									*
C* Output parameters:							*
C*	NEWFIL		LOGICAL		New file flag			*
C*	PARMS (NPARM)	CHAR*		Data set parameters		*
C*	NPARM		INTEGER		Number of parameters		*
C*	IVERT		INTEGER		Dataset vertical coordinate	*
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
	CHARACTER*(*)	snfile, snfcur, parms (*)
	LOGICAL		newfil
C*
C----------------------------------------------------------------------
	iret = 0
C
C*	Check to see if file requested is current file.
C
	IF  ( ( snfcur .eq. snfile ) .and. ( snfile .ne. ' ' ) )  THEN
	    newfil = .false.
	    RETURN
	END IF
C
C*	Close already opened file.
C
	IF  ( iflno .gt. 0 )  CALL SN_CLOS  ( iflno, ier )
C
C*	Open new file and check for error.
C
	CALL SN_OPNF ( snfile, .false., iflno, isrc, nparm, parms, 
     +		       ivert, mrgdat, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret   = -6
	    snfcur = ' '
	  ELSE
	    snfcur = snfile
	    newfil = .true.
C
C*	    Set PC package.
C
	    CALL PC_INIT  ( ivert, nparm, parms, ier )
	    IF  ( ier .ne. 0 )  THEN
                CALL ER_WMSG  ( 'PC',  ier, ' ', ierr )
	        iret = -6
	    END IF
	END IF
C*
	RETURN
	END
