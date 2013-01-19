	SUBROUTINE SNSFIL  ( snfile, snfold, isnfln, newdat, nparms,
     +			    iret )
C************************************************************************
C* SNSFIL								*
C*									*
C* This subroutine opens a sounding file for SNCROSS.			*
C*									*
C* SNSFIL  ( SNFILE, SNFOLD, ISNFLN, NEWDAT, NPARMS, IRET )		*
C*									*
C* Input parameters:							*
C*	SNFILE		CHAR*		Input file name			*
C*									*
C* Input and output parameters:						*
C*	SNFOLD		CHAR*		Old file name			*
C*	ISNFLN		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	NEWDAT		LOGICAL		New file flag			*
C*	NPARMS		INTEGER		Number of parameters in file	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not opened		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/85						*
C* M. desJardins/GSFC	 9/90	GEMPAK 5				*
C* D. Kidwell/NCEP	 5/99	PC_DSET -> PC_INIT                      *
C* D. Kidwell/NCEP	 4/05	Replaced 40 with MMPARM			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snfile, snfold
	LOGICAL		newdat
C*
	LOGICAL		mrgdat
	CHARACTER	parms (MMPARM)*4
C------------------------------------------------------------------------
	iret = 0
C
C*	Check if the new and old files are the same.
C
	IF  ( ( snfile .eq. snfold ) .and. ( snfold .ne. ' ' ) )  THEN
	    newdat = .false.
	  ELSE
	    newdat = .true.
C
C*	    Close already opened file.
C
	    IF  ( isnfln .ne. 0 )  THEN
		CALL SN_CLOS  ( isnfln, ier )
		isnfln = 0
	    END IF
C
C*	    Open new file.
C
	    CALL SN_OPNF  ( snfile, .false., isnfln, isr, nparms, parms,
     +	                    ivert, mrgdat, ier )
C
C*	    Check for error from file open.
C
	    IF  ( ier .ne. 0 ) THEN
		iret   = -4
		snfold = ' '
		isnfln = 0
	      ELSE
		CALL PC_INIT ( ivert, nparms, parms, ier )
		snfold = snfile
	    END IF
	END IF
C*
	RETURN
	END
