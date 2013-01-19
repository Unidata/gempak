	SUBROUTINE GDXCLA  ( gfunc, lflag, iret )
C************************************************************************
C* GDXCLA								*
C*									*
C* This subroutine determines if the function specification contains	*
C* a layer-averaged quantity.						*
C*									*
C* GDXCLA  ( GFUNC, LFLAG, IRET )					*
C*									*
C* Input parameters:							*
C*	GFUNC		  CHAR*		Function specification		*
C*									*
C* Output parameters:							*
C*      LFLAG		  LOGICAL	Flag for layer quantities	*
C*	IRET		  INTEGER	Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. F. Brill/GSC       11/89   					*
C* K. Brill/GSC          12/89  	'LAV(', 'LDF(', and ' ' removal	*
C* J. Whistler/NSSFC	 12/94		Added 'EPV(', 'WSHR','BVSQ'	*
C* K. Brill/EMC		  4/96		Added THRM			*
C* T. Lee/SAIC		  3/05		Removed EPV(, added QVCL	*
C************************************************************************
C*
	PARAMETER	(NPARM=15)
	CHARACTER*(*)	gfunc
	LOGICAL		lflag
C*
	CHARACTER	pfunc*256, parms(NPARM)*4
	DATA parms / 'LAV(', 'LDF(', 'MASS', 'MDIV', 'MSDV', 'PVOR',
     +		   'RICH', 'STAB', 'LTRN', 'VLAV', 'VLDF', 'QVCL',
     +		   'WSHR', 'BVSQ', 'THRM' /
C------------------------------------------------------------------------
	iret = 0
	lflag = .false.
C
C*	Check to see if GFUNC was specified.
C
	CALL ST_LSTR ( gfunc, lngth, iret )
	IF ( lngth .eq. 0 ) THEN
	  iret = 7
	  RETURN
	END IF
C 
C* 	Convert GFUNC to upper case.
C
	CALL ST_LCUC ( gfunc, pfunc, iret )
C
C*	Remove blanks from pfunc.
C
	CALL ST_RMBL ( pfunc, pfunc, lnx, iret )
C
C*      Check for the occurrence of a layer quantity.
C
	i = 0
	DO WHILE ( .not. lflag .and. i .lt. NPARM )
	  i = i + 1
	  ipos = index ( pfunc, parms (i) )
	  IF ( ipos .ne. 0 ) THEN
	    lflag = .true.
	    RETURN
	  END IF
	END DO
C*
	RETURN
	END
