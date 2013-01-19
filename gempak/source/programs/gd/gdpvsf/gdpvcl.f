	SUBROUTINE GDPVCL  ( gfunc, lflag, iret )
C************************************************************************
C* GDPVCL								*
C*									*
C* This subroutine determines if the function specification contains	*
C* a layer-averaged quantity.						*
C*									*
C* GDPVCL  ( GFUNC, LFLAG )						*
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
C* J. Nielsen/SUNYA	12/90	From GDPCLA				*
C* D. Knight/SUNYA	10/94	Added MPVR as layer variable		*
C* D. Knight/SUNYA	 1/96   Removed MPVR since not needed in 5.4    *
C************************************************************************
C*
	CHARACTER*(*)	gfunc
	LOGICAL		lflag
C*
	CHARACTER	pfunc*256, parms(11)*4
	DATA parms / 'LAV(', 'LDF(', 'MASS', 'MDIV', 'MSDV', 'PVOR',
     +		   'RICH', 'STAB', 'LTRN', 'VLAV', 'VLDF' /
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
	DO i = 1, 11
	  iat = index ( pfunc, parms (i) )
	  IF ( iat .ne. 0 ) THEN
	    lflag = .true.
	    RETURN
	  END IF
	END DO
C*
	RETURN
	END
