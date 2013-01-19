	SUBROUTINE RD_XFHR ( ipos,  kpshr, jftmst, jftmen, idxfhr,
     +			     iret )
C************************************************************************
C* RD_XFHR								*
C*									*
C* This subroutine gets the index of the closest forecast hour.		*
C*									*
C* RD_XFHR ( IPOS, KPSHR, JFTMST, JFTMEN, IDXFHR, IRET )		*
C*									*
C* Input parameters:							*
C*      IPOS		INTEGER		Last position of parameter	*
C*      KPSHR (*)	INTEGER		Positions of forecast hours	*
C*      JFTMST		INTEGER		Index of first valid GEMFTM	*
C*      JFTMEN		INTEGER		Index of last GEMFTM		*
C*									*
C* Output parameters:							*
C*      IDXFHR		INTEGER		Index of the closest fcst hour	*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = invalid fcst hr for parm *
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 9/02						*
C************************************************************************
	INTEGER         kpshr (*)
C------------------------------------------------------------------------
	iret = 0
C 
C*	Check for invalid positions
C
	idxfhr = 0
	IF ( ipos .lt. kpshr (jftmst) .or.
     +		ipos .ge. kpshr (jftmen) + 4 ) THEN
	    iret = -1
	    RETURN
	END IF 
C
C*	Find the index of the closest forecast hour
C
	ij = jftmst
	DO WHILE ( idxfhr .eq. 0 .and. ij .lt. jftmen )
	    IF ( ipos .ge. kpshr (ij) .and.
     +		    ipos .lt. kpshr (ij + 1) ) THEN
		idxfhr = ij
	    END IF
	    ij = ij + 1
	END DO
	IF ( idxfhr .eq. 0 ) THEN
	    IF ( ipos .ge. kpshr(jftmen) .and.
     +		    ipos .lt. kpshr (jftmen) + 4 ) THEN
		idxfhr = jftmen
	    END IF
	END IF
C*
	RETURN
	END
