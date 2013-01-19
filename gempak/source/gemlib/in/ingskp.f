	SUBROUTINE IN_GSKP  ( ijskip, ix1, ix2, nsx, iy1, iy2, nsy,
     +			      autos, iret )
C************************************************************************
C* IN_GSKP								*
C*									*
C* This subroutine decodes the user input for grid subsetting using a	*
C* skip.								*
C*									*
C* IJSKIP is parsed by IN_GSKP.  IJSKIP information is entered using	*
C* the following format, where items in square brackets are optional:	*
C*									*
C*	IJSKIP = Iskip[;Istart][;Iend][/Jskip[;Jstart][;Jend]],		*
C*									*
C*	IJSKIP = Y[ES], or IJSKIP = N[O]				*
C*									*
C* If IJSKIP is blank, NSX = NSY = 0, and IX1, IX2, IY1, and IY2 are	*
C* returned as IMISSD.  If IJSKIP is NO, then the skip parameters are	*
C* set as for a blank, and AUTOS is .FALSE. on RETURN.  IF IJSKIP is	*
C* YES, then the skip parameters are set as for a blank, and AUTOS is	*
C* .TRUE. on RETURN.							*
C*									*
C* If only Iskip is given, then NSY = NSX, and IX1, IX2, IY1, and IY2	*
C* are returned as IMISSD.  Otherwise, any value not given is returned  *
C* as IMISSD.								*
C*									*
C* IN_GSKP  ( IJSKIP, IX1, IX2, NSX, IY1, IY2, NSY, AUTOS, IRET )	*
C*									*
C* Input parameters:							*
C*	IJSKIP		CHAR*		Skip subsetting input		*
C*									*
C* Output parameters:							*
C*	IX1		INTEGER		Starting I dimension index	*
C*	IX2		INTEGER		Ending I dimension index	*
C*	NSX		INTEGER		# of points to skip along I	*
C*	IY1		INTEGER		Starting J dimension index	*
C*	IY2		INTEGER		Ending J dimension index	*
C*	NSY		INTEGER		# of points to skip along J	*
C*	AUTOS		LOGICAL		Flag for automatic skipping	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-16 = bounds are not valid	*
C**									*
C* Log:									*
C* K. Brill/HPC		12/02						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	ijskip
	LOGICAL		autos
C*
	CHARACTER	cskp*(LLMXLN)
	INTEGER		iskp (3)
C*
	CHARACTER	carr (2)*72
C------------------------------------------------------------------------
	iret = 0
	autos = .false.
C
C*	If IJSKIP is blank, return 0's and missing values.
C
	nsx = 0
	nsy = 0
	ix1 = IMISSD
	ix2 = IMISSD
	iy1 = IMISSD
	iy2 = IMISSD
	IF ( ijskip .eq. ' ' ) THEN
	    RETURN
	END IF
	CALL ST_LCUC ( ijskip, cskp, ier )
	IF ( cskp(1:1) .eq. 'N' ) THEN
	    RETURN
	ELSE IF ( cskp (1:1) .eq. 'Y' ) THEN
	    autos = .true.
	    RETURN
	END IF
	ibang = INDEX ( cskp, '!' )
	IF ( ibang .gt. 1 ) THEN
	    cskp = cskp (1:ibang-1)
	ELSE IF ( ibang .eq. 1 ) THEN
	    RETURN
	END IF
C
C*	Break input into I and J relevant values.
C
	CALL ST_CLST  ( cskp, '/', ' ', 2, carr, numc, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -16
	    RETURN
	END IF
C
C*	Get the I skip values from the first string.
C
	CALL ST_ILST ( carr (1), ';', IMISSD, 3, iskp, num, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -16
	    RETURN
	END IF
	IF ( iskp (1) .gt. 0 ) THEN
	    nsx = iskp (1)
	ELSE
	    nsx = 0
	END IF
	ix1 = iskp (2)
	ix2 = iskp (3)
	IF ( ix2 .gt. 0 .and. ix2 .le. ix1 ) THEN
	    iret = -16
	    RETURN
	END IF
	IF ( ( ix1 .ne. IMISSD .and. ix1 .lt. 1 ) .or.
     +	     ( ix2 .ne. IMISSD .and. ix2 .lt. 1 ) ) THEN
	    iret = -16
	    RETURN
	END IF
	IF ( numc .eq. 1 ) THEN
	    nsy = nsx
	    iy1 = IMISSD
	    iy2 = IMISSD
	    RETURN
	END IF
C
C*	Get the J skip values from the second string.
C
	CALL ST_ILST ( carr (2), ';', IMISSD, 3, iskp, num, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -16
	    RETURN
	END IF
	IF ( iskp (1) .gt. 0 ) THEN
	    nsy = iskp (1)
	ELSE
	    nsy = 0
	END IF
	iy1 = iskp (2)
	iy2 = iskp (3)
	IF ( iy2 .gt. 0 .and. iy2 .le. iy1 ) THEN
	    iret = -16
	    RETURN
	END IF
	IF ( ( iy1 .ne. IMISSD .and. iy1 .lt. 1 ) .or.
     +	     ( iy2 .ne. IMISSD .and. iy2 .lt. 1 ) ) THEN
	    iret = -16
	    RETURN
	END IF
C*
	RETURN
	END
