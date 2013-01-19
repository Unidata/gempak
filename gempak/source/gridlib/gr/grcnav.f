	SUBROUTINE GR_CNAV  ( rnvblk, gsnvbk, navsz, gsflag, iret )
C************************************************************************
C* GR_CNAV								*
C*									*
C* This subroutine compares two grid navigation blocks.  If the two	*
C* navigation blocks are the same, GSFLAG is true.			*
C*									*
C* GR_CNAV  ( RNVBLK, GSNVBK, NAVSZ, GSFLAG, IRET )			*
C*									*
C* Input parameters:							*
C*	RNVBLK (NAVSZ)	REAL		First grid navigation block	*
C*	GSNVBK (NAVSZ)	REAL		Second grid navigation block	*
C*	NAVSZ		INTEGER		Navigation length		*
C*									*
C* Output parameters:							*
C*	GSFLAG		LOGICAL		Check flag			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		1/92						*
C* S. Jacobs/EAI	8/93		Added tolerance check		*
C************************************************************************
	REAL		rnvblk (*), gsnvbk (*)
	LOGICAL		gsflag
C------------------------------------------------------------------------
	iret = 0
	gsflag = .true.
C
C*	Ignore input length and check only the first 13 elements.
C
	istop = 13
	i = 0
	DO WHILE ( i .lt. istop .and. gsflag )
	    i = i + 1
	    IF ( ABS ( rnvblk (i) - gsnvbk (i) ) .gt. .005 ) THEN
		gsflag = .false.
	    END IF
	END DO
C*
	RETURN
	END
