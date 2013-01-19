	SUBROUTINE LV_DUPL  ( nlev, rlevel, iret )
C************************************************************************
C* LV_DUPL								*
C*									*
C* This subroutine eliminates duplicate levels from a list of levels.	*
C* The variables NLEV and RLEVEL are updated.				*
C*									*
C* LV_DUPL  ( NLEV, RLEVEL, IRET )					*
C*									*
C* Input and output parameters:						*
C*	NLEV		INTEGER		Number of levels		*
C*	RLEVEL (NLEV)	REAL		Levels				*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/86						*
C* M. desJardins/GSFC	 9/88	Documentation				*
C************************************************************************
	REAL		rlevel (*)
C------------------------------------------------------------------------
C*	Search for duplicate levels.
C
	iret = 0
	num  = nlev
	nout = 1
	rold = rlevel (1)
	DO  i = 2, num
	    IF  ( rlevel (i) .ne. rold )  THEN
		nout = nout + 1
		rold = rlevel (i)
		rlevel (nout) = rold
	    END IF
	END DO
C
C*	Update number of levels.
C
	nlev = nout
C*
	RETURN
	END
