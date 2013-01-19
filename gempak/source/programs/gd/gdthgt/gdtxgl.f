	SUBROUTINE GDTXGL ( igdfln, timfnd, npts, ystrt, ystop, ivcord,
     +			    levels, nlev, iret)
C************************************************************************
C*  GDTXGL								*
C*									*
C* This subroutine gets a list of levels given a list of times and	*
C*	YSTART YSTOP.  Each level may not be available for each time.	*
C*									*
C* GDTXGL ( IGDFLN, TIMFND, NPTS, YSTRT, YSTOP, IVCORD, LEVELS, NLEV,	*
C*	    IRET )							*
C*									*
C* Input parameters:							*
C*	IGDFLN			INTEGER		GRID FILE NUMBER	*
C*	TIMFND (NPTS)		CHAR*		TIME STRINGS		*
C*	NPTS			INTEGER		NUMBER OF TIME POINTS	*
C*	YSTRT			REAL		BOTTOM Y VALUE		*
C*	YSTOP			REAL		TOP Y VALUE		*
C*	IVCORD			INTEGER		VERTICAL COORD		*
C*									*
C* Output parameters:							*
C*	LEVELS (NLEV)		INTEGER		LEVELS			*
C*	NLEV			INTEGER		NUMBER OF LEVELS	*
C*	IRET			INTEGER		RETURN CODE		*
C**									*
C* LOG:									*
C* T.W.Barker/WR/SSD		8/91	Created				*
C* T. Lee/GSC		 1/99	Changed timfnd (LLMXTM) to timfnd(*)	*
C* K. Brill/HPC		10/04	Changes for new file-times management	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	CHARACTER*(*)	timfnd (*)
	CHARACTER	time (2)*20
	INTEGER 	levarr (2, LLMXLV), levels (*)
	REAL		rlvl (LLMXLV)

	iret = 0
	nlev = 0
	time (2) = ' '
	DO i = 1, npts
	    time (1) = timfnd (i)
	    CALL DG_GLEV ( igdfln, time, ivcord, LLMXLV, levarr, nlev1,
     +			   iret )
	    IF ( iret .ne. 0 ) RETURN
c
	    IF ( nlev1 .ne. 0 ) THEN
		DO j = 1, nlev1
		    rlvl (j) = float ( levarr (1, j) )
		END DO
		CALL LV_SORT ( ivcord, nlev1, rlvl, iret )
		IF ( iret .ne. 0 ) RETURN
c
		DO j = 1, nlev1
		    insrt = 0
		    IF ( ( ivcord .eq. 1 ) .and.
     +			 ( rlvl (j) .le. ystrt ) .and.
     +			 ( rlvl (j) .ge. ystop ) ) insrt = 1
		    IF ( ( ivcord .ne. 1 ) .and.
     +			 ( rlvl (j) .ge. ystrt ) .and.
     +			 ( rlvl (j) .le. ystop ) ) insrt = 1
		    IF ( insrt .eq. 1 ) THEN
			IF ( nlev .eq. 0 ) THEN
			    nlev = 1
			    levels (nlev) = int ( rlvl (j) )
			ELSE
			    ifnd = 0
			    DO k = 1, nlev
				IF ( levels (k) .eq.
     +				     ( int ( rlvl (j) ) ) ) ifnd = 1
			    END DO
			    IF ( ifnd .eq. 0 ) THEN
				nlev = nlev + 1
				levels (nlev) = int ( rlvl (j) )
			    END IF
			END IF
		    END IF
		END DO
	    END IF
	END DO

	DO i = 1, nlev
	    rlvl (i) = float ( levels (i) )
	END DO
	CALL LV_SORT ( ivcord, nlev, rlvl, iret )
	DO i = 1, nlev
	    levels (i) = int ( rlvl (i) )
	END DO

	RETURN
	END
