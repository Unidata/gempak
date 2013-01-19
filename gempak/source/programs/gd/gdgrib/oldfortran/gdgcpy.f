	SUBROUTINE GDGCPY  ( cpyfil, cprj, kx, ky, rnvblk, igpds, iret )
C************************************************************************
C* GDGCPY								*
C*									*
C* This subroutine takes the user input for CPYFIL to make the grid	*
C* navigation block.							*
C*									*
C* GDGCPY  ( CPYFIL, CPRJ, KX, KY, RNVBLK, IGPDS, IRET )		*
C*									*
C* Input parameters:							*
C*	CPYFIL		CHAR*		User input for CPYFIL		*
C*									*
C* Output parameters:							*
C*	CPRJ		CHAR*		Grid projection			*
C*	KX		INTEGER		Number of points in x dir	*
C*	KY		INTEGER		Number of points in y dir	*
C*	RNVBLK  (13)	REAL		Grid navigation block		*
C*	IGPDS		INTEGER		Grid # from CPYFIL		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +5 = CPYFIL is blank		*
C*					-19 = CPYFIL invalid		*
C**									*
C* Log:									*
C* K. Brill/HPC		 2/00						*
C* K. Brill/HPC		 3/00	Call GR_OPEN; |subset in CPYFIL		*
C* R. Tian/SAIC		 4/005	GR_OPEN -> GD_OPEN			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	cpyfil, cprj
	REAL		rnvblk (*)
C*
	CHARACTER	filnam*(LLMXLN), fullnm*(LLMXLN)
	REAL		anlblk (LLNANL), rltln (4), rlt (2), rln (2)
	LOGICAL		subset, exist
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	igpds = 255
	subset = .false.
C*
	ibar = INDEX ( cpyfil, '|' )
	IF ( ibar .gt. 1 ) THEN
	    filnam = cpyfil (1:ibar-1)
	    CALL ST_RLST ( cpyfil (ibar+1:), ';', RMISSD, 4, rltln,
     +			   num, ier )
	    IF ( num .ne. 4 .or. ier .lt. 0 ) THEN
		iret = -19
		RETURN
	    END IF
	    DO i = 1, 4
		IF ( ERMISS ( rltln (i) ) ) THEN
		    iret = -19
		    RETURN
		END IF
	    END DO
	    rlt (1) = rltln (1)
	    rln (1) = rltln (2)
	    rlt (2) = rltln (3)
	    rln (2) = rltln (4)
	    subset = .true.
	ELSE IF ( ibar .eq. 1 ) THEN
	    iret = -19
	    RETURN
	ELSE
	    filnam = cpyfil
	END IF
	IF  ( filnam .eq. ' ' )  THEN
	    iret = +5
	    RETURN
	END IF
C*
	IF  ( filnam ( 1:1 ) .eq. '#' )  THEN
C
C*	    CASE 1: Build new navigation and analysis blocks from grid
C*	    navigation table input.
C
	    CALL GDGCPT ( filnam, cprj, kx, ky, rnvblk, igpds, iret )
	    IF  ( iret .lt. 0 )  THEN
		RETURN
	    END IF
	ELSE
C
C*	    CASE 2: Get the navigation and analysis blocks from the
C*	    existing file.
C
	    CALL FL_INQR ( filnam, exist, fullnm, iret )
	    IF ( .not. exist .or. iret .ne. 0 )  THEN
		CALL ER_WMSG ( 'FL', iret, ' ', ier )
		iret = -19
		RETURN
	    ELSE
		CALL GD_OPEN ( fullnm, .false., LLNANL, LLNNAV, iflno,
     +                         anlblk, rnvblk, maxgrd, iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG ( 'GD', iret, filnam, ier )
		    iret = -19
		    RETURN
		ELSE
		    CALL GD_CLOS  ( iflno, ier )
		    CALL GR_RNAV  ( rnvblk, cprj, kx, ky, ier )
		END IF
	    END IF
	END IF
	IF ( subset ) THEN
	    igpds = 255
C
C*	    Round subset region to the nearest grid point.
C
	    CALL GR_SNAV ( 16, rnvblk, ier )
	    CALL GTRANS ( 'M', 'G', 2, rlt, rln, rlt, rln, ier )
	    rlt (1) = FLOAT ( NINT ( rlt (1) ) )
	    rln (1) = FLOAT ( NINT ( rln (1) ) )
	    rlt (2) = FLOAT ( NINT ( rlt (2) ) )
	    rln (2) = FLOAT ( NINT ( rln (2) ) )
	    rkx = FLOAT ( kx )
	    rky = FLOAT ( ky )
	    subset = subset .and.
     +		     ( rlt (1) .ge. 1 .and. rlt (1) .le. rkx )
	    subset = subset .and.
     +		     ( rlt (2) .ge. 1 .and. rlt (2) .le. rkx )
	    subset = subset .and.
     +		     ( rlt (2) .gt. rlt (1) )
	    subset = subset .and.
     +		     ( rln (1) .ge. 1 .and. rln (1) .le. rky )
	    subset = subset .and.
     +		     ( rln (2) .ge. 1 .and. rln (2) .le. rky )
	    subset = subset .and.
     +		     ( rln (2) .gt. rln (1) )
	    IF ( .not. subset ) THEN
		iret = -19
		RETURN
	    END IF
C*
	    WRITE (6,1000) kx, ky, rlt(1), rln(1), rlt(2), rln(2)
1000	    FORMAT( ' (1,1)->(',I4,',',I4,') subset to (',F4.0,',',
     +              F4.0,')->(',F4.0,',',F4.0,')' )
C*
	    kx = NINT ( rlt (2) - rlt (1) ) + 1
	    ky = NINT ( rln (2) - rln (1) ) + 1
C*
	    CALL GTRANS ( 'G', 'M', 2, rlt, rln, rlt, rln, ier )
C
C*	    Reset elements of RNVBLK for the subset.
C
	    rnvblk (5) = kx
	    rnvblk (6) = ky
	    rnvblk (7) = rlt (1)
	    rnvblk (8) = rln (1)
	    rnvblk (9) = rlt (2)
	    rnvblk (10) = rln (2)
	END IF
C*
	RETURN
	END
