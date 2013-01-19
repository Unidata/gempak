	SUBROUTINE GR_SNAV  ( navsz, rnvblk, iret )
C************************************************************************
C* GR_SNAV								*
C*									*
C* This subroutine sets up a grid coordinate system in GEMPLT.  The	*
C* navigation block should be sent as it was received from the grid	*
C* file open subroutine.  Note that the graphics projection and mode	*
C* must be defined before GR_SNAV is called.  This subroutine will fail	*
C* if the grid mode is not the same as the current GEMPLT mode.		*
C*									*
C* GR_SNAV  ( NAVSZ, RNVBLK, IRET )					*
C*									*
C* Input parameters:							*
C*	NAVSZ		INTEGER		Length of navigation block	*
C*	RNVBLK (NAVSZ)	REAL		Navigation block		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = invalid navigation type	*
C*					 -7 = GEMPLT error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/84						*
C* M. desJardins/GSFC	 8/88	Fixed for GEMPAK4			*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C************************************************************************
	REAL		rnvblk (*)
	CHARACTER	proj*20
C------------------------------------------------------------------------
	iret   = 0
	rntype = rnvblk (1)
C
C*	Check for map projection.
C
	CALL ST_ITOC  ( rnvblk (2), 1, proj, ier )
C
C*	Set up grid in GEMPLT.  First check for simple map projection.
C
	kx = rnvblk (5)
	ky = rnvblk (6)
	IF  ( ( rntype .eq. 1.0 ) .and. ( navsz .ge. 10 ) )  THEN
	    CALL GSMODE  ( 1, ier )
	    CALL GSGMAP  ( proj, kx, ky, rnvblk (7), rnvblk (8), 
     +			   rnvblk (9), rnvblk (10), ier )
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG  ( 'GEMPLT', ier, ' ', ier2 )
		iret = -7
	    END IF
C
C*	    Check for complex map projection.
C
	  ELSE IF  ( ( rntype .eq. 2. ) .and. ( navsz .ge. 13 ) )  THEN
	    CALL GSMODE  ( 1, ier )
	    CALL GSGPRJ  ( proj, rnvblk (11), rnvblk (12), rnvblk (13),
     +			   kx, ky, rnvblk (7), rnvblk (8), rnvblk (9), 
     +			   rnvblk (10), ier )
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG  ( 'GEMPLT', ier, ' ', ier2 )
		iret = -7
	    END IF
C
C*	    Check for graph projections.
C
	  ELSE IF  ( ( rntype .eq. 3. ) .and. ( navsz .ge. 10 ) )  THEN
	    CALL GSMODE  ( 2, ier )
	    ixtype = 1
	    IF  ( proj .eq. 'LIN' )  THEN
	        iytype = 1
	      ELSE IF  ( proj .eq. 'LOG' )  THEN
	        iytype = 2
	      ELSE IF  ( proj .eq. 'KAP' )  THEN
	        iytype = 3
	      ELSE IF  ( proj .eq. 'POL' )  THEN
	        ixtype = 5
	        iytype = 5
	      ELSE
	        iret = -6
	        RETURN
	    END IF
	    CALL GSGGRF  ( ixtype, iytype, kx, ky, rnvblk (7), 
     +			   rnvblk (8), rnvblk (9), rnvblk (10), ier )
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG  ( 'GEMPLT', ier, ' ', ier2 )
		iret = -7
	    END IF
C
C*	    Set error if navigation type is unknown.
C
	  ELSE
	    iret = -6
	END IF
C*
	RETURN
	END
