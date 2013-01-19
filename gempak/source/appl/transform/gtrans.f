	SUBROUTINE GTRANS ( sysin, sysout, np, xin, yin, xout, yout,
     +			    iret )
C************************************************************************
C* GTRANS								*
C* 									*
C* This subroutine transforms an array of points in any input coordinate*
C* system into the specified output coordinate system.  In the map      *
C* coordinate system, X represents latitude and Y represents longitude. *
C* 									*
C* GTRANS ( SYSIN, SYSOUT, NP, XIN, YIN, XOUT, YOUT, IRET )		*
C* 									*
C* Input parameters:							*
C*	SYSIN		CHAR*		Input coordinate system		*
C*                                        'S' = screen coordinates      *
C*					  'D' = device coordinates	*
C*					  'N' = normalized coordinates	*
C*					  'V' = view coordinates	*
C*					  'P' = plot coordinates	*
C*					  'L' = linear proj plane coords*
C*					  'W' = rotated map coords	*
C*					  'M' = map coordinates		*
C*					  'Q' = rotated grid map coords	*
C*					  'I' = linear proj plane coords*
C*					  'G' = grid coordinates	*
C*	SYSOUT		CHAR*		Output coordinate system	*
C*					   SDNVPLWMQIG as defined above	*
C*	NP		INTEGER		Number of points		*
C* 	XIN (NP)	REAL		X input coordinates 		*
C* 	YIN (NP)	REAL		Y input coordinates 		*
C* 									*
C* Output parameters:							*
C* 	XOUT (NP)	REAL		X output coordinates 		*
C* 	YOUT (NP)	REAL		Y output coordinates 		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/86	Added check for 0 points		*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Williams/EAi	 3/94	Removed blank comments from header	*
C* M. Linda/GSC		 3/96	Changed check for GPLT buffer overflow	*
C* K. Brill/EMC		 3/96	SYSIN/SYSOUT documentation above	*
C* S. Schotz/NCEP        7/97   Update documentation for S coordinate   *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C* D.W.Plummer/NCEP	10/05	Split incoming array size if necessary	*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*) 	sysin, sysout
	REAL		xin (*), yin (*), xout (*), yout (*)
C
	INTEGER 	isend (5)
C
	LOGICAL		done
C------------------------------------------------------------------------
C
C*	Check validity of the coordinate system.
C
	isin  = INDEX ( syslo, sysin  ) + INDEX ( sysup, sysin  )
	isout = INDEX ( syslo, sysout ) + INDEX ( sysup, sysout )
	IF ( ( isin .eq. 0 ) .or. ( isout .eq. 0 ) ) THEN
	    iret = NOCORD
	    RETURN
	END IF
C
C*	Check number of points.
C
	IF ( np .le. 0 ) THEN
	    iret = NORMAL
	    RETURN
	END IF
C
C*	Load input parameters into buffer and write them to the mailbox.
C*	Variables isend(1) [total number of bytes sent] and isend(5) [total
C*      number points to convert] are set below in the WHILE loop.
C
	isend (2) = FTRANS
	isend (3) = isin
	isend (4) = isout
C
C*	Logic to prevent GPLT buffer from overflowing.
C*	The variable 'maxnp' is derived from the following 
C*         original lines of code:
C*
C*         isnd = 2 + ( 3 + ( 2 * np ) )
C*         ircv = 1 + ( ( 2 * np ) + 1 )
C
C*	Such that ( isnd + ircv ) cannot exceed parameter IGBSIZ.
C
	maxnp = ( ( IGBSIZ - 7 ) / 4 )
C
C*	The variable 'iptr' points to the start of the array being sent.
C*	The variable 'nptmp' is the number of points being sent.
C
	iptr = 1
	nptmp = MIN ( np-iptr+1, maxnp )
C
	done = .false.
	DO WHILE ( .not. done ) 
C
C*	    isend(1) - total number of bytes sent
C*          isend(5) - total number points to convert
C
	    isnd = 2 + ( 3 + ( 2 * nptmp ) )
	    isend (1) = isnd
	    isend (5) = nptmp
C
	    CALL GPUT ( isend, 5, iret )
	    IF ( iret .ne. NORMAL ) RETURN
C
	    CALL GPUTR ( xin(iptr), nptmp, iret )
	    IF ( iret .ne. NORMAL ) RETURN
C
	    CALL GPUTR ( yin(iptr), nptmp, iret )
	    IF ( iret .ne. NORMAL ) RETURN
C
C*	    Get output parameters.
C
	    CALL GGET ( iret, 1, ierr )
	    IF ( ierr .ne. NORMAL ) THEN
	        iret = ierr
	        RETURN
	    END IF
C
	    CALL GGETR ( xout(iptr), nptmp, ierr )
	    IF ( ierr .ne. NORMAL ) THEN
	        iret = ierr
	        RETURN
	    END IF
C
	    CALL GGETR ( yout(iptr), nptmp, ierr )
	    IF ( ierr .ne. NORMAL ) iret = ierr
C
	    iptr = iptr + nptmp
	    nptmp = MIN ( np-iptr+1, maxnp )
C
	    IF ( iptr .gt. np )  done = .true.
C
	END DO
C*
	RETURN
	END
