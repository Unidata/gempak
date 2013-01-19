	SUBROUTINE GQBND  ( sys, xl, yb, xr, yt, iret )	
C************************************************************************
C* GQBND								*
C*									*
C* This subroutine returns the boundaries of the specified coordinate   *
C* system.  For the linear coordinate systems (D,N,V,P), the lower left *
C* and upper right corners are returned.  For M coordinates, the minimum*
C* and maximum range of latitude and longitude are estimated.  For G    *
C* coordinates, the minimum and maximum grid points displayed in the    *
C* plot area are given.		                                        *
C*									*
C* GQBND  ( SYS, XL, YB, XR, YT, IRET )					*
C*									*
C* Input parameters:							*
C*	SYS		CHAR*		Coordinate system		*
C*					  'S' = screen coordinates	*
C*					  'D' = device coordinates	*
C*					  'N' = normalized coordinates	*
C*					  'V' = view coordinates	*
C*					  'P' = plot coordinates	*
C*					  'M' = map coordinates		*
C*					  'G' = grid coordinates	*
C*									*
C* Output parameters:							*
C*	XL		REAL		Lower left x / latitude		*
C*	YB		REAL		Lower left y / longitude	*
C*	XR		REAL		Upper right x / latitude	*
C*	YT		REAL		Upper right y / longitude	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* S. Schotz/NCEP        7/97   Update documentation for S coordinate   *
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sys
C*
	INTEGER		isend (3)
	REAL		rcv   (4)
C------------------------------------------------------------------------
C*	Determine which coordinate system to use.
C
	isys = INDEX  ( sysup, sys ) + INDEX  ( syslo, sys )
	IF  ( isys .eq. 0 )  THEN 
	    iret = NOCORD
	    RETURN
	END IF
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 3
	isend (2) = FQBND
	isend (3) = isys
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	    RETURN
	END IF
C
	CALL GGETR  ( rcv, 4, ier )
	IF  ( ier .eq. NORMAL )  THEN
	    xl  = rcv (1)
	    yb  = rcv (2)
	    xr  = rcv (3)
	    yt  = rcv (4)
	END IF
C*
	RETURN
	END
