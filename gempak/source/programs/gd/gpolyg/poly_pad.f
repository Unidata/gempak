	SUBROUTINE POLY_PAD ( kpad, kx, ky, ng, nc, npts, iret )
C************************************************************************
C* POLYT_PAD 								*
C*									*
C* This subroutine adds extra padding to warning grids.			*
C*									*
C* POLY_PAD ( KPAD, KX, KY, NG, NC, NPTS, IRET )			*
C*									*
C* Input parameters:							*
C*	KPAD		INTEGER		Padding type			*
C*					 0 = no padding			*
C*					 1 = positive x-dir		*
C*					 2 = positive y-dir		*
C*					 3 = negative x-dir		*
C*					 4 = negative y-dir		*
C*	KX		INTEGER		Grid point along x-dir		*
C*	KY		INTEGER		Grid point along y-dir		*
C*									*
C* Input and output parameter:						*
C*	NPTS		INTEGER		Number of points		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					-8 = incorrect padding index	*
C**									*
C* Log:									*
C* T. Lee/SAIC		03/08						*
C************************************************************************
	INCLUDE		'gpolyg.cmn'
	INCLUDE		'ERROR.PRM'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Add padding if it is not on the boundaries.
C
	IF  ( kpad .eq. 0 )  THEN
	    npts = npts + 1
	    polygi ( npts, ng, nc ) = kx
	    polygj ( npts, ng, nc ) = ky
	    RETURN
	  ELSE IF ( kpad .eq. 1 )  THEN
	    IF ( kx .ge. igxd )  RETURN
	    npts = npts + 1
	    polygi ( npts, ng, nc ) = kx + 1
	    polygj ( npts, ng, nc ) = ky
	  ELSE IF ( kpad .eq. 2 )  THEN
	    IF ( ky .ge. igyd )  RETURN
	    npts = npts + 1
	    polygi ( npts, ng, nc ) = kx
	    polygj ( npts, ng, nc ) = ky + 1
	  ELSE IF ( kpad .eq. 3 )  THEN
	    IF ( kx .le. 1 )  RETURN
	    npts = npts + 1
	    polygi ( npts, ng, nc ) = kx - 1
	    polygj ( npts, ng, nc ) = ky
	  ELSE IF ( kpad .eq. 4 )  THEN
	    IF ( ky .le. 1 )  RETURN
	    npts = npts + 1
	    polygi ( npts, ng, nc ) = kx
	    polygj ( npts, ng, nc ) = ky - 1
	  ELSE 
	    iret = -8
	END IF
C*
	RETURN
	END
