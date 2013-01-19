	SUBROUTINE POLY_EXIST ( nc, ng, np, ipx, ipy, new, iret )
C************************************************************************
C* POLY_EXIST  								*
C*									*
C* This subroutine checks if a point already on a polygon.		*
C*									*
C* POLY_EXIST ( NC, NG, NP, IPX, IPY, NEW, IRET )			*
C*									*
C* Input parameters:							*
C*	NC		INTEGER		Warning category		*
C*	NG		INTEGER		Nth group			*
C*	NP		INTEGER		Number of points		*
C*	IPX		INTEGER		x-coordinate			*
C*	IPY		INTEGER		y-coordinate			*
C*									*
C* Output parameters:							*
C*	NEW		LOGICAL		Grid existent flag		*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		 6/08						*
C************************************************************************
	INCLUDE		'gpolyg.cmn'
	LOGICAL		new
C-----------------------------------------------------------------------
	iret = 0
	new = .true.
C
	DO  kk = 1, np
	    IF ( ipx .eq. isnake ( kk, ng, nc ) .and.
     +	         ipy .eq. jsnake ( kk, ng, nc ) )  THEN
		new = .false.
		RETURN
	    END IF
	END DO
C*
	RETURN
	END
