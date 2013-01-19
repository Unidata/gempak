	SUBROUTINE SI_GHDR  ( bultin, lenbul, nchar, fhr, vmdy, vhh,
     +			      iret )
C************************************************************************
C* SI_GHDR								*
C*									*
C* This subroutine gets the second part of header information from Sea	*
C* Ice (SI) bulletin.							*
C*									*
C* SI_GHDR  ( BULTIN, LENBUL, NCHAR, FHR, VMDY, VHH, IRET )		*
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		WMO bulletin w/ control chars	*
C*	LENBUL		INTEGER		Length of the WMO bulletin 	*
C*	NCHAR		INTEGER		No of characters in WMO header	*
C*									*
C* Output parameters:							*
C*	FHR		CHAR*		Forecast hour			*
C*	VMDY		CHAR*		Valid time, mm/dd/yy		*
C*	VHH		CHAR*		Valid hour, hh			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = cannot get header info	*
C*					 -6 = invalid time		*
C**									*
C* Log:									*
C* T. Lee/SAIC		 8/02						*	
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( MAXCHR = 48 )
C*
	CHARACTER*(*)	bultin, fhr, vmdy, vhh
C*
	CHARACTER	carr (6)*12
C------------------------------------------------------------------------
	iret  = 0
C
C*	If the first character is not a Control-A, return.
C
	IF  ( bultin (1:1) .ne. CHCTLA )  THEN
	    iret = -5
	    RETURN
	END IF
C
C*	Set the counters and loop over the bulletin looking for the 3rd 
C*	line feed after the WMO header.
C
	istart = nchar + 1
	knt  = 1
	DO  WHILE ( ( istart .lt. lenbul ) .and. ( knt .le. 3 ) )
	    idx = INDEX ( bultin ( istart: ), CHLF )
	    iend = istart + idx - 1
	    istart = iend + 1
	    knt = knt + 1
	END DO
C
	idx = INDEX ( bultin ( istart + 1: ), CHLF )
	iend = istart + idx - 1
C
C*	Look for FORECAST in the header.
C
	ifr = INDEX ( bultin ( istart:iend), 'FORECAST' )
C
C*	Break the seventh string into parts.
C
	IF  ( ifr .ne. 0 )  THEN
	    CALL ST_CLST ( bultin ( istart:iend ), ' ', ' ', 6, 
     +			   carr, n, ier )
	    fhr   = carr (1)
	    vmdy  = carr (4)
	    vhh   = carr (5)
	  ELSE
	    iret = -5
	    RETURN
	END IF
C
	IF ( ier .ne. 0 ) iret = -6
C*
	RETURN
	END
