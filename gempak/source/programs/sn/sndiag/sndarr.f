	SUBROUTINE SNDARR ( delz, nparms, levout, rdata,
     +			    nlevel, hghts, iret )
C************************************************************************
C* SNDARR								*
C*									*
C* This routine will find the average height difference for the 	*
C* sounding. The data will also be interpolated to the new levels.	*
C*									*
C* SNDARR ( DELZ, NPARMS, LEVOUT, RDATA, NLEVEL, HGHTS, IRET )		*
C*									*
C* Input parameters:							*
C*	DELZ		CHAR*		Input requested delta Z		*
C*	NPARMS		INTEGER		Number of parameters		*
C*	LEVOUT		INTEGER		Number of levels of original	*
C*					sounding data			*
C*	RDATA (LLMXLV)	REAL		Original sounding data		*
C*									*
C* Output parameters:							*
C*	NLEVELS		INTEGER		Number of levels of interpolated*
C*					sounding data			*
C*	HGHTS (LLMXLV)	REAL		Interpolated sounding data	*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92						*
C* J. Whistler/SSAI	 4/93	Cleaned up				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sndiag.prm'
C*
	CHARACTER*(*)	delz
	REAL		rdata(*), hghts(*)
C*
	REAL		zdiffs(LLMXLV)
C*
	LOGICAL		respnd
C------------------------------------------------------------------------
	iret = 0
C
	CALL ST_CRNM ( delz, zavg1, ier )
	IF  ( ier .ne. 0 )  zavg1 = 500.
C
	zdiftot = 0
	DO  i = 1, levout-1
	    zdiffs(i) = (rdata((i-1+1)*nparms+IHGHT) -
     +			 rdata((i-1)  *nparms+IHGHT))
	    zdiftot = zdiftot + zdiffs(i)
	END DO
	zavg2 = zdiftot / FLOAT(levout-1)
C
C*	Allow the user a chance to exit.
C
	WRITE  ( 6, 1000 )
1000	FORMAT ( /, 'SNDIAG DELTAZ CALCULATION:', /, /,
     +              ' All height differences:' )
C
	nblk = ( levout - 1 ) / 7 + 1
	i1 = 1
	i2 = 7
	DO  j = 1, nblk
	    IF  ( i2 .gt. (levout-1) ) i2 = levout - 1
	    WRITE ( 6, 1003 ) ( zdiffs(k), k = i1, i2 )
	    i1 = i1 + 7
	    i2 = i2 + 7
	END DO
1003	FORMAT ( '    ', 7F9.2 )
C
	WRITE  ( 6, 1001 ) zavg1
1001	FORMAT ( ' DELZ input    : ', F8.2 )
C
	WRITE  ( 6, 1002 ) zavg2
1002	FORMAT ( ' DELZ computed : ', F8.2 )
C
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP ( ier )
	    IF  ( ier .eq. 2 )  THEN
		iret = -1
	    	RETURN
	    END IF
	END IF
C
C*	Calculate new height levels.
C
	nlevel = 1
	hghts(1) = rdata(0*nparms+IHGHT)
	DO  WHILE ( hghts(nlevel) .lt. rdata((levout-1)*nparms+IHGHT) )
	    nlevel = nlevel + 1
	    hghts(nlevel) = hghts(1) + (nlevel-1)*zavg1
	END DO
	IF  ( hghts(nlevel) .gt. rdata((levout-1)*nparms+IHGHT) ) 
     +		nlevel = nlevel - 1
C*
	RETURN
	END
