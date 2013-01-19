	SUBROUTINE GDGWMO ( wmohdr, ncntr, cdd, chhmm, chdr, iret )
C************************************************************************
C* GDGWMO								*
C*									*
C* This subrutine creates a 21-byte WMO header for a GRIB message.	*
C*									*
C* The user input for the WMO header is entered as:			*
C*									*
C*	            WMO_ID / Origin_ID / DDHHMM				*
C*									*
C* where WMO_ID is the first six bytes of the header, Origin_ID is the	*
C* originating center 4-letter identifier, and DDHHMM is the reference	*
C* two-digit day, hour, and minute.					*
C*									*
C* The first six bytes of the header must be provided by the user.	*
C*									*
C* GDGWMO ( WMOHDR, NCNTR, CDD, CHHMM, CHDR, IRET )			*
C*									*
C* Input parameters:							*
C*	WMOHDR		CHAR*		User input for the WMO header	*
C*	NCNTR		INTEGER		Center ID from PDS byte 5	*
C*	CDD		CHAR*2		2-digit day of month		*
C*	CHHMM		CHAR*4		4-digit hour minute		*
C*									*
C* Output parameters:							*
C*	CHDR		CHAR*		Output WMO header		*
C*	IRET		INTEGER		Return code			*
C*					 +6 = center # inconsistent	*
C*					  0 = normal return		*
C*					 -9 = 1st 6 chars required	*
C*					-10 = supplied hdr is too long	*
C**									*
C* Log:									*
C* K. Brill/HPC		08/99						*
C* K. Brill/HPC		 2/00	WMOHDR entered as 3 parts		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	wmohdr, cdd, chhmm, chdr
C*
	CHARACTER*8	grps (3)
	CHARACTER*32	cdum
C------------------------------------------------------------------------
	iret = 0
	chdr = ' '
C*
	CALL ST_LSTR ( wmohdr, lnt, ier )
	IF ( lnt .le. 0 ) RETURN
C*
	CALL ST_CLST ( wmohdr, '/', ' ', 3, grps, num, ier )
	CALL ST_LSTR ( grps(1), lnt, ier )
	IF ( lnt .eq. 6 ) THEN
	    chdr = grps (1) (1:6) // ' '
	ELSE IF ( lnt .gt. 6 ) THEN
	    iret = -10
	    RETURN
	ELSE
	    iret = -9
	    RETURN
	END IF
	CALL ST_LSTR ( grps(2), lnt, ier )
	IF ( lnt .eq. 4 ) THEN
	    cdum = chdr (1:7) // grps (2) (1:4) // ' '
	    IF ( ncntr .eq. 7 .and. grps (2) (1:4) .ne. 'KWBC' )
     +		iret = +6
	ELSE
	    IF ( ncntr .eq. 7 ) THEN
	        cdum = chdr (1:7) // 'KWBC '
	    ELSE
	        cdum = chdr (1:7) // 'KWBC '
		iret = +6
	    END IF
	END IF
	chdr = cdum (1:12)
	CALL ST_LSTR ( grps(3), lnt, ier )
	IF ( lnt .eq. 6 ) THEN
	    cdum = chdr (1:12) // grps (3) (1:6)
	ELSE
	    cdum = chdr (1:12) // cdd(1:2) // chhmm (1:4)
	END IF
	chdr = cdum (1:18)
	chdr (19:19) = CHCR
	chdr (20:20) = CHCR
	chdr (21:21) = CHLF
C*
	RETURN
	END
