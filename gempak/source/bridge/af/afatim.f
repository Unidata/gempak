	SUBROUTINE AF_ATIM  ( isdx, maxsc, istim, ietim, iret )
C************************************************************************
C* AF_ATIM								*
C*									*
C* This subroutine locates, decodes, and stores the time (HHMM) data	*
C* from	within an AIREP report.  The report must have already been	*
C* broken up into "like-type" groups using subroutine AF_BKGP.		*
C* Output includes the indices of the "like-type" groups which contain	*
C* the start and the end of the time data; these values are set		*
C* to IMISSD if	the search for the data was unsuccessful.		*
C*									*
C* AF_ATIM  ( ISDX, MAXSC, ISTIM, IETIM, IRET )				*
C*									*
C* Input parameters:							*
C*	ISDX		INTEGER		Index of "like-type" group with	*
C*					which to begin search for data	*
C*	MAXSC		INTEGER		Maximum number of "like-type"	*
C*					groups to search following ISDX	*
C*									*
C* Output parameters:							*
C*	ISTIM		INTEGER		Index of "like-type" group which*
C*					contains start of data		*
C*	IETIM		INTEGER		Index of "like-type" group which*
C*					contains end of data		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	INCLUDE		'affnc.fnc'
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret = 0
	istim = IMISSD
	ietim = IMISSD
C
C*	Locate the time data.  It is identifiable as a four-digit
C*	numeric "like-type" group that was not concatenated with any
C*	other "like-type" groups in the original report.
C
	ii = isdx
	maxii = IEDX ( ii, maxsc, nflds )
C
	DO WHILE  (  ( ( ii + 1 ) .le. maxii ) .and.
     +			( istim .eq. IMISSD )  )
	    IF  (  ( itypsf ( ii ) .eq. NMR ) .and.
     +		   ( lensf ( ii ) .eq. 4 )  )  THEN
		IF  ( ( irfnsf ( ii - 1 ) .ne. irfnsf ( ii ) ) .and.
     +		      ( irfnsf ( ii + 1 ) .ne. irfnsf ( ii ) ) )  THEN
C
C*		    The time data has been found.
C
		    istim = ii
		END IF
	    END IF
	    ii = ii + 1
	END DO
C
	IF  ( istim .ne. IMISSD )  THEN
	    ietim = istim
C
C*	    Decode and store the time data.
C
	    CALL AF_HHMM  ( fields ( istim ) ( 1 : lensf ( istim ) ),
     +			    iertim )
	END IF
C*
	RETURN
	END
