	SUBROUTINE TI_DSPL  ( ntime, timlst, timout, iret )
C************************************************************************
C* TI_DSPL								*
C*									*
C* This subroutine displays a list of times at a terminal and prompts 	*
C* the user with: 'Enter a time or <CR> to page'. It does not wait 	*
C* for a user response.							*
C*									*
C* TI_DSPL  ( NTIME, TIMLST, TIMOUT, IRET )				*
C*									*
C* Input parameters:							*
C*	NTIME		INTEGER		Number of times 		*
C*	TIMLST (NTIME)	CHAR*		List of times			*
C*									*
C* Output parameters:							*
C*	TIMOUT		CHAR*		Time entered by user		*
C*	IRET		INTEGER 	Return code			*
C*					  1 = EXIT entered		*
C*					  0 = normal return		*
C*					 -5 = no times in dataset	*
C**									*
C* Log:									*
C* M. Goodman/RDS	 4/84	Original source code			*
C* M. desJardins/GSFC	 5/84	Changed TM calls & corrected errors	*
C* M. desJardins/GSFC	11/87	Rewrote for GEMPAK4			*
C* M. desJardins/GSFC	 4/90	List times across instead of in cols	*
C************************************************************************
	CHARACTER*(*) 	timlst (*), timout
C*
	CHARACTER*48 	mesage
	LOGICAL  	newlin, pagflg
	DATA		mesage / 'Enter a time or' /, 
     +			newlin / .false. /
C-------------------------------------------------------------------------
	iret = 0
C
C*	Check that there are times in dataset.
C
	IF  ( ntime .le. 0 )  THEN
	    iret = -5
	    RETURN
	END IF
C
C*	Find the number of pages, the number of lines and the number of
C*	times on the last page.
C
	npage = ( ntime - 1 ) / 72 + 1
C
C*	Loop through each of the pages to be printed.
C
	ipage = 1
	DO WHILE  ( ipage .le. npage )
C
C*	    Write title.
C
	    WRITE  ( 6, 1001 ) 
1001	    FORMAT ( / ' List of times: ' / )
C
C*	    Compute first time to be written on this page.
C
	    ibegin = ( ipage - 1 ) * 72 + 1
	    IF  ( ipage .lt. npage )  THEN
		ilast  = ibegin + 72
		pagflg = .true.
	      ELSE
		ilast  = ntime
		pagflg = .false.
		mesage = mesage ( 1 : 12 )
	    END IF
C
C*	    Write the times on this page.
C
	    WRITE  ( 6, 2001 )  ( timlst (i), i = ibegin, ilast )
2001	    FORMAT ( 1X, A15, 3X, A15, 3X, A15, 3X, A15 )
C
C*	    Increment values for next page.
C
	    ipage = ipage + 1
C
C*	    Get the user response from this page after writing blank
C*	    line.
C
	    WRITE ( 6, 3001 )
3001	    FORMAT ( / )
	    CALL TM_STR ( mesage, pagflg, newlin, timout, ier )
C
C*	    Check if a time or EXIT was entered.
C
	    IF  ( ier .eq. 2 )  THEN
		iret = 1
		RETURN
	      ELSE IF  (ier .eq. 0 )  THEN
		iret = ier
		RETURN
	    END IF
	END DO
C*
	RETURN
	END
