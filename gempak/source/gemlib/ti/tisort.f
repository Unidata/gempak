	SUBROUTINE TI_SORT ( ntime, timin, outime, iret )
C************************************************************************
C* TI_SORT								*
C*									*
C* This subroutine sorts a list of times from earliest to latest.  	*
C* The input and output arrays may be the same.				*
C*									*
C* TI_SORT  ( NTIME, TIMIN, OUTIME, IRET )				*
C*									*
C* Input parameters:							*
C*	NTIME		INTEGER		Number of times 		*
C*	TIMIN  (NTIME)	CHAR*		GEMPAK times 			*
C*									*
C* Output parameters:							*
C*	OUTIME (NTIME)	CHAR*		Sorted times			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 3/84	from HR_TIMSRT				*
C* M. desJardins/GSFC	 6/84	Corrected				*
C* M. desJardins/GSFC	11/87	Eliminated error returns		*
C* I. Graffman/RDS	12/87	Removed time pointers                   *
C* D. Kidwell/NCEP	 3/99	Fixed for Y2K                           *
C************************************************************************
	CHARACTER*(*) 	timin (*), outime (*)
C*
	CHARACTER 	swpbuf*30
C------------------------------------------------------------------------
	iret  = 0
	istop = ntime - 1
C
C*	Load output array.
C
	DO  ii = 1, ntime
	    outime (ii) = timin (ii)
	END DO
C
C*	Perform bubble sort.
C
	iswflg = 1
	DO WHILE  ( ( iswflg .ne. 0 )  .and.  ( istop .ge. 1 ) )
	  iswflg = 0
C 
	  DO  i = 1, istop
	    IF  ( outime (i) .gt. outime (i+1) )  THEN
	      iswflg       = 1
 	      swpbuf       = outime (i)
	      outime  (i)  = outime (i+1)
	      outime (i+1) = swpbuf
	    ENDIF
	  END DO
	  istop = istop-1
	ENDDO
C 
C*	Make sure 20th century precedes 21st.
C
	CALL TI_YYYY ( ntime, outime, outime, ier )
C*
	RETURN
	END
