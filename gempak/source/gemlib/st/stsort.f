	SUBROUTINE ST_SORT ( itype, nstr, inpstr, nout, outstr, iret )
C************************************************************************
C* ST_SORT								*
C*									*
C* This subroutine sorts a list of strings. The output list may be	*
C* sorted forward or backward, and may contain only the unique entries.	*
C* The input and output arrays may be the same.				*
C*									*
C* ST_SORT  ( ITYPE, NSTR, INPSTR, NOUT, OUTSTR, IRET )			*
C*									*
C* Input parameters:							*
C*	ITYPE		INTEGER		Type of sort			*
C*					   1 = Forward			*
C*					  -1 = Backward			*
C*					   2 = Forward, unique only	*
C*					  -2 = Backward, unique only	*
C*	NSTR		INTEGER		Number of input strings		*
C*	INPSTR (NSTR)	CHAR*		Input strings			*
C*									*
C* Output parameters:							*
C*	NOUT		INTEGER		Number of output strings	*
C*	OUTSTR (NOUT)	CHAR*		Sorted strings			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/01	Copied from TI_SORT			*
C************************************************************************
	CHARACTER*(*) 	inpstr (*), outstr (*)
C*
	CHARACTER 	swpbuf*160
C------------------------------------------------------------------------
	iret  = 0
	istop = nstr - 1
C
C*	Load output array.
C
	DO  ii = 1, nstr
	    outstr (ii) = inpstr (ii)
	END DO
C
C*	Perform bubble sort.
C
	iswflg = 1
	DO WHILE  ( ( iswflg .ne. 0 )  .and.  ( istop .ge. 1 ) )
	  iswflg = 0
C 
	  DO  i = 1, istop
	    IF  ( outstr (i) .gt. outstr (i+1) )  THEN
	      iswflg       = 1
 	      swpbuf       = outstr (i)
	      outstr  (i)  = outstr (i+1)
	      outstr (i+1) = swpbuf
	    ENDIF
	  END DO
	  istop = istop-1
	END DO
C
C*	If the sort order is backward, reverse the array.
C
	IF  ( itype .lt. 0 )  THEN
	    jj = nstr
	    DO  ii = 1, nstr/2
		swpbuf      = outstr (ii)
		outstr (ii) = outstr (jj)
		outstr (jj) = swpbuf
		jj = jj - 1
	    END DO
	END IF
C
C*	If the user has requested, return only unique entries.
C
	IF  ( ABS(itype) .eq. 2 )  THEN
	    jj = 1
	    DO  ii = 2, nstr
	    	IF  ( outstr (ii) .ne. outstr (jj) )  THEN
		    jj = jj + 1
		    outstr (jj) = outstr (ii)
		END IF
	    END DO
	    nout = jj
	    DO  ii = nout+1, nstr
	    	outstr (ii) = ' '
	    END DO
	  ELSE
	    nout = nstr
	END IF
C*
	RETURN
	END
