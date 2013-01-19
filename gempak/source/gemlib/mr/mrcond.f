	SUBROUTINE MR_COND  ( datman, nman, nlev, stndat, idtype,
     +			      ipt, iret )
C************************************************************************
C* MR_COND								*
C*									*
C* This subroutine adds "underground" mandatory data to the STNDAT	*
C* array.  The data is placed after position one, which holds the	*
C* surface data.  The data is assumed to have been ordered.		*
C*									*
C* MR_COND  ( DATMAN, NMAN, NLEV, STNDAT, IDTYPE, IPT, IRET )		*
C*									*
C* Input parameters:							*
C*	DATMAN (6,NMAN)	REAL		Mandatory data below 100 mb	*
C*	NMAN		INTEGER		Number of man lev below 100 mb	*
C*									*
C* Input and output parameters:						*
C*	NLEV		INTEGER		Number of levels		*
C*	STNDAT (6,NLEV)	REAL		Station data			*
C*	IDTYPE (NLEV)	INTEGER		Data type flags			*
C*					  1 = mandatory			*
C*					  2 = sig temperature		*
C*					  3 = sig wind			*
C*	IPT (NLEV)	INTEGER		Pointers			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		01/92						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	REAL		datman (6,*), stndat (6,*)
	INTEGER		idtype (*), ipt (*)
C
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	If the surface pressure is missing nothing can be done.
C
	IF  ( ERMISS ( stndat ( 1, 1 ) ) )  THEN
	    RETURN
C*
	  ELSE
C
C*	    Find the number of underground levels to add.
C
	    iadd = 0
	    DO  i = 2, nman
	    	IF  ( datman ( 1, i ) .gt. stndat ( 1, 1 ) )
     +		    iadd = iadd + 1
	    END DO
C
C*	    If there is something to add, do so.
C
	    IF ( iadd .ne. 0 ) THEN
C
C*		Move the rest of the data to make room.
C
		nlev = nlev + iadd
		istop = iadd + 2
		DO i = nlev, istop, -1
		    iold = i - iadd
		    DO j = 1, 6
			stndat ( j, i ) = stndat ( j, iold)
		    END DO
		    idtype ( i ) = idtype ( iold )
		END DO
C
C*		Add the underground data.
C	
		istop = 0
	        DO i = 2, nman
		    IF ( datman (1,i) .gt. stndat (1,1) ) THEN
			istop = istop + 1
			indx  = istop + 1
			idtype ( indx ) = 1
			DO j = 1, 6
			    stndat ( j, indx ) = datman ( j, i )
			END DO
			IF ( istop .eq. iadd ) THEN
C
C*			    Update the pointers and return.
C
			    istop = nlev - iadd + 1
			    DO j = istop, nlev
				ipt (j) = i
			    END DO
			    RETURN
			END IF
		    END IF
		END DO
	    END IF
	END IF
	RETURN
	END
