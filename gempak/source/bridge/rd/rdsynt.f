	SUBROUTINE RD_SYNT ( gemftm, jf6, jftmst, jftmen, rdata,
     +			     gemstm, ps12tb, iret )
C************************************************************************
C* RD_SYNT								*
C*									*
C* This subroutine converts the GEMPAK date/time array into synoptic	*
C* times.  It also sets up the position moving table for 12-hour	*
C* parameters.								*
C*									*
C* RD_SYNT  ( GEMFTM, JF6, JFTMST, JFTMEN, RDATA, GEMSTM, PS12TB, IRET )*
C*									*
C* Input parameters:							*
C*      GEMFTM (*)	CHAR*		GEMPAK date/time		*
C*      JF6		INTEGER		Index of 1st hour in extended pt*
C*      JFTMST		INTEGER		Index of 1st valid GEMFTM	*
C*									*
C* Input and output parameters:						*
C*      JFTMEN		INTEGER		Index of last GEMFTM		*
C*      RDATA		REAL		Forecast data			*
C*	   ( MMPARM, * )						*
C*									*
C* Output parameters:							*
C*      GEMSTM (*)	CHAR*		GEMPAK Synoptic date/time	*
C*      PS12TB (*)	INTEGER		Position moving table for 12-hr *
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = Bad fcst hrs in 2nd part * 
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 9/02						*
C* F. J. Yen/NCEP	11/02	Use actual time, not 3-hr syn for table;*
C*                          	handled overflow for 12-hr parameter.	*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	gemftm(*), gemstm (*)
	INTEGER		ps12tb (*)
	REAL		rdata ( MMPARM, * )
C*
	CHARACTER	gemttm*15, gem6tm*15
	INTEGER		idtarr (5), idtars (5), idfovf (5)
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize position moving table for 12-hour parameters
C
	DO ii = 1, jftmen + 1
	    ps12tb ( ii ) = 0
	END DO
C
C*	Convert the GEMPAK date/time array into synoptic times.
C*	First convert the first part to 3-hour synoptic times.
C*	Also set up the 12 hour position moving table ps12tb.
C
	IF ( jf6 .eq. 0 ) THEN
	    j1 = jftmen
	  ELSE
	    j1 = jf6 - 1
	END IF
	DO ii = jftmst, j1
C
C*	    Convert to 3-hour synoptic times
C
	    CALL TI_CTOI ( gemftm (ii), idtarr, ier )
	    DO j = 1, 5
		idtars ( j ) = idtarr ( j )
	    END DO
	    irm3 = MOD ( idtarr (4), 3)
	    IF ( irm3 .eq. 1 ) THEN
		CALL TI_SUBM ( idtarr, 60, idtarr, ier )
	      ELSE IF ( irm3 .eq. 2 ) THEN
		CALL TI_ADDM ( idtarr, 60, idtarr, ier )
	    END IF
	    CALL TI_ITOC ( idtarr, gemstm ( ii ), ier)
C
C*	    Set up position moving table
C
	    irm6 = MOD ( idtars (4), 6)
	    IF ( irm6 .eq. 2) THEN
		ps12tb ( ii ) = -1
	      ELSE IF ( irm6 .eq. 3 .or. irm6 .eq. 4 ) THEN
		ps12tb ( ii ) = 1
	    END IF
	END DO
	jf6n = jf6
	jf6c = jf6
	IF ( jf6 .ne. 0 .and. jf6. le. jftmen ) THEN
C
C*	    Have extended part; convert to 6-hour synoptic times.
C*	    Convert the first fcst hour in the extended section.
C
	    CALL TI_CTOI ( gemftm (jf6), idtarr, ier )
	    irm6 = MOD ( idtarr (4), 6 )
	    IF ( irm6 .ge. 3 ) THEN
		im = (6 - irm6) * 60
		CALL TI_ADDM ( idtarr, im, idtarr, ier)
	      ELSE IF ( irm6 .ne. 0 ) THEN
		im = irm6 * 60
		CALL TI_SUBM ( idtarr, im, idtarr, ier)
	    END IF
	    CALL TI_ITOC ( idtarr, gemttm, ier )
C
C*	    Check if have overflow.  There is overflow when
C*	    the 12-hour parameters in the last forecast hour
C*	    of the first section are to be moved to the next
C*	    (non-existent) 3-hour forecast hour.
C
	    IF ( ps12tb ( j1 ) .eq. 1 ) THEN
C
C*		There is overflow; determine the 6 hour
C*		synoptic time of the overflow and 
C*		compare with the first 6 hour synoptic
C*		time of the extended section.
C
		CALL TI_CTOI ( gemftm (j1), idfovf, ier )
	        irm6 = MOD ( idfovf (4), 6 )
	        IF ( irm6 .ge. 3 ) THEN
		    im = (6 - irm6) * 60
		    CALL TI_ADDM ( idfovf, im, idfovf, ier)
	          ELSE IF ( irm6 .ne. 0 ) THEN
		    im = irm6 * 60
		    CALL TI_SUBM ( idfovf, im, idfovf, ier)
	        END IF
		CALL TI_ITOC ( idfovf, gem6tm, ier )
		CALL TI_DIFF ( gemttm, gem6tm, mindif, ier)
		IF ( mindif .gt. 0 ) THEN
C
C*		    Make room for the overflow
C
		    gemstm ( jf6n ) = gem6tm
		    jf6n = jf6n + 1
		    gemstm ( jf6n ) = gemttm
		    jftmen = jftmen + 1
		    DO mm = jftmen, jf6n, -1 
			DO mp = 1, MMPARM
			    rdata ( mp, mm ) = rdata ( mp, mm-1 )
			END DO
		    END DO
		    DO mp = 1, MMPARM
			rdata ( mp, mm ) = RMISSD
		    END DO
		  ELSE IF ( mindif .eq. 0 ) THEN
		    gemstm (jf6n) = gemttm
		  ELSE
C
C*		    Error in forecast time for second section
C
		    iret = -1
		    RETURN
		END IF
	      ELSE
		    gemstm (jf6n) = gemttm
	    END IF

	    IF ( jf6c .lt. jftmen ) THEN
	        jf6n = jf6n + 1
	        DO ii = jf6n, jftmen
		    jf6c = jf6c + 1
		    CALL TI_CTOI ( gemftm (jf6c), idtarr, ier )
		    irm6 = MOD ( idtarr (4), 6 )
		    IF ( irm6 .ge. 3 ) THEN
		        im = (6 - irm6) * 60
		        CALL TI_ADDM ( idtarr, im, idtarr, ier)
		      ELSE IF ( irm6 .ne. 0 ) THEN
		        im = irm6 * 60
		        CALL TI_SUBM ( idtarr, im, idtarr, ier)
		    END IF
		    CALL TI_ITOC ( idtarr, gemstm (ii), ier)
	        END DO
	    END IF
	  ELSE IF ( ps12tb ( j1 ) .eq. 1 ) THEN
C
C*	    Overflow, but no extended section
C
            gemstm ( jf6 ) = gem6tm
            jftmen = jftmen + 1
	END IF
C*
	RETURN
	END
