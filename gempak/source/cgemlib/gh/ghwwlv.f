	SUBROUTINE GH_WWLV ( jseg, jvtec, jbkseg, itype, mseg, mvtec, 
     +			     mbkseg, iret )
C************************************************************************
C* GH_WWLV                                                              *
C*									*
C* This subroutine looks for and regroups overlapping VTEC segments to  *
C* eliminate redundant breakpoint information.  It works on lists of    *
C* points, not pairs.                                                   *
C*                                                                      *
C* GH_WWLV ( JSEG, JVTEC, JBKSEG, ITYPE, MSEG, MVTEC, MBKSEG, IRET )    *
C*           								*
C* Input parameters:                                                    *
C*	JSEG		INTEGER		Number of segments		*
C*	JVTEC(3,*)	INTEGER		VTEC action & event code values *
C*	JBKSEG(*)	INTEGER		Breakpoint numbers for segments *
C*	ITYPE		INTEGER		Type of check                   *
C*					  1 = overlapping HU.A and TR.W *
C*					  2 = overlapping NEW and CAN   *
C*									*
C* Output parameters:                                                   *
C*	MSEG		INTEGER		Number of segments              *
C*	MVTEC(3,*)	INTEGER		VTEC action & event code values *
C*	MBKSEG(*)	INTEGER		Breakpoint numbers for segments *
C*	IRET		INTEGER		Return code                     *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	 2/05	From GH_WWOV 
C************************************************************************
     	INTEGER		jvtec (3,*), jbkseg (*), mvtec (3,*), 
     +			mbkseg (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Load the work arrays.
C
	DO jj = 1, jseg
	    DO ii = 1, 3
		mvtec ( ii, jj )  = jvtec ( ii, jj )
	    END DO
	    mbkseg ( jj ) = jbkseg ( jj )
	END DO
C
C*	Get the VTEC codes to check, depending on the type.
C
	IF ( itype .eq. 1 ) THEN
	    icd1 = 2
	    icd2 = 10
	    incr = 4
	    ncd1 = 3
	    ncd2 = 7
	  ELSE
	    icd1 = 1
	    icd2 = 4
	    incr = 1
	    ncd1 = 9
	    ncd2 = 12
	END IF
C
C*	Look for overlapping segments and regroup as needed.
C
	DO icd = icd1, icd2, incr
	  IF ( itype .eq. 1 ) THEN
	    IF ( icd .eq. 6 ) THEN
		ncd2 = 11
	      ELSE IF ( icd .eq. 10 ) THEN
		ncd1 = 7
	    END IF
	  END IF
	  DO ncd = ncd1, ncd2, incr
	    IF ( ncd .ne. ( icd + 8 ) ) THEN
	      DO ii = 1, jseg
		IF ( jvtec ( 1, ii ) .eq. icd ) THEN
		    DO jj = 1, jseg
			IF ( jvtec ( 1, jj ) .eq. ncd ) THEN
C
C*			    Compare the points.
C
			    IF ( jbkseg ( ii ) .eq. 
     +					  jbkseg ( jj ) ) THEN
C
C*				The segments have the same point.  Zero
C*				out one of them for later removal and
C*				add to the VTEC code list for the 
C*				segment which remains.
C
	    			DO kk = 1, 3
				    mvtec ( kk, jj )  = 0
	    			END DO
				mbkseg ( jj ) = 0
				IF ( mvtec ( 2, ii ) .eq. 0 ) THEN
				    mvtec ( 2, ii ) = ncd
				    IF ( itype .eq. 2 ) THEN
				       IF ( jvtec ( 2, jj ) .ne. 0 )
     +					 mvtec ( 3,ii ) = jvtec ( 2,jj )
				    END IF
				  ELSE
				    mvtec ( 3, ii ) = ncd
				END IF
			    END IF
			END IF
		    END DO
		END IF
	      END DO
	    END IF
	  END DO
	END DO
C
C*	Check to see if any identical segments need to be dropped.
C
	mseg = jseg
	ii = 1
	DO WHILE ( ii .le. mseg )
	    IF ( mbkseg ( ii ) .eq. 0 ) THEN
		mseg = mseg - 1
		DO jj = ii, mseg
		    jj1 = jj + 1
		    DO kk = 1, 3
			mvtec ( kk, jj )  = mvtec ( kk, jj1 )
		    END DO
		    mbkseg ( jj ) = mbkseg ( jj1 )    
		END DO
	      ELSE
		ii = ii + 1
	    END IF
 	END DO
C*
	RETURN
	END
