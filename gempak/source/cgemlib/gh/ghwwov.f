	SUBROUTINE GH_WWOV ( jseg, jvtec, jbkseg, itype, mseg, mvtec, 
     +			     mbkseg, iret )
C************************************************************************
C* GH_WWOV                                                              *
C*									*
C* This subroutine looks for and regroups overlapping VTEC segments to  *
C* eliminate redundant breakpoint information.                          *
C*                                                                      *
C* GH_WWOV ( JSEG, JVTEC, JBKSEG, ITYPE, MSEG, MVTEC, MBKSEG, IRET )    *
C*           								*
C* Input parameters:                                                    *
C*	JSEG		INTEGER		Number of segments		*
C*	JVTEC(3,*)	INTEGER		VTEC action & event code values *
C*	JBKSEG(2,*)	INTEGER		Breakpoint numbers for segments *
C*	ITYPE		INTEGER		Type of check                   *
C*					  1 = overlapping HU.A and TR.W *
C*					  2 = overlapping NEW and CAN   *
C*									*
C* Output parameters:                                                   *
C*	MSEG		INTEGER		Number of segments              *
C*	MVTEC(3,*)	INTEGER		VTEC action & event code values *
C*	MBKSEG(2,*)	INTEGER		Breakpoint numbers for segments *
C*	IRET		INTEGER		Return code                     *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	11/03						*
C* M. Onderlinde/NHC    11/16           Fixed bug when part of TR.W and *
C*                                      HU.A are cancelled but the TR.W *
C*                                      VTEC cancellation line is not   *
C*                                      added to the TCV                *
C*                                                                      *
C************************************************************************
     	INTEGER		jvtec (3,*), jbkseg (2,*), mvtec (3,*), 
     +			mbkseg (2,*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Load the work arrays.
C
	DO jj = 1, jseg
	    DO ii = 1, 2
		mvtec ( ii, jj )  = jvtec ( ii, jj )
		mbkseg ( ii, jj ) = jbkseg ( ii, jj )
	    END DO
	    mvtec ( 3, jj ) = jvtec ( 3, jj )
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
	mm    = jseg
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
C*			    Compare the endpoints.
C
 			    IF ( ( jbkseg (2,ii) .le. jbkseg (1,jj) )
     +			         .or.
     +				 ( jbkseg (2,jj) .le. jbkseg (1,ii) ) )
     +			         THEN
C
C*				There is no overlap.  Change nothing.
C
			      ELSE IF ( ( jbkseg ( 1, ii ) .eq. 
     +					  jbkseg ( 1, jj ) ) .and.
     +					( jbkseg ( 2, ii ) .eq.
     +					  jbkseg ( 2, jj ) ) ) THEN
C
C*				The segments are identical.  Zero out
C*				one of them for later removal.
C
	    			DO kk = 1, 2
				    mvtec ( kk, jj )  = 0
				    mbkseg ( kk, jj ) = 0
	    			END DO
	    			mvtec ( 3, jj ) = 0
				IF ( mvtec ( 2, ii ) .eq. 0 ) THEN
				    mvtec ( 2, ii ) = ncd
				    IF ( itype .eq. 2 ) THEN
				       IF ( jvtec ( 2, jj ) .ne. 0 )
     +					 mvtec ( 3,ii ) = jvtec ( 2,jj )
				    END IF
				  ELSE

C
C*                                  Matt Onderlinde - Nov 2016
C*                                  Bug fix here for missing TR.W cancellation VTEC line
C
C*                                  mvtec ( 3, ii ) = ncd  ---> Notice this line now commented out
C*                                                              and instead listed inside ELSE statement 7 lines below
C
                                    IF ( mvtec(3,ii) .eq. 11 .and. 
     +                                   ncd .eq. 10 ) THEN
                                        mvtec (3,ii) = mvtec (3,ii)
                                    ELSE
				        mvtec ( 3, ii ) = ncd
                                    END IF

				END IF
			      ELSE IF ( jbkseg ( 1, ii ) .eq.
     +					jbkseg ( 1, jj ) ) THEN
C
C*				The first endpoint is the same.
C
      				IF ( jbkseg ( 2, ii ) .lt. 
     +				     jbkseg ( 2, jj ) ) THEN
				    IF ( mvtec ( 2, ii ) .eq. 0 ) THEN
				        mvtec ( 2, ii ) = ncd
				        IF ( itype .eq. 2 ) THEN
				           IF ( jvtec ( 2, jj ) .ne. 0 )
     +					     mvtec (3,ii) = jvtec (2,jj)
				        END IF
				      ELSE
				        mvtec ( 3, ii ) = ncd
				    END IF
				    mbkseg ( 1, jj ) = jbkseg ( 2, ii )
				  ELSE
				    mbkseg ( 1, ii ) = jbkseg ( 2, jj )
				    IF ( mvtec ( 2, jj ) .eq. 0 ) THEN
				        mvtec ( 2, jj ) = icd
				        IF ( itype .eq. 2 ) THEN
				           IF ( jvtec ( 2, ii ) .ne. 0 )
     +					     mvtec (3,jj) = jvtec (2,ii)
				        END IF
				      ELSE
				        mvtec ( 3, jj ) = icd
				    END IF
				END IF
			      ELSE IF ( jbkseg ( 2, ii ) .eq.
     +					jbkseg ( 2, jj ) ) THEN
C
C*				The second endpoint is the same.
C
      				IF ( jbkseg ( 1, ii ) .lt. 
     +				     jbkseg ( 1, jj ) ) THEN
				    mbkseg ( 2, ii ) = jbkseg ( 1, jj )
				    IF ( mvtec ( 2, jj ) .eq. 0 ) THEN
				        mvtec ( 2, jj ) = icd
				        IF ( itype .eq. 2 ) THEN
				           IF ( jvtec ( 2, ii ) .ne. 0 )
     +					     mvtec (3,jj) = jvtec (2,ii)
				        END IF
				      ELSE
				        mvtec ( 3, jj ) = icd
				    END IF
				  ELSE
				    IF ( mvtec ( 2, ii ) .eq. 0 ) THEN
				        mvtec ( 2, ii ) = ncd
				        IF ( itype .eq. 2 ) THEN
				           IF ( jvtec ( 2, jj ) .ne. 0 )
     +					     mvtec (3,ii) = jvtec (2,jj)
				        END IF
				      ELSE
				        mvtec ( 3, ii ) = ncd
				    END IF
				    mbkseg ( 2, jj ) = jbkseg ( 1, ii )
				END IF
			      ELSE
C
C*				There is an overlap, with no endpoints
C*				the same.
C
				mm = mm + 1
				mvtec ( 1, mm ) = icd
				mvtec ( 2, mm ) = ncd
				mvtec ( 3, mm ) = 0
C
C*				Check for adding a third VTEC code.
C
				IF ( itype .eq. 2 ) THEN
				    IF ( jvtec ( 2, ii ) .ne. 0 ) THEN
					mvtec ( 3,mm ) = jvtec ( 2,ii )
				      ELSE IF ( jvtec ( 2, jj ) .ne. 0 )
     +					        THEN
					mvtec ( 3,mm ) = jvtec ( 2,jj )
				    END IF
				END IF
C
				IF ( jbkseg ( 1, ii ) .lt. 
     +				     jbkseg ( 1, jj ) ) THEN
				    IF ( jbkseg ( 2, ii ) .lt.
     +					 jbkseg ( 2, jj ) ) THEN
					mbkseg (1,jj) = jbkseg (2,ii)
					mbkseg (2,mm) = jbkseg (2,ii)
				      ELSE
					mbkseg (1,jj) = jbkseg (2,jj)
					mbkseg (2,jj) = jbkseg (2,ii)
					mvtec (1,jj)  = icd
					IF ( itype .eq. 2 ) THEN
     					    mvtec (2,jj) = jvtec (2,ii)
					END IF
					mbkseg (2,mm) = jbkseg (2,jj)
				    END IF
				    mbkseg ( 2, ii ) = jbkseg ( 1, jj )
				    mbkseg ( 1, mm ) = jbkseg ( 1, jj )
				  ELSE
				    IF ( jbkseg ( 2, jj ) .lt.
     +					 jbkseg ( 2, ii ) ) THEN
					mbkseg (1,ii) = jbkseg (2,jj)
					mbkseg (2,mm) = jbkseg (2,jj)	
				      ELSE
					mbkseg (1,ii) = jbkseg (2,ii)
					mbkseg (2,ii) = jbkseg (2,jj)
					mvtec (1,ii)  = ncd
					IF ( itype .eq. 2 ) THEN
     					    mvtec (2,ii) = jvtec (2,jj)
					END IF
					mbkseg (2,mm) = jbkseg (2,ii)
				    END IF
				    mbkseg ( 2, jj ) = jbkseg ( 1, ii )
				    mbkseg ( 1, mm ) = jbkseg ( 1, ii )
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
C*	Check to see if any identical segments, or any spurious single-
C*	point segments, need to be dropped.
C
	mseg = mm
	ii = 1
	DO WHILE ( ii .le. mseg )
	    IF ( mbkseg ( 1, ii ) .eq. mbkseg ( 2, ii ) ) THEN
		mseg = mseg - 1
		DO jj = ii, mseg
		    jj1 = jj + 1
		    DO kk = 1, 2
			mbkseg ( kk, jj ) = mbkseg ( kk, jj1 )    
			mvtec ( kk, jj )  = mvtec ( kk, jj1 )
		    END DO
		    mvtec ( 3, jj ) = mvtec ( 3, jj1 )
		END DO
	      ELSE
		ii = ii + 1
	    END IF
 	END DO
C*
	RETURN
	END
