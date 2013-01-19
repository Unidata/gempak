       SUBROUTINE GH_WWVC ( ibeg, iend, clist, zlist, bkpstr, ivtec,
     +			    iret )
C************************************************************************
C* GH_WWVC                                                              *
C*                                                                      *
C* This subroutine looks for segments with the same UGC lists and the   *
C* same breakpoint text strings, but different VTECS, and merges them   *
C* into a single segment.                                               *
C*                                                                      *
C* GH_WWVC ( IBEG, IEND, CLIST, ZLIST, BKPSTR, IVTEC, IRET )   		*
C*                                                                      *
C* Input parameters:                                                    *
C*      IBEG		INTEGER	 Beginning segment index 		*
C*	IEND		INTEGER  Ending segment index                   *
C*      CLIST(NSEG)	CHAR*	 Lists of county UGCs by segment	*
C*      ZLIST(NSEG)	CHAR*	 Lists of marine zone UGCs by segment	*
C*      BKPSTR(NSEG)	CHAR*	 Breakpoint text strings by segment	*
C*                                                                      *
C* Input and output parameters:                                         *
C*      IVTEC(3,NSEG)	INTEGER	 VTEC action and watch/warning code 	*
C*				 values by segment			*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* D. Kidwell/NCEP	 9/05	                                        *
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
	INCLUDE		'ghcmn.cmn'
C*
	PARAMETER	( MAXSEG = 50 )
C*
	CHARACTER*(*) 	clist (*), zlist (*), bkpstr (*)
	INTEGER		ivtec (3,*)
C*
	INTEGER 	lenc (MAXSEG), lenz (MAXSEG), lenb (MAXSEG)
	LOGICAL		match
C-----------------------------------------------------------------------
        iret  = 0
C
C*	Get the string lengths.
C
	DO ii = ibeg, iend
	    CALL ST_LSTR ( clist ( ii ), lenc ( ii ), ier )
	    CALL ST_LSTR ( zlist ( ii ), lenz ( ii ), ier )
	    IF ( ( lenc ( ii ) .lt. 6 ) .and. ( lenz ( ii ) .lt. 6 ) )
     +		   ivtec ( 1, ii ) = 0
	    CALL ST_LSTR ( bkpstr ( ii ), lenb ( ii ), ier )
	END DO
C
C*	Loop over segments with non-zero VTEC values.
C
	DO ii = ibeg, iend - 1
	    IF ( ivtec ( 1, ii ) .gt. 0 ) THEN
		DO jj = ii + 1, iend
		    IF ( ivtec ( 1, jj ) .gt. 0 ) THEN
			match = .true.
			IF ( ( lenc ( ii ) .gt. 0 ) .and.
     +			     ( lenc ( jj ) .gt. 0 ) ) THEN
			    IF ( ( clist ( ii ) ( :lenc (ii) ) ) .ne.
     +				 ( clist ( jj ) ( :lenc (jj) ) ) ) THEN
				match = .false.
			    END IF
			END IF
			IF ( ( lenz ( ii ) .gt. 0 ) .and.
     +			     ( lenz ( jj ) .gt. 0 ) ) THEN
			    IF ( ( zlist ( ii ) ( :lenz (ii) ) ) .ne.
     +				 ( zlist ( jj ) ( :lenz (jj) ) ) ) THEN
				match = .false.
			    END IF
			END IF
			IF ( ( lenb ( ii ) .gt. 0 ) .and.
     +			     ( lenb ( jj ) .gt. 0 ) ) THEN
			    IF ( ( bkpstr ( ii ) ( :lenb (ii) ) ) .ne.
     +				 ( bkpstr ( jj ) ( :lenb (jj) ) ) ) THEN
				match = .false.
			    END IF
			END IF
		        IF ( match ) THEN
C
C*			    A match was found on counties, marine zones,
C*			    and breakpoint text strings.  Make sure 
C*			    there is room to merge the VTECs.
C
			    icount = 2
			    DO kk = 2, 3
				IF ( ivtec ( kk, ii ) .gt. 0 ) 
     +				     icount = icount + 1
				IF ( ivtec ( kk, jj ) .gt. 0 ) 
     +				     icount = icount + 1
			    END DO
			    IF ( icount .le. 3 ) THEN
C
C*				Merge the VTECs and zero out the second
C*				one.
C
				IF ( ivtec ( 2, ii ) .eq. 0 ) THEN
				    DO kk = 2, 3
					ivtec ( kk, ii ) = 
     +					        ivtec ( kk - 1, jj )
				    END DO
				  ELSE
				    ivtec ( 3, ii ) = ivtec ( 1, jj ) 
				END IF
				DO kk = 1, 3
				    ivtec ( kk, jj ) = 0
				END DO
			    END IF
			END IF
		    END IF
		END DO
	    END IF
	END DO
C*
	RETURN
	END
