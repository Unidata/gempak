       SUBROUTINE GH_WWDI ( nseg, idxare, clist, zlist, ivtec, bkpstr,
     +			    iret )
C************************************************************************
C* GH_WWDI                                                              *
C*                                                                      *
C* This subroutine checks the segments in the islands geographic area   *
C* (IOTHER) for Dry Tortugas Island.  If found, it moves it to the water*
C* geographic area (IWATER) to simplify later VTEC priority checks.     *
C*                                                                      *
C* GH_WWDI ( NSEG, IDXARE, CLIST, ZLIST, IVTEC, BKPSTR, IRET )    	*
C*                                                                      *
C* Input parameters:                                                    *
C*      NSEG		INTEGER	 Number of segments			*
C*                                                                      *
C* Input and output parameters:                                         *
C*	IDXARE(5)	INTEGER  Beginning seg. index for each geog area*
C*				 (IUSGEC,IOTHER,IPRICO,IWATER,IKEYS)    *
C*      CLIST(NSEG)	CHAR*	 Lists of county UGCs by segment	*
C*      ZLIST(NSEG)	CHAR*	 Lists of marine zone UGCs by segment	*
C*      IVTEC(3,NSEG)	INTEGER	 VTEC action and watch/warning code 	*
C*				 values by segment			*
C*      BKPSTR(NSEG)	CHAR*	 Breakpoint text strings by segment	*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* D. Kidwell/NCEP	10/05	                                        *
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
     	CHARACTER*(*)	clist(*), zlist(*), bkpstr(*)
	INTEGER		ivtec(3,*), idxare (*)
C*
	INTEGER 	ivdry (3)
        CHARACTER	cdry*1000, zdry*1000, bkpdry*605
	LOGICAL 	done
C------------------------------------------------------------------------
	iret = 0
C
C* 	Get the beginning and ending indices for the IOTHER geographic
C*	area.
C
	ibeg = idxare ( 2 )
	IF ( idxare ( 3 ) .ne. 0 ) THEN
	    iend = idxare ( 3 ) - 1
	  ELSE IF ( idxare ( 4 ) .ne. 0 ) THEN
	    iend = idxare ( 4 ) - 1
	  ELSE IF ( idxare ( 5 ) .ne. 0 ) THEN
	    iend = idxare ( 5 ) - 1
	  ELSE
	    iend = nseg
	END IF
C
C*	Look for a segment containing Dry Tortugas Island data and save
C*	it to temporary arrays.  This data has a 'GMZ' marine zone.
C
	ii   = ibeg
	done = .false.
	DO WHILE ( .not. done )
	    idry = INDEX ( zlist ( ii ), 'GMZ' )
	    IF ( idry .gt. 0 ) THEN
		done = .true.
		cdry = clist ( ii )
		zdry = zlist ( ii ) 
		DO jj = 1, 3
		    ivdry ( jj ) = ivtec ( jj, ii )
		END DO
		bkpdry = bkpstr ( ii )	
		idry   = ii
	      ELSE
		ii = ii + 1
		IF ( ii .gt. iend ) done = .true.
	    END IF
	END DO
C
	IF ( idry .gt. 0 ) THEN
C
C*	    Recalculate the beginning indices for areas IOTHER, IPRICO 
C*	    and IWATER.
C
	    IF ( iend .eq. ibeg ) idxare ( 2 ) = 0
	    IF ( idxare ( 3 ) .gt. 0 ) idxare (3) = idxare (3) - 1
	    IF ( idxare ( 4 ) .gt. 0 ) THEN
		idxare ( 4 ) = idxare ( 4 ) - 1
	      ELSE 
		IF ( idxare ( 5 ) .ne. 0 ) THEN
		    idxare ( 4 ) = idxare ( 5 ) - 1
		  ELSE
		    idxare ( 4 ) = nseg
	        END IF
	    END IF
C
C*	    Move the segments following Dry Tortugas up one position in
C*	    the arrays.
C
	    ibeg = idry
	    IF ( idxare ( 5 ) .ne. 0 ) THEN
		iend = idxare ( 5 ) - 1
	      ELSE
		iend = nseg
	    END IF
C
	    DO ii = ibeg, iend - 1
		clist ( ii ) = clist ( ii + 1 )
		zlist ( ii ) = zlist ( ii + 1 )
		DO jj = 1, 3
		    ivtec ( jj, ii ) = ivtec ( jj, ii + 1 )
		END DO
		bkpstr ( ii ) = bkpstr ( ii + 1 ) 
	    END DO
C
C*	    Move the Dry Tortugas Island data into the IWATER area.
C
	    clist ( iend ) = cdry
	    zlist ( iend ) = zdry
	    DO jj = 1, 3
	        ivtec ( jj, iend ) = ivdry ( jj )
	    END DO
	    bkpstr ( iend ) = bkpdry
	END IF
C*
	RETURN
	END
