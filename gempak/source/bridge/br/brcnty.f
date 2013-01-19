	SUBROUTINE BR_CNTY  ( cnties, county, ncnty, iret )
C************************************************************************
C* BR_CNTY								*
C*									*
C* This subroutine decodes the county information given in the header   *
C* of the weather warnings. Assume that the station id is a string of	*
C* 3 letters and the county number is a string of 3 numbers.		*
C*									*
C* Note: the output county array from the calling subroutine needs	*
C* to be dimensioned to LLSTFL						*
C*									*
C* BR_CNTY ( CNTIES, COUNTY, NCNTY, IRET )				*
C*									*
C* Input parameters:							*
C*	CNTIES		CHAR*		County station id               *
C*									*
C* Output parameters:							*
C*	COUNTY (NCNTY)	CHAR*6		County names in warning area    *
C*	NCNTY		INTEGER		Number of warning counties      *
C*	IRET		INTEGER		Return code			*
C*					= -1 - Invalid string found	*
C**									*
C* Log:									*
C* M. Li/SAIC		08/02 	Modified from WN_CNTY			*
C* D. Kidwell/NCEP	11/02	Added checks for invalid substring refs *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)   cnties, county (*)
C*
	CHARACTER 	stid*3, chid*3
	INTEGER 	ncnty
C------------------------------------------------------------------------
	iret = 0
        DO i = 1, LLSTFL
            county (i) = ' '
        END DO
	stid = ' '
C
C*	Search the station name and ID
C*
	CALL ST_LSTR ( cnties, lenc, ier )
	ncnty = 0
	ii = 1
	DO WHILE ( ii .le. lenc ) 
	    CALL ST_ALNM ( cnties(ii:ii), itype1, ier )
C
C*	    If the character is a letter
C
	    IF ( itype1 .eq. 2 ) THEN
		IF ( ii .gt. 1 ) THEN
		    CALL ST_ALNM ( cnties(ii-1:ii-1), itype, ier )
		  ELSE
		    itype = 0
		END IF
		CALL ST_ALNM ( cnties(ii+1:ii+1), itype2, ier )
		CALL ST_ALNM ( cnties(ii+2:ii+2), itype3, ier ) 
		CALL ST_ALNM ( cnties(ii+3:ii+3), itype4, ier )
C
C*		Valid station ID found
C
		IF ( itype2 .eq. 2 .and. itype3 .eq. 2 .and. 
     +		     itype4 .ne. 2 .and. itype  .ne. 2 ) THEN
		    stid = cnties(ii:ii+2)
		    ii = ii + 3
C
C*		Search next valid station ID
C	
		ELSE
		    iret = -1
		    ii = ii + 1

		    kk = 0
                    CALL ST_ALNM ( cnties(ii:ii), itype, ier )
                    jj = ii + 1
                    DO WHILE ( itype .ne. 2 .and. jj .le. lenc )
                        kk = kk + 1
                        CALL ST_ALNM ( cnties(jj:jj), itype, ier )
                        jj = jj + 1
                    END DO
		    ii = ii + kk
		END IF
C
C*	    If the character is a number, check for county # (3 number together)
C
	    ELSE IF ( itype1 .eq. 1 ) THEN 		
		IF ( ii .gt. 1 ) THEN
		    CALL ST_ALNM ( cnties(ii-1:ii-1), itype, ier )
		  ELSE
		    itype = 0
		END IF
		CALL ST_ALNM ( cnties(ii+1:ii+1), itype2, ier )
                CALL ST_ALNM ( cnties(ii+2:ii+2), itype3, ier )
		CALL ST_ALNM ( cnties(ii+3:ii+3), itype4, ier )
		CALL ST_LSTR ( stid, lenid, ier )
C
		IF ( itype2 .eq. 1 .and. itype3 .eq. 1 .and. 
     +		     itype4 .ne. 1 .and. itype  .ne. 1 .and.
     +		     lenid  .eq. 3 ) THEN           
                    chid = cnties(ii:ii+2)              
		    ncnty = ncnty + 1
		    county (ncnty) = stid // chid
                    ii = ii + 3                 
                ELSE            
                    iret = -1          
		    ii = ii + 1
                END IF          		
	    ELSE
C
C*	    Other character 
C
		IF ( cnties(ii:ii) .eq. '>' ) THEN
		    kk = 0
		    ii = ii + 1
		    CALL ST_ALNM ( cnties(ii:ii), itype, ier )
		    jj = ii + 1
		    DO WHILE ( itype .eq. 1 .and. kk .lt. 4 )
			kk = kk + 1
			CALL ST_ALNM ( cnties(jj:jj), itype, ier )	
			jj = jj + 1
		    END DO
C
		    IF ( ( ii .ge. 6 ) .and. ( kk .ge. 1 ) ) THEN
		        CALL ST_NUMB ( county (ncnty)(4:6), ibeg, ier )
		        CALL ST_NUMB ( cnties (ii:ii+kk-1), iend, ier )
		        CALL ST_ALNM ( cnties(ii-2:ii-2), itype2, ier )
                        CALL ST_ALNM ( cnties(ii-3:ii-3), itype3, ier )
                        CALL ST_ALNM ( cnties(ii-4:ii-4), itype4, ier )
                        CALL ST_ALNM ( cnties(ii-5:ii-5), itype5, ier )
		        CALL ST_LSTR ( stid, lenid, ier )
		    
C
C*		        Check for valid range
C
		        IF ( ibeg .gt. 0 .and. ibeg .lt. iend .and. 
     +			     kk .eq. 3 .and. itype2 .eq. 1 .and. itype3
     +   		     .eq. 1 .and. itype4 .eq. 1 .and. itype5
     +			     .ne. 1 ) THEN
			    DO icnt = ibeg+1, iend
			        CALL ST_INCH ( icnt, chid, ier )
                                CALL ST_LSTR ( chid, len, ier )
                                IF ( len .eq. 2 ) chid = '0' // chid
                                IF ( len .eq. 1 ) chid = '00' // chid
			        ncnty = ncnty + 1
			        county (ncnty) = stid//chid
			    END DO
			    ii = ii + kk
		          ELSE IF ( kk .eq. 3 .and. lenid .eq. 3 ) THEN
			    ncnty = ncnty + 1
                            county (ncnty) = stid // cnties (ii:ii+kk-1)
			    ii = ii + kk
		          ELSE
			    iret = -1
		        END IF
		      ELSE
			iret = -1 
		    END IF
		END IF
	    	ii = ii + 1
	    END IF
	END DO
C*
	RETURN
	END
