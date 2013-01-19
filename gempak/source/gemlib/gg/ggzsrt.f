        SUBROUTINE GG_ZSRT ( cnties, ncnty, iret)
C************************************************************************
C* GG_ZSRT								*
C*                                                                      *
C* This subroutine sorts the county list so that states come before 	*
C* marine zones. The marine zones are sorted alphabetically as well.	*
C*                                                                      *
C*  GG_ZSRT ( CNTIES, NCNTY, IRET )					*
C*                                                                      *
C* Input parameters:                                                    *
C* 	NCNTY		INTEGER		Number of counties/marine zones *
C*                                                                      *
C* Input/Output parameters:                                             *
C*	CNTIES(NCNTY)	CHAR*    	List of counties/marine zones	*
C*                                                                      *
C* Return parameters:                                                   *
C*	IRET		INTEGER		Return Code			*
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* A. Hardy/NCEP	 3/04						*
C* A. Hardy/NCEP	 5/04	Added loop checks			*
C* F. J. Yen/NCEP	 3/07	Replaced arrays dim. 400 with MAX_CNTY.	*
C************************************************************************
        INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ggcmn.cmn'
C*
	CHARACTER*(*)	cnties(*)
C* ----------------------------------------------------------------------
	CHARACTER	mzones(14)*3, tmpcnty(MAX_CNTY)*256,
     +			tmpzons(MAX_CNTY)*256
    	LOGICAL		found1, found2
C*
	DATA		mzones / 'AMZ', 'ANZ', 'GMZ', 'PHZ', 'PKZ', 
     +				 'PMZ', 'PSZ', 'PZZ', 'LHZ', 'LOZ', 
     +				 'LMZ', 'LEZ', 'LSZ', 'LCZ' /
C* ----------------------------------------------------------------------
    	iret = 0
    	found1 = .False.
    	found2 = .False.
C
C*  	Number of marine zones to check.
C
    	imz = 14
C
C* 	Check if either arrays have marine zones.
C
        jj = 1
    	DO WHILE ( ( .not. found1 ) .and. ( jj .le. ncnty ) )
            DO ii = 1, imz
	        IF ( cnties(jj)(1:3) .eq. mzones(ii) ) THEN
	            found1 = .True.
	        END IF	
            END DO
            jj = jj + 1
    	END DO
C
C* 	Check to see if the state ids. are the same.
C
        IF ( .not. found1 ) THEN
            iret = 0
            RETURN 
    	  ELSE 
            jj = 0
            kk = 0
            DO ii = 1, ncnty
                IF (  cnties (ii)(3:3) .eq. 'C' ) THEN
                    jj = jj + 1
		    tmpcnty(jj) = cnties(ii)
                  ELSE IF (  cnties (ii)(3:3) .eq. 'Z' ) THEN
                    kk = kk + 1
		    tmpzons(kk) = cnties(ii)
                END IF
            END DO
        END IF 
C
C* 	Set empty the county array.
C
        DO ii = 1, ncnty
            cnties ( ii ) =  ' '
	END DO
C
C*	Fill in the counties first.
C
        DO ii = 1, jj
            cnties ( ii ) = tmpcnty(ii)
	END DO
        ii = ii - 1
C
C*	Put the marine zones after the counties.
C
        IF ( kk .gt. 0 ) THEN 
            DO jj = 1, kk
                cnties ( ii+jj ) = tmpzons(jj)
            END DO
        END IF
C*
        RETURN 
	END
