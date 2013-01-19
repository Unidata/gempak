	SUBROUTINE WP_NOWT ( bultin, lenbul, wtype, strtim, endtim, 
     +			     wnum, iret )
C************************************************************************
C* WP_NOWT 								*
C*									*
C* This subroutine decodes a 'NO WATCHES CURRENTLY ACTIVE' report.	*
C*                                                                      *
C* WP_NOWT ( BULTIN, LENBUL, WTYPE, STRTIM, ENDTIM, WNUM, IRET )	*
C*									*
C* Input parameters:	                                                *
C*      BULTIN          CHAR*           'No Watches' bulletin		*
C*	LENBUL          INTEGER         Length of bulletin		*
C*									*
C* Output parameters:							*
C*	WTYPE	  	CHAR*  	  	Watch type (TN, TS, NA)		*
C*	STRTIM	  	CHAR* 	  	Watch start time--GEMPAK format	*
C*	ENDTIM	  	CHAR* 	  	Watch stop time--GEMPAK format	*
C*	WNUM		CHAR*	  	Watch number			*
C*	IRET  	  	INTEGER	  	Return code			*
C*				  	  0 = normal return		*
C*					  1 = no 'NO WATCHES' string	*
C*					  2 = no 'UTC'; format error	*
C*					  3 = date format error		*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 4/05						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	bultin, wtype, strtim, endtim, wnum
C
	CHARACTER	carr (5)*10, dtarr (3)*3, tmarr (3)*2,
     +			month (12)*3, ichmon*2	  
	DATA            month /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     +			       'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/
	LOGICAL		found
C------------------------------------------------------------------------
	iret  = 0
C
C*	Verify that bulletin has string 'NO WATCHES CURRENTLY ACTIVE'	
C
	ipt = INDEX ( bultin, 'NO WATCHES CURRENTLY ACTIVE' )
	IF ( ipt .eq. 0 ) THEN
	    iret = 1
	  ELSE
C
C*	    Get start and stop times
C
	    ipt = INDEX ( bultin, 'UTC' )
	    IF ( ipt .eq. 0 ) THEN
	        iret = 2
	      ELSE
	        CALL ST_CLST ( bultin, ' ', ' ', 5, carr, numc, ier )
		
C
C*		Break up DD-MMM-YY string
C
		CALL ST_CLST ( carr(3), '-', ' ', 3, dtarr, numc ,ier)
		found = .false.
		ii    = 1
        	DO WHILE ( ( ii .le. 12 ) .and. ( .not. found ) )
            	    IF ( dtarr ( 2 ) ( :3 ) .eq. month ( ii ) ) THEN
                	imonth = ii
                	found  = .true.
              	      ELSE
                	ii = ii + 1
            	    END IF
        	END DO
        	IF ( .not. found ) THEN
	    	    iret = 4
		  ELSE
		    IF ( imonth .gt. 9 ) THEN
		        CALL ST_INCH ( imonth, dtarr (2), ier )
		      ELSE
			CALL ST_INCH ( imonth, ichmon, ier )
			dtarr (2) = '0' // ichmon // ' '
		    END IF
C
C*		    Break up HH:MM:SS string
C
		    CALL ST_CLST ( carr(5), ':', ' ', 3, tmarr, numc ,ier)
C
C*		    Create GEMPAK date/time string
C
		    strtim = dtarr (3) (1:2) // dtarr (2) (1:2) //
     +			 dtarr (1) (1:2) // '/' // tmarr (1) // tmarr (2)
      		    endtim = strtim
	            wtype = 'NA'
	            wnum = ' '
		END IF
	    END IF
	END IF
C*
	RETURN
	END
