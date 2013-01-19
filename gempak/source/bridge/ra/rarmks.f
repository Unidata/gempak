	SUBROUTINE RA_RMKS ( ipnaft, remark, ovrrun, ipraft, iret)
C************************************************************************
C*  RA_RMKS								*
C*									*
C*  This subroutine finds the end of the REMARKS field, and returns	*
C*  the next position.  NOTE:  This routine handles the special remarks *
C*  cases of VSBY, HLSTO, SNOINCR, PRJMP, and PK WND			*
C*									*
C*  RA_RMKS  ( IPNAFT, REMARK, OVRRUN, IPRAFT, IRET )			*
C*									*
C*  Input parameters:							*
C*	IPNAFT		INTEGER		First field after altimeter	*
C*	REMARK		LOGICAL		Flag for remarks present	*
C*									*
C*  Output parameters:							*
C*	OVRRUN		LOGICAL		Flag for over running end of rpt*
C*	IPRAFT		INTEGER		First field after remarks	*
C*	IRET		INTEGER		Return code			*
C*					    0 =  normal			*
C*					   -1 =  error in decoding	*
C**									*
C* Log:									*
C* P. Bruehl/Unidata	11/94						*
C* L. Oolman/UWYO	 4/96	Fixed check for 2 "/" before REMARK end	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'racmn.cmn'
C*
	LOGICAL		remark, done, ovrrun
	LOGICAL		remslash
C-----------------------------------------------------------------------
	iret     = 0
	done     = .false.
	ovrrun   = .false.
	remark   = .false.
C
C*	Skip remarks. 
C*	Look for a "/" to indicate end of remarks
C
	    DO WHILE ( .not. done )
		IF ( ipnaft .gt. nfield )  THEN
		    done = .true.
		    ovrrun = .true.
		ELSE IF ( iftype (ipnaft) .eq. 3 )  THEN
C
C*		End of REMARKS field - Check for another one
C
		    done = .true.
		    ipnaft = ipnaft + 1
		    IF ( iftype (ipnaft) .eq. 1 ) done = .false.
C
		ELSE IF (cfield(ipnaft) .eq. "PRJMP" ) THEN
C
C*		Special REMARKS case "PRJMP n/n/n"
C*		Allow for 2 extra "/"'s before end of REMARKS field
C
		 islash = 0
		 DO WHILE ( ( islash .lt. 2 ) .and. ( .not. done ) )
		   remslash = .false.
            	   DO WHILE ( .not. remslash  )
                    IF ( ipnaft .gt. nfield )  THEN
			remslash = .true.
                    	done = .true.
                  	ovrrun = .true.
                    ELSE IF ( iftype (ipnaft) .eq. 3 )  THEN
                  	 remslash = .true.
                         ipnaft = ipnaft + 1
			 islash = islash + 1
                    ELSE
                    	ipnaft = ipnaft + 1
                    END IF
                   END DO
	         END DO	
C
     		ELSE IF ( (cfield(ipnaft) .eq. "PK") .or.
     +			( cfield(ipnaft) .eq. "PKWND") ) THEN
C
C*		Special REMARKS case "PK WND n/n"
C*		Allow for 1 extra "/" before end of REMARKS field
C
		   remslash = .false.
            	   DO WHILE ( .not. remslash  )
                    IF ( ipnaft .gt. nfield )  THEN
			remslash = .true.
                    	done = .true.
                  	ovrrun = .true.
                    ELSE IF ( iftype (ipnaft) .eq. 3 )  THEN
                  	 remslash = .true.
                         ipnaft = ipnaft + 1
                    ELSE
                    	ipnaft = ipnaft + 1
                    END IF
                   END DO
C
		ELSE IF ( (cfield(ipnaft) .eq. "VSBY") .or.
     +			  (cfield(ipnaft) .eq. "HLSTO") ) THEN
C
C*		Special REMARKS case of "VSBY xxn/n" or "HLSTO n/n" 
C*		(may not have a "/")  Find next "/" & check following 
C*		field for small "n". 
C
		   remslash = .false.
            	   DO WHILE ( .not. remslash  )
                    IF ( ipnaft .gt. nfield )  THEN
			remslash = .true.
                    	done = .true.
                  	ovrrun = .true.
                    ELSE IF ( iftype (ipnaft) .eq. 3 )  THEN
C
C*		Slash - check next number, is it a denominator?
C
                  	 remslash = .true.
			 ipnaft = ipnaft + 1
			 IF ( iftype (ipnaft) .eq. 2 ) THEN
		  	    idenom = ifintg (ipnaft)
			    IF ( idenom .lt. 100 ) THEN
C
C*		Denominator - still in REMARKS field 
C*		Skip ahead and keep looking for end.
C
			      ipnaft = ipnaft + 1
C*		If VSBY, check for "V" and then possibly another /
			      IF ( (iftype (ipnaft) .eq. 1) .and.
     +			 	(cfield(ipnaft) .eq. "V") ) THEN
				remslash = .false.
				ipnaft = ipnaft + 1
			      END IF
			    ELSE
C
C*		Not a denominator - back off one and keep decoding
C
			      ipnaft = ipnaft - 1
		 	    ENDIF
			 ENDIF
                    ELSE
                    	ipnaft = ipnaft + 1
                    END IF
                   END DO
C
		ELSE IF (cfield(ipnaft) .eq. "SNOINCR") THEN
C
C*		Special REMARKS cases "SNOINCR n/n/n" or "SNOINCR n"
C*		Must handle both the 2"/"'s in REMARKS or no "/"'s
C*		in REMARKS cases!
C
		   remslash = .false.
            	   DO WHILE ( .not. remslash  )
                    IF ( ipnaft .gt. nfield )  THEN
			remslash = .true.
                    	done = .true.
                  	ovrrun = .true.
                    ELSE IF ( iftype (ipnaft) .eq. 3 )  THEN
C
C*		Slash - check next number, is it a small number?
C*		(depth increase since last 6-hourly)
C
                  	 remslash = .true.
			 ipnaft = ipnaft + 1
			 IF ( iftype (ipnaft) .eq. 2 ) THEN
		  	    idepth = ifintg (ipnaft)
			    IF ( idepth .lt. 100 ) THEN
C
C*		idepth small - still in REMARKS field 
C*		Skip ahead and keep looking for second slash
C
			      ipnaft = ipnaft + 1
		              remslash = .false.
            	              DO WHILE ( .not. remslash  )
                               IF ( ipnaft .gt. nfield )  THEN
				remslash = .true.
 	                   	done = .true.
        	          	ovrrun = .true.
                               ELSE IF (iftype (ipnaft) .eq. 3) THEN
	                  	remslash = .true.
        	                ipnaft = ipnaft + 1
               		       ELSE
                    		ipnaft = ipnaft + 1
                    	       END IF
                              END DO
C
			    ELSE
C
C*		Not a depth, out of REMARK - back off one 
C
			      ipnaft = ipnaft - 1
		 	    ENDIF

			 ENDIF
                    ELSE
                    	ipnaft = ipnaft + 1
                    END IF
                   END DO
C
		ELSE
C
C*		Still in REMARKS field, keep checking
C
		    ipnaft = ipnaft + 1
		END IF
	    END DO
	ipraft = ipnaft
	RETURN
	END
