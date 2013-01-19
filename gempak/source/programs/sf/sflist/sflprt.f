	SUBROUTINE SFLPRT  ( iflno, nlun, luns, times, ntime,
     +			     tflg, sflg, iret )
C************************************************************************
C* SFLPRT								*
C*									*
C* This subroutine processes and writes text data.			*
C*									*
C* SFLPRT  ( IFLNO, NLUN, LUNS, TIMES, NTIME, TFLG, SFLG, IRET )	*
C*									*
C* Input parameters:							*
C* 	IFLNO		INTEGER		File number			*
C*	NLUN		INTEGER		Number of output devices	*
C*	LUNS   (NLUN)	INTEGER		LUNs of output devices		*
C*	TIMES  (*)	CHAR*		Times to process		*
C*	NTIME		INTEGER		Number of times			*
C*	TFLG		LOGICAL		Text data flag			*
C*	SFLG		LOGICAL		Special data flag		*
C*									*
C* Output parameters:							*
C*	IRET 		INTEGER		Error code			*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* D. Keiser/GSC	 4/96						*
C* S. Jacobs/NCEP	 6/96	Cleaned up calling sequence; added 	*
C*				listing of special reports		*
C* K. Tyle/GSC		 1/97	Print a max of 256 chars for TEXT	*
C* D. Kidwell/NCEP      10/98   Indent special report listing 2 spaces  *
C* A. Hardy/GSC		 3/99	Added priority parameter to SF_SNXT     *
C* A. Hardy/GSC		 3/99	Removed ispri = 0 			*
C* S. Jacobs/NCEP	 8/01	Concatenated specials >80 characters	*
C* A. Hardy/SAIC	 3/02	Removed max of 256 chars for TEXT	*
C* T. Lee/SAIC		 2/03	Prepend STID & time to text output	*
C* D. Kidwell/NCEP	11/05   Allowed for up to 30 specials           *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		luns (*)
	CHARACTER*(*)	times (*)
	LOGICAL		tflg, sflg
C*
	CHARACTER	stid*8, idsav*5, timsav*7 
	CHARACTER	string*2400, spstr(30)*160, tstr*160
	LOGICAL		done, fnd, prnt
C------------------------------------------------------------------------
	iret = 0
	fnd  = .false.
C
C*	Loop through times.
C
	DO  itim = 1, ntime
C
C*	    Set the requested time.
C
	    CALL SF_STIM  ( iflno, times ( itim ), ier )
	    timsav = times ( itim ) ( 5:6 ) // times ( itim ) ( 8: )
C
C*	    Loop through file looking for stations.
C	
	    prnt = .false.
	    done = .false.
	    DO WHILE  ( .not. done )
C
C*	        Get next station.
C
	        CALL SF_SNXT (iflno, stid, istnm, rlat, rlon, elev, 
     +		              ispri, ier)
		idsav  = stid ( :4 )
C
C*	        Set flag if there are no more stations.
C
	        IF  ( ier .ne. 0 )  THEN
	            done = .true.
C
C*	            Otherwise, get data.
C
	        ELSE
		    IF  ( tflg )  THEN
			CALL SF_RSTR  ( iflno, string, ihhmm, lenstr,
     +					ier ) 
C
C*		        Write data to output units.
C
			IF  ( ier .eq. 0 )  THEN
			    ip = INDEX ( string, 'REPORT AT' )
			    fnd = .true.
			    prnt = .true.
			    IF  ( ip .eq. 0 )  THEN
				DO  j = 1, nlun
				   WRITE (luns(j),1000) string (:lenstr)
				END DO
			    ELSE
				DO  j = 1, nlun
                                    WRITE (luns(j),1000) idsav, timsav,
     +                                                  string (:lenstr)
                                END DO
			    END IF
 1000			    FORMAT ( A, A, A )
			END IF
		    END IF
		    IF  ( sflg )  THEN
			CALL SF_RSPC  ( iflno, string, ihhmm, lenstr,
     +					nrep, ier ) 
C
			IF  ( ier .eq. 0 )  THEN
			    fnd  = .true.
			    prnt = .true.
C
C*			    Reconstruct each special report.
C
			    nsp  = 0
			    stid = string(1:4)
			    DO  k = 1, nrep
				indx = (k-1) * 80
				CALL ST_LSTR ( string(indx+1:indx+80),
     +					       jlen, ier )
     				IF  ( string(indx+1:indx+4) .eq. stid )
     +				    				THEN
				    nsp = nsp + 1
				    spstr(nsp) =
     +					    string(indx+1:indx+jlen)
				  ELSE
				    IF  ( nsp .gt. 0 )  THEN
				    	CALL ST_LSTR ( spstr(nsp),
     +						       lensp, ier )
				    	tstr = spstr(nsp)(1:lensp) //
     +					    string(indx+1:indx+jlen)
				    	spstr(nsp) = tstr
				    END IF
     				END IF
			    END DO
C
C*			    Write data to output units.
C
			    DO  j = 1, nlun
				DO  k = 1, nsp
				    CALL ST_LSTR ( spstr(k),
     +						   jlen, ier )
				    WRITE ( luns(j), 2000 )
     +						spstr(k)(1:jlen)
				END DO
 2000			        FORMAT ( 2X, A )
			    END DO
			END IF
		    END IF
	        END IF	        
	    END DO
	    IF  ( prnt )  THEN
		DO  j = 1, nlun
		    WRITE (luns(j), *) ' '
		END DO
	    END IF
	END DO
C
C*	Write error if no stations were found.
C
	IF  ( .not. fnd )  THEN
	    CALL ER_WMSG  ( 'SFLIST', -3, ' ', ier )
	    RETURN
	END IF
C*
	RETURN 
	END
