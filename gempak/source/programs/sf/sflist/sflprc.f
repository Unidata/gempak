	SUBROUTINE SFLPRC  ( iflno, chrflg, ncprm, cprm, nlun, luns,
     +			     times, ntime,  keynam, iret )
C************************************************************************
C* SFLPRC								*
C*									*
C* This subroutine processes the data to be listed.			*
C*									*
C* SFLPRC  ( IFLNO, CHRFLG, NCPRM, CPRM, NLUN, LUNS, TIMES, NTIME, 	*
C*	     KEYNAM, IRET )						*
C*									*
C* Input parameters:							*
C* 	IFLNO		INTEGER		File number			*
C*	CHRFLG (NCPRM)	LOGICAL		Character flag			*
C*	NCPRM		INTEGER		Number of parameters		*
C*	CPRM   (NCPRM)	CHAR*4		Parameter names			*
C*	NLUN		INTEGER		Number of output devices	*
C*	LUNS   (NLUN)	INTEGER		LUNs of output devices		*
C*	TIMES  (*)	CHAR*		Times to process		*
C*	NTIME		INTEGER		Number of times			*
C*	KEYNAM		CHAR*		STID or STNM			*
C*									*
C* Output parameters:							*
C*	IRET 		INTEGER		Error code			*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* I. Grafman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	10/87	Rewritten				*
C* M. desJardins/GSFC	11/89	Changed station time to integer		*
C* K. Brill/NMC		 8/93	Changes for 8-char ID			*
C* D. Keiser/GSC	 4/96	Added some blank lines below data	*
C* S. Jacobs/NCEP	 6/96	Added blank lines to all output devices	*
C* A. Hardy/GSC		 3/99	Added priority parameter to SF_SNXT     *
C* A. Hardy/GSC		 3/99	Removed ispri = 0, added ispri SFLDTA	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		luns (*)
	LOGICAL		chrflg (*)
	CHARACTER*(*)	times (*), cprm (*), keynam
C*
	CHARACTER	stid*8, stn*8, stn2*8, cdata (MMPARM)*8
	REAL		rdata (MMPARM)
	LOGICAL		done, fnd
C------------------------------------------------------------------------
	iret = 0
	fnd  = .false.
	CALL SFLHDR  ( nlun, luns, ncprm, cprm, ier ) 
C
C*	Loop through times.
C
	DO  itim = 1, ntime
C
C*	    Set the requested time.
C
	    CALL SF_STIM  ( iflno, times ( itim ), ier )
C
C*	    Loop through file looking for stations.
C	
	    done = .false.
	    DO WHILE  ( .not. done )
C
C*	        Get next station.
C
	        CALL SF_SNXT (iflno, stid, istnm, rlat, rlon, elev, 
     +			      ispri, ier)
C
C*	        Set flag if there are no more stations.
C
	        IF  ( ier .ne. 0 )  THEN
	            done = .true.
C
C*	            Otherwise, get data.
C
	          ELSE
	            CALL SFLDTA  ( iflno, stid, istnm, rlat, rlon, 
     +				   elev, ispri, rdata, cdata, ier )
C
C*		    Write data to output units.
C
		    IF  ( ier .eq. 0 )  THEN
			fnd = .true.
C
C*			Compute station identifier.
C
			IF  ( keynam .eq. 'STID' )  THEN
			    stn = stid
			  ELSE
			    CALL ST_INCH  ( istnm, stn, ier )
			END IF
			CALL ST_LSTR  ( stn, lens, ier )
			IF (( lens .gt. 0 ) .and. ( lens .lt. 6 )) THEN
			    stn2 = ' '
			    stn2 ( 7 - lens : ) = stn
			  ELSE
			    stn2 = stn
			END IF
			CALL SFLLIS ( nlun,  luns, chrflg, ncprm,
     +				      stn2,   times ( itim ), 
     +				      rdata, cdata, ier )
		    END IF
	        END IF	        
	    END DO
	END DO
	DO  j = 1, nlun
	    WRITE (luns(j),*) 
	    WRITE (luns(j),*)
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
