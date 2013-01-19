	SUBROUTINE SFL6PR  ( iflno, cmpflg, nlun, luns, times, ntime, 
     +			     skpmis, keynam, iret )
C************************************************************************
C* SFL6PR								*
C*									*
C* This subroutine processes the data to be listed.			*
C*									*
C* SFL6PR  ( IFLNO,  CMPFLG, NLUN, LUNS, TIMES, NTIME, SKPMIS, 		*
C*           KEYNAM, IRET )						*
C*									*
C* Input parameters:							*
C* 	IFLNO		INTEGER		File number			*
C*	CMPFLG (*)	LOGICAL		Compute flag			*
C*	NLUN		INTEGER		Number of output devices	*
C*	LUNS  (NLUN)	INTEGER		LUNs of output devices		*
C*	TIMES (*)	CHAR*	 	List of times			*
C*	NTIME		INTEGER		Number of times to process	*
C*	SKPMIS		LOGICAL		Skip missing?			*
C*	KEYNAM		CHAR*		STID or STNM			*
C*									*
C* Output parameters:							*
C*	IRET 		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = error			*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/84						*
C* M. desJardins/GSFC	10/86	Added GEMPAK parameter names		*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	10/87	Added skip missing data flag		*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* M. desJardins/GSFC	11/89	Added SFPARM to set conditions		*
C* K. Brill/NMC		 8/93	Change 8-char ID			*
C* K. Tyle/GSC		 1/97	Call ER_LMSG				*
C* A. Hardy/GSC	 	 3/99	Added priority parameter to PC_SSTN     *
C* A. Hardy/GSC	 	 3/99	Added priority parameter to SF_SNXT     *
C* A. Hardy/GSC	 	 3/99	Removed ispri = 0 			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		luns (*)
	LOGICAL		cmpflg (*), skpmis
	CHARACTER*(*)	times (*),  keynam
C*
	LOGICAL		fnd, done, datflg, dayflg
	REAL		rdata (MMPARM)
	CHARACTER	stid*8, cdata (MMPARM)*8, stn*8
	REAL		data (MMPARM) 
C------------------------------------------------------------------------
C*	Write out title.
C
	fnd = .false.
	CALL SFL6HD  ( nlun, luns, ier ) 
C
C*	Loop through times.
	dayflg = .false.
	DO  itim = 1, ntime
C
C*	    Set the requested time and reset pointers in the file.
C
	    CALL SF_STIM  ( iflno, times (itim), ier )
C
C*	    Loop through file looking for stations.
C
	    done = .false.
	    DO WHILE  ( .not. done )
C
C*		Get next station.
C
		CALL SF_SNXT  ( iflno, stid, istnm, rlat, rlon, elev, 
     +			        ispri, ier )
C
C*		Set flag if there are not more stations.
C
		IF  ( ier .ne. 0 )  THEN
		    done = .true.
C
C*		    Otherwise, print data.
C
		  ELSE
C
C*		    Read data.  Compute data only if there is no error.
C
		    CALL SF_RDAT  ( iflno, data, ihhmm, iret )
		    IF  ( iret .eq. 0 )  THEN
C
C*			Set requested station and get parameters.
C
			CALL PC_SSTN  ( stid, istnm, rlat, rlon, elev, 
     +					ispri, ihhmm, 1, iret )
			CALL PC_CMLV  ( 1, data, rdata, cdata, iret )
		    END IF
C
C*		    Skip missing data.
C
		    IF  ( ( iret .eq. 0 ) .or. ( .not. skpmis ) )  THEN
			IF  ( iret .eq. 0 )  THEN
			    datflg = .true.
			  ELSE
			    datflg = .false.
			END IF
C
C*			Compute station identifier.
C
			IF  ( keynam .eq. 'STID' )  THEN
			    stn = stid
			  ELSE
			    CALL ST_INCH  ( istnm, stn, ier )
			END IF
C*
			CALL SFL6LI  ( nlun, luns, stn, cmpflg,
     +				       times (itim), rdata, 
     +				       cdata, datflg, dayflg, ier )
			fnd = .true.
		    END IF
	        END IF	        
	    END DO
	END DO		
C
C*	Write message if no stations were found.
C
	IF  ( .not.  fnd )  THEN
	    iret = -3
	    CALL ER_LMSG ( 0, 'SFDSL604', iret, ' ', ier ) 
	  ELSE
	    iret = 0
	END IF
C*
	RETURN 
	END
