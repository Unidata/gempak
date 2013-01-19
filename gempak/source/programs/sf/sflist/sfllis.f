	SUBROUTINE SFLLIS  ( nlun,  lun,   chrflg, ncprm, stn, time,
     +			     rdata, cdata, iret )
C************************************************************************
C* SFLLIS								*
C*									*
C* This subroutine writes the data to the requested devices.		*
C*									*
C* SFLLIS  ( NLUN, LUN, CHRFLG, NCPRM, STN, TIME, RDATA, CDATA,		*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	NLUN		INTEGER		Number of output devices	*
C*	LUN (NLUN)	INTEGER		LUNs of output devices		*
C*	CHRFLG (*)	LOGICAL		Character flag			*
C*	NCPRM		INTEGER		Number of output parameters	*
C*	STN		CHAR*		Id of station			*
C*	TIME		CHAR*		Time being processed		*
C*	RDATA (*)	REAL		Real data to output		*
C*	CDATA (*)	CHAR*		Character data to output	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	10/87	Rewritten				*
C* K. Brill/NMC		 8/93	Change for 8-char ID			*
C* S. Maxwell/GSC	 8/96	Fixed format statement spacing		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		lun (*)
	REAL		rdata (*)
	LOGICAL		chrflg (*) 
	CHARACTER*(*)	stn, time, cdata (*)
C*
	CHARACTER	cout (MMPARM) *8, ctemp*8, ccc*8
	CHARACTER	sss*8, ttt*11
C------------------------------------------------------------------------
	iret = 0
C
C*	Encode data in character array.
C
	DO   iparm  =  1, ncprm
	    IF  ( chrflg ( iparm ) )  THEN
C
C*		Character data must be right justified.
C
	        ccc  =  cdata (iparm)
		CALL ST_LSTR ( ccc, isiz, ier )
		IF  ( ( isiz .gt. 0 ) .and. ( isiz .lt. 8 ) )  THEN
		    ctemp = ' '
		    ctemp ( 9-isiz : 8 ) = ccc ( 1: isiz )
		  ELSE
		    ctemp = ccc
		END IF
	        cout ( iparm ) = ctemp
	      ELSE
C
C*		Encode real data.
C
		WRITE ( cout (iparm), 1000, IOSTAT = ier ) rdata (iparm)
1000		FORMAT ( F8.2 )
		IF  ( ier .ne. 0 )  cout (iparm) = ' '
	    END IF	
	END DO
C
C*	Write data to requested luns until all good data has been 
C*	processed.
C
	sss = stn
	ttt = time
	DO  ilun = 1, nlun
	    istart = 1
	    iend   = MIN0 ( 6, ncprm )
	    WRITE ( lun ( ilun ), 2000 )  sss, ttt,
     +	          ( cout ( ippos ), ippos = istart, iend )
2000	    FORMAT ( 1X, A, 2X, A11, 1X, 6 ( 1X, A) )
	    istart = iend + 1
	    iend = MIN0 ( istart + 5, ncprm )
	    DO WHILE  ( istart .le. ncprm )
	        WRITE  ( lun ( ilun ), 3000 ) 
     +	               ( cout ( ippos ), ippos = istart, iend )
3000		FORMAT ( 23X, 6 ( 1X, A ) )
	        istart = iend + 1
	        iend = MIN0 ( istart + 5, ncprm )
	    END DO
	END DO
	iret = 0
C*
	RETURN
	END
