	SUBROUTINE SNSRGE  ( vcord, nstn, stndat, nlvls, ipsdat, 
     +			     toptmp, topwnd, iret )
C************************************************************************
C* SNSRGE								*
C*									*
C* This subroutine finds the top value of the vertical coordinate	*
C* for temperature and wind data.					*
C*									*
C* SNSRGE  ( VCORD, NSTN, STNDAT, NLVLS, IPSDAT, TOPTMP, TOPWND, 	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	VCORD		CHAR*4		Vertical coordinate		*
C* 	NSTN		INTEGER		Number of stations		*
C*	STNDAT (*)	REAL		Station data buffer		*
C*	NLVLS (NSTN)	INTEGER		Number of levels		*
C*	IPSDAT (NSTN)	INTEGER		Pointers to data in STNDAT	*
C*									*
C* Output parameters:							*
C*	TOPTMP (NSTN)	REAL		Minimum pressure with temp data	*
C*	TOPWND (NSTN)	REAL		Minimum pressure with wind data	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -8 = pres or temp not available*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C* M. desJardins/GSFC	 9/90	Add vcord				*
C* J. Whistler/SSAI	 2/91	Removed error check			*
C* A. Hardy/GSC		 3/99   Added priority parameter to PC_SSTN     *
C* J. Wu/GSC             7/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	vcord*4
	REAL		stndat (*), toptmp (*), topwnd (*)
	INTEGER		nlvls (*), ipsdat (*)
C*
	CHARACTER	parm (3)*4, stns*4
	REAL		outdat (3)
	LOGICAL		chrflg (3), cmpflg (3), tmpflg, wndflg
	CHARACTER	chrdat (3)*8
	INCLUDE 	'ERMISS.FNC'
C*
	DATA		parm / 'PRES', 'TEMP', 'SPED' /
	DATA		stns /'STNS'/
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Make vertical coordinate the first parameter to compute.
C
	parm (1) = vcord
C
C*	Set up PC library to return pressure, temperature and wind.
C
	CALL PC_DFLV  ( 3, parm, chrflg, cmpflg, npm, ier )
C
C*	Find the top temperature and wind data for each station.
C
	DO  i = 1, nstn
            ispri = 0
	    CALL PC_SSTN ( stns, 0, 0., 0., 0., ispri, IMISSD, 
     +			   nlvls (i), ier )
	    tmpflg = .false.
	    wndflg = .false.
	    toptmp (i) = RMISSD
	    topwnd (i) = RMISSD
	    lev = nlvls (i)
	    DO WHILE ( ( (.not. tmpflg) .or. (.not. wndflg) ) .and.
     +			 ( lev .ge. 1 ) ) 
		CALL PC_CMLV ( lev, stndat (ipsdat (i) ), outdat,
     +			       chrdat, ier )
		IF ( ( .not. tmpflg ) .and. 
     +		     ( .not. ERMISS (outdat (2) ) ) )  THEN
		    toptmp (i) = outdat (1)
		    tmpflg = .true.
		END IF
		IF  ( ( .not. wndflg ) .and. 
     +		      ( .not. ERMISS ( outdat (3) ) ) )  THEN
		    topwnd (i) = outdat (1)
		    wndflg = .true.
		END IF
		lev = lev - 1
	    END DO
	END DO
C*
	RETURN
	END
