	SUBROUTINE MA_CGWD ( string, parm, iret )
C************************************************************************
C* MA_CGWD                                                              *
C*                                                                      *
C* This subroutine decodes the wind or swell direction in a report.     *
C* The values are stored in common /rintfv/.				*
C*                                                                      *
C* MA_CGWD  ( STRING, PARM, IRET )                                      *
C*                                                                      *
C* Input parameters:                                                    *
C*      STRING          CHAR*           String containing sky cover data*
C*      PARM            CHAR*           If 'WIND', store decoded dir.   *
C*                                      into irdrct.  If 'SWELL', store *
C*                                      into ir, store into irdosw.     *
C*                                                                      *
C* Output parameters:                                                   *
C*      RIVALS(IRDRCT)  REAL            wind direction                  *
C*      RIVALS(IRDOSW)  REAL            swell direction                 *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return             *
C*                                       -1 = invalid direction         *
C**                                                                     *
C* Log:                                                                 *
C* C. Caruso Magee/NCEP	 4/01   Original Author                         *
C* F. J. Yen/NCEP	 4/01	Rewrote and renamed from CG_GTWD.	*
C* F. J. Yen/NCEP	 7/01	Removed setting rtmp to	RMISSD.		*
C*				(S.Chiswell/Unidata)			*
C* D. Kidwell/NCEP	 3/02	Put initializations in DATA statements  *
C************************************************************************
	INCLUDE		'macmn.cmn'
C*
	CHARACTER*(*)	string, parm
C*
	LOGICAL		fnddir
	CHARACTER	idir ( 16 ) * 3
	INTEGER		nopdir (0:3) 
	REAL		dirdeg (16)
C*
	DATA		idir	/'N', 'NNE',  'NE', 'ENE',
     +		        	 'E', 'ESE',  'SE', 'SSE', 
     +				 'S', 'SSW',  'SW', 'WSW',
     +				 'W', 'WNW',  'NW', 'NNW' / 
	DATA		nopdir  / 3,   1,     3,    2  / 
	DATA		dirdeg  /   0.0,  22.5,  45.0,  67.5,
     +				   90.0, 112.5, 135.0, 157.5,
     +				  180.0, 202.5, 225.0, 247.5,
     +				  270.0, 292.5, 315.0, 337.5 /
C------------------------------------------------------------------------
	iret = 0
	fnddir = .false.
	i = 1
	DO WHILE ( .not. fnddir .and. i .le. 16 )
	    j = mod ( i, 4)
	    IF ( string .eq. idir ( i ) (:nopdir(j)) ) THEN
		rtmp = dirdeg ( i )
		fnddir = .true.
	    END IF
	    i = i + 1
	END DO
	IF ( .not. fnddir ) THEN
	    IF ( string .eq. 'VRB' .and. parm .eq. 'WIND') THEN
		rtmp = -99.0
	      ELSE
		logmsg = ' Invalid wind or swell direction!'
		CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
		iret = -1
		RETURN
	    END IF 
	END IF
	IF ( parm .eq. 'WIND') THEN
	    rivals(irdrct) = rtmp
	  ELSE IF ( parm .eq. 'SWELL') THEN
	    rivals(irdosw(1)) = rtmp
	END IF
C*
	RETURN
	END
