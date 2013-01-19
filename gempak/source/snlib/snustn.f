	SUBROUTINE SN_USTN  ( isnfln, stid, istnm, slat, slon, selv, 
     +	                      stat, coun, keynam, iret )
C************************************************************************
C* SN_USTN								*
C*									*
C* This subroutine updates the header information for a station in a	*
C* sounding data file.  This subroutine can only be used if the times 	*
C* and stations are not mixed in row or column headers.			*
C*									*
C* SN_USTN  ( ISNFLN, STID, ISTNM, SLAT, SLON, SELV, STAT, COUN,	*
C*            KEYNAM, IRET )						*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	STID  		CHAR*		Station number or id		*
C*	ISTNM 		INTEGER		Station number			*
C*	SLAT  		REAL		Station latitude		*
C*	SLON  		REAL		Station longitude		*
C*	SELV  		REAL		Station elevation		*
C*	STAT  		CHAR*2	 	State				*
C*	COUN  		CHAR*2		Country				*
C*	KEYNAM		CHAR*4		Key to update (STID or STNM)	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -4 = file not open		*
C*					 -11 = station not in file	*
C*					 -13 = DM error			*
C*				  	 -21 = non-standard file	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC		 8/93	Changes for 8-char ID			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER* (*)	stid, stat, coun, keynam
C*
	LOGICAL		staflg
	CHARACTER	sta*8, key*4
	INTEGER		iheadr (MMKEY), ist (2)
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	CALL SN_CHKF  ( isnfln, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Check that the rows and columns are not intermixed.
C
	IF  ( dttype ( isnfln ) .eq. sttype ( isnfln ) )  THEN
	    iret = -21
	    RETURN
	END IF
C
C*	For station numbers, encode in a string.
C
	IF  ( keynam .eq. 'STNM' )  THEN
	    CALL ST_INCH  ( istnm, sta, ier )
	  ELSE
	    sta = stid
	END IF
C
C*	Determine if station is in file.
C
	CALL SN_CSTN  ( isnfln, sta, ist, key, staflg, irowcl, 
     +			iret )
	IF  ( .not. staflg ) THEN
	    iret = -11
	    RETURN
	END IF
C
C*	Load header array.
C
	CALL ST_STOI  ( stid, 8, nv, ist, ier )
	IF  ( kstid (isnfln) .gt. 0 )
     +			iheadr (kstid (isnfln)) = ist (1)
	IF  ( kstd2 (isnfln) .gt. 0 )
     +			iheadr (kstd2 (isnfln)) = ist (2)
	IF  ( kstnm (isnfln) .gt. 0 )
     +			iheadr (kstnm (isnfln)) = istnm 
	IF  ( kslat (isnfln) .gt. 0 )  THEN
	    IF  ( ERMISS (slat) )  THEN
	        islat = IMISSD
	      ELSE
	        islat = slat * 100
	    END IF
	    iheadr ( kslat (isnfln) ) = islat
	END IF
	IF  ( kslon (isnfln) .gt. 0 )  THEN
	    IF  ( ERMISS (slon) )  THEN
	        islon = IMISSD
	      ELSE
	        islon = slon * 100
	    END IF
	    iheadr ( kslon (isnfln) ) = islon
	END IF
	IF  ( kselv (isnfln) .gt. 0 )  THEN
	    IF  ( ERMISS (selv) )  THEN
	        iselv = IMISSD
	      ELSE
	        iselv = selv
	    END IF
	    iheadr ( kselv (isnfln) ) = iselv
	END IF
	CALL ST_CTOI  ( stat, 1, istat, ier )
	IF  ( kstat (isnfln) .gt. 0 )  iheadr (kstat (isnfln)) = istat
	CALL ST_CTOI  ( coun, 1, icoun, ier )
	IF  ( kcoun (isnfln) .gt. 0 )  iheadr (kcoun (isnfln)) = icoun
C
C*	Write row or column header to file.
C
	IF  ( sttype ( isnfln ) .eq. 'ROW' )  THEN
	    CALL DM_WRWH  ( isnfln, irowcl, iheadr, j, ier )
	  ELSE
	    CALL DM_WCLH  ( isnfln, irowcl, iheadr, j, ier )
	END IF
	IF  ( ier .ne. 0 )  THEN
	    iret = -13
	    RETURN
	END IF
C*
	RETURN
	END
