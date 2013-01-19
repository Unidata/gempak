	SUBROUTINE SF_USTN  ( isffln, stid, istnm, slat, slon, selv, 
     +	                      ispri, stat, coun, keynam, iret )
C************************************************************************
C* SF_USTN								*
C*									*
C* This subroutine updates the header information for a station in a	*
C* surface data file.  This subroutine can only be used if the times 	*
C* and stations are not mixed in row or column headers.			*
C*									*
C* SF_USTN  ( ISFFLN, STID, ISTNM, SLAT, SLON, SELV, ISPRI, STAT, 	*
C*            COUN, KEYNAM, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	STID  		CHAR*8		Station identifier		*
C*	ISTNM 		INTEGER		Station number			*
C*	SLAT  		REAL		Station latitude		*
C*	SLON  		REAL		Station longitude		*
C*	SELV  		REAL		Station elevation		*
C*	ISPRI  		INTEGER		Station priority parameter	*
C*	STAT  		CHAR*2	 	State				*
C*	COUN  		CHAR*2		Country				*
C*	KEYNAM		CHAR*4		Key to update (STID or STNM)	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -3 = file not open		*
C*					 -10 = station not in file	*
C*					 -12 = DM error			*
C*				  	 -19 = non-standard file	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC		 8/93	Changes for 8-char ID			*
C* A. Hardy/GSC		 3/99   Changed calling sequence, ispri         *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER* (*)	stid, stat, coun, keynam
C*
	LOGICAL		staflg
	CHARACTER	sta*8, key*4
	INTEGER		iheadr (MMKEY), ist (2)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Check that the rows and columns are not intermixed.
C
	IF  ( dttype ( isffln ) .eq. sttype ( isffln ) )  THEN
	    iret = -19
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
	CALL SF_CSTN  ( isffln, sta, ist, key, staflg, irowcl, 
     +			iret )
	IF  ( .not. staflg ) THEN
	    iret = -10
	    RETURN
	END IF
C
C*	Load header array.
C
	CALL ST_STOI  ( stid, 8, nv, ist, ier )
	IF  ( kstid (isffln) .gt. 0 )
     +			iheadr (kstid (isffln)) = ist (1)
	IF  ( kstd2 (isffln) .gt. 0 )
     +			iheadr (kstd2 (isffln)) = ist (2)
	IF  ( kstnm (isffln) .gt. 0 )
     +			iheadr (kstnm (isffln)) = istnm 
	IF  ( kslat (isffln) .gt. 0 )  THEN
	    IF  ( ERMISS (slat) )  THEN
	        islat = IMISSD
	      ELSE
	        islat = slat * 100
	    END IF
	    iheadr ( kslat (isffln) ) = islat
	END IF
	IF  ( kslon (isffln) .gt. 0 )  THEN
	    IF  ( ERMISS (slon) )  THEN
	        islon = IMISSD
	      ELSE
	        islon = slon * 100
	    END IF
	    iheadr ( kslon (isffln) ) = islon
	END IF
	IF  ( kselv (isffln) .gt. 0 )  THEN
	    IF  ( ERMISS (selv) )  THEN
	        iselv = IMISSD
	      ELSE
	        iselv = selv
	    END IF
	    iheadr ( kselv (isffln) ) = iselv
	END IF
	IF  ( kspri (isffln) .gt. 0 )
     +			iheadr (kspri (isffln)) = ispri 
	CALL ST_CTOI  ( stat, 1, istat, ier )
	IF  ( kstat (isffln) .gt. 0 )  iheadr (kstat (isffln)) = istat
	CALL ST_CTOI  ( coun, 1, icoun, ier )
	IF  ( kcoun (isffln) .gt. 0 )  iheadr (kcoun (isffln)) = icoun
C
C*	Write row or column header to file.
C
	IF  ( sttype ( isffln ) .eq. 'ROW' )  THEN
	    CALL DM_WRWH  ( isffln, irowcl, iheadr, j, ier )
	  ELSE
	    CALL DM_WCLH  ( isffln, irowcl, iheadr, j, ier )
	END IF
	IF  ( ier .ne. 0 )  THEN
	    iret = -12
	    RETURN
	END IF
C*
	RETURN
	END
