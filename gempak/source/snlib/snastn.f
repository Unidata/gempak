	SUBROUTINE SN_ASTN  ( isnfln, nstn, stid, istnm, slat, slon,
     +                        selv, stat, coun, nadd, iret )
C************************************************************************
C* SN_ASTN								*
C*									*
C* This subroutine adds a list of stations to a sounding data file.	*
C* This subroutine can only be used if the times and stations are 	*
C* not mixed in row or column headers.					*
C*									*
C* SN_ASTN  ( ISNFLN, NSTN, STID, ISTNM, SLAT, SLON, SELV, STAT,	*
C*            COUN,  NADD, IRET )					*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	NSTN		INTEGER		Number of stations		*
C*	STID  (NSTN)	CHAR*8		Station identifiers		*
C*	ISTNM (NSTN)	INTEGER		Station numbers			*
C*	SLAT  (NSTN)	REAL		Station latitudes		*
C*	SLON  (NSTN)	REAL		Station longitudes		*
C*	SELV  (NSTN)	REAL		Station elevations		*
C*	STAT  (NSTN)	CHAR*2	 	States				*
C*	COUN  (NSTN)	CHAR*2		Countries			*
C*									*
C* Output parameters:							*
C*	NADD		INTEGER		Number of stations added	*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -4 = file not open		*
C*				   	  -6 = too many stations	*
C*				  	 -21 = non-standard file	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 8/87						*
C* K. Brill/NMC		 8/93	Changes for 8-char stn ID		*
C* K. Brill/EMC		 8/98	Use NINT in computing ISLAT & ISLON	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	INTEGER		istnm (*)
	CHARACTER* (*)	stid (*), stat (*), coun (*)
	REAL		slat (*), slon (*), selv (*)
C*
	INTEGER		iheadr (MMKEY), istid (2)
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	CALL SN_CHKF ( isnfln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that times and stations are not intermixed.
C
	IF  ( dttype ( isnfln ) .eq. sttype ( isnfln ) ) THEN
	    iret = -21
	    RETURN
	END IF
C
C*	Initialize header array.
C
	nadd = 0
	DO  i = 1, MMKEY
	    iheadr ( i ) = IMISSD
	END DO
C
C*	Put station information in header array.
C
	DO i = 1, nstn
	    CALL ST_STOI ( stid (i), 8, nv, istid, ier )
	    IF (kstid (isnfln) .gt. 0)
     +			iheadr (kstid (isnfln)) = istid (1)
	    IF (kstd2 (isnfln) .gt. 0)
     +			iheadr (kstd2 (isnfln)) = istid (2)
	    IF (kstnm (isnfln) .gt. 0)
     +			iheadr (kstnm (isnfln)) = istnm (i)
	    IF (kslat (isnfln) .gt. 0) THEN
	        IF (ERMISS (slat (i))) THEN
	            islat = IMISSD
	          ELSE
	            islat = NINT ( slat (i) * 100 )
	        END IF
	        iheadr (kslat (isnfln)) = islat
	    END IF
	    IF (kslon (isnfln) .gt. 0) THEN
	        IF (ERMISS (slon (i))) THEN
	            islon = IMISSD
	          ELSE
	            islon = NINT ( slon (i) * 100 )
	        END IF
	        iheadr (kslon (isnfln)) = islon
	    END IF
	    IF (kselv (isnfln) .gt. 0) THEN
	        IF (ERMISS (selv (i))) THEN
	            iselv = IMISSD
	          ELSE
	            iselv = selv (i)
	        END IF
	        iheadr (kselv (isnfln)) = iselv
	    END IF
	    CALL ST_CTOI (stat (i), 1, istat, ier)
	    IF (kstat (isnfln) .gt. 0) iheadr (kstat (isnfln)) = istat
	    CALL ST_CTOI (coun (i), 1, icoun, ier)
	    IF (kcoun (isnfln) .gt. 0) iheadr (kcoun (isnfln)) = icoun
C
C*	    Determine header type (row or col) and add to file.
C
	    IF ( sttype ( isnfln ) .eq. 'ROW' )  THEN
	        CALL DM_WRWH ( isnfln, 0, iheadr, jp, ier )
	      ELSE
	        CALL DM_WCLH ( isnfln, 0, iheadr, jp, ier )
	    END IF
C
C*	    Check error.
C
	    IF ( ier .ne. 0 )  THEN
	        iret = -6
	        RETURN
	      ELSE
	        nadd = nadd + 1
	    END IF
	END DO
C
	RETURN
	END
