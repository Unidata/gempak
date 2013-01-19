	SUBROUTINE SF_ASTN  ( isffln, nstn, stid, istnm, slat, slon,
     +			      selv, stat, coun, ispri, nadd, iret )
C************************************************************************
C* SF_ASTN								*
C*									*
C* This subroutine adds a list of stations to a surface data file.	*
C* This subroutine can only be used if the times and stations are not	*
C* mixed in row or column headers.  NADD returns the number of		*
C* stations actually added.  This number may be less than NSTN if	*
C* the file is full.							*
C*									*
C* SF_ASTN  ( ISFFLN, NSTN, STID, ISTNM, SLAT, SLON, SELV, STAT,	*
C*            COUN,   ISPRI, NADD, IRET )				*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	NSTN		INTEGER		Number of stations		*
C*	STID  (NSTN)	CHAR*8		Station identifiers		*
C*	ISTNM (NSTN)	INTEGER		Station numbers			*
C*	SLAT  (NSTN)	REAL		Station latitudes		*
C*	SLON  (NSTN)	REAL		Station longitudes		*
C*	SELV  (NSTN)	REAL		Station elevations		*
C*	STAT  (NSTN)	CHAR*2		States				*
C*	COUN  (NSTN)	CHAR*2		Countries			*
C*	ISPRI (NSTN)	INTEGER		Station priority		*
C*									*
C* Output parameters:							*
C*	NADD		INTEGER		Number of stations added	*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -3 = file not open		*
C*					  -5 = too many stations	*
C*					 -19 = non-standard file	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC          8/93	Added STD2 (for 8-char id) and SPRI	*
C* K. Brill/EMC		 8/98	Use NINT in computing ISLAT & ISLON	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	INTEGER		istnm (*), ispri (*)
	CHARACTER* (*)	stid (*), stat (*), coun (*)
	REAL		slat (*), slon (*), selv (*)
C*
	INTEGER		iheadr (MMKEY), istid (2)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
	nadd = 0
C
C*	Check that times and stations are not intermixed.
C
	IF  ( dttype (isffln) .eq. sttype (isffln) )  THEN
	    iret = -19
	    RETURN
	END IF
C
C*	Initialize header variable.
C
	DO  i = 1, MMKEY
	    iheadr (i) = IMISSD
	END DO
C
C*	Insert character strings into integer header array.
C
	DO  i = 1, nstn
	    CALL ST_STOI  ( stid (i), 8, nv, istid, ier )
	    IF (kstid (isffln) .gt. 0)
     +             iheadr (kstid (isffln)) = istid (1)
	    IF (kstd2 (isffln) .gt. 0)
     +             iheadr (kstd2 (isffln)) = istid (2)
	    IF (kstnm (isffln) .gt. 0) iheadr (kstnm (isffln)) = 
     +                                                istnm (i)
	    IF (kslat (isffln) .gt. 0) THEN
	        IF  ( ERMISS (slat (i) ) )  THEN
	            islat = IMISSD
	          ELSE
	            islat = NINT ( slat (i) * 100 )
	        END IF
	        iheadr  ( kslat (isffln) ) = islat
	    END IF
	    IF  ( kslon (isffln) .gt. 0 )  THEN
	        IF  ( ERMISS (slon (i) ) )  THEN
	            islon = IMISSD
	          ELSE
	            islon = NINT ( slon (i) * 100 )
	        END IF
	        iheadr ( kslon (isffln) ) = islon
	    END IF
	    IF  ( kselv (isffln) .gt. 0 )  THEN
	        IF  ( ERMISS ( selv (i) ) )  THEN
	            iselv = IMISSD
	          ELSE
	            iselv = selv (i)
	        END IF
	        iheadr  ( kselv (isffln) ) = iselv
	    END IF
	    CALL ST_CTOI  ( stat (i), 1, istat, ier )
	    IF (kstat (isffln) .gt. 0) iheadr (kstat (isffln)) = istat
	    CALL ST_CTOI  ( coun (i), 1, icoun, ier )
	    IF (kcoun (isffln) .gt. 0) iheadr (kcoun (isffln)) = icoun
	    IF (kspri (isffln) .gt. 0)
     +			iheadr (kspri (isffln)) = ispri (i)
C
C*	    Determine data location (row or col) and add to file.
C
	    IF  ( sttype (isffln) .eq. 'ROW' )  THEN
	        CALL DM_WRWH  ( isffln, 0, iheadr, jp, ier1 )
	      ELSE
	        CALL DM_WCLH  ( isffln, 0, iheadr, jp, ier1 )
	    END IF
	    IF  ( ier1 .ne. 0 )  THEN
	        iret = -5
	        RETURN
	      ELSE
	        nadd = nadd + 1
	    END IF
	END DO
C*
	RETURN
	END
