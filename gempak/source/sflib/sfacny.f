	SUBROUTINE SF_ACNY ( isffln, ncny, stid, istnm, slat, slon,
     +			     selv, stat, ispri, swfo, nadd, iret )
C************************************************************************
C* SF_ACNY								*
C*									*
C* This subroutine adds a list of county stations to a surface file.	*
C* This subroutine can only be used if the times and stations are not	*
C* mixed in row or column headers.  NADD returns the number of		*
C* counties actually added.  This number may be less than NCNY if	*
C* the file is full.							*
C*									*
C* SF_ACNY  ( ISFFLN, NCNY, STID, ISTNM, SLAT, SLON, SELV, STAT,	*
C*            ISPRI, SWFO, NADD, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	NCNY		INTEGER		Number of counties		*
C*	STID  (NCNY)	CHAR*8		County identifiers		*
C*	ISTNM (NCNY)	INTEGER		County numbers			*
C*	SLAT  (NCNY)	REAL		County centroid latitudes	*
C*	SLON  (NCNY)	REAL		County centroid longitudes	*
C*	SELV  (NCNY)	REAL		County elevations		*
C*	STAT  (NCNY)	CHAR*2		States				*
C*	ISPRI (NCNY)	INTEGER		County priority			*
C*	SWFO  (NCNY)	CHAR*8		WFO identifiers			*
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
C* T. Lee/GSC		10/97	From SF_ASTN				*
C* S. Jacobs/NCEP	 4/98	Fixed call to ST_STOI			*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	INTEGER		istnm (*), ispri (*)
	CHARACTER* (*)	stid (*), stat (*), swfo (*)
	REAL		slat (*), slon (*), selv (*)
C*
	INTEGER		iheadr (MMKEY), istid (2), iswfo (2)
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
	DO  i = 1, ncny
	    CALL ST_STOI  ( stid (i), 8, nv, istid, ier )
	    IF (kstid (isffln) .gt. 0)
     +             		iheadr (kstid (isffln)) = istid (1)
	    IF (kstd2 (isffln) .gt. 0)
     +             		iheadr (kstd2 (isffln)) = istid (2)
	    IF (kstnm (isffln) .gt. 0) 
     +				iheadr (kstnm (isffln)) = istnm (i)
	    IF (kslat (isffln) .gt. 0) THEN
	        IF  ( ERMISS (slat (i) ) )  THEN
	            islat = IMISSD
	          ELSE
	            islat = slat (i) * 100
	        END IF
	        iheadr  ( kslat (isffln) ) = islat
	    END IF
	    IF  ( kslon (isffln) .gt. 0 )  THEN
	        IF  ( ERMISS (slon (i) ) )  THEN
	            islon = IMISSD
	          ELSE
	            islon = slon (i) * 100
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
	    CALL ST_STOI  ( swfo (i), 8, ns, iswfo, ier )
	    IF (kswfo (isffln) .gt. 0) 
     +				iheadr (kswfo (isffln)) = iswfo (1)
	    IF (kwfo2 (isffln) .gt. 0) 
     +				iheadr (kwfo2 (isffln)) = iswfo (2)
	    IF (kspri (isffln) .gt. 0) 
     +				iheadr (kspri (isffln)) = ispri (i)
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
