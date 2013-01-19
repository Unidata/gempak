	SUBROUTINE SFC_ACNY ( isffln, ncny, stid, istnm, slat, slon,
     +			      selv, stat, ispri, swfo, nadd, iret )
C************************************************************************
C* SFC_ACNY								*
C*									*
C* This subroutine adds a list of county stations to a surface file.	*
C* This subroutine can only be used if the times and stations are not	*
C* mixed in row or column headers.  NADD returns the number of		*
C* counties actually added.  This number may be less than NCNY if	*
C* the file is full.							*
C*									*
C* SFC_ACNY  ( ISFFLN, NCNY, STID, ISTNM, SLAT, SLON, SELV, STAT,	*
C*             ISPRI, SWFO, NADD, IRET )				*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	NCNY		INTEGER		Number of counties		*
C*	STID     	CHAR*		County identifiers		*
C*	ISTNM (NCNY)	INTEGER		County numbers			*
C*	SLAT  (NCNY)	REAL		County centroid latitudes	*
C*	SLON  (NCNY)	REAL		County centroid longitudes	*
C*	SELV  (NCNY)	REAL		County elevations		*
C*	STAT        	CHAR*		States				*
C*	ISPRI (NCNY)	INTEGER		County priority			*
C*	SWFO        	CHAR*		WFO identifiers			*
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
C* D.W.Plummer/NCEP	12/97	From SF_ACNY				*
C* T. Piper/GSC		 3/99	Updated prolog				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	INTEGER		istnm (*), ispri (*)
	CHARACTER* (*)	stid, stat, swfo
	REAL		slat (*), slon (*), selv (*)
C*
	CHARACTER	stid8(LLSTFL)*8, stat2(LLSTFL)*2, swfo8(LLSTFL)*8
C*
C------------------------------------------------------------------------
C
	iptrs = 1
	DO  i = 1, ncny
		iptre = iptrs + INDEX(stid(iptrs:), ";") - 1
		stid8(i) = stid(iptrs:iptre-1)
		iptrs = iptre + 1
	END DO
C
	iptrs = 1
	DO  i = 1, ncny
		iptre = iptrs + INDEX(stat(iptrs:), ";") - 1
		stat2(i) = stat(iptrs:iptre-1)
		iptrs = iptre + 1
	END DO
C
	iptrs = 1
	DO  i = 1, ncny
		iptre = iptrs + INDEX(swfo(iptrs:), ";") - 1
		swfo8(i) = swfo(iptrs:iptre-1)
		iptrs = iptre + 1
	END DO

	CALL SF_ACNY ( isffln, ncny, stid8, istnm, slat, slon,
     +		       selv, stat2, ispri, swfo8, nadd, iret )
C*
	RETURN
	END
