	SUBROUTINE NAMCFC  ( prmfil, sndflg, nparm, confac, snbprm,
     +			     iret )
C************************************************************************
C* NAMCFC								*
C*									*
C* This subroutine reads a fifth and sixth column in a parameter-	*
C* packing file.  The number read is the multiplicative factor		*
C* necessary to convert	the parameter to the expected GEMPAK units.	*
C* The last column is used only for sounding data and contains the	*
C* list of parameters actually found in the BUFR data set, in the	*
C* proper order.  SNDFLG must be .true. for sounding data.		*
C*									*
C* Parameter-packing file format:					*
C*									*
C*      Each parameter in the file must be described on a single line	*
C*      containing the following items separated by blanks or tabs:	*
C*									*
C*             parameter name         CHAR*4				*
C*             minimum data value     REAL				*
C*             maximum data value     REAL				*
C*             resolution             REAL				*
C*	       conversion factor      REAL				*
C*	(      BUFR parm name	      CHAR*4	)			*
C*									*
C*      The resolution should be an integral power of 10.  If not, the	*
C*      next smaller resolution will be used (e.g., res = .5 will 	*
C*      become .1).  If the resolution is 0 or if the minimum, maximum	*
C*      and resolution are not present, the data will not be packed.	*
C*									*
C* NAMCFC  ( PRMFIL, SNDFLG, NPARM, CONFAC, SNBPRM, IRET )		*
C*									*
C* Input parameters:							*
C*	PRMFIL		CHAR*	 	Parameter packing file name	*
C*	SNDFLG		LOGICAL		Flag for sounding packing file	*
C*									*
C* Output parameters:							*
C*	NPARM		INTEGER	 	Number of parameters		*
C*	CONFAC (NPARM)	REAL	 	Conversion factors		*
C*	SNBPRM (NPARM)  CHAR*4		BUFR Profile parm names		*
C*	IRET		INTEGER	 	Return code			*
C*					  0 = normal return		*
C*					-13 = packing file not opened	*
C*					-54 = 6th table column missing	*
C**									*
C* Log:									*
C* K. Brill/NMC		12/93						*
C* K. Brill/NMC		 7/94	Added SNBPRM				*
C* K. Brill/EMC		 9/96	FL_TOPN -> FL_TBOP			*
C* D. Kidwell/NCEP	11/98	'stns' -> 'pack' in call to FL_TBOP     *
C* K. Brill/EMC		11/98	Check for missing 6th column element	*
C* D. Kidwell/NCEP	12/98	SNMCFC -> NAMCFC                        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*) 	prmfil, snbprm (*)
	REAL		confac (*)
	LOGICAL		sndflg
C*
	CHARACTER	record*80, clst(6)*24
C------------------------------------------------------------------------
	iret  = 0
	nparm = 0
C
C*	Open the packing file.
C
	CALL FL_TBOP  ( prmfil, 'pack', ipklun, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -13
	    RETURN
	END IF
C
C*	Read in the parameters.
C
	iostat = 0
	knt    = 0
	DO WHILE  ( iostat .eq. 0 )
	    READ   ( ipklun, 1000, IOSTAT = iostat )  record
1000	    FORMAT ( A )
	    IF  ( ( iostat .eq. 0 ) .and. ( record .ne. ' ' ) )  THEN
C
C*		Break input into 6 character strings.
C
		CALL ST_LCUC  ( record, record, ier )
		CALL ST_RNAN  ( record, record, n, ier )
		CALL ST_CLST  ( record, ' ', ' ', 6, clst, n, ier )
		IF ( n .gt. 4 ) THEN
		    IF ( n .lt. 6 .and. sndflg ) THEN
			iret = -54
			RETURN
		    END IF
		    CALL ST_RLST  ( clst (5), ' ', 1.0, 1, cf, n, ier )
C
C*		    Add to list of parameter conversion factors.
C
	            knt = knt + 1
		    confac (knt) = cf
		    snbprm (knt) = clst (6)
		END IF
	    END IF
	END DO
C
C*	Close the packing file.
C
	CALL FL_CLOS  ( ipklun, ier )
C
C*	Set the number of parameters.
C
	nparm = knt
C*
	RETURN
	END
