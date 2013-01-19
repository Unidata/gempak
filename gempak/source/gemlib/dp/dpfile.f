	SUBROUTINE DP_FILE  ( prmfil, nparm, parms, logscl, ioffst, 
     +			      nbits,  pkflg, iret )
C************************************************************************
C* DP_FILE								*
C*									*
C* This subroutine reads a parameter-packing file.  The parameters in	*
C* the file and the data-packing terms are returned.  If none of the	*
C* data is to be packed, PKFLG is set to false.  If some of the data	*
C* is to be packed and some is not, an error is returned.		*
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
C*									*
C*      The resolution should be an integral power of 10.  If not, the	*
C*      next smaller resolution will be used (e.g., res = .5 will 	*
C*      become .1).  If the resolution is 0 or if the minimum, maximum	*
C*      and resolution are not present, the data will not be packed.	*
C*									*
C* DP_FILE  ( PRMFIL, NPARM, PARMS, LOGSCL, IOFFST, NBITS, PKFLG,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	PRMFIL		CHAR*	 	Parameter packing file name	*
C*									*
C* Output parameters:							*
C*	NPARM		INTEGER	 	Number of parameters		*
C*	PARMS  (NPARM)	CHAR*	 	Parameter names			*
C*	LOGSCL (NPARM)	INTEGER	 	Log10 of scale factor		*
C*	IOFFST (NPARM)	INTEGER	 	Offset				*
C*	NBITS  (NPARM)	INTEGER	 	Number of bits			*
C*	PKFLG		LOGICAL	 	Packing flag			*
C*	IRET		INTEGER	 	Return code			*
C*					  0 = normal return		*
C*					 -3 = invalid number of parms	*
C*					 -7 = packing file not opened	*
C*					 -8 = file read error		*
C*					 -9 = packed and unpacked data	*
C*					      mixed			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 4/87	GEMPAK4					*
C* M. desJardins/GSFC	10/88	Eliminated TB_RPPK			*
C* J. Whistler/SSAI	 6/91	Eliminated error message after FL_TOPN	*
C* D. Keiser/GSC	12/95	Changed FL_TOPN to FL_TBOP		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*) 	prmfil, parms (*)
	INTEGER		logscl (*), ioffst (*), nbits (*)
	LOGICAL		pkflg
C*
	CHARACTER	pm*4, record*80, clst(4)*24
	REAL		xmn (MMPARM), xmx (MMPARM), xres (MMPARM)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret  = 0
	nparm = 0
C
C*	Open the packing file.
C
	CALL FL_TBOP  ( prmfil, 'pack', ipklun, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -7
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
C*		Break input into 4 character strings.
C
		CALL ST_LCUC  ( record, record, ier )
		CALL ST_RNAN  ( record, record, n, ier )
		CALL ST_CLST  ( record, ' ', ' ', 4, clst, n, ier )
		pm = clst (1)
		CALL ST_RLST  ( clst (2), ' ', RMISSD, 1, ymn, n, ier )
		CALL ST_RLST  ( clst (3), ' ', RMISSD, 1, ymx, n, ier )
		CALL ST_RLST  ( clst (4), ' ', RMISSD, 1, yres, n, ier )
C
C*		Add to list of parameters.
C
	        knt = knt + 1
	        IF  ( knt .gt. MMPARM )  THEN
	            iret = -3
	            RETURN
	          ELSE
	            parms (knt) = pm
	            xmn   (knt) = ymn
	            xmx   (knt) = ymx
	            xres  (knt) = yres
	            IF  ( ( ERMISS (ymn) ) .or. ( ERMISS (ymx) ) .or.
     +			  ( ERMISS (yres) ) )  THEN
			IF  ( knt .eq. 1 )  THEN
			    pkflg = .false.
			  ELSE IF  ( pkflg )  THEN
			    iret  = -9
			    RETURN
			END IF
		      ELSE
			IF  ( knt .eq. 1 )  THEN
			    pkflg = .true.
			  ELSE IF  ( .not. pkflg )  THEN
			    iret  = -9
			    RETURN
			END IF
		    END IF
	        END IF
	    END IF
	END DO
C
C*	Set terms in DP package.
C
	IF  ( pkflg )  THEN
	    DO  i = 1, knt
		CALL DP_TERM  ( xmn (i), xmx (i), xres (i), 
     +				logscl (i), ioffst (i), nbits (i), ier )
		IF  ( ier .ne. 0 )  iret = ier
	    END DO
	END IF
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
