	SUBROUTINE DC_FINT ( numfil, itype, parmfl, iret )
C************************************************************************
C* DC_FINT								*
C*									*
C* This routine initializes the common variables for cycling through	*
C* a list of open files.						*
C*									*
C* DC_FINT ( NUMFIL, ITYPE, PARMFL, IRET )				*
C*									*
C* Input parameters:							*
C*	NUMFIL		INTEGER		Requested number of open files	*
C*	ITYPE		INTEGER		Type of output file		*
C*					    1 = Surface land		*
C*					    2 = Surface ship		*
C*					    3 = Sounding unmerged	*
C*					    4 = Sounding merged		*
C*					    5 = Surface Text only	*
C*					    6 = ASCII file only		*
C*	PARMFL		CHAR*		Parameter packing table		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					 -10 = unknown output file type	*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NMC	 8/95						*
C* S. Jacobs/NCEP	 7/96	Updated documentation			*
C* A. Hardy/GSC		 5/99   Added type 6 for ASCII files            *
C************************************************************************
	INCLUDE		'dccmn.cmn'
C*
	CHARACTER*(*)	parmfl
C*
	INTEGER		kscale ( MMPARM ), kofset ( MMPARM ),
     +			kbits ( MMPARM )
	LOGICAL		pkflg
C------------------------------------------------------------------------
	iret = 0
C
C*	Set the maximum number of open files in the common block.
C*	If more than the maximum number of files is requested, set to
C*	the maximum number of files allowed, and return a warning.
C
	IF  ( ( numfil .gt. 0 ) .and. ( numfil .le. MXFILE ) )  THEN
	    maxfil = numfil
	  ELSE
	    maxfil = MXFILE
	    CALL DC_WLOG ( 0, 'DC', 4, ' ', ier )
	END IF
C
C*	Initialize the open file arrays.
C
	DO  i = 1, maxfil
	    filopn (i) = ' '
	    lunopn (i) = 0
	    numopn (i) = 0
	END DO
C
C*	Set the output file type in the common block.
C*	If it is not a valid type, return an error.
C
	IF  ( ( itype .ge. 1 ) .and. ( itype .le. 6 ) )  THEN
	    iftype = itype
	  ELSE
	    iret = -10
	END IF
C
C*	Set the parameter file and the parameters in the common block.
C
	prmfil = parmfl
	IF  ( prmfil .ne. ' ' )  THEN
	    CALL IN_PRMF  ( prmfil, kparm, param, kscale, kofset,
     +			    kbits, pkflg, ier )
	END IF
C*
	RETURN
	END
