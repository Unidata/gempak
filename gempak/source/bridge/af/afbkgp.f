	SUBROUTINE AF_BKGP ( string, iret )
C************************************************************************
C* AF_BKGP								*
C*									*
C* This subroutine breaks apart a string into groups of "like-type" 	*
C* (i.e. letter, number, or non-alphanumeric) in order to facilitate	*
C* decoding.  The output groups and other associated information are	*
C* stored in COMMON / FIELDS /.						*
C*									*
C*	EXAMPLE:							*
C*									*
C*									*
C*	INPUT:								*
C*									*
C*	A string with 9 concatenated groups as follows:			*
C*									*
C*	ARP DLH536   4500N01750W 2331 F350 MS57 310/043 TB 0		*
C*									*
C*	 1     2	  3	  4     5   6      7    8  9		*
C*									*
C*									*
C*	OUTPUT:								*
C*									*
C*	nflds = 17, with:						*
C*									*
C*	nflds	fields	lensf	itypsf	irfnsf				*
C*	 1	ARP	3	ALPHA	1				*
C*	 2	DLH	3	ALPHA	2				*
C*	 3	536	3	NMR	2				*
C*	 4	4500	4	NMR	3				*
C*	 5	N	1	ALPHA	3				*
C*	 6	01750	5	NMR	3				*
C*	 7	W	1	ALPHA	3				*
C*	 8	2331	4	NMR	4				*
C*	 9	F	1	ALPHA	5				*
C*	10	350	3	NMR	5				*
C*	11	MS	2	ALPHA	6				*
C*	12	57	2	NMR	6				*
C*	13	310	3	NMR	7				*
C*	14	/	1	NALNMR	7				*
C*	15	043	3	NMR	7				*
C*	16	TB	2	ALPHA	8				*
C*	17	0	1	NMR	9				*
C*									*
C*									*
C* AF_BKGP ( STRING, IRET )						*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Character string 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = string could not be	*
C*					      successfully broken apart *
C*									*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	string
C*
	CHARACTER	wkstg*(DCMXBF)
	LOGICAL		newgrp
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret = 0
	nflds = 0
	irfn = 1
	newgrp = .true.
C
C*	From the input string, create a working copy which has
C*	all leading blanks/tabs and excess interior blanks/tabs
C*	removed. 
C
	CALL ST_RXBL  ( string, wkstg, lenws, ierrxb )
	IF  ( lenws .le. 0 )  THEN
	    iret = -1
	    RETURN
	END IF
C
	DO ii = 1, lenws
C
C*	    Loop through each character in the working copy of the
C*	    string.
C
	    IF  ( wkstg ( ii : ii ) .eq. ' ' )  THEN
C
C*		This character is a blank, so store the length of the
C*		current group, set the flag to begin a new group with
C*		the following character, and then increment the	count
C*		of concatenated groups within the input string.
C
		lensf ( nflds ) = nfch
		newgrp = .true.
		irfn = irfn + 1
	    ELSE 
		CALL ST_ALNM  ( wkstg ( ii : ii ), ityp, ieraln )
		IF  ( .not. newgrp )  THEN
		    IF  ( ityp .eq. itypsf ( nflds ) )  THEN
C
C*			This character is of the same type as the group
C*			currently being built, so add it to the current
C*			group.
C
			IF  ( nfch .ge. MXLENF )  THEN
			  logmsg = 'Like-type group '
     +			  	   //  fields ( nflds )
     +				   // ' is too big'
			  CALL DC_WLOG  ( 2, 'AF', 1, logmsg, ierwlg )
			  iret = -1
			  RETURN
			END IF
			nfch = nfch + 1
			fields ( nflds ) ( nfch : nfch )
     +				= wkstg ( ii : ii )
		    ELSE
C
C*			This character is of a different type than the
C*			group currently being built, so store the length
C*			of the current group and then set the flag to
C*			begin building a new group with this character.
C
			lensf ( nflds ) = nfch
			newgrp = .true.
		    END IF
		END IF
		IF  ( newgrp )  THEN
C
C*		    Use the current character to begin building a
C*		    new group.
C
		    IF  ( nflds .ge. MXNFLD )  THEN
			WRITE ( UNIT = logmsg, FMT = '( I3, A )' )
     +			    MXNFLD, ' like-type groups'
			CALL DC_WLOG  ( 2, 'AF', 3, logmsg, ierwlg )
			RETURN
		    END IF
		    nfch = 1
		    nflds = nflds + 1
		    fields ( nflds ) = ' '
		    fields ( nflds ) ( nfch : nfch )
     +			= wkstg ( ii : ii )
		    itypsf ( nflds ) = ityp
		    irfnsf ( nflds ) = irfn
		    newgrp = .false.
		END IF
	    END IF
	END DO
C
C*	Store the length of the last group.
C
	lensf ( nflds ) = nfch
C*
	RETURN
	END
