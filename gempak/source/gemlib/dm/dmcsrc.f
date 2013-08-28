	SUBROUTINE DM_CSRC ( iflno, addsrc, nkeys, keynam, iloval, 
     +			     ihival, iret )
C************************************************************************
C* DM_CSRC								*
C*									*
C* This subroutine defines criteria for a conditional search.  The	*
C* conditional search will be made if the primary search succeeds.	*
C*									*
C* DM_CSRC  ( IFLNO, ADDSRC, NKEYS, KEYNAM, ILOVAL, IHIVAL, IRET )	*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	ADDSRC		LOGICAL		Additive search flag		*
C*	NKEYS		INTEGER		Number of keys used in search	*
C*	KEYNAM (NKEYS)	CHAR*4		Key names			*
C*	ILOVAL (NKEYS)	INTEGER		Low values			*
C*	IHIVAL (NKEYS)	INTEGER		High values			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					-14 = invalid key name		*
C*					-22 = too many searches		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* m. gamazaychikov/CWS	04/11	Add code for A2DB connectivity		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	keynam (*)
	INTEGER		iloval (*), ihival (*)
	LOGICAL		addsrc
	CHARACTER	type*4
C------------------------------------------------------------------------
C*	Check that file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Check that at least one key is to be used in search.
C
	IF  ( ( nkeys .lt. 1 ) .or. ( nkeys .gt. MMKEY ) ) THEN
	    iret = -14
	    RETURN
	END IF
C
C*	Initialize variables.
C
	ksrow  (iflno) = 0
	kscol  (iflno) = 0
C
C*	Check that search can be added and find search number.
C
	IF  ( nsrch ( iflno ) .eq. MMSRCH )  THEN
	    iret = -22
	    RETURN
	  ELSE
	    nums = nsrch ( iflno ) + 1
	    ksnrow ( nums, iflno ) = 0
	    ksncol ( nums, iflno ) = 0
	END IF
C
C*	For each key name, determine type and save in common.
C
	DO  i = 1, nkeys
	    CALL DM_FKEY ( iflno, keynam (i), type, loc, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = ier
	      ELSE IF ( type .eq. 'ROW' ) THEN
		ksnrow ( nums, iflno ) = ksnrow ( nums, iflno ) + 1
		kslrow ( ksnrow (nums,iflno), nums, iflno ) = loc
		ksrlov ( ksnrow (nums,iflno), nums, iflno ) = iloval (i)
		ksrhiv ( ksnrow (nums,iflno), nums, iflno ) = ihival (i)
	      ELSE IF ( type .eq. 'COL' ) THEN
		ksncol ( nums, iflno ) = ksncol ( nums, iflno ) + 1
		kslcol ( ksncol (nums,iflno), nums, iflno ) = loc
		ksclov ( ksncol (nums,iflno), nums, iflno ) = iloval (i)
		kschiv ( ksncol (nums,iflno), nums, iflno ) = ihival (i)
	    END IF
	END DO
C
C*	If all keys were found, set search flag.
C
	IF  ( iret .eq. 0 ) THEN
	    kaddsr ( nums, iflno ) = addsrc
	    nsrch  ( iflno ) = nums
	END IF
C*
	RETURN
	END
