	SUBROUTINE DM_WRWH  ( iflno, ipos, iheadr, jpos, iret )
C************************************************************************
C* DM_WRWH								*
C*									*
C* This subroutine writes a row header to a DM file.  If the value	*
C* for IPOS is 0, the next available location will be used.  The	*
C* variables contained in the row headers can be determined using	*
C* DM_KEYS.								*
C*									*
C* DM_WRWH  ( IFLNO, IPOS, IHEADR, JPOS, IRET )				*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IPOS		INTEGER		Location			*
C*	IHEADR (*)	INTEGER		Header array			*
C*									*
C* Output parameters:							*
C*	JPOS		INTEGER		Actual header location		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file is not open		*
C*					 -6 = write error		*
C*					 -9 = invalid row		*
C*					-12 = no more row headers	*
C*					-13 = no write access		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* M. desJardins/NMC	 4/91	Add write for files from other machines	*
C* M. desJardins/NMC	 5/91	Add logical vars for machine types	*
C* S. Jacobs/NCEP	 2/97	Added check for swapping GVCD		*
C* S. Jacobs/NCEP	 4/98	Added check for swapping STD2		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C*
	INTEGER		iheadr (*)
C*
	CHARACTER	type*4, type2*4
C------------------------------------------------------------------------
	jpos  = 0
	nvstd = 6
C
C*	Check that file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Check that user has write access to file.
C
	IF  ( .not. wflag (iflno) ) THEN
	    iret = -13
	    RETURN
	END IF
C
C*	Check for valid location.  
C
	IF ( (ipos .lt. 0) .or. (ipos .gt. krow (iflno)) ) THEN
		iret = -9
C
C*	  Get next location if ipos was set to 0.
C 
	  ELSE IF ( ipos .eq. 0 ) THEN
	    knt = 1
C
C*	    Check each header location to see if it is used.
C
	    DO WHILE ((jpos .eq. 0) .and. (knt .le. krow(iflno)))
		IF ( kheadr (0,knt,iflno) .eq. IMISSD ) jpos = knt
		knt = knt + 1
	    END DO
	    IF ( jpos .eq. 0 ) iret = -12
C
C*	  Otherwise header location was specified.
C
	  ELSE
	    jpos = ipos
C
	END IF
C
C*	Store header in common.
C
	IF ( iret .eq. 0 ) THEN
	    DO  i = 1, krkeys (iflno)
		kheadr ( i, jpos, iflno ) = iheadr ( i )
	    END DO
C
C*	    Set flag indicating header is used.
C
	    kheadr ( 0, jpos, iflno ) = - IMISSD
C
C*	    Write header to file.
C
	    istart = kprowh (iflno) + (jpos-1) * (krkeys (iflno) + 1)
C
C*	    If the file was created on a different machine, 
C*	    swap the bytes so that the swap in the write will be
C*	    nulled.
C
	    IF  ( ( kmachn (iflno) .ne. MTMACH ) .and.
     +		  ( ( kvmst ( iflno ) .and. ( .not. mvmst ) ) .or.
     +		    ( mvmst .and. ( .not. kvmst ( iflno ) ) ) ) )  THEN
C
C*		Check for STID and STD2.
C
		CALL DM_FKEY  ( iflno, 'STID', type, loc, ier )
		CALL DM_FKEY  ( iflno, 'STD2', type2, loc2, ier )
		IF  ( type .eq. 'ROW' )  THEN
		    ier = MV_SWP4 ( 1, kheadr (loc,jpos,iflno),
     +				       kheadr (loc,jpos,iflno) )
		    IF  ( loc2 .gt. 0 )
     +			ier = MV_SWP4 ( 1, kheadr (loc2,jpos,iflno),
     +				           kheadr (loc2,jpos,iflno) )
		END IF
C
C*		Check for STAT.
C
		CALL DM_FKEY  ( iflno, 'STAT', type, loc, ier )
		IF  ( type .eq. 'ROW' )  THEN
		    ier = MV_SWP4 ( 1, kheadr (loc,jpos,iflno),
     +				       kheadr (loc,jpos,iflno) )
		END IF
C
C*		Check for COUN.
C
		CALL DM_FKEY  ( iflno, 'COUN', type, loc, ier )
		IF  ( type .eq. 'ROW' )  THEN
		    ier = MV_SWP4 ( 1, kheadr (loc,jpos,iflno),
     +				       kheadr (loc,jpos,iflno) )
		END IF
C
C*		Check for GPM1 which is the start of three words.
C
		CALL DM_FKEY  ( iflno, 'GPM1', type, loc, ier )
		IF  ( type .eq. 'ROW' )  THEN
		    ier = MV_SWP4 ( 3, kheadr (loc,jpos,iflno),
     +				       kheadr (loc,jpos,iflno) )
		END IF
C
C*		Check for GVCD. Swap only if the value is greater
C*		than the number of standard vertical coordinates.
C
		CALL DM_FKEY  ( iflno, 'GVCD', type, loc, ier )
		IF  ( type .eq. 'ROW' )  THEN
		    IF  ( kheadr (loc,jpos,iflno) .gt. nvstd )  THEN
			ier = MV_SWP4 ( 1, kheadr (loc,jpos,iflno),
     +				       	   kheadr (loc,jpos,iflno) )
		    END IF
		END IF
	    END IF
	    length = krkeys (iflno) + 1
	    CALL DM_WINT ( iflno, istart, length, 
     +			   kheadr ( 0, jpos, iflno ), iret )
C
C*	    On different machines, swap bytes back in common.
C
	    IF  ( ( kmachn (iflno) .ne. MTMACH ) .and.
     +		  ( ( kvmst ( iflno ) .and. ( .not. mvmst ) ) .or.
     +		    ( mvmst .and. ( .not. kvmst ( iflno ) ) ) ) )  THEN
C
C*		Check for STID and STD2.
C
		CALL DM_FKEY  ( iflno, 'STID', type, loc, ier )
		CALL DM_FKEY  ( iflno, 'STD2', type2, loc2, ier )
		IF  ( type .eq. 'ROW' )  THEN
		    ier = MV_SWP4 ( 1, kheadr (loc,jpos,iflno),
     +				       kheadr (loc,jpos,iflno) )
		    IF  ( loc2 .gt. 0 )
     +			ier = MV_SWP4 ( 1, kheadr (loc2,jpos,iflno),
     +				           kheadr (loc2,jpos,iflno) )
		END IF
C
C*		Check for STAT.
C
		CALL DM_FKEY  ( iflno, 'STAT', type, loc, ier )
		IF  ( type .eq. 'ROW' )  THEN
		    ier = MV_SWP4 ( 1, kheadr (loc,jpos,iflno),
     +				       kheadr (loc,jpos,iflno) )
		END IF
C
C*		Check for COUN.
C
		CALL DM_FKEY  ( iflno, 'COUN', type, loc, ier )
		IF  ( type .eq. 'ROW' )  THEN
		    ier = MV_SWP4 ( 1, kheadr (loc,jpos,iflno),
     +				       kheadr (loc,jpos,iflno) )
		END IF
C
C*		Check for GPM1 which is the start of three words.
C
		CALL DM_FKEY  ( iflno, 'GPM1', type, loc, ier )
		IF  ( type .eq. 'ROW' )  THEN
		    ier = MV_SWP4 ( 3, kheadr (loc,jpos,iflno),
     +				       kheadr (loc,jpos,iflno) )
		END IF
C
C*		Check for GVCD. Swap only if the value is greater
C*		than the number of standard vertical coordinates.
C
		CALL DM_FKEY  ( iflno, 'GVCD', type, loc, ier )
		IF  ( type .eq. 'ROW' )  THEN
		    IF  ( kheadr (loc,jpos,iflno) .gt. nvstd )  THEN
			ier = MV_SWP4 ( 1, kheadr (loc,jpos,iflno),
     +				       	   kheadr (loc,jpos,iflno) )
		    END IF
		END IF
	    END IF
C
C*	    If this is the maximum header defined, save in common.
C
	    IF  ( klstrw ( iflno ) .lt. jpos ) klstrw (iflno) = jpos
	END IF
C*
	RETURN
	END
