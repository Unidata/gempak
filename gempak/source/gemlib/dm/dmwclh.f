	SUBROUTINE DM_WCLH ( iflno, ipos, iheadr, jpos, iret )
C************************************************************************
C* DM_WCLH								*
C*									*
C* This subroutine writes a column header to a DM file.  If the value	*
C* for IPOS is 0, the next available location will be used.  The	*
C* variables contained in the row headers can be determined using	*
C* DM_KEYS.								*
C*									*
C* DM_WCLH  ( IFLNO, IPOS, IHEADR, JPOS, IRET )				*
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
C*					 -9 = invalid location		*
C*					-12 = no more column headers	*
C*					-13 = no write access		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* M. desJardins/NMC	 4/91	Write to different machines		*
C* J. Whistler/SSAI	 4/91	Corrected mistype of jpos to jloc	*
C* M. desJardins/NMC	 5/91	Added logical vars for machine type	*
C* S. Jacobs/EAI	 8/92	Added check for ULTRIX machine		*
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
	IF ( (ipos .lt. 0) .or. (ipos .gt. kcol (iflno)) ) THEN
		iret = -9
C
C*	  Get next location if ipos was set to 0.
C 
	  ELSE IF ( ipos .eq. 0 ) THEN
	    knt  = 1
	    kloc = knt + krow ( iflno )
C
C*	    Check each header location to see if it is used.
C
	    DO WHILE ((jpos .eq. 0) .and. (knt .le. kcol(iflno)))
		IF ( kheadr (0,kloc,iflno) .eq. IMISSD ) THEN
		    jpos = knt
		    jloc = kloc
		  ELSE
		    knt  = knt + 1
		    kloc = kloc + 1
		END IF
	    END DO
	    IF ( jpos .eq. 0 ) iret = -12
C
C*	  Otherwise header location was specified.
C
	  ELSE
	    jpos = ipos
	    jloc = jpos + krow ( iflno )
C
	END IF
C
C*	Store header in common.
C
	IF ( iret .eq. 0 ) THEN
	    DO  i = 1, kckeys (iflno)
		kheadr ( i, jloc, iflno ) = iheadr ( i )
	    END DO
C
C*	    Set flag indicating header is used.
C
	    kheadr ( 0, jloc, iflno ) = - IMISSD
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
		IF  ( type .eq. 'COL' )  THEN
		    ier = MV_SWP4 ( 1, kheadr (loc,jloc,iflno),
     +				       kheadr (loc,jloc,iflno) )
		    IF  ( loc2 .gt. 0 )
     +			ier = MV_SWP4 ( 1, kheadr (loc2,jloc,iflno),
     +				           kheadr (loc2,jloc,iflno) )
		END IF
C
C*		Check for STAT.
C
		CALL DM_FKEY  ( iflno, 'STAT', type, loc, ier )
		IF  ( type .eq. 'COL' )  THEN
		    ier = MV_SWP4 ( 1, kheadr (loc,jloc,iflno),
     +				       kheadr (loc,jloc,iflno) )
		END IF
C
C*		Check for COUN.
C
		CALL DM_FKEY  ( iflno, 'COUN', type, loc, ier )
		IF  ( type .eq. 'COL' )  THEN
		    ier = MV_SWP4 ( 1, kheadr (loc,jloc,iflno),
     +				       kheadr (loc,jloc,iflno) )
		END IF
C
C*		Check for GPM1 which is the start of three words.
C
		CALL DM_FKEY  ( iflno, 'GPM1', type, loc, ier )
		IF  ( type .eq. 'COL' )  THEN
		    ier = MV_SWP4 ( 3, kheadr (loc,jloc,iflno),
     +				       kheadr (loc,jloc,iflno) )
		END IF
C
C*		Check for GVCD. Swap only if the value is greater
C*		than the number of standard vertical coordinates.
C
		CALL DM_FKEY  ( iflno, 'GVCD', type, loc, ier )
		IF  ( type .eq. 'COL' )  THEN
		    IF  ( kheadr (loc,jloc,iflno) .gt. nvstd )  THEN
			ier = MV_SWP4 ( 1, kheadr (loc,jloc,iflno),
     +				           kheadr (loc,jloc,iflno) )
		    END IF
		END IF
	    END IF
C
C*	    Write header to file.
C
	    istart = kpcolh (iflno) + (jpos-1) * (kckeys (iflno) + 1)
	    length = kckeys (iflno) + 1
	    CALL DM_WINT ( iflno, istart, length, 
     +			   kheadr ( 0, jloc, iflno ), iret )
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
		IF  ( type .eq. 'COL' )  THEN
		    ier = MV_SWP4 ( 1, kheadr (loc,jloc,iflno),
     +				       kheadr (loc,jloc,iflno) )
		    IF  ( loc2 .gt. 0 )
     +			ier = MV_SWP4 ( 1, kheadr (loc2,jloc,iflno),
     +				           kheadr (loc2,jloc,iflno) )
		END IF
C
C*		Check for STAT.
C
		CALL DM_FKEY  ( iflno, 'STAT', type, loc, ier )
		IF  ( type .eq. 'COL' )  THEN
		    ier = MV_SWP4 ( 1, kheadr (loc,jloc,iflno),
     +				       kheadr (loc,jloc,iflno) )
		END IF
C
C*		Check for COUN.
C
		CALL DM_FKEY  ( iflno, 'COUN', type, loc, ier )
		IF  ( type .eq. 'COL' )  THEN
		    ier = MV_SWP4 ( 1, kheadr (loc,jloc,iflno),
     +				       kheadr (loc,jloc,iflno) )
		END IF
C
C*		Check for GPM1 which is the start of three words.
C
		CALL DM_FKEY  ( iflno, 'GPM1', type, loc, ier )
		IF  ( type .eq. 'COL' )  THEN
		    ier = MV_SWP4 ( 3, kheadr (loc,jloc,iflno),
     +				       kheadr (loc,jloc,iflno) )
		END IF
C
C*		Check for GVCD. Swap only if the value is greater
C*		than the number of standard vertical coordinates.
C
		CALL DM_FKEY  ( iflno, 'GVCD', type, loc, ier )
		IF  ( type .eq. 'COL' )  THEN
		    IF  ( kheadr (loc,jloc,iflno) .gt. nvstd )  THEN
			ier = MV_SWP4 ( 1, kheadr (loc,jloc,iflno),
     +				           kheadr (loc,jloc,iflno) )
		    END IF
		END IF
	    END IF
C
C*	    If this is the maximum header defined, save in common.
C
	    IF  ( klstcl ( iflno ) .lt. jpos ) klstcl (iflno) = jpos
C
	END IF
C*
	RETURN
	END
