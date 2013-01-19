	SUBROUTINE DM_RHDA  ( iflno, iret )
C************************************************************************
C* DM_RHDA								*
C*									*
C* This subroutine reads all the row and column headers from a 		*
C* DM file.								*
C*									*
C* DM_RHDA  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* M. desJardins/GSFC	 5/90	Added check for character headers	*
C* K. Brill/NMC		 3/91	Add calls to MV_ functions		*
C* M. desJardins/GSFC	 5/91	Add logical vars for machine type	*
C* S. Jacobs/NCEP	 2/97	Added check for swapping GVCD		*
C* S. Jacobs/NCEP	 4/98	Added check for swapping STD2		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C*
	INTEGER		ibuff (MBLKSZ)
	CHARACTER	type*4, type2*4
C------------------------------------------------------------------------
C*	Read the row and column headers which are together in the file.
C
	istart = kprowh (iflno)
	CALL DM_RINT  ( iflno, istart, MBLKSZ, ibuff, iret )
	istart = istart + MBLKSZ
	length = 1
	knt    = 1
	klstrw ( iflno ) = 0
	klstcl ( iflno ) = 0
C
C*	Save the row headers.
C
	DO  i = 1, krow (iflno)
	    DO  j = 0, krkeys (iflno)
		IF  ( length .gt. MBLKSZ )  THEN
		    CALL DM_RINT ( iflno, istart, MBLKSZ, ibuff, iret )
		    IF  ( iret .ne. 0 ) RETURN
		    istart = istart + MBLKSZ
		    length = 1
		END IF
		kheadr ( j, knt, iflno ) = ibuff ( length )
		length = length + 1
	    END DO
	    IF  ( kheadr ( 0, knt, iflno ) .ne. IMISSD ) 
     +						klstrw (iflno) = knt
	    knt = knt + 1
	END DO
	IF ( iret .ne. 0 ) RETURN
C
C*	Save the column headers.
C
	DO  i = 1, kcol (iflno)
	    DO  j = 0, kckeys (iflno)
		IF  ( length .gt. MBLKSZ )  THEN
		    CALL DM_RINT ( iflno, istart, MBLKSZ, ibuff, iret )
		    IF  ( iret .ne. 0 ) RETURN
		    istart = istart + MBLKSZ
		    length = 1
		END IF
		kheadr ( j, knt, iflno ) = ibuff ( length )
		length = length + 1
	    END DO
	    IF  ( kheadr ( 0, knt, iflno ) .ne. IMISSD )
     +				klstcl ( iflno ) = knt - krow ( iflno )
	    knt = knt + 1
	END DO
C
C*	If the file was created on a different machine, make sure
C*	that the bytes are not swapped for character keywords.
C
	IF  ( ( kmachn (iflno) .ne. MTMACH ) .and.
     +	      ( ( kvmst ( iflno ) .and. ( .not. mvmst ) ) .or.
     +	      ( mvmst .and. ( .not. kvmst ( iflno ) ) ) ) )  THEN
C
C*	    Check for STID and STD2.
C
	    CALL DM_FKEY  ( iflno, 'STID', type, loc, ier )
	    CALL DM_FKEY  ( iflno, 'STD2', type2, loc2, ier )
	    IF  ( type .eq. 'ROW' )  THEN
		DO  i = 1, krow (iflno)
		    ier = MV_SWP4 ( 1, kheadr (loc,i,iflno),
     +			      	       kheadr (loc,i,iflno) )
		    IF  ( loc2 .gt. 0 )
     +			ier = MV_SWP4 ( 1, kheadr (loc2,i,iflno),
     +			      	           kheadr (loc2,i,iflno) )
		END DO
	      ELSE IF  ( type .eq. 'COL' )  THEN
		knt = krow (iflno) + 1
		DO  i = 1, kcol (iflno)
		    ier = MV_SWP4 ( 1, kheadr (loc,knt,iflno),
     +				       kheadr (loc,knt,iflno) )
		    IF  ( loc2 .gt. 0 )
     +			ier = MV_SWP4 ( 1, kheadr (loc2,knt,iflno),
     +				           kheadr (loc2,knt,iflno) )
		    knt = knt + 1
		END DO
	    END IF
C
C*	    Check for STAT.
C
	    CALL DM_FKEY  ( iflno, 'STAT', type, loc, ier )
	    IF  ( type .eq. 'ROW' )  THEN
		DO  i = 1, krow (iflno)
		    ier = MV_SWP4 ( 1, kheadr (loc,i,iflno),
     +				       kheadr (loc,i,iflno) )
		END DO
	      ELSE IF  ( type .eq. 'COL' )  THEN
		knt = krow (iflno) + 1
		DO  i = 1, kcol (iflno)
		    ier = MV_SWP4 ( 1, kheadr (loc,knt,iflno),
     +				       kheadr (loc,knt,iflno) )
		    knt = knt + 1
		END DO
	    END IF
C
C*	    Check for COUN.
C
	    CALL DM_FKEY  ( iflno, 'COUN', type, loc, ier )
	    IF  ( type .eq. 'ROW' )  THEN
		DO  i = 1, krow (iflno)
		    ier = MV_SWP4 ( 1, kheadr (loc,i,iflno),
     +				       kheadr (loc,i,iflno) )
		END DO
	      ELSE IF  ( type .eq. 'COL' )  THEN
		knt = krow (iflno) + 1
		DO  i = 1, kcol (iflno)
		    ier = MV_SWP4 ( 1, kheadr (loc,knt,iflno),
     +				       kheadr (loc,knt,iflno) )
		    knt = knt + 1
		END DO
	    END IF
C
C*	    Check for GPM1 which is the start of three words.
C
	    CALL DM_FKEY  ( iflno, 'GPM1', type, loc, ier )
	    IF  ( type .eq. 'ROW' )  THEN
		DO  i = 1, krow (iflno)
		    ier = MV_SWP4 ( 3, kheadr (loc,i,iflno),
     +				       kheadr (loc,i,iflno) )
		END DO
	      ELSE IF  ( type .eq. 'COL' )  THEN
		knt = krow (iflno) + 1
		DO  i = 1, kcol (iflno)
		    ier = MV_SWP4 ( 3, kheadr (loc,knt,iflno),
     +				       kheadr (loc,knt,iflno) )
		    knt = knt + 1
		END DO
	    END IF
C
C*	    Check for GVCD. Swap only if the value is greater
C*	    than the number of standard vertical coordinates.
C
	    nvstd = 6
	    CALL DM_FKEY  ( iflno, 'GVCD', type, loc, ier )
	    IF  ( type .eq. 'ROW' )  THEN
		DO  i = 1, krow (iflno)
		    IF  ( kheadr (loc,knt,iflno) .gt. nvstd )  THEN
			ier = MV_SWP4 ( 1, kheadr (loc,i,iflno),
     +					   kheadr (loc,i,iflno) )
		    END IF
		END DO
	      ELSE IF  ( type .eq. 'COL' )  THEN
		knt = krow (iflno) + 1
		DO  i = 1, kcol (iflno)
		    IF  ( kheadr (loc,knt,iflno) .gt. nvstd )  THEN
			ier = MV_SWP4 ( 1, kheadr (loc,knt,iflno),
     +					   kheadr (loc,knt,iflno) )
		    END IF
		    knt = knt + 1
		END DO
	    END IF
	END IF
C*
	RETURN
	END
