	SUBROUTINE DA_INFO ( iflno, iret )
C************************************************************************
C* DA_INFO								*
C*									*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 6/13	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'../dm/dmcmn.cmn'
C*
	PARAMETER	( MMKYHD = (MMKEY+1)*MMHDRS )
C*
	CHARACTER	dalbl*28, label*28, key*8, key2*8, tstr*12,
     +			hdrtyp*8
	INTEGER		rowhdr(MMKYHD), colhdr(MMKYHD)
C
	DATA		dalbl / 'AWIPS DB CONFIGURATION FILE ' /
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the file label and check it
C
	CALL DA_GETLABEL ( iflno, label, ier )
	CALL ST_RNUL ( label, label, lens, ier )
	IF  ( label .ne. dalbl )  THEN
	    iret = -8
	    RETURN
	END IF
C
C*	Set constant values in the DM common block.
C
	kmachn ( iflno ) = MTMACH
C
	kvmst  ( iflno ) = .true.
	kieeet ( iflno ) = .false.
	mvmst  = .true.
	mieeet = .false.
C
	kmissd ( iflno ) = IMISSD
	smissd ( iflno ) = RMISSD
C
	kpfile ( iflno ) = 0
	kprkey ( iflno ) = 0
	kprowh ( iflno ) = 0
	kpckey ( iflno ) = 0
	kpcolh ( iflno ) = 0
	kppart ( iflno ) = 0
	kpdmgt ( iflno ) = 0
	kldmgt ( iflno ) = 0
	kpdata ( iflno ) = 0
C
C*	Set values in the DM common from configuration table.
C
	CALL DA_GETVERS ( iflno, ivers, ier )
	kversn ( iflno ) = ivers
C
	CALL DA_GETSORC ( iflno, itype, isrce, ier )
	kftype ( iflno ) = itype
	kfsrce ( iflno ) = isrce
C
	CALL DA_GETNUMS ( iflno, irow, icol, iprt, ifhd, ier )
	krkeys ( iflno ) = irow
	kckeys ( iflno ) = icol
	kprt   ( iflno ) = iprt
	kfhdrs ( iflno ) = ifhd
C
	izero = 0
	key = 'ROW'
	CALL ST_NULL ( key, key, lenk, ier )
	DO  ii = 1, krkeys(iflno)
	    CALL DA_GETNAME ( iflno, key, ii, izero, tstr, ier )
	    CALL ST_RNUL ( tstr, kkrow(ii,iflno), lens, ier )
	END DO
C
	key = 'COL'
	CALL ST_NULL ( key, key, lenk, ier )
	DO  ii = 1, kckeys(iflno)
	    CALL DA_GETNAME ( iflno, key, ii, izero, tstr, ier )
	    CALL ST_RNUL ( tstr, kkcol(ii,iflno), lens, ier )
	END DO
C
	key = 'PART'
	CALL ST_NULL ( key, key, lenk, ier )
	DO  ii = 1, kprt(iflno)
	    CALL DA_GETNAME ( iflno, key, ii, izero, tstr, ier )
	    CALL ST_RNUL ( tstr, kprtnm(ii,iflno), lens, ier )
	    CALL DA_GETPART ( iflno, ii, ktyprt(ii,iflno),
     +				kparms(ii,iflno), ier )
	    key2 = 'PARM'
	    CALL ST_NULL ( key2, key2, lenk, ier )
	    DO  jj = 1, kparms(ii,iflno)
		CALL DA_GETNAME ( iflno, key2, ii, jj, tstr, ier )
		CALL ST_RNUL ( tstr, kprmnm(jj,ii,iflno), lens, ier )
		CALL DA_GETPARM ( iflno, ii, jj, kscale(jj,ii,iflno),
     +				  koffst(jj,ii,iflno),
     +				  kbits(jj,ii,iflno), ier )
	    END DO
	END DO
C
	key = 'FHDR'
	CALL ST_NULL ( key, key, lenk, ier )
	DO  ii = 1, kfhdrs (iflno)
	    CALL DA_GETNAME ( iflno, key, ii, izero, tstr, ier )
	    CALL ST_RNUL ( tstr, kfhnam(ii,iflno), lens, ier )
	    CALL DA_GETFHDR ( iflno, ii, kfhtyp(ii,iflno),
     +				kfhlen(ii,iflno), ier )
	END DO
C
C*	Get the Row header data
C
	hdrtyp = 'ROW'
	CALL ST_NULL ( hdrtyp, hdrtyp, lenh, ier )
	CALL DA_GETHEADER ( iflno, hdrtyp, krow(iflno), rowhdr, ier )
	ihd = 0
	irw = 1
	DO  nn = 1, krow(iflno)
	    ihd = ihd + 1
	    DO  mm = 0, krkeys(iflno)
	    	kheadr ( mm, ihd, iflno ) = rowhdr ( irw )
		irw = irw + 1
	    END DO
	END DO
	klstrw(iflno) = krow(iflno)
C
C*	Get the Column header data
C
	hdrtyp = 'COL'
	CALL ST_NULL ( hdrtyp, hdrtyp, lenh, ier )
	CALL DA_GETHEADER ( iflno, hdrtyp, kcol(iflno), colhdr, ier )
	icl = 1
	DO  nn = 1, kcol(iflno)
	    ihd = ihd + 1
	    DO  mm = 0, kckeys(iflno)
	    	kheadr ( mm, ihd, iflno ) = colhdr ( icl )
		icl = icl + 1
	    END DO
	END DO
	klstcl(iflno) = kcol(iflno)
C*
	RETURN
	END
