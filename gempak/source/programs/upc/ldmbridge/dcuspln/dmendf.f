	SUBROUTINE DMENDF ( isffln, iret )
C************************************************************************
C* DMENDF								*
C*									*
C* This subroutine makes sure that the buffered writes are written 	*
C* upon receipt of an EOF on the input stream.				*
C*									*
C* DMENDF ( ISFFLN, IHEADR, FIRST, IPOS, ICOUNT, IRET )			*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*					  -1 = DM Write error		*
C*					  -2 = No spot to write in file *
C**									*
C* Log:									*
C* P. Bruehl/Unidata	 4/94   Modified for real time lightning ingest *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'nldn.prm'
	INCLUDE		'dmcmn.cmn'
C*
	CHARACTER	error*160
C------------------------------------------------------------------------
	iptcnt = icount
	ibuffer = 500
C
C*	Write data management block to file
C
	CALL DM_WDMG ( isffln, iret )
        IF ( iret .ne. 0 ) THEN
	  iret = -1
	  RETURN
	ENDIF
C
C*	Write headers to file
C
        IF ( istart .eq. 0 ) THEN
	 iret = -2
	 RETURN
	ENDIF
        ilen = length * iptcnt
        CALL DM_WINT ( isffln, istart, ilen, jheadr, iret)
        IF ( iret .ne. 0 ) THEN
	  iret = -1
	  RETURN
	ENDIF
C
C*	Write data pointers to file
C
	IF (ipoint .eq. 0 ) THEN
	  iret = -2
	  RETURN
	ENDIF
	CALL DM_WINT ( isffln, ipoint, iptcnt, iptlist, iret )
	IF ( iret .ne. 0 ) THEN
	  iret = -1
 	  RETURN
	ENDIF
C
C*      Write data to file
C
        IF ( istdat .eq. 0 ) THEN
	  iret = -2
	  RETURN
	ENDIF
        ilen = lendat * iptcnt
        CALL DM_WINT ( isffln, istdat, ilen, iwarr, iret )
	IF ( iret .ne. 0 ) THEN
	  iret = -1
	  RETURN
	ENDIF
C
C*	Take care of incrementing placement, in case we need to
C*	write more.
C
C*	Inc ipoint, istart, istdat; reset iptcnt, idata, ihead; 
C*	zero out pointer, data, and header arrays.
C
	ipoint = kpdata (isffln) + (icol)
	istart = istart + (length * iptcnt)
	istdat = kpnext (isffln)
	iptcnt = 0
	idata = 0
	ihead = 0
	DO iclean = 1, ibuffer
	  iptlist (iclean) = 0
	END DO
	DO iclean = 1, ( ( MMPARM+2 )*ibuffer )
	  iwarr (iclean) = 0
	END DO
	DO iclean = 1, ( ibuffer * 10 )
	  jheadr (iclean) = 0
	END DO
	icount = iptcnt
	iret = 0
	RETURN
	END

