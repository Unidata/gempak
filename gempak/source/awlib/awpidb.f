	SUBROUTINE AW_PIDB ( bdata, irept, lenbyt, iff, iret )
C************************************************************************
C* AW_PIDB                                                              *
C*                                                                      *
C* This subroutine is the product identification block.  It identifies  *
C* the origin of the product, the classification, retention time,       *
C* product identifier, and file time.  This subroutine represents       *
C* mode 1, submode 1.                                                   *
C* 								        *
C* AW_PIDB ( BDATA, IREPT, LENBYT, IFF, IRET )                        	*
C*								        *
C* Input parameters: 						        *
C*      BDATA(*) BYTE		Byte data array                         *
C*      IREPT   INTEGER         Beginning byte position for the block   *
C*      LENBYT  INTEGER         Length of the block in bytes            *
C*      IFF     INTEGER         Length and checksum indicator           *
C*								        *
C* Output parameters:						        *
C*	IRET	INTEGER		Return code                             *
C**								        *
C* Log:									*
C* A. Hardy/GSC          7/98                                           *
C* T. Piper/GSC		11/98	Updated	prolog				*
C* A. Hardy/GSC         11/98   Added assignment do loop                *
C* A. Hardy/GSC		 3/99	Updated	prolog				*
C* A. Hardy/GSC          2/00   Increase the file byte size 20k -> 80k  *
C* A. Hardy/GSC         10/00   Fixed display;Put file number and dbug  *
C*         			in awcmn.cmn				*
C* T. Piper/SAIC	 2/03	Changed b to bdata and added INT	*
C************************************************************************
 	INCLUDE         'awcmn.cmn'
C*
	BYTE		bdata(*)
        CHARACTER       clas
	CHARACTER*40    orgid, orgcnt, prodid, flid
        INTEGER         icnt, irtm, imon, iday, ihour, 
     +                  imin, itotln, iyear 
        INTEGER         ibte(80000)
C------------------------------------------------------------------------
        IF ( dbug .eq. 'y' ) THEN
            write(flun,*)'In AW_PIDB'
	END IF
 	iret = 0
	icnt = irept + 3
	itotln = irept + lenbyt
	IF ( iff .eq. 0 ) itotln = itotln - 1
C
C*      Reassign byte array to an integer array.
C
        DO i = 1, 80000
            ibte(i) = bdata(i)
        END DO
C
C*      Get the 4 character originator identification, classification, 
C*      and retention time.
C
	DO i = 1, 4
	    orgid( i:i) = CHAR(INT(bdata(icnt+i)))
	END DO
	icnt = icnt + 5
	clas =  CHAR(INT(bdata(icnt)))
        irtm =  ibits ( ibte(icnt+1), 0, 8 )
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,11)'ORGINATING ID','CLASS','RETENTION TIME'
	    write(flun,10)orgid, clas, irtm
 11         format(a13,5x,a5,5x,a14)
 10         format(a6,25x,a,5x,i5)
	END IF
C
C*      Get the file indicator and product identifier.
C
        flid = CHAR(INT(bdata(icnt+2)))
	icnt = icnt + 2
	DO i = 1, 9
	    prodid(i:i) = CHAR(INT(bdata(icnt+i)))
	END DO
	icnt = icnt + 10
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,29)'FILE INDICATOR','PRODUCT ID'
 29         format(a15,15x,a11)
	    write(flun,30)flid, prodid
 30         format(5x,a1,25x,a9)
	END IF
C
C*      Get the product file time: year, month, day, hour and min.
C
	CALL AW_ADBT (bdata, icnt, iyear, iret )
C
	imon   = ibits ( ibte(icnt+2), 0, 8 )
	iday   = ibits ( ibte(icnt+3), 0, 8 )
	ihour  = ibits ( ibte(icnt+4), 0, 8 )
	imin   = ibits ( ibte(icnt+5), 0, 8 )
	icnt = icnt + 5
        IF ( dbug .eq. 'y' ) THEN
            write(flun,31)'YEAR','MONTH','DAY','HOUR',' MIN'
 31         format(4(a5,1x),a5)
            write(flun,32)iyear, imon, iday, ihour, imin
 32         format(4(i5,1x),i5)
	END IF
C
C*      Get the rest of the product identifier, if there is more.
C
        IF ( icnt .ne. itotln ) THEN
	    DO i = 1, 6
	        orgcnt(i:i) = CHAR(INT(bdata( icnt+i)))
	    END DO
            IF ( dbug .eq. 'y' ) THEN
	        write(flun,*)'ORIGNATOR INDICATOR CONTINUED' 
	        write(flun,33)orgcnt
 33             format(5x,a6)
	    END IF
        END IF
C
C*      Check for checksum. 
C
        IF ( dbug .eq. 'y' )  THEN
            IF ( iff .eq. 0 ) write(flun,*)'Checksum found.'
        END IF
C*
	RETURN
	END
