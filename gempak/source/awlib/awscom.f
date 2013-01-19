	SUBROUTINE AW_SCOM ( bdata, irept, lenbyt, iff, iret )
C************************************************************************
C* AW_SCOM                                                              *
C*                                                                      *
C* This subroutine is the system binary data block.  This block holds   *
C* the 23-byte communications header of an AWIPS product.  This         *
C* subroutine represents mode 2, submode 2.                    		*
C* 								        *
C* AW_SCOM ( BDATA, IREPT, LENBYT, IFF, IRET )                        	*
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
C* T. Piper/GSC		11/98	Updated prolog				*
C* A. Hardy/GSC         11/98   Added assignment do loop                *
C* A. Hardy/GSC		 3/99   Updated prolog				*
C* A. Hardy/GSC          2/00   Increase the file byte size 20k -> 80k  *
C* A. Hardy/GSC         10/00   Put file number and dbug in awcmn.cmn	*
C* T. Piper/SAIC	 2/03	Changed b to bdata and added INT	*
C************************************************************************
 	INCLUDE         'awcmn.cmn'
C*
	BYTE		bdata(*)
        CHARACTER*9     nmcpil
        INTEGER         linkad, icntlfd, messid1, messid2, typpry,
     +                  min1, min2, min3, minutes, ivalid,
     +                  iprdver, node, mesadd1, mesadd2
        INTEGER         ibte(80000)
C------------------------------------------------------------------------
	IF ( dbug .eq. 'y' ) THEN
	    write(flun,*)'In AW_SCOM' 
        END IF
 	iret = 0
        icnt = irept + 4
	itotln = irept + lenbyt
	IF ( iff .eq. 0 ) itotln = itotln - 1
C
C*      Reassign byte array to an integer array.
C
        DO i = 1, 80000
            ibte(i) = bdata(i)
        END DO
C
C*      Get the link address, control field, message ID, message 
C*      address and meassage type and priority.
C
        linkad = ibits ( ibte( icnt ), 0, 8 )
        icntlfd = ibits ( ibte( icnt+1 ), 0, 8 )
        icnt = icnt + 2
        messid1 = ibits ( ibte( icnt ), 0, 8 )
        messid2 = ibits ( ibte( icnt+1 ), 0, 8 )
        icnt = icnt + 2
        mesadd1 = ibits ( ibte( icnt ), 0, 8 )
        mesadd2 = ibits ( ibte( icnt+1 ), 0, 8 )
        icnt = icnt + 2
        typpry=  ibits ( ibte( icnt ), 0, 8 )
        IF ( dbug .eq. 'y' ) THEN
            write(flun,49)'LINK ADDRESS','CONTROL FIELD'
 49         format(a12,5x,a15)
            write(flun,50)linkad, icntlfd
 50         format(5x,i2,15x,i2)
            write(flun,69)'MESSAGE ID','MESSAGE ADDRESS','MESSAGE TYPE'
 69         format(a10,5x,a15,5x,a12)
            write(flun,70)messid1,messid2, mesadd1, mesadd2, typpry
 70         format(2x,Z2.2,2x,z2.2,10x,Z2.2,2x,z2.2,15x,i2)
        END IF
C
C*      Get the full pil name.
C
        DO i = 1, 9
            nmcpil( i:i ) = CHAR(INT(bdata(icnt+i)))
        END DO
        icnt = icnt + 10
C
        IF ( dbug .eq. 'y' ) THEN
            write(flun,*)'FULL PIL NAME'
            write(flun,36)nmcpil
 36         format(3x,a9)
        END IF
C
C*      Get the number of minutes from the beginning of the year.
C      
        min1 = ibits ( ibte( icnt ), 0, 8 )
        min2 = ibits ( ibte( icnt+1 ), 0, 8 )
        min3 = ibits ( ibte( icnt+2 ), 0, 8 )
        minutes = ( min1 * 16384 ) + ( min2*128 ) + min3
        icnt = icnt + 3

        CALL AW_ADBT (bdata, icnt, ivalid, iret)
        icnt = icnt + 2

        iprdver = ibits ( ibte( icnt ), 0, 8 )
        node = ibits ( ibte( icnt+1 ), 0, 8 )
        IF ( dbug .eq. 'y' ) THEN
            write(flun,75)'MINUTES','MESSAGE TIME','PRODUCT NUMBER',
     +                    'NODE'
 75         format(a8,5x,a15,5x,a15,5x,a5)
            write(flun,76)minutes, ivalid, iprdver, node
 76         format(i6,15x,i2,15x,i2,15x,i2)
        END IF
C
	IF ( dbug .eq. 'y' ) THEN
	    IF ( iff .eq. 0 ) WRITE (flun,*)'Checksum found.'
	END IF 
C*
	RETURN
	END
