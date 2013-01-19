	SUBROUTINE AW_READ ( fname, dflag, iret)
C************************************************************************
C* AW_READ                                                              *
C*                                                                      *
C* This subroutine reads and plots an AWIPS graphics file.  This        *
C* subroutine uses the FCM-S2-1994 standard format except where noted.  *
C*								        *
C* AW_READ ( FNAME, DFLAG, IRET )                                       *
C*									*
C* Input parameters:                                                    *
C*      FNAME           CHAR*		Input file name                 *
C*      DFLAG           CHAR*		Debug flag                      *
C*									*
C* Input parameters:                                                    *
C*      IRET            INTEGER         Return code                     *
C*									*
C**								        *
C* Log:									*
C* A. Hardy/GSC          7/98                                           *
C* A. Hardy/GSC          8/98           Added initial skip over 23 bytes*
C* A. Hardy/GSC          9/98           Changed header printout sizes   *
C* T. Piper/GSC		11/98		Updated prolog			*
C* A. Hardy/GSC         11/98   	Added do loop;fixed print range *
C* A. Hardy/GSC          3/99           Switched dbug/iend in AW_PROD   *
C* A. Hardy/GSC          2/00   Increase the file byte size 20k -> 80k  *
C* A. Hardy/GSC          3/00   Increased record length storage size    *
C* S. Jacobs/NCEP	 4/00	Added check for data from noaaport feed	*
C* A. Hardy/GSC         10/00   Added FL_SWOP;Put file number and dbug  *
C*				in awcmn.cmn				*
C* T. Piper/SAIC	 2/03	Changed b to bdata and added INT	*
C* S. Jacobs/NCEP	12/10	Changed header length from 46 to 41	*
C* S. Jacobs/NCEP	 2/11	Changed to search for Redbook header	*
C************************************************************************
	INCLUDE 	'awcmn.cmn'
C*
	CHARACTER*(*)	fname, dflag
	CHARACTER*80	hedbeg, stchar
	BYTE		bdata(80000)
	INTEGER		nbytes, irecd, itm1, itm2, itime1, itime2
	INTEGER		iend, irept, mode, itlen, isub, iadbyt 
	LOGICAL		atend, found
	INTEGER		ibte(80000)
C------------------------------------------------------------------------
	atend  = .false.
	iend   = 0
C
C*	Open file to be read in.  Read bytes in the file. 
C
        IF ( ( dflag .eq. 'D' ) .or. (dflag .eq. 'd' ) ) THEN
            dbug = 'y'
            CALL FL_SWOP ( 'awips.out', flun, iret )
        END IF
	CALL ST_NULL (fname, fname, lens, iret)
	CALL AWRFIL ( fname, nbytes, bdata, iret)
C
C*      Reassign byte array to an integer array.
C
        DO i = 1, 80000
            ibte(i) = bdata(i) 
        END DO
C
        IF  ( dbug .eq. 'y' ) THEN
	    write(flun,*)'FILE NAME: ',fname
	    write(flun,*)'TOTAL LENGTH OF FILE IN BYTES : ', nbytes 
        END IF
C
C*      Decoding the queue descriptor information.  The first 79 
C*      characters are encoded in EPCDIC for files from the CRAY
C*      or files created in the AWIPS format.
C
	CALL ST_ETOA ( bdata, 80, hedbeg, iret )
        IF ( hedbeg(1:16) .ne. 'QUEUE DESCRIPTOR') THEN
C
C*	    Find the beginning of the Redbook encoding. The first
C*	    code values are (decimal) 64-16-1.
C
	    istrt = 1
	    found = .false.
	    DO WHILE ( ( istrt .lt. 50 ) .and. ( .not. found ) )
		IF ( bdata(istrt) .eq. 64 ) THEN
		    IF ( ( bdata(istrt+1) .eq. 16 ) .and.
     +			 ( bdata(istrt+2) .eq.  1 ) ) THEN
			irept = istrt
			found = .true.
		    END IF
		END IF
		istrt = istrt + 1
	    END DO
	    IF ( .not. found ) THEN
		iret = -1
		RETURN
	    END IF
          ELSE 
	    irept  = 102
            IF ( dbug .eq. 'y' ) THEN
	        write(flun,*)'===> HEADER INFORMATION <==='
	        write(flun,*)hedbeg(1:16)
	        write(flun,*)' '
C
C*              Byte positions in header for record length and number
C*              of bytes in the record.
C
                CALL AW_ADBT (bdata, 21, iadbyt, iret )
	        irecd = ( iadbyt * 80 ) + bdata(23) 
                write(flun,*)'RECORD SIZE : ',irecd
	        write(flun,*)' '
	        write(flun,*)'HEADER ORIGIN NAME : ',hedbeg(29:38)
	        write(flun,*)' '
	        itm1 = ibits ( ibte(39), 0, 8)
                itime1 = ( ( itm1/16 ) * 10 ) + ( mod ( itm1,16 ) )
	        itm2 = ibits ( ibte(40), 0, 8)
                itime2 = ( ( itm2/16 ) * 10 ) + ( mod ( itm2,16 ) )
	        write(flun,*)'HOURS : ',itime1,'   MINUTES : ',itime2
	        write(flun,*)' '
	        write(flun,*)'FIVES : ',hedbeg(41:45)
C
C*              From 81 to 101 is the WMO Header information if the file
C*              being decoded is from the CRAY.  
C
        	    write(flun,*)'===> WMO INFORMATION <==='
        	j = 1
        	DO i = 81, 98
	            stchar( j:j ) = CHAR(INT(bdata(i)))
	            j = j + 1
               END DO
	       write(flun,62) stchar(1:j-1)
 62            format(a)
	       write(flun,*)' '
               write(flun,*)'===> BEGINNING BLOCK READING <==='
            END IF
        END IF
C
C*      IREPT is the location in bytes for the beginning of the
C*      first block to be decoded. This will skip over the AWIPS 
C*      block mode 0 submode 1, which is their version of the WMO 
C*      header. Otherwise IREPT is set to byte position 102 for
C*      CRAY files.
C
        DO WHILE ( .not. atend )
          IF ( dbug .eq. 'y' ) THEN
           write(flun,*)'*********************************************'
          END IF
C
C*	    Get the field flag and length of byte pairs of the block 
C*          to be decoded.  The field flag can be 00, 01, or 11. 
C
            iff   = ibits ( ibte(irept), 6, 2 )
	    ilen1 = ibits ( ibte(irept), 0, 6 )
	    ilen2 = ibits ( ibte(irept+1), 0, 8 )
C
	    IF ( ilen1 .gt. 127 ) ilen1 = ilen1 - 256
	    IF ( ilen2 .lt. 0 ) ilen2 = ilen2 + 256
            itlen  = ( ilen1 * 256 ) + ilen2
	    lenbyt = itlen * 2
C
C*          Decode the mode and submode of the block.  
C
	    mode =  bdata( irept+2 )
	    isub =  bdata( irept+3 )
            IF ( dbug .eq. 'y' ) THEN 
                write(flun,31)'FF','LENGTH','MODE','SUBMODE','BYTE NUM '
 31             format(a2,5x,a6,7x,a4,5x,a7,5x,a8)
                write(flun,32)iff,lenbyt,mode,isub,irept
 32             format(i2,5x,i4,7x,i4,7x,i4,7x,i5)
	    END IF
C
C*          Send block to appropriate subroutine to be decoded.
C*          Increment the byte count after each block is read.
C
            IF( mode .eq. 1 ) THEN
C
C*              Product data set control.
C
                CALL AW_PROD ( bdata, irept, isub, lenbyt, iff, 
     +                         iend, iret )
 	        irept = irept + lenbyt
                IF ( dbug .eq. 'y' ) THEN 
		    write(flun,2)'Byte at END of PROD ',irept
 2                  format(a20,2x,i5)
		END IF 
              ELSE IF ( mode .eq. 2 ) THEN
C
C*              Systems data.
C
                CALL AW_SYST (bdata, irept, isub, lenbyt, iff, iret )
 	        irept = irept + lenbyt
                IF ( dbug .eq. 'y' ) 
     +              write(flun,2)'Byte at END of SYST ',irept
C             ELSE IF ( mode .eq. 3 ) THEN
C
C*              Formatted Binary.
C
C               CALL AW_FRMB (bdata, irept, isub, lenbyt, iff, iret )
C	        irept = irept + lenbyt
              ELSE IF ( mode .eq. 4 ) THEN
C
C*              Vector graphics.
C
                CALL AW_GRPH (bdata, irept, isub, lenbyt, iff, iret )
 	        irept = irept + lenbyt
                IF ( dbug .eq. 'y' ) 
     +              write(flun,*)'Byte at END of GRPH ',irept
              ELSE IF ( mode .eq. 5 ) THEN
C
C*              Alphanumeric.
C
                CALL AW_ALPH ( bdata, irept, isub, lenbyt, iff, iret )
 	        irept = irept + lenbyt
                IF ( dbug .eq. 'y' )
     +              write(flun,2)'Byte at END of ALPH ',irept
C             ELSE IF ( mode .eq. 6 ) THEN
C
C*              Raster Scan.
C
C               CALL AW_RAST (bdata, irept, isub, lenbyt, iff, iret )
C	        irept = irept + lenbyt
C             ELSE IF ( mode .eq. 7 ) THEN
C
C*              Gridded.
C
C               CALL AW_GRID (bdata, irept, isub, lenbyt, iff, iret )
C	        irept = irept + lenbyt
              ELSE
                IF ( dbug .eq. 'y' ) THEN 
 	            write(flun,*)'ERROR IN READING BLOCK MODE.'
 	            write(flun,3)'Mode, submode was read as: ',mode,isub
 3                  format(a28,2x,i2,2x,i2)
                END IF
		atend = .true.
            END IF
C
C*	    Reached the End-of-Block in the file.
C
           IF( iend .eq. 13 ) atend = .true.
	END DO
	CALL GEPLOT (iret)
        IF ( dbug .eq. 'y' ) THEN 
	    write(flun,*)'End of program.'
	    CALL FL_CLOS ( flun, ier )
        END IF 
C*
	RETURN
	END
