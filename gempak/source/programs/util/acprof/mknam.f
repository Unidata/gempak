        SUBROUTINE MKNAM (ival,istr,iret)
C	*******************************************************
C	Convert a numeric representation of Airport ID
C	into a printable string. Airport id is packed into
C	a single 36^4 number from 36 alphanumeric characters.
C
C	IVAL 	Integer	Value to unpack
C	ISTR	Character	Character string to return
C	IRET	INTEGER		Error code non-zero is error
C	
C	Chiz/Unidata
C	*******************************************************
        INTEGER ival
        CHARACTER*(*) istr
        CHARACTER*4 ichnam
        INTEGER nampos, icnt, iret

        nampos = 1
        ichnam = ' '
        iret = 0
        do while(ival .gt. 0)
           if(nampos.gt.8) then
              iret = -1
              return
           endif
           if(mod(ival,36) .lt. 10) then
              ichnam(nampos:nampos) = char(mod(ival,36) + 48)
           else
              ichnam(nampos:nampos) = char(mod(ival,36) + 65 - 10)
           endif

           ival = ival / 36
           nampos = nampos + 1
        end do
        icnt = 1
        do while(nampos .gt. 1)
           nampos = nampos - 1
           istr(icnt:icnt) = ichnam(nampos:nampos)
           icnt = icnt + 1
        end do
        RETURN
        END
