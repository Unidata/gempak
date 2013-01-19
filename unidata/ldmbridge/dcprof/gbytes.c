#include <stdio.h>
#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define gbytes	gbytes_
#define gbyte	gbyte_
#endif

/* Tools for storage/retrieval of arbitrary size bytes from 64 bit words
    (note - this version is not currently (6/30/88) described in the
    gbytes document)

    gbytes(p,u,q,b,s,n)
    gbyte (p,u,q,b)
    sbytes(p,u,q,b,s,n)
    sbyte (p,u,q,b)

             q >= 0     number of bits to be skipped preceeding first byte in p
      0 <    b < sword  byte size
             s >= 0     number of bits to be skipped between bytes
             n >= 0     number of bytes to be packed/unpacked

    gbytes unpacks n b bit bytes from p into u, starting by skipping
         q bits in p, then skipping s bits between bytes.
    gbyte unpacks one such byte.
    sbytes   packs n b bit bytes from u into p, starting by skipping
         q bits in p, then skipping s bits between bytes.
    sbyte  packs one such byte. */
# define SWORD 32                              /* Word size in bits */
# define MASK 0xffffffff                       /* Mask of sword bits */
# define G1BYTE(p,q,b) ((b==32 ? MASK : ~(MASK<<b)) & (p>>(SWORD-(q+b))))
                                               /* Get 1 word contained byte */
# define MASK1(q,b) (b==32 ? MASK : (~(MASK<<b)<<(SWORD-(q+b))))
                                               /* Mask of sword bits */
gsbytes(p,u,q,b,s,n,gsbyte)   /* Common code for gbytes, sbytes */
unsigned p[],u[],*q,*b,*s,*n;
int (*gsbyte)();
{       unsigned jp,jq,ju;
        jp = 0;
        jq = *q;
        for (ju = 0; ju < *n; ++ju) {
                 (*gsbyte)(&p[jp],&u[ju],&jq,b);
                 jq += *b + *s;
                 jp += jq/SWORD;
                 jq %= SWORD;
        }
}
gbytes (p,u,q,b,s,n)
unsigned p[],u[],*q,*b,*s,*n;
{
        int gbyte ();
        gsbytes(p,u,q,b,s,n,gbyte);
}

do_flip (p, tmp_p)
unsigned p,*tmp_p;
{
unsigned np,tp;

if((MTMACH == MTULTX)||(MTMACH == MTALPH)||
   (MTMACH == MTVAX)||(MTMACH == MTLNUX))
   {
   tp = (p & 0x00ff) << 24;
   np = (p >> 8) & 0x00ff;
   tp = tp + (np << 16);
   np = (p >> 16) & 0x00ff;
   tp = tp + (np << 8);
   np = (p >> 24) & 0x00ff;
   tp = tp + np;
   }
else
   tp = p;

*tmp_p = tp;

}

gbyte (p,u,q,b)
unsigned p[],*u,*q,*b;
{
        unsigned qb,j,lb,jq,jb,tmp_p;

        jq = *q;
        jb = *b;
        if (jq >= SWORD) {
                 j = jq/SWORD; /* number of words offset */
                 jq %= SWORD;  /* odd bits of offset     */
        }
        else {
                 j=0;
        }
        qb = jq + jb;

        if (qb > SWORD) {
                 qb = SWORD - jq;
                 jb -= qb;
                 do_flip(p[j],&tmp_p);
                 lb = (G1BYTE(tmp_p,jq,qb)) << jb;
                 jq = 0;
                 j++;  /* increment to next word */
        }
        else lb = 0;
        do_flip(p[j],&tmp_p);
        *u = lb + (G1BYTE(tmp_p,jq,jb));
}
sbytes_(p,u,q,b,s,n)
long p[],u[],*q,*b,*s,*n;
{
        int sbyte_();
        gsbytes(p,u,q,b,s,n,sbyte_);
}
sbyte_(p,u,q,b)
long p[],*u,*q,*b;
{
        long qb,j,jq,jb,rb;

        jq = *q;
        jb = *b;
        if (jq >= SWORD) {
                 j = jq / SWORD;    /* number of words offset */
                 jq %= SWORD;       /* odd bit offset         */
        }
        else {
                 j = 0;
        }
        qb = jq + jb;
        if (qb > SWORD) {
                 qb = SWORD - jq;
                 jq = SWORD - jb;
                 jb -= qb;
                 p[j] = ((p[j] >> qb) << qb) + (G1BYTE(*u,jq,qb));
                 jq = 0;
                 j++;  /* point to next word */
        }
        rb = G1BYTE(*u,SWORD-jb,jb);
        p[j] = (p[j] & ~MASK1(jq,jb)) + (rb << SWORD-(jb+jq));
}

