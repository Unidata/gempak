#define PNG_DEBUG 3

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <limits.h>
#include "png.h"

png_size_t pngcount=0;

png_structp png_ptr;
png_infop info_ptr,end_info;

void read_row_callback(png_structp pngp,png_uint_32 rownum,int pass)
{
/*printf("test callback %d %d\n",rownum,pass);*/
}

void readpng_row(rowp)
unsigned char *rowp;
{
png_read_row(png_ptr,(png_bytep)rowp, NULL);
}


void png_read(FILE *fp, int line, int rows, unsigned char *imgData, int *ier)
{
/*int bit_depth=8;
int color_type=PNG_COLOR_TYPE_GRAY;
int interlace_type = PNG_INTERLACE_NONE;
int compression_type = PNG_COMPRESSION_TYPE_DEFAULT;
int filter_type = PNG_FILTER_TYPE_DEFAULT;
png_uint_32 height,width;*/

int bit_depth;
int color_type;
int interlace_type;
int compression_type;
int filter_type;
char sig[8];

png_uint_32 height,width;
int i;
long ntot=0;

if ( imgData == NULL )
   {
   *ier = -1;
   printf("whoa, null imgData pointer for png image\n");
   return;
   }

fread ( sig, 1, 8, fp );

i = !png_sig_cmp ((png_bytep)sig, 0, 8);

if ( !i )
   {
   *ier = -2;
   printf("not a png file %d %d %d %d %d %d %d %d\n",
      sig[0], sig[1], sig[2], sig[3],
      sig[4], sig[5], sig[6], sig[7]);
   return;
   }

png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING,
          NULL, NULL, NULL);

if(!png_ptr) 
   {
   *ier = -3;
   printf("png_ptr error\n");
   return;
   }

info_ptr = png_create_info_struct(png_ptr);

if(!info_ptr)
   {
   *ier = -4;
   printf("info_ptr error\n");
   png_destroy_read_struct(&png_ptr,(png_infopp)NULL, (png_infopp)NULL);
   return;
   }

end_info = png_create_info_struct(png_ptr);

if(!end_info)
   {
   *ier = -4;
   printf("end_info error\n");
   png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);
   return;
   }

/*if(setjmp(png_ptr->jmpbuf))*/
if(setjmp(png_jmpbuf(png_ptr)))
   {
   *ier = -5;
   printf("setjmp error\n");
   png_destroy_read_struct(&png_ptr,&info_ptr,&end_info);
   return;
   }

png_init_io(png_ptr,fp);
png_set_sig_bytes(png_ptr, 8);
/*png_set_read_status_fn(png_ptr,read_row_callback);*/

png_read_info(png_ptr,info_ptr);
png_get_IHDR(png_ptr,info_ptr,&width,&height,&bit_depth,&color_type,
             &interlace_type,&compression_type,&filter_type);

for(i=0;i<height;i++)
   {
   /*readpng_row(imgData+ntot);*/
   png_read_row ( png_ptr, (png_bytep)(imgData + ntot), NULL );
   ntot += width;
   }
png_destroy_read_struct(&png_ptr,&info_ptr,&end_info);
}

