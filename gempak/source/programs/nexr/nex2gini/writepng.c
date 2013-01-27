#include <stdio.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <png.h>

#include "writepng.h"

/* Demonstration program for writing a GINI image as a png
image with GINI header */

png_structp ginipng_ptr;
png_infop giniinfo_ptr;
int prodoff;
int png_deflen;
int PNG_write_calls=0;
char *prodmap;


int read_short1(x)
    unsigned char *x;
{
short value;
int ival;
value = (x[0] << 8) | x[1];
ival = value;
return(ival);
}

int png_get_prodlen()
{
return(prodoff);
}

void png_header(char *memheap, int length)
{
memcpy(prodmap+prodoff,memheap,length);
prodoff += length;
png_deflen += length;
}

void unidata_output_flush_fn(png_structp png_p)
{
printf("flush called %d\n",png_deflen);
}

void write_row_callback(png_structp pngp,png_uint_32 rownum,int pass)
{
/*printf("test callback %d %d [deflen %d]\n",rownum,pass,png_deflen);*/
}

void unidata_write_data_fn(png_structp png_p, png_bytep data, png_uint_32 length)
{
png_header((char *)data,length);
}

void png_set_memheap(char *memheap)
{
prodmap = memheap;
prodoff = 0;
png_deflen = 0;
}

void pngout_init(int width, int height)
{
char filename[80];
int bit_depth=8;
int color_type=PNG_COLOR_TYPE_GRAY;
int interlace_type = PNG_INTERLACE_NONE;
int compression_type = PNG_COMPRESSION_TYPE_DEFAULT;
int filter_type = PNG_FILTER_TYPE_DEFAULT;

ginipng_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
          NULL, NULL, NULL);/*
          (png_voidp)user_error_ptr,user_error_fn,
          user_warning_fn);*/

if(!ginipng_ptr) return;

giniinfo_ptr = png_create_info_struct(ginipng_ptr);

if(!giniinfo_ptr)
   {
   png_destroy_write_struct(&ginipng_ptr,(png_infopp)NULL);
   return;
   }

if(setjmp(ginipng_ptr->jmpbuf))
   {
   png_destroy_write_struct(&ginipng_ptr,&giniinfo_ptr);
   return;
   }

png_set_write_fn(ginipng_ptr,giniinfo_ptr,(png_rw_ptr)unidata_write_data_fn,(png_flush_ptr)unidata_output_flush_fn);
png_set_write_status_fn(ginipng_ptr,write_row_callback);

/*png_set_compression_level(ginipng_ptr,Z_BEST_COMPRESSION);*/
png_set_IHDR(ginipng_ptr,giniinfo_ptr,width,height,bit_depth,color_type,
             interlace_type,compression_type,filter_type);
png_write_info(ginipng_ptr,giniinfo_ptr);
}

void pngwrite(char *memheap)
{
png_write_row(ginipng_ptr,(png_bytep)memheap);
PNG_write_calls++;

}

void pngout_end()
{
/* try this, we get core dumps if try to shut down before any writes take place */
if(PNG_write_calls > 0) png_write_end(ginipng_ptr,NULL);
png_destroy_write_struct(&ginipng_ptr,&giniinfo_ptr);
PNG_write_calls = 0;
}

