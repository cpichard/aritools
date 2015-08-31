#include <cstdio>
#include <cstdlib>
#include <cassert>
#include <cstring>


/* Tests to find arri raw packing scheme
 * r v rv r v rv 
 */

struct raw12_pixblock
{
    unsigned int c_hi: 8;
    unsigned int b: 12;
    unsigned int a: 12;
    unsigned int f_hi: 4;
    unsigned int e: 12;
    unsigned int d: 12;
    unsigned int c_lo: 4;
    unsigned int h: 12;
    unsigned int g: 12;
    unsigned int f_lo: 8;
} __attribute__((packed));


int main () {

    FILE *pFile = fopen("0000001.ari", "rb");
    if (pFile) {
        
        // file size:
        fseek (pFile , 0 , SEEK_END);
        size_t lSize = ftell (pFile);
        rewind (pFile);

        // Read width*heigth
        fseek(pFile, 0x0014, SEEK_SET);
        int width, height;
        fread(&width, sizeof(int), 1, pFile); 
        fread(&height, sizeof(int), 1, pFile); 

        // Seek to the beginning of image data
        fseek(pFile, 0x1000, SEEK_SET);
        
        const size_t nBitsPerPixel = 12;
        const size_t bufferSize = width*height*nBitsPerPixel/8;
        const size_t imageSizeInBytes = lSize-0x1000;

        assert(bufferSize == imageSizeInBytes);
        printf("buffer size=%lu\n", bufferSize);
        // Read all buffer 
        char *buffer = (char*)malloc(bufferSize);
        size_t sizeRead = fread(buffer, sizeof(char), bufferSize, pFile); 

        printf("read %lu bytes\n%dx%d\n", sizeRead, width, height);
        printf("sizeof unsigned short %lu\n", sizeof(unsigned short));
        // Allocate red plane and green plane
        typedef unsigned char OutType;
        const size_t outSize = width*height;
        OutType *outImage = (OutType*)calloc(outSize, sizeof(OutType));

        // Advance in buffer
        OutType *outImageIt = outImage;
        const size_t bytesPerLines = width*nBitsPerPixel/8;
        assert(bytesPerLines*height == imageSizeInBytes);
        for (int h=0; h<height; h++) {
            for (int w=0; w<bytesPerLines; w+=sizeof(raw12_pixblock)) {
                // Only 2 component per lines RG or BG
                unsigned char byte1 = 0;
                unsigned char byte2 = 0;
                unsigned char byte3 = 0;
                memcpy(&byte1, &buffer[w+h*bytesPerLines], sizeof(unsigned char));
                memcpy(&byte2, &buffer[w+1+h*bytesPerLines], sizeof(unsigned char));
                memcpy(&byte3, &buffer[w+2+h*bytesPerLines], sizeof(unsigned char));

                raw12_pixblock block;
                memcpy(&block, &buffer[w+h*bytesPerLines], sizeof(raw12_pixblock));
                *outImageIt++ = block.a >> 4;
                *outImageIt++ = block.b >> 4;
                *outImageIt++ = block.c_hi;
                *outImageIt++ = block.d >> 4;
                *outImageIt++ = block.e >> 4;
                *outImageIt++ = block.f_hi<<4 | block.f_lo >> 4;
                *outImageIt++ = block.g >> 4;
                *outImageIt++ = block.h >> 4;
            }
        }

        FILE *fileOut = fopen("redplane.raw", "wb");
        if(fileOut) { 
            fwrite(outImage, outSize, sizeof(OutType), fileOut);    
            fclose(fileOut);
        }

        free(outImage);
        free(buffer);
        fclose(pFile);
    }
}
