
typedef unsigned char stbi_uc;

#ifdef __cplusplus
extern "C" {
#endif

extern stbi_uc *stbi_load_from_memory(stbi_uc *buffer, int len, int *x, int *y, int *comp, int req_comp);

#ifndef STBI_NO_HDR
extern float *stbi_loadf_from_memory(stbi_uc *buffer, int len, int *x, int *y, int *comp, int req_comp);
extern void   stbi_hdr_to_ldr_gamma(float gamma);
extern void   stbi_hdr_to_ldr_scale(float scale);
extern void   stbi_ldr_to_hdr_gamma(float gamma);
extern void   stbi_ldr_to_hdr_scale(float scale);
#endif // STBI_NO_HDR

extern char    *stbi_failure_reason  (void);

// free the loaded image -- this is just free()
extern void     stbi_image_free      (void *retval_from_stbi_load);

// get image dimensions & components without fully decoding
extern int      stbi_info_from_memory(stbi_uc *buffer, int len, int *x, int *y, int *comp);
extern int      stbi_is_hdr_from_memory(stbi_uc *buffer, int len);

// format tests
extern int      stbi_jpeg_test_memory     (stbi_uc *buffer, int len);
extern int      stbi_png_test_memory      (stbi_uc *buffer, int len);
extern int      stbi_bmp_test_memory      (stbi_uc *buffer, int len);
extern int      stbi_tga_test_memory      (stbi_uc *buffer, int len);
extern int      stbi_psd_test_memory      (stbi_uc *buffer, int len);
extern int      stbi_hdr_test_memory      (stbi_uc *buffer, int len);

#if !defined(STBI_NO_WRITE) && !defined(STBI_NO_STDIO)
// write a BMP/TGA file given tightly packed 'comp' channels (no padding, nor bmp-stride-padding)
// (you must include the appropriate extension in the filename).
// returns TRUE on success, FALSE if couldn't open file, error writing file
extern int      stbi_write_bmp       (char *filename,           int x, int y, int comp, void *data);
extern int      stbi_write_tga       (char *filename,           int x, int y, int comp, void *data);
#endif

// ZLIB client - used by PNG, available for other purposes

extern char *stbi_zlib_decode_malloc_guesssize(int initial_size, int *outlen);
extern char *stbi_zlib_decode_malloc(char *buffer, int len, int *outlen);
extern int   stbi_zlib_decode_buffer(char *obuffer, int olen, char *ibuffer, int ilen);

extern char *stbi_zlib_decode_noheader_malloc(char *buffer, int len, int *outlen);
extern int   stbi_zlib_decode_noheader_buffer(char *obuffer, int olen, char *ibuffer, int ilen);
  
#ifdef __cplusplus
}
#endif
