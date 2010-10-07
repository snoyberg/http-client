#include <zlib.h>
#include <stdlib.h>

z_stream * create_z_stream2 (void)
{
	z_stream *ret = malloc(sizeof(z_stream));
	ret->zalloc = Z_NULL;
	ret->zfree = Z_NULL;
	ret->opaque = Z_NULL;
	ret->next_in = NULL;
	ret->avail_in = 0;

	if (inflateInit2(ret, 31) != Z_OK)
	    return NULL;
	else
	    return ret;
}

void free_z_stream2 (z_stream *stream)
{
	inflateEnd(stream);
	free(stream);
}

void set_avail_in2 (z_stream *stream, char *buff, unsigned int avail)
{
	stream->next_in = buff;
	stream->avail_in = avail;
}

void set_avail_out2 (z_stream *stream, char *buff, unsigned int avail)
{
	stream->next_out = buff;
	stream->avail_out = avail;
}

int call_inflate_noflush2 (z_stream *stream)
{
	return inflate(stream, Z_NO_FLUSH);
}

unsigned int get_avail_in2 (z_stream *stream)
{
	return stream->avail_in;
}

unsigned int get_avail_out2 (z_stream *stream)
{
	return stream->avail_out;
}
