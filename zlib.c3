module zlib(<Opaque>);

def AllocFunc = fn void*(Opaque* opaque, uint items, uint size);
def FreeFunc = fn void(Opaque* opaque, void* address);

distinct ZLibRet = int;
const ZLibRet Z_OK            = (ZLibRet)0;
const ZLibRet Z_STREAM_END    = (ZLibRet)1;
const ZLibRet Z_NEED_DICT     = (ZLibRet)2;
const ZLibRet Z_ERRNO         = (ZLibRet)-1;
const ZLibRet Z_STREAM_ERROR  = (ZLibRet)-2;
const ZLibRet Z_DATA_ERROR    = (ZLibRet)-3;
const ZLibRet Z_MEM_ERROR     = (ZLibRet)-4;
const ZLibRet Z_BUF_ERROR     = (ZLibRet)-5;
const ZLibRet Z_VERSION_ERROR = (ZLibRet)-6;

fault ZLibFault
{
	Z_ERRNO,
	Z_STREAM_ERROR,
	Z_DATA_ERROR,
	Z_MEM_ERROR,
	Z_BUF_ERROR,
	Z_VERSION_ERROR,
}

macro bool ZLibRet.is_fault(self) => self < 0;

<*
 @require ret.is_fault()
*>
macro ZLibFault from_ret(ZLibRet ret)
{
	switch (ret)
	{
		case Z_ERRNO:         return Z_ERRNO;
		case Z_STREAM_ERROR:  return Z_STREAM_ERROR;
		case Z_DATA_ERROR:    return Z_DATA_ERROR;
		case Z_MEM_ERROR:     return Z_MEM_ERROR;
		case Z_BUF_ERROR:     return Z_BUF_ERROR;
		case Z_VERSION_ERROR: return Z_VERSION_ERROR;
		default:              unreachable();
	}
}

distinct ZFlush = int;
const ZFlush Z_NO_FLUSH      = (ZFlush)0;
const ZFlush Z_PARTIAL_FLUSH = (ZFlush)1;
const ZFlush Z_SYNC_FLUSH    = (ZFlush)2;
const ZFlush Z_FULL_FLUSH    = (ZFlush)3;
const ZFlush Z_FINISH        = (ZFlush)4;
const ZFlush Z_BLOCK         = (ZFlush)5;
const ZFlush Z_TREES         = (ZFlush)6;

distinct ZCompressionLevel = int;
const ZCompressionLevel Z_NO_COMPRESSION      = (ZCompressionLevel)0;
const ZCompressionLevel Z_BEST_SPEED          = (ZCompressionLevel)1;
const ZCompressionLevel Z_BEST_COMPRESSION    = (ZCompressionLevel)9;
const ZCompressionLevel Z_DEFAULT_COMPRESSION = (ZCompressionLevel)-1;

distinct ZCompressionStrategy = int;
const ZCompressionStrategy Z_FILTERED         = (ZCompressionStrategy)1;
const ZCompressionStrategy Z_HUFFMAN_ONLY     = (ZCompressionStrategy)2;
const ZCompressionStrategy Z_RLE              = (ZCompressionStrategy)3;
const ZCompressionStrategy Z_FIXED            = (ZCompressionStrategy)4;
const ZCompressionStrategy Z_DEFAULT_STRATEGY = (ZCompressionStrategy)0;

distinct DeflateDataType = int;
const DeflateDataType Z_BINARY  = (DeflateDataType)0;
const DeflateDataType Z_TEXT    = (DeflateDataType)1;
const DeflateDataType Z_ASCII   = Z_TEXT; /* for compatibility with 1.2.2 and earlier */
const DeflateDataType Z_UNKNOWN = (DeflateDataType)2;

distinct DeflateCompressionMethod = int;
const DeflateCompressionMethod Z_DEFLATED = (DeflateCompressionMethod)8;

const ZLibArgs DEFAULT_ARGS = {};
struct ZLibArgs
{
	ZFlush flush;
	DeflateDataType data_type;

	AllocFunc* zalloc;
	FreeFunc* zfree;
	Opaque* opaque;
}

struct ZLibStream
{
	ZLibArgs args;
	Allocator allocator;
	void* inner;
}

<*
 @require self.inner == null, "Already initialized"
*>
fn void! ZLibStream.new_init(&self, ZLibArgs args = DEFAULT_ARGS, Allocator allocator = allocator::heap())
{
	*self = {args, allocator, allocator::alloc(allocator, ZLibStream_)};

	ZLibRet ret = _inflate_init(self.inner, self.args.flush);
	if (ret.is_fault()) return from_ret(ret)?;

	assert(ret == Z_OK);
}

<*
 @require self.inner != null, "Not initialized"
*>
fn char[]! ZLibStream.inflate_all(self, char[] data, ZLibRet* p_ret = null)
{
	ZLibStream_* inner = (ZLibStream_*)self.inner;
	inner.next_in = data.ptr;
	inner.avail_in = data.len;
	inner.total_in = 0;

	ulong last_out = 0;

	@pool()
	{
		DString tmp = dstring::temp_new();

		ZLibRet ret = Z_OK;
		while (ret == Z_OK)
		{
			char[1024] buf;

			inner.next_out = &buf[0];
			inner.avail_out = buf.len;

			ret = _inflate(inner, self.args.flush);

			ulong diff_out = inner.total_out - last_out;
			last_out = inner.total_out;

			tmp.append(buf[0:diff_out]);
		}

		if (p_ret != null) *p_ret = ret;

		if (ret.is_fault()) return from_ret(ret)?;

		assert(ret == Z_STREAM_END);
		return tmp.copy_str(self.allocator);
	};
}

<*
 @require self.inner != null, "Not initialized"
*>
fn void ZLibStream.free(self)
{
	ZLibRet ret = _inflate_end(self.inner);
	assert(ret == Z_OK);

	allocator::free(self.allocator, self.inner);
}

module zlib(<Opaque>) @private;

distinct InternalState = void*;

struct ZLibStream_
{
	char* next_in;             /* next input byte */
	uint avail_in;             /* number of bytes available at next_in */
	ulong total_in;            /* total number of input bytes read so far */

	char* next_out;            /* next output byte will go here */
	uint avail_out;            /* remaining free space at next_out */
	ulong total_out;           /* total number of bytes output so far */

	char* msg;                 /* last error message, NULL if no error */
	InternalState* state;      /* not visible by applications */

	AllocFunc* zalloc;         /* used to allocate the internal state */
	FreeFunc* zfree;           /* used to free the internal state */
	Opaque* opaque;            /* private data object passed to zalloc and zfree */

	DeflateDataType data_type; /* best guess about the data type: binary or text
	                              for deflate, or the decoding state for inflate */
	ulong adler;               /* Adler-32 or CRC-32 value of the uncompressed data */
	ulong reserved;            /* reserved for future use */
}

extern fn ZLibRet _inflate_init(ZLibStream_* strm, ZFlush flush) @extern("inflateInit");
extern fn ZLibRet _inflate(ZLibStream_* strm, ZFlush flush) @extern("inflate");
extern fn ZLibRet _inflate_end(ZLibStream_* strm) @extern("inflateEnd");
