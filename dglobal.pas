unit dGlobal;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  TMetaDataKind = (mdkExif, mdkIPTC, mdkComment);
  TMetaDataKinds = set of TMetaDataKind;

const
  mdkAll = [mdkExif, mdkIPTC, mdkComment];

type
{$IFNDEF FPC}
  DWord = Cardinal;
  PDWord = ^DWord;
  PtrInt = NativeInt;

  Int32 = LongInt;
{$ENDIF}

  TBytes = array of byte;    // Needed for Delphi 7 and old Lazarus (1.0)

  TTagID = word;

  // These function variables can be overridden to alter the default formatting
  // for various data types
  TFmtInt  = function(inInt: Integer): String;
  TFmtReal = function(inReal: Double): String;
  TFmtFrac = function(inNum, inDenom: Integer): String;

  TStrFunc = function(s: String): String;

  TTagEntry = record
    TID: integer;        // TagTableID - EXIF use
    TType: word;         // tag type  -- see FMT_XXXX constants
//    ICode: Word;         // iptc code
    Tag: Word;           // primary key
    ParentID: Word;      // Tag ID of the parent folder
    Count: Word;     // count of TType elements
    Name: String;        // searchable
    Desc: String;        // translatable
    Code: String;        // decode capability
    Data: String;            // display value
    Raw: ansistring;         // unprocessed value  -- DO NOT CHANGE TO STRING !!!
    FormatS: string;     // Format string
    Size: integer;       // used by ITPC module
    CallBack: TStrFunc;  // formatting string
//    id: word;            // msta - used for exif-parent-child-structure
//    parentID: word;      // msta - used for exif-parent-child-structure
  end;
  PTagEntry = ^TTagEntry; // msta

  TTagArray = array of TTagEntry;

  TTagType = (ttExif, ttGPS, ttThumb);
  TTagTypes = set of TTagType;

  TTiffHeader = packed record
    BOM: Array[0..1] of AnsiChar;   // 'II' for little endian, 'MM' for big endian
    Signature: Word;     // Signature (42)
    IFDOffset: DWORD;    // Offset where image data begin, from begin of TIFF header
  end;

  TIFDRecord = packed record
    TagID: Word;
    DataType: Word;
    DataCount: DWord;
    DataValue: DWord;
  end;
  { A note on DataCount, from the EXIF specification:
    "Count
    The number of values. It should be noted carefully that the count is not the
    sum of the bytes. In the case of one value of SHORT (16 bits), for example,
    the count is '1' even though it is 2 Bytes." }

  TExifRational = record
    Numerator, Denominator: LongInt;
  end;
  PExifRational = ^TExifRational;

  TJFIFSegment = packed record
    //Marker: Array[0..1] of byte;      // $FF$E0
    Length: Word;                     // Length of segment without marker
    Identifier: packed array[0..4] of AnsiChar;    // 'JFIF'#0
    JFIFVersion: packed array[0..1] of byte;       // 01 02
    DensityUnits: byte;              // 0: aspect ration, 1: inches, 2: cm
    XDensity: Word;
    YDensity: Word;
    ThumbnailWidth: Byte;            // Pixelcount of thumbnail width
    ThumbnailHeight: Byte;           // and height
  end;
  PJFIFSegment = ^TJFIFSegment;

  TGpsCoordType = (ctLatitude, ctLongitude);

  TGpsFormat = (gf_DD, gf_DM, gf_DMS, gf_DD_Short, gf_DM_Short, gf_DMS_Short);

  TSection = record
    Data: ansistring;
    DType: integer;
    Size: longint;
    Base: longint;
  end;
  PSection = ^TSection;

const
  dExifVersion   = '1.04';

  //--------------------------------------------------------------------------
  // JPEG markers consist of one or more= $FF bytes, followed by a marker
  // code byte (which is not an FF).  Here are the marker codes of interest
  // in this program.
  //--------------------------------------------------------------------------
  M_SOF0         = $C0;         // Start Of Frame N
  M_SOF1         = $C1;         // N indicates which compression process
  M_SOF2         = $C2;         // Only SOF0-SOF2 are now in common use
  M_SOF3         = $C3;
  M_DHT          = $C4;         // Define Huffman Table
  M_SOF5         = $C5;         // NB: codes C4 and CC are NOT SOF markers
  M_SOF6         = $C6;
  M_SOF7         = $C7;
  M_SOF9         = $C9;
  M_SOF10        = $CA;
  M_SOF11        = $CB;
  M_DAC          = $CC;         // Define arithmetic coding conditioning
  M_SOF13        = $CD;
  M_SOF14        = $CE;
  M_SOF15        = $CF;
  M_SOI          = $D8;         // Start Of Image (beginning of datastream)
  M_EOI          = $D9;         // End Of Image (end of datastream)
  M_SOS          = $DA;         // Start Of Scan (begins compressed data)
  M_DQT          = $DB;         // Define Quantization table
  M_DNL          = $DC;         // Define number of lines
  M_DRI          = $DD;         // Restart interoperability definition
  M_DHP          = $DE;         // Define hierarchical progression
  M_EXP          = $DF;         // Expand reference component
  M_JFIF         = $E0;         // Jfif marker                             224
  M_EXIF         = $E1;         // Exif marker                             225
  M_EXIFEXT      = $E2;         // Exif extended marker                    225
  //  M_KODAK    = $E3;         // Kodak marker  ???
  M_IPTC         = $ED;         // IPTC - Photoshop                        237
  M_APP14        = $EE;         // Photoshop data:  App14
  M_COM          = $FE;         // Comment                                 254

  // Format of data in an IFD record
  FMT_BYTE       =  1;
  FMT_STRING     =  2;
  FMT_USHORT     =  3;
  FMT_ULONG      =  4;
  FMT_URATIONAL  =  5;
  FMT_SBYTE      =  6;
  FMT_UNDEFINED  =  7;          // better: rename to FMT_BINARY
  FMT_BINARY     =  7;
  FMT_SSHORT     =  8;
  FMT_SLONG      =  9;
  FMT_SRATIONAL  = 10;
  FMT_SINGLE     = 11;
  FMT_DOUBLE     = 12;

  NUM_FORMATS    = 12;

  BYTES_PER_FORMAT: array[0..NUM_FORMATS] of integer = (
    0,   // dummy for dEXIF --- to be removed
    1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8
  );

  ISO_DATETIME_FORMAT  = 'yyyy-mm-dd hh:nn:ss';
  EXIF_DATETIME_FORMAT = 'yyyy:mm:dd hh:nn:ss';

  EmptyEntry: TTagEntry = (TID:0; TType:0; Tag:0; ParentID:0; Count:1;
    Name:''; Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:nil);

  GpsFormat = gf_DMS_Short;

  ValidExifHeader: ansistring = 'Exif'#0;

  DEFAULT_THUMBNAIL_SIZE = 200;

  NO_ERROR = '<none>';

  {$IFDEF FPC}
  crlf = LineEnding;
  {$ELSE}
  crlf: ansistring = #13#10;
  {$ENDIF}

var
  dExifDataSep     : ansistring = ', ';
  dExifDecodeSep   : string = ',';
  dExifDelim       : string = ' = ';
//  dExifDecode      : boolean = true;
  EstimateValues   : boolean = false;
  TiffReadLimit    : longint = 256000;

  // FormatSettings for how to pass floating point values to dExif
  dExifFmtSettings : TFormatSettings;

implementation

initialization
//  dExifFmtSettings := FormatSettings;
  dExifFmtSettings.DecimalSeparator := '.';

end.





