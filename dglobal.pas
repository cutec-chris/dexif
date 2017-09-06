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
    ICode: Word;         // iptc code
    Tag: Word;           // primary key
    ParentID: Word;      // Tag ID of the parent folder
    Count: Word;         // count of TType elements
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


const
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

  EmptyEntry: TTagEntry = (TID:0; TType:0; ICode:0; Tag:0; ParentID:0; Count:1;
    Name:''; Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:nil);

  GpsFormat = gf_DMS_Short;

  ValidExifHeader: ansistring = 'Exif'#0;

  DEFAULT_THUMBNAIL_SIZE = 200;

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

