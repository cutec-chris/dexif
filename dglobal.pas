unit dGlobal;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
{$IFNDEF FPC}
  DWord = Cardinal;
  PDWord = ^DWord;

  Int32 = LongInt;
{$ENDIF}

  TBytes = array of byte;    // Needed for Delphi 7 and old Lazarus (1.0)

  TTagID = word;

  // These function variables can be overridden to alter the default formatting
  // for various data types
  TFmtInt  = function(inInt: Integer): String;
  TFmtReal = function(inReal: Double): String;
  TFmtFrac = function(inNum, inDenom: Integer): String;

  TStrFunc = function(s: AnsiString): String;

  TTagEntry = record
    TID: integer;        // TagTableID - EXIF use
    TType: word;         // tag type  -- see FMT_XXXX constants
    ICode: Word;         // iptc code
    Tag: word;           // primary key
    Name: String;        // searchable
    Desc: String;        // translatable
    Code: String;        // decode capability
    Data: String;            // display value
    Raw: ansistring;         // unprocessed value  -- DO NOT CHANGE TO STRING !!!
    PRaw: integer;       // pointer to unprocessed
    FormatS: string;     // Format string
    Size: integer;       // used by ITPC module
    CallBack: TStrFunc;  // formatting string
    id: word;            // msta - used for exif-parent-child-structure
    parentID: word;      // msta - used for exif-parent-child-structure
  end;
  PTagEntry = ^TTagEntry; // msta

  TTagArray = array of TTagEntry;

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
    Numerator, Denominator: Int32;
  end;
  PExifRational = ^TExifRational;

  TGpsFormat = (gf_DD, gf_DM, gf_DMS, gf_DD_Short, gf_DM_Short, gf_DMS_Short);


const
  // Format of data in an IFD record
  NUM_FORMATS   = 12;

  BYTES_PER_FORMAT: array[0..NUM_FORMATS] of integer = (
    0,   // dummy for dEXIF --- to be removed
    1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8
  );
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

  ISODateFormat  = 'yyyy-mm-dd hh:nn:ss';
  EXIFDateFormat = 'yyyy:mm:dd hh:nn:ss';

  EmptyEntry: TTagEntry = (TID:0; TType:0; ICode:0; Tag:0; Name:''; Desc:'';
    Code:''; Data:''; Raw:''; PRaw:0; FormatS:''; Size:0);

  GpsFormat = gf_DMS_Short;

  ValidExifHeader: ansistring = 'Exif'#0;

  {$IFDEF FPC}
  crlf = LineEnding;
  {$ELSE}
  crlf: ansistring = #13#10;
  {$ENDIF}

var
  dExifDataSep   : ansistring = ', ';
  dExifDecodeSep : string = ',';
  dExifDelim     : string = ' = ';
  dExifDecode    : boolean = true;
  EstimateValues : boolean = false;
  TiffReadLimit  : longint = 256000;

implementation

end.

