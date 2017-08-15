unit dGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


type
  TTagID = word;

  TStrFunc = function(s: AnsiString): AnsiString;

  TTagEntry = record
    TID: integer;        // TagTableID - EXIF use
    TType: word;         // tag type
    ICode: Word;         // iptc code
    Tag: word;           // primary key
    Name:ansistring;        // searchable
    Desc:ansistring;        // translatable
    Code:ansistring;        // decode capability
    Data:ansistring;        // display value
    Raw:ansistring;         // unprocessed value
    PRaw: integer;       // pointer to unprocessed
    FormatS:ansistring;      // Format string
    Size: integer;       // used by ITPC module
    CallBack: TStrFunc;  // formatting string
    id : word;           // msta - used for exif-parent-child-structure
    parentID : word;     // msta - used for exif-parent-child-structure
  end;
  PTagEntry = ^TTagEntry; // msta

  TTiffHeader = packed record
    BOM: Array[0..1] of AnsiChar;   // 'II' for little endian, 'MM' for big endian
    Signature: Word;     // Signature (42)
    IFDOffset: DWORD;    // Offset where image data begin, from begin of TIFF header
  end;

  TIFDRecord = packed record
    TagID: Word;
    DataType: Word;
    DataSize: DWord;
    DataValue: DWord;
  end;

  TExifRational = record
    Numerator, Denominator: LongInt;
  end;
  PExifRational = ^TExifRational;

const
  // Format of data in an IFD record
  NUM_FORMATS   = 12;

  BYTES_PER_FORMAT: array[0..NUM_FORMATS] of integer = (
    0,   // dummy for dEXIF --- to be removed
    1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8
  );
  FMT_BYTE      =  1;
  FMT_STRING    =  2;
  FMT_USHORT    =  3;
  FMT_ULONG     =  4;
  FMT_URATIONAL =  5;
  FMT_SBYTE     =  6;
  FMT_UNDEFINED =  7;          // better: rename to FMT_BINARY
  FMT_BINARY    =  7;
  FMT_SSHORT    =  8;
  FMT_SLONG     =  9;
  FMT_SRATIONAL = 10;
  FMT_SINGLE    = 11;
  FMT_DOUBLE    = 12;


implementation

end.

