unit dGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  // Tags with offsets to subdirectories (subIFD)
  TAG_EXIF_OFFSET        = $8769;
  TAG_GPS_OFFSET         = $8825;
  TAG_INTEROP_OFFSET     = $A005;
  TAG_SUBIFD_OFFSET      = $014A;
  // the following tag ids with more offset are taken from
  // https://sno.phy.queensu.ca/~phil/exiftool/TagNames/EXIF.html
  TAG_GLOBALPARAMS_OFFSET= $0190;
  TAG_KODAK_OFFSET       = $8290;
  // there are some more...

  TAG_IMAGEWIDTH         = $0100;
  TAG_IMAGELENGTH        = $0101;
  TAG_THUMBTYPE          = $0103;
  TAG_IMAGEDESCRIPTION   = $010E;
  TAG_MAKE               = $010F;
  TAG_MODEL              = $0110;
  TAG_DATETIME_MODIFY    = $0132;
  TAG_ARTIST             = $013B;

  TAG_EXPOSURETIME       = $829A;
  TAG_FNUMBER            = $829D;

  TAG_EXIFVERSION        = $9000;
  TAG_DATETIME_ORIGINAL  = $9003;
  TAG_DATETIME_DIGITIZED = $9004;
  TAG_SHUTTERSPEED       = $9201;
  TAG_APERTURE           = $9202;
  TAG_MAXAPERTUREVALUE   = $9205;
  TAG_SUBJECT_DISTANCE   = $9206;
  TAG_LIGHT_SOURCE       = $9208;
  TAG_FLASH              = $9209;
  TAG_FOCALLENGTH        = $920A;
  TAG_MAKERNOTE          = $927C;
  TAG_USERCOMMENT        = $9286;

  TAG_EXIF_IMAGEWIDTH    = $A002;
  TAG_EXIF_IMAGELENGTH   = $A003;
  TAG_FOCALPLANEXRES     = $A20E;
  TAG_FOCALPLANEYRES     = $A20F;
  TAG_FOCALPLANEUNITS    = $A210;
  TAG_FOCALLENGTH35MM    = $A405;

  // Format descriptor
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

type
  TTagID = word;

type
  TStrFunc = function (instr:ansistring):ansistring;

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

  TExifBinaryData = array of byte;

  TExifRational = record
    Numerator, Denominator: LongInt;
  end;
  PExifRational = ^TExifRational;


implementation

end.

