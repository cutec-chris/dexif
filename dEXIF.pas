unit dEXIF;

////////////////////////////////////////////////////////////////////////////////
// unit dEXIF - Copyright 2001-2006, Gerry McGuire
//--------------------------------------------------------------------------
// Program to pull the information out of various types of EXIF digital
// camera files and show it in a reasonably consistent way
//
// This module parses the very complicated exif structures.
//
// Matthias Wandel,  Dec 1999 - August 2000  (most of the comments)
//
// Translated to Delphi:
//         Gerry McGuire, March - April 2001 - Currently - read only
//                        May 2001 - add EXIF to jpeg output files
//                        September 2001 - read TIF files, IPTC data
//                        June 2003 - First (non-beta) Release
//--------------------------------------------------------------------------
//   In addition to the basic information provided by Matthias, the
//   following web page contains reference informtion regarding the
//   exif standard: http://www.pima.net/standards/iso/tc42/wg18/WG18_POW.htm
//   (the documents themselves are PDF).
//--------------------------------------------------------------------------
//  17.05.2002 MS Corrections/additions M. Schwaiger
//--------------------------------------------------------------------------

{$IFDEF FPC}
 {$MODE Delphi}
{$ENDIF}

{$I dExif.inc}

interface

uses
  SysUtils, Classes, Math, Variants,
 {$IFDEF FPC}
  LazUTF8,
 {$ELSE}
  {$IFNDEF dExifNoJpeg} jpeg, {$ENDIF}
 {$ENDIF}
  dGlobal, dUtils, dTags, dIPTC;

const
  {
  ExifTag = 1;  // default tag Types
  GpsTag = 2;
  ThumbTag = 4;
  }

  GenericEXIF = 0;
  CustomEXIF = 1;
  AllEXIF = -1;
  GenNone = 0;
  GenAll = 255;
  GenString = 2;
  GenList = 4;
  VLMin = 0;
  VLMax = 1;

type
  TImgData = class;

  { TBasicMetadataWriter }

  TBasicMetadataWriter = class
  protected
    FImgData: TImgData;
    FErrLog: TStrings;
  public
    constructor Create(AImgData: TImgData); virtual;
    destructor Destroy; override;
    procedure LogError(const AMsg: String);
    procedure WriteToStream(AStream: TStream); virtual;
  end;

  { TEndInd }

  TEndInd = class
  private
    FData: ansistring;
  public
    MotorolaOrder: boolean;
    function Get16u(AOffs: integer): word;
    function Get32s(AOffs: integer): Longint;
    function Get32u(AOffs: integer): Longword;
    function Put32s(data: Integer): AnsiString;
    procedure WriteInt16(var buff: AnsiString; int,posn: integer);
    procedure WriteInt32(var buff: AnsiString; int,posn: longint);
    function GetDataBuff: Ansistring;
    procedure SetDataBuff(const Value: AnsiString);
    property DataBuff: AnsiString read GetDataBuff write SetDataBuff;
  end;

  { TImageInfo }

  TImageInfo = class(tEndInd)
  private
    FITagArray: array of TTagEntry;
    FITagCount: integer;

    FIThumbArray: array of TTagEntry;
    FIThumbCount: integer;

    FThumbStart: integer;
    FThumbLength: integer;
    FThumbType: integer;

    FThumbnailBuffer: TBytes;
    FThumbnailStartOffset: Integer;
    FThumbnailSize: Integer;

    iterator: integer;
    iterThumb: integer;

      // Getter / setter
    function GetDateTimeOriginal: TDateTime;
    procedure SetDateTimeOriginal(const AValue: TDateTime);

    function GetDateTimeDigitized: TDateTime;
    procedure SetDateTimeDigitized(const AValue: TDateTime);

    function GetDateTimeModified: TDateTime;
    procedure SetDateTimeModified(const AValue: TDateTime);

    function GetArtist: String;
    procedure SetArtist(v: String);

    function GetExifComment: String;
    procedure SetExifComment(AValue: String);

    function GetImageDescription: String;
    procedure SetImageDescription(const AValue: String);

    function GetCameraMake: String;
    procedure SetCameraMake(const AValue: String);

    function GetCameraModel: String;
    procedure SetCameraModel(const AValue: String);

    function GetCopyright: String;
    procedure SetCopyright(const AValue: String);

    function GetGPSCoordinate(ATagName: String;
      ACoordType: TGpsCoordType): Extended;
    procedure SetGPSCoordinate(ATagName: String; const AValue: Extended;
      ACoordType: TGpsCoordType);
    function GetGPSLatitude: Extended;
    procedure SetGPSLatitude(const AValue: Extended);
    function GetGPSLongitude: Extended;
    procedure SetGPSLongitude(const AValue: Extended);

    function GetHeight: Integer;
    procedure Setheight(AValue: Integer);
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);

    function GetTagByID(ATagID: Word): TTagEntry;
    procedure SetTagByID(ATagID: Word; const AValue: TTagEntry);
    function GetTagByIndex(AIndex: Integer): TTagEntry;
    procedure SetTagByIndex(AIndex: Integer; const AValue: TTagEntry);
    function GetTagByName(ATagName: String): TTagEntry;
    procedure SetTagByName(ATagName: String; const AValue: TTagEntry);
    function GetTagValue(ATagName: String): variant;
    procedure SetTagValue(ATagName: String; AValue: variant);
    function GetTagValueAsString(ATagName: String): String;
    procedure SetTagValueAsString(ATagName: String; AValue: String);

    function GetThumbTagByID(ATagID: Word): TTagEntry;
    procedure SetThumbTagByID(ATagID: Word; const AValue: TTagEntry);
    function GetThumbTagByIndex(AIndex: Integer): TTagEntry;
    procedure SetThumbTagByIndex(AIndex: Integer; const AValue: TTagEntry);
    function GetThumbTagByName(ATagName: String): TTagEntry;
    procedure SetThumbTagByName(ATagName: String; const AValue: TTagEntry);
    function GetThumbTagValue(ATagName: String): Variant;
    procedure SetThumbTagValue(ATagName: String; AValue: variant);

    procedure InternalGetBinaryTagValue(ATag: TTagEntry; var ABuffer: ansistring);
    function InternalGetTagValue(ATag: TTagEntry): Variant;
    procedure InternalSetTagValue(ATagName: String; AValue: Variant;
      ATagTypes: TTagTypes; ABinaryData: Pointer = nil; ABinaryDataCount: Word = 0);
    function NumericTagToVar(ABuffer: Pointer; ATagType: Integer): Variant;
    procedure VarToNumericTag(AValue:variant; ATag: PTagEntry);

    // misc
    function CreateTagPtr(const ATagDef: TTagEntry; IsThumbTag: Boolean; AParentID: Word = 0): PTagEntry;
    function FindTagPtr(const ATagDef: TTagEntry; IsThumbTag: Boolean): PTagEntry;

    (*
    function GetTagPtr(ATagTypes: TTagTypes; ATagID: Word; AForceCreate: Boolean=false;
      AParentID: word=0; ATagType: word=65535): PTagEntry;
      *)
    procedure RemoveTag(ATagTypes: TTagTypes; ATagID: Word; AParentID: word=0);

    procedure ClearDirStack;
    procedure PushDirStack(dirStart, offsetbase: Integer);
    function TestDirStack(dirStart, offsetbase: Integer): boolean;

  protected
    function AddTagToArray(ANewTag: iTag): integer;
    function AddTagToThumbArray(ANewTag: iTag): integer;
    function CvtInt(ABuffer: Ansistring): Longint;
    function ExifDateToDateTime(ARawStr: ansistring): TDateTime;
    procedure ExtractThumbnail;
    function FormatNumber(ABuffer: ansistring; AFmt: integer; AFmtStr: string;
      ADecodeStr: string=''): String;
    function GetNumber(ABuffer: ansistring; AFmt: integer): double;
    procedure ProcessExifDir(DirStart, OffsetBase, ExifLength: LongInt;
      ATagType: TTagType = ttExif; APrefix: string=''; AParentID: word=0);

  public
    MaxTag: integer;
    Parent: timgdata;
    ExifVersion : string[6];
//    Height, Width, HPosn, WPosn: integer;
    FlashUsed: integer;
    BuildList: integer;
    MakerNote: ansistring;
    TiffFmt: boolean;
// Add support for thumbnail
    ThumbTrace: ansistring;
    MaxThumbTag: integer;
//  Added the following elements to make the structure a little more code-friendly
    TraceLevel: integer;
    TraceStr: ansistring;
    msTraceStr: ansistring;
    msAvailable: boolean;
    msName:ansistring;
    MakerOffset : integer;

    procedure ProcessHWSpecific(MakerBuff:ansistring;
                  TagTbl: array of TTagEntry;
                  DirStart:longint;
                  aMakerOffset:Longint;
                  spOffset:integer = 0);

    procedure AddMSTag(fname: String; ARawStr: ansistring; fType: word);
    procedure AdjExifSize(AHeight, AWidth: Integer);

    procedure ResetIterator;
    function IterateFoundTags(TagId:integer; var retVal:TTagEntry):boolean;
    function IterateFoundThumbTags(TagId: integer;
      var retVal: TTagEntry): boolean;
    procedure ResetThumbIterator;

    procedure Calc35Equiv;
    function LookupRatio: double;

  public
    constructor Create(p: TImgData; BuildCode: integer = GenAll);
    procedure Assign(source: TImageInfo);
    destructor Destroy; override;

    // Date/time routines
    procedure AdjDateTime(ADays, AHours, AMins, ASecs: integer);
    function  GetImgDateTime: TDateTime;

    // Thumbnail
    procedure CreateThumbnail(AThumbnailSize: Integer = DEFAULT_THUMBNAIL_SIZE);
    function HasThumbnail: boolean;
    procedure ProcessThumbnail;
    procedure RemoveThumbnail;
    procedure LoadThumbnailFromStream(AStream: TStream);
    procedure SaveThumbnailToStream(AStream: TStream);
    property ThumbnailBuffer: TBytes read FThumbnailBuffer;
    property ThumbTagValue[ATagName: String]: variant
        read GetThumbTagValue write SetThumbTagValue;

    // Collective output
    procedure EXIFArrayToXML(AList: TStrings); overload;
    function EXIFArrayToXML: TStringList; overload;
      deprecated {$IFDEF FPC}'Use procedure instead.'{$ENDIF};
    function ToShortString: String;   //  Summarizes in a single line
    function ToLongString(ALabelWidth: Integer = 15): String;

    // Looking up tags and tag values
    function GetRawFloat(ATagName: String): double;
    function GetRawInt(ATagName: String): integer;
    function GetTagByDesc(SearchStr: String): TTagEntry;
    function LookupTag(ATagName: String): integer; virtual;
    function LookupTagVal(ATagName: String): String; virtual;
    function LookupTagDefn(ATagName: String): integer;
    function LookupTagByDesc(ADesc: String): integer;
    function LookupTagInt(ATagName: String): integer;

    property TagValue[ATagName: String]: Variant
        read GetTagValue write SetTagValue; default;
    property TagValueAsString[ATagName: String]: String
        read GetTagValueAsString write SetTagValueAsString;

    property TagByID[ATagID: Word]: TTagEntry
        read GetTagByID write SetTagByID;
    property TagByIndex[AIndex: Integer]: TTagEntry
        read GetTagByIndex write SetTagByIndex;
    property TagByName[ATagName: String]: TTagEntry
        read GetTagByName write SetTagByName;
    property TagCount: Integer
        read fiTagCount;

    property Artist: String
        read GetArtist write SetArtist;
    property CameraMake: String
        read GetCameraMake write SetCameraMake;
    property CameraModel: String
        read GetCameraModel write SetCameraModel;
    property Copyright: String
        read GetCopyright write SetCopyright;
    property DateTimeOriginal: TDateTime
        read GetDateTimeOriginal write SetDateTimeOriginal;
    property DateTimeDigitized: TDateTime
        read GetDateTimeDigitized write SetDateTimeDigitized;
    property DateTimeModified: TDateTime
        read GetDateTimeModified write SetDateTimeModified;
    property ExifComment: String
        read GetExifComment write SetExifComment;
    property GPSLatitude: Extended
        read GetGPSLatitude write SetGPSLatitude;
    property GPSLongitude: Extended
        read GetGPSLongitude write SetGPSLongitude;
    property ImageDescription: String
        read GetImageDescription write SetImageDescription;
    property Height: Integer
        read GetHeight write SetHeight;
    property Width: Integer
        read GetWidth write SetWidth;

    property ThumbTagByIndex[AIndex: Integer]: TTagEntry
        read GetThumbTagByIndex write SetThumbTagByIndex;
    property ThumbTagCount: Integer
        read fiThumbCount;
  end; // TInfoData

  TSection = record
    Data: ansistring;
    DType: integer;
    Size: longint;
    Base: longint;
  end;
  PSection = ^TSection;


  { TImgData }

  TImgData = class(TEndInd) // One per image object
  private
    FFilename: string;
    FFileDateTime: TDateTime;
    FFileSize: Int64;
    FHeight: Integer;
    FWidth: Integer;
    FErrStr: String;
    FComment: String;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetResolutionUnit: String;
    function GetXResolution: Integer;
    function GetYResolution: Integer;
//    function GetComment: String;
    procedure SetComment(const AValue: String);
    procedure SetFileInfo(const AFilename: string);
    procedure SetHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);

  protected
    ExifSegment: pSection;
    HeaderSegment: pSection;
    function ExtractThumbnailBuffer: TBytes;
//    function GetCommentSegment: ansistring;
//    function GetCommentStr: ansistring;
//    procedure MakeCommentSegment(ABuffer: AnsiString);
    procedure MergeToStream(AInputStream, AOutputStream: TStream;
      AWriteMetadata: TMetadataKinds = mdkAll);
//    procedure MergeToStream(AInputStream, AOutputStream: TStream;
//      AEnabledMeta: Byte = $FF; AFreshExifBlock: Boolean = false);
    procedure ProcessEXIF;
    function ReadJpegSections(AStream: TStream):boolean;
    function ReadTiffSections(AStream: TStream):boolean;
    function SaveExif(AStream: TStream; AWriteMetadata: TMetadataKinds = mdkAll): LongInt;

  public
    Sections: array[1..21] of TSection;
    TiffFmt: boolean;
    BuildList: integer;
    SectionCnt : integer;
    IPTCSegment: pSection;
    ExifObj: TImageInfo;
    IptcObj: TIPTCData;
    TraceLevel: integer;

    procedure Reset;
    procedure MakeIPTCSegment(buff:ansistring);
    procedure ClearSections;
    procedure ClearEXIF;
    procedure ClearIPTC;
    procedure ClearComments;

    function FillInIptc: boolean;

  public
    constructor Create(buildCode: integer = GenAll);
    destructor Destroy; override;

    // Manually create empty EXIF and IPTC structures
    function CreateExifObj: TImageInfo;
    function CreateIPTCObj: TIPTCData;

    // Reading
    function ProcessFile(const AFileName: String): boolean;
    function ReadJpegFile(const AFileName: string): boolean;
    function ReadTiffFile(const AFileName: string): boolean;
    function ReadExifInfo(AFilename: String): boolean;
    procedure ReadIPTCStrings(const AFilename: String; AList: TStrings); overload;
    function ReadIPTCStrings(const AFilename: String): TStringList; overload;
      deprecated {$IFDEF FPC} 'Use procedure instead' {$ENDIF};

    // Thumbnail
    function ExtractThumbnailJpeg(AStream: TStream): Boolean; overload;
    {$IFNDEF dExifNoJpeg}
    function ExtractThumbnailJpeg: TJpegImage; overload;
    {$ENDIF}

    // Status
    function HasMetaData:boolean;
    function HasEXIF: boolean;
    function HasIPTC: boolean;
    function HasComment: boolean;
    function HasThumbnail: boolean;
    property ErrStr: String read FErrStr;

    // Collective output
    procedure MetaDataToXML(AList: TStrings); overload;
    function MetaDataToXML: TStringList; overload;
      deprecated {$IFDEF FPC} 'Use procedure instead' {$ENDIF};

    // Writing
    procedure WriteEXIFJpeg(AJpeg: TStream; AFileName: String; AdjSize: Boolean = true); overload;
    procedure WriteEXIFJpeg(AFileName, AOrigName: String; AdjSize: Boolean = true); overload;
    procedure WriteEXIFJpeg(AFileName: String; AdjSize: Boolean = true); overload;
    procedure WriteEXIFJpegTo(AFileName: String);
   {$IFNDEF dExifNoJpeg}
    procedure WriteEXIFJpeg(j:TJpegImage; fname, origName: String;
      AdjSize: boolean = true);  overload;
    procedure WriteEXIFJpeg(fname: String); overload;
    procedure WriteEXIFJpeg(j:tjpegimage; fname:String; adjSize:boolean = true);  overload;
   {$ENDIF}

    // Basic properties
    property FileName: String read FFilename;
    property FileDatetime: TDateTime read FFileDateTime;
    property FileSize: Int64 read FFileSize;
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
    property XResolution: Integer read GetXResolution;
    property YResolution: Integer read GetYResolution;
    property ResolutionUnit: String read GetResolutionUnit;
    property Comment: String read FComment write SetComment;  // Comment from COM segment
  end; // TImgData

const
//--------------------------------------------------------------------------
// JPEG markers consist of one or more= $FF bytes, followed by a marker
// code byte (which is not an FF).  Here are the marker codes of interest
// in this program.
//--------------------------------------------------------------------------

     M_SOF0 = $C0;            // Start Of Frame N
     M_SOF1 = $C1;            // N indicates which compression process
     M_SOF2 = $C2;            // Only SOF0-SOF2 are now in common use
     M_SOF3 = $C3;
     M_DHT  = $C4;            // Define Huffman Table
     M_SOF5 = $C5;            // NB: codes C4 and CC are NOT SOF markers
     M_SOF6 = $C6;
     M_SOF7 = $C7;
     M_SOF9 = $C9;
     M_SOF10= $CA;
     M_SOF11= $CB;
     M_DAC  = $CC;            // Define arithmetic coding conditioning
     M_SOF13= $CD;
     M_SOF14= $CE;
     M_SOF15= $CF;
     M_SOI  = $D8;            // Start Of Image (beginning of datastream)
     M_EOI  = $D9;            // End Of Image (end of datastream)
     M_SOS  = $DA;            // Start Of Scan (begins compressed data)
     M_DQT  = $DB;            // Define Quantization table
     M_DNL  = $DC;            // Define number of lines
     M_DRI  = $DD;            // Restart interoperability definition
     M_DHP  = $DE;            // Define hierarchical progression
     M_EXP  = $DF;            // Expand reference component
     M_JFIF = $E0;            // Jfif marker                             224
     M_EXIF = $E1;            // Exif marker                             225
  M_EXIFEXT = $E2;            // Exif extended marker                    225
     //  M_KODAK = $E3;           // Kodak marker  ???
     M_IPTC = $ED;            // IPTC - Photoshop                        237
    M_APP14 = $EE;            // Photoshop data:  App14
     M_COM  = $FE;            // Comment                                 254

    ProcessTable : array [0..29] of TTagEntry =
    (( TID:0;TType:0;ICode: 0;Tag: M_SOF0;   Name:'SKIP';Desc: 'Baseline'),
     ( TID:0;TType:0;ICode: 0;Tag: M_SOF1;   Name:'';Desc: 'Extended sequential'),
     ( TID:0;TType:0;ICode: 0;Tag: M_SOF2;   Name:'';Desc: 'Progressive'),
     ( TID:0;TType:0;ICode: 0;Tag: M_SOF3;   Name:'';Desc: 'Lossless'),
     ( TID:0;TType:0;ICode: 0;Tag: M_DHT;    Name:'';Desc: 'Define Huffman table'),
     ( TID:0;TType:0;ICode: 0;Tag: M_SOF5;   Name:'';Desc: 'Differential sequential'),
     ( TID:0;TType:0;ICode: 0;Tag: M_SOF6;   Name:'';Desc: 'Differential progressive'),
     ( TID:0;TType:0;ICode: 0;Tag: M_SOF7;   Name:'';Desc: 'Differential lossless'),
     ( TID:0;TType:0;ICode: 0;Tag: M_SOF9;   Name:'';Desc: 'Extended sequential, arithmetic coding'),
     ( TID:0;TType:0;ICode: 0;Tag: M_SOF10;  Name:'';Desc: 'Progressive, arithmetic coding'),
     ( TID:0;TType:0;ICode: 0;Tag: M_SOF11;  Name:'';Desc: 'Lossless, arithmetic coding'),
     ( TID:0;TType:0;ICode: 0;Tag: M_SOF13;  Name:'';Desc: 'Differential sequential, arithmetic coding'),
     ( TID:0;TType:0;ICode: 0;Tag: M_DAC;    Name:'';Desc: 'Define arithmetic coding conditioning'),
     ( TID:0;TType:0;ICode: 0;Tag: M_SOF14;  Name:'';Desc: 'Differential progressive, arithmetic coding'),
     ( TID:0;TType:0;ICode: 0;Tag: M_SOF15;  Name:'';Desc: 'Differential lossless, arithmetic coding'),
     ( TID:0;TType:0;ICode: 0;Tag: M_SOI;    Name:'';Desc: 'Start of Image'),
     ( TID:0;TType:0;ICode: 0;Tag: M_EOI;    Name:'';Desc: 'End of Image'),
     ( TID:0;TType:0;ICode: 0;Tag: M_SOS;    Name:'';Desc: 'Start of Scan'),
     ( TID:0;TType:0;ICode: 0;Tag: M_DQT;    Name:'';Desc: 'Define quantization table'),
     ( TID:0;TType:0;ICode: 0;Tag: M_DNL;    Name:'';Desc: 'Define number of lines'),
     ( TID:0;TType:0;ICode: 0;Tag: M_DRI;    Name:'';Desc: 'Restart interoperability definition'),
     ( TID:0;TType:0;ICode: 0;Tag: M_DHP;    Name:'';Desc: 'Define hierarchical progression'),
     ( TID:0;TType:0;ICode: 0;Tag: M_EXP;    Name:'';Desc: 'Expand reference component'),
     ( TID:0;TType:0;ICode: 0;Tag: M_JFIF;   Name:'';Desc: 'JPG marker'),
     ( TID:0;TType:0;ICode: 0;Tag: M_EXIF;   Name:'';Desc: 'Exif Data'),
     ( TID:0;TType:0;ICode: 0;Tag: M_EXIFEXT; Name:'';Desc: 'Exif Extended Data'),
     ( TID:0;TType:0;ICode: 0;Tag: M_COM;    Name:'';Desc: 'Comment'),
     ( TID:0;TType:0;ICode: 0;Tag: M_IPTC;   Name:'';Desc: 'IPTC data'),
     ( TID:0;TType:0;ICode: 0;Tag: M_APP14;  Name:'';Desc: 'Photoshop data'),
     ( TID:0;TType:0;ICode: 0;Tag: 0;        Name:'';Desc: 'Unknown')
    );

const
  dExifVersion: string = '1.04';

var
  CurTagArray: TImageInfo = nil;
  fmtInt: tfmtInt = defIntFmt;
  fmtReal: tfmtReal = defRealFmt;
  fmtFrac: tfmtFrac = defFracFmt;

  ExifNonThumbnailLength : integer;
  ShowTags: integer;
  ExifTrace: integer = 0;
{$IFDEF dEXIFpredeclare}
  ImgData:timgData;
{$ENDIF}

function FindExifTagDefByID(ATagID: Word): PTagEntry;
function FindGPSTagDefByID(ATagID: Word): PTagEntry;

function FindExifTagDefByName(ATagName: String): PTagEntry;
function FindGPSTagDefByName(ATagName: String): PTagEntry;

function LookupType(idx: integer): String;


implementation

uses
  dExifWrite, msData;

const
// Compression Type Constants
  JPEG_COMP_TYPE = 6;
  TIFF_COMP_TYPE = 1;

  GPSCnt = 32;
  ExifTagCnt = 251 - 5;  // NOTE: was 250 before, but "count" is 251
  TotalTagCnt = GPSCnt + ExifTagCnt;

var
  Whitelist: array [0..37] of Word = (
    TAG_EXIF_OFFSET,
    TAG_IMAGEWIDTH,
    TAG_IMAGELENGTH,
    TAG_BITSPERSAMPLE,
    TAG_COMPRESSION,
    TAG_PHOTOMETRICINTERPRETATION,
    TAG_IMAGEDESCRIPTION,
    TAG_MAKE,
    TAG_MODEL,
    TAG_DATETIME_MODIFY,
    TAG_ARTIST,
    TAG_WHITEPOINT,          // $013E
    (*
    $8769,  $100,  $101,  $102,
    $103,  $106,
    $10E,  $10F,  $110,  $132,
    $13B,   $13E,                    *)
     $301,
     $304,
     $5010,
     $5011,
    TAG_COPYRIGHT,           // $8298,
    TAG_EXPOSURETIME,        // $829A,
    TAG_TIMEZONEOFFSET,      // $882A,
    TAG_DATETIME_ORIGINAL,   // $9003,
    TAG_DATETIME_DIGITIZED,  // $9004,
    TAG_SHUTTERSPEED,        // $9201,
    TAG_APERTURE,            // $9202,
    TAG_BRIGHTNESSVALUE,     // $9203,
    TAG_EXPOSUREBIASVALUE,   // $9204,
    TAG_MAXAPERTUREVALUE,    // $9205,
    TAG_SUBJECT_DISTANCE,    // $9206,
    TAG_LIGHT_SOURCE,        // $9208,
    TAG_FLASH,               // $9209,
    TAG_FOCALLENGTH,         // $920A,
    TAG_FLASH_ENERGY,        // $920B,
    TAG_NOISE,               // $920D,
    TAG_USERCOMMENT,         // $9286,
    TAG_XP_TITLE,            // $9C9B,
    TAG_XP_COMMENT,          // $9C9C,
    TAG_XP_AUTHOR,           // $9C9D,
    TAG_XP_KEYWORDS,         // $9C9E,
    TAG_XP_SUBJECT           // $9C9F
  );

{ Many tags added based on Php4 source...
    http://lxr.php.net/source/php4/ext/exif/exif.c

  See also: https://sno.phy.queensu.ca/~phil/exiftool/TagNames/EXIF.html
}
var
 TagTable : array [0..ExifTagCnt-1] of TTagEntry =
// TagTable : array of TTagEntry =
// TagTable : TTagDefArray [0..ExifTagCnt] =
// TagTable: TTagDefArray =
 ((TID:0; TType:2; ICode:2; Tag:$0001; Count:1; Name:'InteroperabilityIndex'  ),         {0}
  (TID:0; TType:7; ICode:2; Tag:$0002; Count:1; Name:'InteroperabilityVersion'),
  (TID:0; TType:2; ICode:2; Tag:$000B; Count:1; Name:'ACDComment'             ),
  (TID:0; TType:4; ICode:2; Tag:$00FE; Count:1; Name:'NewSubfileType'         ),
  (TID:0; TType:3; ICode:2; Tag:$00FF; Count:1; Name:'SubfileType'            ),
  (TID:0; TType:4; ICode:2; Tag:$0100; Count:1; Name:'ImageWidth'             ),
  (TID:0; TType:4; ICode:2; Tag:$0101; Count:1; Name:'ImageLength'            ),
  (TID:0; TType:3; ICode:2; Tag:$0102; Count:3; Name:'BitsPerSample'),
  (TID:0; TType:3; ICode:2; Tag:$0103; Count:1; Name:'Compression'            ; Desc:'';
    Code:'6:Jpeg,3:Uncompressed,1:TIFF'),
  (TID:0; TType:3; ICode:2; Tag:$0106; Count:1; Name:'PhotometricInterpretation';Desc:'';
    Code:'1:Monochrome, 2:RGB, 6:YCbCr'),
  (TID:0; TType:3; ICode:2; Tag:$010A; Count:1; Name:'FillOrder'              ),         {10}
  (TID:0; TType:2; ICode:2; Tag:$010D; Count:1; Name:'DocumentName'           ),
  (TID:0; TType:2; ICode:2; Tag:$010E; Count:1; Name:'ImageDescription'       ),
  (TID:0; TType:2; ICode:2; Tag:$010F; Count:1; Name:'Make'                   ),
  (TID:0; TType:2; ICode:2; Tag:$0110; Count:1; Name:'Model'                  ),
  (TID:0; TType:4; ICode:2; Tag:$0111; Count:1; Name:'StripOffsets'           ),
  (TID:0; TType:3; ICode:2; Tag:$0112; Count:1; Name:'Orientation'            ; Desc:'';
    Code:'1:Normal,2:Mirror horizontal,3:Rotated 180°,'+
         '4:Mirror vertical,5:Mirror horizontal and rotate 90° CCW,'+
         '6:Rotate 90° CCW,7:Mirror horizontal and rotate 90° CW,'+
         '8:Clockwise 90°'),
  (TID:0; TType:3; ICode:2; Tag:$0115; Count:1; Name:'SamplesPerPixel'        ),
  (TID:0; TType:4; ICode:2; Tag:$0116; Count:1; Name:'RowsPerStrip'           ),
  (TID:0; TType:4; ICode:2; Tag:$0117; Count:1; Name:'StripByteCounts'        ),
  (TID:0; TType:3; ICode:2; Tag:$0118; Count:1; Name:'MinSampleValue'         ),         {20}
  (TID:0; TType:3; ICode:2; Tag:$0119; Count:1; Name:'MaxSampleValue'         ),
  (TID:0; TType:5; ICode:2; Tag:$011A; Count:1; Name:'XResolution';
    Desc:''; Code:''; Data:''; Raw:''; FormatS:'%5.2f'),
  (TID:0; TType:5; ICode:2; Tag:$011B; Count:1; Name:'YResolution';
    Desc:''; Code:''; Data:''; Raw:''; FormatS:'%5.2f'),
  (TID:0; TType:3; ICode:2; Tag:$011C; Count:1; Name:'PlanarConfiguration'    ),
  (TID:0; TType:2; ICode:2; Tag:$011D; Count:1; Name:'PageName'               ),
  (TID:0; TType:5; ICode:2; Tag:$011E; Count:1; Name:'XPosition'              ),
  (TID:0; TType:5; ICode:2; Tag:$011F; Count:1; Name:'YPosition'              ),
  (TID:0; TType:0; ICode:2; Tag:$0120; Count:1; Name:'FreeOffsets'            ),
  (TID:0; TType:0; ICode:2; Tag:$0121; Count:1; Name:'FreeByteCounts'         ),
  (TID:0; TType:3; ICode:2; Tag:$0122; Count:1; Name:'GrayReponseUnit'        ),         {30}
  (TID:0; TType:0; ICode:2; Tag:$0123; Count:1; Name:'GrayReponseCurve'       ),
  (TID:0; TType:0; ICode:2; Tag:$0124; Count:1; Name:'T4Options'              ),
  (TID:0; TType:0; ICode:2; Tag:$0125; Count:1; Name:'T6Options'              ),
  (TID:0; TType:3; ICode:2; Tag:$0128; Count:1; Name:'ResolutionUnit'         ; Desc:'';
    Code:'1:None specified,2:inches,3:cm'),
  (TID:0; TType:3; ICode:2; Tag:$0129; Count:2; Name:'PageNumber'             ),
  (TID:0; TType:3; ICode:2; Tag:$012D; Count:768; Name:'TransferFunction'     ),
  (TID:0; TType:2; ICode:2; Tag:$0131; Count:1; Name:'Software'               ),
  (TID:0; TType:2; ICode:2; Tag:$0132; Count:1; Name:'DateTime'               ),
  (TID:0; TType:2; ICode:2; Tag:$013B; Count:1; Name:'Artist'                 ),
  (TID:0; TType:2; ICode:2; Tag:$013C; Count:1; Name:'HostComputer'           ),         {40}
  (TID:0; TType:3; ICode:2; Tag:$013D; Count:1; Name:'Predictor'              ),
  (TID:0; TType:5; ICode:2; Tag:$013E; Count:2; Name:'WhitePoint'             ),
  (TID:0; TType:5; ICode:2; Tag:$013F; Count:6; Name:'PrimaryChromaticities'  ),
  (TID:0; TType:0; ICode:2; Tag:$0140; Count:1; Name:'ColorMap'               ),
  (TID:0; TType:3; ICode:2; Tag:$0141; Count:2; Name:'HalfToneHints'          ),
  (TID:0; TType:4; ICode:2; Tag:$0142; Count:1; Name:'TileWidth'              ),
  (TID:0; TType:4; ICode:2; Tag:$0143; Count:1; Name:'TileLength'             ),
  (TID:0; TType:0; ICode:2; Tag:$0144; Count:1; Name:'TileOffsets'            ),
  (TID:0; TType:0; ICode:2; Tag:$0145; Count:1; Name:'TileByteCounts'         ),
  (TID:0; TType:0; ICode:2; Tag:$014A; Count:1; Name:'SubIFDs'                ),         {50}
  (TID:0; TType:3; ICode:2; Tag:$014C; Count:1; Name:'InkSet'                 ),
  (TID:0; TType:0; ICode:2; Tag:$014D; Count:1; Name:'InkNames'               ),
  (TID:0; TType:0; ICode:2; Tag:$014E; Count:1; Name:'NumberOfInks'           ),
  (TID:0; TType:0; ICode:2; Tag:$0150; Count:1; Name:'DotRange'               ),
  (TID:0; TType:2; ICode:2; Tag:$0151; Count:1; Name:'TargetPrinter'          ),
  (TID:0; TType:0; ICode:2; Tag:$0152; Count:1; Name:'ExtraSample'            ),
  (TID:0; TType:0; ICode:2; Tag:$0153; Count:1; Name:'SampleFormat'           ),
  (TID:0; TType:0; ICode:2; Tag:$0154; Count:1; Name:'SMinSampleValue'        ),
  (TID:0; TType:0; ICode:2; Tag:$0155; Count:1; Name:'SMaxSampleValue'        ),
  (TID:0; TType:0; ICode:2; Tag:$0156; Count:1; Name:'TransferRange'          ),         {60}
  (TID:0; TType:0; ICode:2; Tag:$0157; Count:1; Name:'ClipPath'               ),
  (TID:0; TType:0; ICode:2; Tag:$0158; Count:1; Name:'XClipPathUnits'         ),
  (TID:0; TType:0; ICode:2; Tag:$0159; Count:1; Name:'YClipPathUnits'         ),
  (TID:0; TType:0; ICode:2; Tag:$015A; Count:1; Name:'Indexed'                ),
  (TID:0; TType:0; ICode:2; Tag:$015B; Count:1; Name:'JPEGTables'             ),
  (TID:0; TType:0; ICode:2; Tag:$015F; Count:1; Name:'OPIProxy'               ),
  (TID:0; TType:0; ICode:2; Tag:$0200; Count:1; Name:'JPEGProc'               ),
  (TID:0; TType:4; ICode:2; Tag:$0201; Count:1; Name:'JPEGInterchangeFormat'  ; Desc:'';Code:''; Data:''; Raw:''; FormatS:''; Size:4),
  (TID:0; TType:4; ICode:2; Tag:$0202; Count:1; Name:'JPEGInterchangeFormatLength'),
  (TID:0; TType:0; ICode:2; Tag:$0203; Count:1; Name:'JPEGRestartInterval'    ),         {70}
  (TID:0; TType:0; ICode:2; Tag:$0205; Count:1; Name:'JPEGLosslessPredictors' ),
  (TID:0; TType:0; ICode:2; Tag:$0206; Count:1; Name:'JPEGPointTransforms'    ),
  (TID:0; TType:0; ICode:2; Tag:$0207; Count:1; Name:'JPEGQTables'            ),
  (TID:0; TType:0; ICode:2; Tag:$0208; Count:1; Name:'JPEGDCTables'           ),
  (TID:0; TType:0; ICode:2; Tag:$0209; Count:1; Name:'JPEGACTables'           ),
  (TID:0; TType:5; ICode:2; Tag:$0211; Count:3; Name:'YCbCrCoefficients'      ),
  (TID:0; TType:3; ICode:2; Tag:$0212; Count:2; Name:'YCbCrSubSampling'       ),
  (TID:0; TType:3; ICode:2; Tag:$0213; Count:1; Name:'YCbCrPositioning';        Desc:'';
    Code:'1:Centered,2:Co-sited'),
  (TID:0; TType:5; ICode:2; Tag:$0214; Count:6; Name:'ReferenceBlackWhite'    ),
  (TID:0; TType:1; ICode:2; Tag:$02BC; Count:1; Name:'ExtensibleMetadataPlatform' ),     {80}
  (TID:0; TType:0; ICode:2; Tag:$0301; Count:1; Name:'Gamma'                     ),
  (TID:0; TType:0; ICode:2; Tag:$0302; Count:1; Name:'ICCProfileDescriptor'      ),
  (TID:0; TType:0; ICode:2; Tag:$0303; Count:1; Name:'SRGBRenderingIntent'       ),
  (TID:0; TType:0; ICode:2; Tag:$0304; Count:1; Name:'ImageTitle'                ),
  (TID:0; TType:2; ICode:2; Tag:$1000; Count:1; Name:'RelatedImageFileFormat' ),
  (TID:0; TType:3; ICode:2; Tag:$1001; Count:1; Name:'RelatedImageWidth'      ),
  (TID:0; TType:3; ICode:2; Tag:$1002; Count:1; Name:'RelatedImageHeight'     ),
  (TID:0; TType:0; ICode:2; Tag:$5001; Count:1; Name:'ResolutionXUnit'        ),
  (TID:0; TType:0; ICode:2; Tag:$5002; Count:1; Name:'ResolutionYUnit'        ),
  (TID:0; TType:0; ICode:2; Tag:$5003; Count:1; Name:'ResolutionXLengthUnit'  ),         {90}
  (TID:0; TType:0; ICode:2; Tag:$5004; Count:1; Name:'ResolutionYLengthUnit'  ),
  (TID:0; TType:0; ICode:2; Tag:$5005; Count:1; Name:'PrintFlags'             ),
  (TID:0; TType:0; ICode:2; Tag:$5006; Count:1; Name:'PrintFlagsVersion'      ),
  (TID:0; TType:0; ICode:2; Tag:$5007; Count:1; Name:'PrintFlagsCrop'         ),
  (TID:0; TType:0; ICode:2; Tag:$5008; Count:1; Name:'PrintFlagsBleedWidth'   ),
  (TID:0; TType:0; ICode:2; Tag:$5009; Count:1; Name:'PrintFlagsBleedWidthScale'),
  (TID:0; TType:0; ICode:2; Tag:$500A; Count:1; Name:'HalftoneLPI'            ),
  (TID:0; TType:0; ICode:2; Tag:$500B; Count:1; Name:'HalftoneLPIUnit'        ),
  (TID:0; TType:0; ICode:2; Tag:$500C; Count:1; Name:'HalftoneDegree'         ),
  (TID:0; TType:0; ICode:2; Tag:$500D; Count:1; Name:'HalftoneShape'          ),         {100}
  (TID:0; TType:0; ICode:2; Tag:$500E; Count:1; Name:'HalftoneMisc'           ),
  (TID:0; TType:0; ICode:2; Tag:$500F; Count:1; Name:'HalftoneScreen'         ),
  (TID:0; TType:0; ICode:2; Tag:$5010; Count:1; Name:'JPEGQuality'            ),
  (TID:0; TType:0; ICode:2; Tag:$5011; Count:1; Name:'GridSize'               ),
  (TID:0; TType:0; ICode:2; Tag:$5012; Count:1; Name:'ThumbnailFormat'        ),
  (TID:0; TType:0; ICode:2; Tag:$5013; Count:1; Name:'ThumbnailWidth'         ),
  (TID:0; TType:0; ICode:2; Tag:$5014; Count:1; Name:'ThumbnailHeight'        ),
  (TID:0; TType:0; ICode:2; Tag:$5015; Count:1; Name:'ThumbnailColorDepth'    ),
  (TID:0; TType:0; ICode:2; Tag:$5016; Count:1; Name:'ThumbnailPlanes'        ),
  (TID:0; TType:0; ICode:2; Tag:$5017; Count:1; Name:'ThumbnailRawBytes'      ),         {110}
  (TID:0; TType:0; ICode:2; Tag:$5018; Count:1; Name:'ThumbnailSize'          ),
  (TID:0; TType:0; ICode:2; Tag:$5019; Count:1; Name:'ThumbnailCompressedSize'),
  (TID:0; TType:0; ICode:2; Tag:$501A; Count:1; Name:'ColorTransferFunction'  ),
  (TID:0; TType:0; ICode:2; Tag:$501B; Count:1; Name:'ThumbnailData'          ),
  (TID:0; TType:0; ICode:2; Tag:$5020; Count:1; Name:'ThumbnailImageWidth'    ),
  (TID:0; TType:0; ICode:2; Tag:$5021; Count:1; Name:'ThumbnailImageHeight'   ),
  (TID:0; TType:0; ICode:2; Tag:$5022; Count:1; Name:'ThumbnailBitsPerSample' ),
  (TID:0; TType:0; ICode:2; Tag:$5023; Count:1; Name:'ThumbnailCompression'   ),
  (TID:0; TType:0; ICode:2; Tag:$5024; Count:1; Name:'ThumbnailPhotometricInterp'),
  (TID:0; TType:0; ICode:2; Tag:$5025; Count:1; Name:'ThumbnailImageDescription' ),      {120}
  (TID:0; TType:2; ICode:2; Tag:$5026; Count:1; Name:'ThumbnailEquipMake'     ),
  (TID:0; TType:2; ICode:2; Tag:$5027; Count:1; Name:'ThumbnailEquipModel'    ),
  (TID:0; TType:0; ICode:2; Tag:$5028; Count:1; Name:'ThumbnailStripOffsets'  ),
  (TID:0; TType:0; ICode:2; Tag:$5029; Count:1; Name:'ThumbnailOrientation'   ),
  (TID:0; TType:0; ICode:2; Tag:$502A; Count:1; Name:'ThumbnailSamplesPerPixel'),
  (TID:0; TType:0; ICode:2; Tag:$502B; Count:1; Name:'ThumbnailRowsPerStrip'  ),
  (TID:0; TType:0; ICode:2; Tag:$502C; Count:1; Name:'ThumbnailStripBytesCount'),
  (TID:0; TType:0; ICode:2; Tag:$502D; Count:1; Name:'ThumbnailResolutionX'   ),
  (TID:0; TType:0; ICode:2; Tag:$502E; Count:1; Name:'ThumbnailResolutionY'   ),
  (TID:0; TType:0; ICode:2; Tag:$502F; Count:1; Name:'ThumbnailPlanarConfig'  ),         {130}
  (TID:0; TType:0; ICode:2; Tag:$5030; Count:1; Name:'ThumbnailResolutionUnit'),
  (TID:0; TType:0; ICode:2; Tag:$5031; Count:1; Name:'ThumbnailTransferFunction'),
  (TID:0; TType:2; ICode:2; Tag:$5032; Count:1; Name:'ThumbnailSoftwareUsed'  ),
  (TID:0; TType:2; ICode:2; Tag:$5033; Count:1; Name:'ThumbnailDateTime'      ),
  (TID:0; TType:2; ICode:2; Tag:$5034; Count:1; Name:'ThumbnailArtist'        ),
  (TID:0; TType:0; ICode:2; Tag:$5035; Count:1; Name:'ThumbnailWhitePoint'    ),
  (TID:0; TType:0; ICode:2; Tag:$5036; Count:1; Name:'ThumbnailPrimaryChromaticities'),
  (TID:0; TType:0; ICode:2; Tag:$5037; Count:1; Name:'ThumbnailYCbCrCoefficients'    ),
  (TID:0; TType:0; ICode:2; Tag:$5038; Count:1; Name:'ThumbnailYCbCrSubsampling'     ),
  (TID:0; TType:0; ICode:2; Tag:$5039; Count:1; Name:'ThumbnailYCbCrPositioning'     ),  {140}
  (TID:0; TType:0; ICode:2; Tag:$503A; Count:1; Name:'ThumbnailRefBlackWhite' ),
  (TID:0; TType:2; ICode:2; Tag:$503B; Count:1; Name:'ThumbnailCopyRight'     ),
  (TID:0; TType:0; ICode:2; Tag:$5090; Count:1; Name:'LuminanceTable'         ),
  (TID:0; TType:0; ICode:2; Tag:$5091; Count:1; Name:'ChrominanceTable'       ),
  (TID:0; TType:0; ICode:2; Tag:$5100; Count:1; Name:'FrameDelay'             ),
  (TID:0; TType:0; ICode:2; Tag:$5101; Count:1; Name:'LoopCount'              ),
  (TID:0; TType:0; ICode:2; Tag:$5110; Count:1; Name:'PixelUnit'              ),
  (TID:0; TType:0; ICode:2; Tag:$5111; Count:1; Name:'PixelPerUnitX'          ),
  (TID:0; TType:0; ICode:2; Tag:$5112; Count:1; Name:'PixelPerUnitY'          ),
  (TID:0; TType:0; ICode:2; Tag:$5113; Count:1; Name:'PaletteHistogram'       ),         {150}
  (TID:0; TType:0; ICode:2; Tag:$800D; Count:1; Name:'ImageID'                ),
  (TID:0; TType:0; ICode:2; Tag:$80E3; Count:1; Name:'Matteing'               ),   //* obsoleted by ExtraSamples */
  (TID:0; TType:0; ICode:2; Tag:$80E4; Count:1; Name:'DataType'               ),   //* obsoleted by SampleFormat */
  (TID:0; TType:0; ICode:2; Tag:$80E5; Count:1; Name:'ImageDepth'             ),
  (TID:0; TType:0; ICode:2; Tag:$80E6; Count:1; Name:'TileDepth'              ),
  (TID:0; TType:3; ICode:2; Tag:$828D; Count:2; Name:'CFARepeatPatternDim'    ),
  (TID:0; TType:1; ICode:2; Tag:$828E; Count:1; Name:'CFAPattern'             ),  //count: ???
  (TID:0; TType:0; ICode:2; Tag:$828F; Count:1; Name:'BatteryLevel'           ),
  (TID:0; TType:2; ICode:2; Tag:$8298; Count:1; Name:'Copyright'              ),
  (TID:0; TType:5; ICode:2; Tag:$829A; Count:1; Name:'ExposureTime';            Desc:'Exposure time'; Code:''; Data:''; Raw:''; FormatS:'%s sec'),   {160}
  (TID:0; TType:5; ICode:2; Tag:$829D; Count:1; Name:'FNumber';                 Desc:''; Code:''; Data:''; Raw:''; FormatS:'F%0.1f'),
  (TID:0; TType:4; ICode:2; Tag:$83BB; Count:1; Name:'IPTC/NAA';                Desc:'IPTC/NAA'),
  (TID:0; TType:0; ICode:2; Tag:$84E3; Count:1; Name:'IT8RasterPadding'        ),
  (TID:0; TType:0; ICode:2; Tag:$84E5; Count:1; Name:'IT8ColorTable'           ),
  (TID:0; TType:0; ICode:2; Tag:$8649; Count:1; Name:'ImageResourceInformation'),
  (TID:0; TType:0; ICode:2; Tag:$8769; Count:1; Name:'ExifOffset';              Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:4),
  (TID:0; TType:0; ICode:2; Tag:$8773; Count:1; Name:'InterColorProfile'        ),
  (TID:0; TType:3; ICode:2; Tag:$8822; Count:1; Name:'ExposureProgram';         Desc:'';
    Code:'0:Unidentified,1:Manual,2:Normal,3:Aperture priority,'+
         '4:Shutter priority,5:Creative(slow),'+
         '6:Action(high-speed),7:Portrait mode,8:Landscape mode'),
  (TID:0; TType:2; ICode:2; Tag:$8824; Count:1; Name:'SpectralSensitivity'    ),
  (TID:0; TType:0; ICode:2; Tag:$8825; Count:1; Name:'GPSInfo'; Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:4),         {170}
  (TID:0; TType:3; ICode:2; Tag:$8827; Count:1; Name:'ISOSpeedRatings'        ), { 171 }
  (TID:0; TType:0; ICode:2; Tag:$8828; Count:1; Name:'OECF'                   ),
  (TID:0; TType:0; ICode:2; Tag:$8829; Count:1; Name:'Interlace'              ),
  (TID:0; TType:8; ICode:2; Tag:$882A; Count:1; Name:'TimeZoneOffset'         ),
  (TID:0; TType:3; ICode:2; Tag:$882B; Count:1; Name:'SelfTimerMode'          ),
  (TID:0; TType:7; ICode:2; Tag:$9000; Count:1; Name:'ExifVersion'            ),
  (TID:0; TType:2; ICode:2; Tag:$9003; Count:1; Name:'DateTimeOriginal'       ),
  (TID:0; TType:2; ICode:2; Tag:$9004; Count:1; Name:'DateTimeDigitized'      ),
  (TID:0; TType:7; ICode:2; Tag:$9101; Count:1; Name:'ComponentsConfiguration'; Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:0; Callback:GenCompConfig),
  (TID:0; TType:5; ICode:2; Tag:$9102; Count:1; Name:'CompressedBitsPerPixel' ),         {180}
  (TID:0; TType:10;ICode:2; Tag:$9201; Count:1; Name:'ShutterSpeedValue'; Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:0; Callback:SSpeedCallBack),
  (TID:0; TType:5; ICode:2; Tag:$9202; Count:1; Name:'ApertureValue'; Desc:'Aperture value'; Code:''; Data:''; Raw:''; FormatS:'F%0.1f'),
  (TID:0; TType:10;ICode:2; Tag:$9203; Count:1; Name:'BrightnessValue'        ),
  (TID:0; TType:10;ICode:2; Tag:$9204; Count:1; Name:'ExposureBiasValue'      ),
  (TID:0; TType:5; ICode:2; Tag:$9205; Count:1; Name:'MaxApertureValue';        Desc:''; Code:''; Data:''; Raw:''; FormatS:'F%0.1f'),
  (TID:0; TType:5; ICode:2; Tag:$9206; Count:1; Name:'SubjectDistance'        ),
  (TID:0; TType:3; ICode:2; Tag:$9207; Count:1; Name:'MeteringMode';            Desc:'';
    Code:'0:Unknown,1:Average,2:Center,3:Spot,4:MultiSpot,5:MultiSegment,6:Partial'),
  (TID:0; TType:3; ICode:2; Tag:$9208; Count:1; Name:'LightSource';             Desc:'';
    Code:'0:Unidentified,1:Daylight,2:Fluorescent,3:Tungsten,10:Flash,17:Std A,18:Std B,19:Std C'),
  (TID:0; TType:3; ICode:2; Tag:$9209; Count:1; Name:'Flash';                   Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:FlashCallBack),
  (TID:0; TType:5; ICode:2; Tag:$920A; Count:1; Name:'FocalLength'            ; Desc:'Focal length'; Code:''; Data:''; Raw:''; FormatS:'%0.2f mm'), {190}
  (TID:0; TType:0; ICode:2; Tag:$920B; Count:1; Name:'FlashEnergy'            ),
  (TID:0; TType:0; ICode:2; Tag:$920C; Count:1; Name:'SpatialFrequencyResponse'),
  (TID:0; TType:0; ICode:2; Tag:$920D; Count:1; Name:'Noise'                  ),
  (TID:0; TType:0; ICode:2; Tag:$920E; Count:1; Name:'FocalPlaneXResolution'  ),
  (TID:0; TType:0; ICode:2; Tag:$920F; Count:1; Name:'FocalPlaneYResolution'  ),
  (TID:0; TType:0; ICode:2; Tag:$9210; Count:1; Name:'FocalPlaneResolutionUnit'; Desc:'';
    Code:'1:None Specified,2:Inch,3:Centimeter'),
  (TID:0; TType:4; ICode:2; Tag:$9211; Count:1; Name:'ImageNumber'            ),
  (TID:0; TType:2; ICode:2; Tag:$9212; Count:1; Name:'SecurityClassification' ),
  (TID:0; TType:2; ICode:2; Tag:$9213; Count:1; Name:'ImageHistory'           ),
  (TID:0; TType:3; ICode:2; Tag:$9214; Count:2; Name:'SubjectLocation'        ),         {200}
  (TID:0; TType:0; ICode:2; Tag:$9215; Count:1; Name:'ExposureIndex'          ),
  (TID:0; TType:0; ICode:2; Tag:$9216; Count:1; Name:'TIFF/EPStandardID'      ),
  (TID:0; TType:0; ICode:2; Tag:$9217; Count:1; Name:'SensingMethod'          ),
  (TID:0; TType:0; ICode:2; Tag:$923F; Count:1; Name:'StoNits'                ),
  (TID:0; TType:7; ICode:2; Tag:$927C; Count:1; Name:'MakerNote'              ),
  (TID:0; TType:7; ICode:2; Tag:$9286; Count:1; Name:'UserComment'            ),
  (TID:0; TType:2; ICode:2; Tag:$9290; Count:1; Name:'SubSecTime'             ),
  (TID:0; TType:2; ICode:2; Tag:$9291; Count:1; Name:'SubSecTimeOriginal'     ),
  (TID:0; TType:2; ICode:2; Tag:$9292; Count:1; Name:'SubSecTimeDigitized'    ),
  (TID:0; TType:0; ICode:2; Tag:$953C; Count:1; Name:'ImageSourceData'        ),  // "Adobe Photoshop Document Data Block": 8BIM...  {210}
//  (TID:0; TType:0; ICode:2; Tag:$9C9B; Count:1; Name:'Title'                  ;  Callback: xpTranslate),  // Win XP specific, Unicode
//  (TID:0; TType:0; ICode:2; Tag:$9C9C; Count:1; Name:'Comments'               ;  Callback: xpTranslate),  // Win XP specific, Unicode
//  (TID:0; TType:0; ICode:2; Tag:$9C9D; Count:1; Name:'Author'                 ;  Callback: xpTranslate),  // Win XP specific, Unicode
//  (TID:0; TType:0; ICode:2; Tag:$9C9E; Count:1; Name:'Keywords'               ;  Callback: xpTranslate),  // Win XP specific, Unicode
//  (TID:0; TType:0; ICode:2; Tag:$9C9F; Count:1; Name:'Subject'                ;  Callback: xpTranslate),  // Win XP specific, Unicode
  (TID:0; TType:0; ICode:2; Tag:$A000; Count:1; Name:'FlashPixVersion'        ),
  (TID:0; TType:3; ICode:2; Tag:$A001; Count:1; Name:'ColorSpace'             ; Desc:''; Code:'0:sBW,1:sRGB'),
  (TID:0; TType:3; ICode:2; Tag:$A002; Count:1; Name:'ExifImageWidth'         ),
  (TID:0; TType:3; ICode:2; Tag:$A003; Count:1; Name:'ExifImageLength'        ),
  (TID:0; TType:2; ICode:2; Tag:$A004; Count:1; Name:'RelatedSoundFile'       ),         {220}
  (TID:0; TType:0; ICode:2; Tag:$A005; Count:1; Name:'InteroperabilityOffset' ),
  (TID:0; TType:5; ICode:2; Tag:$A20B; Count:1; Name:'FlashEnergy'            ),    // TID:0;TType:0;ICode: 2;Tag: $920B in TIFF/EP
  (TID:0; TType:0; ICode:2; Tag:$A20C; Count:1; Name:'SpatialFrequencyResponse'),   // TID:0;TType:0;ICode: 2;Tag: $920C    -  -
  (TID:0; TType:5; ICode:2; Tag:$A20E; Count:1; Name:'FocalPlaneXResolution'  ),    // TID:0;TType:0;ICode: 2;Tag: $920E    -  -
  (TID:0; TType:5; ICode:2; Tag:$A20F; Count:1; Name:'FocalPlaneYResolution'  ),    // TID:0;TType:0;ICode: 2;Tag: $920F    -  -
  (TID:0; TType:3; ICode:2; Tag:$A210; Count:1; Name:'FocalPlaneResolutionUnit'; Desc:'';
    Code:'1:None Specified,2:Inch,3:Centimeter'),      // TID:0;TType:0;ICode: 2;Tag: $9210    -  -
  (TID:0; TType:0; ICode:2; Tag:$A211; Count:1; Name:'ImageNumber'            ),
  (TID:0; TType:0; ICode:2; Tag:$A212; Count:1; Name:'SecurityClassification' ),
  (TID:0; TType:0; ICode:2; Tag:$A213; Count:1; Name:'ImageHistory'           ),
  (TID:0; TType:3; ICode:2; Tag:$A214; Count:2; Name:'SubjectLocation'        ),        {230}
  (TID:0; TType:5; ICode:2; Tag:$A215; Count:1; Name:'ExposureIndex'          ),
  (TID:0; TType:0; ICode:2; Tag:$A216; Count:1; Name:'TIFF/EPStandardID'      ; Desc:'TIFF/EPStandardID' ),
  (TID:0; TType:3; ICode:2; Tag:$A217; Count:1; Name:'SensingMethod'          ; Desc:'';
    Code:'0:Unknown,1:MonochromeArea,1:Not defined,2:OneChipColorArea,'+
         '3:TwoChipColorArea,4:ThreeChipColorArea,5:ColorSequentialArea,'+
         '6:MonochromeLinear,7:TriLinear,8:ColorSequentialLinear'),	       	           // TID:0;TType:0;ICode: 2;Tag: $9217    -  -
  (TID:0; TType:7; ICode:2; Tag:$A300; Count:1; Name:'FileSource'             ; Desc:'';
    Code:'0:Unknown,1:Film scanner,2:Reflection print scanner,3:Digital camera'),
  (TID:0; TType:7; ICode:2; Tag:$A301; Count:1; Name:'SceneType'              ; Desc:'';
    Code:'0:Unknown,1:Directly Photographed'),
  (TID:0; TType:7; ICode:2; Tag:$A302; Count:1; Name:'CFAPattern'             ),
  (TID:0; TType:3; ICode:2; Tag:$A401; Count:1; Name:'CustomRendered'         ; Desc:'';
    Code:'0:Normal process,1:Custom process'),
  (TID:0; TType:3; ICode:2; Tag:$A402; Count:1; Name:'ExposureMode'           ; Desc:'';
    Code:'0:Auto,1:Manual,2:Auto bracket'),
  (TID:0; TType:3; ICode:2; Tag:$A403; Count:1; Name:'WhiteBalance'           ; Desc:'';
    Code:'0:Auto,1:Manual'),
  (TID:0; TType:5; ICode:2; Tag:$A404; Count:1; Name:'DigitalZoomRatio'       ),        {240}
  (TID:0; TType:3; ICode:2; Tag:$A405; Count:1; Name:'FocalLengthIn35mmFilm'  ; Desc:'Focal Length in 35mm Film'; Code:''; Data:''; Raw:''; FormatS:'%5.2f mm'),
  (TID:0; TType:3; ICode:2; Tag:$A406; Count:1; Name:'SceneCaptureType'       ; Desc:'';
    Code:'0:Standard,1:Landscape,2:Portrait,3:Night scene'),
  (TID:0; TType:3; ICode:2; Tag:$A407; Count:1; Name:'GainControl'            ; Desc:'';
    Code:'0:None,1:Low gain up,2:High gain up,3:Low gain down,4:High gain down'),
  (TID:0; TType:3; ICode:2; Tag:$A408; Count:1; Name:'Contrast'               ; Desc:'';
    Code:'0:Normal,1:Soft,2:Hard'),
  (TID:0; TType:3; ICode:2; Tag:$A409; Count:1; Name:'Saturation'             ; Desc:'';
    Code:'0:Normal,1:Low,2:High'),
  (TID:0; TType:3; ICode:2; Tag:$A40A; Count:1; Name:'Sharpness'              ; Desc:'';
    Code:'0:Normal,1:Soft,2:Hard'),
  (TID:0; TType:0; ICode:2; Tag:$A40B; Count:1; Name:'DeviceSettingDescription'),
  (TID:0; TType:3; ICode:2; Tag:$A40C; Count:1; Name:'SubjectDistanceRange'   ; Desc:''; {250}
    Code:'0:Unknown,1:Macro,2:Close view,3:Distant view'),
  (TID:0; TType:2; ICode:2; Tag:$A420; Count:1; Name:'ImageUniqueID'          ; Desc:'';
    Code:'0:Close view,1:Distant view'),  {250}
  (TID:0; TType:0; ICode:2; Tag:0;     Count:1; Name:'Unknown')
);                        {250}

 GPSTable : array [0..GPSCnt-1] of TTagEntry = (
  (TID:0; TType:1; ICode:2; Tag:$000; Count:4; Name:'GPSVersionID'; Desc:''),
  (TID:0; TType:2; ICode:2; Tag:$001; Count:2; Name:'GPSLatitudeRef'; Desc:''),
  (TID:0; TType:5; ICode:2; Tag:$002; Count:3; Name:'GPSLatitude'; Desc:'';
    Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:GpsPosn),
  (TID:0; TType:2; ICode:2; Tag:$003; Count:2; Name:'GPSLongitudeRef';Desc:''),
  (TID:0; TType:5; ICode:2; Tag:$004; Count:3; Name:'GPSLongitude'; Desc:'';
    Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:GpsPosn),
  (TID:0; TType:1; ICode:2; Tag:$005; Count:1; Name:'GPSAltitudeRef'; Desc:'';
    Code:'0:Above Sealevel,1:Below Sealevel'),
  (TID:0; TType:5; ICode:2; Tag:$006; Count:1; Name:'GPSAltitude'; Desc:'';
    Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:GpsAltitude),
  (TID:0; TType:5; ICode:2; Tag:$007; Count:3; Name:'GPSTimeStamp'; Desc:'';
    Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:CvtTime),
  (TID:0; TType:2; ICode:2; Tag:$008; Count:1; Name:'GPSSatellites'; Desc:''),
  (TID:0; TType:2; ICode:2; Tag:$009; Count:2; Name:'GPSStatus'; Desc:'';
    Code:'A:Active;V:Void'),
  (TID:0; TType:2; ICode:2; Tag:$00A; Count:2; Name:'GPSMeasureMode'; Desc:'';
    Code:'2:2D,3:3D'),
  (TID:0; TType:5; ICode:2; Tag:$00B; Count:1; Name:'GPSDOP'; Desc:''),
  (TID:0; TType:2; ICode:2; Tag:$00C; Count:2; Name:'GPSSpeedRef'; Desc:'';
    Code:'K:km/h,M:mph,N:knots'),
  (TID:0; TType:5; ICode:2; Tag:$00D; Count:1; Name:'GPSSpeed'; Desc:''),
  (TID:0; TType:2; ICode:2; Tag:$00E; Count:2; Name:'GPSTrackRef'; Desc:'';
    Code:'M:Magnetic North,T:True North'),
  (TID:0; TType:5; ICode:2; Tag:$00F; Count:1; Name:'GPSTrack'; Desc:''),
  (TID:0; TType:2; ICode:2; Tag:$010; Count:2; Name:'GPSImageDirectionRef'; Desc:'';
    Code:'M:Magnetic North,T:True North'),
  (TID:0; TType:5; ICode:2; Tag:$011; Count:1; Name:'GPSImageDirection'; Desc:''),
  (TID:0; TType:2; ICode:2; Tag:$012; Count:1; Name:'GPSMapDatum'; Desc:''),
  (TID:0; TType:2; ICode:2; Tag:$013; Count:2; Name:'GPSDestLatitudeRef'; Desc:'';
    Code:'N:North,S:South'),
  (TID:0; TType:5; ICode:2; Tag:$014; Count:3; Name:'GPSDestLatitude'; Desc:'';
    Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:GpsPosn),
  (TID:0; TType:2; ICode:2; Tag:$015; Count:2; Name:'GPSDestLongitudeRef'; Desc:'';
    Code: 'E:East,W:West'),
  (TID:0; TType:5; ICode:2; Tag:$016; Count:3; Name:'GPSDestLongitude'; Desc:'';
    Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:GpsPosn),
  (TID:0; TType:2; ICode:2; Tag:$017; Count:2; Name:'GPSDestBearingRef'; Desc:'';
    Code:'M:Magnetic North,T:True North'),
  (TID:0; TType:5; ICode:2; Tag:$018; Count:1; Name:'GPSDestBearing'; Desc:''),
  (TID:0; TType:2; ICode:2; Tag:$019; Count:2; Name:'GPSDestDistanceRef'; Desc:'';
    Code:'K:Kilometers,M:Miles,N:Nautic Miles'),
  (TID:0; TType:5; ICode:2; Tag:$01A; Count:1; Name:'GPSDestDistance'; Desc:''),
  (TID:0; TType:7; ICode:2; Tag:$01B; Count:1; Name:'GPSProcessingMode'; Desc:''),
  (TID:0; TType:7; ICode:2; Tag:$01C; Count:1; Name:'GPSAreaInformation'; Desc:''),
  (TID:0; TType:2; ICode:2; Tag:$01D; Count:7; Name:'GPSDateStamp'; Desc:''),
  (TID:0; TType:3; ICode:2; Tag:$01E; Count:1; Name:'GPSDifferential'; Desc:'';
    Code:'0:No Correction,1:Differential Correction'),
  (TID:0; TType:5; ICode:2; Tag:$01F; Count:1; Name:'GPSHPositioningError'; Desc:'')
  );

  tagInit : boolean = false;

function FindExifTagDefByName(ATagName: String): PTagEntry;
var
  i: Integer;
begin
  for i:=0 to High(TagTable) do begin
    Result := @TagTable[i];
    if AnsiSameText(Result^.Name, ATagName) then
      exit;
  end;
  Result := nil;
end;

function FindExifTagDefByID(ATagID: word): PTagEntry;
var
  i: Integer;
begin
  for i:=0 to High(TagTable) do begin
    Result := @TagTable[i];
    if Result^.Tag = ATagID then
      exit;
  end;
  Result := nil;
end;

function FindGpsTagDefByName(ATagName: String): PTagEntry;
var
  i: Integer;
begin
  for i:=0 to High(GpsTable) do begin
    Result := @GpsTable[i];
    if AnsiSameText(Result^.Name, ATagName) then
      exit;
  end;
  Result := nil;
end;

function FindGpsTagDefByID(ATagID: word): PTagEntry;
var
  i: Integer;
begin
  for i:=0 to High(GpsTable) do begin
    Result := @GpsTable[i];
    if Result^.Tag = ATagID then
      exit;
  end;
  Result := nil;
end;

Procedure FixTagTable(var tags:array of TTagEntry);
var i:integer;
begin
  for i := low(tags) to high(tags) do
  begin
    if Length(tags[i].Desc) <= 0 then
      tags[i].Desc := tags[i].Name;
  end;
end;

Procedure FixTagTableParse(var tags:array of TTagEntry);
var i:integer;
begin
  for i := low(tags) to high(tags) do
  begin
    if Length(tags[i].Desc) <= 0 then
      tags[i].Desc := InsertSpaces(tags[i].Name);
  end;
end;

procedure LoadTagDescs(fancy:boolean = false);
begin
  if tagInit
    then exit
    else tagInit := true;
  if fancy then
  begin
    FixTagTableParse(TagTable);
    FixTagTableParse(GPSTable);
  end
  else
  begin
    FixTagTable(TagTable);
    FixTagTable(GPSTable);
  end;
end;

function LookupMTagID(idx:integer; ManuTable: array of TTagEntry):integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to high(ManuTable) do
    if ManuTable[i].Tag = idx then
    begin
      result := i;
      break;
    end;
end;

function LookupType(idx: integer): String;
var
  i: integer;
begin
  result := 'Unknown';
//  for i := 0 to (Sizeof(ProcessTable) div SizeOf(TTagEntry))-1 do
  for i := 0 to High(ProcessTable) do
    if ProcessTable[i].Tag = idx then begin
      Result := ProcessTable[i].Desc;
      exit;
    end;
end;

function LookupTagDefByID(idx: integer; ATagType: TTagType = ttExif): integer;
var
  i:integer;
begin
  Result := -1;
  case ATagType of
    ttExif, ttThumb:
      for i := 0 to ExifTagCnt-1 do
        if TagTable[i].Tag = idx then begin
          Result := i;
          break;
        end;
    ttGps:
      for i := 0 to GPSCnt-1 do
        if GPSTable[i].Tag = idx then begin
          Result := i;
          break;
        end;
  end;
end;

function FetchTagDefByID(idx: integer; ATagType: TTagType = ttExif): TTagEntry;
var
  i: integer;
begin
  Result := TagTable[ExifTagCnt-1];
  case ATagType of
    ttExif, ttThumb:
      for i := 0 to ExifTagCnt-1 do
        if TagTable[i].Tag = idx then begin
          result := TagTable[i];
          break;
        end;
    ttGps:
      for i := 0 to GPSCnt-1 do
        if GPSTable[i].Tag = idx then begin
          result := GPSTable[i];
          break;
        end;
  end;
end;

function LookupCode(ATagID: Word; ATagType: TTagType=ttExif): String; overload;
var
  i:integer;
begin
  Result := '';
  case ATagType of
    ttExif, ttThumb:
      for i := 0 to ExifTagCnt-1 do
        if TagTable[i].Tag = ATagID then begin
          Result := TagTable[i].Code;
          break;
        end;
    ttGps:
      for i := 0 to GPSCnt-1 do
        if GPSTable[i].Tag = ATagID then begin
          Result := GPSTable[i].Code;
          break;
        end;
  end;
end;

function LookupCode(ATagID: Word; TagTbl: array of TTagEntry): String; overload;
var
  i: integer;
begin
  Result := '';
  for i := 0 to High(TagTbl) do
    if TagTbl[i].Tag = ATagID then begin
      Result := TagTbl[i].Code;
      break;
    end;
end;

{ Tries to find the string AValue within TTagEntry.Code and
  returns the numerical value assigned to the Code (before the colon).

  Example:
  The codes defined for the Tag "ResolutionUnits" are
    '1:None Specified,2:Inch,3:Centimeter'.
  If AValue is 'Inch' then the value 2 is returned. }
function GetTagCode(ATag: TTagEntry; AValue: String): Integer;
var
  i: Integer;
begin
  if ATag.Code <> '' then
    Result := FindTextIndexInCode(AValue, ATag.Code)
  else
  if TryStrToInt(AValue, i) then
    Result := i
  else
    Result := -1;
end;



{------------------------------------------------------------------------------}
{                        TBasicMetaDataWriter                                  }
{------------------------------------------------------------------------------}

constructor TBasicMetadataWriter.Create(AImgData: TImgData);
begin
  FImgData := AImgData;
  FErrLog := TStringList.Create;
end;

destructor TBasicMetadataWriter.Destroy;
begin
  FErrLog.Free;
  inherited;
end;

procedure TBasicMetadataWriter.LogError(const AMsg: String);
begin
  FErrLog.Add(AMsg);
end;

procedure TBasicMetadataWriter.WriteToStream(AStream: TStream);
begin
  FErrLog.Clear;
end;


//------------------------------------------------------------------------------
//                                 TEndInd
//
// Here we implement the Endian Independent layer.  Outside of these methods
// we don't care about endian issues.
//------------------------------------------------------------------------------

function TEndInd.GetDataBuff: AnsiString;
begin
  result := FData;
end;

procedure TEndInd.SetDataBuff(const Value: AnsiString);
begin
  FData := Value;
end;

procedure TEndInd.WriteInt16(var buff: AnsiString; int,posn: integer);
begin
  if MotorolaOrder then
  begin
    buff[posn+1] := ansichar(int mod 256);
    buff[posn]   := ansichar(int div 256);
  end
  else
  begin
    buff[posn]   := ansichar(int mod 256);
    buff[posn+1] := ansichar(int div 256);
  end
end;

procedure TEndInd.WriteInt32(var buff: ansistring; int, posn: longint);
begin
  if MotorolaOrder then
  begin
    buff[posn+3] := ansichar(int mod 256);
    buff[posn+2] := ansichar((int shr  8) mod 256);
    buff[posn+1] := ansichar((int shr 16) mod 256);
    buff[posn]   := ansichar((int shr 24) mod 256);
  end
  else
  begin
    buff[posn]   := ansichar(int mod 256);
    buff[posn+1] := ansichar((int shr  8) mod 256);
    buff[posn+2] := ansichar((int shr 16) mod 256);
    buff[posn+3] := ansichar((int shr 24) mod 256);
  end
end;

// Convert a 16 bit unsigned value from file's native byte order
function TEndInd.Get16u(AOffs: integer):word;
// var hibyte,lobyte:byte;
begin
// To help debug, uncomment the following two lines
//  hibyte := byte(llData[oset+1]);
//  lobyte := byte(llData[oset]);
  if MotorolaOrder then
    result := (byte(FData[AOffs]) shl 8) or byte(FData[AOffs+1])
  else
    result := (byte(FData[AOffs+1]) shl 8) or byte(FData[AOffs]);
end;

// Convert a 32 bit signed value from file's native byte order
function TEndInd.Get32s(AOffs: integer):Longint;
begin
  if MotorolaOrder then
    result := (byte(FData[AOffs])   shl 24)
           or (byte(FData[AOffs+1]) shl 16)
           or (byte(FData[AOffs+2]) shl 8)
           or  byte(FData[AOffs+3])
  else
    result := (byte(FData[AOffs+3]) shl 24)
           or (byte(FData[AOffs+2]) shl 16)
           or (byte(FData[AOffs+1]) shl 8)
           or  byte(FData[AOffs]);
end;

// Convert a 32 bit unsigned value from file's native byte order
function TEndInd.Put32s(data: Longint): AnsiString;
var
  data2: integer;
 // buffer: string[4] absolute data2;
 // bbuff: AnsiChar;
begin
  data2 := data;
  if MotorolaOrder then
    data2 := NtoBE(data) else
    data2 := NtoLE(data);
  SetLength(Result, 4);
  Move(data2, Result[1], 4);
    {
  begin
    bbuff     := buffer[1];
    buffer[1] := buffer[4];
    buffer[4] := bbuff;
    bbuff     := buffer[2];
    buffer[2] := buffer[3];
    buffer[3] := bbuff;
  end;
  }
//  Result := buffer;
end;

// Convert a 32 bit unsigned value from file's native byte order
function TEndInd.Get32u(AOffs: integer): Longword;
begin
  result := Longword(Get32S(AOffs)) and $FFFFFFFF;
end;


{------------------------------------------------------------------------------}
{                            TImageInfo                                        }
{------------------------------------------------------------------------------}

constructor TImageInfo.Create(p: timgdata; buildCode: integer = GenAll);
begin
  inherited Create;
  LoadTagDescs(True);  // initialize global structures
  FITagCount := 0;
  buildList := BuildCode;
  clearDirStack;
  parent := p;
end;

// These destructors provided by Keith Murray of byLight Technologies - Thanks!
destructor TImageInfo.Destroy;
begin
  SetLength(fITagArray, 0);
  inherited;
end;

procedure TImageInfo.Assign(Source: TImageInfo);
begin
//  FCameraMake     := Source.FCameraMake;
//  FCameraModel    := Source.FCameraModel;
//  DateTime        := Source.DateTime;
  Height          := Source.Height;
  Width           := Source.Width;
  FlashUsed       := Source.FlashUsed;
//  Comments        := Source.Comments;
  MakerNote       := Source.MakerNote;
  TraceStr        := Source.TraceStr;
  msTraceStr      := Source.msTraceStr;
  msAvailable     := Source.msAvailable;
  msName          := Source.msName;
end;

function TImageInfo.GetTagByDesc(SearchStr: String): TTagEntry;
var
  i: integer;
begin
  i := LookupTagByDesc(SearchStr);
  if i >= 0 then
    Result := fiTagArray[i]
  else
    Result := EmptyEntry;
end;


//  This function returns the index of a tag name in the tag buffer.
function TImageInfo.LookupTag(ATagName: String): integer;
var
  i: integer;
begin
  ATagName := UpperCase(ATagName);
  for i := 0 to fiTagCount-1 do
    if UpperCase(fiTagArray[i].Name) = ATagName then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

// This function returns the data value for a given tag name.
function TImageInfo.LookupTagVal(ATagName: String): String;
var
  i: integer;
begin
  ATagName := UpperCase(ATagName);
  for i := 0 to fiTagCount-1 do
    if UpperCase(fiTagArray[i].Name) = ATagName then
    begin
      Result := fiTagArray[i].Data;
      Exit;
    end;
  Result := '';
end;

// This function returns the integer data value for a given tag name.
function TImageInfo.LookupTagInt(ATagName: String):integer;
var
  i: integer;
  x: Double;
  {$IFDEF FPC}
  fs: TFormatSettings;
  {$ELSE}
  res: Integer;
  {$ENDIF}
begin
  ATagName := UpperCase(ATagName);
  for i := 0 to fiTagCount-1 do
    if UpperCase(fiTagArray[i].Name) = ATagName then
    begin
      if not TryStrToInt(fiTagArray[i].Data, Result) then
      begin
        if TryStrToFloat(fiTagArray[i].Data, x) then
          Result := Round(x)
        else
        begin
         {$IFDEF FPC}
          fs := FormatSettings;
          if fs.DecimalSeparator = '.' then fs.DecimalSeparator := ',' else
             fs.DecimalSeparator := '.';
          if TryStrToFloat(fiTagArray[i].Data, x, fs) then
            Result := Round(x)
          else
            Result := -1;
         {$ELSE}
          val(fiTagArray[i].Data, x, res);
          if res = 0 then
            Result := Round(x)
          else
            Result := -1;
         {$ENDIF}
        end;
      end;
      Exit;
    end;
  Result := -1;
end;

//  This function returns the index of a tag in the tag buffer.
//  It searches by the description which is most likely to be used as a label
Function TImageInfo.LookupTagByDesc(ADesc: String):integer;
var
  i: integer;
begin
  ADesc := UpperCase(ADesc);
  for i := 0 to FITagCount-1 do
    if UpperCase(fiTagArray[i].Desc) = ADesc then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

//  This function returns the index of a tag definition for a given tag name.
function TImageInfo.LookupTagDefn(ATagName: String): integer;
var
  i: integer;
begin
  for i := 0 to ExifTagCnt-1 do
  begin
    if LowerCase(ATagName) = LowerCase(TagTable[i].Name) then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function TImageInfo.ExifDateToDateTime(ARawStr: ansistring): TDateTime;
type
  TConvert= packed record
     year: Array [1..4] of ansichar; f1:ansichar;
     mon:  Array [1..2] of ansichar; f2:ansichar;
     day:  Array [1..2] of ansichar; f3:ansichar;
     hr:   Array [1..2] of ansichar; f4:ansichar;
     min:  Array [1..2] of ansichar; f5:ansichar;
     sec:  Array [1..2] of ansichar;
  end;
  PConvert= ^TConvert;
var
  yr, mn, dy, h, m, s: Integer;
  d: TDateTime;
  t: TDateTime;
begin
  Result := 0;
  if Length(ARawStr) >= SizeOf(TConvert) then
    with PConvert(@ARawStr[1])^ do
      if TryStrToInt(year, yr) and
         TryStrToInt(mon, mn) and
         TryStrToInt(day, dy) and
         TryEncodeDate(yr, mn, dy, d)
      and
         TryStrToInt(hr, h) and
         TryStrToInt(min, m) and
         TryStrToInt(sec, s) and
         TryEncodeTime(h, m, s, 0, t)
      then
        Result := d + t;
end;


function TImageInfo.GetImgDateTime: TDateTime;
begin
  Result := GetDateTimeOriginal;
  if Result = 0 then
    Result := GetDateTimeDigitized;
  if Result = 0 then
    Result := GetDateTimeModified;
  if Result = 0 then
    Result := Parent.FileDatetime;
end;

function TImageInfo.GetDateTimeOriginal: TDateTime;
var
  t: TTagEntry;
begin
  Result := 0.0;
  t := TagByName['DateTimeOriginal'];
  if t.Tag <> 0 then
    Result := ExifDateToDateTime(t.Raw);
end;

procedure TImageInfo.SetDateTimeOriginal(const AValue: TDateTime);
var
  v: Variant;
begin
  v := FormatDateTime(ExifDateFormat, AValue);
  SetTagValue('DateTimeOriginal', v);
end;
  (*
var
  p: PTagEntry;
begin
  p := GetTagPtr([ttExif], TAG_EXIF_OFFSET, true, 0, FMT_ULONG{, true});
  if (AValue = 0) then begin
    RemoveTag([ttExif], TAG_DATETIME_ORIGINAL, TAG_EXIF_OFFSET);
    exit;
  end;
  p := GetTagPtr([ttExif], TAG_DATETIME_ORIGINAL, true, TAG_EXIF_OFFSET, FMT_STRING);
  p^.Raw := FormatDateTime(ExifDateFormat, AValue);
  p^.Data := p^.Raw;
  p^.Size := Length(p^.Raw);
end;
    *)
function TImageInfo.GetDateTimeDigitized: TDateTime;
var
  t: TTagEntry;
begin
  Result := 0.0;
  t := TagByName['DateTimeDigitized'];
  if t.Tag <> 0 then
    Result := ExifDateToDateTime(t.Raw);
end;

procedure TImageInfo.SetDateTimeDigitized(const AValue: TDateTime);
var
  v: Variant;
begin
  v := FormatDateTime(ExifDateFormat, AValue);
  SetTagValue('DateTimeDigitized', v);
end;
    (*
var
  p: PTagEntry;
begin
  p := GetTagPtr([ttExif], TAG_EXIF_OFFSET, true, 0, FMT_ULONG{, true});
  if (AValue = 0) then begin
    RemoveTag([ttExif], TAG_DATETIME_DIGITIZED, TAG_EXIF_OFFSET);
    exit;
  end;
  p := GetTagPtr([ttExif], TAG_DATETIME_DIGITIZED, true, TAG_EXIF_OFFSET, FMT_STRING);
  p^.Raw := FormatDateTime(ExifDateFormat, AValue);
  p^.Data := p^.Raw;
  p^.Size := Length(p^.Raw);
end;     *)

function TImageInfo.GetDateTimeModified: TDateTime;
var
  t: TTagEntry;
begin
  Result := 0.0;
  t := TagByName['DateTime'];
  if t.Tag <> 0 then
    Result := ExifDateToDateTime(t.Raw);
end;

procedure TImageInfo.SetDateTimeModified(const AValue: TDateTime);
var
  v: Variant;
begin
  v := FormatDateTime(ExifDateFormat, AValue);
  SetTagValue('DateTime', v);
end;
(*
var
  p: PTagEntry;
begin
  p := GetTagPtr([ttExif], TAG_DATETIME_MODIFY, true, 0, FMT_STRING);
  if AValue = 0 then begin
    RemoveTag([ttExif], TAG_DATETIME_MODIFY, 0);
    exit;
  end;
  p^.Raw := FormatDateTime(ExifDateFormat, AValue);
  p^.Data := p^.Raw;
  p^.Size := Length(p^.Raw);
end;
  *)
Procedure TImageInfo.AdjDateTime(ADays, AHours, AMins, ASecs: Integer);
var
  delta: double;
  dt: TDateTime;
begin
  //                hrs/day       min/day        sec/day
  delta := ADays + (AHours/24) + (AMins/1440) + (ASecs/86400);

  dt := GetDateTimeOriginal;
  if dt > 0 then SetDateTimeOriginal(dt + delta);

  dt := GetDateTimeDigitized;
  if dt > 0 then SetDateTimeDigitized(dt + delta);

  dt := GetDateTimeModified;
  if dt > 0 then SetDateTimeModified(dt + delta);
end;

function TImageInfo.AddTagToArray(ANewTag:iTag):integer;
begin
  if ANewTag.Tag <> 0 then     // Empty fields are masked out
  begin
    if fITagCount >= MaxTag-1 then
    begin
      inc(MaxTag, TagArrayGrowth);
      SetLength(fITagArray, MaxTag);
    end;
    fITagArray[fITagCount] := ANewTag;
    inc(fITagCount);
  end;
  result := fITagCount-1;
end;

function TImageInfo.AddTagToThumbArray(ANewTag: iTag): integer;
begin
  if ANewTag.Tag <> 0 then     // Empty fields are masked out
  begin
    if fIThumbCount >= MaxThumbTag-1 then
    begin
      inc(MaxThumbTag, TagArrayGrowth);
      SetLength(fIThumbArray, MaxThumbTag);
    end;
    fIThumbArray[fIThumbCount] := ANewTag;
    inc(fIThumbCount);
  end;
  result := fIThumbCount-1;
end;

function TImageInfo.CvtInt(ABuffer: ansistring): Longint;
var
  i: integer;
  r: Int64;
begin
  r := 0;
  if MotorolaOrder then
    for i := 1 to Length(ABuffer) do
      r := r*256 + ord(ABuffer[i])
  else
    for i := Length(ABuffer) downto 1 do
      r := r*256 + ord(ABuffer[i]);
  Result := LongInt(r);
end;

function TImageInfo.FormatNumber(ABuffer: ansistring; AFmt: integer;
  AFmtStr: String; ADecodeStr: String=''): String;
var
  buff2: ansistring;
  i, vlen: integer;
  tmp, tmp2: longint;
  dv: double;
begin
  Result := '';
  vlen := BYTES_PER_FORMAT[AFmt];
  if vlen = 0 then
    exit;

  for i := 0 to min((Length(ABuffer) div vlen), 128)-1 do
  begin
    if Result <> '' then
      Result := Result + dExifDataSep;  // Used for data display
    buff2 := copy(ABuffer, i*vlen + 1, vlen);
    case AFmt of
      FMT_SBYTE,
      FMT_BYTE,
      FMT_USHORT,
      FMT_ULONG,
      FMT_SSHORT,
      FMT_SLONG:
        begin
          tmp := CvtInt(buff2);
          if (ADecodeStr = '') or not dExifDecode then
            Result := Result + defIntFmt(tmp) // IntToStr(tmp)
          else
            Result := Result + DecodeField(ADecodeStr, IntToStr(tmp));
          end;
      FMT_URATIONAL,
      FMT_SRATIONAL:
        begin
          tmp := CvtInt(copy(buff2, 1, 4));
          tmp2 := CvtInt(copy(buff2, 5, 4));
          Result := Result + defFracFmt(tmp, tmp2);
          if (ADecodeStr <> '') or not dExifDecode then
            Result := Result + DecodeField(ADecodeStr, Result);
        end;
      FMT_SINGLE,
      FMT_DOUBLE:
        begin
          // not used anyway; not sure how to interpret endian issues
          Result := Result +  '-9999.99';
        end;
    else
      Result := Result + '?';
    end;
  end;

  if AFmtStr <> '' then
  begin
    if Pos('%s', AFmtStr) > 0 then
      Result := Format(AFmtStr, [Result], PointSeparator)
    else begin
      dv := GetNumber(ABuffer, AFmt);
      Result := Format(AFmtStr, [dv], PointSeparator);
    end;
  end;
end;

function TImageInfo.GetNumber(ABuffer: ansistring; AFmt:integer): Double;
var
  tmp: Longint;
  tmp2: Longint;
begin
  Result := 0;
  try
    case AFmt of
      FMT_SBYTE,
      FMT_BYTE,
      FMT_USHORT,
      FMT_ULONG,
      FMT_SSHORT,
      FMT_SLONG:
        Result := CvtInt(ABuffer);
      FMT_URATIONAL,
      FMT_SRATIONAL:
        begin
          tmp := CvtInt(copy(ABuffer,1,4));
          tmp2 := CvtInt(copy(ABuffer,5,4));
          Result := tmp / tmp2;
        end;
      FMT_SINGLE:
        Result := PSingle(@ABuffer[1])^;
      FMT_DOUBLE:
        Result := PDouble(@ABuffer[1])^;
    end;
  except
  end;
end;

var dirStack:ansistring = '';

procedure TImageInfo.clearDirStack;
begin
  dirStack := '';
end;

procedure TImageInfo.pushDirStack(dirStart, offsetbase:longint);
var
  ts: Ansistring;
begin
  ts := '['+AnsiString(IntToStr(offsetbase))+':'+AnsiString(IntToStr(dirStart))+']';
  dirStack := dirStack+ts;
end;

function TImageInfo.TestDirStack(dirStart, offsetbase: Longint): boolean;
var
  ts: Ansistring;
begin
  ts := '[' + AnsiString(IntToStr(offsetbase)) + ':' + AnsiString(IntToStr(dirStart))+']';
  result := Pos(ts,dirStack) > 0;
end;

//{$DEFINE CreateExifBufDebug}  // uncomment to see written Exif data
{$ifdef CreateExifBufDebug}var CreateExifBufDebug : String;{$endif}
                           (*
function TImageInfo.CreateExifBuf(ParentID:word=0; OffsetBase:integer=0): AnsiString;
  {offsetBase required, because the pointers of subIFD are referenced from parent IFD (WTF!!)}
  // msta Creates APP1 block with IFD0 only
var
  i, f, n: integer;
  size, pDat, p: Cardinal;
  head: ansistring;

  function Check (const t: TTagEntry; pid: word): Boolean; //inline;
  var
    i: integer;
  begin
    if (t.parentID <> pid) or (t.TType >= Length(BYTES_PER_FORMAT)) or
       (BYTES_PER_FORMAT[t.TType] = 0)
    then
      Result := false
    else begin
      Result := Length(whitelist) = 0;
      for i := 0 to Length(whitelist)-1 do if (whitelist[i] = t.Tag) then begin
        Result := true;
        break;
      end;
    end;
  end;

  function CalcSubIFDSize(pid : integer) : integer;
  var
    i: integer;
  begin
    Result := 6;
    for i := 0 to Length(fiTagArray)-1 do begin
      if (not check(fiTagArray[i], pid)) then continue;
      Result := Result + 12;
      if (fiTagArray[i].id <> 0) then
        Result := Result + calcSubIFDSize(fiTagArray[i].id)
      else
        if (Length(fiTagArray[i].Raw) > 4) then
          Result := Result + Length(fiTagArray[i].Raw);  // calc size
    end;
  end;

begin
  {$ifdef CreateExifBufDebug}
  if (parentID = 0) then CreateExifBufDebug := '';
  {$endif}

  if (parentID = 0) then
    head := #0#0                        // APP1 block size (calculated later)
          + 'Exif' + #$00+#$00                         // Exif Header
          + 'II' + #$2A+#$00 + #$08+#$00+#$00+#$00     // TIFF Header (Intel)
  else
    head := '';
  n := 0;
  size := 0;
//  for i := 0 to Length(fiTagArray)-1 do begin
  for i := 0 to fiTagCount-1 do begin
    if (not Check(fiTagArray[i], parentID)) then
      continue;
    n := n + 1; // calc number of Tags in current IFD
    if (fiTagArray[i].id <> 0) then
      size := size + CalcSubIFDSize(fiTagArray[i].id)
    else
    if (Length(fiTagArray[i].Raw) > 4) then
      size := size + Length(fiTagArray[i].Raw);  // calc size
  end;
  pDat := Length(head) + 2 + n*12 + 4; // position of data area
  p := pDat;
  size := size + pDat;
  SetLength(Result, size);
  if (parentID = 0) then begin
    head[1] := ansichar(size div 256);
    head[2] := ansichar(size mod 256);
    move(head[1], Result[1], Length(head));            // write header
  end;
  PWord(@Result[1+Length(head)])^ := n;                // write tag count
  PCardinal(@Result[1+Length(head)+2+12*n])^ := 0;     // write offset to next IFD (0, because just IFD0 is included)
  n := 0;
  for f := 0 to 1 do for i := 0 to Length(fiTagArray)-1 do begin          // write tags
  if (not check(fiTagArray[i], parentID)) then continue;
    if (f = 0) and (fiTagArray[i].Tag <> TAG_EXIF_OFFSET) then
      continue; // Sub-IFD must be first data block... more or less (WTF)
    if (f = 1) and (fiTagArray[i].Tag = TAG_EXIF_OFFSET) then
      continue;
    PWord(@Result[1+Length(head)+2+12*n+0])^ := fiTagArray[i].Tag;
    if (fiTagArray[i].Tag = TAG_EXIF_OFFSET) then begin
      PWord(@Result[1+Length(head)+2+12*n+2])^ := 4;  // Exif-Pointer is not a real data block but really a pointer (WTF)
      PCardinal(@Result[1+Length(head)+2+12*n+4])^ := 1;
    end
    else begin
      PWord(@Result[1+Length(head)+2+12*n+2])^ := fiTagArray[i].TType;
      PCardinal(@Result[1+Length(head)+2+12*n+4])^ := Length(fiTagArray[i].Raw) div BYTES_PER_FORMAT[fiTagArray[i].TType];
    end;
    {$ifdef CreateExifBufDebug}CreateExifBufDebug := CreateExifBufDebug + '  ' + fiTagArray[i].Name;{$endif}
    if (Length(fiTagArray[i].Raw) <= 4) and (fiTagArray[i].id = 0) then begin
      PCardinal(@Result[1+Length(head)+2+12*n+8])^ := 0;
      if (Length(fiTagArray[i].Raw) > 0) then
        move(fiTagArray[i].Raw[1], Result[1+Length(head)+2+12*n+8], Length(fiTagArray[i].Raw));
    end
    else begin
      PCardinal(@Result[1+Length(head)+2+12*n+8])^ := p - 8 + offsetBase;
      if (fiTagArray[i].id <> 0) then begin
        {$ifdef CreateExifBufDebug}CreateExifBufDebug := CreateExifBufDebug + ' { ';{$endif}
        fiTagArray[i].Raw := CreateExifBuf(fiTagArray[i].id, p); // create sub IFD
        fiTagArray[i].Size := Length(fiTagArray[i].Raw);
        {$ifdef CreateExifBufDebug}CreateExifBufDebug := CreateExifBufDebug + ' } ';{$endif}
      end;
      move(fiTagArray[i].Raw[1], Result[1+p], Length(fiTagArray[i].Raw));
      p := p + Length(fiTagArray[i].Raw);
    end;
    n := n+1;
  end;
  {$ifdef CreateExifBufDebug}if (parentID = 0) then ShowMessage(CreateExifBufDebug);{$endif}
end;                         *)

//--------------------------------------------------------------------------
// Process one of the nested EXIF directories.
//--------------------------------------------------------------------------
procedure  TImageInfo.ProcessExifDir(DirStart, OffsetBase, ExifLength: longint;
  ATagType: TTagType = ttExif; APrefix: string=''; AParentID: word = 0);
var
  byteCount: integer;
  tag, tagFormat, tagComponents: integer;
  de, dirEntry, offsetVal, numDirEntries, valuePtr, subDirStart: Longint;
  value: Integer;
  rawStr, fStr, transStr: ansistring;
  msInfo: TMsInfo;
  lookupEntry, newEntry: TTagEntry;
  tmpTR: ansistring;
  tagID: word;

  i: Integer;
begin
  PushDirStack(DirStart, OffsetBase);
  numDirEntries := Get16u(DirStart);
  if (ExifTrace > 0) then
    TraceStr := TraceStr + crlf +
      Format('Directory: Start, entries = %d, %d', [DirStart, numDirEntries]);
  if (DirStart + 2 + numDirEntries*12) > (DirStart + OffsetBase + ExifLength) then
  begin
    Parent.FErrStr := 'Illegally sized directory';
    exit;
  end;

  //  Uncomment to trace directory structure
  {
  Parent.ErrStr:=
    Format('%d,%d,%d,%d+%s', [DirStart, numDirEntries,OffsetBase,ExifLength, parent.ErrStr]);
  }

  if (ATagType = ttExif) and (FThumbStart = 0) and not TiffFmt then
  begin
    DirEntry := DirStart + 2 + 12*numDirEntries;
    FThumbStart := Get32u(DirEntry);
    FThumbLength := OffsetBase + ExifLength - FThumbStart;
  end;

  for de := 0 to numDirEntries-1 do
  begin
    tagID := 0;
    dirEntry := DirStart + 2 + 12*de;
    tag := Get16u(dirEntry);
    tagFormat := Get16u(dirEntry + 2);
    tagComponents := Get32u(dirEntry + 4);
    byteCount := tagComponents * BYTES_PER_FORMAT[tagFormat];
    if byteCount = 0 then
      Continue;
    if byteCount > 4 then
    begin
      offsetVal := Get32u(dirEntry+8);
      valuePtr := OffsetBase + offsetVal;
    end
    else
      valuePtr := dirEntry + 8;
    rawStr := Copy(parent.EXIFsegment^.Data, valuePtr, byteCount);

    fStr := '';
    if BuildList in [GenString, GenAll] then
    begin
      lookUpEntry := FetchTagDefByID(tag, ATagType);

      with lookUpEntry do
      begin
        case tagFormat of
          FMT_UNDEFINED:
            fStr := '"' + StrBefore(rawStr,#0) + '"';
          FMT_STRING:
            begin
              fStr := Copy(parent.EXIFsegment^.Data, valuePtr, byteCount);
              if fStr[byteCount] = #0 then
                Delete(fStr, byteCount, 1);
            end;
        else
          fStr := FormatNumber(rawStr, tagFormat, FormatS, Code);
        end;
        if (tag > 0) and Assigned(Callback) and dExifDecode then
          fStr := Callback(fStr)
        else
          fStr := MakePrintable(fStr);
        transStr := Desc;
      end;

      case tag of
        TAG_USERCOMMENT:
          // strip off comment header
          fStr := trim(Copy(rawStr, 9, byteCount-8));
        TAG_DATETIME_MODIFY,
        TAG_DATETIME_ORIGINAL,
        TAG_DATETIME_DIGITIZED:
          // Conversion from EXIF format (2009:01:02 12:10:12) to ISO (2009-01-02 12:10:12)
          fStr := FormatDateTime(ISODateFormat, ExifDateToDateTime(fStr));
      end;

      // Update trace strings
      tmpTR := crlf +
        siif(ExifTrace > 0, 'tag[$' + IntToHex(tag,4) + ']: ', '') +
        transStr + dExifDelim + fStr +
        siif(ExifTrace > 0, ' [size: ' + IntToStr(byteCount) + ']', '') +
        siif(ExifTrace > 0, ' [start: ' + IntToStr(valuePtr) + ']', '');

      if ATagType = ttThumb then
        Thumbtrace := ThumbTrace + tmpTR
      else
        TraceStr := TraceStr + tmpTR;
    end;

    //   Additional processing done here:
    case tag of
      TAG_SUBIFD_OFFSET,
      TAG_EXIF_OFFSET,
      TAG_INTEROP_OFFSET:
        begin
          try
            value := Get32u(valuePtr);
            subdirStart := OffsetBase + LongInt(value);
            // some mal-formed images have recursive references...
            // if (subDirStart <> DirStart) then
            if not TestDirStack(subDirStart, OffsetBase) then begin
              tagID := tag;
              ProcessExifDir(subdirStart, OffsetBase, ExifLength, ttExif, '', tagID);
            end;
          except
          end;
        end;
      TAG_GPS_OFFSET:
        begin
          try
            subdirStart := OffsetBase + LongInt(Get32u(ValuePtr));
            if not TestDirStack(subDirStart, OffsetBase) then begin
              tagID := tag;
              ProcessExifDir(subdirStart, OffsetBase, ExifLength, ttGps, '', tagID);
            end;
          except
          end;
        end;
      TAG_EXIFVERSION:
        ExifVersion := rawstr;
      TAG_MAKERNOTE:
        begin
          MakerNote := rawStr;
          MakerOffset := valuePtr;
          msInfo := TMsInfo.Create(TiffFmt, self);
          msAvailable := msInfo.ReadMSData(self);
          FreeAndNil(msInfo);
        end;
      TAG_FLASH:
        FlashUsed := round(getNumber(rawStr, tagFormat));
      (*
      TAG_IMAGELENGTH,
      TAG_EXIF_IMAGELENGTH:
        begin
          HPosn := DirEntry + 8;
          Height := round(GetNumber(rawStr, tagFormat));
        end;
      TAG_IMAGEWIDTH,
      TAG_EXIF_IMAGEWIDTH:
        begin
          WPosn := DirEntry + 8;
          Width := round(GetNumber(rawStr, tagFormat));
        end;
        *)
      TAG_THUMBSTARTOFFSET:
        FThumbnailStartOffset := Get32u(ValuePtr);
      TAG_THUMBSIZE:
        FThumbnailSize := Get32u(ValuePtr);
      TAG_COMPRESSION:
        if ATagType = ttThumb then
          FThumbType := round(GetNumber(RawStr, tagFormat));
    end;

    if BuildList in [GenList,GenAll] then
    begin
      try
        NewEntry := LookupEntry;
        NewEntry.Data := fStr;
        NewEntry.Raw := rawStr;
        NewEntry.Size := Length(rawStr);
        NewEntry.TType := tagFormat;
        NewEntry.Count := tagComponents;
        NewEntry.ParentID := AParentID;
        if ATagType = ttThumb then
          AddTagToThumbArray(newEntry)
        else
          AddTagToArray(newEntry);
      except
        // if we're here: unknown tag.
        // item is recorded in trace string
      end;
    end;
  end;
end;


procedure TImageInfo.ProcessHWSpecific(MakerBuff: ansistring;
  TagTbl: array of TTagEntry; DirStart: Longint; AMakerOffset: Longint;
  spOffset: Integer = 0);
var
  NumDirEntries: integer;
  de, ByteCount, tagID: integer;
  DirEntry, tag, tagFormat, tagComponents: integer;
  OffsetVal, ValuePtr: Longint;
  rawStr: ansistring;
  tagStr: String;
  fStr, fStr2, ds: ansistring;
  OffsetBase: longint;
  NewEntry: TTagEntry;
begin
  DirStart := DirStart+1;
  OffsetBase := DirStart - aMakerOffset + 1;
  SetDataBuff(MakerBuff);
  try
    NumDirEntries := Get16u(DirStart);
    for de := 0 to NumDirEntries-1 do
    begin
      DirEntry := DirStart+2+12*de;
      tag := Get16u(DirEntry);
      tagFormat := Get16u(DirEntry+2);
      tagComponents := Get32u(DirEntry+4);
      ByteCount := tagComponents * BYTES_PER_FORMAT[tagFormat];
      OffsetVal := 0;
      if ByteCount > 4 then
      begin
        OffsetVal := Get32u(DirEntry + 8);
        ValuePtr := OffsetBase + OffsetVal;
      end
      else
        ValuePtr := DirEntry + 8;

      // Adjustment needed by Olympus Cameras
      if ValuePtr + ByteCount > Length(MakerBuff) then
        rawStr := Copy(parent.DataBuff, OffsetVal + spOffset, ByteCount)
      else
        rawStr := copy(MakerBuff, ValuePtr, ByteCount);

      tagID := LookupMTagID(tag, TagTbl);
      if tagID < 0 then
        tagStr := 'Unknown'
      else
        tagStr := TagTbl[tagID].Desc;

      fstr := '';
      if UpperCase(tagStr) = 'SKIP' then
        continue;

      if BuildList in [GenList, GenAll] then
      begin
        case tagFormat of
          FMT_STRING:
            fStr := '"' + StrBefore(rawStr, #0) + '"';
          FMT_UNDEFINED:
            fStr := '"' + rawStr + '"';
          else
            try
              ds := siif(dEXIFdecode, LookupCode(tag, TagTbl), '');
              if tagID < 0 then
                fStr := FormatNumber(rawStr, tagFormat, '', '')
              else
                fStr := FormatNumber(rawStr, tagFormat, TagTbl[tagID].FormatS, ds);
            except
              fStr := '"' + rawStr + '"';
            end;
        end;

        rawDefered := false;
        if (tagID > 0) and Assigned(TagTbl[tagID].CallBack) and dExifDecode then
          fstr2 := TagTbl[tagID].CallBack(fstr)
        else
          fstr2 := MakePrintable(fstr);

        if (ExifTrace > 0) then
        begin
          if not rawDefered then
            msTraceStr := msTraceStr + crlf +
              'tag[$' + IntToHex(tag, 4) + ']: ' +
              TagStr + dExifDelim + fstr2 +
              ' [size: '  + IntToStr(ByteCount) + ']' +
              ' [raw: '   + MakeHex(RawStr) + ']' +
              ' [start: ' + IntToStr(ValuePtr) + ']'
          else
            msTraceStr := msTraceStr + crlf +
              'tag[$' + IntToHex(tag, 4) + ']: '+
              TagStr + dExifDelim +
              ' [size: ' + IntToStr(ByteCount) + ']' +
              ' [raw: '  + MakeHex(RawStr) + ']' +
              ' [start: '+ IntToStr(ValuePtr) + ']' +
              fstr2;
        end else
        begin
          if not rawDefered then
            msTraceStr := msTraceStr + crlf +
                          tagStr + dExifDelim + fstr2
          else
            msTraceStr := msTraceStr +
                          fstr2 + // has cr/lf as first element
                          crlf + TagStr + dExifDelim + fstr;
        end;
      end;

      if (BuildList in [GenList, GenAll]) and (tagID > 0) then
      begin
        try
          NewEntry := TagTbl[tagID];

          if rawdefered then
            NewEntry.Data := fStr
          else
            NewEntry.Data := fStr2;

          NewEntry.Raw   := rawStr;
          NewEntry.TType := tagFormat;
          NewEntry.Count := tagComponents;
          NewEntry.TID   := 1; // MsSpecific

          AddTagToArray(NewEntry);
        except
          // if we're here: unknown tag.
          // item is recorded in trace string
        end;
      end;

    end;

  except
     on E: Exception do
       Parent.FErrStr := 'Error detected: ' + E.Message;
  end;

   SetDataBuff(parent.DataBuff);
end;

procedure TImageInfo.AddMSTag(fname: String; ARawStr: ansistring; fType: word);
var
  newEntry: TTagEntry;
begin
  if BuildList in [GenList,GenAll] then
  begin
    try
      newEntry.Name := fname;
      newEntry.Desc := fname;
      newEntry.Data := ARawStr;
      newEntry.Raw  := ARawStr;
      newEntry.Size := Length(ARawStr);
      NewEntry.TType:= fType;
      NewEntry.Count := 1;
      newEntry.parentID := 0;
      newEntry.TID  := 1; // MsSpecific
      AddTagToArray(newEntry);
    except
      // if we're here: unknown tag.
      // item is recorded in trace string
    end;
  end;
end;

{ Creates a thumbnail image from the main image loaded. The size of the thumbnail
  (width or height whichever is longer) is specified as AThumbnailSize.
  The current thumbnail image is replaced by the new one, or, if the image did
  not have a thumbnail image so far it is added to the image. }
procedure TImageInfo.CreateThumbnail(AThumbnailSize: Integer = DEFAULT_THUMBNAIL_SIZE);
var
  srcStream, destStream: TMemoryStream;
begin
  srcStream := TMemoryStream.Create;
  destStream := TMemoryStream.Create;
  try
    srcStream.LoadFromFile(Parent.FileName);
    JpegScaleImage(srcStream, destStream, AThumbnailSize);
    destStream.Position := 0;
    LoadThumbnailFromStream(destStream);
  finally
    destStream.Free;
    srcStream.Free;
  end;
end;

function TImageInfo.HasThumbnail: boolean;
begin
  Result := Length(FThumbnailBuffer) > 0;
end;

procedure TImageInfo.ProcessThumbnail;
var
  start: Integer;
begin
  FiThumbCount := 0;
  start := FThumbStart + 9;
  ProcessExifDir(start, 9, FThumbLength - 12, ttThumb, 'Thumbnail', 1);
    ExtractThumbnail;
end;

procedure TImageInfo.ExtractThumbnail;
begin
  if FThumbnailStartOffset > 0 then begin
    SetLength(FThumbnailBuffer, FThumbnailSize);
    Move(Parent.ExifSegment^.Data[FThumbnailStartOffset + 9], FThumbnailBuffer[0], FThumbnailSize);
  end else
    FThumbnailBuffer := nil;
end;

procedure TImageInfo.LoadThumbnailFromStream(AStream: TStream);
var
  n: Integer;
  w, h: Integer;
begin
  RemoveThumbnail;

  // Check whether the image is a jpeg, and extract size of the thrumbnail image
  if not JPEGImageSize(AStream, w, h) then
    exit;

  // Write the image from the stream into the thumbnail buffer
  n := AStream.Size;
  if n > 65000 then  // limit probably still too high, thumbnail must fit into a 64k segment along with all other tags...
    raise Exception.Create('Thumbnail too large.');

  SetLength(FThumbnailBuffer, n);
  if AStream.Read(FThumbnailBuffer[0], n) < n then
    raise Exception.Create('Could not read thumbnail image.');

  // Make sure that the IFD1 tags for the thumbnail are correct
  SetThumbTagValue('Compression', 6);     // 6 = JPEG - this was checked above.
  SetThumbTagValue('ImageWidth', w);
  SetThumbTagValue('ImageLength', h);
  SetThumbTagValue('JPEGInterchangeFormat', 0);  // to be replaced by the offset to the thumbnail
  SetThumbTagValue('JPEGInterchangeFormatLength', n);
end;

procedure TImageInfo.RemoveThumbnail;
var
  newSize: integer;
begin
  SetLength(FThumbnailBuffer, 0);
  fiThumbCount := 0;

  if FThumbStart > 1 then begin
    newSize := FThumbStart - 6;
    with parent do
    begin
      SetLength(ExifSegment^.Data, newSize);
      ExifSegment^.Size := newSize;
      // size calculations should really be moved to save routine
      ExifSegment^.data[1] := ansichar(newSize div 256);
      ExifSegment^.data[2] := ansichar(newSize mod 256);
    end;

    FThumbStart := 0;
  end;
end;

procedure TImageInfo.SaveThumbnailToStream(AStream: TStream);
var
  n: Int64;
begin
  if HasThumbnail then
  begin
    n := Length(FThumbnailBuffer);
    if  AStream.Write(FThumbnailBuffer[0], n) <> n then
      raise Exception.Create('Cannot write Thumbnail image to stream.');
  end;
end;

function TImageInfo.ToLongString(ALabelWidth: Integer = 15): String;
var
  tmpStr: String;
  FileDateTime: String;
  L: TStringList;
  W: Integer;
begin
  W := ALabelWidth;
  L := TStringList.Create;
  try
    (*
    if parent.ExifSegment = nil then
      Result := ''
    else
    *)
    if Parent.ErrStr <> '<none>' then
    begin
      L.Add(Format('File Name:     %s', [ExtractFileName(parent.Filename)]));
      L.Add(Format('Exif Error:    %s', [Parent.ErrStr]));
      Result := L.Text;
    end else
    begin
      DateTimeToString(FileDateTime, ISODateFormat, Parent.FileDateTime);

      L.Add(Format('%-*s %s',      [w, 'File name:', ExtractFileName(Parent.Filename)]));
      L.Add(Format('%-*s %dkB',    [w, 'File size:', Parent.FileSize div 1024]));
      L.Add(Format('%-*s %s',      [w, 'File date:', FileDateTime]));
      L.Add(Format('%-*s %s',      [w, 'Photo date:', FormatDateTime(IsoDateFormat, GetImgDateTime)]));
      L.Add(Format('%-*s %s (%s)', [w, 'Make (model):', CameraMake, CameraModel]));
      L.Add(Format('%-*s %d x %d', [w, 'Dimensions:', Width, Height]));

      if BuildList in [GenString,GenAll] then
      begin
        tmpStr := LookupTagVal('ExposureTime');
        if tmpStr <> '' then
          L.Add(Format('%-*s %s', [w, 'Exposure time:', tmpStr]))
        else
        begin
          tmpStr := LookupTagVal('ShutterSpeedValue');
          if tmpStr <> '' then
            L.Add(Format('%-*s %s', [w, 'Exposure time:', tmpStr]));
        end;

        tmpStr := LookupTagVal('FocalLength');
        if tmpStr <> '' then
          L.Add(Format('%-*s %s', [w, 'Focal length:', tmpStr]));

        tmpStr := LookupTagVal('FocalLengthIn35mm');
        if tmpStr <> '' then
          L.Add(Format('%-*s %s', [w, 'Focal length (35mm):', tmpStr]));

        tmpStr := LookupTagVal('FNumber');
        if tmpStr <> '' then
          L.Add(Format('%-*s %s', [w, 'F number', tmpStr]));

        tmpStr := LookupTagVal('ISOSpeedRatings');
        if tmpStr <> '' then
          L.Add(Format('%-*s %s', [w, 'ISO:', tmpStr]));
      end;
      L.Add(Format('%-*s %s', [w, 'Flash fired:', siif(odd(FlashUsed),'Yes','No')]));
      Result := L.Text;
    end;
  finally
    L.Free;
  end;
end;

function TImageInfo.ToShortString: String;
begin
  {
  if parent.ExifSegment = nil then
    Result := ''
  else
  }
  if Parent.ErrStr <> '<none>' then
    Result := ExtractFileName(parent.Filename) + ' Exif Error: '+Parent.ErrStr
  else
    Result := ExtractFileName(parent.Filename) + ' ' +
              IntToStr(parent.FileSize div 1024) + 'kB '+
              FormatDateTime(IsoDateFormat, GetImgDateTime) + ' ' +
              IntToStr(Width) + 'w ' + IntToStr(Height) + 'h '+
              siif(odd(FlashUsed),' Flash', '');
end;

procedure TImageInfo.AdjExifSize(AHeight, AWidth: Integer);
begin
  TagValue['ImageWidth'] :=  AWidth;
  TagValue['ImageLength'] := AHeight;
end;


//------------------------------------------------------------------------------
//                            TImgData
//------------------------------------------------------------------------------

constructor TImgData.Create(buildCode: integer = GenAll);
begin
  inherited create;
  buildList := BuildCode;
  Reset;
end;

destructor TImgdata.Destroy;
begin
  ExifObj.Free;
  IptcObj.Free;
  inherited;
end;

procedure TImageInfo.InternalGetBinaryTagValue(ATag: TTagEntry;
  var ABuffer: ansistring);
begin
  ABuffer := '';

  if ATag.Tag = 0 then
    exit;

  if ATag.TType = FMT_BINARY then begin
    SetLength(ABuffer, Length(ATag.Raw));
    Move(ATag.Raw[1], ABuffer[1], Length(ATag.Raw));
  end;
end;

function TImageInfo.InternalGetTagValue(ATag: TTagEntry): Variant;
var
  s: String;
  r: TExifRational;
  i: Integer;
  intValue: Integer;
  floatValue: Extended;
begin
  Result := Null;
  if ATag.Tag = 0 then
    exit;

  // Handle strings
  case ATag.TType of
    FMT_STRING:
      begin
       {$IFDEF FPC}
        {$IFDEF FPC3+}
        s := ATag.Raw;
        {$ELSE}
        s := AnsiToUTF8(ATag.Raw);
        {$ENDIF}
       {$ELSE}
        s := ATag.Raw;
       {$ENDIF}
        while s[Length(s)] = #0 do
          Delete(s, Length(s), 1);
        Result := s;
        exit;
      end;
    FMT_BINARY:
      begin
        Result := '<binary>';
        exit;
      end;
  end;

  // Handle numeric data. Be aware that they may be arrays
  if ATag.Count = 1 then
//    Result := NumericTagToInt(@ATag.Raw[1], ATag.TType)
    Result := NumericTagToVar(@ATag.Raw[1], ATag.TType)
  else begin
    case ATag.TType of
      FMT_BYTE, FMT_USHORT, FMT_ULONG:
        Result := VarArrayCreate([0, ATag.Count-1], varInteger);
      FMT_URATIONAL, FMT_SRATIONAL:
        Result := VarArrayCreate([0, ATag.Count-1], varDouble);
    end;
    for i:=0 to ATag.Count-1 do
      Result[i] := NumericTagToVar(@ATag.Raw[1 + BYTES_PER_FORMAT[ATag.TType]*i], ATag.TType);
  end;

  // Correction for some special cases
  case ATag.Tag of
    TAG_SHUTTERSPEED:
      // Is stored as -log2 of exposure time
      Result := power(2.0, -Result);
  end;
end;

{ ABuffer points into the raw buffer of a tag. The number pointed to will be
  converted to a numeric value; its type depends on ATagType. }
function TImageInfo.NumericTagToVar(ABuffer: Pointer; ATagType: Integer): Variant;
var
  r: TExifRational;
begin
  case ATagType of
    FMT_BYTE:
      Result := PByte(ABuffer)^;
    FMT_USHORT:
      if MotorolaOrder then
        Result := BEToN(PWord(ABuffer)^) else
        Result := LEToN(PWord(ABuffer)^);
    FMT_ULONG:
      if MotorolaOrder then
        Result := BEToN(PDWord(ABuffer)^) else
        Result := LEToN(PDWord(ABuffer)^);
    FMT_URATIONAL,
    FMT_SRATIONAL:
      begin
        r := PExifRational(ABuffer)^;
        if MotorolaOrder then begin
          r.Numerator := LongInt(BEToN(DWord(r.Numerator)));       // Type cast needed for D7
          r.Denominator := LongInt(BEToN(DWord(r.Denominator)));
        end else begin
          r.Numerator := LongInt(LEToN(DWord(r.Numerator)));
          r.Denominator := LongInt(LEtoN(DWord(r.Denominator)));
        end;
        if ATagType = FMT_SRATIONAL then begin
          r.Numerator := LongInt(r.Numerator);
          r.Denominator := LongInt(r.Denominator);
        end;
        Result := Extended(r.Numerator / r.Denominator);
      end;
    {
    FMT_BINARY:
      if ATag.Size = 1 then
        Result := PByte(@ATag.Raw[1])^
      else
        Result := '<binary>';
    }
    else
      raise Exception.CreateFmt('NumericTagToVar does not handle Tag type %d', [ord(ATagType)]);
  end;
end;

{ Central routine for writing data to a tag.
  ATagName ........... Name of the tag
  AValue ............. Value to be written to the tag if the tag is not binary
  ABinaryData ........ Data to be written to the tag if it is binary
  ABinaryDataCount ... Number of bytes to be written to a binary tag.
  ATagTypes .......... Determines in which list the tag definition is found
                       (Exif&Thumb, or GPS), and which list will get the new tag
                       (Exif&GPS, or thumb }
procedure TImageInfo.InternalSetTagValue(ATagName: String; AValue: Variant;
  ATagTypes: TTagTypes; ABinaryData: Pointer = nil; ABinaryDataCount: Word = 0);
const
  IGNORE_PARENT = $FFFF;
var
  P: PTagEntry;
  tagDef: PTagEntry;
  tagID: Word;
  parentID: Word;
  strValue: String;
  i: Integer;
begin
  // Find the tag's ID from the lists of tag definitions.
  // Note: Normal ("Exif") and thumbnail tags share the same list, gps tags
  // are separate.
  if (ATagTypes * [ttExif, ttThumb] <> []) then
    tagDef := FindExifTagDefByName(ATagName) else
    tagDef := nil;
  if (tagDef = nil) and (ttGps in ATagTypes) then
    tagDef := FindGpsTagDefByName(ATagName);
  if tagDef = nil then
    raise Exception.CreateFmt('Tag "%s" not found.', [ATagName]);
  tagID := tagDef.Tag;

  // Delete this tag if the provided value is varNull or varEmpty
  if tagDef.TType = FMT_BINARY then begin
    if ABinaryData = nil then begin
      RemoveTag(ATagTypes, tagID);
      exit;
    end;
  end else begin
    if VarIsNull(AValue) or VarIsEmpty(AValue) then begin
      RemoveTag(ATagTypes, tagID);
      exit;
    end;
  end;

  // Find the pointer to the tag
  P := FindTagPtr(tagDef^, (ttThumb in ATagTypes));
//  P := GetTagPtr(ATagTypes, tagID, false, IGNORE_PARENT);
  if P = nil then begin
    // The tag does not yet exist --> create a new one.
    // BUT: The TagTable does not show the ParentIDs...
    // Until somebody updates this we put the new tag into the root directory
    // (IFD0). Since this may not be allowed there's a risk that the EXIF in the
    // modified file cannot be read correctly...
    parentID := 0;
//    P := GetTagPtr(ATagTypes, tagID, true, parentID, tagDef.TType);
    P := CreateTagPtr(tagDef^, (ttThumb in ATagTypes), parentID);
  end;
  if P = nil then
    raise Exception.CreateFmt('Failure to create tag "%s"', [ATagName]);

  // Handle string data
  if P^.TType = FMT_STRING then begin
    strValue := VarToStr(AValue);
    {$IFDEF FPC}
    P^.Raw := UTF8ToAnsi(strValue) + #0;
    {$ELSE}
    P^.Raw := AnsiString(strValue) + #0;
    {$ENDIF}
    p^.Size := Length(p^.Raw);
    P^.Data := P^.Raw;
    exit;
  end;

  // Handle binary data
  if P^.TType = FMT_BINARY then begin
    SetLength(P^.Raw, ABinaryDataCount);
    Move(ABinaryData^, P^.Raw[1], ABinaryDataCount);
    P^.Size := ABinaryDataCount;
    P^.Data := '<binary>';
    exit;
  end;

  // NOTE: Since hardware-specific data are not yet decoded the element Raw
  // is still in the endianness of the source!

  // Handle some special cases
  case tagID of
    TAG_SHUTTERSPEED:
      begin
        strValue := VarToStr(AValue);
        if pos('/', strValue) > 0 then
          AValue := CvtRational(ansistring(strValue));
        // The shutter speed value is stored as -log2 of exposure time
        AValue := -log2(AValue);
      end;
    TAG_EXPOSURETIME:
      begin
        strValue := VarToStr(AValue);
        if pos('/', strValue) > 0 then
          AValue := CvtRational(ansistring(strValue));
      end;
  end;

  p^.Raw := '';
  p^.Data := '';
  p^.Size := 0;
  if VarIsArray(AValue) then
    for i:=VarArrayLowBound(AValue, 1) to VarArrayHighBound(AValue, 1) do
      VarToNumericTag(AValue[i], p)
  else
    VarToNumericTag(AValue, p);
end;

procedure TImageInfo.VarToNumericTag(AValue:variant; ATag: PTagEntry);
var
  intValue: Integer;
  fracvalue: TExifRational;
  len: Integer;
  s: String;
  w: Word;
  dw: DWord;
  ok: Boolean;
begin
  if VarIsArray(AValue) then
    raise Exception.Create('No variant arrays allowed in VarToTag');

  // fractional data
  if (ATag^.TType in [FMT_URATIONAL, FMT_SRATIONAL]) then
  begin
    fracvalue := DoubleToRational(AValue);
    if MotorolaOrder then begin
      fracvalue.Numerator := LongInt(NToBE(DWord(fracValue.Numerator)));       // Type-cast needed for D7
      fracValue.Denominator := LongInt(NToBE(DWord(fracValue.Denominator)));
    end else begin
      fracValue.Numerator := LongInt(NtoLE(DWord(fracValue.Numerator)));
      fracValue.Denominator := LongInt(NtoLE(DWord(fracValue.Denominator)));
    end;
    len := Length(ATag^.Raw);
    SetLength(ATag^.Raw, len + 8);
    Move(fracValue, ATag^.Raw[len + 1], 8);
    ATag^.Size := Length(ATag^.Raw);
    s := FormatNumber(ATag^.Raw, ATag^.TType, ATag^.FormatS, ATag^.Code);
    ATag^.Data := s; //siif(len = 0, s, ATag^.Data + dExifDataSep + s);
    exit;
  end;

  // integer data
  if VarIsType(AValue, vtInteger) then begin
    case ATag^.TType of
      FMT_BYTE   : ok := (AValue >= 0) and (AValue <= 255);
      FMT_USHORT : ok := (AValue >= 0) and (AValue <= Word($FFFF));
      FMT_ULONG  : ok := (AValue >= 0) and (AValue <= DWord($FFFFFFFF));
      FMT_SBYTE  : ok := (AValue >= -128) and (AValue <= 127);
      FMT_SSHORT : ok := (AValue >= -32768) and (AValue <= 32767);
      FMT_SLONG  : ok := (AValue >= -2147483647) and (AValue <= 2147483647);
        { NOTE: D7 does not run with the correct lower limit -2147483648 }
    end;
    if not ok then
      raise Exception.CreateFmt('Tag "%s": Value "%s" is out of range.', [ATag^.Name, VarToStr(AValue)]);
  end;

  if not TryStrToInt(VarToStr(AValue), intValue) then begin
    intValue := GetTagCode(ATag^, VarToStr(AValue));
    if (intValue = -1) then
      raise Exception.CreateFmt('Lookup value "%s" of tag "%s" not found', [VarToStr(AValue), ATag^.Name]);
  end;

  len := Length(ATag^.Raw);
  SetLength(ATag^.Raw, len + BYTES_PER_FORMAT[ATag^.TType]);
  case ATag^.TType of
    FMT_BYTE:
      Move(intValue, ATag^.Raw[1+len], 1);
    FMT_USHORT:
      begin
        if MotorolaOrder then w := NtoBE(word(intValue)) else w := NtoLE(word(intvalue));
        Move(w, ATag^.Raw[1+len], 2);
      end;
    FMT_ULONG:
      begin
        if MotorolaOrder then
          dw := NtoBE(DWord(intValue)) else
          dw := NtoLE(DWord(intValue));
        Move(dw, ATag^.Raw[1+len], 4);
      end;
    else
      raise Exception.Create('Unhandled data format in VarToNumericTag');
  end;
  ATag^.Size := Length(ATag^.Raw);
  s := FormatNumber(ATag^.Raw, ATag^.TType, ATag^.FormatS, ATag^.Code);
  ATag^.Data := siif(len = 0, s, ATag^.Data + dExifDataSep + s);
end;

function TImageInfo.GetTagByID(ATagID: Word): TTagEntry;
var
  i: Integer;
begin
  for i:= 0 to fiTagCount - 1 do
    if fiTagArray[i].Tag = ATagID then begin
      Result := fiTagArray[i];
      exit;
    end;
  Result := EmptyEntry;
end;

procedure TImageInfo.SetTagByID(ATagID: Word; const AValue: TTagEntry);
var
  i: Integer;
  P: PTagEntry;
begin
  for i:=0 to fiTagCount-1 do
    if fITagArray[i].Tag = ATagID then begin
      fITagArray[i] := AValue;
      exit;
    end;

  // If not found: add it as a new tag to the array
  P := FindExifTagDefByID(ATagID);
  if P = nil then begin
    P := FindGpsTagDefByID(ATagID);
    if P = nil then
      raise Exception.CreateFmt('TagID $%.4x unknown.', [ATagID]);
  end;
  AddTagToArray(AValue);
end;

function TImageInfo.GetTagByIndex(AIndex: Integer): TTagEntry;
begin
  Result := fiTagArray[AIndex];
end;

procedure TImageInfo.SetTagByIndex(AIndex: Integer; const AValue: TTagEntry);
begin
  FITagArray[AIndex] := AValue;
end;


function TImageInfo.GetTagByName(ATagName: String): TTagEntry;
var
  i: integer;
begin
  i := LookupTag(ATagName);
  if i >= 0 then
    Result := fITagArray[i]
  else
    Result := EmptyEntry;
end;

procedure TImageInfo.SetTagByName(ATagName: String; const AValue: TTagEntry);
var
  i: integer;
  P: PTagEntry;
begin
  i := LookupTag(ATagName);
  if i >= 0 then
    fITagArray[i] := AValue
  else
  begin
    // If not found: add it as a new tag to the array
    P := FindExifTagDefByName(ATagName);
    if P = nil then begin
      P := FindGpsTagDefByName(ATagName);
      if P = nil then
        raise Exception.Create('Tag "' + ATagName + '" unknown.');
    end;
    AddTagToArray(AValue);
  end;
end;

function TImageInfo.GetTagValue(ATagName: String): Variant;
var
  tag: TTagEntry;
begin
  Result := Null;
  tag := GetTagByName(ATagName);
  if tag.Tag = 0 then
    exit;
  Result := InternalGetTagValue(tag);
end;

procedure TImageInfo.SetTagValue(ATagName: String; AValue: Variant);
begin
  InternalSetTagValue(ATagName, AValue, [ttExif, ttGps]);
end;

function TImageInfo.GetTagValueAsString(ATagName: String): String;
var
  tag: TTagEntry;
  s: String;
begin
  Result := '';

  tag := GetTagByName(ATagName);
  if tag.Tag = 0 then
    exit;

  if tag.TType = FMT_STRING then
  begin
   {$IFDEF FPC}
    {$IFDEF FPC3+}
    s := tag.Raw;
    {$ELSE}
    s := AnsiToUTF8(tag.Raw);
    {$ENDIF}
   {$ELSE}
    s := tag.Raw;
   {$ENDIF}
    while (s <> '') and (s[Length(s)] = #0) do
      Delete(s, Length(s), 1);
    Result := s;
  end
  else
    Result := FormatNumber(tag.Raw, tag.TType, tag.FormatS, tag.Code);
end;

procedure TImageInfo.SetTagValueAsString(ATagName: String; AValue: String);
var
  v: Variant;
begin
  v := AValue;
  SetTagValue(ATagName, v);
end;

function TImageInfo.GetThumbTagByID(ATagID: Word): TTagEntry;
var
  i: Integer;
begin
  for i:= 0 to fiThumbCount - 1 do
    if fiThumbArray[i].Tag = ATagID then begin
      Result := fiThumbArray[i];
      exit;
    end;
  Result := EmptyEntry;
end;

procedure TImageInfo.SetThumbTagByID(ATagID: Word; const AValue: TTagEntry);
var
  i: Integer;
  P: PTagEntry;
begin
  for i:=0 to fiThumbCount-1 do
    if fIThumbArray[i].Tag = ATagID then begin
      fIThumbArray[i] := AValue;
      exit;
    end;
  {
  // If not found: add it as a new tag to the array
  P := FindExifTagDefByID(ATagID);   // Thumb tags are stored in Exif table
  if P = nil then
    raise Exception.CreateFmt('TagID $%.4x unknown.', [ATagID]);
  AddTagToThumbArray(AValue);
  }
end;

function TImageInfo.GetThumbTagByIndex(AIndex: Integer): TTagEntry;
begin
  Result := fiThumbArray[AIndex];
end;

procedure TImageInfo.SetThumbTagByIndex(AIndex: Integer; const AValue: TTagEntry);
begin
  fiThumbArray[AIndex] := AValue;
end;

function TImageInfo.GetThumbTagByName(ATagName: String): TTagEntry;
var
  i: integer;
begin
  ATagName := Uppercase(ATagName);
  for i:= 0 to fiThumbCount - 1 do
    if Uppercase(fiThumbArray[i].Name) = ATagName then begin
      Result := fiThumbArray[i];
      exit;
    end;
  Result := EmptyEntry;
end;

procedure TImageInfo.SetThumbTagByName(ATagName: String; const AValue: TTagEntry);
var
  i: Integer;
  P: PTagEntry;
begin
  ATagName := Uppercase(ATagName);
  for i:=0 to fiThumbCount-1 do
    if Uppercase(fIThumbArray[i].Name) = ATagName then begin
      fIThumbArray[i] := AValue;
      exit;
    end;
  {
  // If not found: add it as a new tag to the array
  P := FindExifTagDefByName(ATagName);   // Thumb tags are stored in Exif table
  if P = nil then
    raise Exception.Create('Tag "' + ATagName + '" unknown.');
  AddTagToThumbArray(AValue);
  }
end;

function TImageInfo.GetThumbTagValue(ATagName: String): Variant;
var
  tag: TTagEntry;
begin
  tag := GetThumbTagByName(ATagName);
  Result := InternalGetTagValue(tag);
end;

procedure TImageInfo.SetThumbTagValue(ATagName: String; AValue: Variant);
begin
  InternalSetTagValue(ATagName, AValue, [ttThumb]);
end;

function TImageInfo.GetWidth: Integer;
var
  v: Variant;
begin
  Result := 0;
  v := TagValue['ImageWidth'];
  if VarIsNull(v) then begin
    v := TagValue['ExifImageWidth'];
    if VarIsNull(v) then
      exit;
  end;
  Result := v;
end;

procedure TImageInfo.SetWidth(AValue: Integer);
begin
  TagValue['ImageWidth'] := AValue;
end;

function TImageInfo.GetHeight: Integer;
var
  v: Variant;
begin
  Result := 0;
  v := TagValue['ImageLength'];
  if VarIsNull(v) then begin
    v := TagValue['ExifImageLength'];
    if VarIsNull(v) then
      exit;
  end;
  Result := v;
end;

procedure TImageInfo.SetHeight(AValue: Integer);
begin
  TagValue['ImageLength'] := AValue;
end;

procedure TImageInfo.RemoveTag(ATagTypes: TTagTypes; ATagID: Word; AParentID: Word=0);
var
  i, j: integer;
begin
  j := 0;
  if ttThumb in ATagTypes then begin
    for i := 0 to fiThumbCount-1 do begin
      if (j <> 0) then
        fiThumbArray[i-j] := fiThumbArray[i];
      if (fiThumbArray[i].ParentID = AParentID) and (fiThumbArray[i].Tag = ATagID) then
        inc(j);
    end;
    if (j <> 0) and (fiThumbCount > 0) then
      dec(fiThumbCount);
  end else
  begin
    for i := 0 to fiTagCount-1 do begin
      if (j <> 0) then
        fiTagArray[i-j] := fiTagArray[i];
      if (fiTagArray[i].ParentID = AParentID) and (fiTagArray[i].Tag = ATagID) then
        inc(j);
    end;
    if (j <> 0) and (fiTagCount > 0) then
      dec(fiTagCount);
  end;
end;

function TImageInfo.CreateTagPtr(const ATagDef: TTagEntry; IsThumbTag: Boolean;
  AParentID: Word = 0): PTagEntry;
var
  tag: TTagEntry;
  idx: Integer;
begin
  tag := ATagDef;
  if tag.Size > 0 then
    tag.Raw := StringOfChar(#0, tag.Size);
  if IsThumbTag then
  begin
    tag.ParentID := 1;
    idx := AddTagToThumbArray(tag);
    Result := @fiThumbArray[idx];
  end else
  begin
    tag.ParentID := AParentID;
    idx := AddTagToArray(tag);
    Result := @fiTagArray[idx];
  end;
end;

function TImageInfo.FindTagPtr(const ATagDef: TTagEntry; IsThumbTag: Boolean): PTagEntry;
var
  i: Integer;
begin
  if IsThumbTag then
  begin
    for i:=0 to fiThumbCount-1 do
      if (fiThumbArray[i].Tag = ATagDef.Tag) and (fiThumbArray[i].Name = ATagDef.Name) then
      begin
        Result := @fiThumbArray[i];
        exit;
      end;
  end else
  begin
    for i:=0 to fiTagCount-1 do
      if (fiTagArray[i].Tag = ATagDef.Tag) and (fiTagArray[i].Name = ATagDef.Name) then
      begin
        Result := @fiTagArray[i];
        exit;
      end;
  end;
  Result := nil;
end;
                    (*
function TImageInfo.GetTagPtr(ATagTypes: TTagTypes; ATagID: word;
  AForceCreate: Boolean=false; AParentID:word=0; ATagType: word=65535): PTagEntry;
var
  i, j: integer;
  tag: TTagEntry;
begin
  Result := nil;

  if (ttThumb in ATagTypes) then begin
    if AParentID = $FFFF then     // $FFFF: ignore parent
      for i:= 0 to fiThumbCount-1 do
        if (fiThumbArray[i].Tag = ATagID) then begin
          Result := @fiThumbArray[i];
          exit;
        end;
    for i := 0 to fiThumbCount-1 do
      if (fiThumbArray[i].ParentID = AParentID) and (fiThumbArray[i].Tag = ATagID) then
      begin
        Result := @fiThumbArray[i];
        exit;
      end;
  end else
  begin
    if AParentID = $FFFF then        // $FFFF: ignore parent
      for i := 0 to fiTagCount - 1 do
        if (fiTagArray[i].Tag = ATagID) then begin
          Result := @fiTagArray[i];
          exit;
        end;
    for i := 0 to fiTagCount-1 do
      if (fiTagArray[i].ParentID = AParentID) and (fiTagArray[i].Tag = ATagID) then
      begin
        Result := @fiTagArray[i];
        exit;
      end;
  end;

  if AForceCreate then begin
    tag := FindExifTagDefByID(ATagID)^;
    if ATagType <> 65535 then
      tag.TType := ATagType;
    tag.Id := 0;
    if tag.Size > 0 then
      tag.Raw := StringOfChar(#0, tag.Size);
    if (ttThumb in ATagTypes) then begin
      tag.ParentID := 1;
      i := AddTagToThumbArray(tag);
      Result := @fiThumbArray[i];
    end;
    if ([ttExif, ttGps] * ATagTypes <> []) then begin
      tag.parentID := AParentID;
      i := AddTagToArray(tag);
      Result := @fiTagArray[i];
    end;
  end;
end;
            *)
function TImageInfo.GetArtist: String;
begin
  Result := GetTagValueAsString('Artist');
end;

procedure TImageInfo.SetArtist(v: String);
begin
  SetTagValue('Artist', v);
end;
(*

var
  p : PTagEntry;
begin
  if (v = '') then begin
    RemoveTag([ttExif], TAG_ARTIST, 0);
    exit;
  end;
  p := GetTagPtr([ttExif], TAG_ARTIST, true, 0, 2);
  {$IFDEF FPC}
  p^.Raw := UTF8ToAnsi(v) + #0;
  {$ELSE}
  p^.Raw := AnsiString(v) + #0;
  {$ENDIF}
  p^.Data := p^.Raw;
  p^.Size := Length(p^.Raw);
end;
  *)
function TImageInfo.GetExifComment: String;
var
  buf: ansistring;
  w: WideString;
  a: ansistring;
  n: Integer;
  tag: TTagEntry;
begin
  Result := '';

  tag := GetTagByName('UserComment');
  if tag.Tag = 0 then
    exit;

  InternalGetBinaryTagValue(tag, buf);
  if buf = '' then
    exit;

  if pos('UNICODE', buf) = 1 then begin
    SetLength(w, (Length(buf) - 8) div SizeOf(WideChar));
    Move(buf[9], w[1], Length(w) * Sizeof(WideChar));
    {$IFDEF FPC}
    Result := UTF8Encode(w);
    {$ELSE}
    Result := w;
    {$ENDIF}
  end else
  if pos('ASCII', buf) = 1 then begin
    a := Copy(buf, 9, MaxInt);
    Result := a;
  end else
  if pos(#0#0#0#0#0#0#0#0, buf) = 1 then begin
    a := Copy(buf, 9, MaxInt);
   {$IFDEF FPC}
    {$IFDEF FPC3+}
    Result := WinCPToUTF8(a);
    {$ELSE}
    Result := SysToUTF8(a);
    {$ENDIF}
   {$ELSE}
    Result := a;
   {$ENDIF}
  end else
  if Pos('JIS', buf) = 1 then
    raise Exception.Create('JIS-encoded user comment is not supported.');
end;

(*
function TImageInfo.GetExifComment: String;
var
  p : PTagEntry;
  w : WideString;
  n: Integer;
  sa: AnsiString;
begin
  Result := '';
  w := '';
  p := GetTagPtr([ttExif], TAG_EXIF_OFFSET);
  if (p = nil) then
    exit;
  p := GetTagPtr([ttExif], TAG_USERCOMMENT, false, TAG_EXIF_OFFSET);
  if (p = nil) or (Length(p^.Raw) <= 10) then
    exit;

  if Pos('UNICODE', p^.Raw) = 1 then begin
    SetLength(w, (Length(p^.Raw) - 8) div SizeOf(WideChar));
    Move(p^.Raw[9], w[1], Length(w) * SizeOf(WideChar));
    {$IFDEF FPC}
    Result := UTF8Encode(w);
    {$ELSE}
    Result := w;
    {$ENDIF}
  end else
  if Pos('ASCII', p^.Raw) = 1 then begin
    SetLength(Result, Length(p^.Raw)-9);
    sa := p^.Raw;
    Delete(sa, 1, 8);
    Result := sa;
  end else
  if Pos(#0#0#0#0#0#0#0#0, p^.Raw) = 1 then begin
    SetLength(sa, Length(p^.Raw) - 9);
    Move(p^.raw[9], sa[1], Length(sa));
    {$IFDEF FPC}
    {$IFNDEF FPC3+}
    Result := SysToUTF8(sa);
    {$ELSE}
    Result := WinCPToUTF8(sa);
    {$ENDIF}
    {$ELSE}
    Result := sa;
    {$ENDIF}
  end else
  if Pos('JIS', p^.Raw) = 1 then
    raise Exception.Create('JIS-encoded user comment is not supported.');
end;
*)

procedure TImageInfo.SetExifComment(AValue: String);
var
  p: PTagEntry;
  i: integer;
  w: WideString;
  a: AnsiString;
  u: Boolean;
  buf: array of byte;
  len: Integer;
begin
  if AValue = '' then
    SetLength(buf, 0)
  else
  begin
    u := false;
    for i:=1 to Length(AValue) do
      if byte(AValue[i]) > 127 then begin
        u := true;
        break;
      end;

    if u then begin
      {$IFDEF FPC}
      w := UTF8Decode(AValue);
      {$ELSE}
      w := AValue;
      {$ENDIF}
      SetLength(buf, 8 + Length(w) * SizeOf(WideChar));  // +8 for header
      a := 'UNICODE'#0;
      Move(a[1], buf[0], 8);
      Move(w[1], buf[8], Length(w) * Sizeof(WideChar));
    end else
    begin
      SetLength(buf, 8 + Length(AValue));
      a := 'ASCII'#0#0#0;
      Move(a[1], buf[0], 8);
      a := ansistring(AValue);
      Move(a[1], buf[8], Length(a));
    end;
  end;
  InternalSetTagValue('UserComment', NULL, [ttExif, ttGps], @buf[0], Length(buf));

(*
  p := GetTagPtr([ttExif], TAG_EXIF_OFFSET, true, 0, FMT_ULONG{, true});
  if (v = '') then begin
    RemoveTag([ttExif], TAG_USERCOMMENT, TAG_EXIF_OFFSET);
    exit;
  end;

  p := GetTagPtr([ttExif], TAG_USERCOMMENT, true, TAG_EXIF_OFFSET, FMT_BINARY);
  u := false;
  for i:=1 to Length(v) do
    if byte(v[i]) > 127 then begin
      u := true;
      break;
    end;

  if u then begin
    p^.Raw := 'UNICODE'#0;
    // According to docs: no need to add a trailing zero byte
   {$IFDEF FPC}
    w := UTF8Decode(v);
   {$ELSE}
    w := v;
   {$ENDIF}
    SetLength(p^.Raw, Length(w) * SizeOf(WideChar) + 8);
    Move(w[1], p^.Raw[9], Length(w) * SizeOf(WideChar));
  end else begin
    p^.Raw := 'ASCII'#0#0#0;
    // According to docs: no need to add a trailing zero byte
    a := AnsiString(v);
    SetLength(p^.Raw, Length(a) + 8);
    i := Length(p^.Raw);
    Move(a[1], p^.Raw[9], Length(a));
  end;
  p^.Size := Length(p^.Raw);
  p^.Data := v;
  *)
end;

function TImageInfo.GetImageDescription: String;
begin
  Result := GetTagValueAsString('ImageDescription');
end;

procedure TImageInfo.SetImageDescription(const AValue: String);
begin
  SetTagValue('ImageDescription', AValue);
end;

function TImageInfo.GetCameraMake: String;
begin
  Result := GetTagValueAsString('Make');
end;

procedure TImageInfo.SetCameraMake(const AValue: String);
begin
  SetTagValue('Make', AValue);
end;

function TImageInfo.GetCameraModel: String;
begin
  Result := GetTagValueAsString('Model');
end;

procedure TImageInfo.SetCameraModel(const AValue: String);
begin
  SetTagValue('Model', AValue);
end;

function TImageInfo.GetCopyright: String;
begin
  Result := GetTagValueAsString('Copyright');
end;

procedure TImageInfo.SetCopyright(const AValue: String);
begin
  SetTagValue('Copyright', AValue);
end;

function TImageInfo.GetGPSCoordinate(ATagName: String;
  ACoordType: TGPSCoordType): Extended;
var
  vDeg, vSgn: Variant;
begin
  Result := 0.0;
  vDeg := GetTagValue(ATagName);
  if VarIsNull(vDeg) then
    exit;
  if not VarIsArray(vDeg) then
    exit;

  Result := vDeg[0] + vDeg[1]/60 + vDeg[2]/3600;
  vSgn := GetTagValue(ATagName + 'Ref');
  if VarIsNull(vSgn) then
    exit;
  case ACoordType of
    ctLatitude  : if VarToStr(vSgn)[1] in ['S', 's'] then Result := -Result;
    ctLongitude : if VarToStr(vSgn)[1] in ['W', 'w'] then Result := -Result;
  end;
end;

procedure TImageInfo.SetGPSCoordinate(ATagName: String; const AValue: Extended;
  ACoordType: TGPSCoordType);
const
  Ref: array[TGPSCoordType] of string[2] = ('NS', 'EW');
var
  v: Variant;
  degs, mins, secs: double;
  val: Extended;
begin
  val := abs(AValue);
  degs := trunc(val);
  mins := trunc(frac(val) * 60);
  secs := (frac(val) * 60 - mins) * 60;
//  secs := trunc(frac(AValue) * 3600) - mins*60;
//  secs := trunc(mins*60 - frac(frac(AValue) * 60));
  v := VarArrayOf([degs, mins, secs]);
  InternalSetTagValue(ATagName, v, [ttGps]);
  if AValue > 0 then
    InternalSetTagValue(ATagName + 'Ref', Ref[ACoordType, 1], [ttGps])
  else
    InternalSetTagValue(ATagName + 'Ref', Ref[ACoordType, 2], [ttGps]);
  VarClear(v);
end;


function TImageInfo.GetGPSLatitude: Extended;
begin
  Result := GetGPSCoordinate('GPSLatitude', ctLatitude);
end;

procedure TImageInfo.SetGPSLatitude(const AValue: Extended);
begin
  SetGPSCoordinate('GPSLatitude', AValue, ctLatitude);
end;

function TImageInfo.GetGPSLongitude: Extended;
begin
  Result := GetGPSCoordinate('GPSLongitude', ctLongitude);
end;

procedure TImageInfo.SetGPSLongitude(const AValue: Extended);
begin
  SetGPSCoordinate('GPSLongitude', AValue, ctLongitude);
end;

function TImageInfo.IterateFoundTags(TagId: integer; var RetVal: TTagEntry): boolean;
begin
  InitTagEntry(Retval);

  while (iterator < FITagCount) and (FITagArray[iterator].TID <> TagId) do
    inc(iterator);
  if (iterator < FITagCount) then
  begin
    RetVal := FITagArray[iterator];
    inc(iterator);
    Result := true;
  end
  else
    Result := false;
end;

procedure TImageInfo.ResetIterator;
begin
  iterator := 0;
end;

function TImageInfo.IterateFoundThumbTags(TagId: integer;
  var RetVal: TTagEntry): boolean;
begin
  InitTagEntry(RetVal);

  while (iterThumb < FIThumbCount) and (FITagArray[iterThumb].TID <> TagId) do
    inc(iterThumb);
  if (iterThumb < FIThumbCount) then
  begin
    RetVal := FIThumbArray[iterThumb];
    inc(iterThumb);
    Result := true;
  end
  else
    Result := false;
end;

procedure TImageInfo.ResetThumbIterator;
begin
  iterThumb := 0;
end;

function TImageInfo.GetRawFloat(ATagName: String): Double;
var
  tiq: TTagEntry;
begin
  tiq := GetTagByName(ATagName);
  if tiq.Tag = 0 then     // EmptyEntry
    Result := 0.0
  else
    Result := GetNumber(tiq.Raw, tiq.TType);
end;

function TImageInfo.GetRawInt(ATagName: String): Integer;
var
  tiq: TTagEntry;
begin
  tiq := GetTagByName(ATagName);
  if tiq.Tag = 0 then  // EmptyEntry
    Result := -1
  else
  if (tiq.TType = FMT_BINARY) and (tiq.Size = 1) then
    Result := byte(tiq.Raw[1])
  else
    result := round(GetNumber(tiq.Raw, tiq.TType));
end;

//  Unfortunatly if we're calling this function there isn't
//  enough info in the EXIF to calculate the equivalent 35mm
//  focal length and it needs to be looked up on a camera
//  by camera basis. - next rev - maybe
Function TImageInfo.LookupRatio: double;
var
  estRatio: double;
  upMake, upModel: String;
begin
  upMake := Uppercase(copy(CameraMake, 1, 5));
  upModel := Uppercase(copy(Cameramodel, 1, 5));
  estRatio := 4.5;  // ballpark for *my* camera -
  result := estRatio;
end;

procedure TImageInfo.Calc35Equiv;
const
  Diag35mm : double = 43.26661531; // sqrt(sqr(24)+sqr(36))
var
  tmp: integer;
  CCDWidth, CCDHeight, fpu, fl, fl35, ratio: double;
  NewE, LookUpE: TTagEntry;
  w: Word;
begin
  if LookUpTag('FocalLengthin35mmFilm') >= 0 then
    exit;  // no need to calculate - already have it

  CCDWidth  := 0.0;
  CCDHeight := 0.0;
  tmp := GetRawInt('FocalPlaneResolutionUnit');
  if (tmp <= 0) then
    tmp := GetRawInt('ResolutionUnit');
  case tmp of
    2: fpu := 25.4;   // inch
    3: fpu := 10;     // centimeter
  else
    fpu := 0.0
  end;

  fl := GetRawFloat('FocalLength');
  if (fpu = 0.0) or (fl = 0.0) then
    exit;

  tmp := GetRawInt('FocalPlaneXResolution');
  if (tmp <= 0) then
    exit;
  CCDWidth := Width * fpu / tmp;

  tmp := GetRawInt('FocalPlaneYResolution');
  if (tmp <= 0) then
    exit;

  CCDHeight := Height * fpu / tmp;

  if CCDWidth*CCDHeight <= 0 then  // if either is zero
  begin
    if not estimateValues then
      exit;
    ratio := LookupRatio()
  end
  else
    ratio :=  Diag35mm / sqrt (sqr (CCDWidth) + sqr (CCDHeight));

  fl35 := fl *  ratio;
  w := Round(fl35);

// now load it into the tag array
  tmp := LookupTagDefn('FocalLengthIn35mmFilm');
  if tmp = -1 then
    exit;

  LookUpE := TagTable[tmp];
  NewE := LookupE;
  NewE.Data := ansistring(Format('%0.2f',[fl35]));
  NewE.FormatS := '%s mm';
  SetLength(NewE.Raw, 2);
  Move(w, NewE.Raw[1], 2);
  NewE.TType := FMT_USHORT;
  AddTagToArray(NewE);

  TraceStr := TraceStr + crlf +
    siif(ExifTrace > 0, 'tag[$' + IntToHex(tmp,4) + ']: ', '') +
    NewE.Desc + dExifDelim + NewE.Data +
    siif(ExifTrace > 0,' [size: 0]', '') +
    siif(ExifTrace > 0,' [start: 0]', '');
end;

// WARNING: The calling procedure must destroy the stringlist created here.
function TImageInfo.EXIFArrayToXML: TStringList;
begin
  Result := TStringList.Create;
  EXIFArrayToXML(Result);
end;

procedure TImageInfo.EXIFArrayToXML(AList: TStrings);
var
  i: integer;
begin
  Assert(AList <> nil, 'TImageInfo.ExifArrayToXML called with AList=nil.');
  AList.Add('   <EXIFdata>');
  for i := 0 to fiTagCount-1 do
    with fITagArray[i] do
    begin
      AList.Add('   <' + Name + '>');
      if Tag in [105, 120] // headline and image caption         // wp: ?? 105 = $0069, 120 = $0078 -- there are no such tags!
        then AList.Add('      <![CDATA[' + Data + ']]>')
        else AList.Add('      ' + Data);
      AList.Add('   </' + Name + '>');
    end;
  AList.Add('   </EXIFdata>');
end;


//--------------------------------------------------------------------------
// The following methods implement the outer parser which
// decodes the segments.  Further parsing isthen passed on to
// the TImageInfo (for EXIF) and TIPTCData objects
//--------------------------------------------------------------------------
Procedure TImgData.MakeIPTCSegment(buff:ansistring);
var bl:integer;
begin
  bl := length(buff)+2;
  if IPTCSegment = nil then
  begin
    inc(SectionCnt);
    IPTCSegment := @(sections[SectionCnt]);
  end;
  IPTCSegment^.data := ansichar(bl div 256)+ansichar(bl mod 256)+buff;
  IPTCSegment^.size := bl;
  IPTCSegment^.dtype := M_IPTC;
end;
           (*
Procedure TImgData.MakeCommentSegment(ABuffer: ansistring);
var
  w: word;
begin
  if CommentSegment = nil then
  begin
    inc(SectionCnt);
    CommentSegment := @(Sections[SectionCnt]);
  end;

  CommentSegment^.DType := M_COM;
  CommentSegment^.Size := Length(ABuffer) + 2;
  w := NtoBE(word(CommentSegment^.Size));
  Move(w, CommentSegment^.Data[1], 2);
  Move(ABuffer[1], CommentSegment^.Data[3], Length(ABuffer));
//  CommentSegment^.Data := AnsiChar(bl div 256)+ansichar(bl mod 256)+buff;
end;

Function TImgData.GetCommentSegment:ansistring;
begin
  result := '';
  if CommentSegment <> nil then
    result := copy(CommentSegment^.data,2,maxint);
end;
               *)
(*
function TImgData.SaveExif(var jfs2:tstream):longint;
var cnt:longint;
    buff:ansistring;
begin
  cnt:=0;
  buff := #$FF#$D8;
  jfs2.Write(pointer(buff)^,length(buff));
  if ExifSegment <> nil then
    with ExifSegment^ do
    begin
      buff := #$FF+AnsiChar(Dtype)+data;
      cnt := cnt+jfs2.Write(pointer(buff)^,length(buff));
    end
  else
    if HeaderSegment <> nil then
      with HeaderSegment^ do
      begin
        buff := AnsiChar($FF)+AnsiChar(Dtype)+data;
        // buff := #$FF+AnsiChar(Dtype)+#$00#$10'JFIF'#$00#$01#$02#$01#$01','#$01','#$00#$00;
        cnt := cnt+jfs2.Write(pointer(buff)^,length(buff));
      end
    else if (cnt = 0) then
    begin
      // buff := AnsiChar($FF)+AnsiChar(Dtype)+data;
      buff := #$FF+AnsiChar(M_JFIF)+#$00#$10'JFIF'#$00#$01#$02#$01#$01','#$01','#$00#$00;
      cnt := cnt+jfs2.Write(pointer(buff)^,length(buff));
    end;
  if IPTCSegment <> nil then
    with IPTCSegment^ do
    begin
      buff := AnsiChar($FF)+AnsiChar(Dtype)+data;
      cnt := cnt+jfs2.Write(pointer(buff)^,length(buff));
    end;
  if CommentSegment <> nil then
    with CommentSegment^ do
    begin
      buff := AnsiChar($FF)+AnsiChar(Dtype)+data;
      cnt := cnt+jfs2.Write(pointer(buff)^,length(buff));
    end;
   result := cnt;
end;
*)

function TImgData.SaveExif(AStream: TStream;
  AWriteMetadata: TMetadataKinds = mdkAll): LongInt;
const
  SOI_MARKER: array[0..1] of byte = ($FF, $D8);
  JFIF_MARKER: array[0..1] of byte = ($FF, $E0);
  JFIF: ansistring = 'JFIF'#0;
var
  APP0Segment: TJFIFSegment;
  buff: AnsiString;
  writer: TExifWriter;
  a: Ansistring;
  w: Word;
begin
  // Write Start-Of-Image segment (SOI)
  AStream.WriteBuffer(SOI_MARKER, SizeOf(SOI_MARKER));

  // No Exif --> write an APP0 segment
  if not (mdkExif in AWriteMetadata) or (not HasExif) then begin
    if HeaderSegment = nil then begin
      APP0Segment.Length := NtoBE(SizeOf(APP0Segment) - 2);
      Move(JFIF[1], APP0Segment.Identifier[0], Length(JFIF));
      APP0Segment.JFIFVersion[0] := 1;
      APP0Segment.JFIFVersion[1] := 2;
      APP0Segment.DensityUnits := 1;         // inch
      APP0Segment.XDensity := NtoBE(72);     // 72 ppi
      APP0Segment.YDensity := NtoBE(72);
      APP0Segment.ThumbnailWidth := 0;
      APP0Segment.ThumbnailHeight := 0;
      AStream.WriteBuffer(JFIF_MARKER, SizeOf(JFIF_MARKER));
      AStream.WriteBuffer(APP0Segment, SizeOf(APP0Segment));
    end else
      with HeaderSegment^ do
      begin
        buff := chr($FF) + chr(Dtype) + Data;
        AStream.WriteBuffer(buff[1], Length(buff));
      end;
  end else
  begin
    // EXIF --> Write APP1 segment
    writer := TExifWriter.Create(self);
    try
      writer.BigEndian := MotorolaOrder;
      writer.WriteExifHeader(AStream);
      writer.WriteToStream(AStream);
    finally
      writer.Free;
    end;
  end;

  // Write IPTCSegment
  if (mdkIPTC in AWriteMetadata) and HasIPTC then
    with IPTCSegment^ do
    begin
      buff := chr($FF) + chr(Dtype) + data;
      AStream.Write(pointer(buff)^, Length(buff));
    end;

  // Write comment segment
  if (mdkComment in AWriteMetadata) and HasComment then begin
   {$IFDEF FPC}
    {$IFDEF FPC+}
    a := FComment;
   {$ELSE}
     a := UTF8ToAnsi(FComment);
    {$ENDIF}
   {$ELSE}
    a := FComment;
   {$ENDIF}
    SetLength(buff, 2 + 2 + Length(a));
    buff[1] := ansichar($FF);
    buff[2] := ansichar(M_COM);
    w := NToBE(word(Length(a) + 2));  // Length of the segment, in Big Endian
    Move(w, buff[3], SizeOf(w));
    Move(a[1], buff[5], Length(a));
    AStream.Write(buff[1], Length(buff));
  end;

  Result := AStream.Position;
end;

function TImgData.ExtractThumbnailBuffer: TBytes;
begin
  Result := nil;
  if HasExif and ExifObj.HasThumbnail then
    Result := ExifObj.ThumbnailBuffer;
  {
var
  STARTmarker, STOPmarker: integer;
  tb: ansistring;
begin
  result := nil;
  if HasThumbnail then
  begin
    try
      tb := copy(DataBuff, ExifObj.FThumbStart, ExifObj.FThumbLength);
      STARTmarker := Pos(#$ff#$d8#$ff#$db, tb);
      if Startmarker = 0 then
        STARTmarker := Pos(#$ff#$d8#$ff#$c4, tb);
      if STARTmarker <= 0 then
        exit;
      tb := copy(tb, STARTmarker, Length(tb));  // strip off thumb data block
      // ok, this is fast and easy - BUT what we really need
      // is to read the length bytes to do the extraction...
      STOPmarker := Pos(#$ff#$d9, tb) + 2;
      tb := copy(tb, 1, STOPmarker);
      SetLength(Result, Length(tb));
      Move(tb[1], Result[0], Length(Result));
    except
      // Result will nil...
    end;
  end;
  }
end;

//{$IFDEF FPC}
function TImgData.ExtractThumbnailJpeg(AStream: TStream): Boolean;
var
  b: TBytes;
  p: Int64;
begin
  Result := false;
  if (AStream <> nil) and HasExif and ExifObj.HasThumbnail then
  begin
    ExifObj.SaveThumbnailToStream(AStream);
    Result := true;
  end;
  {
  else
  if (AStream <> nil) and HasThumbnail and (ExifObj.FThumbType = JPEG_COMP_TYPE) then
  begin
    b := ExtractThumbnailBuffer();
    if b <> nil then begin
      p := AStream.Position;
      AStream.WriteBuffer(b[0], Length(b));
      AStream.Position := p;
      Result := true;
    end;
  end;
  }
end;

{ A jpeg image has been written to a stream. The current EXIF data will be
  merged with this stream and saved to the specified file.
  If AdjSize is true TImgData's image width/height are replaced by the
  values found in the stream.
  NOTE: It is in the responsibility of the programmer to make sure that
  AJpeg is a stream of a valid jpeg image. }
procedure TImgData.WriteEXIFJpeg(AJpeg: TStream; AFileName: String;
  AdjSize: Boolean = true);
var
  jms: TMemoryStream;
  jfs: TFileStream;
  w, h: Integer;
  NewExifBlock: boolean;
begin
  jfs := TFileStream.Create(AFilename, fmCreate or fmShareExclusive);
  try
    AJpeg.Position := 0;               // JPEG reader must be at begin of stream
    if AdjSize and (EXIFobj <> nil) then begin
      JPEGImageSize(AJpeg, w, h);
      EXIFobj.AdjExifSize(h, w);       // Adjust EXIF to image size
      AJpeg.Position := 0;             // Rewind stream
    end;
    //  SaveExif(jfs);
    // If no exif block is here create a new one
    NewExifBlock:= (ExifObj = nil);
    jms := TMemoryStream.Create;
    try
      jms.CopyFrom(AJpeg, AJpeg.Size);
      MergeToStream(jms, jfs);
    finally
      jms.Free;
    end;
  finally
    jfs.Free;
  end;
end;

{ Replaces or adds the currently loaded EXIF data to the image in AOrigName
  and saves as AFileName.
  WARNING: This destroys the currently loaded exif data! }
procedure TImgData.WriteEXIFJpeg(AFileName, AOrigName: String;
  AdjSize: Boolean = true);
var
  js: TMemoryStream;
begin
  if AOrigName = '' then
    exit;  // nothing to do --

  js := TMemoryStream.Create;
  try
    js.LoadFromFile(AOrigName);
    if ReadExifInfo(AOrigName) then
      WriteEXIFJpeg(js, AFilename, AdjSize)
    else
      js.SaveToFile(AFilename);
  finally
    js.Free;
  end;
end;

{ Write the current EXIF data into the existing jpeg file named AFileName.
  NOTE: THis does not work if the specified file does not exist because this
  is where the image data come from. }
procedure TImgData.WriteEXIFJpeg(AFilename: String; AdjSize: Boolean = true);
var
  imgStream: TMemoryStream;
begin
  if not FileExists(AFileName) then
    raise Exception.Create('Image file "' + AFilename + '" does not exist.');
  imgStream := TMemoryStream.Create;
  try
    imgStream.LoadFromFile(AFileName);
    WriteEXIFJpeg(imgstream, AFileName, AdjSize);
  finally
    imgStream.Free;
  end;
end;

{ Writes Exif and image data of the currently loaded file to a file with
  the specified name.
  NOTE: This does not work if the exif data were created manaully because
  there is no filename where to get the image data from. }
procedure TImgData.WriteExifJpegTo(AFileName: String);
var
  imgStream: TMemoryStream;
begin
  if FFileName = '' then
    raise Exception.Create('TImgData has no filename.');

  imgStream := TMemoryStream.Create;
  try
    imgStream.LoadFromFile(FFileName);
    WriteExifJpeg(imgStream, AFilename, false);
  finally
    imgStream.Free;
  end;
end;

{$IFNDEF dExifNoJpeg}
function TImgData.ExtractThumbnailJpeg: TJpegImage;
var
  ms: TMemoryStream;
  b: TBytes;
begin
  Result := nil;
  if HasExif and ExifObj.HasThumbnail then begin
    ms := TMemoryStream.Create;
    try
      ExifObj.SaveThumbnailToStream(ms);
      ms.Position := 0;
      Result := TJpegImage.Create;
      Result.LoadFromStream(ms);
    finally
      ms.Free;
    end;
  end;
end;

procedure TImgData.WriteEXIFJpeg(j: TJpegImage; fname, origName: String;
  AdjSize: boolean = true);
begin
  if origName = '' then
    origName := fname;
  if not ReadExifInfo(origName) then
  begin
    j.SaveToFile(fname);
    exit;
  end;
  WriteEXIFJpeg(j,fname,adjSize);
end;

procedure TImgData.WriteEXIFJpeg(fname: String);
var
  img: TJpegImage;
begin
  img := TJPEGImage.Create;
  try
    img.LoadFromFile(Filename);
    WriteEXIFJpeg(img, fname, false);
  finally
    img.Free;
  end;
end;

procedure TImgData.WriteEXIFJpeg(j: TJpegImage; fname: String;
  AdjSize:boolean = true);
var
  jms: tmemorystream;
  jfs: TFileStream;
  NewExifBlock: Boolean;
begin
  NewExifBlock := (ExifObj = nil);

  // to do: Create a new exif block here if AdjSize is true
  if AdjSize and (EXIFobj <> nil) then
    EXIFobj.AdjExifSize(j.height,j.width);

  jms := tmemorystream.Create;
  try  { Thanks to Erik Ludden... }
    j.SaveToStream(jms);
    jfs := TFileStream.Create(fname, fmCreate or fmShareExclusive);
    try
      MergeToStream(jms, jfs);
    finally
      jfs.Free;
    end
  finally
    jms.Free;
  end
end;
{$ENDIF}

//procedure TImgData.MergeToStream(AInputStream, AOutputStream: TStream;
//  AEnabledMeta: Byte = $FF; AFreshExifBlock: Boolean = false);
procedure TImgData.MergeToStream(AInputStream, AOutputStream: TStream;
  AWriteMetadata: TMetadataKinds = mdkAll);
type
  TSegmentHeader = packed record
    Key: byte;
    Marker: byte;
    Size: Word;
  end;
var
  header: TSegmentHeader;
  n, count: Integer;
  savedPos: Int64;
begin
  // Write the header segment and all segments modified by dEXIF
  // to the beginning of the stream
  AOutputStream.Position := 0;
  SaveExif(AOutputStream, AWriteMetaData);

  // Now write copy all segments which were not modified by dEXIF.
  AInputStream.Position := 0;
  while AInputStream.Position < AInputStream.Size do begin
    savedPos := AInputStream.Position;  // just for debugging
    n := AInputStream.Read(header, SizeOf(header));
    if n <> Sizeof(header) then
      raise Exception.Create('Defective JPEG structure: Incomplete segment header');
    if header.Key <> $FF then
       raise Exception.Create('Defective JPEG structure: $FF expected.');
     header.Size := BEToN(header.Size);

     // Save stream position before segment size value.
     savedPos := AInputStream.Position - 2;
     case header.Marker of
       M_SOI:
         header.Size := 0;
       M_JFIF, M_EXIF, M_IPTC, M_COM:  // these segments were already written by SaveExif
         ;
       M_SOS:
         begin
           // this is the last segment before compressed data which don't have a marker
           // --> just copy the rest of the file
           count := AInputStream.Size - savedPos;
           AInputStream.Position := savedPos;
           AOutputStream.WriteBuffer(header, 2);
           n := AOutputStream.CopyFrom(AInputStream, count);
           if n <> count then
             raise Exception.Create('Read/write error detected for compressed data.');
           break;
         end;
       else
         AInputstream.Position := AInputStream.Position - 4;  // go back to where the segment begins
         n := AOutputStream.Copyfrom(AInputStream, header.Size + 2);
         if n <> header.Size + 2 then
           raise Exception.CreateFmt('Read/write error in segment $FF%.2x', [header.Marker]);
     end;
     AInputStream.Position := savedPos + header.Size;
  end;
end;

function TImgData.FillInIptc:boolean;
begin
  if IPTCSegment = nil then
    CreateIPTCObj
  else
    IPTCObj.ParseIPTCArray(IPTCSegment^.Data);
    // To do: Here's a memory leak because ParseIPTCArray returns a StringList which is not destroyed!

//    filename := FName;
  result := IPTCObj.HasData();
end;

procedure TImgData.ClearSections;
begin
  ClearEXIF;
  ClearIPTC;
  ClearComments;
end;

procedure TImgData.ClearEXIF;
begin
  ExifSegment := nil;
  FreeAndNil(ExifObj);
end;

procedure TImgData.ClearIPTC;
begin
  IPTCSegment := nil;
//  HeaderSegment := nil;
  FreeAndNil(IptcObj);
end;

procedure TImgData.ClearComments;
begin
  FComment := '';
//  CommentSegment := nil;
//  HeaderSegment := nil;
end;

(*
function TImgData.GetCommentStr:ansistring;
begin
  Result := GetComment;
end;

function TImgData.GetComment: String;
var
  buffer: ansistring;
  bufLen: integer;
begin
  if CommentSegment = nil then
    Result := ''
  else begin
    buffer := CommentSegment^.Data;
    bufLen := (byte(buffer[1]) shl 8) or byte(buffer[2]);
    {$IFDEF FPC}
    Result := AnsiToUTF8(copy(buffer, 3, bufLen - 2));
    {$ELSE}
    Result := ansistring(Copy(buffer, 3, bufLen - 2));
    {$ENDIF}
  end;
end;
 *)
procedure TImgData.SetComment(const AValue: String);
var
  a: ansistring;
begin
 {$IFDEF FPC}
  {$IFDEF FPC3+}
  a := UTF8ToWinCP(AValue);
  {$ELSE}
  a := UTF8ToAnsi(AValue);
  {$ENDIF}
 {$ELSE}
  a := AValue;
 {$ENDIF}
  if Length(a) > Word($FFFF) - 4 then
    raise Exception.CreateFmt('Comment too long, max %d characters', [Word($FFFF) - 4]);
  FComment := AValue;
end;

function TImgData.ReadExifInfo(AFilename: String): boolean;
begin
  ProcessFile(AFilename);
  result := HasMetaData();
end;

function TImgData.ProcessFile(const AFileName: string): boolean;
var
  ext: string;
begin
  Reset;
  Result := false;
  if not FileExists(aFileName) then
    exit;

  SetFileInfo(AFileName);
  try
    FErrStr := 'Not an EXIF file';
    ext := LowerCase(ExtractFileExt(filename));
    if (ext = '.jpg') or (ext = '.jpeg') or (ext = '.jpe') then
    begin
      if not ReadJpegFile(FileName) then
        exit;
    end else
    if (ext = '.tif') or (ext = '.tiff') or (ext = '.nef') then
    begin
      if not ReadTiffFile(FileName) then
        exit;
    end else
      exit;

    FErrStr := '<none>';
//    msAvailable := ReadMSData(Imageinfo);
//    msName := gblUCMaker;
    Result := true;
  except
    FErrStr := 'Illegal EXIF construction';
  end;
end;

procedure TImgData.SetFileInfo(const AFileName: String);
var
  sr: TSearchRec;
  stat: word;
begin
  stat := FindFirst(AFilename, faAnyFile, sr);
  if stat = 0 then
  begin
    FFilename := AFilename;
    FFileDateTime := FileDateToDateTime(sr.Time);
    FFileSize := sr.Size;
  end;
  FindClose(sr);
end;

procedure TImgData.SetHeight(AValue: Integer);
begin
  CreateExifObj;
  ExifObj.TagValue['ImageLength'] := AValue;
  FHeight := AValue;
end;

procedure TImgData.SetWidth(AValue: Integer);
begin
  CreateExifObj;
  ExifObj.TagValue['ImageWidth'] := AValue;
  FWidth := AValue;
end;

function TImgData.CreateExifObj: TImageInfo;
begin
  ExifObj.Free;
  ExifObj := TImageInfo.Create(self);
  FErrStr := '<none>';
  Result := ExifObj;
end;

function TImgData.CreateIPTCObj: TIPTCData;
begin
  IPTCObj.Free;
  MakeIPTCSegment('');
  IPTCobj := TIPTCdata.Create(self);
  Result := IPTCObj;
end;

//--------------------------------------------------------------------------
// Parse the marker stream until SOS or EOI is seen
//--------------------------------------------------------------------------
function TImgData.ReadJpegSections(AStream: TStream): boolean;
var
  a, b: byte;
  ll, lh, itemLen, marker: integer;
  pw: PWord;
  sa: ansistring;
begin
  a := GetByte(AStream);
  b := GetByte(AStream);
  if (a <> $ff) or (b <> M_SOI) then
  begin
    Result := false;
    exit;
  end;
  SectionCnt := 0;
  while SectionCnt < 20 do  // prevent overruns on bad data
  begin
    repeat
      marker := GetByte(AStream);
    until marker <> $FF;
    inc(SectionCnt);
    // Read the length of the section.
    lh := GetByte(AStream);
    ll := GetByte(AStream);
    itemLen := (lh shl 8) or ll;
    with Sections[SectionCnt] do
    begin
      DType := marker;
      Size := itemLen;
      SetLength(data, itemLen);
      if itemLen > 0 then
        begin
          data[1] := ansichar(lh);
          data[2] := ansichar(ll);
        end;
      try
        AStream.Read(data[3], itemLen-2);
      except
        continue;
      end;
    end;
    if (SectionCnt = 5) and not HasMetaData() then
      break;  // no exif by 8th - let's not waste time
    case marker of
      M_SOS:
        break;
      M_EOI:
        break;  // in case it's a tables-only JPEG stream
      M_COM:
        begin
          SetLength(sa, Sections[SectionCnt].Size - 2);
          Move(Sections[SectionCnt].Data[3], sa[1], Length(sa));
         {$IFDEF FPC}
          {$IFDEF FPC3+}
          FComment := WinCPToUTF8(sa);
          {$ELSE}
          FComment := AnsiToUTF8(sa);
          {$ENDIF}
         {$ELSE}
          FComment := sa;
         {$ENDIF}
          dec(SectionCnt);  // No need to store the Comment segment any more
        end;
      M_IPTC:
        begin // IPTC section
          if (IPTCSegment = nil) then
          begin
            IPTCSegment := @Sections[SectionCnt];
            IPTCobj := TIPTCdata.Create(self);
          end;
        end;
      M_JFIF:
        begin
          // Regular jpegs always have this tag, exif images have the exif
          // marker instead, althogh ACDsee will write images with both markers.
          // This program will re-create this marker on absence of exif marker.
          HeaderSegment := @sections[SectionCnt];
        end;
      M_EXIF:
        begin
          if ((SectionCnt <= 5) and (EXIFsegment = nil)) then
          begin
            // Seen files from some 'U-lead' software with Vivitar scanner
            // that uses marker 31 later in the file (no clue what for!)
            EXIFsegment := @sections[SectionCnt];
            EXIFobj := TImageInfo.Create(self,BuildList);
            EXIFobj.TraceLevel := TraceLevel;
            SetDataBuff(EXIFsegment^.data);
            ProcessEXIF;
          end else
          begin
            // Discard this section.
            dec(SectionCnt);
          end;
        end;
      M_SOF0:
        with Sections[SectionCnt] do begin
          pw := @data[4];
          FHeight := BEToN(pw^);
          pw := @data[6];
          FWidth := BEToN(pw^);
          dec(SectionCnt);
        end;
      {
      M_SOF1..M_SOF15:
        begin
          // process_SOFn(Data, marker);
        end;
        }
      else
        dec(SectionCnt);  // Discard this section
    end;  // case
  end;
  Result := HasMetaData();
end;

function TImgData.ReadJpegFile(const AFileName: string): boolean;
var
  fs: TFilestream;
begin
  ClearSections;
  TiffFmt := false;  // default mode
  fs := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    try
      result := ReadJpegSections(fs);
    except
      result := false;
    end;
  finally
    fs.Free;
  end;
end;

function TImgData.ReadTiffSections(AStream: TStream): boolean;
var
  itemLen: Integer;
  fmt: string[2];
begin
  Result := true;
  AStream.ReadBuffer(fmt[1], 2);
  if (fmt <> 'II') and (fmt <> 'MM') then
  begin
    Result := false;
    exit;
  end;

  SetLength(Sections[1].Data, 6);
  AStream.Read(Sections[1].Data[1], 6);
{
  // length calculations are inconsistant for TIFFs
  lh := byte(Sections[1].data[1]);
  ll := byte(Sections[1].data[2]);

  if MotorolaOrder
    then itemlen := (lh shl 8) or ll
    else itemlen := (ll shl 8) or lh;
}
//  itemlen := (ll shl 8) or lh;

  itemLen := TiffReadLimit;

  SetLength(Sections[1].Data, itemLen);
  AStream.Read(Sections[1].Data[1], itemLen);

  SectionCnt := 1;
  EXIFsegment := @sections[1];

  EXIFobj := TImageInfo.Create(self, BuildList);
  EXIFobj.TraceLevel := TraceLevel;
  ExifObj.TiffFmt := TiffFmt;
  ExifObj.TraceStr := '';
  EXIFsegment := @sections[SectionCnt];
  ExifObj.DataBuff := Sections[1].Data;
  ExifObj.parent.DataBuff :=  Sections[1].data;
  ExifObj.MotorolaOrder := fmt = 'MM';
  EXIFobj.ProcessExifDir(1, -7 , itemlen);
  EXIFobj.Calc35Equiv();
end;

function TImgData.ReadTiffFile(const AFileName: string): boolean;
var
  fs: TFileStream;
begin
  TiffFmt := true;
  ClearSections;
  fs := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    try
      Result := ReadTiffSections(fs);
    except
      Result := false;
    end;
  finally
    fs.Free;
  end;
  TiffFmt := false;
end;

Procedure TImgData.ProcessEXIF;
var
  hdr: ansistring;
  offset: integer;
begin
  if ExifSegment = nil then
    exit;

  if not Assigned(ExifObj) then
    ExifObj := TImageInfo.Create(self, BuildList);
  hdr := copy(EXIFsegment^.Data, 3, Length(validExifHeader));
  if hdr <> validExifHeader then
  begin
    FErrStr := 'Incorrect Exif header';
    exit;
  end;
  if copy(EXIFsegment^.Data, 9, 2) = 'II' then
    MotorolaOrder := false
  else if copy(EXIFsegment^.Data, 9, 2) = 'MM' then
    MotorolaOrder := true
  else
  begin
    FErrStr := 'Invalid Exif alignment marker';
    exit;
  end;
  ExifObj.TraceStr := '';
  ExifObj.DataBuff := DataBuff;
  ExifObj.MotorolaOrder := MotorolaOrder;

  offset := Get32u(17-4);
  if offset = 0
    then ExifObj.ProcessExifDir(17, 9, EXIFsegment^.Size-6)
    else ExifObj.ProcessExifDir(9+offset, 9, EXIFsegment^.Size-6);
  if ErrStr <> '' then
    ExifObj.Calc35Equiv();

  ExifObj.ProcessThumbnail;
end;

procedure TImgData.Reset;
begin
  SectionCnt := 0;
  ExifSegment := nil;
  IPTCSegment := nil;
  FComment := '';
//  CommentSegment := nil;
  HeaderSegment := nil;
  FFilename := '';
  FFileDateTime := 0;
  FFileSize := 0;
  FErrStr := '';
  FreeAndNil(ExifObj);
  FreeAndNil(IptcObj);
  MotorolaOrder := false;
end;

function TImgData.GetHeight: Integer;
begin
  if (EXIFObj <> nil) and (ExifObj.Height > 0) then
    Result := ExifObj.Height
  else
    Result := FHeight;
end;

function TImgData.GetResolutionUnit: String;
const
  RESOLUTION_UNIT: array[0..2] of string = ('none', 'inches', 'cm');
var
  v: variant;
  b: Byte;
begin
  Result := '';
  if ExifObj <> nil then begin
    v := ExifObj.GetTagValue('ResolutionUnit');   // 1=none, 2=Inch, 3=cm
    if not VarIsNull(v) and (v >= 1) and (v <= 3) then
      Result := RESOLUTION_UNIT[byte(v)-1];
  end;
  if (Result = '') and (HeaderSegment <> nil) then begin
    b := byte(HeaderSegment^.Data[10]);           // 0=none, 1=Inch, 2=cm
    if (b <= 2) then
      Result := RESOLUTION_UNIT[b];
  end;
end;

function TImgData.GetWidth: Integer;
begin
  if (ExifObj <> nil) and (ExifObj.Width > 0) then
    Result := ExifObj.Width
  else
    Result := FWidth;
end;

function TImgData.GetXResolution: Integer;
var
  v: variant;
  pw: PWord;
begin
  Result := 0;
  if (ExifObj <> nil) then begin
    v := ExifObj.GetTagValue('XResolution');
    if not VarIsNull(v) then
      Result := v;
  end;
  if (Result <= 0) and (HeaderSegment <> nil) then begin
    pw := @HeaderSegment^.Data[11];
    Result := BEToN(pw^);
  end;
end;

function TImgData.GetYResolution: Integer;
var
  v: variant;
  pw: PWord;
begin
  Result := 0;
  if ExifObj <> nil then begin
    v := ExifObj.GetTagValue('YResolution');
    if not VarIsNull(v) then
      Result := v;
  end;
  if (Result <= 0) and (HeaderSegment <> nil) then begin
    pw := @HeaderSegment^.Data[13];
    Result := BEToN(pw^);
  end;
end;

function TImgData.HasMetaData: boolean;
begin
  result := HasExif or HasComment or HasIPTC;
end;

function TImgData.HasEXIF: boolean;
begin
  result := Assigned(ExifObj); //ExifObj <> nl; //(EXIFsegment <> nil);
end;

function TImgData.HasThumbnail: boolean;
begin
  Result := Assigned(ExifObj) and ExifObj.HasThumbnail;
end;

function TImgData.HasIPTC: boolean;
begin
  result := (IPTCsegment <> nil);
end;

function TImgData.HasComment: boolean;
begin
  result := FComment <> '';
end;

// WARNING: The calling routine must destroy the returned stringlist!
function TImgData.ReadIPTCStrings(const AFilename: String): TStringList;
begin
  Result := TStringList.Create;
  ReadIPTCStrings(AFileName, Result);
  if Result.Count = 0 then
    FreeAndNil(Result);
end;

procedure TImgData.ReadIPTCStrings(const AFileName: String; AList: TStrings);
begin
  if ProcessFile(AFilename) and HasIPTC then
    IPTCObj.ParseIPTCStrings(IPTCSegment^.Data, AList);
end;

// WARNING: The calling procedure must destroy the StringList created here!
function TImgData.MetaDataToXML: TStringList;
begin
  Result := TStringList.Create;
  MetaDataToXML(Result);
end;

procedure TImgData.MetaDataToXML(AList: TStrings);
var
  sr: TSearchRec;
begin
  Assert(AList <> nil, 'MetaDataToXML called with AList=nil.');

  if FindFirst(Filename,faAnyFile, sr) <> 0 then
  begin
    FindClose(sr);
    exit;
  end;

  AList.Add('<dImageFile>');
  AList.Add('   <OSdata>');
  AList.Add('      <name>' + ExtractFileName(sr.Name) + '</name>');
  AList.Add('      <path>' + ExtractFilePath(Filename) + '</path>');
  AList.Add('      <size>' + IntToStr(sr.Size) + '</size>');
  AList.Add('      <date>' + DateToStr(FileDateToDateTime(sr.time)) + '</date>');
  AList.Add('   </OSdata>');

  if ExifObj <> nil then
    ExifObj.EXIFArrayToXML(AList);

  if IptcObj <> nil then
    IptcObj.IPTCArrayToXML(AList);

  AList.Add('</dImageFile>');
end;

{$IFDEF dEXIFpredeclare}

initialization
  ImgData := TImgData.create;
finalization
  ImgData.Free;
{$ENDIF}

end.
























