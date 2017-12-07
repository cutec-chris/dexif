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

  // To be used in Exifobj.IterateFoundTags
  GenericEXIF = 0;
  CustomEXIF = 1;

//  AllEXIF = -1;
  GenNone = 0;
  GenAll = 255;
  GenString = 2;
  GenList = 4;
//  VLMin = 0;
//  VLMax = 1;

type
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
    FParent: TObject;       // must be cast to TImgData, can't be done here due to unit circular reference
    FExifVersion: string;

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

    FIterator: integer;
    FThumbIterator: integer;

      // Getter / setter
    function GetDateTimeOriginal: TDateTime;
    procedure SetDateTimeOriginal(const AValue: TDateTime);

    function GetDateTimeDigitized: TDateTime;
    procedure SetDateTimeDigitized(const AValue: TDateTime);

    function GetDateTimeModified: TDateTime;
    procedure SetDateTimeModified(const AValue: TDateTime);

    function GetArtist: String;
    procedure SetArtist(v: String);

    function GetExifComment: String; overload;
    procedure SetExifComment(AValue: String);
    function GetUserComment(const ATag: TTagEntry): String; overload;

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

    function GetVersion(ATag: TTagEntry): String;

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
    function GetThumbTagValueAsString(ATagName: String): string;
    procedure SetThumbTagValueAsString(ATagName: String; AValue: String);

    procedure InternalGetBinaryTagValue(const ATag: TTagEntry; var ABuffer: ansistring);
    function InternalGetTagValue(const ATag: TTagEntry): Variant;
    function InternalGetTagValueAsString(const ATag: TTagEntry): String;
    procedure InternalSetTagValue(const ATagName: String; AValue: Variant;
      ATagTypes: TTagTypes; ABinaryData: Pointer = nil; ABinaryDataCount: Word = 0);
    function BinaryTagToStr(const ATag: TTagEntry): String;
    function BinaryTagToVar(const ATag: TTagEntry): Variant;
    function NumericTagToVar(ABuffer: Pointer; ATagType: Integer): Variant;
    procedure VarToNumericTag(AValue:variant; ATag: PTagEntry);

    // misc
    function CreateTagPtr(const ATagDef: TTagEntry; IsThumbTag: Boolean; AParentID: Word = 0): PTagEntry;
    function FindTagPtr(const ATagDef: TTagEntry; IsThumbTag: Boolean): PTagEntry;

    (*
    function GetTagPtr(ATagTypes: TTagTypes; ATagID: Word; AForceCreate: Boolean=false;
      AParentID: word=0; ATagType: word=65535): PTagEntry;
      *)
    procedure RemoveTag(ATagTypes: TTagTypes; ATagID: Word; AParentID: Word=0);

    procedure ClearDirStack;
    procedure PushDirStack(dirStart, offsetbase: Integer);
    function TestDirStack(dirStart, offsetbase: Integer): boolean;

  protected
    function AddTagToArray(ANewTag: iTag): integer;
    function AddTagToThumbArray(ANewTag: iTag): integer;
    procedure Calc35Equiv;
    function CvtInt(ABuffer: Pointer; ABufferSize: Integer): Longint;
    function Decode: Boolean; 
    function ExifDateToDateTime(ARawStr: ansistring): TDateTime;
    procedure ExtractThumbnail;
    function FormatNumber(ABuffer: PByte; ABufferSize: Integer;
      AFmt: integer; AFmtStr: string; ADecodeStr: string=''): String;
    function GetNumber(ABuffer: PByte; ABufferSize: Integer;
      AFmt: integer): double;
    function LookupRatio: double;

  public
    MaxTag: integer;
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

  public
    constructor Create(AParent: TObject; BuildCode: integer = GenAll);
    procedure Assign(source: TImageInfo);
    destructor Destroy; override;

    // Date/time routines
    procedure AdjDateTime(ADays, AHours, AMins, ASecs: integer);
    function  GetImgDateTime: TDateTime;

    // Manufacturer-specific
    procedure AddMSTag(ATagName: String; ARawStr: ansistring; AType: word);

    // Iterate through found tags
    procedure ResetIterator;
    procedure ResetThumbIterator;
    function IterateFoundTags(TagId:integer; var retVal:TTagEntry):boolean;
    function IterateFoundThumbTags(TagId: integer;
      var retVal: TTagEntry): boolean;

    // Collective output
    procedure EXIFArrayToXML(AList: TStrings); overload;
    function ToShortString: String;   //  Summarizes in a single line
    function ToLongString(ALabelWidth: Integer = 15): String;

    // Special actions
    procedure AdjExifSize(AHeight, AWidth: Integer);

    // Looking up tags and tag values
    function GetRawFloat(ATagName: String): double;
    function GetRawInt(ATagName: String): integer;
    function GetTagByDesc(SearchStr: String): TTagEntry;
    function LookupTagIndex(ATagName: String): integer; virtual;
//    function LookupTagVal(ATagName: String): String; virtual;
    function LookupTagDefn(ATagName: String): integer;
    function LookupTagByDesc(ADesc: String): integer;
    function LookupTagInt(ATagName: String): integer;

    // Tag values as variant
    property TagValue[ATagName: String]: Variant
        read GetTagValue write SetTagValue; default;

    // Tag values as string
    property TagValueAsString[ATagName: String]: String
        read GetTagValueAsString write SetTagValueAsString;

    // Accessing entire tag record
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
    property ExifVersion: String
        read FExifVersion;
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

  public
    // General processing, called internally
    procedure ProcessExifDir(DirStart, OffsetBase, ExifLength: LongInt;
      ATagType: TTagType = ttExif; APrefix: string=''; AParentID: word=0);
    procedure ProcessHWSpecific(AMakerBuff: ansistring;
      TagTbl: array of TTagEntry; ADirStart, AMakerOffset: Longint;
      spOffset: integer = 0);

  public
    // Thumbnail
    procedure CreateThumbnail(AThumbnailSize: Integer = DEFAULT_THUMBNAIL_SIZE);
    function HasThumbnail: boolean;
    procedure ProcessThumbnail;
    procedure RemoveThumbnail;
    procedure LoadThumbnailFromStream(AStream: TStream);
    procedure SaveThumbnailToStream(AStream: TStream);
    property ThumbnailBuffer: TBytes
        read FThumbnailBuffer;
    property ThumbTagByID[ATagID: Word]: TTagEntry
        read GetThumbTagByID write SetThumbTagByID;
    property ThumbTagByIndex[AIndex: Integer]: TTagEntry
        read GetThumbTagByIndex write SetThumbTagByIndex;
    property ThumbTagCount: Integer
        read fiThumbCount;
    property ThumbTagValue[ATagName: String]: variant
        read GetThumbTagValue write SetThumbTagValue;
    property ThumbTagValueAsString[ATagName: String]: String
        read GetThumbTagValueAsString;

    property Parent: TObject
        read FParent;
  end; // TInfoData

var
  CurTagArray: TImageInfo = nil;
  fmtInt: tfmtInt = defIntFmt;
  fmtReal: tfmtReal = defRealFmt;
  fmtFrac: tfmtFrac = defFracFmt;

  ExifNonThumbnailLength : integer;
  ShowTags: integer;
  ExifTrace: integer = 0;

function FindExifTagDefByID(ATagID: Word): PTagEntry;
function FindGPSTagDefByID(ATagID: Word): PTagEntry;

function FindExifTagDefByName(ATagName: String): PTagEntry;
function FindGPSTagDefByName(ATagName: String): PTagEntry;

function LookupType(idx: integer): String;


implementation

uses
  dMetadata, msData;

const
// Compression Type Constants
  JPEG_COMP_TYPE = 6;
  TIFF_COMP_TYPE = 1;

  GPSCnt = 32;
  ExifTagCnt = 251;  // NOTE: was 250 before, but "count" is 251
  TotalTagCnt = GPSCnt + ExifTagCnt;

{ Many tags added based on Php4 source...
    http://lxr.php.net/source/php4/ext/exif/exif.c

  See also: https://sno.phy.queensu.ca/~phil/exiftool/TagNames/EXIF.html  }
var
 TagTable : array [0..ExifTagCnt-1] of TTagEntry =
// TagTable : array of TTagEntry =
// TagTable : TTagDefArray [0..ExifTagCnt] =
// TagTable: TTagDefArray =
 ((TID:0; TType:2; Tag:$0001; Count:1; Name:'InteroperabilityIndex'  ),         {0}
  (TID:0; TType:7; Tag:$0002; Count:1; Name:'InteroperabilityVersion';
    Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:4; Callback:VersionCallback),
  (TID:0; TType:2; Tag:$000B; Count:1; Name:'ACDComment'             ),
  (TID:0; TType:4; Tag:$00FE; Count:1; Name:'NewSubfileType'         ),
  (TID:0; TType:3; Tag:$00FF; Count:1; Name:'SubfileType'            ),
  (TID:0; TType:4; Tag:$0100; ParentID:$0000; Count:1; Name:'ImageWidth'),
  (TID:0; TType:4; Tag:$0101; ParentID:$0000; Count:1; Name:'ImageLength'),
  (TID:0; TType:3; Tag:$0102; ParentID:$0000; Count:3; Name:'BitsPerSample'),
  (TID:0; TType:3; Tag:$0103; ParentID:$0000; Count:1; Name:'Compression';
    Desc:''; Code:'6:Jpeg,3:Uncompressed,1:TIFF'),
  (TID:0; TType:3; Tag:$0106; ParentID:$0000; Count:1; Name:'PhotometricInterpretation';
    Desc:''; Code:'1:Monochrome, 2:RGB, 6:YCbCr'),
  (TID:0; TType:3; Tag:$010A; ParentID:$0000; Count:1; Name:'FillOrder'),         {10}
  (TID:0; TType:2; Tag:$010D; ParentID:$0000; Count:1; Name:'DocumentName'),
  (TID:0; TType:2; Tag:$010E; ParentID:$0000; Count:1; Name:'ImageDescription'),
  (TID:0; TType:2; Tag:$010F; ParentID:$0000; Count:1; Name:'Make'),
  (TID:0; TType:2; Tag:$0110; ParentID:$0000; Count:1; Name:'Model'),
  (TID:0; TType:4; Tag:$0111; ParentID:$0000; Count:1; Name:'StripOffsets'),
  (TID:0; TType:3; Tag:$0112; ParentID:$0000; Count:1; Name:'Orientation';
    Desc:''; Code:'1:Horizontal (normal),2:Mirror horizontal,3:Rotate 180,'+
                  '4:Mirror vertical,5:Mirror horizontal and rotate 270 CW,'+
                  '6:Rotate 90 CW,7:Mirror horizontal and rotate 90 CW,'+
                  '8:Rotate 270 CW'),
  (TID:0; TType:3; Tag:$0115; ParentID:$0000; Count:1; Name:'SamplesPerPixel'),
  (TID:0; TType:4; Tag:$0116; ParentID:$0000; Count:1; Name:'RowsPerStrip'),
  (TID:0; TType:4; Tag:$0117; ParentID:$0000; Count:1; Name:'StripByteCounts'),
  (TID:0; TType:3; Tag:$0118; ParentID:$0000; Count:1; Name:'MinSampleValue'),         {20}
  (TID:0; TType:3; Tag:$0119; ParentID:$0000; Count:1; Name:'MaxSampleValue'),
  (TID:0; TType:5; Tag:$011A; ParentID:$0000; Count:1; Name:'XResolution'),
//    Desc:''; Code:''; Data:''; Raw:''; FormatS:'%f'),
  (TID:0; TType:5; Tag:$011B; ParentID:$0000; Count:1; Name:'YResolution'),
//    Desc:''; Code:''; Data:''; Raw:''; FormatS:'%f'),
  (TID:0; TType:3; Tag:$011C; ParentID:$0000; Count:1; Name:'PlanarConfiguration'),
  (TID:0; TType:2; Tag:$011D; ParentID:$0000; Count:1; Name:'PageName'),
  (TID:0; TType:5; Tag:$011E; ParentID:$0000; Count:1; Name:'XPosition'),
  (TID:0; TType:5; Tag:$011F; ParentID:$0000; Count:1; Name:'YPosition'),
  (TID:0; TType:0; Tag:$0120; ParentID:$0000; Count:1; Name:'FreeOffsets'),
  (TID:0; TType:0; Tag:$0121; ParentID:$0000; Count:1; Name:'FreeByteCounts'),
  (TID:0; TType:3; Tag:$0122; ParentID:$0000; Count:1; Name:'GrayReponseUnit'),         {30}
  (TID:0; TType:0; Tag:$0123; ParentID:$0000; Count:1; Name:'GrayReponseCurve'),
  (TID:0; TType:0; Tag:$0124; ParentID:$0000; Count:1; Name:'T4Options'),
  (TID:0; TType:0; Tag:$0125; ParentID:$0000; Count:1; Name:'T6Options'),
  (TID:0; TType:3; Tag:$0128; ParentID:$0000; Count:1; Name:'ResolutionUnit';
    Desc:''; Code:'1:None specified,2:inches,3:cm'),
  (TID:0; TType:3; Tag:$0129; ParentID:$0000; Count:2; Name:'PageNumber'),
  (TID:0; TType:3; Tag:$012D; ParentID:$0000; Count:768; Name:'TransferFunction'),
  (TID:0; TType:2; Tag:$0131; ParentID:$0000; Count:1; Name:'Software'),
  (TID:0; TType:2; Tag:$0132; ParentID:$0000; Count:1; Name:'DateTime'),
  (TID:0; TType:2; Tag:$013B; ParentID:$0000; Count:1; Name:'Artist'),
  (TID:0; TType:2; Tag:$013C; ParentID:$0000; Count:1; Name:'HostComputer'),         {40}
  (TID:0; TType:3; Tag:$013D; ParentID:$0000; Count:1; Name:'Predictor'),
  (TID:0; TType:5; Tag:$013E; ParentID:$0000; Count:2; Name:'WhitePoint'),
  (TID:0; TType:5; Tag:$013F; ParentID:$0000; Count:6; Name:'PrimaryChromaticities'),
  (TID:0; TType:0; Tag:$0140; ParentID:$0000; Count:1; Name:'ColorMap'),
  (TID:0; TType:3; Tag:$0141; ParentID:$0000; Count:2; Name:'HalfToneHints'),
  (TID:0; TType:4; Tag:$0142; ParentID:$0000; Count:1; Name:'TileWidth'),
  (TID:0; TType:4; Tag:$0143; ParentID:$0000; Count:1; Name:'TileLength'),
  (TID:0; TType:0; Tag:$0144; ParentID:$0000; Count:1; Name:'TileOffsets'),
  (TID:0; TType:0; Tag:$0145; ParentID:$0000; Count:1; Name:'TileByteCounts'),
  (TID:0; TType:0; Tag:$014A; ParentID:$0000; Count:1; Name:'SubIFDs'),         {50}
  (TID:0; TType:3; Tag:$014C; ParentID:$0000; Count:1; Name:'InkSet'),
  (TID:0; TType:0; Tag:$014D; ParentID:$0000; Count:1; Name:'InkNames'),
  (TID:0; TType:0; Tag:$014E; ParentID:$0000; Count:1; Name:'NumberOfInks'),
  (TID:0; TType:0; Tag:$0150; ParentID:$0000; Count:1; Name:'DotRange'),
  (TID:0; TType:2; Tag:$0151; ParentID:$0000; Count:1; Name:'TargetPrinter'),
  (TID:0; TType:0; Tag:$0152; ParentID:$0000; Count:1; Name:'ExtraSample'),
  (TID:0; TType:0; Tag:$0153; ParentID:$0000; Count:1; Name:'SampleFormat'),
  (TID:0; TType:0; Tag:$0154; ParentID:$0000; Count:1; Name:'SMinSampleValue'),
  (TID:0; TType:0; Tag:$0155; ParentID:$0000; Count:1; Name:'SMaxSampleValue'),
  (TID:0; TType:0; Tag:$0156; ParentID:$0000; Count:1; Name:'TransferRange'),         {60}
  (TID:0; TType:0; Tag:$0157; ParentID:$0000; Count:1; Name:'ClipPath'),
  (TID:0; TType:0; Tag:$0158; ParentID:$0000; Count:1; Name:'XClipPathUnits'),
  (TID:0; TType:0; Tag:$0159; ParentID:$0000; Count:1; Name:'YClipPathUnits'),
  (TID:0; TType:0; Tag:$015A; ParentID:$0000; Count:1; Name:'Indexed'),
  (TID:0; TType:0; Tag:$015B; ParentID:$0000; Count:1; Name:'JPEGTables'),
  (TID:0; TType:0; Tag:$015F; ParentID:$0000; Count:1; Name:'OPIProxy'),
  (TID:0; TType:0; Tag:$0200; ParentID:$0000; Count:1; Name:'JPEGProc'),
  (TID:0; TType:4; Tag:$0201; ParentID:$0000; Count:1; Name:'JPEGInterchangeFormat';
     Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:4),
  (TID:0; TType:4; Tag:$0202; ParentID:$0000; Count:1; Name:'JPEGInterchangeFormatLength'),
  (TID:0; TType:0; Tag:$0203; ParentID:$0000; Count:1; Name:'JPEGRestartInterval'),         {70}
  (TID:0; TType:0; Tag:$0205; ParentID:$0000; Count:1; Name:'JPEGLosslessPredictors'),
  (TID:0; TType:0; Tag:$0206; ParentID:$0000; Count:1; Name:'JPEGPointTransforms'),
  (TID:0; TType:0; Tag:$0207; ParentID:$0000; Count:1; Name:'JPEGQTables'),
  (TID:0; TType:0; Tag:$0208; ParentID:$0000; Count:1; Name:'JPEGDCTables'),
  (TID:0; TType:0; Tag:$0209; ParentID:$0000; Count:1; Name:'JPEGACTables'),
  (TID:0; TType:5; Tag:$0211; ParentID:$0000; Count:3; Name:'YCbCrCoefficients'),
  (TID:0; TType:3; Tag:$0212; ParentID:$0000; Count:2; Name:'YCbCrSubSampling'),
  (TID:0; TType:3; Tag:$0213; ParentID:$0000; Count:1; Name:'YCbCrPositioning';
    Desc:''; Code:'1:Centered,2:Co-sited'),
  (TID:0; TType:5; Tag:$0214; ParentID:$0000; Count:6; Name:'ReferenceBlackWhite'),
  (TID:0; TType:1; Tag:$02BC; ParentID:$0000; Count:1; Name:'ExtensibleMetadataPlatform'),     {80}
  (TID:0; TType:0; Tag:$0301; ParentID:$0000; Count:1; Name:'Gamma'),
  (TID:0; TType:0; Tag:$0302; ParentID:$0000; Count:1; Name:'ICCProfileDescriptor'),
  (TID:0; TType:0; Tag:$0303; ParentID:$0000; Count:1; Name:'SRGBRenderingIntent'),
  (TID:0; TType:0; Tag:$0304; ParentID:$0000; Count:1; Name:'ImageTitle'),
  (TID:0; TType:2; Tag:$1000; ParentID:$0000; Count:1; Name:'RelatedImageFileFormat'),
  (TID:0; TType:3; Tag:$1001; ParentID:$0000; Count:1; Name:'RelatedImageWidth'),
  (TID:0; TType:3; Tag:$1002; ParentID:$0000; Count:1; Name:'RelatedImageHeight'),
  (TID:0; TType:0; Tag:$5001; ParentID:$0000; Count:1; Name:'ResolutionXUnit'),
  (TID:0; TType:0; Tag:$5002; ParentID:$0000; Count:1; Name:'ResolutionYUnit'),
  (TID:0; TType:0; Tag:$5003; ParentID:$0000; Count:1; Name:'ResolutionXLengthUnit'),         {90}
  (TID:0; TType:0; Tag:$5004; ParentID:$0000; Count:1; Name:'ResolutionYLengthUnit'),
  (TID:0; TType:0; Tag:$5005; ParentID:$0000; Count:1; Name:'PrintFlags'),
  (TID:0; TType:0; Tag:$5006; ParentID:$0000; Count:1; Name:'PrintFlagsVersion'),
  (TID:0; TType:0; Tag:$5007; ParentID:$0000; Count:1; Name:'PrintFlagsCrop'),
  (TID:0; TType:0; Tag:$5008; ParentID:$0000; Count:1; Name:'PrintFlagsBleedWidth'),
  (TID:0; TType:0; Tag:$5009; ParentID:$0000; Count:1; Name:'PrintFlagsBleedWidthScale'),
  (TID:0; TType:0; Tag:$500A; ParentID:$0000; Count:1; Name:'HalftoneLPI'),
  (TID:0; TType:0; Tag:$500B; ParentID:$0000; Count:1; Name:'HalftoneLPIUnit'),
  (TID:0; TType:0; Tag:$500C; ParentID:$0000; Count:1; Name:'HalftoneDegree'),
  (TID:0; TType:0; Tag:$500D; ParentID:$0000; Count:1; Name:'HalftoneShape'),         {100}
  (TID:0; TType:0; Tag:$500E; ParentID:$0000; Count:1; Name:'HalftoneMisc'),
  (TID:0; TType:0; Tag:$500F; ParentID:$0000; Count:1; Name:'HalftoneScreen'),
  (TID:0; TType:0; Tag:$5010; ParentID:$0000; Count:1; Name:'JPEGQuality'),
  (TID:0; TType:0; Tag:$5011; ParentID:$0000; Count:1; Name:'GridSize'),
  (TID:0; TType:0; Tag:$5012; ParentID:$0000; Count:1; Name:'ThumbnailFormat'),
  (TID:0; TType:0; Tag:$5013; ParentID:$0000; Count:1; Name:'ThumbnailWidth'),
  (TID:0; TType:0; Tag:$5014; ParentID:$0000; Count:1; Name:'ThumbnailHeight'),
  (TID:0; TType:0; Tag:$5015; ParentID:$0000; Count:1; Name:'ThumbnailColorDepth'),
  (TID:0; TType:0; Tag:$5016; ParentID:$0000; Count:1; Name:'ThumbnailPlanes'),
  (TID:0; TType:0; Tag:$5017; ParentID:$0000; Count:1; Name:'ThumbnailRawBytes'),         {110}
  (TID:0; TType:0; Tag:$5018; ParentID:$0000; Count:1; Name:'ThumbnailSize'),
  (TID:0; TType:0; Tag:$5019; ParentID:$0000; Count:1; Name:'ThumbnailCompressedSize'),
  (TID:0; TType:0; Tag:$501A; ParentID:$0000; Count:1; Name:'ColorTransferFunction'),
  (TID:0; TType:0; Tag:$501B; ParentID:$0000; Count:1; Name:'ThumbnailData'),
  (TID:0; TType:0; Tag:$5020; ParentID:$0000; Count:1; Name:'ThumbnailImageWidth'),
  (TID:0; TType:0; Tag:$5021; ParentID:$0000; Count:1; Name:'ThumbnailImageHeight'),
  (TID:0; TType:0; Tag:$5022; ParentID:$0000; Count:1; Name:'ThumbnailBitsPerSample'),
  (TID:0; TType:0; Tag:$5023; ParentID:$0000; Count:1; Name:'ThumbnailCompression'),
  (TID:0; TType:0; Tag:$5024; ParentID:$0000; Count:1; Name:'ThumbnailPhotometricInterp'),
  (TID:0; TType:0; Tag:$5025; ParentID:$0000; Count:1; Name:'ThumbnailImageDescription'),      {120}
  (TID:0; TType:2; Tag:$5026; ParentID:$0000; Count:1; Name:'ThumbnailEquipMake'),
  (TID:0; TType:2; Tag:$5027; ParentID:$0000; Count:1; Name:'ThumbnailEquipModel'),
  (TID:0; TType:0; Tag:$5028; ParentID:$0000; Count:1; Name:'ThumbnailStripOffsets'),
  (TID:0; TType:0; Tag:$5029; ParentID:$0000; Count:1; Name:'ThumbnailOrientation'),
  (TID:0; TType:0; Tag:$502A; ParentID:$0000; Count:1; Name:'ThumbnailSamplesPerPixel'),
  (TID:0; TType:0; Tag:$502B; ParentID:$0000; Count:1; Name:'ThumbnailRowsPerStrip'),
  (TID:0; TType:0; Tag:$502C; ParentID:$0000; Count:1; Name:'ThumbnailStripBytesCount'),
  (TID:0; TType:0; Tag:$502D; ParentID:$0000; Count:1; Name:'ThumbnailResolutionX'),
  (TID:0; TType:0; Tag:$502E; ParentID:$0000; Count:1; Name:'ThumbnailResolutionY'),
  (TID:0; TType:0; Tag:$502F; ParentID:$0000; Count:1; Name:'ThumbnailPlanarConfig'),         {130}
  (TID:0; TType:0; Tag:$5030; ParentID:$0000; Count:1; Name:'ThumbnailResolutionUnit'),
  (TID:0; TType:0; Tag:$5031; ParentID:$0000; Count:1; Name:'ThumbnailTransferFunction'),
  (TID:0; TType:2; Tag:$5032; ParentID:$0000; Count:1; Name:'ThumbnailSoftwareUsed'),
  (TID:0; TType:2; Tag:$5033; ParentID:$0000; Count:1; Name:'ThumbnailDateTime'),
  (TID:0; TType:2; Tag:$5034; ParentID:$0000; Count:1; Name:'ThumbnailArtist'),
  (TID:0; TType:0; Tag:$5035; ParentID:$0000; Count:1; Name:'ThumbnailWhitePoint'),
  (TID:0; TType:0; Tag:$5036; ParentID:$0000; Count:1; Name:'ThumbnailPrimaryChromaticities'),
  (TID:0; TType:0; Tag:$5037; ParentID:$0000; Count:1; Name:'ThumbnailYCbCrCoefficients'),
  (TID:0; TType:0; Tag:$5038; ParentID:$0000; Count:1; Name:'ThumbnailYCbCrSubsampling'),
  (TID:0; TType:0; Tag:$5039; ParentID:$0000; Count:1; Name:'ThumbnailYCbCrPositioning'),  {140}
  (TID:0; TType:0; Tag:$503A; ParentID:$0000; Count:1; Name:'ThumbnailRefBlackWhite'),
  (TID:0; TType:2; Tag:$503B; ParentID:$0000; Count:1; Name:'ThumbnailCopyRight'),
  (TID:0; TType:0; Tag:$5090; ParentID:$0000; Count:1; Name:'LuminanceTable'),
  (TID:0; TType:0; Tag:$5091; ParentID:$0000; Count:1; Name:'ChrominanceTable'),
  (TID:0; TType:0; Tag:$5100; ParentID:$0000; Count:1; Name:'FrameDelay'),
  (TID:0; TType:0; Tag:$5101; ParentID:$0000; Count:1; Name:'LoopCount'),
  (TID:0; TType:0; Tag:$5110; ParentID:$0000; Count:1; Name:'PixelUnit'),
  (TID:0; TType:0; Tag:$5111; ParentID:$0000; Count:1; Name:'PixelPerUnitX'),
  (TID:0; TType:0; Tag:$5112; ParentID:$0000; Count:1; Name:'PixelPerUnitY'),
  (TID:0; TType:0; Tag:$5113; ParentID:$0000; Count:1; Name:'PaletteHistogram'),         {150}
  (TID:0; TType:0; Tag:$800D; ParentID:$0000; Count:1; Name:'ImageID'),
  (TID:0; TType:0; Tag:$80E3; ParentID:$0000; Count:1; Name:'Matteing'),   //* obsoleted by ExtraSamples */
  (TID:0; TType:0; Tag:$80E4; ParentID:$0000; Count:1; Name:'DataType'),   //* obsoleted by SampleFormat */
  (TID:0; TType:0; Tag:$80E5; ParentID:$0000; Count:1; Name:'ImageDepth'),
  (TID:0; TType:0; Tag:$80E6; ParentID:$0000; Count:1; Name:'TileDepth'),
  (TID:0; TType:3; Tag:$828D; ParentID:$0000; Count:2; Name:'CFARepeatPatternDim'),
  (TID:0; TType:1; Tag:$828E; ParentID:$0000; Count:1; Name:'CFAPattern'),  //count: ???
  (TID:0; TType:0; Tag:$828F; ParentID:$0000; Count:1; Name:'BatteryLevel'),
  (TID:0; TType:2; Tag:$8298; ParentID:$0000; Count:1; Name:'Copyright'),
  (TID:0; TType:5; Tag:$829A; ParentID:$8769; Count:1; Name:'ExposureTime';
    Desc:'Exposure time'; Code:''; Data:''; Raw:''; FormatS:'%s sec'; Size:8; Callback:nil), //SSpeedCallback),   {160}
  (TID:0; TType:5; Tag:$829D; ParentID:$8769; Count:1; Name:'FNumber';
    Desc:''; Code:''; Data:''; Raw:''; FormatS:'F%0.1f'),
  (TID:0; TType:4; Tag:$83BB; ParentID:$0000; Count:1; Name:'IPTC/NAA';
    Desc:'IPTC/NAA'),
  (TID:0; TType:0; Tag:$84E3; ParentID:$0000; Count:1; Name:'IT8RasterPadding'),
  (TID:0; TType:0; Tag:$84E5; ParentID:$0000; Count:1; Name:'IT8ColorTable'),
  (TID:0; TType:0; Tag:$8649; ParentID:$0000; Count:1; Name:'ImageResourceInformation'),
  (TID:0; TType:4; Tag:$8769; ParentID:$0000; Count:1; Name:'ExifOffset';
    Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:4),
  (TID:0; TType:0; Tag:$8773; ParentID:$0000; Count:1; Name:'InterColorProfile'),
  (TID:0; TType:3; Tag:$8822; ParentID:$8769; Count:1; Name:'ExposureProgram';
    Desc:''; Code:'0:Not denfined,1:Manual,2:Program AE,3:Aperture-priority AE,'+
                  '4:Shutter speed priority AE,5:Creative (slow speed),'+
                  '6:Action (high speed),7:Portrait,8:Landscape;9:Bulb'),
  (TID:0; TType:2; Tag:$8824; ParentID:$8769; Count:1; Name:'SpectralSensitivity'),
  (TID:0; TType:4; Tag:$8825; ParentID:$0000; Count:1; Name:'GPSInfo';
    Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:4),                                {170}
  (TID:0; TType:3; Tag:$8827; ParentID:$8769; Count:1; Name:'ISOSpeedRatings'),   {171}
  (TID:0; TType:0; Tag:$8828; ParentID:$8769; Count:1; Name:'OECF'),
  (TID:0; TType:0; Tag:$8829; ParentID:$8769; Count:1; Name:'Interlace'),
  (TID:0; TType:8; Tag:$882A; ParentID:$8769; Count:1; Name:'TimeZoneOffset'),
  (TID:0; TType:3; Tag:$882B; ParentID:$8769; Count:1; Name:'SelfTimerMode'),
  (TID:0; TType:7; Tag:$9000; ParentID:$8769; Count:1; Name:'ExifVersion';
    Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:4; Callback:VersionCallback),
  (TID:0; TType:2; Tag:$9003; ParentID:$8769; Count:1; Name:'DateTimeOriginal'),
  (TID:0; TType:2; Tag:$9004; ParentID:$8769; Count:1; Name:'DateTimeDigitized'),
  (TID:0; TType:7; Tag:$9101; ParentID:$8769; Count:1; Name:'ComponentsConfiguration';
    Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:0; Callback:CompCfgCallBack),
  (TID:0; TType:5; Tag:$9102; ParentID:$8769; Count:1; Name:'CompressedBitsPerPixel'),         {180}
  (TID:0; TType:10; Tag:$9201; ParentID:$8769; Count:1; Name:'ShutterSpeedValue';
    Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:0; Callback:SSpeedCallBack),
  (TID:0; TType:5; Tag:$9202; ParentID:$8769; Count:1; Name:'ApertureValue';
    Desc:'Aperture value'; Code:''; Data:''; Raw:''; FormatS:'F%0.1f'),
  (TID:0; TType:10;Tag:$9203; ParentID:$8769; Count:1; Name:'BrightnessValue'),
  (TID:0; TType:10;Tag:$9204; ParentID:$8769; Count:1; Name:'ExposureBiasValue'),
  (TID:0; TType:5; Tag:$9205; ParentID:$8769; Count:1; Name:'MaxApertureValue';
    Desc:''; Code:''; Data:''; Raw:''; FormatS:'F%0.1f'),
  (TID:0; TType:5; Tag:$9206; ParentID:$8769; Count:1; Name:'SubjectDistance'),
  (TID:0; TType:3; Tag:$9207; ParentID:$8769; Count:1; Name:'MeteringMode';
    Desc:'';
    Code:'0:Unknown,1:Average,2:Center,3:Spot,4:Multi-spot,5:Multi-segment,6:Partial'),
  (TID:0; TType:3; Tag:$9208; ParentID:$8769; Count:1; Name:'LightSource';
    Desc:'';
    Code:'0:Unknown,1:Daylight,2:Fluorescent,3:Tungsten,10:Flash,17:Std A,18:Std B,19:Std C'),
  (TID:0; TType:3; Tag:$9209; ParentID:$8769; Count:1; Name:'Flash';
    Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:FlashCallBack),
  (TID:0; TType:5; Tag:$920A; ParentID:$8769; Count:1; Name:'FocalLength';
    Desc:'Focal length'; Code:''; Data:''; Raw:''; FormatS:'%0.1f mm'), {190}
  (TID:0; TType:0; Tag:$920B; ParentID:$8769; Count:1; Name:'FlashEnergy'),
  (TID:0; TType:0; Tag:$920C; ParentID:$8769; Count:1; Name:'SpatialFrequencyResponse'),
  (TID:0; TType:0; Tag:$920D; ParentID:$8769; Count:1; Name:'Noise'),
  (TID:0; TType:0; Tag:$920E; ParentID:$8769; Count:1; Name:'FocalPlaneXResolution';
    Desc:''; code:''; Data:''; Raw:''; FormatS:'%f'; Size:0; CallBack:nil),
  (TID:0; TType:0; Tag:$920F; ParentID:$8769; Count:1; Name:'FocalPlaneYResolution';
    Desc:''; Code:''; Data:''; Raw:''; FormatS:'%f'; Size:0; CallBack:nil),
  (TID:0; TType:0; Tag:$9210; ParentID:$8769; Count:1; Name:'FocalPlaneResolutionUnit';
    Desc:''; Code:'1:None specified,2:inches,3:cm'),
  (TID:0; TType:4; Tag:$9211; ParentID:$8769; Count:1; Name:'ImageNumber'),
  (TID:0; TType:2; Tag:$9212; ParentID:$8769; Count:1; Name:'SecurityClassification'),
  (TID:0; TType:2; Tag:$9213; ParentID:$8769; Count:1; Name:'ImageHistory'),
  (TID:0; TType:3; Tag:$9214; ParentID:$8769; Count:2; Name:'SubjectLocation'),         {200}
  (TID:0; TType:0; Tag:$9215; ParentID:$8769; Count:1; Name:'ExposureIndex'),
  (TID:0; TType:0; Tag:$9216; ParentID:$8769; Count:1; Name:'TIFF/EPStandardID'),
  (TID:0; TType:0; Tag:$9217; ParentID:$8769; Count:1; Name:'SensingMethod'),
  (TID:0; TType:0; Tag:$923F; ParentID:$8769; Count:1; Name:'StoNits'),
  (TID:0; TType:7; Tag:$927C; ParentID:$8769; Count:1; Name:'MakerNote'),
  (TID:0; TType:7; Tag:$9286; ParentID:$8769; Count:1; Name:'UserComment'),
  (TID:0; TType:2; Tag:$9290; ParentID:$8769; Count:1; Name:'SubSecTime'),
  (TID:0; TType:2; Tag:$9291; ParentID:$8769; Count:1; Name:'SubSecTimeOriginal'),
  (TID:0; TType:2; Tag:$9292; ParentID:$8769; Count:1; Name:'SubSecTimeDigitized'),
  (TID:0; TType:0; Tag:$953C; ParentID:$0000; Count:1; Name:'ImageSourceData'),  // "Adobe Photoshop Document Data Block": 8BIM...  {210}
  (TID:0; TType:0; Tag:$9C9B; ParentID:$0000; Count:1; Name:'Title';
    Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:xpTranslate),  // Win XP specific, Unicode
  (TID:0; TType:0; Tag:$9C9C; ParentID:$0000; Count:1; Name:'Comments';
    Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:xpTranslate),  // Win XP specific, Unicode
  (TID:0; TType:0; Tag:$9C9D; ParentID:$0000; Count:1; Name:'Author';
    Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:xpTranslate),  // Win XP specific, Unicode
  (TID:0; TType:0; Tag:$9C9E; ParentID:$0000; Count:1; Name:'Keywords';
    Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:xpTranslate),  // Win XP specific, Unicode
  (TID:0; TType:0; Tag:$9C9F; ParentID:$0000; Count:1; Name:'Subject';
    Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:xpTranslate),  // Win XP specific, Unicode
  (TID:0; TType:0; Tag:$A000; ParentID:$8769; Count:1; Name:'FlashPixVersion'),
  (TID:0; TType:3; Tag:$A001; ParentID:$8769; Count:1; Name:'ColorSpace';
    Desc:''; Code:'0:sBW,1:sRGB'),
  (TID:0; TType:3; Tag:$A002; ParentID:$8769; Count:1; Name:'ExifImageWidth'),
  (TID:0; TType:3; Tag:$A003; ParentID:$8769; Count:1; Name:'ExifImageLength'),
  (TID:0; TType:2; Tag:$A004; ParentID:$8769; Count:1; Name:'RelatedSoundFile'),         {220}
  (TID:0; TType:0; Tag:$A005; ParentID:$8769; Count:1; Name:'InteroperabilityOffset'),
  (TID:0; TType:5; Tag:$A20B; ParentID:$8769; Count:1; Name:'FlashEnergy'),    // TID:0;TType:0;ICode: 2;Tag: $920B in TIFF/EP
  (TID:0; TType:0; Tag:$A20C; ParentID:$8769; Count:1; Name:'SpatialFrequencyResponse'),   // TID:0;TType:0;ICode: 2;Tag: $920C    -  -
  (TID:0; TType:5; Tag:$A20E; ParentID:$8769; Count:1; Name:'FocalPlaneXResolution';
    Desc:''; code:''; Data:''; Raw:''; FormatS:'%f'; Size:0; CallBack:nil),
  (TID:0; TType:5; Tag:$A20F; ParentID:$8769; Count:1; Name:'FocalPlaneYResolution';
    Desc:''; code:''; Data:''; Raw:''; FormatS:'%f'; Size:0; CallBack:nil),
  (TID:0; TType:3; Tag:$A210; ParentID:$8769; Count:1; Name:'FocalPlaneResolutionUnit';
    Desc:''; Code:'1:None specified,2:inches,3:cm'),      // TID:0;TType:0;ICode: 2;Tag: $9210    -  -
  (TID:0; TType:0; Tag:$A211; ParentID:$8769; Count:1; Name:'ImageNumber'),
  (TID:0; TType:0; Tag:$A212; ParentID:$8769; Count:1; Name:'SecurityClassification'),
  (TID:0; TType:0; Tag:$A213; ParentID:$8769; Count:1; Name:'ImageHistory'),
  (TID:0; TType:3; Tag:$A214; ParentID:$8769; Count:2; Name:'SubjectLocation'),        {230}
  (TID:0; TType:5; Tag:$A215; ParentID:$8769; Count:1; Name:'ExposureIndex'),
  (TID:0; TType:0; Tag:$A216; ParentID:$8769; Count:1; Name:'TIFF/EPStandardID';
    Desc:'TIFF/EPStandardID'),
  (TID:0; TType:3; Tag:$A217; ParentID:$8769; Count:1; Name:'SensingMethod'; Desc:'';
    Code:'0:Unknown,1:Not defined,2:One-chip color area,3:Two-chip color area,'+
         '4:Three-chip color area,5:Color sequential area,7:Trilinear,'+
         '8:Color-sequential linear'),
  (TID:0; TType:1; Tag:$A300; ParentID:$8769; Count:1; Name:'FileSource'; Desc:'';
    Code:'0:Unknown,1:Film scanner,2:Reflection print scanner,3:Digital camera'),
  (TID:0; TType:7; Tag:$A301; ParentID:$8769; Count:1; Name:'SceneType';
    Desc:''; Code:'0:Unknown,1:Directly Photographed'),
  (TID:0; TType:7; Tag:$A302; ParentID:$8769; Count:1; Name:'CFAPattern'),
  (TID:0; TType:3; Tag:$A401; ParentID:$8769; Count:1; Name:'CustomRendered';
    Desc:''; Code:'0:Normal,1:Custom'),
  (TID:0; TType:3; Tag:$A402; ParentID:$8769; Count:1; Name:'ExposureMode';
    Desc:''; Code:'0:Auto,1:Manual,2:Auto bracket'),
  (TID:0; TType:3; Tag:$A403; ParentID:$8769; Count:1; Name:'WhiteBalance';
    Desc:''; Code:'0:Auto,1:Manual'),
  (TID:0; TType:5; Tag:$A404; ParentID:$8769; Count:1; Name:'DigitalZoomRatio'),        {240}
  (TID:0; TType:3; Tag:$A405; ParentID:$8769; Count:1; Name:'FocalLengthIn35mmFilm';
    Desc:'Focal Length in 35mm Film'; Code:''; Data:''; Raw:''; FormatS:'%.1f mm'),
  (TID:0; TType:3; Tag:$A406; ParentID:$8769; Count:1; Name:'SceneCaptureType';
    Desc:''; Code:'0:Standard,1:Landscape,2:Portrait,3:Night scene'),
  (TID:0; TType:3; Tag:$A407; ParentID:$8769; Count:1; Name:'GainControl'; Desc:'';
    Code:'0:None,1:Low gain up,2:High gain up,3:Low gain down,4:High gain down'),
  (TID:0; TType:3; Tag:$A408; ParentID:$8769; Count:1; Name:'Contrast';
    Desc:''; Code:'0:Normal,1:Soft,2:Hard'),
  (TID:0; TType:3; Tag:$A409; ParentID:$8769; Count:1; Name:'Saturation';
    Desc:''; Code:'0:Normal,1:Low,2:High'),
  (TID:0; TType:3; Tag:$A40A; ParentID:$8769; Count:1; Name:'Sharpness';
    Desc:''; Code:'0:Normal,1:Soft,2:Hard'),
  (TID:0; TType:0; Tag:$A40B; ParentID:$8769; Count:1; Name:'DeviceSettingDescription'),
  (TID:0; TType:3; Tag:$A40C; ParentID:$8769; Count:1; Name:'SubjectDistanceRange';    {250}
    Desc:''; Code:'0:Unknown,1:Macro,2:Close view,3:Distant view'),
  (TID:0; TType:2; Tag:$A420; ParentID:$8769; Count:1; Name:'ImageUniqueID';
    Desc:'';  Code:'0:Close view,1:Distant view'),
  (TID:0; TType:0; Tag:0;     ParentID:$0000; Count:1; Name:'Unknown')
);

 GPSTable : array [0..GPSCnt-1] of TTagEntry = (
  (TID:0; TType:1; Tag:$000; ParentID:$8825; Count:4; Name:'GPSVersionID';
    Desc:''; Code:''; Data:''; RAw:''; FormatS:''; Size:0; CallBack:GpsVersionID),
  (TID:0; TType:2; Tag:$001; ParentID:$8825; Count:2; Name:'GPSLatitudeRef'; Desc:''),
  (TID:0; TType:5; Tag:$002; ParentID:$8825; Count:3; Name:'GPSLatitude';
    Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:GpsPosn),
  (TID:0; TType:2; Tag:$003; ParentID:$8825; Count:2; Name:'GPSLongitudeRef';Desc:''),
  (TID:0; TType:5; Tag:$004; ParentID:$8825; Count:3; Name:'GPSLongitude';
    Desc:''; Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:GpsPosn),
  (TID:0; TType:1; Tag:$005; ParentID:$8825; Count:1; Name:'GPSAltitudeRef'; Desc:'';
    Code:'0:Above Sealevel,1:Below Sealevel'),
  (TID:0; TType:5; Tag:$006; ParentID:$8825; Count:1; Name:'GPSAltitude'; Desc:'';
    Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:GpsAltitude),
  (TID:0; TType:5; Tag:$007; ParentID:$8825; Count:3; Name:'GPSTimeStamp'; Desc:'';
    Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:CvtTime),
  (TID:0; TType:2; Tag:$008; ParentID:$8825; Count:1; Name:'GPSSatellites'; Desc:''),
  (TID:0; TType:2; Tag:$009; ParentID:$8825; Count:2; Name:'GPSStatus';
    Desc:''; Code:'A:Active;V:Void'),
  (TID:0; TType:2; Tag:$00A; ParentID:$8825; Count:2; Name:'GPSMeasureMode';
    Desc:''; Code:'2:2D,3:3D'),
  (TID:0; TType:5; Tag:$00B; ParentID:$8825; Count:1; Name:'GPSDOP'; Desc:''),
  (TID:0; TType:2; Tag:$00C; ParentID:$8825; Count:2; Name:'GPSSpeedRef';
    Desc:''; Code:'K:km/h,M:mph,N:knots'),
  (TID:0; TType:5; Tag:$00D; ParentID:$8825; Count:1; Name:'GPSSpeed'; Desc:''),
  (TID:0; TType:2; Tag:$00E; ParentID:$8825; Count:2; Name:'GPSTrackRef';
    Desc:''; Code:'M:Magnetic North,T:True North'),
  (TID:0; TType:5; Tag:$00F; ParentID:$8825; Count:1; Name:'GPSTrack'; Desc:''),
  (TID:0; TType:2; Tag:$010; ParentID:$8825; Count:2; Name:'GPSImageDirectionRef';
    Desc:''; Code:'M:Magnetic North,T:True North'),
  (TID:0; TType:5; Tag:$011; ParentID:$8825; Count:1; Name:'GPSImageDirection'; Desc:''),
  (TID:0; TType:2; Tag:$012; ParentID:$8825; Count:1; Name:'GPSMapDatum'; Desc:''),
  (TID:0; TType:2; Tag:$013; ParentID:$8825; Count:2; Name:'GPSDestLatitudeRef';
    Desc:''; Code:'N:North,S:South'),
  (TID:0; TType:5; Tag:$014; ParentID:$8825; Count:3; Name:'GPSDestLatitude'; Desc:'';
    Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:GpsPosn),
  (TID:0; TType:2; Tag:$015; ParentID:$8825; Count:2; Name:'GPSDestLongitudeRef';
    Desc:''; Code: 'E:East,W:West'),
  (TID:0; TType:5; Tag:$016; ParentID:$8825; Count:3; Name:'GPSDestLongitude'; Desc:'';
    Code:''; Data:''; Raw:''; FormatS:''; Size:0; CallBack:GpsPosn),
  (TID:0; TType:2; Tag:$017; ParentID:$8825; Count:2; Name:'GPSDestBearingRef';
    Desc:''; Code:'M:Magnetic North,T:True North'),
  (TID:0; TType:5; Tag:$018; ParentID:$8825; Count:1; Name:'GPSDestBearing'; Desc:''),
  (TID:0; TType:2; Tag:$019; ParentID:$8825; Count:2; Name:'GPSDestDistanceRef';
    Desc:''; Code:'K:Kilometers,M:Miles,N:Nautic Miles'),
  (TID:0; TType:5; Tag:$01A; ParentID:$8825; Count:1; Name:'GPSDestDistance'; Desc:''),
  (TID:0; TType:7; Tag:$01B; ParentID:$8825; Count:1; Name:'GPSProcessingMode'; Desc:''),
  (TID:0; TType:7; Tag:$01C; ParentID:$8825; Count:1; Name:'GPSAreaInformation'; Desc:''),
  (TID:0; TType:2; Tag:$01D; ParentID:$8825; Count:7; Name:'GPSDateStamp'; Desc:''),
  (TID:0; TType:3; Tag:$01E; ParentID:$8825; Count:1; Name:'GPSDifferential';
    Desc:''; Code:'0:No Correction,1:Differential Correction'),
  (TID:0; TType:5; Tag:$01F; ParentID:$8825; Count:1; Name:'GPSHPositioningError'; Desc:'')
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

constructor TImageInfo.Create(AParent: TObject; buildCode: integer = GenAll);
begin
  inherited Create;
  FParent := AParent;
  LoadTagDescs(True);  // initialize global structures
  FITagCount := 0;
  buildList := BuildCode;
  ClearDirStack;
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
function TImageInfo.LookupTagIndex(ATagName: String): integer;
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

(*
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
  *)

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
    Result := TImgData(Parent).FileDatetime;
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
  v := FormatDateTime(EXIF_DATETIME_FORMAT, AValue);
  SetTagValue('DateTimeOriginal', v);
end;
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
  v := FormatDateTime(EXIF_DATETIME_FORMAT, AValue);
  SetTagValue('DateTimeDigitized', v);
end;

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
  v := FormatDateTime(EXIF_DATETIME_FORMAT, AValue);
  SetTagValue('DateTime', v);
end;

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
  if not ((ANewTag.Name = '') or (ANewTag.Name = 'Unknown')) then     // Empty fields are masked out
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

function TImageInfo.CvtInt(ABuffer: Pointer; ABufferSize: Integer): Longint;
var
  i: integer;
  r: Int64;
  P: PByte;
begin
  r := 0;
  if MotorolaOrder then begin
    P := PByte(ABuffer);
    for i := 1 to ABufferSize do begin
      r := r*256 + P^;
      inc(P);
    end;
  end else begin
    P := PByte(ABuffer);
    inc(P, ABufferSize - 1);
    for i := 1 to ABufferSize do begin
      r := r*256 + P^;
      dec(P);
    end;
  end;
  Result := LongInt(r);
end;

function TImageInfo.Decode: Boolean;
begin
  Result := TImgData(FParent).Decode;
end;

function TImageInfo.FormatNumber(ABuffer: PByte; ABufferSize: Integer;
  AFmt: integer; AFmtStr: String; ADecodeStr: String=''): String;
var
  P: PByte;
  i, len: integer;
  tmp, tmp2: longint;
  dv: double;
begin
  Result := '';
  len := BYTES_PER_FORMAT[AFmt];
  if len = 0 then
    exit;

  P := ABuffer;
  for i := 0 to min(ABufferSize div len, 128) - 1 do
  begin
    if Result <> '' then
      Result := Result + dExifDataSep;  // Used for data display
    case AFmt of
      FMT_SBYTE,
      FMT_BYTE,
      FMT_USHORT,
      FMT_ULONG,
      FMT_SSHORT,
      FMT_SLONG:
        begin
          tmp := CvtInt(P, len);
          if (ADecodeStr = '') or not Decode then
            Result := Result + defIntFmt(tmp)
          else
            Result := Result + DecodeField(ADecodeStr, IntToStr(tmp));
          end;
      FMT_URATIONAL,
      FMT_SRATIONAL:
        begin
          tmp := CvtInt(P, 4);
          inc(P, 4);
          tmp2 := CvtInt(P, 4);
          dec(P, 4);
          Result := Result + defFracFmt(tmp, tmp2);
          if (ADecodeStr <> '') or not Decode then
            Result := Result + DecodeField(ADecodeStr, Result);
        end;
      FMT_SINGLE,
      FMT_DOUBLE:
        begin
          // not used anyway; not sure how to interpret endian issues
          Result := Result +  '-9999.99';
        end;
      FMT_BINARY:
        if ABufferSize = 1 then begin
          tmp := CvtInt(P, 1);
          if (ADecodeSTr = '') or not Decode then
            Result := Result + DefIntFmt(tmp)
          else
            Result := Result + DecodeField(ADecodeStr, IntToStr(tmp));
        end else
          Result := Result + '?';
    else
      Result := Result + '?';
    end;
    inc(P, len);
  end;

  if AFmtStr <> '' then
  begin
    if Pos('%s', AFmtStr) > 0 then
      Result := Format(AFmtStr, [Result], dExifFmtSettings)
    else begin
      dv := GetNumber(ABuffer, ABufferSize, AFmt);           // wp: Will this always work?
      Result := Format(AFmtStr, [dv], dExifFmtSettings);
    end;
  end;
end;

function TImageInfo.GetNumber(ABuffer: PByte; ABufferSize: Integer;
  AFmt:integer): Double;
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
        Result := CvtInt(ABuffer, ABufferSize);
      FMT_URATIONAL,
      FMT_SRATIONAL:
        begin
          tmp := CvtInt(ABuffer, 4);
          inc(ABuffer, 4);
          tmp2 := CvtInt(ABuffer, 4);
          Result := tmp / tmp2;
        end;
      FMT_SINGLE:
        Result := PSingle(ABuffer)^;
      FMT_DOUBLE:
        Result := PDouble(ABuffer)^;
    end;
  except
  end;
end;

var
  dirStack: String = '';

procedure TImageInfo.ClearDirStack;
begin
  dirStack := '';
end;

procedure TImageInfo.PushDirStack(dirStart, offsetbase:longint);
var
  ts: String;
begin
  ts := '[' + IntToStr(offsetbase) + ':' + IntToStr(dirStart) + ']';
  dirStack := dirStack + ts;
end;

function TImageInfo.TestDirStack(dirStart, offsetbase: Longint): boolean;
var
  ts: String;
begin
  ts := '[' + IntToStr(offsetbase) + ':' + IntToStr(dirStart) + ']';
  result := Pos(ts,dirStack) > 0;
end;
                           (*
//{$DEFINE CreateExifBufDebug}  // uncomment to see written Exif data
{$ifdef CreateExifBufDebug}var CreateExifBufDebug : String;{$endif}

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
begin
  PushDirStack(DirStart, OffsetBase);
  numDirEntries := Get16u(DirStart);
  if (ExifTrace > 0) then
    TraceStr := TraceStr + crlf +
      Format('Directory: Start, entries = %d, %d', [DirStart, numDirEntries]);
  if (DirStart + 2 + numDirEntries*12) > (DirStart + OffsetBase + ExifLength) then
  begin
    TImgData(FParent).SetError('Illegally sized directory');
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
    rawStr := Copy(TImgData(FParent).EXIFsegment^.Data, valuePtr, byteCount);

    fStr := '';
    if BuildList in [GenString, GenAll] then
    begin
      lookUpEntry := FetchTagDefByID(tag, ATagType);

      with lookUpEntry do
      begin
        case tagFormat of
          FMT_UNDEFINED:
            fStr := '"' + StrBefore(rawStr, #0) + '"';
          FMT_STRING:
            begin
              fStr := Copy(TImgData(FParent).EXIFsegment^.Data, valuePtr, byteCount);
              if fStr[byteCount] = #0 then
                Delete(fStr, byteCount, 1);
            end;
          else
            fStr := FormatNumber(@rawStr[1], Length(rawStr), tagFormat, FormatS, Code);
        end;
        if ((tag > 0) or (lookupEntry.Name <> 'Unknown')) and Assigned(Callback) and Decode then
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
          fStr := FormatDateTime(TImgData(FParent).DateTimeFormat, ExifDateToDateTime(fStr));
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
        FExifVersion := rawstr;

      TAG_MAKERNOTE:
        begin
          MakerNote := rawStr;
          MakerOffset := valuePtr;
          msInfo := TMsInfo.Create(TiffFmt, self);
          msAvailable := msInfo.ReadMSData(self);
          FreeAndNil(msInfo);
        end;
      TAG_FLASH:
        FlashUsed := round(getNumber(@rawStr[1], Length(rawSTr), tagFormat));
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
          FThumbType := round(GetNumber(@rawStr[1], Length(rawStr), tagFormat));
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
        NewEntry.TID := GenericEXIF;  // 0
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

  if (ATagType = ttExif) and
     ((TImgData(FParent).ErrStr = '') or (TImgData(FParent).ErrStr = NO_ERROR))
  then
    Calc35Equiv();
end;

procedure TImageInfo.ProcessHWSpecific(AMakerBuff: ansistring;
  TagTbl: array of TTagEntry; ADirStart, AMakerOffset: Longint;
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
  ADirStart := ADirStart+1;
  OffsetBase := ADirStart - AMakerOffset + 1;
  SetDataBuff(AMakerBuff);
  try
    NumDirEntries := Get16u(ADirStart);
    for de := 0 to NumDirEntries-1 do
    begin
      DirEntry := ADirStart + 2 + 12*de;
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
      if ValuePtr + ByteCount > Length(AMakerBuff) then
        rawStr := Copy(TImgData(FParent).DataBuff, OffsetVal + spOffset, ByteCount)
      else
        rawStr := copy(AMakerBuff, ValuePtr, ByteCount);

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
              ds := siif(Decode, LookupCode(tag, TagTbl), '');
              if tagID < 0 then
                fStr := FormatNumber(@rawStr[1], Length(rawStr), tagFormat, '', '')
              else
                fStr := FormatNumber(@rawStr[1], Length(rawStr), tagFormat, TagTbl[tagID].FormatS, ds);
            except
              fStr := '"' + rawStr + '"';
            end;
        end;

        rawDefered := false;
        if (tagID > 0) and Assigned(TagTbl[tagID].CallBack) and Decode then
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
              ' [raw: '   + MakeHex(rawStr) + ']' +
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
          NewEntry.TID   := CustomEXIF;  // = 1 --> Manufacturer-specific

          AddTagToArray(NewEntry);
        except
          // if we're here: unknown tag.
          // item is recorded in trace string
        end;
      end;

    end;

  except
     on E: Exception do
       TImgData(FParent).SetError('Error detected: ' + E.Message);
  end;

   SetDataBuff(TImgData(FParent).DataBuff);
end;

procedure TImageInfo.AddMSTag(ATagName: String; ARawStr: ansistring; AType: word);
var
  newEntry: TTagEntry;
begin
  if BuildList in [GenList,GenAll] then
  begin
    try
      InitTagEntry(newEntry);
      newEntry.Name := ATagName;
      newEntry.Desc := InsertSpaces(ATagName);
      newEntry.Data := ARawStr;
      newEntry.Raw  := ARawStr;
      newEntry.Size := Length(ARawStr);
      NewEntry.TType:= AType;
      NewEntry.Count := 1;
      newEntry.ParentID := 0;
      newEntry.TID  := CustomEXIF;  // = 1 --> manufacturer-specific
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
    srcStream.LoadFromFile(TImgData(FParent).FileName);
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
    Move(TImgData(FParent).ExifSegment^.Data[FThumbnailStartOffset + 9], FThumbnailBuffer[0], FThumbnailSize);
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
    with TImgData(FParent) do
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
  lParent: TImgData;
begin
  lParent := TImgData(FParent);
  W := ALabelWidth;
  L := TStringList.Create;
  try
    (*
    if parent.ExifSegment = nil then
      Result := ''
    else
    *)
    if lParent.ErrStr <> NO_ERROR then
    begin
      L.Add(Format('File Name:     %s', [ExtractFileName(lParent.Filename)]));
      L.Add(Format('Exif Error:    %s', [lParent.ErrStr]));
      Result := L.Text;
    end else
    begin
      FileDateTime := FormatDateTime(lParent.DateTimeFormat, lParent.FileDateTime);

      L.Add(Format('%-*s %s',      [w, 'File name:', ExtractFileName(lParent.Filename)]));
      L.Add(Format('%-*s %dkB',    [w, 'File size:', lParent.FileSize div 1024]));
      L.Add(Format('%-*s %s',      [w, 'File date:', FileDateTime]));
      L.Add(Format('%-*s %s',      [w, 'Photo date:', FormatDateTime(lParent.DateTimeFormat, GetImgDateTime)]));
      L.Add(Format('%-*s %s (%s)', [w, 'Make (model):', CameraMake, CameraModel]));
      L.Add(Format('%-*s %d x %d', [w, 'Dimensions:', Width, Height]));

      if BuildList in [GenString,GenAll] then
      begin
        tmpStr := TagValueAsString['ExposureTime'];
        if tmpStr <> '' then
          L.Add(Format('%-*s %s', [w, 'Exposure time:', tmpStr]))
        else
        begin
          tmpStr := TagValueAsstring['ShutterSpeedValue'];
          if tmpStr <> '' then
            L.Add(Format('%-*s %s', [w, 'Exposure time:', tmpStr]));
        end;

        tmpStr := TagValueAsString['FocalLength'];
        if tmpStr <> '' then
          L.Add(Format('%-*s %s', [w, 'Focal length:', tmpStr]));

        tmpStr := TagValueAsString['FocalLengthIn35mm'];
        if tmpStr <> '' then
          L.Add(Format('%-*s %s', [w, 'Focal length (35mm):', tmpStr]));

        tmpStr := TagValueAsString['FNumber'];
        if tmpStr <> '' then
          L.Add(Format('%-*s %s', [w, 'F number', tmpStr]));

        tmpStr := TagValueAsString['ISOSpeedRatings'];
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
var
  lParent: TImgData;
begin
  lParent := TImgData(FParent);
  if lParent.ErrStr <> NO_ERROR then
    Result := ExtractFileName(lParent.Filename) + ' Exif Error: ' + lParent.ErrStr
  else
    Result := ExtractFileName(lParent.Filename) + ' ' +
              IntToStr(lParent.FileSize div 1024) + 'kB '+
              FormatDateTime(lParent.DateTimeFormat, GetImgDateTime) + ' ' +
              IntToStr(Width) + 'w ' + IntToStr(Height) + 'h '+
              siif(odd(FlashUsed),' Flash', '');
end;

procedure TImageInfo.AdjExifSize(AHeight, AWidth: Integer);
begin
  TagValue['ImageWidth'] :=  AWidth;
  TagValue['ImageLength'] := AHeight;
end;

procedure TImageInfo.InternalGetBinaryTagValue(const ATag: TTagEntry;
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

function TImageInfo.InternalGetTagValue(const ATag: TTagEntry): Variant;
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
        while (s <> '') and (s[Length(s)] = #0) do
          Delete(s, Length(s), 1);
        Result := s;
        exit;
      end;
    FMT_BINARY:
      begin
        Result := BinaryTagToVar(ATag);
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

function TImageInfo.BinaryTagToStr(const ATag: TTagEntry): String;
begin
  Result := ATag.Raw;
end;

function TImageInfo.BinaryTagToVar(const ATag: TTagEntry): Variant;
var
  s: String;
begin
  case ATag.Tag of
    TAG_EXIFVERSION,
    TAG_FLASHPIXVERSION,
    TAG_INTEROPVERSION:
      begin
        SetLength(s, Length(ATag.Raw));
        Move(ATag.Raw[1], s[1], Length(s));
        Result := s;
      end;
    TAG_USERCOMMENT:
      begin
        Result := GetExifComment;
      end;
  else
    Result := '<binary>';
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
        Result :=  r.Numerator / r.Denominator;
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
procedure TImageInfo.InternalSetTagValue(const ATagName: String; AValue: Variant;
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
      RemoveTag(ATagTypes, tagID, tagDef^.ParentID);
      exit;
    end;
  end else begin
    if VarIsNull(AValue) or VarIsEmpty(AValue) then begin
      RemoveTag(ATagTypes, tagID, tagDef^.ParentID);
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
    {
    if(ttGps in ATagTypes) then
      parentID := TAG_GPS_OFFSET
    else
      parentID := 0;
    }
    P := CreateTagPtr(tagDef^, (ttThumb in ATagTypes), tagDef^.ParentID);
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
    s := FormatNumber(@ATag^.Raw[1], Length(ATag^.Raw), ATag^.TType, ATag^.FormatS, ATag^.Code);
    {
    if Assigned(ATag.Callback) and Parent.Decode then
      s := ATag.Callback(s);
      }
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
  s := FormatNumber(@ATag^.Raw[1], Length(ATag^.Raw), ATag^.TType, ATag^.FormatS, ATag^.Code);
  ATag^.Data := siif(len = 0, s, ATag^.Data + dExifDataSep + s);
end;

function TImageInfo.GetTagByID(ATagID: Word): TTagEntry;
var
  i: Integer;
begin
  for i:= 0 to fiTagCount - 1 do
    if (fiTagArray[i].Tag = ATagID) and (fiTagArray[i].TID = GenericEXIF) then
    begin
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
    if (fITagArray[i].Tag = ATagID) and (fiTagArray[i].TID = GenericEXIF) then
    begin
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
  i := LookupTagIndex(ATagName);
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
  i := LookupTagIndex(ATagName);
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
  if (tag.Name = '') or (tag.Name = 'Unknown') then
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
begin
  Result := '';
  tag := GetTagByName(ATagName);
  if (tag.Name = '') or (tag.Name = 'Unknown') then
    exit;
  Result := InternalGetTagValueAsString(tag);
end;

function TImageInfo.InternalGetTagValueAsString(const ATag: TTagEntry): String;
var
  s: String;
begin
  if ATag.TType = FMT_STRING then
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
    while (s <> '') and ((s[Length(s)] = #0) or (s[Length(s)] = ' ')) do
      Delete(s, Length(s), 1);
    Result := s;
  end else
  if ATag.TType = FMT_BINARY then
  begin
    if (ATag.Size=1) then begin
      Result := FormatNumber(@ATag.Raw[1], Length(ATag.Raw), ATag.TType, ATag.FormatS, ATag.Code);
      if Assigned(ATag.Callback) and Decode then
        Result := ATag.Callback(Result);
    end else
    if ATag.Name = 'ExifVersion' then
      Result := GetVersion(ATag)
    else if ATag.Name = 'FlashPixVersion' then
      Result := GetVersion(ATag)
    else if ATag.Name = 'InteroperabilityVersion' then
      Result := GetVersion(ATag)
    else if ATag.Name = 'UserComment' then
      Result := GetExifComment
    else begin
      Result := BinaryTagToStr(ATag);
      if Assigned(ATag.Callback) and Decode then
        Result := ATag.Callback(Result);
    end;
  end else
  begin
    Result := FormatNumber(@ATag.Raw[1], Length(ATag.Raw), ATag.TType, ATag.FormatS, ATag.Code);
    if Assigned(ATag.Callback) and Decode then
      Result := ATag.Callback(Result)
  end;
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
    if (fiThumbArray[i].Tag = ATagID) then
    begin
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

function TImageInfo.GetThumbTagValueAsString(ATagName: String): String;
var
  tag: TTagEntry;
begin
  Result := '';
  tag := GetThumbTagByName(ATagName);
  if (tag.Name = '') or (tag.Name = 'Unknown') then
    exit;
  Result := InternalGetTagValueAsString(tag);
end;

procedure TImageInfo.SetThumbTagValueAsString(ATagName: String; AValue: String);
var
  v: Variant;
begin
  v := AValue;
  SetThumbTagValue(ATagName, v);
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
  i: Integer;
begin
  i := 0;
  if ttThumb in ATagTypes then
  begin
    while i < fiThumbCount do
    begin
      if (fiThumbArray[i].Tag = ATagID) and (fiThumbArray[i].ParentID = AParentID) then
      begin
        while (i < fiThumbCount-1) do begin
          fiThumbArray[i] := fiThumbArray[i+1];
          inc(i);
        end;
        dec(fiThumbCount);
        break;
      end else
        inc(i);
    end;
  end else
  begin
    while i < fiTagCount do
    begin
      if (fiTagArray[i].Tag = ATagID) and (fiTagArray[i].ParentID = AParentID) then
      begin
        while (i < fiTagCount-1) do begin
          fiTagArray[i] := fiTagArray[i+1];
          inc(i);
        end;
        dec(fiTagCount);
        break;
      end else
        inc(i);
    end;
  end;
end;
  (*
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
          *)
function TImageInfo.CreateTagPtr(const ATagDef: TTagEntry; IsThumbTag: Boolean;
  AParentID: Word = 0): PTagEntry;
var
  pTag: PTagEntry;
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
    // Create the parent tag if it does not exist, yet.
    if (AParentID <> 0) and (GetTagByID(AParentID).Tag = 0) then begin
      pTag := FindExifTagDefByID(AParentID);
      if pTag = nil then
        raise Exception.CreateFmt('Definition for tag $%.4x not found.', [AParentID]);
      pTag^.ParentID := 0;
      pTag^.Raw := StringOfChar(#0, pTag^.Size);
      AddTagToArray(pTag^);
    end;
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

function TImageInfo.GetUserComment(const ATag: TTagEntry): String;
var
  buf: ansistring;
  w: widestring;
  a: ansistring;
  n: Integer;
begin
  Result := '';

  InternalGetBinaryTagValue(ATag, buf);
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
    while (a <> '') and ((a[Length(a)] = #0) or (a[Length(a)] = ' ')) do
      Delete(a, Length(a), 1);
    Result := a;
  end else
  if pos(#0#0#0#0#0#0#0#0, buf) = 1 then begin
    a := Copy(buf, 9, MaxInt);
    while (a <> '') and ((a[Length(a)] = #0) or (a[Length(a)] = ' ')) do
      Delete(a, Length(a), 1);
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

function TImageInfo.GetExifComment: String;
var
  tag: TTagEntry;
begin
  tag := GetTagByName('UserComment');
  if tag.Tag <> 0 then
    Result := GetUserComment(tag)
  else
    Result := '';
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
  Result := NaN;
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
  if IsNaN(AValue) then
    v := NULL
  else begin
    val := abs(AValue);
    degs := trunc(val);
    mins := trunc(frac(val) * 60);
    secs := (frac(val) * 60 - mins) * 60;
    v := VarArrayOf([degs, mins, secs]);
  end;
  InternalSetTagValue(ATagName, v, [ttGps]);
  if IsNaN(AValue) then
    InternalSetTagValue(ATagName + 'Ref', NULL, [ttGps])
  else
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

{ The version of the supported Exif or FlashPix standard.

  All four bytes should be interpreted as ASCII values. The first two bytes
  encode the upper part of the standard version, the next two bytes encode the
  lower part. For example, the byte sequence 48, 50, 50, 48, is the equivalent
  of the ASCII value "0220", and denotes version 2.20.

  http://www.awaresystems.be/imaging/tiff/tifftags/privateifd/exif/exifversion.html
  http://www.awaresystems.be/imaging/tiff/tifftags/privateifd/exif/flashpixversion.html
}
function TImageInfo.GetVersion(ATag: TTagEntry): String;
var
  s: AnsiString;
begin
  Result := '';
  InternalGetBinaryTagValue(ATag, s);
  Result := s;
end;

function TImageInfo.IterateFoundTags(TagId: integer; var RetVal: TTagEntry): boolean;
begin
  InitTagEntry(Retval);

  while (FIterator < FITagCount) and (FITagArray[FIterator].TID <> TagId) do
    inc(FIterator);
  if (FIterator < FITagCount) then
  begin
    RetVal := FITagArray[FIterator];
    inc(FIterator);
    Result := true;
  end
  else
    Result := false;
end;

procedure TImageInfo.ResetIterator;
begin
  FIterator := 0;
end;

function TImageInfo.IterateFoundThumbTags(TagId: integer;
  var RetVal: TTagEntry): boolean;
begin
  InitTagEntry(RetVal);

  while (FThumbIterator < FIThumbCount) and (FITagArray[FThumbIterator].TID <> TagId) do
    inc(FThumbIterator);
  if (FThumbIterator < FIThumbCount) then
  begin
    RetVal := FIThumbArray[FThumbIterator];
    inc(FThumbIterator);
    Result := true;
  end
  else
    Result := false;
end;

procedure TImageInfo.ResetThumbIterator;
begin
  FThumbIterator := 0;
end;

function TImageInfo.GetRawFloat(ATagName: String): Double;
var
  tiq: TTagEntry;
begin
  tiq := GetTagByName(ATagName);
  if tiq.Tag = 0 then     // EmptyEntry
    Result := 0.0
  else
    Result := GetNumber(@tiq.Raw[1], Length(tiq.Raw), tiq.TType);
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
    result := round(GetNumber(@tiq.Raw[1], Length(tiq.Raw), tiq.TType));
end;

//  Unfortunatly if we're calling this function there isn't
//  enough info in the EXIF to calculate the equivalent 35mm
//  focal length and it needs to be looked up on a camera
//  by camera basis. - next rev - maybe
function TImageInfo.LookupRatio: double;
var
  estRatio: double;
  upMake, upModel: String;
begin
  upMake := Uppercase(copy(CameraMake, 1, 5));
  upModel := Uppercase(copy(Cameramodel, 1, 5));
  estRatio := 4.5;  // ballpark for *my* camera -
  Result := estRatio;
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
  if LookUpTagIndex('FocalLengthin35mmFilm') >= 0 then
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


end.
























