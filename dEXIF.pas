Unit dEXIF;

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

{$I dExif.inc}

interface

uses
  sysutils, classes, math,
 {$IFDEF FPC}
  fpimage, fpreadjpeg, LazUTF8,
 {$ELSE}
  {$IFNDEF dExifNoJpeg} jpeg, {$ENDIF}
 {$ENDIF}
  dGlobal, dUtils, dTags, dIPTC;

const
  ExifTag = 1;  // default tag Types
  GpsTag = 2;
  ThumbTag = 4;
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
    iterator:integer;
    iterThumb:integer;

//    FCameraMake: String;
//    FCameraModel: String;

    // Getter / setter
    function GetDateTimeOriginal: TDateTime;
    procedure SetDateTimeOriginal(const AValue: TDateTime);

    function GetDateTimeDigitized: TDateTime;
    procedure SetDateTimeDigitized(const AValue: TDateTime);

    function GetDateTimeModify: TDateTime;
    procedure SetDateTimeModify(const AValue: TDateTime);

    function GetArtist: String;
    procedure SetArtist(v: String);

    function GetExifComment: String;
    procedure SetExifComment(v: String);

    function GetImageDescription: String;
    procedure SetImageDescription(const v: String);

    function GetCameraMake: String;
    procedure SetCameraMake(const AValue: String);

    function GetCameraModel: String;
    procedure SetCameraModel(const AValue: String);

    function GetTagElement(TagID: integer): TTagEntry;
    procedure SetTagElement(TagID: integer; const Value: TTagEntry);

    function GetTagByName(TagName: ansistring): TTagEntry;
    procedure SetTagByName(TagName: ansistring; const Value: TTagEntry);

    function GetTagValueAsString(ATagName: String): String;
    procedure SetTagValueAsString(ATagName: String; AValue: String);

    // misc
    function GetTag(ATagID: Word; AForceCreate: Boolean=false; AParentID: word=0;
      ATagType: word=65535; AForceID: Boolean=false): PTagEntry; overload;
    procedure RemoveTag(TagID: integer; parentID: word=0);
    procedure TagWriteThru16(te: ttagentry; NewVal16: word);
    procedure TagWriteThru32(te: ttagentry; NewVal32: longint);

    procedure ClearDirStack;
    procedure PushDirStack(dirStart, offsetbase: Integer);
    function TestDirStack(dirStart, offsetbase: Integer): boolean;

  protected
    function AddTagToArray(nextTag: iTag): integer;
    function AddTagToThumbArray(nextTag: iTag): integer;

    function CreateExifBuf(parentID: word=0; offsetBase: integer=0): String;

  public
    FITagArray: array of TTagEntry;
    FITagCount: integer;
    MaxTag: integer;
    Parent: timgdata;
    ExifVersion : string[ 6];
    DateTime:     string[20];
    Height, Width, HPosn, WPosn: integer;
    FlashUsed: integer;
    BuildList: integer;
    MakerNote: ansistring;
    TiffFmt: boolean;
//    Comments: ansistring;
//    CommentPosn: integer;
//    CommentSize: integer;
// DateTime tag locations
    dt_modify_oset:integer;
    dt_orig_oset:integer;
    dt_digi_oset:integer;
// Add support for thumbnail
    ThumbTrace:ansistring;
    ThumbStart: integer;
    ThumbLength: integer;
    ThumbType: integer;
    FIThumbArray: array of TTagEntry;
    FIThumbCount: integer;
    MaxThumbTag: integer;
//  Added the following elements to make the
//  structure a little more code-friendly
    TraceLevel: integer;
    TraceStr: ansistring;
    msTraceStr: ansistring;
    msAvailable: boolean;
    msName:ansistring;
    MakerOffset : integer;

//    procedure SetExifComment(newComment: ansistring);

//  The following functions manage the date
    function  GetImgDateTime: TDateTime;
    function  ExtrDateTime(oset: integer): TDateTime;
    function  ExifDateToDateTime(dstr: ansistring): TDateTime;
    procedure SetDateTimeStr(oset: integer; TimeIn: TDateTime);
    procedure AdjDateTime(days, hours, mins, secs: integer);
    procedure OverwriteDateTime(ADateTime: TDateTime);   //  Contains embedded CR/LFs

    procedure ProcessExifDir(DirStart, OffsetBase, ExifLength: longint;
      ATagType: integer=ExifTag; APrefix: string=''; AParentID: word=0);
    procedure ProcessThumbnail;
    procedure ProcessHWSpecific(MakerBuff:ansistring;
                  TagTbl: array of TTagEntry;
                  DirStart:longint;
                  aMakerOffset:Longint;
                  spOffset:integer = 0);

    procedure AddMSTag(fname,fstr:ansistring;fType:word);
    function CvtInt(buff: ansistring): longint;
    function FormatNumber(buffer: ansistring; fmt: integer; fmtStr:ansistring;
      decodeStr: ansistring=''): ansistring;
    function GetNumber(buffer: ansistring; fmt: integer): double;
    procedure removeThumbnail;
    procedure AdjExifSize(nh,nw:longint);

    function GetTagByDesc(SearchStr: ansistring): TTagEntry;
    function LookupTag(SearchStr: ansistring): integer; virtual;
    function LookupTagVal(SearchStr: ansistring): ansistring; virtual;
//    function LookupTagValue(ASearchStr: String): String;
    function LookupTagDefn(item: ansistring): integer;
    function LookupTagByDesc(SearchStr: ansistring): integer;
    function LookupTagInt(SearchStr: ansistring): integer;

    procedure ResetIterator;
    function IterateFoundTags(TagId:integer; var retVal:TTagEntry):boolean;
    function IterateFoundThumbTags(TagId: integer;
      var retVal: TTagEntry): boolean;
    procedure ResetThumbIterator;

    procedure Calc35Equiv;
    function EXIFArrayToXML: tstringlist;
    function GetRawFloat(tagName: ansistring): double;
    function GetRawInt(tagName: ansistring): integer;
    function LookupRatio: double;
    function WriteThruInt(tname: ansistring; value: Integer): boolean;
    function WriteThruString(tname, value: ansistring): boolean;
    Function HasThumbnail:boolean;

  public
    constructor Create(p: TImgData; BuildCode: integer = GenAll);
    procedure Assign(source: TImageInfo);
    destructor Destroy; override;

    //  The following functions format this structure into a string
    function ToShortString: String;   //  Summarizes in a single line
    function ToLongString(ALabelWidth: Integer = 15): String;

    property ITagArray[TagID: Integer]: TTagEntry
        read GetTagElement write SetTagElement; default;
    property Data[TagName: AnsiString]: TTagEntry
        read GetTagByName write SetTagByName;
    property TagValueAsString[ATagName: String]: String
        read GetTagValueAsString write SetTagValueAsString;

    property DateTimeOriginal: TDateTime read GetDateTimeOriginal write SetDateTimeOriginal;
    property DateTimeDigitized: TDateTime read GetDateTimeDigitized write SetDateTimeDigitized;
    property DateTimeModify: TDateTime read GetDateTimeModify write SetDateTimeModify;
    property Artist: String read GetArtist write SetArtist;
    property ExifComment: String read GetExifComment write SetExifComment;
    property ImageDescription: String read GetImageDescription write SetImageDescription;
    property CameraMake: String read GetCameraMake write SetCameraMake;
    property CameraModel: String read GetCameraModel write SetCameraModel;
  end; // TInfoData

  TSection = record
    Data: ansistring;
    DType: integer;
    Size: longint;
    Base: longint;
  end;
  pSection = ^TSection;

  { TImgData }

  TImgData = class(TEndInd) // One per image object
  private
    FHeight: Integer;
    FWidth: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetResolutionUnit: String;
    function GetXResolution: Integer;
    function GetYResolution: Integer;
    function GetComment: String;
    procedure SetComment(v: String);

  protected
    function ReadJpegSections(var f: TStream):boolean;
    function ReadTiffSections(var f: TStream):boolean;
    function SaveExif(jfs2: TStream; EnabledMeta: Byte=$FF;
      FreshExifBlock: Boolean=false): longint;

  public
    Sections: array[1..21] of TSection;
    TiffFmt: boolean;
    BuildList: integer;
    SectionCnt : integer;
    ExifSegment: pSection;
    IPTCSegment: pSection;
    CommentSegment: pSection;
    HeaderSegment : pSection;
    Filename: ansistring;
    FileDateTime: tDateTime;
    FileSize: longint;
    ErrStr: ansistring;
    ExifObj: TImageInfo;
    IptcObj: TIPTCData;
    TraceLevel: integer;

    procedure Reset;
    procedure SetFileInfo(fname:ansistring);
    procedure MakeIPTCSegment(buff:ansistring);
    procedure MakeCommentSegment(buff:ansistring);
    function GetCommentStr:ansistring;
    Function GetCommentSegment:ansistring;
    procedure ClearSections;
    procedure ClearEXIF;
    procedure ClearIPTC;
    procedure ClearComments;
    procedure CreateIPTCObj;

    function ReadIPTCStrings(fname: ansistring):tstringlist;
    function ReadJpegFile(const AFileName: string):boolean;
    function ReadTiffFile(const AFileName: string):boolean;

    function MetaDataToXML: TStringList;
    function FillInIptc: boolean;

  public
    constructor Create(buildCode: integer = GenAll);
    destructor Destroy; override;

    // Reading
    function ReadExifInfo(AFilename: String): boolean;

    // Processing
    procedure ProcessEXIF;
    function ProcessFile(const AFileName: string):boolean;

    // Thumbnail
    function ExtractThumbnailBuffer: TBytes;
    {$IFDEF FPC}
    function ExtractThumbnailJpeg(AStream: TStream): Boolean;
    {$ELSE}
    function ExtractThumbnailJpeg: TJpegImage;
    {$ENDIF}

    // Status
    function HasMetaData:boolean;
    function HasEXIF: boolean;
    function HasIPTC: boolean;
    function HasComment: boolean;
    function HasThumbnail: boolean;

    // Writing
   {$IFDEF FPC}
    procedure WriteEXIFJpeg(AJpeg: TStream; AFileName: String; AdjSize: Boolean = true); overload;
    procedure WriteEXIFJpeg(AFileName, AOrigName: String; AdjSize: Boolean = true); overload;
    procedure WriteEXIFJpeg(AFileName: String); overload;
   {$ENDIF}
   {$IFNDEF dExifNoJpeg}
    procedure WriteEXIFJpeg(j:TJpegImage; fname, origName: String;
      AdjSize: boolean = true);  overload;
    procedure WriteEXIFJpeg(fname: String); overload;
    procedure WriteEXIFJpeg(j:tjpegimage; fname:String; adjSize:boolean = true);  overload;
   {$ENDIF}
    procedure MergeToStream(AInputStream, AOutputStream: TStream;
      AEnabledMeta: Byte = $FF; AFreshExifBlock: Boolean = false);

    // Basic properties
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
    property XResolution: Integer read GetXResolution;
    property YResolution: Integer read GetYResolution;
    property ResolutionUnit: String read GetResolutionUnit;
    property Comment: String read GetComment write SetComment;  // Comment from COM segment
  end; // TImgData

  function FindExifTag(ATag: Word): PTagEntry;
  function FindGPSTag(ATag: Word): PTagEntry;

var
  CurTagArray: TImageInfo = nil;
  fmtInt: tfmtInt = defIntFmt;
  fmtReal: tfmtReal = defRealFmt;
  fmtFrac: tfmtFrac = defFracFmt;

const
  ValidHeader: ansistring = 'Exif'#0;

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
     M_SOF13= $CD;                              M_DAC  = $CC;            // Define arithmetic coding conditioning
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

   Function LookupType(idx:integer):ansistring;

   Function MakePrintable(s:ansistring):ansistring;

var
  ExifNonThumbnailLength : integer;
  ShowTags: integer;
  ExifTrace: integer = 0;
{$IFDEF dEXIFpredeclare}
  ImgData:timgData;
{$ENDIF}

implementation

uses
  dExifWrite, msData;

const
// Compression Type Constants
   JPEG_COMP_TYPE = 6;
   TIFF_COMP_TYPE = 1;

//-------------------------------------------------------
// Describes only tag values needed for physical access
// all others are found in tag array.
//-------------------------------------------------------

   GPSCnt = 31 - 4;
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
    TAG_DATETIME_ORIGINAL,   //$9003,
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

{   Many tags added based on Php4 source...
http://lxr.php.net/source/php4/ext/exif/exif.c
}
var
 TagTable : array [0..ExifTagCnt-1] of TTagEntry =
// TagTable : array of TTagEntry =
// TagTable : TTagDefArray [0..ExifTagCnt] =
// TagTable: TTagDefArray =
 ((TID:0;TType:0;ICode: 2;Tag: $001;   Name:'InteroperabilityIndex'  ),         {0}
  (TID:0;TType:0;ICode: 2;Tag: $002;   Name:'InteroperabilityVersion'),
  (TID:0;TType:0;ICode: 2;Tag: $00B;   Name:'ACDComment'             ),
  (TID:0;TType:0;ICode: 2;Tag: $0FE;   Name:'NewSubfileType'         ),
  (TID:0;TType:0;ICode: 2;Tag: $0FF;   Name:'SubfileType'            ),
  (TID:0;TType:0;ICode: 2;Tag: $100;   Name:'ImageWidth'             ),
  (TID:0;TType:0;ICode: 2;Tag: $101;   Name:'ImageLength'            ),
  (TID:0;TType:0;ICode: 2;Tag: $102;   Name:'BitsPerSample'          ),
  (TID:0;TType:0;ICode: 2;Tag: $103;   Name:'Compression'            ;Desc:'';Code:'6:Jpeg,3:Uncompressed,1:TIFF'),
  (TID:0;TType:0;ICode: 2;Tag: $106;   Name:'PhotometricInterpretation';Desc:''; Code:'1:Monochrome, 2:RGB, 6:YCbCr'),
  (TID:0;TType:0;ICode: 2;Tag: $10A;   Name:'FillOrder'              ),         {10}
  (TID:0;TType:0;ICode: 2;Tag: $10D;   Name:'DocumentName'           ),
  (TID:0;TType:0;ICode: 2;Tag: $10E;   Name:'ImageDescription'       ),
  (TID:0;TType:0;ICode: 2;Tag: $10F;   Name:'Make'                   ),
  (TID:0;TType:0;ICode: 2;Tag: $110;   Name:'Model'                  ),
  (TID:0;TType:0;ICode: 2;Tag: $111;   Name:'StripOffsets'           ),
  (TID:0;TType:0;ICode: 2;Tag: $112;   Name:'Orientation'            ; Desc:''; Code:'1:Normal,3:Rotated 180°,6:CounterClockwise 90°,8:Clockwise 90°'),
  (TID:0;TType:0;ICode: 2;Tag: $115;   Name:'SamplesPerPixel'        ),
  (TID:0;TType:0;ICode: 2;Tag: $116;   Name:'RowsPerStrip'           ),
  (TID:0;TType:0;ICode: 2;Tag: $117;   Name:'StripByteCounts'        ),
  (TID:0;TType:0;ICode: 2;Tag: $118;   Name:'MinSampleValue'         ),         {20}
  (TID:0;TType:0;ICode: 2;Tag: $119;   Name:'MaxSampleValue'         ),
  (TID:0;TType:0;ICode: 2;Tag: $11A;   Name:'XResolution'            ; Desc:''; Code:''; Data:''; Raw:''; PRaw:0; FormatS:'%5.2f'),
  (TID:0;TType:0;ICode: 2;Tag: $11B;   Name:'YResolution'            ; Desc:''; Code:''; Data:''; Raw:''; PRaw:0; FormatS:'%5.2f'),
  (TID:0;TType:0;ICode: 2;Tag: $11C;   Name:'PlanarConfiguration'    ),
  (TID:0;TType:0;ICode: 2;Tag: $11D;   Name:'PageName'               ),
  (TID:0;TType:0;ICode: 2;Tag: $11E;   Name:'XPosition'              ),
  (TID:0;TType:0;ICode: 2;Tag: $11F;   Name:'YPosition'              ),
  (TID:0;TType:0;ICode: 2;Tag: $120;   Name:'FreeOffsets'            ),
  (TID:0;TType:0;ICode: 2;Tag: $121;   Name:'FreeByteCounts'         ),
  (TID:0;TType:0;ICode: 2;Tag: $122;   Name:'GrayReponseUnit'        ),         {30}
  (TID:0;TType:0;ICode: 2;Tag: $123;   Name:'GrayReponseCurve'       ),
  (TID:0;TType:0;ICode: 2;Tag: $124;   Name:'T4Options'              ),
  (TID:0;TType:0;ICode: 2;Tag: $125;   Name:'T6Options'              ),
  (TID:0;TType:0;ICode: 2;Tag: $128;   Name:'ResolutionUnit'         ;Desc:''; Code:'1:None Specified,2:Inch,3:Centimeter'),
  (TID:0;TType:0;ICode: 2;Tag: $129;   Name:'PageNumber'             ),
  (TID:0;TType:0;ICode: 2;Tag: $12D;   Name:'TransferFunction'       ),
  (TID:0;TType:0;ICode: 2;Tag: $131;   Name:'Software'               ),
  (TID:0;TType:0;ICode: 2;Tag: $132;   Name:'DateTimeModify'         ),
  (TID:0;TType:0;ICode: 2;Tag: $13B;   Name:'Artist'                 ),
  (TID:0;TType:0;ICode: 2;Tag: $13C;   Name:'HostComputer'           ),         {40}
  (TID:0;TType:0;ICode: 2;Tag: $13D;   Name:'Predictor'              ),
  (TID:0;TType:0;ICode: 2;Tag: $13E;   Name:'WhitePoint'             ),
  (TID:0;TType:0;ICode: 2;Tag: $13F;   Name:'PrimaryChromaticities'  ),
  (TID:0;TType:0;ICode: 2;Tag: $140;   Name:'ColorMap'               ),
  (TID:0;TType:0;ICode: 2;Tag: $141;   Name:'HalfToneHints'          ),
  (TID:0;TType:0;ICode: 2;Tag: $142;   Name:'TileWidth'              ),
  (TID:0;TType:0;ICode: 2;Tag: $143;   Name:'TileLength'             ),
  (TID:0;TType:0;ICode: 2;Tag: $144;   Name:'TileOffsets'            ),
  (TID:0;TType:0;ICode: 2;Tag: $145;   Name:'TileByteCounts'         ),
  (TID:0;TType:0;ICode: 2;Tag: $14A;   Name:'SubIFDs'                ),         {50}
  (TID:0;TType:0;ICode: 2;Tag: $14C;   Name:'InkSet'                 ),
  (TID:0;TType:0;ICode: 2;Tag: $14D;   Name:'InkNames'               ),
  (TID:0;TType:0;ICode: 2;Tag: $14E;   Name:'NumberOfInks'           ),
  (TID:0;TType:0;ICode: 2;Tag: $150;   Name:'DotRange'               ),
  (TID:0;TType:0;ICode: 2;Tag: $151;   Name:'TargetPrinter'          ),
  (TID:0;TType:0;ICode: 2;Tag: $152;   Name:'ExtraSample'            ),
  (TID:0;TType:0;ICode: 2;Tag: $153;   Name:'SampleFormat'           ),
  (TID:0;TType:0;ICode: 2;Tag: $154;   Name:'SMinSampleValue'        ),
  (TID:0;TType:0;ICode: 2;Tag: $155;   Name:'SMaxSampleValue'        ),
  (TID:0;TType:0;ICode: 2;Tag: $156;   Name:'TransferRange'          ),         {60}
  (TID:0;TType:0;ICode: 2;Tag: $157;   Name:'ClipPath'               ),
  (TID:0;TType:0;ICode: 2;Tag: $158;   Name:'XClipPathUnits'         ),
  (TID:0;TType:0;ICode: 2;Tag: $159;   Name:'YClipPathUnits'         ),
  (TID:0;TType:0;ICode: 2;Tag: $15A;   Name:'Indexed'                ),
  (TID:0;TType:0;ICode: 2;Tag: $15B;   Name:'JPEGTables'             ),
  (TID:0;TType:0;ICode: 2;Tag: $15F;   Name:'OPIProxy'               ),
  (TID:0;TType:0;ICode: 2;Tag: $200;   Name:'JPEGProc'               ),
  (TID:0;TType:0;ICode: 2;Tag: $201;   Name:'JPEGInterchangeFormat'  ),
  (TID:0;TType:0;ICode: 2;Tag: $202;   Name:'JPEGInterchangeFormatLength'),
  (TID:0;TType:0;ICode: 2;Tag: $203;   Name:'JPEGRestartInterval'    ),         {70}
  (TID:0;TType:0;ICode: 2;Tag: $205;   Name:'JPEGLosslessPredictors' ),
  (TID:0;TType:0;ICode: 2;Tag: $206;   Name:'JPEGPointTransforms'    ),
  (TID:0;TType:0;ICode: 2;Tag: $207;   Name:'JPEGQTables'            ),
  (TID:0;TType:0;ICode: 2;Tag: $208;   Name:'JPEGDCTables'           ),
  (TID:0;TType:0;ICode: 2;Tag: $209;   Name:'JPEGACTables'           ),
  (TID:0;TType:0;ICode: 2;Tag: $211;   Name:'YCbCrCoefficients'      ),
  (TID:0;TType:0;ICode: 2;Tag: $212;   Name:'YCbCrSubSampling'       ),
  (TID:0;TType:0;ICode: 2;Tag: $213;   Name:'YCbCrPositioning'       ; Desc:'';Code:'1:Centered,2:Co-sited'),
  (TID:0;TType:0;ICode: 2;Tag: $214;   Name:'ReferenceBlackWhite'    ),
  (TID:0;TType:0;ICode: 2;Tag: $2BC;   Name:'ExtensibleMetadataPlatform' ),     {80}
  (TID:0;TType:0;ICode: 2;Tag: $301;   Name:'Gamma'                     ),
  (TID:0;TType:0;ICode: 2;Tag: $302;   Name:'ICCProfileDescriptor'      ),
  (TID:0;TType:0;ICode: 2;Tag: $303;   Name:'SRGBRenderingIntent'       ),
  (TID:0;TType:0;ICode: 2;Tag: $304;   Name:'ImageTitle'                ),
  (TID:0;TType:0;ICode: 2;Tag: $1000;  Name:'RelatedImageFileFormat' ),
  (TID:0;TType:0;ICode: 2;Tag: $1001;  Name:'RelatedImageWidth'      ),
  (TID:0;TType:0;ICode: 2;Tag: $1002;  Name:'RelatedImageHeight'     ),
  (TID:0;TType:0;ICode: 2;Tag: $5001;  Name:'ResolutionXUnit'        ),
  (TID:0;TType:0;ICode: 2;Tag: $5002;  Name:'ResolutionYUnit'        ),
  (TID:0;TType:0;ICode: 2;Tag: $5003;  Name:'ResolutionXLengthUnit'  ),         {90}
  (TID:0;TType:0;ICode: 2;Tag: $5004;  Name:'ResolutionYLengthUnit'  ),
  (TID:0;TType:0;ICode: 2;Tag: $5005;  Name:'PrintFlags'             ),
  (TID:0;TType:0;ICode: 2;Tag: $5006;  Name:'PrintFlagsVersion'      ),
  (TID:0;TType:0;ICode: 2;Tag: $5007;  Name:'PrintFlagsCrop'         ),
  (TID:0;TType:0;ICode: 2;Tag: $5008;  Name:'PrintFlagsBleedWidth'   ),
  (TID:0;TType:0;ICode: 2;Tag: $5009;  Name:'PrintFlagsBleedWidthScale'),
  (TID:0;TType:0;ICode: 2;Tag: $500A;  Name:'HalftoneLPI'            ),
  (TID:0;TType:0;ICode: 2;Tag: $500B;  Name:'HalftoneLPIUnit'        ),
  (TID:0;TType:0;ICode: 2;Tag: $500C;  Name:'HalftoneDegree'         ),
  (TID:0;TType:0;ICode: 2;Tag: $500D;  Name:'HalftoneShape'          ),         {100}
  (TID:0;TType:0;ICode: 2;Tag: $500E;  Name:'HalftoneMisc'           ),
  (TID:0;TType:0;ICode: 2;Tag: $500F;  Name:'HalftoneScreen'         ),
  (TID:0;TType:0;ICode: 2;Tag: $5010;  Name:'JPEGQuality'            ),
  (TID:0;TType:0;ICode: 2;Tag: $5011;  Name:'GridSize'               ),
  (TID:0;TType:0;ICode: 2;Tag: $5012;  Name:'ThumbnailFormat'        ),
  (TID:0;TType:0;ICode: 2;Tag: $5013;  Name:'ThumbnailWidth'         ),
  (TID:0;TType:0;ICode: 2;Tag: $5014;  Name:'ThumbnailHeight'        ),
  (TID:0;TType:0;ICode: 2;Tag: $5015;  Name:'ThumbnailColorDepth'    ),
  (TID:0;TType:0;ICode: 2;Tag: $5016;  Name:'ThumbnailPlanes'        ),
  (TID:0;TType:0;ICode: 2;Tag: $5017;  Name:'ThumbnailRawBytes'      ),         {110}
  (TID:0;TType:0;ICode: 2;Tag: $5018;  Name:'ThumbnailSize'          ),
  (TID:0;TType:0;ICode: 2;Tag: $5019;  Name:'ThumbnailCompressedSize'),
  (TID:0;TType:0;ICode: 2;Tag: $501A;  Name:'ColorTransferFunction'  ),
  (TID:0;TType:0;ICode: 2;Tag: $501B;  Name:'ThumbnailData'          ),
  (TID:0;TType:0;ICode: 2;Tag: $5020;  Name:'ThumbnailImageWidth'    ),
  (TID:0;TType:0;ICode: 2;Tag: $5021;  Name:'ThumbnailImageHeight'   ),
  (TID:0;TType:0;ICode: 2;Tag: $5022;  Name:'ThumbnailBitsPerSample' ),
  (TID:0;TType:0;ICode: 2;Tag: $5023;  Name:'ThumbnailCompression'   ),
  (TID:0;TType:0;ICode: 2;Tag: $5024;  Name:'ThumbnailPhotometricInterp'),
  (TID:0;TType:0;ICode: 2;Tag: $5025;  Name:'ThumbnailImageDescription' ),      {120}
  (TID:0;TType:0;ICode: 2;Tag: $5026;  Name:'ThumbnailEquipMake'     ),
  (TID:0;TType:0;ICode: 2;Tag: $5027;  Name:'ThumbnailEquipModel'    ),
  (TID:0;TType:0;ICode: 2;Tag: $5028;  Name:'ThumbnailStripOffsets'  ),
  (TID:0;TType:0;ICode: 2;Tag: $5029;  Name:'ThumbnailOrientation'   ),
  (TID:0;TType:0;ICode: 2;Tag: $502A;  Name:'ThumbnailSamplesPerPixel'),
  (TID:0;TType:0;ICode: 2;Tag: $502B;  Name:'ThumbnailRowsPerStrip'  ),
  (TID:0;TType:0;ICode: 2;Tag: $502C;  Name:'ThumbnailStripBytesCount'),
  (TID:0;TType:0;ICode: 2;Tag: $502D;  Name:'ThumbnailResolutionX'   ),
  (TID:0;TType:0;ICode: 2;Tag: $502E;  Name:'ThumbnailResolutionY'   ),
  (TID:0;TType:0;ICode: 2;Tag: $502F;  Name:'ThumbnailPlanarConfig'  ),         {130}
  (TID:0;TType:0;ICode: 2;Tag: $5030;  Name:'ThumbnailResolutionUnit'),
  (TID:0;TType:0;ICode: 2;Tag: $5031;  Name:'ThumbnailTransferFunction'),
  (TID:0;TType:0;ICode: 2;Tag: $5032;  Name:'ThumbnailSoftwareUsed'  ),
  (TID:0;TType:0;ICode: 2;Tag: $5033;  Name:'ThumbnailDateTime'      ),
  (TID:0;TType:0;ICode: 2;Tag: $5034;  Name:'ThumbnailArtist'        ),
  (TID:0;TType:0;ICode: 2;Tag: $5035;  Name:'ThumbnailWhitePoint'    ),
  (TID:0;TType:0;ICode: 2;Tag: $5036;  Name:'ThumbnailPrimaryChromaticities'),
  (TID:0;TType:0;ICode: 2;Tag: $5037;  Name:'ThumbnailYCbCrCoefficients'    ),
  (TID:0;TType:0;ICode: 2;Tag: $5038;  Name:'ThumbnailYCbCrSubsampling'     ),
  (TID:0;TType:0;ICode: 2;Tag: $5039;  Name:'ThumbnailYCbCrPositioning'     ),  {140}
  (TID:0;TType:0;ICode: 2;Tag: $503A;  Name:'ThumbnailRefBlackWhite' ),
  (TID:0;TType:0;ICode: 2;Tag: $503B;  Name:'ThumbnailCopyRight'     ),
  (TID:0;TType:0;ICode: 2;Tag: $5090;  Name:'LuminanceTable'         ),
  (TID:0;TType:0;ICode: 2;Tag: $5091;  Name:'ChrominanceTable'       ),
  (TID:0;TType:0;ICode: 2;Tag: $5100;  Name:'FrameDelay'             ),
  (TID:0;TType:0;ICode: 2;Tag: $5101;  Name:'LoopCount'              ),
  (TID:0;TType:0;ICode: 2;Tag: $5110;  Name:'PixelUnit'              ),
  (TID:0;TType:0;ICode: 2;Tag: $5111;  Name:'PixelPerUnitX'          ),
  (TID:0;TType:0;ICode: 2;Tag: $5112;  Name:'PixelPerUnitY'          ),
  (TID:0;TType:0;ICode: 2;Tag: $5113;  Name:'PaletteHistogram'       ),         {150}
  (TID:0;TType:0;ICode: 2;Tag: $800D;  Name:'ImageID'                ),
  (TID:0;TType:0;ICode: 2;Tag: $80E3;  Name:'Matteing'               ),   //* obsoleted by ExtraSamples */
  (TID:0;TType:0;ICode: 2;Tag: $80E4;  Name:'DataType'               ),   //* obsoleted by SampleFormat */
  (TID:0;TType:0;ICode: 2;Tag: $80E5;  Name:'ImageDepth'             ),
  (TID:0;TType:0;ICode: 2;Tag: $80E6;  Name:'TileDepth'              ),
  (TID:0;TType:0;ICode: 2;Tag: $828D;  Name:'CFARepeatPatternDim'    ),
  (TID:0;TType:0;ICode: 2;Tag: $828E;  Name:'CFAPattern'             ),
  (TID:0;TType:0;ICode: 2;Tag: $828F;  Name:'BatteryLevel'           ),
  (TID:0;TType:0;ICode: 2;Tag: $8298;  Name:'Copyright'              ),
  (TID:0;TType:0;ICode: 2;Tag: $829A;  Name:'ExposureTime'             ; Desc:'Exposure time'; Code:''; Data:''; Raw:''; PRaw:0; FormatS:'%s sec'),   {160}
  (TID:0;TType:0;ICode: 2;Tag: $829D;  Name:'FNumber'                  ; Desc:''; Code:''; Data:''; Raw:''; PRaw:0; FormatS:'F%0.1f'),
  (TID:0;TType:0;ICode: 2;Tag: $83BB;  Name:'IPTC/NAA'                 ; Desc:'IPTC/NAA'),
  (TID:0;TType:0;ICode: 2;Tag: $84E3;  Name:'IT8RasterPadding'         ),
  (TID:0;TType:0;ICode: 2;Tag: $84E5;  Name:'IT8ColorTable'            ),
  (TID:0;TType:0;ICode: 2;Tag: $8649;  Name:'ImageResourceInformation' ),
  (TID:0;TType:0;ICode: 2;Tag: $8769;  Name:'ExifOffset'               ),
  (TID:0;TType:0;ICode: 2;Tag: $8773;  Name:'InterColorProfile'        ),
  (TID:0;TType:0;ICode: 2;Tag: $8822;  Name:'ExposureProgram'          ; Desc:'';Code:
        '0:Unidentified,1:Manual,2:Normal,3:Aperture priority,'+
        '4:Shutter priority,5:Creative(slow),'+
        '6:Action(high-speed),7:Portrait mode,8:Landscape mode'),
  (TID:0;TType:0;ICode: 2;Tag: $8824;  Name:'SpectralSensitivity'    ),
  (TID:0;TType:0;ICode: 2;Tag: $8825;  Name:'GPSInfo'                ),         {170}
  (TID:0;TType:0;ICode: 2;Tag: $8827;  Name:'ISOSpeedRatings'        ),
  (TID:0;TType:0;ICode: 2;Tag: $8828;  Name:'OECF'                   ),
  (TID:0;TType:0;ICode: 2;Tag: $8829;  Name:'Interlace'              ),
  (TID:0;TType:0;ICode: 2;Tag: $882A;  Name:'TimeZoneOffset'         ),
  (TID:0;TType:0;ICode: 2;Tag: $882B;  Name:'SelfTimerMode'          ),
  (TID:0;TType:0;ICode: 2;Tag: $9000;  Name:'ExifVersion'            ),
  (TID:0;TType:0;ICode: 2;Tag: $9003;  Name:'DateTimeOriginal'       ),
  (TID:0;TType:0;ICode: 2;Tag: $9004;  Name:'DateTimeDigitized'      ),
  (TID:0;TType:0;ICode: 2;Tag: $9101;  Name:'ComponentsConfiguration'; Desc:''; Code:''; Data:''; Raw:''; PRaw:0; FormatS:''; Size:0; Callback:GenCompConfig),
  (TID:0;TType:0;ICode: 2;Tag: $9102;  Name:'CompressedBitsPerPixel' ),         {180}
  (TID:0;TType:0;ICode: 2;Tag: $9201;  Name:'ShutterSpeedValue'      ; Desc:''; Code:''; Data:''; Raw:''; PRaw:0; FormatS:''; Size:0; Callback:SSpeedCallBack),
  (TID:0;TType:0;ICode: 2;Tag: $9202;  Name:'ApertureValue'          ; Desc:'Aperture value'; Code:''; Data:''; Raw:''; PRaw:0; FormatS:'F%0.1f'),
  (TID:0;TType:0;ICode: 2;Tag: $9203;  Name:'BrightnessValue'        ),
  (TID:0;TType:0;ICode: 2;Tag: $9204;  Name:'ExposureBiasValue'      ),
  (TID:0;TType:0;ICode: 2;Tag: $9205;  Name:'MaxApertureValue'       ; Desc:''; Code:''; Data:''; Raw:''; PRaw:0; FormatS:'F%0.1f'),
  (TID:0;TType:0;ICode: 2;Tag: $9206;  Name:'SubjectDistance'        ),
  (TID:0;TType:0;ICode: 2;Tag: $9207;  Name:'MeteringMode'           ; Desc:'';Code:'0:Unknown,1:Average,2:Center,3:Spot,4:MultiSpot,5:MultiSegment,6:Partial'),
  (TID:0;TType:0;ICode: 2;Tag: $9208;  Name:'LightSource'            ; Desc:'';Code:'0:Unidentified,1:Daylight,2:Fluorescent,3:Tungsten,10:Flash,17:Std A,18:Std B,19:Std C'),
  (TID:0;TType:0;ICode: 2;Tag: $9209;  Name:'Flash'                  ; Desc:'';Code:''; Data:''; Raw:''; PRaw:0; FormatS:''; Size:0; CallBack:FlashCallBack),
  (TID:0;TType:0;ICode: 2;Tag: $920A;  Name:'FocalLength'            ; Desc:'Focal length'; Code:''; Data:''; Raw:''; PRaw:0; FormatS:'%0.2f mm'), {190}
  (TID:0;TType:0;ICode: 2;Tag: $920B;  Name:'FlashEnergy'             ),
  (TID:0;TType:0;ICode: 2;Tag: $920C;  Name:'SpatialFrequencyResponse'),
  (TID:0;TType:0;ICode: 2;Tag: $920D;  Name:'Noise'                   ),
  (TID:0;TType:0;ICode: 2;Tag: $920E;  Name:'FocalPlaneXResolution'   ),      // TID:0;TType:0;ICode: 2;Tag: $920E    -  -
  (TID:0;TType:0;ICode: 2;Tag: $920F;  Name:'FocalPlaneYResolution'   ),	    // TID:0;TType:0;ICode: 2;Tag: $920F    -  -
  (TID:0;TType:0;ICode: 2;Tag: $9210;  Name:'FocalPlaneResolutionUnit';  Desc:'';Code:'1:None Specified,2:Inch,3:Centimeter'),      // TID:0;TType:0;ICode: 2;Tag: $9210    -  -
  (TID:0;TType:0;ICode: 2;Tag: $9211;  Name:'ImageNumber'            ),
  (TID:0;TType:0;ICode: 2;Tag: $9212;  Name:'SecurityClassification' ),
  (TID:0;TType:0;ICode: 2;Tag: $9213;  Name:'ImageHistory'           ),
  (TID:0;TType:0;ICode: 2;Tag: $9214;  Name:'SubjectLocation'        ),         {200}
  (TID:0;TType:0;ICode: 2;Tag: $9215;  Name:'ExposureIndex'          ),
  (TID:0;TType:0;ICode: 2;Tag: $9216;  Name:'TIFF/EPStandardID'      ),
  (TID:0;TType:0;ICode: 2;Tag: $9217;  Name:'SensingMethod'          ),
  (TID:0;TType:0;ICode: 2;Tag: $923F;  Name:'StoNits'                ),
  (TID:0;TType:0;ICode: 2;Tag: $927C;  Name:'MakerNote'              ),
  (TID:0;TType:0;ICode: 2;Tag: $9286;  Name:'UserComment'            ),
  (TID:0;TType:0;ICode: 2;Tag: $9290;  Name:'SubSecTime'             ),
  (TID:0;TType:0;ICode: 2;Tag: $9291;  Name:'SubSecTimeOriginal'     ),
  (TID:0;TType:0;ICode: 2;Tag: $9292;  Name:'SubSecTimeDigitized'    ),
  (TID:0;TType:0;ICode: 2;Tag: $953C;  Name:'ImageSourceData'        ),  // "Adobe Photoshop Document Data Block": 8BIM...  {210}
//  (TID:0;TType:0;ICode: 2;Tag: $9C9B;  Name:'Title'                  ;  Callback: xpTranslate),  // Win XP specific, Unicode
//  (TID:0;TType:0;ICode: 2;Tag: $9C9C;  Name:'Comments'               ;  Callback: xpTranslate),  // Win XP specific, Unicode
//  (TID:0;TType:0;ICode: 2;Tag: $9C9D;  Name:'Author'                 ;  Callback: xpTranslate),  // Win XP specific, Unicode
//  (TID:0;TType:0;ICode: 2;Tag: $9C9E;  Name:'Keywords'               ;  Callback: xpTranslate),  // Win XP specific, Unicode
//  (TID:0;TType:0;ICode: 2;Tag: $9C9F;  Name:'Subject'                ;  Callback: xpTranslate),  // Win XP specific, Unicode
  (TID:0;TType:0;ICode: 2;Tag: $A000;  Name:'FlashPixVersion'        ),
  (TID:0;TType:0;ICode: 2;Tag: $A001;  Name:'ColorSpace'             ; Desc:''; Code:'0:sBW,1:sRGB'),
  (TID:0;TType:0;ICode: 2;Tag: $A002;  Name:'ExifImageWidth'         ),
  (TID:0;TType:0;ICode: 2;Tag: $A003;  Name:'ExifImageLength'        ),
  (TID:0;TType:0;ICode: 2;Tag: $A004;  Name:'RelatedSoundFile'       ),         {220}
  (TID:0;TType:0;ICode: 2;Tag: $A005;  Name:'InteroperabilityOffset' ),
  (TID:0;TType:0;ICode: 2;Tag: $A20B;  Name:'FlashEnergy'            ),    // TID:0;TType:0;ICode: 2;Tag: $920B in TIFF/EP
  (TID:0;TType:0;ICode: 2;Tag: $A20C;  Name:'SpatialFrequencyResponse'),   // TID:0;TType:0;ICode: 2;Tag: $920C    -  -
  (TID:0;TType:0;ICode: 2;Tag: $A20E;  Name:'FocalPlaneXResolution'   ),      // TID:0;TType:0;ICode: 2;Tag: $920E    -  -
  (TID:0;TType:0;ICode: 2;Tag: $A20F;  Name:'FocalPlaneYResolution'   ),	    // TID:0;TType:0;ICode: 2;Tag: $920F    -  -
  (TID:0;TType:0;ICode: 2;Tag: $A210;  Name:'FocalPlaneResolutionUnit'; Desc:'';Code:'1:None Specified,2:Inch,3:Centimeter'),      // TID:0;TType:0;ICode: 2;Tag: $9210    -  -
  (TID:0;TType:0;ICode: 2;Tag: $A211;  Name:'ImageNumber'             ),
  (TID:0;TType:0;ICode: 2;Tag: $A212;  Name:'SecurityClassification'  ),
  (TID:0;TType:0;ICode: 2;Tag: $A213;  Name:'ImageHistory'            ),
  (TID:0;TType:0;ICode: 2;Tag: $A214;  Name:'SubjectLocation'         ),        {230}
  (TID:0;TType:0;ICode: 2;Tag: $A215;  Name:'ExposureIndex'           ),
  (TID:0;TType:0;ICode: 2;Tag: $A216;  Name:'TIFF/EPStandardID'       ;   Desc:'TIFF/EPStandardID' ),
  (TID:0;TType:0;ICode: 2;Tag: $A217;  Name:'SensingMethod'           ;   Desc:'';Code:'0:Unknown,1:MonochromeArea,'+
    '2:OneChipColorArea,3:TwoChipColorArea,4:ThreeChipColorArea,'+
    '5:ColorSequentialArea,6:MonochromeLinear,7:TriLinear,'+
    '8:ColorSequentialLinear'),	       	           // TID:0;TType:0;ICode: 2;Tag: $9217    -  -
  (TID:0;TType:0;ICode: 2;Tag: $A300;  Name:'FileSource'              ;  Desc:'';Code:'0:Unknown,1:Film scanner,2:Reflection print scanner,3:Digital camera'),
  (TID:0;TType:0;ICode: 2;Tag: $A301;  Name:'SceneType'               ;  Desc:'';Code:'0:Unknown,1:Directly Photographed'),
  (TID:0;TType:0;ICode: 2;Tag: $A302;  Name:'CFAPattern'              ),
  (TID:0;TType:0;ICode: 2;Tag: $A401;  Name:'CustomRendered'          ;  Desc:'';Code:'0:Normal process,1:Custom process'),
  (TID:0;TType:0;ICode: 2;Tag: $A402;  Name:'ExposureMode'            ;  Desc:'';Code:'0:Auto,1:Manual,2:Auto bracket'),
  (TID:0;TType:0;ICode: 2;Tag: $A403;  Name:'WhiteBalance'            ;  Desc:'';Code:'0:Auto,1:Manual'),
  (TID:0;TType:0;ICode: 2;Tag: $A404;  Name:'DigitalZoomRatio'        ),        {240}
  (TID:0;TType:0;ICode: 2;Tag: $A405;  Name:'FocalLengthIn35mmFilm'   ;  Desc:'Focal Length in 35mm Film'; Code:''; Data:''; Raw:''; PRaw:0; FormatS:'%5.2f mm'),
  (TID:0;TType:0;ICode: 2;Tag: $A406;  Name:'SceneCaptureType'        ;  Desc:'';Code:'0:Standard,1:Landscape,2:Portrait,3:Night scene'),
  (TID:0;TType:0;ICode: 2;Tag: $A407;  Name:'GainControl'             ; Desc:''; Code:'0:None,1:Low gain up,2:High gain up,3:Low gain down,4:High gain down'),
  (TID:0;TType:0;ICode: 2;Tag: $A408;  Name:'Contrast'                ; Desc:''; Code:'0:Normal,1:Soft,2:Hard'),
  (TID:0;TType:0;ICode: 2;Tag: $A409;  Name:'Saturation'              ; Desc:''; Code:'0:Normal,1:Low,2:High'),
  (TID:0;TType:0;ICode: 2;Tag: $A40A;  Name:'Sharpness'               ; Desc:''; Code:'0:Normal,1:Soft,2:Hard'),
  (TID:0;TType:0;ICode: 2;Tag: $A40B;  Name:'DeviceSettingDescription'),
  (TID:0;TType:0;ICode: 2;Tag: $A40C;  Name:'SubjectDistanceRange'    ; Desc:''; Code:'0:Unknown,1:Macro,2:Close view,3:Distant view'),  {250}
  (TID:0;TType:0;ICode: 2;Tag: $A420;  Name:'ImageUniqueID'           ; Desc:''; Code:'0:Close view,1:Distant view'),  {250}
  (TID:0;TType:0;ICode: 2;Tag: 0;      Name:'Unknown'));                        {250}


 GPSTable : array [0..GPSCnt-1] of TTagEntry =
 ((TID:0;TType:0;ICode: 2;Tag: $000;   Name:'GPSVersionID'           ),
  (TID:0;TType:0;ICode: 2;Tag: $001;   Name:'GPSLatitudeRef'         ),
  (TID:0;TType:0;ICode: 2;Tag: $002;   Name:'GPSLatitude'            ; Desc:''; Code:''; Data:''; Raw:''; PRaw:0; FormatS:''; Size:0; CallBack:GpsPosn),
  (TID:0;TType:0;ICode: 2;Tag: $003;   Name:'GPSLongitudeRef'        ),
  (TID:0;TType:0;ICode: 2;Tag: $004;   Name:'GPSLongitude'           ; Desc:''; Code:''; Data:''; Raw:''; PRaw:0; FormatS:''; Size:0; CallBack:GpsPosn),
  (TID:0;TType:0;ICode: 2;Tag: $005;   Name:'GPSAltitudeRef'         ;  Desc:''; Code:'0:Above Sealevel,1:Below Sealevel'),
//  (TID:0;TType:0;ICode: 2;Tag: $006;   Name:'GPSAltitude'            ;   CallBack:GpsAltitude),
//  (TID:0;TType:0;ICode: 2;Tag: $007;   Name:'GPSTimeStamp'           ;   CallBack:CvtTime),
  (TID:0;TType:0;ICode: 2;Tag: $008;   Name:'GPSSatellites'          ),
  (TID:0;TType:0;ICode: 2;Tag: $009;   Name:'GPSStatus'              ),
  (TID:0;TType:0;ICode: 2;Tag: $00A;   Name:'GPSMeasureMode'         ),
  (TID:0;TType:0;ICode: 2;Tag: $00B;   Name:'GPSDOP'                 ),
  (TID:0;TType:0;ICode: 2;Tag: $00C;   Name:'GPSSpeedRef'            ),
  (TID:0;TType:0;ICode: 2;Tag: $00D;   Name:'GPSSpeed'               ),
  (TID:0;TType:0;ICode: 2;Tag: $00E;   Name:'GPSTrackRef'            ),
  (TID:0;TType:0;ICode: 2;Tag: $00F;   Name:'GPSTrack'               ),
  (TID:0;TType:0;ICode: 2;Tag: $010;   Name:'GPSImageDirectionRef'   ),
  (TID:0;TType:0;ICode: 2;Tag: $011;   Name:'GPSImageDirection'      ),
  (TID:0;TType:0;ICode: 2;Tag: $012;   Name:'GPSMapDatum'            ),
  (TID:0;TType:0;ICode: 2;Tag: $013;   Name:'GPSDestLatitudeRef'     ),
//  (TID:0;TType:0;ICode: 2;Tag: $014;   Name:'GPSDestLatitude'        ;   CallBack:GpsPosn),
  (TID:0;TType:0;ICode: 2;Tag: $015;   Name:'GPSDestLongitudeRef'    ),
//  (TID:0;TType:0;ICode: 2;Tag: $016;   Name:'GPSDestLongitude'       ;   CallBack:GpsPosn),
  (TID:0;TType:0;ICode: 2;Tag: $017;   Name:'GPSDestBearingkRef'     ),
  (TID:0;TType:0;ICode: 2;Tag: $018;   Name:'GPSDestBearing'         ),
  (TID:0;TType:0;ICode: 2;Tag: $019;   Name:'GPSDestDistanceRef'     ),
  (TID:0;TType:0;ICode: 2;Tag: $01A;   Name:'GPSDestDistance'        ),
  (TID:0;TType:0;ICode: 2;Tag: $01B;   Name:'GPSProcessingMode'      ),
  (TID:0;TType:0;ICode: 2;Tag: $01C;   Name:'GPSAreaInformation'     ),
  (TID:0;TType:0;ICode: 2;Tag: $01D;   Name:'GPSDateStamp'           ),
  (TID:0;TType:0;ICode: 2;Tag: $01E;   Name:'GPSDifferential'        )
  );

  tagInit : boolean = false;

function FindExifTag(ATag: word): PTagEntry;
var
  i: Integer;
begin
  for i:=0 to High(TagTable) do begin
    Result := @TagTable[i];
    if Result^.Tag = ATag then
      exit;
  end;
  Result := nil;
end;

function FindGpsTag(ATag: word): PTagEntry;
var
  i: Integer;
begin
  for i:=0 to High(GpsTable) do begin
    Result := @GpsTable[i];
    if Result^.Tag = ATag then
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

Function InsertSpaces(instr:ansistring):ansistring;
var i:integer;
  rslt:ansistring;
  tc:ansichar;
  lastUc:boolean;
begin
  LastUC := true;
  rslt := copy(instr,1,1);
  for i := 2 to length(instr) do
  begin
    tc := instr[i];
    if (tc >= 'A') and (tc <= 'Z') then
    begin
      if LastUC then
        rslt := rslt+tc
      else
        rslt := rslt+' '+tc;
      LastUc := true;
    end
    else
    begin
      lastUC := false;
      rslt := rslt+tc;
    end;
  end;
  result := rslt;
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
var i:integer;
begin
  result := -1;
  for i := 0 to high(ManuTable) do
    if ManuTable[i].Tag = idx then
    begin
      result := i;
      break;
    end;
end;

function LookupType(idx:integer):ansistring;
var i:integer;
begin
  result := 'Unknown';
  for i := 0 to (sizeof(processTable) div sizeof(TTagEntry))-1 do
    if ProcessTable[i].Tag = idx then
      result := ProcessTable[i].desc;
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


//--------------------------------------------------------------------------
//   tEndInd
//
// Here we implement the Endian Independent layer.  Outside of these methods
// we don't care about endian issues.
//--------------------------------------------------------------------------
function tEndInd.GetDataBuff: AnsiString;
begin
  result := FData;
end;

procedure tEndInd.SetDataBuff(const Value: AnsiString);
begin
  FData := Value;
end;

procedure tEndInd.WriteInt16(var buff: AnsiString; int,posn: integer);
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

procedure tEndInd.WriteInt32(var buff: ansistring; int, posn: longint);
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
function tEndInd.Get16u(AOffs: integer):word;
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
function tEndInd.Get32s(AOffs: integer):Longint;
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
function tEndInd.Put32s(data: Longint): AnsiString;
var
  data2: integer;
  buffer: string[4] absolute data2;
  bbuff: AnsiChar;
begin
  data2 := data;
  if MotorolaOrder then
  begin
    bbuff     := buffer[1];
    buffer[1] := buffer[4];
    buffer[4] := bbuff;
    bbuff     := buffer[2];
    buffer[2] := buffer[3];
    buffer[3] := bbuff;
  end;
  Result := buffer;
end;

// Convert a 32 bit unsigned value from file's native byte order
function tEndInd.Get32u(AOffs: integer): Longword;
begin
  result := Longword(Get32S(AOffs)) and $FFFFFFFF;
end;


{------------------------------------------------------------------------------}
{                            TImageInfo                                        }
{------------------------------------------------------------------------------}
// These destructors provided by Keith Murray
// of byLight Technologies - Thanks!
Destructor TImageInfo.Destroy;
begin
  SetLength(fITagArray,0);
  inherited;
end;

destructor TImgdata.Destroy;
begin
  if assigned(ExifObj) then
    ExifObj.free;
  if assigned(IptcObj) then
    IptcObj.free;
  inherited;
end;

//  This function returns the index of a tag name in the tag buffer.
function TImageInfo.LookupTag(SearchStr: ansistring): integer;
var
  i: integer;
begin
  SearchStr := AnsiString(AnsiUpperCase(SearchStr));
  result := -1;
  for i := 0 to fiTagCount-1 do
    if AnsiString(AnsiUpperCase(fiTagArray[i].Name)) = SearchStr then
    begin
      result := i;
      break;
    end;
end;

//  This function returns the data value for a given tag name.
function TImageInfo.LookupTagVal(SearchStr: ansistring): ansistring;
var
  i: integer;
begin
  SearchStr := AnsiString(AnsiUpperCase(SearchStr));
  result := '';
  for i := 0 to fiTagCount-1 do
    if AnsiString(AnsiUpperCase(fiTagArray[i].Name)) = SearchStr then
    begin
      result := fiTagArray[i].Data;
      break;
    end;
end;
(*
function TImageInfo.LookupTagValue(ASearchStr: String): String;
var
  i: Integer;
  searchStr: AnsiString;
  s: AnsiString;
begin
  Result := '';
  searchStr := AnsiString(Uppercase(ASearchStr));
  for i := 0 to fiTagCount - 1 do
    if searchstr = AnsiUppercase(fiTagArray[i].Name) then
    begin
      s := fiTagArray[i].Raw;
      while s[Length(s)] = #0 do Delete(s, Length(s), 1);
      {$IFDEF FPC}
       {$IFNDEF FPC3+}
       Result := AnsiToUTF8(s);
       {$ELSE}
       Result := s;
       {$ENDIF}
     {$ELSE}
      Result := s;
     {$ENDIF}
    end;
end;
     *)

//  This function returns the integer data value for a given tag name.
Function TImageInfo.LookupTagInt(SearchStr:ansistring):integer;
var
  i: integer;
  x: Double;
  {$IFDEF FPC}
  fs: TFormatSettings;
  {$ELSE}
  res: Integer;
  {$ENDIF}
begin
 SearchStr := AnsiString(AnsiUpperCase(SearchStr));
 result := -1;
 for i := 0 to fiTagCount-1 do
   if AnsiString(AnsiUpperCase(fiTagArray[i].Name)) = SearchStr then
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
            fs.DecimalSeparator := ',';
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
     break;
   end;
end;

//  This function returns the index of a tag name
//  in the tag buffer. It searches by the description
//  which is most likely to be used as a label
Function TImageInfo.LookupTagByDesc(SearchStr:ansistring):integer;
var
  i: integer;
begin
  SearchStr := AnsiString(AnsiUpperCase(SearchStr));
  result := -1;
  for i := 0 to FITagCount-1 do
    if AnsiString(AnsiUpperCase(fiTagArray[i].Desc)) = SearchStr then
    begin
      result := i;
      break;
    end;
end;

Function TImageInfo.GetTagByDesc(SearchStr:ansistring):TTagEntry;
var
  i:integer;
begin
  i := LookupTagByDesc(SearchStr);
  if i >= 0 then
    result := fiTagArray[i]
  else
    result := EmptyEntry;
end;

//  This function returns the index of a tag definition for a given tag name.
function TImageInfo.LookupTagDefn(item:ansistring): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to ExifTagCnt-1 do
  begin
    if AnsiString(AnsiLowerCase(item)) = AnsiString(AnsiLowerCase(TagTable[i].Name)) then
    begin
      result := i;
      break;
    end;
  end;
end;

function LookupTagByID(idx:integer; TagType:integer=ExifTag): integer;
var
  i:integer;
begin
  result := -1;
  case tagType of
    ThumbTag,
    ExifTag: for i := 0 to ExifTagCnt-1 do
               if TagTable[i].Tag = idx then begin
                 result := i;
                 break;
               end;
    GpsTag : for i := 0 to GPSCnt-1 do
               if GPSTable[i].Tag = idx then begin
                 result := i;
                 break;
               end;
  end;
end;

function FetchTagByID(idx:integer; TagType:integer=ExifTag): TTagEntry;
var
  i:integer;
begin
  result := TagTable[ExifTagCnt-1];
  case tagType of
    ThumbTag,
    ExifTag: for i := 0 to ExifTagCnt-1 do
               if TagTable[i].Tag = idx then begin
                 result := TagTable[i];
                 break;
               end;
    GpsTag : for i := 0 to GPSCnt-1 do
               if GPSTable[i].Tag = idx then begin
                 result := GPSTable[i];
                 break;
               end;
  end;
end;

function LookupCode(idx:integer;TagType:integer=ExifTag):ansistring; overload;
var
  i:integer;
begin
  result := '';
  case tagType of
    ThumbTag,
    ExifTag: for i := 0 to ExifTagCnt-1 do
               if TagTable[i].Tag = idx then begin
                 result := TagTable[i].Code;
                 break;
               end;
    GpsTag : for i := 0 to GPSCnt-1 do
               if GPSTable[i].Tag = idx then begin
                 result := GPSTable[i].Code;
                 break;
               end;
  end;
end;

function LookupCode(idx:integer; TagTbl:array of TTagEntry): ansistring; overload;
var
  i: integer;
begin
  result := '';
  for i := 0 to High(TagTbl) do
    if TagTbl[i].Tag = idx then begin
      result := TagTbl[i].Code;
      break;
    end;
end;

procedure TImageInfo.Assign(Source: TImageInfo);
begin
//  FCameraMake     := Source.FCameraMake;
//  FCameraModel    := Source.FCameraModel;
  DateTime        := Source.DateTime;
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

const BadVal = -1;

function TImageInfo.ExifDateToDateTime(dstr:ansistring): TDateTime;
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
begin
  try
    with PConvert( @dstr[1] )^ do
      Result := EncodeDate( StrToInt( year),
                            StrToInt( mon ),
                            StrToInt( day ))
             +  EncodeTime( StrToInt( hr  ),
                            StrToInt( min ),
                            StrToInt( sec ), 0);
  except
    result := 0;
  end;
end;

function TImageInfo.ExtrDateTime(oset:integer):TDateTime;
var tmpStr:ansistring;
begin
  tmpStr := copy(parent.exifSegment^.data,oset,19);
  result := ExifDateToDateTime(tmpStr);
end;

//  2001:01:09 16:17:32
Procedure TImageInfo.SetDateTimeStr(oset:integer; TimeIn:TDateTime);
var tmp:ansistring;
  i:integer;
begin
  tmp := AnsiString(FormatDateTime(EXIFDateFormat, TimeIn));
  for i := 1 to length(tmp) do
    parent.ExifSegment^.data[oset+i-1] := tmp[i];
end;

function TImageInfo.GetImgDateTime: TDateTime;
begin
  if dt_orig_oset > 0 then
    Result := ExtrDateTime(dt_orig_oset)
  else if dt_digi_oset > 0 then
    Result := ExtrDateTime(dt_digi_oset)
  else if dt_modify_oset > 0 then
    Result := ExtrDateTime(dt_modify_oset)
  else
    Result := 0.0;
end;

function TImageInfo.GetDateTimeOriginal: TDateTime;
begin
  if dt_orig_oset > 0 then
    Result := ExtrDateTime(dt_orig_oset)
  else
    Result := 0.0;
end;

procedure TImageInfo.SetDateTimeOriginal(const AValue: TDateTime);
var
  p: PTagEntry;
begin
  p := GetTag(TAG_EXIF_OFFSET, true, 0, FMT_ULONG, true);
  if (AValue = 0) then begin
    RemoveTag(TAG_DATETIME_ORIGINAL, p^.ID);
    exit;
  end;
  p := GetTag(TAG_DATETIME_ORIGINAL, true, p^.ID, FMT_STRING);
  p^.Raw := FormatDateTime(ExifDateFormat, AValue);
  p^.Size := Length(p^.Raw);

  if dt_orig_oset > 0 then
    SetDateTimeStr(dt_orig_oset, AValue)
  else
    raise Exception.Create('Tag DateTimeOriginal not available');
end;

function TImageInfo.GetDateTimeDigitized: TDateTime;
begin
  if dt_digi_oset > 0 then
    Result := ExtrDateTime(dt_digi_oset)
  else
    Result := 0.0;
end;

procedure TImageInfo.SetDateTimeDigitized(const AValue: TDateTime);
var
  p: PTagEntry;
begin
  p := GetTag(TAG_EXIF_OFFSET, true, 0, FMT_ULONG, true);
  if (AValue = 0) then begin
    RemoveTag(TAG_DATETIME_DIGITIZED, p^.ID);
    exit;
  end;
  p := GetTag(TAG_DATETIME_DIGITIZED, true, p^.ID, FMT_STRING);
  p^.Raw := FormatDateTime(ExifDateFormat, AValue);
  p^.Size := Length(p^.Raw);

  if dt_digi_oset > 0 then
    SetDateTimeStr(dt_digi_oset, AValue)
  else
    raise Exception.Create('Tag DateTimeDigitized not available.');
end;

function TImageInfo.GetDateTimeModify: TDateTime;
begin
  if dt_modify_oset > 0 then
    Result := ExtrDateTime(dt_modify_oset)
  else
    Result := 0.0;
end;

procedure TImageInfo.SetDateTimeModify(const AValue: TDateTime);
var
  p: PTagEntry;
begin
  p := GetTag(TAG_DATETIME_MODIFY, true, 0, FMT_STRING);
  if AValue = 0 then begin
    RemoveTag(TAG_DATETIME_MODIFY, p^.ID);
    exit;
  end;
  p^.Raw := FormatDateTime(ExifDateFormat, AValue);
  p^.Size := Length(p^.Raw);

  if dt_modify_oset > 0 then
    SetDateTimeStr(dt_modify_oset, AValue)
  else
    raise Exception.Create('Tag DateTimeModify not available.');
end;




Procedure TImageInfo.AdjDateTime(days,hours,mins,secs:integer);
var delta:double;
    x: TDateTime;
begin
  //                hrs/day     min/day        sec/day
  delta := days + (hours/24)+ (mins/1440) + (secs/86400);
  if dt_modify_oset > 0 then
  begin
    x := ExtrDateTime(dt_modify_oset);
    SetDateTimeStr(dt_modify_oset, x+delta);
  end;
  if dt_orig_oset > 0 then
  begin
    x := ExtrDateTime(dt_orig_oset);
    SetDateTimeStr(dt_orig_oset,x+delta);
  end;
  if dt_digi_oset > 0 then
  begin
    x := ExtrDateTime(dt_digi_oset);
    SetDateTimeStr(dt_digi_oset,x+delta);
  end;
end;

Procedure TImageInfo.OverwriteDateTime(ADateTime: TDateTime);
begin
  if dt_modify_oset > 0 then
    SetDateTimeStr(dt_modify_oset, ADateTime);
  if dt_orig_oset > 0 then
    SetDateTimeStr(dt_orig_oset, ADateTime);
  if dt_digi_oset > 0 then
    SetDateTimeStr(dt_digi_oset, ADateTime);
end;

function TImageInfo.AddTagToArray(nextTag:iTag):integer;
begin
  if nextTag.tag <> 0 then     // Empty fields are masked out
  begin
    if fITagCount >= MaxTag-1 then
    begin
      inc(MaxTag,TagArrayGrowth);
      SetLength(fITagArray,MaxTag);
    end;
    fITagArray[fITagCount] := nextTag;
    inc(fITagCount);
  end;
  result := fITagCount-1;
end;

function TImageInfo.AddTagToThumbArray(nextTag: iTag): integer;
begin
  if nextTag.tag <> 0 then     // Empty fields are masked out
  begin
    if fIThumbCount >= MaxThumbTag-1 then
    begin
      inc(MaxThumbTag,TagArrayGrowth);
      SetLength(fIThumbArray,MaxThumbTag);
    end;
    fIThumbArray[fIThumbCount] := nextTag;
    inc(fIThumbCount);
  end;
  result := fIThumbCount-1;
end;

function TImageInfo.CvtInt(buff: ansistring): longint;
var
  i: integer;
  r: Int64;
begin
  r := 0;
  try
  if MotorolaOrder then
    for i := 1 to length(buff) do
      r := r*256+ord(buff[i])
  else
    for i := length(buff) downto 1 do
      r := r*256+ord(buff[i]);
  except
  end;
  result := longint(r);
end;

function TImageInfo.FormatNumber(buffer:ansistring;fmt:integer;
    fmtStr:ansistring;decodeStr:ansistring=''):ansistring;
var buff2,os:ansistring;
    i,vlen:integer;
    tmp,tmp2:longint;
    dv:double;
begin
  os := '';
  vlen := BYTES_PER_FORMAT[fmt];
  if vlen = 0 then
  begin
    result := '0';
    exit;
  end;
  for i := 0 to min((length(buffer) div vlen), 128)-1 do
  begin
    if os <> '' then
      os := os+DexifDataSep;  // Used for data display
    buff2 := copy(buffer,(i*vlen)+1,vlen);
    case fmt of
      FMT_SBYTE,
      FMT_BYTE,
      FMT_USHORT,
      FMT_ULONG,
      FMT_SSHORT,
      FMT_SLONG:     begin
                       tmp := CvtInt(buff2);
                       if (decodeStr = '') or not DexifDecode then
                         os := os + defIntFmt(tmp) // IntToStr(tmp)
                       else
                         os := os + DecodeField(decodeStr, AnsiString(IntToStr(tmp))); //+
//                           ' ('+IntToStr(tmp)+')';
                     end;
      FMT_URATIONAL,
      FMT_SRATIONAL: begin
                       tmp := CvtInt(copy(buff2,1,4));
                       tmp2 := CvtInt(copy(buff2,5,4));
                       os := os + defFracFmt(tmp,tmp2); //format('%d/%d',[tmp,tmp2]);
                       if (decodeStr <> '') or not DexifDecode then
                         os := os + DecodeField(decodeStr,os); // +' ('+os+')';
                     end;
      FMT_SINGLE,
      FMT_DOUBLE:    begin                       // not used anyway
                       os := os+ '-9999.99';     // not sure how to
                     end;                        // interpret endian issues
    else
      os := os + '?';
    end;
  end;
  if fmtStr <> '' then
  begin
    if Pos('%s', fmtStr) > 0 then
    begin
      os := ansistring(format(fmtStr,[os]));
    end
    else
    begin
      dv := GetNumber(buffer,fmt);
      os := ansistring(format(fmtStr,[dv]));
    end;
  end;
  result := os;
end;

function TImageInfo.GetNumber(buffer:ansistring;fmt:integer):double;
var os:double;
    tmp:longint;
//    dbl:double absolute tmp;
    tmp2:longint;
begin
  try
    case fmt of
      FMT_SBYTE,
      FMT_BYTE,
      FMT_USHORT,
      FMT_ULONG,
      FMT_SSHORT,
      FMT_SLONG:  os := CvtInt(buffer);
      FMT_URATIONAL,
      FMT_SRATIONAL: begin
                       tmp := CvtInt(copy(buffer,1,4));
                       tmp2 := CvtInt(copy(buffer,5,4));
                       os := tmp / tmp2;
                     end;
      FMT_SINGLE: os := PSingle(@buffer[1])^;
      FMT_DOUBLE: os := PDouble(@buffer[1])^;
//    FMT_SINGLE: os := dbl;   // wp: This can't be correct! tmp is indefined here, and single and double ARE different!
//    FMT_DOUBLE: os := dbl;
    else
      os := 0;
    end;
  except
    os := 0;
  end;
  result := os;
end;

function MakePrintable(s:ansistring):ansistring;
var r:ansistring;
  i:integer;
begin
  for i := 1 to min(length(s),50) do
    if not (ord(s[i]) in [32..255]) then
      r := r+'.'
    else
      r := r+s[i];
  result := r;
end;

function MakeHex(s:ansistring):ansistring;
var r:ansistring;
  i:integer;
begin
  for i := 1 to min(length(s),16) do
    r := r + AnsiString(IntToHex(ord(s[i]),2)) + ' ';
  if length(s) > 16 then
    r := r+'...';
  result := r;
end;

var dirStack:ansistring = '';

procedure TImageInfo.clearDirStack;
begin
  dirStack := '';
end;

procedure TImageInfo.pushDirStack(dirStart, offsetbase:longint);
var ts:ansistring;
begin
  ts := '['+AnsiString(IntToStr(offsetbase))+':'+AnsiString(IntToStr(dirStart))+']';
  dirStack := dirStack+ts;
end;

function TImageInfo.testDirStack(dirStart, offsetbase:longint):boolean;
var ts:ansistring;
begin
  ts := '['+AnsiString(IntToStr(offsetbase))+':'+AnsiString(IntToStr(dirStart))+']';
  result := Pos(ts,dirStack) > 0;
end;

//{$DEFINE CreateExifBufDebug}  // uncomment to see written Exif data
{$ifdef CreateExifBufDebug}var CreateExifBufDebug : String;{$endif}

function TImageInfo.CreateExifBuf (parentID:word=0; offsetBase:integer=0 {offsetBase required, because the pointers of subIFD are referenced from parent IFD (WTF!!)}) : String;  // msta Creates APP1 block with IFD0 only
var
  i, f, n : integer;
  size, pDat, p : Cardinal;
  head : String;

  function check (const t : TTagEntry; pid : word) : Boolean; //inline;
  var
    i : integer;
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

  function calcSubIFDSize(pid : integer) : integer;
  var
    i : integer;
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
  for i := 0 to Length(fiTagArray)-1 do begin
    if (not check(fiTagArray[i], parentID)) then
      continue;
    n := n + 1; // calc number of Tags in current IFD
    if (fiTagArray[i].id <> 0) then
      size := size + calcSubIFDSize(fiTagArray[i].id)
    else
      if (Length(fiTagArray[i].Raw) > 4) then
        size := size + Length(fiTagArray[i].Raw);  // calc size
  end;
  pDat := Length(head) + 2 + n*12 + 4; // position of DataArea
  p := pDat;
  size := size + pDat;
  SetLength(Result, size);
  if (parentID = 0) then begin
    head[1] := char(size div 256);
    head[2] := char(size mod 256);
    move(head[1], Result[1], Length(head));             // write header
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
end;

//--------------------------------------------------------------------------
// Process one of the nested EXIF directories.
//--------------------------------------------------------------------------
procedure  TImageInfo.ProcessExifDir(DirStart, OffsetBase, ExifLength: longint;
  ATagType:integer = ExifTag; APrefix:string=''; AParentID: word = 0);
var
  ByteCount: integer;
  tag,TFormat,components: integer;
  de,DirEntry,OffsetVal,NumDirEntries,ValuePtr,subDirStart: Longint;
  RawStr,Fstr,transStr: ansistring;
  msInfo: tmsInfo;
  lookupE, newE: TTagEntry;
  tmpTR: ansistring;
  tmpDateTime: string;
  tagID: word;
begin
  pushDirStack(dirStart,OffsetBase);
  NumDirEntries := Get16u(DirStart);
  if (ExifTrace > 0) then
    TraceStr := TraceStr + crlf +
      ansistring(format('Directory: Start, entries = %d, %d',[DirStart, NumDirEntries]));
  if (DirStart+2+(NumDirEntries*12)) > (DirStart+OffsetBase+ExifLength) then
  begin
    Parent.ErrStr := 'Illegally sized directory';
    exit;
  end;
//Parent.ErrStr:=
//format('%d,%d,%d,%d+%s',[DirStart,NumDirEntries,OffsetBase,ExifLength,
//parent.errstr]);
//  Uncomment to trace directory structure
  if (ATagType = ExifTag) and (ThumbStart = 0) and not TiffFmt then
  begin
    DirEntry := DirStart + 2 + 12*NumDirEntries;
    ThumbStart := Get32u(DirEntry);
    ThumbLength := OffsetBase+ExifLength-ThumbStart;
  end;

  for de := 0 to NumDirEntries-1 do
  begin
    tagID := 0;
    DirEntry := DirStart+2+12*de;
    Tag := Get16u(DirEntry);
    TFormat := Get16u(DirEntry+2);
    Components := Get32u(DirEntry+4);
    ByteCount := Components * BYTES_PER_FORMAT[TFormat];
    if ByteCount = 0 then
      continue;
    If ByteCount > 4 then
    begin
      OffsetVal := Get32u(DirEntry+8);
      ValuePtr := OffsetBase+OffsetVal;
    end
    else
      ValuePtr := DirEntry+8;

    RawStr := copy(parent.EXIFsegment^.data,ValuePtr,ByteCount);
    fstr := '';

    if BuildList in [GenString, GenAll] then
    begin
      LookUpE := FetchTagByID(tag, ATagType);

      with LookUpE do
      begin
        case tformat of
          FMT_UNDEFINED:
            fStr := '"' + StrBefore(RawStr,#0) + '"';
          FMT_STRING:
            begin
              fStr := copy(parent.EXIFsegment^.data, ValuePtr,ByteCount);
              if fStr[ByteCount] = #0 then
                fStr := copy(fStr, 1, ByteCount-1);
            end;
        else
          fStr := FormatNumber(RawStr, TFormat, FormatS, Code);
        end;
        if (Tag > 0) and assigned(callback) and DexifDecode then
          fstr := Callback(fStr)
        else
          fstr := MakePrintable(fstr);
        transStr := Desc;
      end;

      Case tag of
       TAG_USERCOMMENT:
         begin
           // here we strip off comment header
           fStr := trim(copy(RawStr, 9, ByteCount-8));    // msta - one letter is missing, when using ByteCount-9...    // old one is erroneous
//           Comments := AnsiString(trim(string(copy(RawStr,9,ByteCount-9))));
//           fStr := Comments;  // old one is erroneous
//
//           CommentPosn := ValuePtr;
//           CommentSize := ByteCount-9;
         end;
       TAG_DATETIME_MODIFY, //Umwandlung vom EXIF-Format 2009:01:02 12:10:12 nach 2009-01-02 12:10:12
       TAG_DATETIME_ORIGINAL,
       TAG_DATETIME_DIGITIZED:
         begin
           //Konvertierung des EXIF-Formats ins System-Format///////////////////
           DateTimeToString(tmpDateTime, ISODateFormat, ExifDateToDateTime(fStr));
           Fstr := AnsiString(tmpDateTime);
           /////////////////////////////////////////////////////////////////////
         end;
      else
      end;

      //Tracestrings updaten
      tmpTR := crlf +
          siif(ExifTrace > 0,'tag[$'+AnsiString(inttohex(tag,4))+']: ','')+
          transStr + DexifDelim + fstr +
          siif(ExifTrace > 0,' [size: '+AnsiString(inttostr(ByteCount))+']','')+
          siif(ExifTrace > 0,' [start: '+AnsiString(inttostr(ValuePtr))+']','');

      if ATagType = ThumbTag then
        Thumbtrace := ThumbTrace + tmpTR
      else
        TraceStr := TraceStr + tmpTR;
    end;

    //   Additional processing done here:
    Case tag of
       TAG_SUBIFD_OFFSET,
       TAG_EXIF_OFFSET,
       TAG_INTEROP_OFFSET:
         begin
           try
             SubdirStart := OffsetBase + LongInt(Get32u(ValuePtr));
             // some mal-formed images have recursive references...
             // if (subDirStart <> DirStart) then
             if not testDirStack(SubDirStart,OffsetBase) then begin
               tagID := tag; //IDCnt;
//               IDCnt := IDCnt + 1;
               ProcessExifDir(SubdirStart, OffsetBase, ExifLength, ExifTag, '', tagID);
             end;
           except
           end;
         end;
       TAG_GPS_OFFSET:
         begin
           try
             SubdirStart := OffsetBase + LongInt(Get32u(ValuePtr));
             if not testDirStack(SubDirStart,OffsetBase) then begin
               tagID := tag; //idCnt;
//               inc(idCnt);
               ProcessExifDir(SubdirStart, OffsetBase, ExifLength, GpsTag, '', tagID);
             end;
           except
           end;
         end;
       (*
       TAG_MAKE:
         FCameraMake := fstr;
       TAG_MODEL:
         CameraModel := fstr;
         *)
       TAG_EXIFVERSION:
         ExifVersion := rawstr;
       TAG_DATETIME_MODIFY:
         begin
           dt_modify_oset := ValuePtr;
           DateTime := fstr;
         end;
       TAG_DATETIME_ORIGINAL:
         begin
           dt_orig_oset := ValuePtr;
           DateTime := fstr;
         end;
       TAG_DATETIME_DIGITIZED:
         begin
           dt_digi_oset := ValuePtr;
         end;
       TAG_MAKERNOTE:
         begin
            MakerNote := RawStr;
            MakerOffset := ValuePtr;
            Msinfo := tmsinfo.create(TiffFmt,self);
            msAvailable := msInfo.ReadMSData(self);
            FreeAndNil(msinfo);
          end;
       TAG_FLASH:
          FlashUsed := round(getNumber(RawStr, TFormat));
       TAG_IMAGELENGTH,
       TAG_EXIF_IMAGELENGTH:
           begin
             HPosn := DirEntry+8;
             Height := round(getNumber(RawStr, TFormat));
           end;
       TAG_IMAGEWIDTH,
       TAG_EXIF_IMAGEWIDTH:
           begin
             WPosn := DirEntry+8;
             Width := round(getNumber(RawStr, TFormat));
           end;
       TAG_THUMBTYPE:
           if ATagType = ThumbTag then
             ThumbType := round(getNumber(RawStr, TFormat));
    else
      // no special processing
    end;

    if BuildList in [GenList,GenAll] then
    begin
      try
        NewE := LookupE;
        NewE.Data := fstr;
        NewE.Raw := RawStr;
        NewE.Size := length(RawStr);
        NewE.PRaw := ValuePtr;
        NewE.TType := tFormat;
        NewE.ParentID := AParentID;
        NewE.id := tagID;
        if ATagType = ThumbTag then
          AddTagToThumbArray(NewE)
        else
          AddTagToArray(NewE);
      except
        // if we're here: unknown tag.
        // item is recorded in trace string
      end;
    end;
  end;
end;

Procedure TImageInfo.AddMSTag(fname,fstr:ansistring;fType:word);
var  newE: TTagEntry;
begin
  if BuildList in [GenList,GenAll] then
  begin
    try
      newE.Name := fname;
      newE.Desc := fname;
      NewE.Data := fstr;
      NewE.Raw  := fStr;
      NewE.Size := length(fStr);
      NewE.PRaw := 0;
      NewE.TType:= fType;
      newE.parentID := 0;
      newE.id := 0;
      NewE.TID  := 1; // MsSpecific
      AddTagToArray(NewE);
    except
      // if we're here: unknown tag.
      // item is recorded in trace string
    end;
  end;
end;

Procedure TImageInfo.ProcessThumbnail;
var start:integer;
begin
  start := ThumbStart+9;
  ProcessExifDir(start, 9, ThumbLength-12,ThumbTag,'Thumbnail', 1);  // wp: added 1
end;

Procedure TImageInfo.removeThumbnail;
var newSize:integer;
begin
  newSize := ThumbStart-6;
  with parent do
  begin
    SetLength(ExifSegment^.data,newSize);
    ExifSegment^.size := newSize;
  // size calculations should really be moved to save routine
    ExifSegment^.data[1] := ansichar(newSize div 256);
    ExifSegment^.data[2] := ansichar(newSize mod 256);
  end;
end;

procedure TImageInfo.ProcessHWSpecific(MakerBuff:ansistring;
                TagTbl: array of TTagEntry;
                DirStart:longint;
                aMakerOffset:Longint;
                spOffset:integer = 0);
var NumDirEntries:integer;
    de,ByteCount,TagID:integer;
    DirEntry,tag,TFormat,components:integer;
    OffsetVal,ValuePtr:Longint;
    RawStr,Fstr,Fstr2,TagStr,ds:ansistring;
    OffsetBase: longint;
    NewE:TTagEntry;
begin
  DirStart := DirStart+1;
  OffsetBase := DirStart-aMakerOffset+1;
  SetDataBuff(MakerBuff);
  try
    NumDirEntries := Get16u(DirStart);
    for de := 0 to NumDirEntries-1 do
    begin
      DirEntry := DirStart+2+12*de;
      Tag := Get16u(DirEntry);
      TFormat := Get16u(DirEntry+2);
      Components := Get32u(DirEntry+4);
      ByteCount := Components * BYTES_PER_FORMAT[TFormat];
      OffsetVal := 0;
      If ByteCount > 4 then
      begin
        OffsetVal := Get32u(DirEntry+8);
        ValuePtr := OffsetBase+OffsetVal;
      end
      else
        ValuePtr := DirEntry+8;

      // Adjustment needed by Olympus Cameras
      if ValuePtr+ByteCount > length(MakerBuff) then
        RawStr := copy(parent.DataBuff,OffsetVal+spOffset,ByteCount)
      else
        RawStr := copy(MakerBuff,ValuePtr,ByteCount);

      TagID := LookupMTagID(tag,TagTbl);
      if TagID < 0
        then TagStr := 'Unknown'
        else TagStr := TagTbl[TagID].Desc;
      fstr := '';
      if AnsiString(AnsiUpperCase(TagStr)) = 'SKIP' then
        continue;

      if BuildList in [GenList,GenAll] then
      begin
        case tformat of
             FMT_STRING: fStr := '"'+strbefore(RawStr,#0)+'"';
          FMT_UNDEFINED: fStr := '"'+RawStr+'"';
  //         FMT_STRING: fStr := '"'+copy(MakerBuff,ValuePtr,ByteCount-1)+'"';
        else
          try
            ds := siif(dEXIFdecode, LookupCode(tag,TagTbl),'');
            if TagID < 0
              then fStr := FormatNumber(RawStr, TFormat, '', '')
              else fStr := FormatNumber(RawStr, TFormat, TagTbl[TagID].FormatS, ds);
          except
            fStr := '"'+RawStr+'"';
          end;
        end;

        rawDefered := false;
        if (TagID > 0) and assigned(TagTbl[TagID].CallBack) and DexifDecode then
          fstr2 := TagTbl[TagID].CallBack(fstr)
        else
          fstr2 := MakePrintable(fstr);

        if (ExifTrace > 0) then
        begin
          if not rawDefered then
            msTraceStr := msTraceStr + crlf +
              'tag[$'+AnsiString(inttohex(tag,4))+']: '+
              TagStr+DexifDelim+fstr2+
              ' [size: ' +AnsiString(inttostr(ByteCount))+']'+
              ' [raw: '  +MakeHex(RawStr)+']'+
              ' [start: '+AnsiString(inttostr(ValuePtr))+']'
          else
            msTraceStr := msTraceStr + crlf +
              'tag[$'+AnsiString(inttohex(tag,4))+']: '+
              TagStr+DexifDelim+
              ' [size: ' +AnsiString(inttostr(ByteCount))+']'+
              ' [raw: '  +MakeHex(RawStr)+']'+
              ' [start: '+AnsiString(inttostr(ValuePtr))+']'+
              fstr2;
        end
        else
        begin
          if not rawDefered then
            msTraceStr := msTraceStr + crlf +
                          TagStr+DexifDelim+fstr2
          else
            msTraceStr := msTraceStr+
                          fstr2+ // has cr/lf as first element
                          crlf+TagStr+DexifDelim+fstr;
        end;
        (*
        msTraceStr := msTraceStr +crlf+
           siif(ExifTrace > 0,'tag[$'+inttohex(tag,4)+']: ','')+
           TagStr+DexifDelim+fstr+
           siif(ExifTrace > 0,' [size: '+inttostr(ByteCount)+']','')+
           siif(ExifTrace > 0,' [raw: '+MakeHex(RawStr)+']','')+
           siif(ExifTrace > 0,' [start: '+inttostr(ValuePtr)+']','');
        *)
      end;

      if (BuildList in [GenList, GenAll]) and (TagID > 0) then
      begin
        try
          NewE := TagTbl[TagID];

          if rawdefered then
            NewE.Data := fstr
          else
            NewE.Data := fstr2;

          NewE.Raw   := RawStr;
          NewE.TType := tFormat;
          NewE.TID   := 1; // MsSpecific
          
          AddTagToArray(NewE);
        except
          // if we're here: unknown tag.
          // item is recorded in trace string
        end;
      end;

    end;

  except
     on e:exception do
       Parent.ErrStr := 'Error Detected = '+AnsiString(e.message);
  end;

   SetDataBuff(parent.DataBuff);
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
    if parent.ExifSegment = nil then
      Result := ''
    else
    if Parent.ErrStr <> '<none>' then
    begin
      L.Add(Format('File Name:     %s', [ExtractFileName(parent.Filename)]));
      L.Add(Format('Exif Error:    %s', [Parent.ErrStr]));
      Result := L.Text;
    end else
    begin
      DateTimeToString(FileDateTime, ISODateFormat, Parent.FileDateTime);

      L.Add(Format('%-*s %s',      [w, 'File name:', ExtractFileName(Parent.Filename)]));
      L.Add(Format('%-*s %dk',     [w, 'File size:', Parent.FileSize div 1024]));
      L.Add(Format('%-*s %s',      [w, 'File date:', FileDateTime]));
      L.Add(Format('%-*s %s',      [w, 'Photo date:', DateTime]));
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
  if parent.ExifSegment = nil then
    Result := ''
  else
  if Parent.ErrStr <> '<none>' then
    Result := ExtractFileName(parent.Filename) + ' Exif Error: '+Parent.ErrStr
  else
    Result := ExtractFileName(parent.Filename) + ' ' +
              IntToStr(parent.FileSize div 1024) + 'k '+
              DateTime + ' ' +
              IntToStr(Width) + 'w ' + IntToStr(Height)+'h '+
              siif(odd(FlashUsed),' Flash', '');
end;

(*************************************************
The following methods write data back into the
EXIF buffer.
*************************************************)
(*
procedure TImageInfo.SetExifComment( newComment:ansistring);
begin
  WriteThruString('UserComment','ASCII'#0#0#0+newComment);
end;
  *)

procedure TImageInfo.AdjExifSize(nh,nw:longint);
begin
  if (Height <=0) or (Width <=0) then
    exit;
  if (nw <> Width) or (nh <> Height) then
  begin
    parent.WriteInt32(parent.ExifSegment^.data,nh,hPosn);
    parent.WriteInt32(parent.ExifSegment^.data,nw,wPosn);
  end;
end;

procedure TImageInfo.TagWriteThru16(te:ttagentry;NewVal16:word);
begin
  parent.WriteInt16(parent.ExifSegment^.data,newVal16,te.praw);
end;

procedure TImageInfo.TagWriteThru32(te:ttagentry;NewVal32:longint);
begin
  parent.WriteInt16(parent.ExifSegment^.data,newVal32,te.praw);
end;

function TImageInfo.WriteThruInt(tname:ansistring;value:longint):boolean;
var
  te:ttagentry;
  vlen:integer;
begin
  result := false;  // failure
  te := Data[tname];
  if te.Tag = 0 then
    exit;

  result := true;   // success
  vlen := BYTES_PER_FORMAT[te.TType];
  if vlen = 2 then
    TagWriteThru16(te,word(value))
  else
  if vlen = 4 then
    TagWriteThru32(te,value)
  else
    result := false;    // don't recognize the type
end;

function TImageInfo.WriteThruString(tname:ansistring;value:ansistring):boolean;
var
  te:ttagentry;
  i,sPosition:integer;
begin
  result := false;  // failure
  te := Data[tname];
  if te.Tag = 0 then
    exit;
  with parent.ExifSegment^ do
  begin
    sPosition := te.PRaw;
    for i := 0 to te.Size-2 do
      if i > length(value)-1 then
        data[i+sPosition] := #0
      else
        data[i+sPosition] := value[i+1];
    data[sPosition+te.Size-1] := #0; // strings are null terminated
  end;
  result := true;   // success
end;
//
//   Sample call  -
//        ImgData.ExifObj.WriteThruInt('Orientation',3);
//
//*********************************************

constructor TImageInfo.Create(p: timgdata; buildCode: integer = GenAll);
begin
  inherited create;

  LoadTagDescs(True);  // initialize global structures
  FITagCount := 0;
  buildList := BuildCode;
  clearDirStack;
  parent := p;
end;

constructor TImgData.Create(buildCode: integer = GenAll);
begin
  inherited create;

  buildList := BuildCode;
  reset;
end;

function TImageInfo.GetTagElement(TagID: integer): TTagEntry;
begin
  result := fITagArray[TagID]
end;

procedure TImageInfo.SetTagElement(TagID: integer;
  const Value: TTagEntry);
begin
  fITagArray[TagID] := Value;
end;

function TImageInfo.GetTagByName(TagName:ansistring): TTagEntry;
var i:integer;
begin
  i := LookupTag(TagName);
  if i >= 0 then
    result := fITagArray[i]
  else
    result := EmptyEntry;
end;

procedure TImageInfo.SetTagByName(TagName:ansistring; const Value: TTagEntry);
var i:integer;
begin
  i := LookupTag(TagName);
  if i >= 0 then
    fITagArray[i] := Value
  else
  begin
    AddTagToArray(value);
  end;
end;

function TImageInfo.GetTagValueAsString(ATagName: String): String;
var
  tag: TTagEntry;
  name: ansistring;
  s: ansistring;
begin
  Result := '';

  name := AnsiString(ATagName);
  tag := GetTagByName(name);
  if tag.Name = 'Unknown' then
    exit;

  case tag.TType of
    FMT_STRING:
      begin
       {$IFDEF FPC}
        {$IFNDEF FPC3+}
          Result := AnsiToUTF8(tag.Raw);
        {$ELSE}
          Result := tag.Raw;
        {$ENDIF}
       {$ELSE}
          Result := tag.Raw;
       {$ENDIF}
       while Result[Length(Result)] = #0 do
         Delete(Result, Length(Result), 1);
      end;
    FMT_BYTE:
      Result := IntToStr(PByte(@tag.Raw[1])^);
    FMT_SBYTE:
      Result := IntToStr(ShortInt(PByte(@tag.Raw[1])));
    FMT_USHORT:
      Result := IntToStr(PWord(@tag.Raw[1])^);
    FMT_SSHORT:
      Result := IntToStr(SmallInt(PWord(@tag.Raw[1])^));
    FMT_ULONG:
      Result := IntToStr(PDWord(@tag.Raw[1])^);
    FMT_SLONG:
      Result := IntToStr(LongInt(PDWord(@tag.Raw[1])^));
    FMT_SINGLE:
      Result := FloatToStr(PSingle(@tag.Raw[1])^);
    FMT_DOUBLE:
      Result := FloatToStr(PDouble(@tag.Raw[1])^);
    FMT_URATIONAL, FMT_SRATIONAL:
      Result := FloatToStr(CvtRational(tag.Raw));
    FMT_BINARY:
      Result := '<binary>';
  end;
end;

procedure TImageInfo.SetTagValueAsString(ATagName: String; AValue: String);
begin
end;

procedure TImageInfo.RemoveTag(TagID:integer; parentID:word=0);
var i,j : integer;
begin
 j := 0;
 for i := 0 to Length(fiTagArray)-1 do begin
  if (j <> 0) then fiTagArray[i-j] := fiTagArray[i];
  if (fiTagArray[i].ParentID = parentID) and (fiTagArray[i].Tag = TagID) then j := j+1;
 end;
 if (j <> 0) then SetLength(fiTagArray, Length(fiTagArray)-j);
end;

function TImageInfo.GetTag(ATagID: word; AForceCreate: Boolean=false;
  AParentID:word=0; ATagType: word=65535; AForceID: Boolean=false): PTagEntry;
var
  i, j: integer;
begin
  Result := nil;
  for i := 0 to Length(fiTagArray)-1 do
    if (fiTagArray[i].ParentID = AParentID) and (fiTagArray[i].Tag = ATagID) then
    begin
      Result := @fiTagArray[i];
      exit;
    end;
  if AForceCreate then begin
    i := Length(fiTagArray);
    SetLength(fiTagArray, i+1);
    fiTagArray[i].Tag := ATagID;
    for j := 0 to Length(TagTable)-1 do
      if (TagTable[j].Tag = ATagID) then begin
        fiTagArray[i] := TagTable[j];
        break;
      end;
    if (ATagType <> 65535) then
      fiTagArray[i].TType := ATagType;
    fiTagArray[i].ParentID := AParentID;
    fiTagArray[i].Id := 0;
    if AForceID then begin
      j := 1;
      for i := 0 to Length(fiTagArray)-1 do
        if (fiTagArray[i].id >= j) then
          j := fiTagArray[i].id+1;
      fiTagArray[i].Id := j;
    end;
    Result := @fiTagArray[i];
  end;
end;

function TImageInfo.GetArtist: String;
begin
  Result := GetTagValueAsString('Artist');
end;

procedure TImageInfo.SetArtist(v: String);
var
  p : PTagEntry;
begin
  if (v = '') then begin
    RemoveTag(TAG_ARTIST, 0);
    exit;
  end;
  p := GetTag(TAG_ARTIST, true, 0, 2);
  {$IFDEF FPC}
  p^.Raw := UTF8ToAnsi(v) + #0;
  {$ELSE}
  p^.Raw := AnsiString(v) + #0;
  {$ENDIF}
  p^.Data := p^.Raw;
  p^.Size := Length(p^.Raw);
end;

function TImageInfo.GetExifComment: String;
var
  p : PTagEntry;
  w : WideString;
  n: Integer;
  sa: AnsiString;
begin
  Result := '';
  w := '';
  p := GetTag(TAG_EXIF_OFFSET);
  if (p = nil) then
    exit;
  p := GetTag(TAG_USERCOMMENT, false, p^.ID);
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

procedure TImageInfo.SetExifComment(v: String);
var
  p: PTagEntry;
  i: integer;
  w: WideString;
  a: AnsiString;
  u: Boolean;
begin
  p := GetTag(TAG_EXIF_OFFSET, true, 0, 4, true);
  if (v = '') then begin
    RemoveTag(TAG_USERCOMMENT, p^.ID);
    exit;
  end;

  p := GetTag(TAG_USERCOMMENT, true, p^.ID, FMT_BINARY);
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
end;

function TImageInfo.GetImageDescription: String;
begin
  Result := GetTagValueAsString('ImageDescription');
end;

procedure TImageInfo.SetImageDescription(const v: String);
var
  p: PTagEntry;
begin
  if (v = '') then begin
    RemoveTag(TAG_IMAGEDESCRIPTION, 0);
    exit;
  end;
  p := GetTag(TAG_IMAGEDESCRIPTION, true, 0, FMT_STRING);
 {$IFDEF FPC}
  p^.Raw := UTF8ToAnsi(v) + #0;
 {$ELSE}
  p^.Raw := ansistring(v) + #0;
 {$ENDIF}
  p^.Size := Length(p^.Raw);
end;

function TImageInfo.GetCameraMake: String;
begin
  Result := GetTagValueAsString('Make');
end;

procedure TImageInfo.SetCameraMake(const AValue: String);
var
  p: PTagEntry;
begin
  if (AValue = '') then begin
    RemoveTag(TAG_MAKE, 0);
    exit;
  end;
  p := GetTag(TAG_MAKE, true, 0, FMT_STRING);
 {$IFDEF FPC}
  p^.Raw := UTF8ToAnsi(AValue) + #0;
 {$ELSE}
  p^.Raw := AnsiString(AValue) + #0;
 {$ENDIF}
  p^.Data := p^.Raw;
  p^.Size := Length(p^.Raw);

  {
  FCameraMake := AValue;
  WriteThruString('Make', AValue);
  }
end;

function TImageInfo.GetCameraModel: String;
begin
  Result := GetTagValueAsString('Model');
end;

procedure TImageInfo.SetCameraModel(const AValue: String);
var
  p: PTagEntry;
begin
  if (AValue = '') then begin
    RemoveTag(TAG_MODEL, 0);
    exit;
  end;
  p := GetTag(TAG_MODEL, true, 0, FMT_STRING);
 {$IFDEF FPC}
  p^.Raw := UTF8ToAnsi(AValue) + #0;
 {$ELSE}
  p^.Raw := AnsiString(AValue) + #0;
 {$ENDIF}
  p^.Data := p^.Raw;
  p^.Size := Length(p^.Raw);

  {
  FCameraModel := AValue;
  WriteThruString('Model', AValue);
  }
end;

function TImageInfo.IterateFoundTags(TagId: integer; var retVal: TTagEntry): boolean;
begin
  InitTagEntry(retval);
  //FillChar(retVal,sizeof(retVal),0);
  while (iterator < FITagCount) and (FITagArray[iterator].TID <> TagId) do
    inc(iterator);
  if (iterator < FITagCount) then
  begin
    retVal := FITagArray[iterator];
    inc(iterator);
    result := true;
  end
  else
    result := false;
end;

procedure TImageInfo.ResetIterator;
begin
  iterator := 0;
end;

function TImageInfo.IterateFoundThumbTags(TagId: integer;
        var retVal:TTagEntry):boolean;
begin
  InitTagEntry(retVal);
//  FillChar(retVal,sizeof(retVal),0);
  while (iterThumb < FIThumbCount) and (FITagArray[iterThumb].TID <> TagId) do
    inc(iterThumb);
  if (iterThumb < FIThumbCount) then
  begin
    retVal := FIThumbArray[iterThumb];
    inc(iterThumb);
    result := true;
  end
  else
    result := false;
end;

procedure TImageInfo.ResetThumbIterator;
begin
  iterThumb := 0;
end;

function TImageInfo.GetRawFloat( tagName:ansistring ):double;
var tiq :TTagEntry;
begin
  tiq := GetTagByName( tagName );
  if tiq.Tag = 0 // EmptyEntry
    then result := 0.0
    else result := GetNumber(tiq.Raw, tiq.TType);
end;

function TImageInfo.GetRawInt( tagName:ansistring ):integer;
var
  tiq: TTagEntry;
begin
  tiq := GetTagByName(tagName);
  if tiq.Tag = 0 then  // EmptyEntry
    Result := -1
  else
  if (tiq.TType = FMT_UNDEFINED) and (tiq.Size = 1) then
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
  tmp:integer;
  CCDWidth, CCDHeight, fpu, fl, fl35, ratio : double;
  NewE, LookUpE : TTagEntry;
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
  NewE.TType := 3;
  AddTagToArray(NewE);

  TraceStr := TraceStr + crlf +
    siif(ExifTrace > 0, 'tag[$' + IntToHex(tmp,4) + ']: ', '') +
    NewE.Desc + dExifDelim + NewE.Data +
    siif(ExifTrace > 0,' [size: 0]', '') +
    siif(ExifTrace > 0,' [start: 0]', '');
end;

function TImageInfo.EXIFArrayToXML: tstringlist;
var buff:tstringlist;
  i:integer;
begin
  buff := TStringList.Create;
  buff.add('   <EXIFdata>');
  for i := 0 to fiTagCount-1 do
    with fITagArray[i] do
    begin
      buff.add('   <'+name+'>');
      if tag in [105,120] // headline and image caption
        then buff.add('      <![CDATA['+data+']]>')
        else buff.add('      '+data);
      buff.add('   </'+name+'>');
    end;
  buff.add('   </EXIFdata>');
  result := buff;
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

Procedure TImgData.MakeCommentSegment(buff:ansistring);
var bl:integer;
begin
  bl := length(buff)+2;
  if CommentSegment = nil then
  begin
    inc(SectionCnt);
    CommentSegment := @(sections[SectionCnt]);
  end;
  CommentSegment^.data := ansichar(bl div 256)+ansichar(bl mod 256)+buff;
  CommentSegment^.size := bl;
  CommentSegment^.dtype := M_COM;
end;

Function TImgData.GetCommentSegment:ansistring;
begin
  result := '';
  if CommentSegment <> nil then
    result := copy(CommentSegment^.data,2,maxint);
end;

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

function TImgData.SaveExif(jfs2: TStream; EnabledMeta: Byte = $FF;
  FreshExifBlock: Boolean = false): LongInt;
var
  cnt: Longint;
  buff: AnsiString;
  writer: TExifWriter;
begin
  cnt := 0;
  buff := #$FF#$D8;
  jfs2.Write(pointer(buff)^, Length(buff));
  if (EnabledMeta and 1 <> 0) then
  begin
    if FreshExifBlock then
    begin
      // af begin
      if not Assigned(ExifObj) then begin
        EXIFsegment := @sections[SectionCnt+1];
        EXIFobj := TImageInfo.Create(self,BuildList);
        //ExifObj.SetTagElement();
        //EXIFobj.TraceLevel := TraceLevel;
        //ExifObj.CameraModel:= 'Lazarus';
        //ExifObj.CameraMake:= 'dExif V' + DexifVersion;
        //SetDataBuff(EXIFsegment^.data);
      end;
      // af end
      buff := #$FF + Chr(M_EXIF);
      cnt := cnt + jfs2.Write(buff[1], Length(buff));
      buff := ExifObj.CreateExifBuf;
      cnt := cnt + jfs2.Write(buff[1], Length(buff));
      buff := '';
    end else
    if HasExif then begin
      writer := TExifWriter.Create(self);
      try
        writer.WriteExifHeader(jfs2);
        writer.WriteToStream(jfs2);
        cnt := jfs2.Position;
      finally
        writer.Free;
      end;
    end else
    (*
    if (ExifSegment <> nil) then
      with ExifSegment^ do
      begin
        buff := #$FF + chr(Dtype) + data;
        cnt := cnt + jfs2.Write(pointer(buff)^, Length(buff));
      end
    else
    *)
    if (HeaderSegment <> nil) then
      with HeaderSegment^ do
      begin
        buff := chr($FF) + chr(Dtype) + data;
     // buff := #$FF+chr(Dtype)+#$00#$10'JFIF'#$00#$01#$02#$01#$01','#$01','#$00#$00;
        cnt := cnt + jfs2.Write(pointer(buff)^, Length(buff));
      end
    else
    if (cnt = 0) then
    begin
      // buff := chr($FF)+chr(Dtype)+data;
      buff := #$FF + chr(M_JFIF) + #$00#$10'JFIF'#$00#$01#$02#$01#$01','#$01','#$00#$00;       // To do:  contains wrong resolution!!!
      cnt := cnt + jfs2.Write(pointer(buff)^, Length(buff));
    end;
  end;

  if (EnabledMeta and 2 <> 0) and (IPTCSegment <> nil) then
    with IPTCSegment^ do
    begin
      buff := chr($FF) + chr(Dtype) + data;
      cnt := cnt + jfs2.Write(pointer(buff)^, Length(buff));
    end;

  if (EnabledMeta and 4 <> 0) and (CommentSegment <> nil) then
    with CommentSegment^ do
    begin
      buff := chr($FF) + chr(Dtype) + data;
      cnt := cnt + jfs2.Write(pointer(buff)^, Length(buff));
    end;

  Result := cnt;
end;

function TImgData.ExtractThumbnailBuffer: TBytes;
var
  STARTmarker, STOPmarker: integer;
  tb: ansistring;
begin
  result := nil;
  if HasThumbnail then
  begin
    try
      tb := copy(DataBuff, ExifObj.ThumbStart, ExifObj.ThumbLength);
      STARTmarker := Pos(#$ff#$d8#$ff#$db, tb);
      if Startmarker = 0 then
        STARTmarker := Pos(#$ff#$d8#$ff#$c4, tb);
      if STARTmarker <= 0 then
        exit;
      tb := copy(tb,STARTmarker,length(tb));  // strip off thumb data block
      // ok, this is fast and easy - BUT what we really need
      // is to read the length bytes to do the extraction...
      STOPmarker := Pos(#$ff#$d9, tb) + 2;
      tb := copy(tb,1,STOPmarker);
      SetLength(Result, Length(tb));
      Move(tb[1], Result[0], Length(Result));
    except
      // Result will nil...
    end;
  end;
end;

{$IFDEF FPC}
function TImgData.ExtractThumbnailJpeg(AStream: TStream): Boolean;
var
  b: TBytes;
  p: Int64;
begin
  Result := false;
  if (AStream <> nil) and HasThumbnail and (ExifObj.ThumbType = JPEG_COMP_TYPE) then
  begin
    b := ExtractThumbnailBuffer();
    if b <> nil then begin
      p := AStream.Position;
      AStream.WriteBuffer(b[0], Length(b));
      AStream.Position := p;
      Result := true;
    end;
  end;
end;

{$IFNDEF FPC3+}
function JPGImageSize(AStream: TStream): TPoint;
type
  TJPGHeader = array[0..1] of Byte; //FFD8 = StartOfImage (SOI)
  TJPGRecord = packed record
    Marker: Byte;
    RecType: Byte;
    RecSize: Word;
  end;
var
  n: integer;
  hdr: TJPGHeader;
  rec: TJPGRecord = (Marker: $FF; RecType: 0; RecSize: 0);
  p: Int64;
  savedPos: Int64;
begin
  savedPos := AStream.Position;

  Result := Point(0, 0);
  // Check for SOI (start of image) record
  n := AStream.Read(hdr{%H-}, SizeOf(hdr));
  if (n < SizeOf(hdr)) or (hdr[0] <> $FF) or (hdr[1] <> $D8) then
    exit;

  while (AStream.Position < AStream.Size) and (rec.Marker = $FF) do begin
    if AStream.Read(rec, SizeOf(rec)) < SizeOf(rec) then exit;
    rec.RecSize := BEToN(rec.RecSize);
    p := AStream.Position - 2;
    case rec.RecType of
      $C0..$C3:
        if (rec.RecSize >= 4) then // Start of frame markers
        begin
          AStream.Seek(1, soFromCurrent);  // Skip "bits per sample"
          Result.Y := BEToN(AStream.ReadWord);
          Result.X := BEToN(AStream.ReadWord);
          exit;
        end;
      $D9:  // end of image;
        break;
    end;
    AStream.Position := p + rec.RecSize;
  end;
  AStream.Position := savedPos;
end;
{$ENDIF}

{ A jpeg image has been written to a stream. The current EXIF data will be
  merged with this stream and saved to the specified file.
  NOTE: It is in the responsibility of the programmer to make sure that
  AJpeg is a stream of a valid jpeg image. }
procedure TImgData.WriteEXIFJpeg(AJpeg: TStream; AFileName: String;
  AdjSize: Boolean = true);
var
  jms: TMemoryStream;
  jfs: TFileStream;
  imgSize: TPoint;
  NewExifBlock: boolean;
begin
  jfs := TFileStream.Create(AFilename, fmCreate or fmShareExclusive);
  try
    AJpeg.Position := 0;                          // JPEG reader must be at begin of stream
    if AdjSize and (EXIFobj <> nil) then begin
     {$IFDEF FPC3+}
      imgSize := TFPReaderJpeg.ImageSize(AJpeg);  // Read image size from stream
     {$ELSE}
      imgSize := JPGImageSize(AJpeg);
     {$ENDIF}
      EXIFobj.AdjExifSize(imgSize.Y, imgSize.X);  // Adjust EXIF to image size
      AJpeg.Position := 0;                        // Rewind stream
    end;
    //  SaveExif(jfs);
    // if no exif block is here
    //   create a new one
    NewExifBlock:= (ExifObj = nil);
    jms := TMemoryStream.Create;
    try
      jms.CopyFrom(AJpeg, AJpeg.Size);
      MergeToStream(jms, jfs, 255, NewExifBlock);
    finally
      jms.Free;
    end;
  finally
    jfs.Free;
  end;
end;

{ Replaces or adds the currently loaded EXIF data to the image in AOrigName
  and saves as AFileName }
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

{ Write the current EXIF data into the existing jpeg file named AFileName }
procedure TImgData.WriteEXIFJpeg(AFilename: String);
var
  imgStream: TMemoryStream;
begin
  imgStream := TMemoryStream.Create;
  try
    imgStream.LoadFromFile(FileName);
    WriteEXIFJpeg(imgstream, AFileName, false);
  finally
    imgStream.Free;
  end;
end;
{$ENDIF}

{$IFNDEF dExifNoJpeg}
function TImgData.ExtractThumbnailJpeg: TJpegImage;
var
  ms: TMemoryStream;
  b: TBytes;
begin
  Result := nil;
  if HasThumbnail and (ExifObj.ThumbType = JPEG_COMP_TYPE) then
  begin
    b := ExtractThumbnailBuffer();
    if (b = nil) then
      exit;
    ms := TMemoryStream.Create;
    try
      ms.WriteBuffer(b[0], Length(b));
      ms.Position := 0;
      Result := TJpegImage.create;
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
      MergeToStream(jms, jfs, 255, NewExifBlock); // msta
    finally
      jfs.Free;
    end
  finally
    jms.Free;
  end
end;
{$ENDIF}

procedure TImgData.MergeToStream(AInputStream, AOutputStream: TStream;
  AEnabledMeta: Byte = $FF; AFreshExifBlock: Boolean = false);
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
  // Write the header segment and all segments modified by dEXIF to  the
  // beginning of the stream
  AOutputStream.Position := 0;
  SaveExif(AOutputStream, AEnabledMeta, AFreshExifBlock);

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
  HeaderSegment := nil;
  FreeAndNil(IptcObj);
end;

procedure TImgData.ClearComments;
begin
  CommentSegment := nil;
  HeaderSegment := nil;
end;

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

procedure TImgData.SetComment(v: String);
begin
  {$IFDEF FPC}
  MakeCommentSegment(UTF8ToAnsi(v));
  {$ELSE}
  MakeCommentSegment(ansistring(v));
  {$ENDIF}
end;

function TImgData.ReadExifInfo(AFilename: String): boolean;
begin
  ProcessFile(AFilename);
  result := HasMetaData();
end;

function TImgData.ProcessFile(const AFileName: string): boolean;
var
  extn: string;
begin
  reset;
  result := false;
  if not FileExists(aFileName) then
    exit;
  SetFileInfo(aFileName);
  try
      errstr := 'Not an EXIF file';
      extn := LowerCase(ExtractFileExt(filename));
      if (extn = '.jpg') or (extn = '.jpeg') or (extn = '.jpe') then
      begin
        if not ReadJpegFile(FileName) then
          exit;
      end
      else
      if (extn = '.tif') or (extn = '.tiff') or (extn = '.nef') then
      begin
        if not ReadTiffFile(FileName) then
          exit;
      end
      else
      begin
        exit;
      end;
      errstr := '<none>';
//      msAvailable := ReadMSData(Imageinfo);
//      msName := gblUCMaker;
      result := true;
  except
    errstr := 'Illegal Exif construction';
  end;
end;

procedure TImgData.SetFileInfo(fname:ansistring);
var s:tsearchrec;
    stat:word;
begin
   stat := findfirst(fname,faAnyFile,s);
   if stat = 0 then
   begin
     Filename := fname;
     FileDateTime := FileDateToDateTime(s.Time);
     FileSize := s.Size;
   end;
   FindClose(s);
end;

procedure TImgData.CreateIPTCObj;
begin
  MakeIPTCSegment('');
  IPTCobj := TIPTCdata.Create(self);
  // IPTCdata := IPTCobj;  // old style global pointer
end;

//--------------------------------------------------------------------------
// Parse the marker stream until SOS or EOI is seen;
//--------------------------------------------------------------------------
function TImgData.ReadJpegSections (var f: tstream):boolean;
var
  a, b: byte;
  ll, lh, itemlen, marker: integer;
  pw: PWord;
begin
  a := getbyte(f);
  b := getbyte(f);
  if (a <> $ff) or (b <> M_SOI) then
  begin
    result := FALSE;
    exit;
  end;
  SectionCnt := 0;
  while SectionCnt < 20 do  // prevent overruns on bad data
  begin
    repeat
      marker := getByte(f);
    until marker <> $FF;
    Inc(SectionCnt);
    // Read the length of the section.
    lh := getByte(f);
    ll := getByte(f);
    itemlen := (lh shl 8) or ll;
    with Sections[SectionCnt] do
    begin
      DType := marker;
      Size := itemlen;
      setlength(data,itemlen);
      if itemlen > 0 then
        begin
          data[1] := ansichar(lh);
          data[2] := ansichar(ll);
        end;
      try
        F.Read(data[3],itemlen-2);
      except
        continue;
      end;
    end;
    if (SectionCnt = 5) and not HasMetaData() then
      break;  // no exif by 8th - let's not waste time
    case marker of
      M_SOS: begin
               break;
             end;
      M_EOI: begin  // in case it's a tables-only JPEG stream
               break;
             end;
      M_COM: begin // Comment section
               CommentSegment := @sections[SectionCnt];
             end;
      M_IPTC: begin // IPTC section
               if (IPTCSegment = nil) then
               begin
                 IPTCSegment := @sections[SectionCnt];
                 IPTCobj := TIPTCdata.Create(self);
                 // IPTCdata := IPTCobj;  // old style global pointer
               end;
             end;
      M_JFIF: begin
                // Regular jpegs always have this tag, exif images have the exif
                // marker instead, althogh ACDsee will write images with both markers.
                // this program will re-create this marker on absence of exif marker.
               // dec(SectionCnt);
                HeaderSegment := @sections[SectionCnt];
                // break;
              end;
      M_EXIF: begin
                if ((SectionCnt <= 5) and (EXIFsegment = nil) )then
                begin
                    // Seen files from some 'U-lead' software with Vivitar scanner
                    // that uses marker 31 later in the file (no clue what for!)
                    EXIFsegment := @sections[SectionCnt];
                    EXIFobj := TImageInfo.Create(self,BuildList);
                    EXIFobj.TraceLevel := TraceLevel;
                    // ImageInfo := EXIFobj;  // old style global pointer
                    SetDataBuff(EXIFsegment^.data);
                    ProcessEXIF;
                end
                else
                begin
                  // Discard this section.
                  dec(SectionCnt);
                end;
              end;
      M_SOF0: with Sections[SectionCnt] do begin
                pw := @data[4];
                FHeight := BEToN(pw^);
                pw := @data[6];
                FWidth := BEToN(pw^);
                dec(SectionCnt);
              end;
      M_SOF1..M_SOF15: begin
                 // process_SOFn(Data, marker);
             end;
    else
      // break;
    end;
 end;
 result := HasMetaData();
end;

function TImgData.ReadJpegFile(const AFileName: string):boolean;
var
  F: tfilestream;
begin
  TiffFmt := false;  // default mode
  F := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    try
      result := ReadJpegSections(tstream(F));
    except
      result := false;
    end;
  finally
    F.Free;
  end;
end;

function TImgData.ReadTiffSections (var f: tstream):boolean;
var // lh,ll,
    itemlen:integer;
    fmt:ansistring;
begin
  result := true;
  fmt := ansichar(getbyte(f))+ansichar(getbyte(f));
  if (fmt <> 'II') and (fmt <> 'MM') then
  begin
    result := FALSE;
    exit;
  end;

  setlength(Sections[1].data,6);
  F.Read(Sections[1].data[1],6);
{
  // length calculations are inconsistant for TIFFs
  lh := byte(Sections[1].data[1]);
  ll := byte(Sections[1].data[2]);

  if MotorolaOrder
    then itemlen := (lh shl 8) or ll
    else itemlen := (ll shl 8) or lh;
}
//  itemlen := (ll shl 8) or lh;

  itemlen := TiffReadLimit;

  setlength(Sections[1].data,itemlen);
  F.Read(Sections[1].data[1],itemlen);

  SectionCnt := 1;
  EXIFsegment := @(sections[1]);

  EXIFobj := TImageInfo.Create(self,BuildList);
  EXIFobj.TraceLevel := TraceLevel;
  ExifObj.TiffFmt := TiffFmt;
  ExifObj.TraceStr := '';
  EXIFsegment := @sections[SectionCnt];
  ExifObj.DataBuff := Sections[1].data;
  ExifObj.parent.DataBuff :=  Sections[1].data;
  ExifObj.MotorolaOrder := fmt = 'MM';
  EXIFobj.ProcessExifDir(1, -7 , itemlen);
  EXIFobj.Calc35Equiv();
end;

function TImgData.ReadTiffFile(const AFileName: string):boolean;
var
  F: TFileStream;
begin
  TiffFmt := true;
  F := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    try
      result := ReadTiffSections(tstream(F));
    except
      result := false;
    end;
  finally
    F.Free;
  end;
  TiffFmt := false;
end;

Procedure TImgData.ProcessEXIF;
var hdr:ansistring;
    toset:integer;
begin
  if not assigned(ExifObj) then
    ExifObj := TImageInfo.Create(self,BuildList);
  hdr := copy(EXIFsegment^.Data,3,length(validHeader));
  if  hdr <> validHeader then
  begin
    errStr := 'Incorrect Exif header';
    exit;
  end;
  if copy(EXIFsegment^.Data,9,2) = 'II' then
    MotorolaOrder := false
  else if copy(EXIFsegment^.Data,9,2) = 'MM' then
    MotorolaOrder := true
  else
  begin
    errStr := 'Invalid Exif alignment marker';
    exit;
  end;
  ExifObj.TraceStr := '';
  ExifObj.DataBuff := DataBuff;
  ExifObj.MotorolaOrder := MotorolaOrder;

  toset := Get32u(17-4);
  if toset = 0
    then ExifObj.ProcessExifDir(17, 9, EXIFsegment^.Size-6)
    else ExifObj.ProcessExifDir(9+toset, 9, EXIFsegment^.Size-6);
  if errstr <> '' then
  begin
    EXIFobj.Calc35Equiv();
  end;
end;

procedure TImgData.Reset;
begin
  SectionCnt := 0;
  ExifSegment := nil;
  IPTCSegment := nil;
  CommentSegment := nil;
  HeaderSegment := nil;
  Filename := '';
  FileDateTime := 0;
  FileSize := 0;
  ErrStr := '';
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
var
  b: Byte;
begin
  Result := '';
  if ExifObj <> nil then
    Result := ExifObj.LookupTagVal('ResolutionUnit');
  if Result = '' then begin
    b := byte(HeaderSegment^.Data[10]);
    case b of
      1: Result := 'Inches';
      2: Result := 'mm';
    end;
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
  pw: PWord;
begin
  Result := 0;
  if (ExifObj <> nil) then
    Result := ExifObj.LookupTagInt('XResolution');
  if Result < 0 then begin
    pw := @HeaderSegment^.Data[11];
    Result := BEToN(pw^);
  end;
end;

function TImgData.GetYResolution: Integer;
var
  pw: PWord;
begin
  Result := 0;
  if ExifObj <> nil then
    Result := ExifObj.LookupTagInt('YResolution');
  if Result < 0 then begin
    pw := @HeaderSegment^.Data[13];
    Result := BEToN(pw^);
  end;
end;

function TImgData.HasMetaData: boolean;
begin
  result := (EXIFsegment <> nil) or (CommentSegment <> nil) or
            (IPTCsegment <> nil);
end;

function TImgData.HasEXIF: boolean;
begin
  result := (EXIFsegment <> nil);
end;

function TImgData.HasThumbnail: boolean;
begin
  result := (EXIFsegment <> nil) and EXIFobj.HasThumbnail;
end;

function TImgData.HasIPTC: boolean;
begin
  result := (IPTCsegment <> nil);
end;

function TImgData.HasComment: boolean;
begin
  result := (Commentsegment <> nil);
end;

function TImageInfo.HasThumbnail: boolean;
begin
  // 19 is minimum valid starting position
  result := (ThumbStart > 21) and (ThumbLength > 256);
end;

function TImgData.ReadIPTCStrings(fname:ansistring): tstringlist;
begin
  if ProcessFile(fname) and HasIPTC then
    result := IPTCObj.ParseIPTCStrings(IPTCSegment^.Data)
  else
    result := nil;
end;

function TImgData.MetaDataToXML: tstringlist;
var buff,buff2:tstringlist;
  s:tsearchrec;
begin
  if FindFirst(Filename,faAnyFile,s) <> 0 then
  begin
    FindClose(s);
    result := nil;
    exit;
  end;
  buff := TStringList.Create;
  buff.add('<dImageFile>');
  buff.add('   <OSdata>');
  buff.add('      <name> '+ExtractFileName(s.Name)+' </name>');
  buff.add('      <path> '+ExtractFilePath(Filename)+' </path>');
  buff.add('      <size> '+inttostr(s.Size)+' </size>');
  buff.add('      <date> '+DateToStr(FileDateToDateTime(s.time))+' </date>');
  buff.add('   </OSdata>');
  if ExifObj <> nil then
  begin
    buff2 := ExifObj.EXIFArrayToXML;
    if buff2 <> nil then
    begin
      buff.AddStrings(buff2);
      buff2.Clear;
      buff2.Free;
    end;
  end;
  if IptcObj <> nil then
  begin
    buff2 := IptcObj.IPTCArrayToXML;
    if buff2 <> nil then
    begin
      buff.AddStrings(buff2);
      buff2.Clear;
      buff2.Free;
    end;
  end;
  buff.add('</dImageFile>');
  result := buff;
end;

{$IFDEF dEXIFpredeclare}

initialization
  ImgData := TImgData.create;
finalization
  ImgData.Free;
{$ENDIF}
end.
























