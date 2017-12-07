unit dMetadata;

{$IFDEF FPC}
 {$MODE Delphi}
{$ENDIF}

{$I dExif.inc}

interface

uses
  Classes, SysUtils,
 {$IFDEF FPC}
  LazUTF8,
 {$ELSE}
  {$IFNDEF dExifNoJpeg} jpeg, {$ENDIF}
 {$ENDIF}
  dGlobal, dExif, dIPTC;

type
  TImgData = class;

  { TBasicMetadataWriter }
  TBasicMetadataWriter = class
  protected
    FImgData: TImgData;
    FErrLog: TStrings;
    procedure UpdateSegmentSize(AStream: TStream; ASegmentStartPos: Int64);
  public
    constructor Create(AImgData: TImgData); virtual;
    destructor Destroy; override;
    procedure LogError(const AMsg: String);
    procedure WriteToStream(AStream: TStream); virtual;
  end;

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
    FDateTimeFormat: String;
    FDecode: Boolean;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetResolutionUnit: String;
    function GetXResolution: Integer;
    function GetYResolution: Integer;
    procedure SetComment(const AValue: String);
    procedure SetFileInfo(const AFilename: string);
    procedure SetHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);

  protected
    HeaderSegment: pSection;
    function ExtractThumbnailBuffer: TBytes;
    procedure MergeToStream(AInputStream, AOutputStream: TStream;
      AWriteMetadata: TMetadataKinds = mdkAll);
    procedure ProcessEXIF;

  public
    Sections: array[1..21] of TSection;
    TiffFmt: boolean;
    BuildList: integer;
    SectionCnt : integer;
    ExifSegment: pSection;
//    IPTCSegment: pSection;
    ExifObj: TImageInfo;
    IptcObj: TIPTCData;
    TraceLevel: integer;

    function ReadJpegSections(AStream: TStream;
      AMetadataKinds: TMetadataKinds = mdkAll): boolean;
    function ReadTiffSections(AStream: TStream): boolean;
    function SaveExif(AStream: TStream; AWriteMetadata: TMetadataKinds = mdkAll): LongInt;
    procedure Reset;
//    procedure MakeIPTCSegment(buff: ansistring);
    procedure ClearSections;
    procedure ClearEXIF;
    procedure ClearIPTC;
    procedure ClearComments;

//    function FillInIptc: boolean;

  public
    constructor Create(buildCode: integer = GenAll);
    destructor Destroy; override;

    // Manually create empty EXIF and IPTC structures
    function CreateExifObj: TImageInfo;
    function CreateIptcObj: TIptcData;

    // Reading
    function ProcessFile(const AFileName: String;
      AMetadataKinds: TMetadataKinds = mdkAll): boolean;
    function ReadJpegFile(const AFileName: string;
      AMetadataKinds: TMetadataKinds = mdkAll): boolean;
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
    procedure SetError(const AMsg: String);
    property ErrStr: String read FErrStr;

    // Collective output
    procedure MetaDataToXML(AList: TStrings); overload;

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

    // Formatting
    property DateTimeFormat: String read FDateTimeFormat write FDateTimeFormat;
    property Decode: Boolean read FDecode write FDecode default true;

  end; // TImgData


const
  ProcessTable : array [0..29] of TTagEntry =
  (( TID:0; TType:0; Tag:M_SOF0;   Name:'SKIP';Desc: 'Baseline'),
   ( TID:0; TType:0; Tag:M_SOF1;   Name:'';Desc: 'Extended sequential'),
   ( TID:0; TType:0; Tag:M_SOF2;   Name:'';Desc: 'Progressive'),
   ( TID:0; TType:0; Tag:M_SOF3;   Name:'';Desc: 'Lossless'),
   ( TID:0; TType:0; Tag:M_DHT;    Name:'';Desc: 'Define Huffman table'),
   ( TID:0; TType:0; Tag:M_SOF5;   Name:'';Desc: 'Differential sequential'),
   ( TID:0; TType:0; Tag:M_SOF6;   Name:'';Desc: 'Differential progressive'),
   ( TID:0; TType:0; Tag:M_SOF7;   Name:'';Desc: 'Differential lossless'),
   ( TID:0; TType:0; Tag:M_SOF9;   Name:'';Desc: 'Extended sequential, arithmetic coding'),
   ( TID:0; TType:0; Tag:M_SOF10;  Name:'';Desc: 'Progressive, arithmetic coding'),
   ( TID:0; TType:0; Tag:M_SOF11;  Name:'';Desc: 'Lossless, arithmetic coding'),
   ( TID:0; TType:0; Tag:M_SOF13;  Name:'';Desc: 'Differential sequential, arithmetic coding'),
   ( TID:0; TType:0; Tag:M_DAC;    Name:'';Desc: 'Define arithmetic coding conditioning'),
   ( TID:0; TType:0; Tag:M_SOF14;  Name:'';Desc: 'Differential progressive, arithmetic coding'),
   ( TID:0; TType:0; Tag:M_SOF15;  Name:'';Desc: 'Differential lossless, arithmetic coding'),
   ( TID:0; TType:0; Tag:M_SOI;    Name:'';Desc: 'Start of Image'),
   ( TID:0; TType:0; Tag:M_EOI;    Name:'';Desc: 'End of Image'),
   ( TID:0; TType:0; Tag:M_SOS;    Name:'';Desc: 'Start of Scan'),
   ( TID:0; TType:0; Tag:M_DQT;    Name:'';Desc: 'Define quantization table'),
   ( TID:0; TType:0; Tag:M_DNL;    Name:'';Desc: 'Define number of lines'),
   ( TID:0; TType:0; Tag:M_DRI;    Name:'';Desc: 'Restart interoperability definition'),
   ( TID:0; TType:0; Tag:M_DHP;    Name:'';Desc: 'Define hierarchical progression'),
   ( TID:0; TType:0; Tag:M_EXP;    Name:'';Desc: 'Expand reference component'),
   ( TID:0; TType:0; Tag:M_JFIF;   Name:'';Desc: 'JPG marker'),
   ( TID:0; TType:0; Tag:M_EXIF;   Name:'';Desc: 'Exif Data'),
   ( TID:0; TType:0; Tag:M_EXIFEXT; Name:'';Desc: 'Exif Extended Data'),
   ( TID:0; TType:0; Tag:M_COM;    Name:'';Desc: 'Comment'),
   ( TID:0; TType:0; Tag:M_IPTC;   Name:'';Desc: 'IPTC data'),
   ( TID:0; TType:0; Tag:M_APP14;  Name:'';Desc: 'Photoshop data'),
   ( TID:0; TType:0; Tag:0;        Name:'';Desc: 'Unknown')
  );

{$IFDEF dEXIFpredeclare}
var
  ImgData: TImgData;
{$ENDIF}

implementation

uses
  Variants,
  dUtils, dExifWrite, dIptcWrite;

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

procedure TBasicMetadataWriter.UpdateSegmentSize(AStream: TStream;
  ASegmentStartPos: Int64);
var
  startPos: Int64;
  segmentSize: Word;
  w: Word;
begin
  // If the exif structure is part of a jpeg file then WriteExifHeader has
  // been called which determines the position where the Exif header starts.
  if ASegmentStartPos < 0 then
    exit;

  // From the current stream position (at the end) and the position where
  // the segment size must be written, we calculate the size of the segment
  startPos := ASegmentStartPos + SizeOf(word);
  segmentSize := AStream.Position - startPos;

  // Move the stream to where the segment size must be written...
  AStream.Position := startPos;

  // ... and write the segment size.
  w := BEToN(segmentSize);
  AStream.WriteBuffer(w, SizeOf(w));

  // Rewind stream to the end
  AStream.Seek(0, soFromEnd);
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
//                                 TImgData
//------------------------------------------------------------------------------

constructor TImgData.Create(buildCode: integer = GenAll);
begin
  inherited create;
  FDateTimeFormat := ISO_DATETIME_FORMAT;
  FDecode := true;
  buildList := BuildCode;
  Reset;
end;

destructor TImgdata.Destroy;
begin
  ExifObj.Free;
  IptcObj.Free;
  inherited;
end;

{
procedure TImgData.MakeIPTCSegment(buff: ansistring);
var bl:integer;
begin
  bl := length(buff)+2;
  if IPTCSegment = nil then
  begin
    inc(SectionCnt);
    IPTCSegment := @(sections[SectionCnt]);
  end;
  IPTCSegment^.Data := ansichar(bl div 256) + ansichar(bl mod 256) + buff;
  IPTCSegment^.Size := bl;
  IPTCSegment^.DType := M_IPTC;
end;
}
function TImgData.SaveExif(AStream: TStream;
  AWriteMetadata: TMetadataKinds = mdkAll): LongInt;
const
  SOI_MARKER: array[0..1] of byte = ($FF, $D8);
  JFIF_MARKER: array[0..1] of byte = ($FF, $E0);
  JFIF: ansistring = 'JFIF'#0;
var
  APP0Segment: TJFIFSegment;
  buff: AnsiString;
  writer: TBasicMetadataWriter;
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
      TExifWriter(writer).BigEndian := MotorolaOrder;
      TExifWriter(writer).WriteExifHeader(AStream);
      writer.WriteToStream(AStream);
    finally
      writer.Free;
    end;
  end;

  // Write IPTCSegment
  if (mdkIPTC in AWriteMetadata) and HasIPTC then begin
    writer := TIptcWriter.Create(self);
    try
      TIptcWriter(writer).WriteIPTCHeader(AStream);
      TIptcWriter(writer).WriteToStream(AStream);
    finally
      writer.Free;
    end;
  end;
  (*
    MakeIPTCSegment(IPTCObj.IPTCArrayToBuffer);      // Create IPTC segment from buffer
    with IPTCSegment^ do
    begin
      buff := chr($FF) + chr(Dtype) + data;
      AStream.Write(pointer(buff)^, Length(buff));
    end;
  end;
  *)

  // Write comment segment
  if (mdkComment in AWriteMetadata) and HasComment then begin
   {$IFDEF FPC}
    {$IFDEF FPC+}
    a := UTF8ToWinCP(FComment);
   {$ELSE}
     a := UTF8ToAnsi(FComment);
    {$ENDIF}
   {$ELSE}
    a := AnsiString(FComment);
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
  //NewExifBlock: boolean;
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
    //NewExifBlock:= (ExifObj = nil);
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
     (*
function TImgData.FillInIptc:boolean;
begin
  if IPTCSegment = nil then
    CreateIPTCObj
  else
    IPTCObj.ParseIPTCArray(IPTCSegment^.Data);
    // To do: Here's a memory leak because ParseIPTCArray returns a StringList which is not destroyed!

//    filename := FName;
  result := IPTCObj.HasData();
end;   *)

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
  FreeAndNil(IptcObj);
end;

procedure TImgData.ClearComments;
begin
  FComment := '';
end;

procedure TImgData.SetComment(const AValue: String);
var
  w: WideString;
begin
  // Check whether the provided string fits into a 64k segment
 {$IFDEF FPC}
  w := UTF8Decode(AValue);
 {$ENDIF}
  if Length(w) > Word($FFFF) - 4 then
    raise Exception.CreateFmt('Comment too long, max %d characters', [Word($FFFF) - 4]);
  FComment := AValue;
end;

function TImgData.ReadExifInfo(AFilename: String): boolean;
begin
  ProcessFile(AFilename);
  result := HasMetaData();
end;

function TImgData.ProcessFile(const AFileName: string;
  AMetadataKinds: TMetadataKinds = mdkAll): boolean;
var
  ext: string;
begin
  Reset;
  Result := false;
  if not FileExists(aFileName) then
    exit;

  SetFileInfo(AFileName);
  try
//    FErrStr := 'Not an EXIF file';
    ext := LowerCase(ExtractFileExt(filename));
    if (ext = '.jpg') or (ext = '.jpeg') or (ext = '.jpe') then
    begin
      if not ReadJpegFile(FileName, AMetadataKinds) then begin
        FErrStr := 'Cannot read "' + AFileName + '"';
        exit;
      end;
    end else
    if (ext = '.tif') or (ext = '.tiff') or (ext = '.nef') then
    begin
      if not ReadTiffFile(FileName) then begin
        FErrStr := 'Cannot read "' + AFileName + '"';
        exit;
      end;
    end else begin
      FErrStr := 'File format not supported';
      exit;
    end;

    FErrStr := NO_ERROR;
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
  FErrStr := NO_ERROR;
  Result := ExifObj;
end;

function TImgData.CreateIPTCObj: TIptcData;
begin
  IptcObj.Free;
  IptcObj := TIptcData.Create(self);
  Result := IptcObj;
end;

//--------------------------------------------------------------------------
// Parse the marker stream until SOS or EOI is seen
//--------------------------------------------------------------------------
function TImgData.ReadJpegSections(AStream: TStream;
  AMetadataKinds: TMetadataKinds = mdkAll): boolean;
var
  a, b: byte;
  ll, lh, itemLen, marker: integer;
  pw: PWord;
  sa: ansistring;
  buf: ansistring;
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
        if mdkComment in AMetadataKinds then
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
          if (mdkIPTC in AMetadataKinds) then
          begin
            IPTCobj := TIPTCdata.Create(self);
            IPTCobj.ParseIPTCArray(Sections[SectionCnt].Data);
          end else
            dec(SectionCnt);  // Segment not needed any more.
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
          if (mdkEXIF in AMetadataKinds) and (SectionCnt <= 5) and (EXIFsegment = nil) then
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

function TImgData.ReadJpegFile(const AFileName: string;
  AMetadataKinds: TMetadataKinds = mdkAll): boolean;
var
  fs: TFilestream;
begin
  ClearSections;
  TiffFmt := false;  // default mode
  fs := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    try
      result := ReadJpegSections(fs, AMetadataKinds);
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
  TImgData(ExifObj.parent).DataBuff :=  Sections[1].data;
  ExifObj.MotorolaOrder := fmt = 'MM';
  EXIFobj.ProcessExifDir(1, -7 , itemlen);
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
  ExifObj.ProcessThumbnail;
end;

procedure TImgData.Reset;
begin
  SectionCnt := 0;
  ExifSegment := nil;
  FComment := '';
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
    v := ExifObj.TagValue['ResolutionUnit'];   // 1=none, 2=Inch, 3=cm
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
    v := ExifObj.TagValue['XResolution'];
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
    v := ExifObj.TagValue['YResolution'];
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
  result := Assigned(ExifObj);
end;

function TImgData.HasThumbnail: boolean;
begin
  Result := Assigned(ExifObj) and ExifObj.HasThumbnail;
end;

function TImgData.HasIPTC: boolean;
begin
  result := Assigned(IptcObj) and (IptcObj.Count > 0);
end;

function TImgData.HasComment: boolean;
begin
  result := FComment <> '';
end;

procedure TImgData.SetError(const AMsg: String);
begin
  FErrStr := AMsg;
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
  ProcessFile(AFilename, [mdkIPTC]);
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

