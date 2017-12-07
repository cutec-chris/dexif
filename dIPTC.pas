unit dIPTC;
////////////////////////////////////////////////////////////////////////////////
// unit IPTC - Copyright 2001-2006, Gerry McGuire
//--------------------------------------------------------------------------
// Program to pull the IPTC (Photoshop) information out of various
// types of digital camera files.  This information can coexist in
// files containing EXIF data.  See README.TXT and LICENSE.TXT for
// information regarding the lawful use of this code.
//
// Initial Delphi unit - Gerry McGuire September 2001 - V 0.9 beta
//--------------------------------------------------------------------------
// This is based on an example provided by Earl F. Glynn.
// His web pages on graphics and Delphi programming at http://www.efg2.com
// have no equal!
//--------------------------------------------------------------------------
// I have found several often conflicting IPTC definitions in use.
// This code is designed to be easily extended.  For each new field
// enter one line in the IPTCTable and increment the TagCnt constant.
//--------------------------------------------------------------------------

{$IFDEF FPC}
 {$MODE Delphi}
 {$WARN 3177 off : Some fields coming after "$1" were not initialized}
{$ENDIF}

{$I dExif.inc}


interface

uses
  classes, sysutils,
 {$IFNDEF FPC}
  {$IFNDEF dExifNoJpeg} jpeg, {$ENDIF}
 {$ENDIF}
 {$IFDEF MSWINDOWS}
  windows,
 {$ENDIF}
 {$IFDEF UNIX}
  unix,unixutil,
 {$endif}
  dglobal;

const
  dIPTCVersion: ansistring = '1.04';
  TagArrayGrowth = 25;

type
  TTagDefArray = array of TTagEntry;

  {
  ITag = record
    ICode: word;
    Tag:  word;
    Name:ansistring;
    Desc:ansistring;
    Size: word;
    Data:ansistring;
  end;
    }
  ITag = TTagEntry;

  TIPTCdata = class
  private
    function GetCount: integer;
    procedure SetCount(const AValue: integer);
    function GetTagElement(TagID: integer): ITag;
    procedure SetTagElement(TagID: integer; const Value: ITag);
    function GetTagValueAsString(ATagName: String): String;
    procedure SetTagValueAsString(ATagName, AValue: String);
    function GetTimeZoneStr: string;
    procedure SetDateTimePrim(TimeIn: TDateTime; prefix: string);

  protected
    FBuffer: ansistring;
    MaxTag: integer;
    FParent: TObject;
    fITagCount : integer;
    fITagArray: array of iTag;
    function ExtractTag(const ABuffer: ansistring; var AStart: integer): ITag;
    function RawToData(ARaw: ansistring; ATagType: Byte): String;
  public
    constructor Create(AParent: TObject);
    procedure Reset;
    function HasData: boolean;
    function Clone(ASource: TIPTCData): TIPTCData;

//    function IPTCArrayToBuffer:ansistring;
    procedure IPTCArrayToList(AList: TStrings);
    procedure IPTCArrayToXML(AList: TStrings);

    procedure ParseIPTCStrings(buff: ansistring; AList: TStrings); overload;
    procedure ParseIPTCArray(ABuffer: ansistring);

    function LookupTag(ATagName: String): integer;
    function LookupNextTag(AtagName: String; ATagIndex: Integer): Integer;
    function LookupTagByDesc(ATagDesc: String): integer;

    procedure RemoveTag(ATagName: String); virtual;
    function AddTag(ATagName: String; ARawVal: ansistring = '';
      AForceNew: Boolean = false): integer; virtual;
    function AppendToTag(ATagName: String; ARawVal: ansistring): integer; virtual;
    function AddOrAppend(ATagName: String; ARawVal: ansistring): integer; virtual;
    function UpdateTagDesc(ATagName: String; AValue: string): integer;
    function GetTag(ATagName: String; ADefaultVal: string=''): string; virtual;
    procedure SetTagByIdx(idx: Integer; ADataVal: String);

    class procedure ReadFileStrings(const AFilename: String; AList: TStrings);

    function AddTagToArray(ANewTag: iTag): integer;
    function GetDateTime: TDateTime;
    procedure SetDateTime(TimeIn: TDateTime);
    procedure SetDateTimeExt(TimeIn: TDateTime; APrefix:ansistring);
    function GetMultiPartTag(ATagName: String): TStringList;

    property ITagArray[TagID:integer]: ITag
        read GetTagElement write SetTagElement; default;
    property Count: integer read GetCount write SetCount;
    property TagValueAsString[ATagName: String]: String
        read GetTagValueAsString write SetTagValueAsString;
  end;

const
  IptcTagCnt = 50;
  MultiTagSep: ansistring = ',';
  MultiTagCount = $FFFF;

var
  rawDefered : boolean = false;
  defaultTimeZone:ansistring = '_0000';

  { In its specification a tag is identified by two 1-byte: its record number
    and its dataset number, written as a pair separated by a color, e.g, 2:3
    -- record 2, dataset 3 (--> "ObjectTape")

    Since TTagEntry reserves two bytes for "Tag" we combine both values into
    the element tag = RecordNo shl 8 + DatasetNo

    Source of dataset and record numbers:
    https://sno.phy.queensu.ca/~phil/exiftool/TagNames/IPTC.html

    If Count = $FFFF then the tag can have multiple values.
  }
  IPTCTable : array [0..IPTCTAGCNT-1] of ITag = (
    ( TID:0; TType:2; Tag:$015A {1:90}; Count:1;     Name:'CodedCharacterSet';Desc:'Coded character set'; Code:''; Data:''; Raw:''; FormatS:''; Size:32),
    ( TID:0; TType:3; Tag:$0200 {2: 0}; Count:1;     Name:'SKIP';             Desc:'Record version';Code:'';Data:'';Raw:'';FormatS:'';Size:64),
    ( TID:0; TType:2; Tag:$0203 {2: 3}; Count:1;     Name:'ObjectType';       Desc:'Object type ref';Code:'';Data:'';Raw:'';FormatS:'';Size:67),
    ( TID:0; TType:2; Tag:$0204 {2: 4}; Count:$FFFF; Name:'ObjectAttr';       Desc:'Object attribute ref';  Code:'';Data:'';Raw:'';FormatS:'';Size:67),
    ( TID:0; TType:2; Tag:$0205 {2: 5}; Count:1;     Name:'ObjectName';       Desc:'Object name';  Code:'';Data:'';Raw:'';FormatS:'';Size:64),
    ( TID:0; TType:2; Tag:$0207 {2: 7}; Count:1;     Name:'EditStatus';       Desc:'Edit status';  Code:'';Data:'';Raw:'';FormatS:'';Size:64),
    ( TID:0; TType:2; Tag:$0208 {2: 8}; Count:1;     Name:'EditorialUpdate';  Desc:'Editorial update';  Code:'';Data:'';Raw:'';FormatS:'';Size:2),
    ( TID:0; TType:2; Tag:$020A {2:10}; Count:1;     Name:'Urgency';          Desc:'Urgency';      Code:'';Data:'';Raw:'';FormatS:'';Size:1),
    ( TID:0; TType:2; Tag:$020C {2:12}; Count:$FFFF; Name:'SubRef';           Desc:'Subject reference';     Code:'';Data:'';Raw:'';FormatS:'';Size:236),
    ( TID:0; TType:2; Tag:$020F {2:15}; Count:1;     Name:'Category';         Desc:'Category';     Code:'';Data:'';Raw:'';FormatS:'';Size:3),
    ( TID:0; TType:2; Tag:$0214 {2:20}; Count:$FFFF; Name:'SuppCategory';     Desc:'Supplemental category'; Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0216 {2:22}; Count:1;     Name:'FixtureID';        Desc:'Fixture ID';   Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0219 {2:25}; Count:$FFFF; Name:'Keywords';         Desc:'Keywords';     Code:'';Data:'';Raw:'';FormatS:'';Size:64),
    ( TID:0; TType:2; Tag:$021A {2:26}; Count:$FFFF; Name:'ContentLocCode';   Desc:'Content location code'; Code:'';Data:'';Raw:'';FormatS:'';Size: 3),
    ( TID:0; TType:2; Tag:$021B {2:27}; Count:$FFFF; Name:'ContentLocName';   Desc:'Content location name'; Code:'';Data:'';Raw:'';FormatS:'';Size: 64),
    ( TID:0; TType:2; Tag:$021E {2:30}; Count:1;     Name:'ReleaseDate';      Desc:'Release date'; Code:'';Data:'';Raw:'';FormatS:'';Size:8),
    ( TID:0; TType:2; Tag:$0223 {2:35}; Count:1;     Name:'ReleaseTime';      Desc:'Release time'; Code:'';Data:'';Raw:'';FormatS:'';Size:11),
    ( TID:0; TType:2; Tag:$0225 {2:37}; Count:1;     Name:'ExpireDate';       Desc:'Expiration date'; Code:'';Data:'';Raw:'';FormatS:'';Size:8),
    ( TID:0; TType:2; Tag:$0226 {2:38}; Count:1;     Name:'ExpireTime';       Desc:'Expiration time'; Code:'';Data:'';Raw:'';FormatS:'';Size:11),
    ( TID:0; TType:2; Tag:$0228 {2:40}; Count:1;     Name:'SpecialInstruct';  Desc:'Special instructions'; Code:'';Data:'';Raw:'';FormatS:'';Size:256),
    ( TID:0; TType:2; Tag:$022A {2:42}; Count:1;     Name:'ActionAdvised';    Desc:'Action advised'; Code:'';Data:'';Raw:'';FormatS:'';Size:2),
    ( TID:0; TType:2; Tag:$022D {2:45}; Count:$FFFF; Name:'RefService';       Desc:'Reference service'; Code:'';Data:'';Raw:'';FormatS:'';Size:10),
    ( TID:0; TType:2; Tag:$022F {2:47}; Count:$FFFF; Name:'RefDate';          Desc:'Reference date'; Code:'';Data:'';Raw:'';FormatS:'';Size:8),
    ( TID:0; TType:2; Tag:$0232 {2:50}; Count:$FFFF; Name:'RefNumber';        Desc:'Reference number'; Code:'';Data:'';Raw:'';FormatS:'';Size:8),
    ( TID:0; TType:2; Tag:$0237 {2:55}; Count:1;     Name:'DateCreated';      Desc:'Date created'; Code:'';Data:'';Raw:'';FormatS:'';Size:8),
    ( TID:0; TType:2; Tag:$023C {2:60}; Count:1;     Name:'TimeCreated';      Desc:'Time created'; Code:'';Data:'';Raw:'';FormatS:'';Size:11),
    ( TID:0; TType:2; Tag:$023E {2:62}; Count:1;     Name:'DigitizeDate';     Desc:'Digital creation date'; Code:'';Data:'';Raw:'';FormatS:'';Size:8),
    ( TID:0; TType:2; Tag:$023F {2:63}; Count:1;     Name:'DigitizeTime';     Desc:'Digital creation time'; Code:'';Data:'';Raw:'';FormatS:'';Size:11),
    ( TID:0; TType:2; Tag:$0241 {2:65}; Count:1;     Name:'OriginatingProgram'; Desc:'Originating program'; Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0246 {2:70}; Count:1;     Name:'ProgramVersion';   Desc:'Program version'; Code:'';Data:'';Raw:'';FormatS:'';Size: 10),
    ( TID:0; TType:2; Tag:$024B {2:75}; Count:1;     Name:'ObjectCycle';      Desc:'Object cycle'; Code:'';Data:'';Raw:'';FormatS:'';Size:1),
    ( TID:0; TType:2; Tag:$0250 {2:80}; Count:$FFFF; Name:'ByLine';           Desc:'ByLine';       Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0255 {2:85}; Count:$FFFF; Name:'ByLineTitle';      Desc:'ByLine title'; Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$025A {2:90}; Count:1;     Name:'City';             Desc:'City';         Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$025C {2:92}; Count:1;     Name:'SubLocation';      Desc:'Sublocation';  Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$025F {2:95}; Count:1;     Name:'State';            Desc:'Province/State';  Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0264 {2:100};Count:1;     Name:'LocationCode';     Desc:'Country/primary location code'; Code:'';Data:'';Raw:'';FormatS:'';Size:3),
    ( TID:0; TType:2; Tag:$0265 {2:101};Count:1;     Name:'LocationName';     Desc:'Country/primary location name'; Code:'';Data:'';Raw:'';FormatS:'';Size:64),
    ( TID:0; TType:2; Tag:$0267 {2:103};Count:1;     Name:'TransmissionRef';  Desc:'Original transmission reference';     Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0269 {2:105};Count:1;     Name:'ImageHeadline';    Desc:'Image headline'; Code:'';Data:'';Raw:'';FormatS:'';Size:256),
    ( TID:0; TType:2; Tag:$026E {2:110};Count:1;     Name:'ImageCredit';      Desc:'Image credit';  Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0273 {2:115};Count:1;     Name:'Source';           Desc:'Source';        Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0274 {2:116};Count:1;     Name:'Copyright';        Desc:'Copyright notice';  Code:'';Data:'';Raw:'';FormatS:'';Size:128),
    ( TID:0; TType:2; Tag:$0276 {2:118};Count:$FFFF; Name:'Contact';          Desc:'Contact';       Code:'';Data:'';Raw:'';FormatS:'';Size:128),
    ( TID:0; TType:2; Tag:$0278 {2:120};Count:1;     Name:'ImageCaption';     Desc:'Image caption'; Code:'';Data:'';Raw:'';FormatS:'';Size:2000),
    ( TID:0; TType:2; Tag:$027A {2:122};Count:1;     Name:'ImageCaptionWriter'; Desc:'Image caption writer'; Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0282 {2:130};Count:1;     Name:'ImageType';        Desc:'Image type';    Code:'';Data:'';Raw:'';FormatS:'';Size:2),
    ( TID:0; TType:2; Tag:$0283 {2:131};Count:1;     Name:'Orientation';      Desc:'Image orientation'; Code:'';Data:'';Raw:'';FormatS:''; Size:1),
    ( TID:0; TType:2; Tag:$0287 {2:135};Count:1;     Name:'LangID';           Desc:'Language ID';   Code:'';Data:'';Raw:'';FormatS:'';Size:3),
    ( TID:0; TType:0; Tag:$080A {8:10}; Count:$FFFF; Name:'Subfile';          Desc:'Subfile';       Code:'';Data:'';Raw:'';FormatS:'';Size:2)
   );

function LookupTagDef(AName: String): integer;

procedure IPTCWriteTransFile(const AFileName: String);
function IPTCReadTransFile(const AFileName: String): boolean;

procedure InitITag(out ATag: ITag);


implementation

uses
  dUtils, dMetadata, dEXIF;

procedure InitITag(out ATag: ITag);
begin
  InitTagEntry(TTagEntry(ATag));
end;

//  This function returns the index of a tag definition for a given tag name.
function LookupTagDef(AName: string): integer;
var
  i:integer;
begin
  Result := -1;
  for i := 0 to High(IptcTable) do
  begin
    if LowerCase(AName) = LowerCase(IPTCtable[i].Name) then
    begin
      Result := i;
      break;
    end;
  end;
end;


//------------------------------------------------------------------------------
//                             TIPTCData
//------------------------------------------------------------------------------

constructor TIPTCdata.Create(AParent: TObject);
begin
  inherited Create;
  fITagCount := 0;
  FParent := AParent;
end;

function TIPTCdata.GetCount: integer;
begin
  result := fITagCount;
end;

procedure TIPTCdata.SetCount(const AValue: integer);
begin
  fITagCount := AValue;
end;

function TIPTCdata.GetTagElement(TagID: integer): ITag;
begin
  Result := fITagArray[TagID]
end;

procedure TIPTCdata.SetTagElement(TagID: integer; const Value: ITag);
begin
  fITagArray[TagID] := Value;
end;

function TIptcData.GetTagValueAsString(ATagName: String): String;
var
  idx: integer;
begin
  idx := LookupTag(ATagName);
  if idx >= 0 then begin
    Result := ITagArray[idx].Data;
    {
    if ITagArray[idx].Count = MultiTagCount then
      while true do begin
        idx := LookupNextTag(ATagName, idx+1);
        if idx > -1 then
          Result := Result + MultiTagSep + ITagArray[idx].Data
        else
          exit;
      end;
      }
  end else
    Result := '';
end;

procedure TIptcData.SetTagValueAsString(ATagName, AValue: String);
begin
  AddOrAppend(ATagName, AValue);
end;

{ Note: recordNo : datasetNo
  - 1 byte:  tag "marker" ($1C)  // has already been read by caller
  - 1 byte:  record number
  - 1 byte:  dataset number
  - 2 bytes: datafield byte count
}
function TIPTCData.ExtractTag(const ABuffer: ansistring; var AStart: Integer): iTag;
var
  bLen, tagId, i: integer;
  tmp: iTag;
  recordNo, datasetNo: byte;
  w: Word;
begin
  InitITag(tmp);
  recordNo := byte(ABuffer[AStart]);
  datasetNo := byte(ABuffer[AStart+1]);
  bLen := (byte(ABuffer[AStart+2]) shl 8) or byte(ABuffer[AStart+3]);
  inc(AStart, 4);                     // skip length bytes
  if recordNo in [1, 2, 8] then
  begin
    tagID := recordNo shl 8 or datasetNo;
    tmp.Tag := 65534;
    for i := 0 to IptcTagCnt - 1 do
      if (IPTCTable[i].Tag = tagID) then
      begin
        if IPTCTable[i].name <> 'SKIP' then
        begin
          tmp := IPTCTable[i];
          tmp.Raw := Copy(ABuffer, AStart, blen);
          case tmp.TType of
            FMT_STRING, 0:
              tmp.Data := RawToData(tmp.Raw, tmp.TType);
            FMT_USHORT:
              begin
                w := PWord(@tmp.Raw[1])^;
                tmp.Data := IntToStr(BEToN(w));
              end;
            else
              raise Exception.Create('Tag type not supported.');
          end;
        end;
        break;
      end;

    if tmp.Tag = 65534 then
    begin
      tmp.Name := 'Custom_' + IntToStr(datasetNo);
      tmp.Desc := 'Custom_' + IntToStr(datasetNo);
      tmp.Tag := tagID;
      tmp.Raw := Copy(ABuffer, AStart, blen);
      case tmp.TType of
        FMT_STRING,
        0:
          begin
            tmp.Data := RawToData(tmp.Raw, tmp.TType);
            if Length(tmp.Raw) <= 64 then
              tmp.Size := 64   // length for unknown fields ?
            else
              tmp.Size := Length(tmp.Raw);
          end;
        FMT_USHORT:
          begin
            w := PWord(@tmp.Raw[1])^;
            tmp.Data := IntToStr(BEToN(w));
          end;
        else
          raise Exception.Create('Tag type not supported.');
      end;
    end;
  end;
  AStart := AStart + blen + 1;
  Result := tmp;
end;

//  This function returns the index of a tag name in the tag buffer.
function TIPTCdata.LookupTag(ATagName: String): Integer;
begin
  Result := LookupNextTag(ATagName, 0);
end;

function TIptcData.LookupNextTag(ATagName: String; ATagIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  if ATagIndex >= Count then
    exit;
  ATagName := Uppercase(ATagName);
  for i:= ATagIndex to Count-1 do
    if (UpperCase(iTagArray[i].Name) = ATagName) then
    begin
      Result := i;
      exit;
    end;
end;

//  This function returns the index of a tag name in the tag buffer.
//  It searches by the description which is most likely to be used as a label
function TIPTCdata.LookupTagByDesc(ATagDesc: string): integer;
var
  i: integer;
begin
  ATagDesc := UpperCase(ATagDesc);
  Result := -1;
  for i := 0 to Count-1 do
    if UpperCase(iTagArray[i].Desc) = ATagDesc then
    begin
      Result := i;
      break;
    end;
end;

procedure TIPTCData.ParseIPTCStrings(buff: ansistring; AList: TStrings);
var
  tmpItem: itag;
  start, i, j: Integer;
begin
  Assert(AList <> nil, 'TIPTCData.ParseIPTCStrings called with AList=nil');
  //FBuffer := buff;
  i := Pos('Photoshop 3.0', buff) + 13;
  for j := i to Length(buff) do       // Look for first field marker
    if (byte(buff[j]) = $1C) and (byte(buff[j+1]) in [1, 2, 8]) then
      break;
  start := j+1;
  while (start < Length(buff)-2) do   // Work through buffer
  begin
    tmpItem := ExtractTag(buff, start);
    if tmpItem.Name <> '' then         // Empty fields are masked out
      AList.Add(tmpItem.Desc + DexifDelim + tmpItem.Data);
  end;
end;
 
function TIPTCdata.AddTagToArray(ANewTag: iTag): Integer;
begin
  if ANewTag.Tag and $00FF <> 0 then     // Empty fields are masked out
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
 
Procedure TIPTCdata.ParseIPTCArray(ABuffer: Ansistring);
const
  RESOURCE_MARKER: ansistring = '8BIM';
  MARKER_SIZE = 4;  // = Length(RESOURCE_MARKER);
  IPTC_IMAGERESOURCEID = #04#04;
var
  nextTag: ITag;
  start, i, j, k: Integer;
  id: String[2];
  len: Byte;
  size: DWord;
  marker, nam: ansistring;
begin
  Reset;
  i := Pos('Photoshop 3.0', ABuffer);
  if i = 0 then
    exit;
  inc(i, 14);

  j := Length(ABuffer);
  while i <= Length(ABuffer) do begin
    marker := Copy(ABuffer, i, MARKER_SIZE);
    if marker <> RESOURCE_MARKER then
      break;
    j := i + MARKER_SIZE;
    id := Copy(ABuffer, j, 2);
    inc(j, 2);
    len := ord(ABuffer[j]);
    inc(j);
    if len = 0 then begin
      len := ord(ABuffer[j]);
      inc(j);
      nam := '';
    end else begin
      nam := Copy(ABuffer, j, len);
      inc(j, len);
    end;
    nam := copy(ABuffer, j, 4);
    inc(j, 4);
    size := PDWord(@nam[1])^;
    size := BEToN(size);

    if id = IPTC_IMAGERESOURCEID then begin
      // read IPTC tags
      if not ((byte(ABuffer[j]) = $1C) and (byte(ABuffer[j+1]) in [1, 2, 8])) then
        exit;
      start := j + 1;
      while (start < j + 1 + size) do begin
        nextTag := ExtractTag(ABuffer, start);  // start is incremented by function
        if nextTag.Count = MultiTagCount then   // MultiTagCount means: there can be multiple values
          AppendToTag(nextTag.Name, nextTag.Data)
        else
          AddTagToArray(nextTag);
      end;
      exit;
    end
    else
      inc(j, size);
    i := j;
  end;
end;
 
function MakeEntry(code,tag: integer; data:ansistring): ansistring;
var
  buff,sLen: ansistring;
  bLen:integer;
begin
  bLen := Length(Data);
  sLen := ansichar(blen div 256) + ansichar(blen mod 256);
  result := buff+ansichar($1C)+ansichar(code)+ansichar(tag)+sLen+Data;
end;

procedure TIptcData.IptcArrayToList(AList: TStrings);
var
  i: Integer;
  tag: ITag;
begin
  for i:= 0 to Count-1 do begin
    tag := ITagArray[i];
    if (tag.Count = MultiTagCount) and (tag.Data = '') then
      Continue;
    AList.Add(tag.Desc + dExifDelim + tag.Data);
  end;
  //buf := IptcArrayToBuffer;
//  ParseIptcStrings(buf, AList);
end;

procedure TIPTCData.IPTCArrayToXML(AList: TStrings);
var
  i: integer;
begin
  Assert(AList <> nil, 'TIPTCData.IPTCArrayToXML called with AList=nil.');
  AList.Add('   <ITPCdata>');
  for i := 0 to Count-1 do
    with ITagArray[i] do
    begin
      AList.Add('   <' + name + '>');
      if Tag in [105, 120] // headline and image caption
        then AList.Add('      <![CDATA[' + Data + ']]>')
        else AList.Add('      ' + Data);
      AList.Add('   </' + Name + '>');
    end;
  AList.Add('   </ITPCdata>');
end;
     (*
function SplitMultiTag(code, tag:integer; buff:ansistring):ansistring;
var
  tmps:ansistring;
  j:integer;
begin
  result := '';
  while trim(string(buff)) <> '' do
  begin
    j := Pos(MultiTagSep,buff);
    if j > 0 then
    begin
      tmps := AnsiString(trim(string(copy(buff,1,j-1))));
      buff := AnsiString(trim(string(copy(buff,j+1,maxint))));
    end else
    begin
      tmps := buff;
      buff := '';
    end;
    result := result+MakeEntry(code,tag,tmps);
  end;
end;

function TIPTCdata.IPTCArrayToBuffer: Ansistring;
var
  buff: ansistring;
  i: integer;
  recordNo, datasetNo: Byte;
begin
  buff := '';
  // load up the particular data
  for i := 0 to Count-1 do
    with ITagArray[i] do begin
      recordNo := Tag shr 8;
      datasetNo := Tag and $00FF;
      if (recordNo in [1, 2]) and (Count = MultiTagCount) then   // Multiple tag values
        buff := buff + SplitMultiTag(recordNo, datasetNo, data)
      else                                            // Single tag value
        buff := buff + MakeEntry(recordNo, datasetNo, data);
    end;
  Result := buff;

{
// Photoshop requires the following headers:
  if not odd(length(buff)) then
    buff := buff+#0;
  h2 := MakeEntry(2,0,#0#2);
  bLen := length(buff)+length(h2);
  sLen := ansichar(blen div 256)+ansichar(blen mod 256);
  buff := 'Photoshop 3.0'#0'8BIM'#4#4#0#0#0#0+slen+h2+buff;
 
// Photoshop requires the following End-of-data marker:
  result := buff+'8BIM'#$04#$0B#0#0#0#0#0#0;
}
end;
 *)
function TIPTCdata.Clone(ASource: TIPTCdata): TIPTCdata;
begin
  Result := TIPTCdata.Create(FParent);
  Result.fITagArray := Copy(ASource.fITagArray, 0, MaxTag);
  Result.fITagCount := ASource.fITagCount;
end;

(*
procedure TOPTCdata.MakeIPTCSegment(buff: ansisstring);
var
  blen: integer;
begin
  bl := length(buff) + 2;
  if IPTCSegment = nil then
  begin
    inc(SectionCnt);
    IPTCSegment := @(sections[SectionCnt]);
  end;
  IPTCSegment^.Data := ansichar(bl div 256) + ansichar(bl mod 256) + buff;
  IPTCSegment^.Size := bl;
  IPTCSegment^.DType := M_IPTC;
end;
  *)
function TIPTCdata.AddOrAppend(ATagName: String; ARawVal: ansistring): integer;
var
  idx: integer;
begin
  Result := -1;
  idx := LookupTagDef(ATagName);  // see if keyword is valid
  if idx >= 0 then
  begin
    if (IPTCTable[idx].Count = MultiTagCount) then  // Multiple tag values
      Result := AppendToTag(ATagName, ARawVal)
    else
      Result := AddTag(ATagName, ARawVal);
  end;
end;

{ exstr and newstr are comma-separated lists of strings.
  Adds the strings contained in newstr to exstr (ex = "existing") such that
  exstr does not contain duplicates. }
function noDups(exst, newstr: String): String;
var
  lst,nlst: TStringList;
  i: integer;
  s: String;
begin
  lst := TStringlist.Create;
  nlst := TStringlist.Create;
  try
    lst.CommaText := exst;
    lst.CaseSensitive := false;
    nlst.CommaText := newstr;
    for i := 0 to nlst.Count-1 do
    begin
      s := trim(nlst[i]);
      if (lst.IndexOf(s) = -1) then
        lst.Add(s);
    end;
    Result := lst.CommaText;
  finally
    nlst.Free;
    lst.Free;
  end;
end;

function TIPTCdata.AppendToTag(ATagName: String; ARawVal:ansistring): integer;
var
  idx: integer;
  dataVal: String;
begin
  idx := LookupTag(ATagName);
  if (idx >= 0) then
  begin
    if ARawVal <> '' then begin
      dataVal := RawToData(ARawVal, fITagArray[idx].TType);
      fITagArray[idx].Data := NoDups(fITagArray[idx].Data, dataVal);
      AddTag(ATagname, ARawVal, true);
    end;
  end else
    idx := AddTag(ATagName, ARawVal);
  result := idx;
end;

function TIPTCdata.UpdateTagDesc(ATagName: String; AValue: String): integer;
var
  idx: integer;
begin
  idx := LookupTag(ATagName);
  if (idx >= 0) then
  begin
    if AValue <> '' then
      fITagArray[idx].Desc := AValue;
  end;
  result := idx;
end;

function TIptcData.GetMultiPartTag(ATagName: String): TStringList;
begin
  Result := TStringlist.Create;
  Result.CommaText := StringReplace(GetTag(ATagname), MultiTagSep, ',' ,[rfReplaceAll]);
end;

function TIptcData.RawToData(ARaw: ansistring; ATagType: Byte): String;
var
  w: Word;
begin
  case ATagType of
    FMT_STRING, 0:
      begin
        Result := ARaw;   // to do: respect encoding
        if Result[Length(Result)] = #0 then
          Delete(Result, Length(Result), 1);
      end;
    FMT_USHORT:
      begin
        w := BEtoN(PWord(@ARaw[1])^);
        Result := IntToStr(w);
      end;
    else
      raise Exception.Create('Tag type not supported.');
  end;
end;

function TIPTCdata.AddTag(ATagName: String; ARawVal: ansistring = '';
  AForceNew: Boolean = false): integer;
var
  idx, defIdx: integer;
  newTag: itag;
begin
  idx := LookupTag(ATagName);
  if AForceNew or (idx = -1) then
  begin
    defIdx := LookupTagDef(ATagName);
    if defIdx < 0 then
    begin
      Result := -1;
      exit;  // not a defined node, do not insert
    end;
    newTag := IPTCTable[defIdx];
    newTag.Raw := ARawVal;
    newTag.Data := RawToData(ARawVal, newTag.TType);
    idx := AddTagToArray(newTag);
  end else
  begin
    if ARawVal <> '' then begin
      fITagArray[idx].Raw := ARawVal;
      fITagArray[idx].Data := RawToData(ARawVal, fITagArray[idx].TType);
    end;
  end;
  Result := idx;
end;

procedure TIPTCdata.RemoveTag(ATagName: String);
var
  idx, i: integer;
begin
  idx := LookupTag(ATagName);
  if (idx >= 0) then
  begin
    for i := idx to fITagCount - 2 do
      fITagArray[i] := fITagArray[i+1];
    dec(fITagCount);
  end;
end;
 
procedure TIPTCdata.Reset;
begin
  Count := 0;
  SetLength(fITagArray, 0);
end;

function TIPTCdata.GetTag(ATagName: string; ADefaultVal: String=''): String;
var
  idx: integer;
begin
  idx := LookupTag(ATagName);
  if idx >= 0 then
    Result := ITagArray[idx].Data
  else
    Result := ADefaultVal;
end;

Function TIPTCdata.HasData: boolean;
begin
  result := Count > 0;
end;

class procedure TIPTCData.ReadFileStrings(const AFileName: String;
  AList: TStrings);
var
  imgdata: TImgData;
begin
  Assert(AList <> nil, 'TIPTCData.ReadFileStrings called with AList=nil.');
  imgData := TImgData.Create;
  try
    imgData.ReadIptcStrings(AFileName, AList);
  finally
    imgData.Free;
  end;
end;

procedure TIPTCdata.SetTagByIdx(idx: integer; ADataVal: String);
var
  w: Integer;
  sa: ansistring;
begin
  fITagArray[idx].Data := ADataVal;
  case FITagArray[idx].TType of
    FMT_USHORT:
      begin
        w := StrToInt(ADataVal);
        SetLength(fITagArray[idx].Raw, 2);
        Move(w, fITagArray[idx].Raw[1], 2);
      end;
    FMT_STRING,
    0:
      begin
        sa := ADataVal;
        fITagArray[idx].Raw := sa;   // To do: Fix encoding
      end;
  end;
end;

{$IFDEF MSWINDOWS}
function GetTimeZoneBias: Longint;
var
  TZoneInfo: TTimeZoneInformation;
begin
  GetTimeZoneInformation(TZoneInfo);
  result := TZoneInfo.Bias;
end;
{$ENDIF}

{$IFDEF UNIX}
function GetTimeZoneBias: Longint;
begin
  Result := -TZSeconds div 60;
end;
{$ENDIF}

function TIPTCdata.GetTimeZoneStr: String;
var
  tmp,h,m: integer;
  sign: ansistring;
begin
  result := DefaultTimeZone;
  if DefaultTimeZone <> '_0000' then
    exit;
  tmp := GetTimeZoneBias();
  h := abs(tmp) div 60; // hours
  m := abs(tmp) mod 60; // minutes
  if tmp < 0         // local time correction: invertsign
    then sign := '+'
    else sign := '-';
  result := Format('%s%.2d%.2d',[sign,h,m]);
end;

procedure TIPTCdata.SetDateTimePrim(TimeIn:TDateTime; prefix:string);
var
  dateStr, timeStr, timeZone:ansistring;
begin
  if AnsiString(AnsiLowerCase(prefix)) = 'default' then
  begin
    datestr := 'DateCreated';
    timestr := 'TimeCreated';
  end
  else
  begin
    datestr := prefix+'Date';
    timestr := prefix+'Time';
  end;
  timeZone := getTimeZoneStr();  // use local time zone
  AddTag(datestr, AnsiString(FormatDateTime('yyyymmdd',TimeIn)));
  AddTag(timestr, AnsiString(FormatDateTime('hhnnss',TimeIn))+timeZone);
end;

procedure TIPTCdata.SetDateTime(TimeIn:TDateTime);
begin
  SetDateTimePrim(TimeIn, 'Default');
end;

procedure TIPTCdata.SetDateTimeExt(TimeIn:TDateTime; APrefix:ansistring);
begin
  SetDateTimePrim(TimeIn, APrefix);
end;

function TIPTCdata.GetDateTime: TDateTime;
type
  TConvert= packed record
     year: Array[1..4] of ansichar;
     mon, day, hr, min, sec: array[1..2] of ansichar;
  end;
  PConvert= ^TConvert;
var
  str: ansistring;
  yr, mn, dy, h, m, s: Integer;
  d: TDateTime;
  t: TDateTime;
begin
  Result := 0;
  str := GetTag('DateCreated', '00000000') + GetTag('TimeCreated', '000000');
  if Length(str) >= SizeOf(TConvert) then
    with PConvert(@str[1])^ do
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

procedure IPTCWriteTransFile(const AFileName: String);
var
  L: TStringList;
  i: integer;
begin
  L := TStringList.Create;
  try
    for i := 0 to IPTCTAGCNT-1 do
      L.Add(IPTCTable[i].Name + '=' + IPTCTable[i].Desc);
    L.SaveToFile(AFileName);
  finally
    L.Free;
  end;
end;

function IPTCReadTransFile(const AFileName: String): boolean;
var
  L: TStringList;
  i: integer;
  s: string;
begin
  Result := false;
  if not FileExists(AFilename) then
    exit;

  L := TStringlist.Create;
  try
    L.LoadFromFile(AFileName);
    for i := 0 to IPTCTAGCNT-1 do
    begin
      s := L.Values[IPTCTable[i].Name];
      if s > '' then
        IPTCTable[i].Desc := s;
    end;
  finally
    L.Free;
  end;
end;

end.
