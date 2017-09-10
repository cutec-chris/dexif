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
    function GetTimeZoneStr: string;
    procedure SetDateTimePrim(TimeIn: TDateTime; prefix: string);
  protected
    FBuffer: ansistring;
    MaxTag: integer;
    FParent: TObject;
    fITagCount : integer;
    fITagArray: array of iTag;
    function ExtractTag(const ABuffer: ansistring; var AStart: integer): iTag;
  public
    constructor Create(AParent: TObject);
    procedure Reset;
    function HasData: boolean;
    function Clone(ASource: TIPTCData): TIPTCData;

    function IPTCArrayToBuffer:ansistring;
    procedure IPTCArrayToList(AList: TStrings);
    procedure IPTCArrayToXML(AList: TStrings); overload;
    function IPTCArrayToXML: TStringList; overload;
      deprecated {$IFDEF FPC}'Use procedure instead.'{$ENDIF};

    procedure ParseIPTCStrings(buff: ansistring; AList: TStrings); overload;
    function ParseIPTCStrings(buff: ansistring): TStringlist; overload;
      deprecated {$IFDEF FPC}'Use procedure instead.'{$ENDIF};
    procedure ParseIPTCArray(ABuffer: ansistring);

    function LookupTag(SearchStr: String): integer; virtual;
    Function LookupTagDefn(AName: String): integer;
    function LookupTagByDesc(SearchStr: String): integer;

    procedure RemoveTag(ATagName: String); virtual;
    function AddTag(ATagName: String; ADataVal: ansistring = ''): integer; virtual;
    function AppendToTag(ATagName: String; ADataVal: ansistring): integer; virtual;
    function AddOrAppend(ATagName: String; ADataVal: ansistring): integer; virtual;
    function UpdateTag(ATagName: String; ADataVal: ansistring): integer;
    procedure SetTagByIdx(idx:integer; AValue:ansistring);
    function GetTag(ATagName: String; ADefaultVal: string=''): string; virtual;

    class procedure ReadFileStrings(const AFilename: String; AList: TStrings);

    function AddTagToArray(ANewTag: iTag): integer;
    function GetDateTime: TDateTime;
    procedure SetDateTime(TimeIn: TDateTime);
    procedure SetDateTimeExt(TimeIn: TDateTime; APrefix:ansistring);
    function GetMultiPartTag(ATagName: String): TStringList;

    property ITagArray[TagID:integer]: ITag
        read GetTagElement write SetTagElement; default;
    property Count: integer read GetCount write SetCount;
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
    ( TID:0; TType:3; Tag:$0200 {2: 0}; Count:1;     Name:'SKIP';             Desc:'Record Version';Code:'';Data:'';Raw:'';FormatS:'';Size:64),
    ( TID:0; TType:2; Tag:$0203 {2: 3}; Count:1;     Name:'ObjectType';       Desc:'Object Type Ref';Code:'';Data:'';Raw:'';FormatS:'';Size:67),
    ( TID:0; TType:2; Tag:$0204 {2: 4}; Count:$FFFF; Name:'ObjectAttr';       Desc:'Object Attribute Ref';  Code:'';Data:'';Raw:'';FormatS:'';Size:67),
    ( TID:0; TType:2; Tag:$0205 {2: 5}; Count:1;     Name:'ObjectName';       Desc:'Object name';  Code:'';Data:'';Raw:'';FormatS:'';Size:64),
    ( TID:0; TType:2; Tag:$0207 {2: 7}; Count:1;     Name:'EditStatus';       Desc:'Edit Status';  Code:'';Data:'';Raw:'';FormatS:'';Size:64),
    ( TID:0; TType:2; Tag:$0208 {2: 8}; Count:1;     Name:'EditorialUpdate';  Desc:'Editorial Update';  Code:'';Data:'';Raw:'';FormatS:'';Size:2),
    ( TID:0; TType:2; Tag:$020A {2:10}; Count:1;     Name:'Urgency';          Desc:'Urgency';      Code:'';Data:'';Raw:'';FormatS:'';Size:1),
    ( TID:0; TType:2; Tag:$020C {2:12}; Count:$FFFF; Name:'SubRef';           Desc:'Subject Reference';     Code:'';Data:'';Raw:'';FormatS:'';Size:236),
    ( TID:0; TType:2; Tag:$020F {2:15}; Count:1;     Name:'Category';         Desc:'Category';     Code:'';Data:'';Raw:'';FormatS:'';Size:3),
    ( TID:0; TType:2; Tag:$0214 {2:20}; Count:$FFFF; Name:'SuppCategory';     Desc:'Supplemental category'; Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0216 {2:22}; Count:1;     Name:'FixtureID';        Desc:'Fixture ID';   Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0219 {2:25}; Count:$FFFF; Name:'Keywords';         Desc:'Keywords';     Code:'';Data:'';Raw:'';FormatS:'';Size:64),
    ( TID:0; TType:2; Tag:$021A {2:26}; Count:$FFFF; Name:'ContentLocCode';   Desc:'Content Location Code'; Code:'';Data:'';Raw:'';FormatS:'';Size: 3),
    ( TID:0; TType:2; Tag:$021B {2:27}; Count:$FFFF; Name:'ContentLocName';   Desc:'Content Location Name'; Code:'';Data:'';Raw:'';FormatS:'';Size: 64),
    ( TID:0; TType:2; Tag:$021E {2:30}; Count:1;     Name:'ReleaseDate';      Desc:'Release Date'; Code:'';Data:'';Raw:'';FormatS:'';Size:8),
    ( TID:0; TType:2; Tag:$0223 {2:35}; Count:1;     Name:'ReleaseTime';      Desc:'Release Time'; Code:'';Data:'';Raw:'';FormatS:'';Size:11),
    ( TID:0; TType:2; Tag:$0225 {2:37}; Count:1;     Name:'ExpireDate';       Desc:'Expiration Date'; Code:'';Data:'';Raw:'';FormatS:'';Size:8),
    ( TID:0; TType:2; Tag:$0226 {2:38}; Count:1;     Name:'ExpireTime';       Desc:'Expiration Time'; Code:'';Data:'';Raw:'';FormatS:'';Size:11),
    ( TID:0; TType:2; Tag:$0228 {2:40}; Count:1;     Name:'SpecialInstru';    Desc:'Special Instructions'; Code:'';Data:'';Raw:'';FormatS:'';Size:256),
    ( TID:0; TType:2; Tag:$022A {2:42}; Count:1;     Name:'ActionAdvised';    Desc:'Action Advised'; Code:'';Data:'';Raw:'';FormatS:'';Size:2),
    ( TID:0; TType:2; Tag:$022D {2:45}; Count:$FFFF; Name:'RefService';       Desc:'Reference Service'; Code:'';Data:'';Raw:'';FormatS:'';Size:10),
    ( TID:0; TType:2; Tag:$022F {2:47}; Count:$FFFF; Name:'RefDate';          Desc:'Reference Date'; Code:'';Data:'';Raw:'';FormatS:'';Size:8),
    ( TID:0; TType:2; Tag:$0232 {2:50}; Count:$FFFF; Name:'RefNumber';        Desc:'Reference Number'; Code:'';Data:'';Raw:'';FormatS:'';Size:8),
    ( TID:0; TType:2; Tag:$0237 {2:55}; Count:1;     Name:'DateCreated';      Desc:'Date created'; Code:'';Data:'';Raw:'';FormatS:'';Size:8),
    ( TID:0; TType:2; Tag:$023C {2:60}; Count:1;     Name:'TimeCreated';      Desc:'Time created'; Code:'';Data:'';Raw:'';FormatS:'';Size:11),
    ( TID:0; TType:2; Tag:$023E {2:62}; Count:1;     Name:'DigitizeDate';     Desc:'Digital Creation Date'; Code:'';Data:'';Raw:'';FormatS:'';Size:8),
    ( TID:0; TType:2; Tag:$023F {2:63}; Count:1;     Name:'DigitizeTime';     Desc:'Digital Creation Time'; Code:'';Data:'';Raw:'';FormatS:'';Size:11),
    ( TID:0; TType:2; Tag:$0241 {2:65}; Count:1;     Name:'OriginatingProgram'; Desc:'Originating Program'; Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0246 {2:70}; Count:1;     Name:'ProgramVersion';   Desc:'Program version'; Code:'';Data:'';Raw:'';FormatS:'';Size: 10),
    ( TID:0; TType:2; Tag:$024B {2:75}; Count:1;     Name:'ObjectCycle';      Desc:'Object Cycle'; Code:'';Data:'';Raw:'';FormatS:'';Size:1),
    ( TID:0; TType:2; Tag:$0250 {2:80}; Count:$FFFF; Name:'ByLine';           Desc:'ByLine';       Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0255 {2:85}; Count:$FFFF; Name:'ByLineTitle';      Desc:'ByLine Title'; Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$025A {2:90}; Count:1;     Name:'City';             Desc:'City';         Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$025C {2:92}; Count:1;     Name:'SubLocation';      Desc:'Sublocation';  Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$025F {2:95}; Count:1;     Name:'State';            Desc:'Province/State';  Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0264 {2:100};Count:1;     Name:'LocationCode';     Desc:'Country/Primary Location Code'; Code:'';Data:'';Raw:'';FormatS:'';Size:3),
    ( TID:0; TType:2; Tag:$0265 {2:101};Count:1;     Name:'LocationName';     Desc:'Country/Primary Location Name'; Code:'';Data:'';Raw:'';FormatS:'';Size:64),
    ( TID:0; TType:2; Tag:$0267 {2:103};Count:1;     Name:'TransmissionRef';  Desc:'Original Transmission Reference';     Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0269 {2:105};Count:1;     Name:'ImageHeadline';    Desc:'Image headline'; Code:'';Data:'';Raw:'';FormatS:'';Size:256),
    ( TID:0; TType:2; Tag:$026E {2:110};Count:1;     Name:'ImageCredit';      Desc:'Image credit';  Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0273 {2:115};Count:1;     Name:'Source';           Desc:'Source';        Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0274 {2:116};Count:1;     Name:'Copyright';        Desc:'Copyright Notice';  Code:'';Data:'';Raw:'';FormatS:'';Size:128),
    ( TID:0; TType:2; Tag:$0276 {2:118};Count:$FFFF; Name:'Contact';          Desc:'Contact';       Code:'';Data:'';Raw:'';FormatS:'';Size:128),
    ( TID:0; TType:2; Tag:$0278 {2:120};Count:1;     Name:'ImageCaption';     Desc:'Image caption'; Code:'';Data:'';Raw:'';FormatS:'';Size:2000),
    ( TID:0; TType:2; Tag:$027A {2:122};Count:1;     Name:'ImageCaptionWriter'; Desc:'Image caption writer'; Code:'';Data:'';Raw:'';FormatS:'';Size:32),
    ( TID:0; TType:2; Tag:$0282 {2:130};Count:1;     Name:'ImageType';        Desc:'Image type';    Code:'';Data:'';Raw:'';FormatS:'';Size:2),
    ( TID:0; TType:2; Tag:$0283 {2:131};Count:1;     Name:'Orientation';      Desc:'Image Orientation'; Code:'';Data:'';Raw:'';FormatS:''; Size:1),
    ( TID:0; TType:2; Tag:$0287 {2:135};Count:1;     Name:'LangID';           Desc:'Language ID';   Code:'';Data:'';Raw:'';FormatS:'';Size:3),
    ( TID:0; TType:0; Tag:$080A {8:10}; Count:$FFFF; Name:'Subfile';          Desc:'Subfile';       Code:'';Data:'';Raw:'';FormatS:'';Size:2)
   );

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

{ Note: recordNo : datasetNo }
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
          case tmp.TType of
            2: begin
                 tmp.Raw := copy(ABuffer, AStart, blen);
                 tmp.Data := copy(ABuffer, AStart, blen);
               end;
            3: begin
                 w := PWord(@ABuffer[AStart])^;
                 Move(w, tmp.Raw[1], 2);
                 tmp.Data := IntToStr(BEToN(w));
               end;
          end;
        end;
        break;
      end;

    if tmp.Tag = 65534 then
    begin
      tmp.Name := 'Custom_' + IntToStr(datasetNo);
      tmp.Desc := 'Custom_' + IntToStr(datasetNo);
      tmp.Tag := tagID;
      case tmp.TType of
        2: begin
             tmp.Data := copy(ABuffer, AStart, blen);
             tmp.Raw := copy(ABuffer, AStart, blen);
             tmp.Size := 64; // length for unknown fields ?
           end;
        3: begin
             w := PWord(@ABuffer[AStart])^;
             Move(w, tmp.Raw[1], 2);
             tmp.Data := IntToStr(BEToN(w));
             tmp.Size := 2;
           end;
      end;
    end;
  end;
  AStart := AStart + blen + 1;
  Result := tmp;
end;

//  This function returns the index of a tag name in the tag buffer.
function TIPTCdata.LookupTag(SearchStr: String): Integer;
var
  i: integer;
begin
  SearchStr := UpperCase(SearchStr);
  Result := -1;
  for i := 0 to Count-1 do
    if UpperCase(iTagArray[i].Name) = SearchStr then
    begin
      Result := i;
      break;
    end;
end;

//  This function returns the index of a tag name in the tag buffer.
//  It searches by the description which is most likely to be used as a label
function TIPTCdata.LookupTagByDesc(SearchStr:string): integer;
var
  i: integer;
begin
  SearchStr := UpperCase(SearchStr);
  Result := -1;
  for i := 0 to Count-1 do
    if UpperCase(iTagArray[i].Desc) = SearchStr then
    begin
      Result := i;
      break;
    end;
end;

//  This function returns the index of a tag definition for a given tag name.
function TIPTCdata.LookupTagDefn(AName: string): integer;
var
  i:integer;
begin
  Result := -1;
  for i := 0 to IPTCTAGCNT-1 do
  begin
    if LowerCase(AName) = LowerCase(IPTCtable[i].Name) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TIPTCdata.ParseIPTCStrings(buff: ansistring): TStringList;
begin
  Result := TStringList.Create;
  ParseIPTCStrings(buff, Result);
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
var
  nextTag: ITag;
  start, i, j: Integer;
begin
  Reset;
  i := Pos('Photoshop 3.0', ABuffer) + 13;
  for j := i to Length(ABuffer) do       // Look for first field marker
    if (byte(ABuffer[j]) = $1C) and (byte(ABuffer[j+1]) in [1, 2, 8]) then
      break;

  start := j+1;
  while (start < Length(ABuffer)-2) do   // Work through buffer
  begin
    nextTag := ExtractTag(ABuffer, start);   // Start is incremented by function
//    if nextTag.Tag in IPTCMultiTags then
    if nextTag.Count = MultiTagCount then    // MultiTagCount means: there can be multiple values
    begin
      AppendToTag(nextTag.Name, nextTag.Data)
    end
    else
      AddTagToArray(nextTag);
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
  buf: ansistring;
begin
  buf := IptcArrayToBuffer;
  ParseIptcStrings(buf, AList);
end;

function TIPTCdata.IPTCArrayToXML: TStringList;
begin
  Result := TStringList.Create;
  IPTCArrayToXML(Result);
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

                         (*
// Photoshop requires the following headers:
  if not odd(length(buff)) then
    buff := buff+#0;
  h2 := MakeEntry(2,0,#0#2);
  bLen := length(buff)+length(h2);
  sLen := ansichar(blen div 256)+ansichar(blen mod 256);
  buff := 'Photoshop 3.0'#0'8BIM'#4#4#0#0#0#0+slen+h2+buff;
 
// Photoshop requires the following End-of-data marker:
  result := buff+'8BIM'#$04#$0B#0#0#0#0#0#0;
  *)
end;

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
function TIPTCdata.AddOrAppend(ATagName: String; ADataVal: ansistring): integer;
var
  i:integer;
begin
  Result := -1;
  i := LookupTagDefn(ATagName);  // see if keyword is valid
  if i >= 0 then
  begin
    if (IPTCTable[i].Count = MultiTagCount) then  // Multiple tag values
      Result := AppendToTag(ATagName, ADataVal)
    else
      Result := AddTag(ATagName, ADataVal);
  end;
end;

function noDups(exst, newstr: Ansistring): Ansistring;
var
  lst,nlst: TStringList;
  s:ansistring;
  i:integer;
begin
  lst := TStringlist.Create;
  nlst := TStringlist.Create;
  try
    lst.CommaText := exst;
    lst.CaseSensitive := false;
    nlst.CommaText := newstr;
    for i := 0 to nlst.Count-1 do
    begin
      s := AnsiString(trim(string(nlst[i])));
      if (lst.IndexOf(s) < 0) then
      begin
        lst.Add(s);
      end;
    end;
    result := AnsiString(lst.CommaText);
  finally
    nlst.Free;
    lst.Free;
  end;
end;

function TIPTCdata.AppendToTag(ATagName: String; ADataVal:ansistring): integer;
var
  insPt: integer;   // INSertion PoinT  (array index)
begin
  insPt := LookupTag(ATagName);
  if (insPt >= 0) then
  begin
    if ADataval <> '' then
      fITagArray[insPt].Data := NoDups(fITagArray[inspt].Data, ADataVal)
  end
  else
    insPt := AddTag(ATagName, NoDups('', ADataVal));
  result := insPt;
end;

function TIPTCdata.UpdateTag(ATagName: String; ADataVal: ansistring): integer;
var
  insPt: integer;   // INSertion PoinT
begin
  insPt := LookupTag(ATagName);
  if (insPt >= 0) then
  begin
    if ADataVal <> '' then
      fITagArray[insPt].Desc := ADataVal
  end;
  result := insPt;
end;

function TIptcData.GetMultiPartTag(ATagName: String): TStringList;
begin
  Result := TStringlist.Create;
  Result.CommaText := StringReplace(GetTag(ATagname), MultiTagSep, ',' ,[rfReplaceAll]);
end;
 
function TIPTCdata.AddTag(ATagName: String; ADataVal: ansistring): integer;
var
  insPt, defIdx: integer;
  newTag: itag;
begin
  insPt := LookupTag(ATagName);
  if (inspt >= 0) then
  begin
    if ADataval <> '' then
      fITagArray[inspt].Data := ADataval
  end else
  begin
    defIdx := LookupTagDefn(ATagname);
    if defIdx < 0 then
    begin
      Result := -1;
      exit;  // not a defined node, do not insert
    end;
    newTag := IPTCTable[defIdx];
    newTag.Raw := ADataVal;
    newTag.Data := ADataVal;
    insPt := AddTagToArray(newTag);
  end;
  Result := insPt;
end;

procedure TIPTCdata.RemoveTag(ATagName: String);
var
  remPt,i: integer;
begin
  remPt := LookupTag(ATagName);
  if (remPt >= 0) then
  begin
    for i := remPt to fITagCount - 2 do
      fITagArray[i] := fITagArray[i+1];
    dec(fITagCount);
  end;
end;
 
procedure TIPTCdata.Reset;
var
  i: Integer;
begin
  Count := 0;
  // clear out old data
  for i:=0 to High(fITagArray) do
    InitITag(fITagArray[i]);
end;

function TIPTCdata.GetTag(ATagName: string; ADefaultVal: String=''): String;
var
  i: integer;
begin
  Result := ADefaultVal;
  i := LookupTag(ATagName);
  if i >= 0 then
    Result := ITagArray[i].Data;
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

procedure TIPTCdata.SetTagByIdx(idx: integer; AValue: ansistring);
begin
  fITagArray[idx].Data := AValue;
  fITagArray[idx].Raw := AValue;
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
