unit rwMain;

{$I ..\..\..\dExif.inc}

interface

uses
 {$IFDEF FPC}
  LazUtf8,
 {$ELSE}
  Windows, Messages, jpeg,
 {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, Variants,
  dGlobal, dExif, ImgList;

type

  { TMainForm }

  TMainForm = class(TForm)
    EdTestfile: TEdit;
    BtnTest1: TSpeedButton;
    BtnTest2: TSpeedButton;
    ImageList1: TImageList;
    Label1: TLabel;
    ListView: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    ExifListView: TListView;
    ExifTabControl: TTabControl;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnTest1Click(Sender: TObject);
    procedure ExifTabControlChange(Sender: TObject);
  private
    ImgData: TImgData;
    OutFile: String;
    procedure ExecTest(const AParamsFile: String);
    procedure ExifToListview(AImgData: TImgData; AListView: TListView);
    function GetTagType(ATagName: String): Integer;
    function ReadTagValue(ATagName: String): String;
    function Success(ACurrValue, AExpectedValue: String): Boolean;
    procedure WriteTagValue(ATagName, ATagValue: String);

  public

  end;

var
  MainForm: TMainForm;

implementation

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
  StrUtils, Math, dUtils;

const
  IMGINDEX_SUCCESS = 0;
  IMGINDEX_FAIL = 1;

  TESTCASES_DIR = '..\common\';

type
  TStringArray = array of string;

function Split(s: String; Separator: Char = #9): TStringArray;
const
  BLOCK_SIZE = 20;
var
  p, p1: PChar;
  i, j, n, L: Integer;
  part: String;
begin
  if s = '' then begin
    SetLength(Result, 0);
    exit;
  end;
  L := Length(s);
  SetLength(Result, BLOCK_SIZE);
  i := 1;
  j := 1;
  n := 0;
  while (i <= L) do begin
    if (s[i] = Separator) or (i = L)  then begin
      if i=L then inc(i);
      Result[n] := Copy(s, j, i-j);
      inc(n);
      if n mod BLOCK_SIZE = 0 then
        SetLength(Result, Length(Result) + BLOCK_SIZE);
      j := i+1;
    end;
    inc(i);
  end;
  SetLength(Result, n);
end;

{ The date/time string is expected in the ISO format "yyyy-mm-dd hh:nn:ss" }
function ExtractDateTime(AValue: String): TDateTime;
var
  p: Integer;
  yr, mn, dy, h, m, s: Integer;
begin
  Result := 0;
  p := pos('-', AValue);
  if p = 0 then
    raise Exception.Create('ISO date/time format expected: "yyyy-mm-dd hh:nn:ss"');
  yr := StrToInt(copy(AValue, 1, p-1));
  Delete(AValue, 1, p);
  p := pos('-', AValue);
  if p = 0 then
    raise Exception.Create('ISO date/time format expected: "yyyy-mm-dd hh:nn:ss"');
  mn := StrToInt(copy(AValue, 1, p-1));
  Delete(AValue, 1, p);
  p := pos(' ', AValue);
  if p = 0 then begin
    dy := StrToInt(AValue);
    Result := EncodeDate(yr, mn, dy);
    exit;
  end;
  dy := StrToInt(copy(AValue, 1, p-1));
  Delete(AValue, 1, p);
  p := pos(':', AValue);
  if p = 0 then
    raise Exception.Create('ISO date/time format expected: "yyyy-mm-dd hh:nn:ss"');
  h := StrToInt(copy(AValue, 1, p-1));
  Delete(AValue, 1, p);
  p := pos(':', AValue);
  if p = 0 then begin
    m := StrToInt(AValue);
    s := 0;
  end else begin
    m := StrToInt(copy(AValue, 1, p-1));
    s := StrToInt(copy(AValue, p+1, MaxInt));
  end;
  Result := EncodeDate(yr, mn, dy) + EncodeTime(h, m, s, 0);
end;


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ImgData := TImgData.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ImgData.Free;
end;

procedure TMainForm.BtnTest1Click(Sender: TObject);
begin
  if Sender = BtnTest1 then
    ExecTest(TESTCASES_DIR + 'testcases1.txt')
  else if Sender = BtnTest2 then
    ExecTest(TESTCASES_DIR + 'testcases2.txt')
  else
    raise Exception.Create('BtnTextClick: Unexpected Sender');
end;

procedure TMainForm.ExecTest(const AParamsFile: String);
var
  testCases: TStringList;
  i, j, n: Integer;
  s: String;
  testdata: TStringArray;
  listitem: TListItem;
  tagName: String;
  currTagValue: String;
  newTagValue: String;
  altTagValue: String;
  {$IFDEF FPC}
  stream: TMemorystream;
  {$ELSE}
  jpeg: TJpegImage;
  stream: TMemoryStream;
  a: ansistring;
  {$ENDIF}
begin
  Listview.Items.Clear;

  if not FileExists(AParamsFile) then begin
    showMessage('Parameter file "' + AParamsFile + '" not found.');
    exit;
  end;
  if not FileExists(EdTestFile.Text) then begin
    ShowMessage('Test picture file "' + EdTestFile.Text + '" not found.');
    exit;
  end;

  // Read test parameters
  testCases := TStringList.Create;
  try
  {$IFDEF FPC}
    // The testcases text files are encoded in ANSI for Delphi7 compatibility
    // In Lazarus we must convert to UTF8 }
    testCases.LoadFromFile(AParamsFile);
    s := testCases.Text;
   {$IFDEF FPC3+}
    testCases.Text := WinCPToUTF8(s);
   {$ELSE}
    testCases.Text := AnsiToUTF8(s);
   {$ENDIF}
  {$ELSE}
    stream := TMemoryStream.Create;
    stream.LoadFromFile(AParamsFile);
    SetLength(a, stream.Size);
    stream.Read(a[1], Length(a));
    testcases.Text := a;
    (*
   {$IFDEF DELPHI_UNICODE}
    testcases.LoadFromFile(AParamsFile, TEncoding.ANSI);
   {$ELSE}
    testcases.LoadfromFile(AParamsFile);
   {$ENDIF}
   *)
  {$ENDIF}

    // Read EXIF tags from image file
    ImgData.ProcessFile(EdTestFile.Text);

    OutFile := 'test-image.jpg';   // File name of the modified test image

    ListView.Items.BeginUpdate;
    try
      j := 0;
      n := testCases.Count;
      for i:=0 to n-1 do begin
        if (TestCases[i] = ':quit') then
          break;

        if (testCases[i] = '') or (testCases[i][1] = ';') then
          Continue;

        // Extract test parameters
        testdata := Split(TestCases[i]);
        tagName := testdata[0];
        newTagValue := testdata[1];

        // Add test to listview
        listitem := ListView.Items.Add;
        listItem.Caption := testdata[0];

        // Read current tag value
        currTagValue := ReadTagValue(tagName);
        listItem.SubItems.Add(currTagValue);

        // Write new tag value into ExifObj
        WriteTagValue(tagName, newTagValue);
        listItem.SubItems.Add(newTagValue);
      end;
    finally
      ListView.Items.EndUpdate;
    end;

    // Write new tags to file
    ImgData.WriteEXIFJpegTo(OutFile);

    // read back
    ImgData.ProcessFile(OutFile);
    j := 0;
    for i:=0 to testCases.Count-1 do begin
      if (testcases[i] = ':quit') then
        break;
      if (testcases[i] = '') or (testcases[i][1] = ';') then
        Continue;
      testdata := Split(testCases[i]);
      tagname := testdata[0];
      newTagValue := testdata[1];
      currTagValue := ReadTagValue(tagname);
      listItem := ListView.Items[j];
      listItem.SubItems.Add(currTagValue);
      if Success(currTagValue, newTagValue) then
        listItem.ImageIndex := IMGINDEX_SUCCESS else
        listItem.ImageIndex := IMGINDEX_FAIL;
      inc(j);
    end;
  finally
    testCases.Free;
  end;

  ExifTabControlChange(nil);
end;

function TMainForm.Success(ACurrValue, AExpectedValue: String): Boolean;
const
  relEPS = 1E-3;
var
  p: Integer;
  expected1, expected2: String;
  valexp, valcurr: Double;
begin
  Result := ACurrValue = AExpectedValue;
  if Result then
    exit;

  { Check for alternative expected value }
  p := pos('|', AExpectedValue);
  if p > 0 then begin
    expected2 := Copy(AExpectedValue, p+1, MaxInt);;
    expected1 := Copy(AExpectedValue, 1, p-1);
    Result := (ACurrValue = expected1);
    if Result then
      exit;
    Result := (ACurrValue = expected2);
    if Result then
      exit;
  end;

  { Check for fractional result, e.g. exposure time }
  p := pos('/', AExpectedvalue);
  if p > 0 then begin
    valcurr := StrToFloat(ACurrValue);
    expected1 := Copy(AExpectedValue, 1, p-1);
    expected2 := Copy(AExpectedValue, p+1, MaxInt);
    valexp := StrToInt(expected1) / StrToInt(expected2);
    Result := SameValue(valexp, valcurr, relEPS * valexp);
    if Result then
      exit;
  end;
end;

procedure TMainForm.ExifToListview(AImgData: TImgData; AListView: TListView);
var
  i: Integer;
  lTag: TTagEntry;
begin
  AListview.Items.BeginUpdate;
  try
    AListview.Items.Clear;
    if not AImgData.HasExif then
      exit;
    for i:=0 to AImgData.ExifObj.TagCount-1 do begin
      lTag := AImgData.ExifObj.TagByIndex[i];
      if lTag.Tag = 0 then
        Continue;
      with AListView.Items.Add do begin
        Caption := lTag.Desc;
        SubItems.Add(lTag.Data);
      end;
    end;
    for i:=0 to AImgData.ExifObj.ThumbTagCount-1 do begin
      lTag := AImgData.ExifObj.ThumbTagByIndex[i];
      if lTag.Tag = 0 then
        Continue;
      with AListView.Items.Add do begin
        Caption := lTag.Desc;
        SubItems.Add(lTag.Data);
      end;
    end;
    AListView.AlphaSort;
  finally
    AListview.Items.EndUpdate;
  end;
end;

function TMainForm.GetTagType(ATagName: String): Integer;
var
  i: Integer;
  P: PTagEntry;
begin
  P := FindExifTagDefByName(ATagName);
  if P = nil then
    raise Exception.Create('Tag "' + ATagName + '" not found.');
  Result := P^.TType;
end;

function TMainForm.ReadTagValue(ATagName: String): String;
var
  dt: TDateTime;
  tt: Integer;
  v: variant;
  e: Extended;
begin
  if ATagName = 'Comment' then
    Result := ImgData.Comment    // not an EXIF tag: the value is in the COM segment
  else if ATagName = 'Artist' then
    Result := ImgData.ExifObj.Artist
  else if ATagName = 'ImageDescription' then
    Result := ImgData.ExifObj.ImageDescription
  else if ATagName = 'UserComment' then
    Result := ImgData.ExifObj.ExifComment
  else if ATagName = 'Make' then
    Result := ImgData.ExifObj.CameraMake
  else if ATagName = 'Model' then
    Result := ImgData.ExifObj.CameraModel
  else if ATagName = 'DateTimeOriginal' then begin
    dt := ImgData.ExifObj.DateTimeOriginal;
    Result := FormatDateTime(ISODateFormat, dt);
  end
  else if ATagName = 'DateTimeDigitized' then begin
    dt := ImgData.ExifObj.DateTimeDigitized;
    Result := FormatDateTime(ISODateFormat, dt);
  end
  else if ATagName = 'DateTime' then begin
    dt := ImgData.ExifObj.DateTimeModified;
    Result := FormatDateTime(ISODateFormat, dt);
  end
  else if ATagName = 'GPSLatitude' then begin
    e := ImgData.ExifObj.GPSLatitude;
//    Result := FloatToStr(e);
    Result := GPSToStr(e, ctLatitude);
  end
  else if ATagName = 'GPSLongitude' then begin
    e := ImgData.ExifObj.GPSLongitude;
  //  Result := FloatToStr(e);
    Result := GPSToStr(e, ctLongitude);
  end
  else begin
    v := ImgData.ExifObj.TagValue[ATagName];
    if VarIsNull(v) then
      Result := ''
    else
      Result := VarToStr(v);
  end;
end;

procedure TMainForm.ExifTabControlChange(Sender: TObject);
var
  data: TImgData;
begin
  data := TImgData.Create;
  try
    case ExifTabControl.TabIndex of
      0: data.ProcessFile(EdTestFile.Text);
      1: data.ProcessFile(OutFile);
    end;
    ExifToListView(data, ExifListView);
  finally
    data.Free;
  end;
end;

procedure TMainForm.WriteTagValue(ATagName, ATagValue: String);
var
  tt: Integer;
  p: Integer;
begin
  p := pos('|', ATagValue);
  if p > 0 then
    ATagValue := Copy(ATagValue, 1, p-1);

  if ATagName = 'Comment' then
    ImgData.Comment := ATagValue    // This is no EXIF tag - it's the COM segment
  else if ATagName = 'Artist' then
    ImgData.ExifObj.Artist := ATagValue
  else if ATagName = 'ImageDescription' then
    ImgData.ExifObj.ImageDescription := ATagValue
  else if ATagName = 'UserComment' then
    ImgData.ExifObj.ExifComment := ATagValue
  else if ATagName = 'Make' then
    ImgData.ExifObj.CameraMake := ATagValue
  else if ATagName = 'Model' then
    ImgData.ExifObj.CameraModel := ATagValue
  else if ATagName = 'DateTimeOriginal' then
    ImgData.ExifObj.DateTimeOriginal := ExtractDateTime(ATagValue)
  else if ATagName = 'DateTimeDigitized' then
    ImgData.ExifObj.DateTimeDigitized := ExtractDateTime(ATagValue)
  else if ATagName = 'DateTime' then
    ImgData.ExifObj.DateTimeModified := ExtractDateTime(ATagValue)
  else if ATagName = 'GPSLatitude' then
//    ImgData.ExifObj.GPSLatitude := StrToFloat(ATagValue)
    ImgData.ExifObj.GPSLatitude := StrToGPS(ATagValue)
  else if ATagName = 'GPSLongitude' then
//    ImgData.ExifObj.GPSLongitude := StrToFloat(ATagvalue)
    ImgData.ExifObj.GPSLongitude := StrToGPS(ATagValue)
  else
    ImgData.ExifObj.TagValue[ATagName] := ATagValue;
end;

end.

