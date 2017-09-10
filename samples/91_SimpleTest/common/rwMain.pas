unit rwMain;

{$I ..\..\..\dExif.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
 {$IFDEF FPC}
  LazUtf8,
 {$ELSE}
  Windows, Messages, jpeg,
 {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, Variants,
  dGlobal, dMetadata, ImgList;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnTest1: TSpeedButton;
    BtnTest2: TSpeedButton;
    CbTestfile: TComboBox;
    ImageList1: TImageList;
    Label1: TLabel;
    ListView: TListView;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    ExifListView: TListView;
    ExifTabControl: TTabControl;
    BtnBrowse: TSpeedButton;
    Splitter1: TSplitter;
    procedure CbTestfileEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnTest1Click(Sender: TObject);
    procedure ExifTabControlChange(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
  private
    ImgData: TImgData;
    OutFile: String;
    procedure ExecTest(const AParamsFile: String);
    procedure ExifToListview(AImgData: TImgData; AListView: TListView);
    function GetTagType(ATagName: String): Integer;
    function ReadTagValue(ATagName: String): String;
    function Success(ACurrValue, AExpectedValue: String): Boolean;
    procedure WriteTagValue(ATagName, ATagValue: String);

    procedure AddToHistory(AFilename: String);
    procedure ReadFromIni;
    procedure WriteToIni;

  public
    procedure BeforeRun;

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
  StrUtils, Math, IniFiles,
  dUtils, dExif;

const
  IMGINDEX_SUCCESS = 0;
  IMGINDEX_FAIL = 1;

  TESTCASES_DIR = '..\common\';

type
  TStringArray = array of string;

function Split(s: String; AMinCount: Integer; Separator: Char = #9): TStringArray;
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

  s := s + Separator;
  L := Length(s);
  SetLength(Result, BLOCK_SIZE);
  i := 1;
  j := 1;
  n := 0;
  while (i <= L) do begin
    if (s[i] = Separator) or (i = L)  then begin
      Result[n] := Copy(s, j, i-j);
      inc(n);
      if n mod BLOCK_SIZE = 0 then
        SetLength(Result, Length(Result) + BLOCK_SIZE);
      j := i+1;
    end;
    inc(i);
  end;
  while n < AMinCount do begin
    Result[n] := '';
    inc(n);
    if n mod BLOCK_SIZE = 0 then
      SetLength(Result, Length(Result) + BLOCK_SIZE);
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

function DecimalSep: Char;
begin
 {$IFDEF FPC}
   Result := FormatSettings.DecimalSeparator;
 {$ELSE}
  {$IFDEF VER150}  // Delphi 7
   Result := DecimalSeparator;
  {$ELSE}
   Result := FormatSettings.DecimalSeparator;
  {$ENDIF}
 {$ENDIF}
end;


{ TMainForm }

procedure TMainForm.AddToHistory(AFileName: String);
var
  i: Integer;
begin
  if (AFileName = '') or (not FileExists(AFileName)) then
    exit;

  i := CbTestFile.Items.Indexof(AFileName);
  if i > -1 then
    CbTestfile.Items.Delete(i);
  CbTestFile.Items.Insert(0, AFileName);
  CbTestFile.ItemIndex := 0;
end;

procedure TMainForm.BeforeRun;
begin
  ReadFromIni;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ImgData := TImgData.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteToIni;
  ImgData.Free;
end;

procedure TMainForm.BtnTest1Click(Sender: TObject);
begin
  AddToHistory(CbTestFile.Text);
  if Sender = BtnTest1 then
    ExecTest(TESTCASES_DIR + 'testcases1.txt')
  else if Sender = BtnTest2 then
    ExecTest(TESTCASES_DIR + 'testcases2.txt')
  else
    raise Exception.Create('BtnTextClick: Unexpected Sender');
end;

procedure TMainForm.CbTestfileEditingDone(Sender: TObject);
begin
  AddToHistory(CbTestFile.Text);
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
  jpeg: TJpegImage;
  {$IFDEF FPC}
  stream: TMemorystream;
  {$ELSE}
  stream: TMemoryStream;
  a: ansistring;
  {$ENDIF}
begin
  Listview.Items.Clear;

  if not FileExists(AParamsFile) then begin
    showMessage('Parameter file "' + AParamsFile + '" not found.');
    exit;
  end;
  if not FileExists(CbTestfile.Text) then begin
    ShowMessage('Test picture file "' + CbTestfile.Text + '" not found.');
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
  {$ENDIF}

    // Read EXIF tags from image file
    ImgData.ProcessFile(CbTestfile.Text);
    if not ImgData.HasExif then
      ImgData.CreateExifObj;

    OutFile := 'test-image.jpg';   // File name of the modified test image

    ListView.Items.BeginUpdate;
    try
      j := 0;
      n := testCases.Count;
      for i:=0 to n-1 do begin
        if (testCases[i] = ':quit') then
          break;

        if (testCases[i] = '') or (testCases[i][1] = ';') then
          Continue;

        // Extract test parameters
        testdata := Split(testCases[i], 2);
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
    if not ImgData.HasExif then
      raise Exception.Create('No exif structure detected in "' + Outfile + '"');

    j := 0;
    for i:=0 to testCases.Count-1 do begin
      if (testcases[i] = ':quit') then
        break;
      if (testcases[i] = '') or (testcases[i][1] = ';') then
        Continue;
      testdata := Split(testCases[i], 2);
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

    jpeg := TJpegImage.Create;
    try
      jpeg.LoadFromFile(OutFile);
      listitem := ListView.Items.Add;
      listItem.Caption := 'Successfully loaded';
      listItem.ImageIndex := IMGINDEX_SUCCESS;
    except
      listitem := ListView.Items.Add;
      listItem.Caption := 'Loading failed.';
      listItem.ImageIndex := IMGINDEX_FAIL;
    end;

  finally
    testCases.Free;
  end;

  ExifTabControlChange(nil);
end;

procedure TMainForm.BtnBrowseClick(Sender: TObject);
var
  olddir: String;
begin
  olddir := GetCurrentDir;
  OpenDialog.FileName := '';
  if OpenDialog.Execute then
    AddToHistory(OpenDialog.Filename);
  SetCurrentDir(oldDir);
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

  if (ACurrValue = '') or (AExpectedValue = '') then begin
    Result := false;
    exit;
  end;

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
  p := pos('/', AExpectedValue);
  if p > 0 then begin
    valcurr := StrToFloat(ACurrValue, dExifFmtSettings);
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

  procedure FixDecimalSeparator(var s: String);
  var
    i: Integer;
    decsep: char;
  begin
    decsep := DecimalSep;
    for i:=1 to Length(s) do
      if not ((s[i] in ['0'..'9']) or (s[i] = decsep)) then
        exit;
    for i:=1 to Length(s) do
      if s[i] = decsep then s[i] := '.';
  end;

var
  dt: TDateTime;
  v: variant;
  e: Extended;
  i: Integer;
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
    Result := FormatDateTime(ISO_DATETIME_FORMAT, dt);
  end
  else if ATagName = 'DateTimeDigitized' then begin
    dt := ImgData.ExifObj.DateTimeDigitized;
    Result := FormatDateTime(ISO_DATETIME_FORMAT, dt);
  end
  else if ATagName = 'DateTime' then begin
    dt := ImgData.ExifObj.DateTimeModified;
    Result := FormatDateTime(ISO_DATETIME_FORMAT, dt);
  end
  else if ATagName = 'GPSLatitude' then begin
    e := ImgData.ExifObj.GPSLatitude;
    Result := GPSToStr(e, ctLatitude);
  end
  else if ATagName = 'GPSLongitude' then begin
    e := ImgData.ExifObj.GPSLongitude;
    Result := GPSToStr(e, ctLongitude);
  end
  else begin
    v := ImgData.ExifObj.TagValue[ATagName];
    if VarIsNull(v) then
      Result := ''
    else
    if VarIsArray(v) then begin
      Result := '';
      i := VarArrayHighBound(v, 1);
      for i:=VarArrayLowBound(v, 1) to VarArrayHighBound(v, 1) do
        Result := Result + dExifDataSep + VarToStr(v[i]);
      if Result <> '' then Delete(Result, 1, Length(dExifDataSep));
    end
    else begin
      Result := VarToStr(v);
      FixDecimalSeparator(Result);
    end;
  end;
end;

procedure TMainForm.ExifTabControlChange(Sender: TObject);
var
  data: TImgData;
begin
  data := TImgData.Create;
  try
    case ExifTabControl.TabIndex of
      0: data.ProcessFile(CbTestfile.Text);
      1: data.ProcessFile(OutFile);
    end;
    ExifToListView(data, ExifListView);
  finally
    data.Free;
  end;
end;

procedure TMainForm.WriteTagValue(ATagName, ATagValue: String);

  procedure FixDecimalSeparator(var s: String);
  var
    i: Integer;
  begin
    for i:=1 to Length(s) do
      if not (s[i] in ['0'..'9', '.']) then
        exit;
    for i:=1 to Length(s) do
      if s[i] = '.' then s[i] := DecimalSep;
  end;

var
  tt: Integer;
  p: Integer;
begin
  p := pos('|', ATagValue);
  if p > 0 then
    ATagValue := Copy(ATagValue, 1, p-1);

  FixDecimalSeparator(ATagValue);

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
    ImgData.ExifObj.GPSLatitude := StrToGPS(ATagValue)
  else if ATagName = 'GPSLongitude' then
    ImgData.ExifObj.GPSLongitude := StrToGPS(ATagValue)
  else
    ImgData.ExifObj.TagValue[ATagName] := ATagValue;
end;

function CreateIni: TCustomIniFile;
begin
  Result := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
end;

procedure TMainForm.ReadFromIni;
var
  ini: TCustomIniFile;
  list: TStrings;
  i: Integer;
  W, H, L, T: Integer;
  R: TRect;
begin
  ini := CreateIni;
  try
    list := TStringList.Create;
    try
      if WindowState = wsNormal then begin
        W := ini.ReadInteger('MainForm', 'Width', Width);
        H := ini.ReadInteger('MainForm', 'Height', Height);
        L := ini.ReadInteger('MainForm', 'Left', Left);
        T := ini.ReadInteger('MainForm', 'Top', Top);
        R := Screen.DesktopRect;
        if W > R.Right - R.Left then W := R.Right - R.Left;
        if L+W > R.Right then L := R.Right - W;
        if L < R.Left then L := R.Left;
        if H > R.Bottom - R.Top then H := R.Bottom - R.Top;
        if T+H > R.Bottom then T := R.Bottom - H;
        if T < R.Top then T := R.Top;
        SetBounds(L, T, W, H);
      end;

      ini.ReadSection('History', list);
      for i:=list.Count-1 downto 0 do  // count downward because AddToHistory adds to the beginning of the list
        AddToHistory(ini.ReadString('History', list[i], ''));
      CbTestFile.ItemIndex := 0;
    finally
      list.Free;
    end;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.WriteToIni;
var
  ini: TCustomIniFile;
  i: Integer;
begin
  ini := CreateIni;
  try
    ini.WriteInteger('MainForm', 'Left', Left);
    ini.WriteInteger('MainForm', 'Top', Top);
    ini.WriteInteger('MainForm', 'Width', Width);
    ini.WriteInteger('MainForm', 'Height', Height);

    for i:=0 to CbTestFile.Items.Count-1 do
      if (CbTestFile.Items[i] <> '') and FileExists(CbTestFile.Items[i]) then
        ini.WriteString('History', 'Item'+IntToStr(i+1), CbTestFile.Items[i]);
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

end.

