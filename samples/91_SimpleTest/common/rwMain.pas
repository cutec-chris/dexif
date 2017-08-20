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
  Grids, StdCtrls, Buttons, ComCtrls, dExif, ImgList;

type

  { TMainForm }

  TMainForm = class(TForm)
    EdTestfile: TEdit;
    BtnTest1: TSpeedButton;
    BtnTest2: TSpeedButton;
    ImageList1: TImageList;
    ListView: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnTest1Click(Sender: TObject);
  private
    ImgData: TImgData;
    function ReadTagValue(ATagName: String): String;
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
  StrUtils;

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
var
  testCases: TStringList;
  i, j: Integer;
  fn: String;
  s: String;
  testdata: TStringArray;
  listitem: TListItem;
  tagName: String;
  currTagValue: String;
  newTagValue: String;
  {$IFDEF FPC}
  stream: TMemorystream;
  {$ELSE}
  jpeg: TJpegImage;
  {$ENDIF}
begin
  Listview.Items.Clear;

  // Read test parameters
  testCases := TStringList.Create;
  try
    if Sender = BtnTest1 then
      fn := TESTCASES_DIR + 'testcases1.txt'
    else
      fn := TESTCASES_DIR + 'testcases2.txt';
    testCases.LoadFromFile(fn);

    {$IFDEF FPC}
    // The testcases text files are encoded in ANSI for Delphi7 compatibility
    // In Lazarus we must convert to UTF8 }
    s := testCases.Text;
    {$IFDEF FPC3+}
    testCases.Text := WinCPToUTF8(s);
    {$ELSE}
    testCases.Text := AnsiToUTF8(s);
    {$ENDIF}
    {$ENDIF}

    // Read EXIF tags from image file
    ImgData.ProcessFile(EdTestFile.Text);

    fn := 'test-image.jpg';   // File name of the modified test image

    j := 0;
    for i:=0 to testCases.Count-1 do begin
      if (TestCases[i] = '') or (TestCases[i][1] = ';') then
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

    // Write new tags to file
    {$IFDEF FPC}
    stream := TMemoryStream.Create;
    try
      stream.LoadfromFile(EdTestFile.Text);
      ImgData.WriteEXIFJpeg(stream, fn);
    finally
      stream.Free;
    end;
    {$ELSE}
    jpeg := TJpegImage.Create;
    try
      jpeg.LoadFromFile(EdTestFile.Text);
      ImgData.WriteEXIFJpeg(jpeg, fn);
    finally
      jpeg.Free;
    end;
    {$ENDIF}

    // read back
    ImgData.ProcessFile(fn);
    j := 0;
    for i:=0 to testCases.Count-1 do begin
      if (testcases[i] = '') or (testcases[i][1] = ';') then
        Continue;
      testdata := Split(testCases[i]);
      tagname := testdata[0];
      newTagValue := testdata[1];
      currTagValue := ReadTagValue(tagname);
      listItem := ListView.Items[j];
      listItem.SubItems.Add(currTagValue);
      if currTagValue = newTagValue then
        listItem.ImageIndex := IMGINDEX_SUCCESS
      else
        listItem.ImageIndex := IMGINDEX_FAIL;
      inc(j);
    end;
  finally
    testCases.Free;
  end;
end;

function TMainForm.ReadTagValue(ATagName: String): String;
begin
  if ATagName = 'Artist' then
    Result := ImgData.ExifObj.Artist
  else if ATagName = 'ImageDescription' then
    Result := ImgData.ExifObj.ImageDescription
  else if ATagName = 'UserComment' then
    Result := ImgData.ExifObj.ExifComment
  else if ATagName = 'Make' then
    Result := ImgData.ExifObj.CameraMake
  else if ATagName = 'Model' then
    Result := ImgData.ExifObj.CameraModel
  else
    Result := ImgData.ExifObj.TagValueAsString[ATagName];
end;

procedure TMainForm.WriteTagValue(ATagName, ATagValue: String);
begin
  if ATagName = 'Artist' then
    ImgData.ExifObj.Artist := ATagValue
  else if ATagName = 'ImageDescription' then
    ImgData.ExifObj.ImageDescription := ATagValue
  else if ATagName = 'UserComment' then
    ImgData.ExifObj.ExifComment := ATagValue
  else if ATagName = 'Make' then
    ImgData.ExifObj.CameraMake := ATagValue
  else if ATagName = 'Model' then
    ImgData.ExifObj.CameraModel := ATagValue
  else
    ImgData.ExifObj.TagValueAsString[ATagName] := ATagValue;
end;

end.

