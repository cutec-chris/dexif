unit tstreadexif;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, dEXIF;

const
  // Picture with EXIF Data
  co_TestPic01 = './testpictures/original/with_exif_large.jpeg';
  co_DUTPicName01 = './testpictures/DUTPic01.jpeg';

  TEST_PIC = co_DUTPicName01;

type
  { TTsTBasic_dEXIF }

  TTstReadFile_dEXIF= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TstReadFile_ByteOrder;
    procedure TstReadFile_CameraMake;
    procedure TstReadFile_CameraModel;
    procedure TstReadFile_Date;
    procedure TstReadFile_ExposureTime;
    procedure TstReadFile_Flash;
    procedure TstReadFile_FNumber;
    procedure TstReadFile_FocalLength;
    procedure TstReadFile_ImageSize;
    procedure TstReadFile_ISO;
    procedure TstReadFile_Resolution;
  end;


implementation

uses
  FileUtil, DateUtils, StrUtils;

{ Output of ExifTool for DUTPic01.jpeg:

    ExifTool Version Number         : 10.60
    File Name                       : DUTPic01.jpeg
    Directory                       : .
    File Size                       : 2.9 MB
    File Modification Date/Time     : 2017:08:05 11:16:56+02:00
    File Access Date/Time           : 2017:08:05 11:16:56+02:00
    File Creation Date/Time         : 2017:08:05 11:16:56+02:00
    File Permissions                : rw-rw-rw-
    File Type                       : JPEG
    File Type Extension             : jpg
    MIME Type                       : image/jpeg
    JFIF Version                    : 1.01

    Resolution Unit                 : inches
+   X Resolution                    : 300
+   Y Resolution                    : 300
+   Exif Byte Order                 : Big-endian (Motorola, MM)
+   Make                            : SAMSUNG
+   Camera Model Name               : SM-G850F
+   Exposure Time                   : 1/3376
+   F Number                        : 2.2
+   ISO                             : 40, 0                                     Tag "ISOSpeedRatings"
+   Date/Time Original              : 2017:03:15 10:35:11
+   Focal Length                    : 4.1 mm                                    Tag "FocalLength"
+   Image Width                     : 4608
+   Image Height                    : 2592
    Encoding Process                : Baseline DCT, Huffman coding
    Bits Per Sample                 : 8
    Color Components                : 3
    Y Cb Cr Sub Sampling            : YCbCr4:2:0 (2 2)
+   Aperture                        : 2.2                                       Tag "FNumber"
    Image Size                      : 4608x2592
    Megapixels                      : 11.9
+   Shutter Speed                   : 1/3376                                    Tag "ExposureTime"
+   Focal Length                    : 4.1 mm                                    Tag "FocalLength"
    Light Value                     : 15.3

+  <--- test is passed
-  <--- test fails, Tag not found by dExif
}

procedure TTstReadFile_dEXIF.SetUp;
begin
  if not FileExists(co_DUTPicName01) then
    if FileExists(co_TestPic01) then
      CopyFile(co_TestPic01,co_DUTPicName01);
end;

procedure TTstReadFile_dEXIF.TearDown;
begin
  //if FileExists(co_DUTPicName01) then
  //  DeleteFile(co_DUTPicName01);
end;

procedure TTstReadFile_dEXIF.TstReadFile_ByteOrder;
const
  EXIFTOOL_BYTEORDER = true;  // Big-endian (Motorola, MM)
var
  DUT: TImgData;
begin
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(TEST_PIC);
    CheckEquals(EXIFTOOL_BYTEORDER, DUT.ExifObj.MotorolaOrder, 'ByteOrder mismatch');
  finally
    DUT.Free;
  end;
end;

procedure TTstReadFile_dEXIF.TstReadFile_CameraMake;
const
  EXIFTOOL_MAKE = 'SAMSUNG';
var
  DUT: TImgData;
  currStrValue: String;
begin
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(TEST_PIC);
    currStrValue := DUT.ExifObj.LookupTagVal('Make');
    CheckEquals(EXIFTOOL_MAKE, currStrValue, 'Camera Make mismatch');
  finally
    DUT.Free;
  end;
end;

procedure TTstReadFile_dEXIF.TstReadFile_CameraModel;
const
  EXIFTOOL_MODEL = 'SM-G850F';
var
  DUT: TImgData;
  currStrValue: String;
begin
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(TEST_PIC);
    currStrValue := DUT.ExifObj.LookupTagVal('Model');
    CheckEquals(EXIFTOOL_MODEL, currStrValue, 'Camera model mismatch');
  finally
    DUT.Free;
  end;
end;

procedure TTstReadFile_dEXIF.TstReadFile_Date;
const
  EXIFTOOL_DATETIME = '2017:03:15 10:35:11';
var
  DUT: TImgData;
  expectedDateTime: TDateTime;
  currValue: TDateTime;
begin
  DUT := TImgData.Create();
  try
    DUT.ProcessFile(TEST_PIC);
    CheckTRUE(DUT.HasEXIF, 'TImgData cannot detect EXIF in file:'+co_DUTPicName01);
    currValue := DUT.EXIFObj.GetImgDateTime;
    expectedDateTime := ScanDateTime('yyyy:mm:dd hh:nn:ss', EXIFTOOL_DATETIME);
    CheckEquals(expectedDateTime, currValue, 'Date/time mismatch (GetImgDateTime)');
    currvalue := ScanDateTime('yyyy-mm-dd hh:nn:ss', DUT.ExifObj.DateTime);
    CheckEquals(ExpectedDateTime, currValue, 'Date/time mismatch (ExifObj.DateTime)');
  finally
    DUT.Free;
  end;
end;

procedure TTstReadFile_dEXIF.TstReadFile_ExposureTime;
const
  EXIFTOOL_EXPOSURE_TIME = '1/3376';
var
  DUT: TImgData;
  currStrValue: String;
  p: Integer;
begin
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(TEST_PIC);
    currStrValue := DUT.ExifObj.LookupTagVal('ExposureTime');
    if currStrValue <> EXIFTOOL_EXPOSURE_TIME then begin
      p := pos('sec', currStrValue);
      if p <> 0 then Delete(currStrValue, p, MaxInt);
      currStrValue := trim(currStrValue);
    end;
    CheckEquals(EXIFTOOL_EXPOSURE_TIME, currStrValue, 'Exposure time mismatch');
  finally
    DUT.Free;
  end;
end;

procedure TTstReadFile_dEXIF.TstReadFile_Flash;
const
  EXIFTOOL_FLASH_USED = false;  // well... at least it is not mentioned.
var
  DUT: TImgData;
begin
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(TEST_PIC);
    CheckEquals(EXIFTOOL_FLASH_USED, DUT.ExifObj.FlashUsed <> 0, 'Flash used mismatch');
  finally
    DUT.Free;
  end;
end;

procedure TTstReadFile_dEXIF.TstReadFile_FNumber;
const
  EXIFTOOL_FNUMBER = '2.2';
var
  DUT: TImgData;
  currStrValue: String;
begin
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(TEST_PIC);
    currStrValue := DUT.ExifObj.LookupTagVal('FNumber');
    if currStrValue <> EXIFTOOL_FNUMBER then begin
      if Uppercase(currStrValue[1]) = 'F' then
        Delete(currStrValue, 1,1);
      if pos('.', EXIFTOOL_FNUMBER) > 0 then
        currStrValue := StringReplace(currStrValue, ',', '.', [])
      else if pos(',', EXIFTOOL_FNUMBER) > 0 then
        currStrValue := StringReplace(currStrValue, '.', ',', []);
      currStrValue := trim(currStrValue);
    end;
    CheckEquals(EXIFTOOL_FNUMBER, currStrValue, 'FNumber mismatch');
  finally
    DUT.Free;
  end;
end;

procedure StripUnits(s: String; out AValue: Double; out AUnits: String);
var
  i: Integer;
  valStr: String;
  res: Integer;
begin
  valStr := '';
  AUnits := '';
  for i := 1 to Length(s) do begin
    if s[i] in ['0'..'9', '.'] then
      valStr := valstr + s[i]
    else if s[i] = ',' then
      valStr := valstr + '.'
    else if s[i] <> ' ' then
      AUnits := AUnits + s[i];
  end;

  val(valstr, AValue, res);
end;

procedure TTstReadFile_dEXIF.TstReadFile_FocalLength;
const
  EXIFTOOL_FOCAL_LENGTH = '4.1 mm';
var
  DUT: TImgData;
  currStrValue: String;
  exp_val: double;
  exp_units: String;
  curr_val: double;
  curr_units: String;
begin
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(TEST_PIC);
    currStrValue := DUT.ExifObj.LookupTagVal('FocalLength');
    if currStrValue =  EXIFTOOL_FOCAL_LENGTH then
      CheckEquals(EXIFTOOL_FOCAL_LENGTH, currStrValue, 'Focal length mismatch')
    else begin
      StripUnits(EXIFTOOL_FOCAL_LENGTH, exp_val, exp_units);
      StripUnits(currStrValue, curr_val, curr_units);
      if (exp_units = curr_units) and (exp_units = 'mm') then begin
        CheckEquals(FormatFloat('0.0', exp_val), FormatFloat('0.0', curr_val),
          'Focal length mismatch');
      end else
        Ignore('Different focal length units.');
    end;
  finally
    DUT.Free;
  end;
end;

procedure TTstReadFile_dEXIF.TstReadFile_ImageSize;
const
  EXIFTOOL_IMAGE_WIDTH = 4608;
  EXIFTOOL_IMAGE_HEIGHT = 2592;
var
  DUT: TImgData;
begin
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(TEST_PIC);
    CheckEquals(EXIFTOOL_IMAGE_WIDTH, DUT.Width, 'Image width mismatch');
    CheckEquals(EXIFTOOL_IMAGE_HEIGHT, DUT.Height, 'Imgae height mismatch');
  finally
    DUT.Free;
  end;
end;

procedure TTstReadFile_dEXIF.TstReadFile_ISO;
const
  EXIFTOOL_ISO = '40, 0';   // "hat does the ", 0" mean ???
var
  DUT: TImgData;
  currStrValue: String;
begin
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(TEST_PIC);
    currStrValue := DUT.ExifObj.LookupTagVal('ISOSpeedRatings');
    CheckEquals(EXIFTOOL_ISO, currStrValue, 'ISO mismatch');
  finally
    DUT.Free;
  end;
end;

procedure TTstReadFile_dEXIF.TstReadFile_Resolution;
const
  EXIFTOOL_X_RESOLUTION = 300;
  EXIFTOOL_Y_RESOLUTION = 300;
  EXIFTOOL_RESOLUTION_UNIT = 'inches';
var
  DUT: TImgData;
  currIntValue: Integer;
  currStrValue: String;
begin
  DUT:= TImgData.Create();
  DUT.ProcessFile(TEST_PIC);
  CheckTRUE(DUT.HasEXIF, 'TImgData cannot detect EXIF in file:'+co_DUTPicName01);
  currIntValue := DUT.XResolution;
  CheckEquals(EXIFTOOL_X_RESOLUTION, currIntValue, 'X resolution mismatch');
  currIntValue := DUT.YResolution;
  CheckEquals(EXIFTOOL_Y_RESOLUTION, currIntValue, 'Y resolution mismatch');
  currStrValue := DUT.ResolutionUnit;
  CheckEquals(Uppercase(EXIFTOOL_RESOLUTION_UNIT), Uppercase(currStrValue), 'Resolution unit mismatch');
  DUT.Free;
end;


initialization

  RegisterTest(TTstReadFile_dEXIF);

end.

