unit tstwritereadexif;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif FPC}

// If ERASE_TESTIMAGE is active then the test images are deleted after the test.
// Deactivate this define for debugging purposes.
{$DEFINE ERASE_TESTIMAGE}

// If TEST_FILE_INTEGRITY is activated then the written image file is opened
// by a TJpegImage to make sure that it is a valid jpeg.
// Slows down the test!
{$DEFINE TEST_FILE_INTEGRITY}

interface

uses
  Classes
  {$ifdef FPC}
     , Graphics, SysUtils, fpcunit, testutils, testregistry
  {$else}
     , TestFrameWork
  {$endif}
     , dEXIF;

const
  // Picture with EXIF data taken from CANON camera }
  co_SrcPic = './testpictures/original/img_9438.jpg';
  co_DestPic = './testpictures/ReadWriteTest.jpg';

type
  { TTsWriteReadFile_dEXIF }

  TTstWriteReadFile_dEXIF= class(TTestCase)
  {$ifdef FPC}
    protected
  {$else}
    public
  {$endif}
    procedure SetUp; override;
    procedure TearDown; override;
  protected
    FSourceFilename: String;
    FDestFileName: String;
    procedure GenericTest(ATestID: Integer);
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    procedure Test_DateTimeOriginal;
    procedure Test_DateTimeDigitized;
    procedure Test_DateTimeModify;
    procedure Test_CommentSegment;
    procedure Test_CommentSegment_UTF8;
    procedure Test_Artist;
    procedure Test_Artist_Umlaut;
    procedure Test_ExifComment_ASCII;
    procedure Test_ExifComment_UNICODE;
    procedure Test_ImageDescription;
    procedure Test_CameraMake;
    procedure Test_CameraMake_TooLong;
    procedure Test_CameraModel;
    procedure Test_Copyright;
    procedure Test_GPSLatitude_DMS;
    procedure Test_GPSLongitude_DMS;
    procedure Test_GPSLatitude_DM;
    procedure Test_GPSLongitude_DM;
  end;

implementation

uses
  FileUtil, DateUtils, Math, dGlobal, dUtils
{$ifdef FPC}
{$else}
  , Winapi.Windows
{$endif}
  ;


type
  TWriteReadParam = record
    Tag: String;
    Value: String;      // all results will be converted to strings
    Decimals: Integer;  // needed for conversion of floats to string
  end;

const
  // !!! INCREMENT WHEN ADDING TESTS !!!
  TestCount = 18;

{$WARN 3177 off : Some fields coming after "$1" were not initialized}

  // !!! ADD NEW TESTS HERE !!!
  TestParams: Array[0..TestCount-1] of TWriteReadParam = (
{0}  (Tag:'DateTimeOriginal';   Value:'2017-01-01 12:00:00'),
     (Tag:'DateTimeDigitized';  Value:'2017-01-01 12:00:00'),
     (Tag:'DateTimeModify';     Value:'2017-01-01 12:00:00'),
     (Tag:'Comment section';    Value:'This is a comment.'),
     (Tag:'Comment section';    Value:'This is a comment - äöüß.'),
{5}  (Tag:'Artist';             Value:'Ansel Adams'),
     (Tag:'Artist';             Value:'Hansi Müller'),   // Arist with Umlaut
     (Tag:'ExifComment';        Value:'This is a comment'),
     (Tag:'ExifComment';        Value:'äöü αβγ'),
     (Tag:'ImageDescription';   Value:'My image'),
{10} (Tag:'CameraMake';         Value:'Kodak'),   // same length as text in file (Canon)  --> pass
     (Tag:'CameraMake';         Value:'Minolta'), // longer than text in file --> fail
     (Tag:'CameraModel';        Value:'My Super Camera'),
     (Tag:'Copyright';          Value:'(c) Team'),
     (Tag:'GPSLatitude';        Value:'20° 30'' 40.123" N'),
     (Tag:'GPSLongitude';       Value:'10° 20'' 30.456" W'),
     (Tag:'GPSLatitude';        Value:'22° 31.1234567'' S'),
     (Tag:'GPSLongitude';       Value:'11° 21.3456788'' E')
  );


{ TTstWriteReadFile_dEXIF }

constructor TTstWriteReadFile_dEXIF.Create;
begin
  inherited;
  FSourceFileName := co_SrcPic;
  FDestFileName := co_DestPic;
end;

destructor TTstWriteReadFile_dEXIF.Destroy;
begin
  {$IFDEF ERASE_TESTIMAGE}
    if FileExists(FDestFileName) then
      DeleteFile(FDestFileName);
  {$ENDIF}
  inherited;
end;

procedure TTstWriteReadFile_dEXIF.SetUp;
begin
  if not FileExists(co_DestPic) then
    if FileExists(co_SrcPic) then
      CopyFile(co_SrcPic, co_DestPic);
end;

procedure TTstWriteReadFile_dEXIF.TearDown;
begin
  // nothing to do here
end;


{ Standard test }

procedure TTstWriteReadFile_dEXIF.GenericTest(ATestID: Integer);
var
  srcDUT: TImgData;
  destDUT: TImgData;
  currVal: string;
  expVal: string;
  oldVal: string;
  msg: String;
  imgStream: TMemoryStream;
  jpeg: TJpegImage;

  function ReadTagValue(ATestID: Integer; DUT: TImgData): string;
  var
    f: Extended;
  begin
    Result := '';
    // !!!!!  ADD NEW TESTS HERE !!!!!!
    case ATestID of
      0: Result := FormatDateTime(ISODateFormat, DUT.EXIfObj.DateTimeOriginal);
      1: Result := FormatDateTime(ISODateFormat, DUT.EXIfObj.DateTimeDigitized);
      2: Result := FormatDateTime(ISODateFormat, DUT.EXIfObj.DateTimeModified);
      3: Result := DUT.Comment;
      4: Result := DUT.Comment;
      5: Result := DUT.ExifObj.Artist;
      6: Result := DUT.ExifObj.Artist;
      7: Result := DUT.ExifObj.ExifComment;
      8: Result := DUT.ExifObj.ExifComment;
      9: Result := DUT.ExifObj.ImageDescription;
     10: Result := DUT.ExifObj.CameraMake;
     11: Result := DUT.ExifObj.CameraMake;
     12: Result := DUT.ExifObj.CameraModel;
     13: Result := DUT.ExifObj.TagValueAsString['Copyright'];
     14: begin
           f := DUT.ExifObj.GPSLatitude;
           Result := GpsToStr(f, ctLatitude, gf_DMS_Short, 3);
         end;
     15: begin
           f := DUT.ExifObj.GPSLongitude;
           Result := GpsToStr(f, ctLongitude, gf_DMS_Short, 3);
         end;
     16: begin
           f := DUT.ExifObj.GPSLatitude;
           Result := GpsToStr(f, ctLatitude, gf_DM_Short, 7);
         end;
     17: begin
           f := DUT.ExifObj.GPSLongitude;
           Result := GpsToStr(f, ctLongitude, gf_DM_Short, 7);
         end;
    end;
  end;

  procedure WriteTagValue(ATestID: Integer; DUT: TImgData);
  var
    strValue: String;
  begin
    strValue := TestParams[ATestID].Value;
    // !!!!!  ADD NEW TESTS HERE !!!!!!
    case ATestID of
      0: DUT.ExifObj.DatetimeOriginal := ScanDatetime(ISODateFormat, strValue);
      1: DUT.ExifObj.DatetimeDigitized := ScanDatetime(ISODateFormat, strValue);
      2: DUT.ExifObj.DatetimeModified := ScanDatetime(ISODateFormat, strValue);
      3: DUT.Comment := strValue;
      4: DUT.Comment := strValue;
      5: DUT.ExifObj.Artist := strValue;
      6: DUT.ExifObj.Artist := strValue;
      7: DUT.ExifObj.ExifComment := strValue;
      8: DUT.ExifObj.ExifComment := strValue;
      9: DUT.ExifObj.ImageDescription := strValue;
     10: DUT.ExifObj.CameraMake := strValue;
     11: DUT.ExifObj.CameraMake := strValue;
     12: DUT.ExifObj.CameraModel := strValue;
     13: DUT.ExifObj.TagValueAsString['Copyright'] := strValue;
     14: DUT.ExifObj.GpsLatitude := StrToGps(strValue);
     15: DUT.ExifObj.GPSLongitude := StrToGps(strValue);
     16: DUT.ExifObj.GpsLatitude := StrToGps(strValue);
     17: DUT.ExifObj.GPSLongitude := StrToGps(strValue);
    end;
  end;

begin
  if ATestID >= TestCount then
    fail('Unknown test ID');

  srcDUT := TImgData.Create;
  try
    srcDUT.ProcessFile(FSourceFileName);
    // -------------------------------------------------------------------------
    // First test
    // -------------------------------------------------------------------------
    // Read the requested tag. If the tag is present, then save the file
    // unchanged, and make sure that the tag has not changed.
    // -------------------------------------------------------------------------
    oldVal := ReadTagValue(ATestID, srcDUT);
    if oldVal <> '' then begin

      imgStream := TMemoryStream.Create;
      try
        imgStream.LoadFromFile(FSourceFilename);
        srcDUT.WriteEXIFJpeg(imgStream, FDestFileName);
      finally
        imgStream.Free;
      end;

      //srcDUT.WriteExifJpeg(FDestFileName, FSourceFilename);

      destDUT := TImgData.Create;
      try
        destDUT.ProcessFile(FDestFileName);
        currVal := ReadTagValue(ATestID, destDUT);
        msg := 'Unintended change of tag ' + TestParams[ATestID].Tag;
        CheckEquals(oldval, currVal, msg);
      finally
        destDUT.Free;
      end;
    end;

    // -------------------------------------------------------------------------
    // Second test
    // -------------------------------------------------------------------------
    // Change the requested tag according to the value in TestParams
    // Read the parameter without saving and check whether it really has
    // changed as intended.
    // -------------------------------------------------------------------------
    WriteTagValue(ATestID, srcDUT);
    currVal := ReadTagValue(ATestID, srcDUT);

    msg := TestParams[ATestID].Tag + ' creation mismatch.';
    CheckEquals(TestParams[ATestID].Value, currVal, msg);

    // -------------------------------------------------------------------------
    // Third test
    // -------------------------------------------------------------------------
    // Save the modified file, read back and check wether the tag value
    // matches the written value
    // -------------------------------------------------------------------------

    srcDUT.WriteExifJpegTo(FDestFileName);
    (*
    imgStream := TMemoryStream.Create;
    try
      imgStream.LoadFromFile(FSourceFilename);
      srcDUT.WriteEXIFJpeg(imgStream, FDestFileName);
    finally
      imgStream.Free;
    end;
      *)
//    srcDUT.WriteExifJpeg(FDestFileName, FSourceFilename);

    destDUT := TImgData.Create;
    try
      destDUT.ProcessFile(FDestFileName);
      currVal := ReadTagValue(ATestID, destDUT);
      msg := TestParams[ATestID].Tag + ' readback mismatch.';
      CheckEquals(TestParams[ATestID].Value, currVal, msg);
    finally
      destDUT.Free;
    end;

    {$IFDEF TEST_FILE_INTEGRITY}
    // -------------------------------------------------------------------------
    // Forth test
    // -------------------------------------------------------------------------
    // Open the written file into a TJpegImage. It must open without an error.
    //--------------------------------------------------------------------------
    jpeg := TJpegImage.Create;
    try
      jpeg.LoadfromFile(FDestFileName);
    except
      fail('Incorrectly written file "' + FDestFileName + '"');
    end;
    {$ENDIF}

  finally
    srcDUT.Free;
  end;
end;

{ DateTime Original }
procedure TTstWriteReadFile_dEXIF.Test_DateTimeOriginal;
begin
  GenericTest(0);
end;

{ DateTime Digitized }
procedure TTstWriteReadFile_dEXIF.Test_DateTimeDigitized;
begin
  GenericTest(1);
end;

{ DateTime Modify }
procedure TTstWriteReadFile_dEXIF.Test_DateTimeModify;
begin
  GenericTest(2);
end;

{ Comment in COM segment }
procedure TTstWriteReadFile_dEXIF.Test_CommentSegment;
begin
  GenericTest(3);
end;

{ Comment in COM segment }
procedure TTstWriteReadFile_dEXIF.Test_CommentSegment_UTF8;
begin
  GenericTest(4);
end;

{ Artist }
procedure TTstWriteReadFile_dEXIF.Test_Artist;
begin
  GenericTest(5);
end;

{ Artist with Umlaut}
procedure TTstWriteReadFile_dEXIF.Test_Artist_Umlaut;
begin
  GenericTest(6);
end;

{ UserComment in EXIF segment - ASCII }
procedure TTstWriteReadFile_dEXIF.Test_ExifComment_ASCII;
begin
  GenericTest(7);
end;

{ UserComment in EXIF - UNICODE }
procedure TTstWriteReadFile_dEXIF.Test_ExifComment_UNICODE;
begin
  GenericTest(8);
end;

{ Image description }
procedure TTstWriteReadFile_dEXIF.Test_ImageDescription;
begin
  GenericTest(9);
end;

{ Camera make }
procedure TTstWriteReadFile_dEXIF.Test_CameraMake;
begin
  GenericTest(10);
end;

procedure TTstWriteReadFile_dEXIF.Test_CameraMake_TooLong;
begin
  GenericTest(11);
end;

{ Camera model }
procedure TTstWriteReadFile_dEXIF.Test_CameraModel;
begin
  GenericTest(12);
end;

{ Copyright }
procedure TTstWriteReadFile_dEXIF.Test_Copyright;
begin
  GenericTest(13);
end;

{ GPS Latitude - DMS format }
procedure TTstWriteReadFile_dEXIF.Test_GPSLatitude_DMS;
begin
  GenericTest(14);
end;

{ GPS Longitude - DMS format }
procedure TTstWriteReadFile_dEXIF.Test_GPSLongitude_DMS;
begin
  GenericTest(15);
end;

{ GPS Latitude - DM format }
procedure TTstWriteReadFile_dEXIF.Test_GPSLatitude_DM;
begin
  GenericTest(16);
end;

{ GPS Longitude - DM format }
procedure TTstWriteReadFile_dEXIF.Test_GPSLongitude_DM;
begin
  GenericTest(17);
end;


initialization
  RegisterTest(TTstWriteReadFile_dEXIF);

end.

