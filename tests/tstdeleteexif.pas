unit tstDeleteExif;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif FPC}

{$i dExifTest.inc}

// If ERASE_TESTIMAGE is active then the test images are deleted after the test.
// Deactivate this define for debugging purposes.
{$DEFINE ERASE_TESTIMAGE}

// If TEST_FILE_INTEGRITY is activated then the written image file is opened
// by a TJpegImage to make sure that it is a valid jpeg.
// Slows down the test!
{$DEFINE TEST_FILE_INTEGRITY}

interface

uses
  Classes, SysUtils
  {$ifdef FPC}
    , Graphics, FileUtil, fpcunit, testutils, testregistry
  {$else}
    , Windows, Graphics, jpeg, TestFrameWork
  {$endif}
    , dMetadata, dEXIF;

const
  // Picture with EXIF data taken from CANON camera }
  co_DeleteSrcPic_01 = './testpictures/original/with_exif_small.jpg';
  co_DeleteSrcPic_02 = './testpictures/original/img_9438.jpg';
  co_DeleteSrcPic_03 = './testpictures/original/Schilfgebiet.jpg';

  co_DeleteTestPic   = './testpictures/DeleteTest.jpg';


type
  { TTstDeleteTag_dEXIF }

  TTstDeleteTag_dEXIF= class(TTestCase)
  {$ifdef FPC}
    protected
  {$else}
    public
  {$endif}
    procedure SetUp; override;
    procedure TearDown; override;
  protected
    FSourceFilename: String;
    FTestFileName: String;
    procedure GenericTest(const ATagName: String);
  published
  end;

  TTstDeleteTag_dEXIF_01 = class(TTstDeleteTag_dEXIF)
  {$ifdef FPC}
    protected
  {$else}
    public
  {$endif}
    procedure SetUp; override;
  published
    procedure TstDelete_Artist;
    procedure TstDelete_CommentExif;
    procedure TstDelete_CommentSegment;
    procedure TstDelete_ImageDescription;
    procedure TstDelete_GPSLatitude;
    procedure TstDelete_FNumber;
  end;

  TTstDeleteTag_dEXIF_02 = class(TTstDeleteTag_dEXIF)
  {$ifdef FPC}
    protected
  {$else}
    public
  {$endif}
    procedure SetUp; override;
  published
    procedure TstDelete_Artist;
    procedure TstDelete_CommentExif;
    procedure TstDelete_CommentSegment;
    procedure TstDelete_ImageDescription;
    procedure TstDelete_GPSLatitude;
    procedure TstDelete_FNumber;
  end;

  TTstDeleteTag_dEXIF_03 = class(TTstDeleteTag_dEXIF)
  {$ifdef FPC}
    protected
  {$else}
    public
  {$endif}
    procedure SetUp; override;
  published
    procedure TstDelete_Artist;
    procedure TstDelete_CommentExif;
    procedure TstDelete_CommentSegment;
    procedure TstDelete_ImageDescription;
    procedure TstDelete_GPSLatitude;
    procedure TstDelete_FNumber;
  end;


implementation

uses
  variants;

{$ifndef FPC}
  function CopyFile(f1,f2:string):boolean;
  begin
    Result:=  {$ifndef DELPHI7}Winapi.{$endif}Windows.CopyFile(PChar(f1),PChar(f2),true);
  end;
{$endif}

function TagAvail(aDUT: TImgData; aTagName: String): Boolean;
begin
  if SameText(aTagName, 'Comment') then
    Result := aDUT.HasComment
  else
    Result := aDUT.HasExif and not VarIsNull(aDUT.ExifObj.TagValue[aTagName]);
end;

procedure DeleteTag(aDUT: TImgData; aTagName: String);
begin
  if SameText(aTagName, 'Comment') then
    aDUT.Comment := ''
  else
  if aDUT.HasExif then
    aDUT.ExifObj.TagValue[aTagName] := NULL;
end;


{ TTstDeleteTag_dEXIF }

procedure TTstDeleteTag_dEXIF.SetUp;
begin
  FTestFileName := co_DeleteTestPic;
end;

procedure TTstDeleteTag_dEXIF.TearDown;
begin
  {$IFDEF ERASE_TESTIMAGE}
    if FileExists(FTestFileName) then
      SysUtils.DeleteFile(FTestFileName);
  {$ENDIF}
end;


{ Standard test }

procedure TTstDeleteTag_dEXIF.GenericTest(const ATagName: String);
var
  DUT: TImgData;
  msg: String;
  imgStream: TMemoryStream;
  jpeg: TJpegImage;
  bmp: TBitmap;
  TagAvailAfter: Boolean;
  TagAvailBefore: Boolean;
begin
  if ATagName = '' then
    Fail('No TagName specified.');
  if FSourceFileName = '' then
    Fail('No source image file specified.');

  // Copy source file to destination file to avoid destruction of source img.
  // Then work only with copied image.
  if FileExists(FTestFilename) then
    SysUtils.DeleteFile(FTestFileName);
  if FileExists(FSourceFilename) then
    CopyFile(FSourceFileName, FTestFileName)
  else
    Fail('Source "' + FSourceFilename + '" image cannot be found.');

  DUT := TImgData.Create;
  try
    DUT.ProcessFile(FTestFileName);
    TagAvailBefore := TagAvail(DUT, ATagName);

    // Delete tag
    DeleteTag(DUT, ATagName);

    // Check if requested tag is available
    TagAvailAfter := TagAvail(DUT, ATagName);
    CheckFalse(TagAvailAfter, 'Deleted tag "'+ATagName+'" found.');

    // Save to file
    DUT.WriteExifJpegTo(FTestFileName);
  finally
    DUT.Free;
  end;

  DUT := TImgData.Create;
  try
    DUT.PRocessFile(FTestFileName);
    TagAvailAfter := TagAvail(DUT, ATagName);
    CheckFalse(TagAvailAfter, 'Deleted tag "'+ATagName+'" found in saved file.');
  finally
    DUT.Free;
  end;

  {$IFDEF TEST_FILE_INTEGRITY}
  // -------------------------------------------------------------------------
  // Open the written file into a TJpegImage. It must open without an error.
  //--------------------------------------------------------------------------
  jpeg := TJpegImage.Create;
  bmp := TBitmap.Create;
  try
    try
      jpeg.LoadfromFile(FTestFileName);
      bmp.Assign(jpeg);
    except
      fail('Incorrectly written file "' + FTestFileName + '"');
    end;
  finally
    bmp.Free;
    jpeg.Free;
  end;
  {$ENDIF}
end;


{ Setting up the tests }

procedure TTstDeleteTag_dEXIF_01.SetUp;
begin
  { Test ...01 will operate on image co_DUTPicName01 }
  FSourcefileName := co_DeleteSrcPic_01;
  inherited SetUp;
end;

procedure TTstDeleteTag_dEXIF_02.SetUp;
begin
  { Test ...01 will operate on image co_DUTPicName01 }
  FSourcefileName := co_DeleteSrcPic_02;
  inherited SetUp;
end;

procedure TTstDeleteTag_dEXIF_03.SetUp;
begin
  { Test ...01 will operate on image co_DUTPicName01 }
  FSourcefileName := co_DeleteSrcPic_03;
  inherited SetUp;
end;


{ Artist }

procedure TTstDeleteTag_dEXIF_01.TstDelete_Artist;
begin
  GenericTest('Artist');
end;

procedure TTstDeleteTag_dEXIF_02.TstDelete_Artist;
begin
  GenericTest('Artist');
end;

procedure TTstDeleteTag_dEXIF_03.TstDelete_Artist;
begin
  GenericTest('Artist');
end;


{ UserComment }

procedure TTstDeleteTag_dEXIF_01.TstDelete_CommentEXIF;
begin
  GenericTest('UserComment');
end;

procedure TTstDeleteTag_dEXIF_02.TstDelete_CommentEXIF;
begin
  GenericTest('UserComment');
end;

procedure TTstDeleteTag_dEXIF_03.TstDelete_CommentEXIF;
begin
  GenericTest('UserComment');
end;


{ Comment segment }

procedure TTstDeleteTag_dEXIF_01.TstDelete_CommentSegment;
begin
  GenericTest('Comment');
end;

procedure TTstDeleteTag_dEXIF_02.TstDelete_CommentSegment;
begin
  GenericTest('Comment');
end;

procedure TTstDeleteTag_dEXIF_03.TstDelete_CommentSegment;
begin
  GenericTest('Comment');
end;


{ Image description }

procedure TTstDeleteTag_dEXIF_01.TstDelete_ImageDescription;
begin
  GenericTest('ImageDescription');
end;

procedure TTstDeleteTag_dEXIF_02.TstDelete_ImageDescription;
begin
  GenericTest('ImageDescription');
end;

procedure TTstDeleteTag_dEXIF_03.TstDelete_ImageDescription;
begin
  GenericTest('ImageDescription');
end;


{ GPS latitude }

procedure TTstDeleteTag_dEXIF_01.TstDelete_GPSLatitude;
begin
  GenericTest('GPSLatitude');
end;

procedure TTstDeleteTag_dEXIF_02.TstDelete_GPSLatitude;
begin
  GenericTest('GPSLatitude');
end;

procedure TTstDeleteTag_dEXIF_03.TstDelete_GPSLatitude;
begin
  GenericTest('GPSLatitude');
end;


{ FNumber }

procedure TTstDeleteTag_dEXIF_01.TstDelete_FNumber;
begin
  GenericTest('FNumber');
end;

procedure TTstDeleteTag_dEXIF_02.TstDelete_FNumber;
begin
  GenericTest('FNumber');
end;

procedure TTstDeleteTag_dEXIF_03.TstDelete_FNumber;
begin
  GenericTest('FNumber');
end;


initialization
  {$ifndef FPC}TestFramework.{$endif}RegisterTest(TTstDeleteTag_dEXIF_01{$ifndef FPC}.Suite{$endif});
  {$ifndef FPC}TestFramework.{$endif}RegisterTest(TTstDeleteTag_dEXIF_02{$ifndef FPC}.Suite{$endif});
  {$ifndef FPC}TestFramework.{$endif}RegisterTest(TTstDeleteTag_dEXIF_03{$ifndef FPC}.Suite{$endif});

end.

