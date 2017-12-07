unit tstselfimage;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif FPC}

{$i dExifTest.inc}

interface

uses
  Classes, SysUtils
  {$ifdef FPC}
     , fpcunit, testutils, testregistry
  {$else}
     , TestFrameWork
  {$endif}
     ;

const
  co_DUTPicSelfImage01 = './testpictures/DUTPicSelfImage01.jpg';

type

  { TTstSelfImage }

  TTstSelfImage= class(TTestCase)
  {$ifdef FPC}
    protected
  {$else}
    public
  {$endif}
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckForNoPicture;
    procedure CreateImageJpg;
    procedure CreateImageJpgWithoutEXIF;
    procedure CreateImageJpgWithEmptyEXIF;
    procedure CreateImageJpgWithMiniEXIF;
  end;

implementation
uses
  Graphics, Variants,
  dMetadata, dExif
{$ifdef FPC}
{$else}
  , {$ifndef DELPHI7}Winapi.Windows{$else}Windows{$endif}
  , jpeg
{$endif}
  ;


procedure TTstSelfImage.SetUp;
begin
  if FileExists(co_DUTPicSelfImage01) then
    DeleteFile(co_DUTPicSelfImage01);
end;

procedure TTstSelfImage.TearDown;
begin
  // nothing to do yet
end;

procedure TTstSelfImage.CheckForNoPicture;
begin
  CheckFalse(FileExists(co_DUTPicSelfImage01),'Internal error: The file is present (should not !!!!)');
end;

procedure CreateGreenJpg(aFilename: String);
const
  co_BMPWidht = 400;
  co_BMPHeigh = 250;
var
  bmp : {$ifndef FPC}Graphics.{$endif}TBitmap;
  jpg : TJPEGImage;
begin
  // Bitmap in green
   bmp:= {$ifndef FPC}Graphics.{$endif}TBitmap.Create;
   jpg:= TJPEGImage.Create;
   try
     bmp.PixelFormat:=pf32bit;
   {$ifndef DELPHI7}
     bmp.SetSize(co_BMPWidht, co_BMPHeigh);
   {$else}
     bmp.Height:= co_BMPHeigh;
     bmp.Width:= co_BMPWidht;
   {$endif}
     bmp.Canvas.Brush.Color:=clgreen;
  {$ifdef FPC}
     bmp.Canvas.FillRect(0, 0, co_BMPWidht, co_BMPHeigh);
   {$else}
     {$ifndef DELPHI7}
     bmp.Canvas.FloodFill(0, 0, clgreen, TFillStyle.fsSurface);
     {$else}
     bmp.Canvas.FloodFill(0,0, clGreen, fsSurface);
     {$endif}
   {$endif}
     jpg.Assign(bmp);
     jpg.SaveToFile(aFilename);
   finally
     bmp.Free;
     jpg.free;
   end;

end;

procedure TTstSelfImage.CreateImageJpg;
var
  DUT: TImgData;
begin
  CreateGreenJpg(co_DUTPicSelfImage01);
  CheckTrue(FileExists(co_DUTPicSelfImage01),'Internal error: File:'+ co_DUTPicSelfImage01+' is missing');
  DUT:= TImgData.Create();
  // Lazarus gerated Files have no EXIF information
  CheckFalse(DUT.ProcessFile(co_DUTPicSelfImage01),'TImgData cannot process file:'+co_DUTPicSelfImage01);
  DUT.Free;
end;

procedure TTstSelfImage.CreateImageJpgWithoutEXIF;
var
  DUT: TImgData;
begin
  CreateGreenJpg(co_DUTPicSelfImage01);
  CheckTrue(FileExists(co_DUTPicSelfImage01),'Internal error: File:'+ co_DUTPicSelfImage01+' is missing');
  DUT:= TImgData.Create(GenAll);
  // Lazarus generated files have no EXIF information
  CheckFalse(DUT.ProcessFile(co_DUTPicSelfImage01),'TImgData can process file, but shouldnt:'+co_DUTPicSelfImage01);
  CheckFalse(DUT.HasEXIF,'TImgData should not have EXIF'+co_DUTPicSelfImage01);
  if not DUT.HasEXIF then begin
    // write a file with an empty exif
    DUT.WriteEXIFJpegTo(co_DUTPicSelfImage01);
    FreeAndNil(DUT);
    // Reread the file
    DUT:= TImgData.Create(GenAll);
    DUT.ProcessFile(co_DUTPicSelfImage01);
    CheckFalse(DUT.HasEXIF, 'TImgData still should not have an EXIF: '+co_DUTPicSelfImage01);
  end;
  DUT.Free;
end;

procedure TTstSelfImage.CreateImageJpgWithEmptyEXIF;
var
  DUT: TImgData;
begin
  CreateGreenJpg(co_DUTPicSelfImage01);
  CheckTrue(FileExists(co_DUTPicSelfImage01),'Internal error: File:'+ co_DUTPicSelfImage01+' is missing');
  DUT:= TImgData.Create(GenAll);
  // Lazarus generated files have no EXIF information
  CheckFalse(DUT.ProcessFile(co_DUTPicSelfImage01),'TImgData can process file, but shouldnt:'+co_DUTPicSelfImage01);
  CheckFalse(DUT.HasEXIF,'TImgData should not have EXIF'+co_DUTPicSelfImage01);
  if not DUT.HasEXIF then begin
    DUT.CreateExifObj;  // Create a new EXIF object, but don't add any tags.
    // Write file with the empty exif
    DUT.WriteEXIFJpegTo(co_DUTPicSelfImage01);
    FreeAndNil(DUT);
    // Reread the file
    DUT:= TImgData.Create(GenAll);
    DUT.ProcessFile(co_DUTPicSelfImage01);
    CheckTrue(DUT.HasEXIF,'TImgData should have an EXIF now: '+co_DUTPicSelfImage01);
  end;
  DUT.Free;
end;

procedure TTstSelfImage.CreateImageJpgWithMiniEXIF;
const
  EXPECTED_ARTIST = 'dExif';
  EXPECTED_WIDTH = 1000;
  EXPECTED_EXPOSURETIME = 1/100;
  EPS = 1E-8;
var
  DUT: TImgData;
  s: String;
  v: variant;
  imgStream: TMemoryStream;
begin
  CreateGreenJpg(co_DUTPicSelfImage01);
  CheckTrue(FileExists(co_DUTPicSelfImage01),'Internal error: File:'+ co_DUTPicSelfImage01+' is missing');

  DUT:= TImgData.Create(GenAll);
  with DUT.CreateEXIFObj do begin
    CheckTrue(DUT.HasExif, 'TImgData should have an EXIF: ' + co_DUTPicSelfImage01);

    TagValue['Artist'] := EXPECTED_ARTIST;
    s := TagValueAsString['Artist'];
    CheckEquals(EXPECTED_ARTIST, s, 'Tag "Artist" mismatch');

    TagValue['ImageWidth'] := EXPECTED_WIDTH;
    v := TagValue['ImageWidth'];
    CheckFalse(VarIsNull(v), 'Tag "ImageWidth" not found.');
    CheckEquals(EXPECTED_WIDTH, Integer(v), 'Tag "ImageWidth" mismatch');

    TagValue['ExposureTime'] := EXPECTED_EXPOSURETIME;
    v := TagValue['ExposureTime'];
    CheckFalse(VarIsNull(v), 'Tag "ExposureTime" not found.');
    CheckEquals(EXPECTED_EXPOSURETIME, Double(v), EPS, 'Tag "ExposureTime" mismatch');
  end;

  // Merge these exif data with the image file
  imgStream := TMemorystream.Create;
  try
    imgStream.LoadFromFile(co_DUTPicSelfImage01);
    DUT.WriteEXIFJpeg(imgStream, co_DUTPicSelfImage01, false);
  finally
    imgStream.Free;
  end;
  FreeAndNil(DUT);

  DUT := TImgData.Create;
  CheckTrue(DUT.ProcessFile(co_DUTPicSelfImage01),'TImgData cannot process file:'+co_DUTPicSelfImage01);
  CheckTrue(DUT.HasEXIF,'TImgData should have EXIF now '+co_DUTPicSelfImage01);
  s := DUT.ExifObj.TagValueAsString['Artist'];
  CheckEquals(EXPECTED_ARTIST, s, 'Tag "Artist" mismatch');
  v := DUT.ExifObj.TagValue['ImageWidth'];
  CheckFalse(VarIsNull(v), 'Tag "ImageWidth" not found.');
  CheckEquals(EXPECTED_WIDTH, Integer(v), 'Tag "ImageWidth" mismatch');
  v := DUT.ExifObj.TagValue['ExposureTime'];
  CheckFalse(VarIsNull(v), 'Tag "ExposureTime" not found.');
  CheckEquals(EXPECTED_EXPOSURETIME, Double(v), EPS, 'Tag "ExposureTime" mismatch');
  DUT.Free;
end;


initialization
  {$ifndef FPC}TestFramework.{$endif}RegisterTest(TTstSelfImage{$ifndef FPC}.Suite{$endif});

end.

