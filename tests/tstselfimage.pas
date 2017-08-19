unit tstselfimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

const
  co_DUTPicSelfImage01 = './testpictures/DUTPicSelfImage01.jpg';

type

  { TTstSelfImage }

  TTstSelfImage= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckForNoPicture;
    procedure CreateImageJpg;
    procedure CreateImageJpgWithEXIF;
  end;

implementation
uses
  Graphics
  , dExif;

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
  CheckFalse(FileExists(co_DUTPicSelfImage01),'Internal error: The file is prsent (should not !!!!)');
end;

procedure CreateGeenJpg(aFilename: String);
const
  co_BMPWidht = 400;
  co_BMPHeigh = 250;
var
  bmp : TBitmap;
  jpg : TJPEGImage;
begin
  // Bitmap in green
   bmp:=TBitmap.Create;
   jpg:= TJPEGImage.Create;
   try
     bmp.PixelFormat:=pf32bit;
     bmp.SetSize(co_BMPWidht, co_BMPHeigh);
     bmp.Canvas.Brush.Color:=clgreen;
     bmp.Canvas.FillRect(0, 0, co_BMPWidht, co_BMPHeigh);
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
  CreateGeenJpg(co_DUTPicSelfImage01);
  CheckTrue(FileExists(co_DUTPicSelfImage01),'Internal error: File:'+ co_DUTPicSelfImage01+' is missing');
  DUT:= TImgData.Create();
  // Lazarus gerated Files have no EXIF information
  CheckFalse(DUT.ProcessFile(co_DUTPicSelfImage01),'TImgData cannot process file:'+co_DUTPicSelfImage01);
  DUT.Free;
end;

procedure TTstSelfImage.CreateImageJpgWithEXIF;
var
  DUT: TImgData;
begin
  CreateGeenJpg(co_DUTPicSelfImage01);
  CheckTrue(FileExists(co_DUTPicSelfImage01),'Internal error: File:'+ co_DUTPicSelfImage01+' is missing');
  DUT:= TImgData.Create(GenAll);
  // Lazarus gerated Files have no EXIF information
  CheckFalse(DUT.ProcessFile(co_DUTPicSelfImage01),'TImgData can process file, but shouldnt:'+co_DUTPicSelfImage01);
  CheckFalse(DUT.HasEXIF,'TImgData should not have EXIF'+co_DUTPicSelfImage01);
  if not DUT.HasEXIF then begin
    // write a file with an empty exif
    DUT.WriteEXIFJpeg(co_DUTPicSelfImage01);
    FreeAndNil(DUT);
    // Reread the file
    DUT:= TImgData.Create(GenAll);
    CheckTrue(DUT.ProcessFile(co_DUTPicSelfImage01),'TImgData cannot process file:'+co_DUTPicSelfImage01);
    CheckTrue(DUT.HasEXIF,'TImgData should have EXIF now '+co_DUTPicSelfImage01);
  end;
  DUT.Free;
end;

initialization
  RegisterTest(TTstSelfImage);

end.

