unit uCreateEXIF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  dMetadata, dExif;

type

  { TForm1 }

  TForm1 = class(TForm)
    BuCreateImage1: TButton;
    BuCreateImageAndSave: TButton;
    StaticText1: TStaticText;
    procedure BuCreateImageAndSaveClick(Sender: TObject);
    procedure BuCreateImageClick(Sender: TObject);
  private
    procedure CreateEXIF(const ImgData: TImgData);
    procedure ExifToStaticText(AImgData: TImgData; ATitle: String);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure CreateGreenJpg(aFilename: String);
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


{ TForm1 }
const
  co_ImageName = 'dummy.jpg';

procedure TForm1.BuCreateImageAndSaveClick(Sender: TObject);
var
  ImgData : TImgData;
begin
  if FileExists(co_ImageName) then
    DeleteFile(co_ImageName);
  CreateGreenJpg(co_ImageName);
  ImgData:= TImgData.Create(GenAll);
  try
    CreateEXIF(ImgData);

    imgData.WriteExifJpeg(co_ImageName);

    ImgData.Reset;
    ImgData.ProcessFile(co_ImageName);
    if ImgData.HasExif then
      ExifToStaticText(ImgData, 'Exif saved')
    else
      StaticText1.Caption := 'No EXIF found - something is wrong...';
  finally
    ImgData.Free;
  end;
end;

procedure TForm1.BuCreateImageClick(Sender: TObject);
var
  ImgData : TImgData;
begin
  if FileExists(co_ImageName) then
    DeleteFile(co_ImageName);
  CreateGreenJpg(co_ImageName);
  ImgData:= TImgData.Create(GenAll);
  try
    CreateEXIF(ImgData);
    if ImgData.HasExif then
      ExifToStaticText(ImgData, 'Exif not saved')
    else
      StaticText1.Caption := 'No EXIF found - something is wrong...';
  finally
    ImgData.Free;
  end;
end;

procedure TForm1.CreateEXIF(const ImgData: TImgData);
var
  dt: TDateTime;
begin
  dt := Now;
  ImgData.CreateExifObj;
  with ImgData.ExifObj do begin
    CameraMake := 'Freepascal';
    CameraModel := 'Lazarus';
    DateTimeOriginal := dt;
    DateTimeDigitized := dt;
    DateTimeModified := dt;
    Artist := 'af0815';
    ImageDescription := 'This is the description of the image';
    ExifComment := 'Ätsch!';
    TagValue['Orientation'] := 1;
    TagValue['FocalLength'] := 300;
    TagValue['FNumber'] := 2.8;
  end;
  ImgData.Comment := 'This is in the COMMENT segment';
end;

procedure TForm1.ExifToStaticText(AImgData: TImgData; ATitle: String);
var
  s: String;
begin
  s := ATitle + LineEnding + LineEnding +
    AImgData.ExifObj.ToLongString(0) + LineEnding +
    'Orientation: ' + AImgData.ExifObj.TagValueAsString['Orientation'] + LineEnding +
    'Artist: ' + AImgData.ExifObj.Artist + LineEnding +
    'Exif comment: ' + AImgData.ExifObj.ExifComment + LineEnding +
    'Image description: ' + AImgData.ExifObj.ImageDescription;
  if AImgData.Comment <> '' then
    s := s + LineEnding + LineEnding + AImgData.Comment;
  StaticText1.Caption := s;
end;

end.

