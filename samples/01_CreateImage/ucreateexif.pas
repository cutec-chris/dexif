unit uCreateEXIF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  dExif;

type

  { TForm1 }

  TForm1 = class(TForm)
    BuCreateImage: TButton;
    StaticText1: TStaticText;
    procedure BuCreateImageClick(Sender: TObject);
  private
    procedure CreateEXIF(const ImgData: TImgData);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

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

{ TForm1 }
const
  co_ImageName = 'dummy.jpg';

procedure TForm1.BuCreateImageClick(Sender: TObject);
var
  ImgData : TImgData;
begin
  if FileExists(co_ImageName) then
    DeleteFile(co_ImageName);
  CreateGeenJpg(co_ImageName);
  ImgData:= TImgData.Create(GenAll);
  ImgData.ProcessFile(co_ImageName);
  if not ImgData.HasEXIF then begin
    // write a file with an empty exif
    ImgData.WriteEXIFJpeg(co_ImageName);
    // Reread the file
    ImgData.reset;
    ImgData.ProcessFile(co_ImageName);
  end;
  if not ImgData.HasEXIF then
    showmessage('somethings goes wrong');
end;

procedure TForm1.CreateEXIF(const ImgData: TImgData);
var
  ImgInfo: TImageInfo;
  Source: TImageInfo;
begin
  ImgInfo:= TImageInfo.Create(ImgData,GenAll);
  with ImgInfo do begin
    CameraMake      := 'Freepascal';
    CameraModel     := 'Lazarus';
    DateTime        := '';
    Height          := ImgData.Height;
    Width           := ImgData.Width;
    FlashUsed       := 0;
    Comments        := '';
    MakerNote       := '';
    TraceStr        := '';
    msTraceStr      := '';
    msAvailable     := False;
    msName          := '';
  end;
  if (ImgData.ExifObj = ImgInfo) then
    ShowMessage('same object');
  ImgData.ExifObj:= ImgInfo;
end;


end.

