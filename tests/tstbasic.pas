unit tstBasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, FileUtil;

const
  // Picture with EXIF Data
  co_TestPic01 = './testpictures/original/with_exif_large.jpeg';
  co_DUTPicName01 = './testpictures/DUTPic01.jpeg';
  // Picture without data
  co_TestPic02 = './testpictures/original/no_exif.jpg';
  co_DUTPicName02 = './testpictures/DUTPic02.jpeg';

type

  { TTsTBasic_dEXIF }

  TTsTBasic_dEXIF= class(TTestCase)
  private
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckForPicture;
    procedure TstImgDataCreate;
    procedure TstImgDataCanProcessFile;
    procedure TstImgDataDetectEXIF_True;
    procedure TstImgDataDetectEXIF_False;
    procedure TstImgDataCoutEXIF;
  end;

implementation

uses
 dEXIF
 ,dIPTC;

procedure TTsTBasic_dEXIF.CheckForPicture;
begin
  CheckTrue(FileExists(co_DUTPicName01));
end;

procedure TTsTBasic_dEXIF.TstImgDataCreate;
var
  DUT: TImgData;
begin
  DUT:= TImgData.Create();
  CheckIs(DUT,TImgData,'Not TImgData');
  DUT.Free;
end;

procedure TTsTBasic_dEXIF.TstImgDataCanProcessFile;
var
  DUT: TImgData;
begin
  DUT:= TImgData.Create();
  CheckTrue(DUT.ProcessFile(co_DUTPicName01),'TImgData should process the file:'+co_DUTPicName01);
  CheckFalse(DUT.ProcessFile(co_DUTPicName02),'TImgData shold not process the file (as no information inside):'+co_DUTPicName02);
  DUT.Free;
end;

procedure TTsTBasic_dEXIF.TstImgDataDetectEXIF_True;
var
  DUT: TImgData;
begin
  DUT:= TImgData.Create();
  DUT.ProcessFile(co_DUTPicName01);
  CheckTrue(DUT.HasEXIF,'TImgData cannot detect EXIF in file:'+co_DUTPicName01);
  DUT.ClearEXIF;
  CheckFalse(DUT.HasEXIF,'TImgData cannot reset EXIF');
  DUT.Free;
end;

procedure TTsTBasic_dEXIF.TstImgDataDetectEXIF_False;
var
  DUT: TImgData;
begin
  DUT:= TImgData.Create();
  DUT.ProcessFile(co_DUTPicName02);
  CheckFalse(DUT.HasEXIF,'TImgData cannot detect EXIF in file:'+co_DUTPicName02);
  DUT.Free;
end;

procedure TTsTBasic_dEXIF.TstImgDataCoutEXIF;
var
  DUT: TImgData;
  cnt: integer;
  dummy: TTagEntry;
begin
  //ImgData.ExifObj.ResetIterator;
  //while ImgData.ExifObj.IterateFoundTags(GenericEXIF ,item) do
  //  Memo(item.Desc+DexifDelim+item.Data);
  DUT:= TImgData.Create();
  DUT.ProcessFile(co_DUTPicName01);
  CheckTrue(DUT.HasEXIF,'TImgData cannot detect EXIF in file:'+co_DUTPicName01);
  cnt:= 0;
  DUT.ExifObj.ResetIterator;
  // Ceck for 5 Tags
  while DUT.ExifObj.IterateFoundTags(GenericEXIF ,dummy) do
    inc(cnt);
  CheckEquals(5,cnt,'TImgData expected 5 but '+IntToStr(cnt)+' Items found');
  DUT.Free;
end;

procedure TTsTBasic_dEXIF.SetUp;
begin
  if not FileExists(co_DUTPicName01) then
    if FileExists(co_TestPic01) then
      CopyFile(co_TestPic01,co_DUTPicName01);
  if not FileExists(co_DUTPicName02) then
    if FileExists(co_TestPic02) then
      CopyFile(co_TestPic02,co_DUTPicName02);
end;

procedure TTsTBasic_dEXIF.TearDown;
begin
  //if FileExists(co_DUTPicName01) then
  //  DeleteFile(co_DUTPicName01);
  //if FileExists(co_DUTPicName02) then
  //  DeleteFile(co_DUTPicName02);
end;

initialization

  RegisterTest(TTsTBasic_dEXIF);
end.

