unit tstBasic;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif FPC}

{$I dExifTest.inc}


interface

uses
  Classes, SysUtils
{$ifdef FPC}
   , fpcunit, testutils, testregistry, FileUtil
{$else}
   , TestFrameWork
{$endif}
   ;

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
{$ifdef FPC}
  protected
{$else}
  public
{$endif}
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckForPicture;
    procedure TstImgDataCreate;
    procedure TstImgDataCanProcessFile;
    procedure TstImgDataDetectEXIF_True;
    procedure TstImgDataDetectEXIF_False;
    procedure TstGpsFormat;
  end;

implementation

uses
  dGlobal, dUtils, dMetadata, dEXIF
{$ifdef FPC}
{$else}
  , {$ifndef DELPHI7}Winapi.Windows{$else}Windows{$endif}
{$endif}
  ;


procedure TTsTBasic_dEXIF.CheckForPicture;
begin
  CheckTrue(FileExists(co_DUTPicName01), 'Test picture file does not exit');
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
  CheckTrue(DUT.ProcessFile(co_DUTPicName01),'TImgData cannot process file:'+co_DUTPicName01);
  CheckFALSE(DUT.ProcessFile(co_DUTPicName02),'TImgData cannot process file:'+co_DUTPicName02);
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
  DUT.ClearEXIF;
  CheckFalse(DUT.HasEXIF,'TImgData cannot reset EXIF');
  DUT.Free;
end;

procedure TTstBasic_dEXIF.TstGPSFormat;
const
  Expected: array[0..1] of Extended = (
     12.345678,      //  12° 20' 44.44404"   =  12° 20.7407441'
    -23.456789       // -23° 27' 24.440436"  = -23° 27.4073406'
  );
  NSEW: array[TGpsCoordType] of String = ('NS', 'EW');
var
  gf: TGpsFormat;
  ct: TGpsCoordType;
  expStr: String;
  currstr: String;
  current: Extended;
  i, j: Integer;
  decs: Integer;
begin
  // Format degree-minutes-seconds
  gf := gf_DMS_Short;
  decs := 3;
  for ct := Low(TGpsCoordtype) to High(TGpsCoordType) do begin
    for i:=0 to High(Expected) do begin
      expStr := GpsToStr(Expected[i], ct, gf, decs);
      current := StrToGps(expStr);
      currStr := GpsToStr(current, ct, gf, decs);
      CheckEquals(expstr, currstr, 'GPS mismatch (' + expstr + ')');
    end;
  end;

  // Format degree-minutes
  gf := gf_DM_Short;
  decs := 9;
  for ct := Low(TGpsCoordtype) to High(TGpsCoordType) do begin
    for i:=0 to High(Expected) do begin
      expStr := GpsToStr(Expected[i], ct, gf, decs);
      current := StrToGps(expStr);
      currStr := GpsToStr(current, ct, gf, decs);
      CheckEquals(expstr, currstr, 'GPS mismatch (' + expstr + ')');
    end;
  end;
end;

procedure TTsTBasic_dEXIF.SetUp;
{$ifndef FPC}
  function CopyFile(f1,f2:string):boolean;
  begin
    Result:=  {$ifndef DELPHI7}Winapi.{$endif}Windows.CopyFile(PChar(f1),PChar(f2),true);
  end;
{$endif}
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
  {$ifndef FPC}TestFramework.{$endif}RegisterTest(TTsTBasic_dEXIF{$ifndef FPC}.Suite{$endif});
end.

