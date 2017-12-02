program TestDelphi7dEXIF;

uses
  TestFramework,
  Forms,
  GUITestRunner,
  TextTestRunner,
  dEXIF in '..\dEXIF.pas',
  dexifwrite in '..\dexifwrite.pas',
  dglobal in '..\dglobal.pas',
  dIPTC in '..\dIPTC.pas',
  dtags in '..\dtags.pas',
  dUtils in '..\dUtils.pas',
  msData in '..\msData.pas',
  tstbasic in 'tstbasic.pas',
  tstreadexif in 'tstreadexif.pas',
  tstselfimage in 'tstselfimage.pas',
  tstwritereadexif in 'tstwritereadexif.pas',
  tstDeleteExif in 'tstdeleteexif.pas',
  dIptcWrite in '..\diptcwrite.pas',
  dMetadata in '..\dmetadata.pas';

{$R *.RES}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;

end.


