program TestDelphidEXIF;

uses
  TestFramework,
  Forms,
  GUITestRunner,
  TextTestRunner,
  tstbasic in 'tstbasic.pas' {/  tstreadexif in 'tstreadexif.pas',},
  dEXIF in '..\dEXIF.pas',
  dexifwrite in '..\dexifwrite.pas',
  dglobal in '..\dglobal.pas',
  dIPTC in '..\dIPTC.pas',
  dtags in '..\dtags.pas',
  dUtils in '..\dUtils.pas',
  msData in '..\msData.pas',
  tstreadexif in 'tstreadexif.pas',
  tstselfimage in 'tstselfimage.pas',
  tstwritereadexif in 'tstwritereadexif.pas',
  tstdeleteexif in 'tstdeleteexif.pas',
  diptcwrite in '..\diptcwrite.pas',
  dmetadata in '..\dmetadata.pas';

//  tstreadexif in 'tstreadexif.pas',
//  tstselfimage in 'tstselfimage.pas',
//  tstthumbnail in 'tstthumbnail.pas',
//  tstwritereadexif in 'tstwritereadexif.pas';

{$R *.RES}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;

end.


