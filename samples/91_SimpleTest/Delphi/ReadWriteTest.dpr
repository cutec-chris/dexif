program ReadWriteTest;

uses
  Forms,
  rwMain in '..\common\rwMain.pas',
  dEXIF in '..\..\..\dEXIF.pas',
  dExifWrite in '..\..\..\dExifWrite.pas',
  dGlobal in '..\..\..\dGlobal.pas',
  dIPTC in '..\..\..\dIPTC.pas',
  dTags in '..\..\..\dTags.pas',
  dUtils in '..\..\..\dUtils.pas',
  msData in '..\..\..\msData.pas',
  diptcwrite in '..\..\..\diptcwrite.pas',
  dmetadata in '..\..\..\dmetadata.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  MainForm.BeforeRun;
  Application.Run;
end.

