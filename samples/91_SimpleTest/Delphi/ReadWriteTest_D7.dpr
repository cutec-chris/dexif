program ReadWriteTest_D7;

uses
  Forms,
  rwMain in '..\common\rwMain.pas',
  dEXIF in '..\..\..\dEXIF.pas',
  dexifwrite in '..\..\..\dExifWrite.pas',
  dGlobal in '..\..\..\dGlobal.pas',
  dIPTC in '..\..\..\dIPTC.pas',
  dTags in '..\..\..\dTags.pas',
  dUtils in '..\..\..\dUtils.pas',
  msData in '..\..\..\msData.pas',
  dMetadata in '..\..\..\dmetadata.pas',
  dIptcWrite in '..\..\..\diptcwrite.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  MainForm.BeforeRun;
  Application.Run;
end.

