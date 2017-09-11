program DExView;

uses
  Forms,
  MainView in '..\common\MainView.pas' {Form1},
  dEXIF in '..\..\..\dEXIF.pas',
  dIPTC in '..\..\..\dIPTC.pas',
  msData in '..\..\..\msData.pas',
  dExifWrite in '..\..\..\dExifWrite.pas',
  dGlobal in '..\..\..\dGlobal.pas',
  dTags in '..\..\..\dTags.pas',
  dUtils in '..\..\..\dUtils.pas',
  About in '..\common\About.pas' {AboutBox},
  diptcwrite in '..\..\..\diptcwrite.pas',
  dmetadata in '..\..\..\dmetadata.pas';

{$R *.RES}
begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
