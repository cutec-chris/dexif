program IPTCView;

uses
  Forms,
  ViewIPTC in '..\common\ViewIPTC.pas' {IPTCform},
  TagPickU in '..\common\TagPickU.pas' {TagPickDlg},
  About in '..\common\About.pas' {AboutBox},
  dEXIF in '..\..\..\dEXIF.pas',
  dExifWrite in '..\..\..\dExifWrite.pas',
  dGlobal in '..\..\..\dGlobal.pas',
  dIPTC in '..\..\..\dIPTC.pas',
  dTags in '..\..\..\dTags.pas',
  dUtils in '..\..\..\dUtils.pas',
  msData in '..\..\..\msData.pas',
  diptcwrite in '..\..\..\diptcwrite.pas',
  dmetadata in '..\..\..\dmetadata.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TIPTCform, IPTCform);
  Application.CreateForm(TTagPickDlg, TagPickDlg);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
