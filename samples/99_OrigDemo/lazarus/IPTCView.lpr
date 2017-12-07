program IPTCView;

uses
  Forms, Interfaces,
  ViewIPTC in '../common/ViewIPTC.pas' {IPTCform},
  About in '../common/About.pas' {AboutBox},
  TagPickU in '../common/TagPickU.pas' {TagPickDlg},
  dEXIF in 'dEXIF.pas',
  dIPTC in 'dIPTC.pas',
  msData in 'msData.pas';
  
{.$R *.RES}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TIPTCform, IPTCform);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TTagPickDlg, TagPickDlg);
  Application.Run;
end.
