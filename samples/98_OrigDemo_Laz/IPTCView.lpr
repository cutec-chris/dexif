program IPTCView;

uses
  Forms, Interfaces,
  ViewIPTC in 'ViewIPTC.pas' {IPTCform},
  About in 'About.pas' {AboutBox},
  dEXIF in 'dEXIF.pas',
  dIPTC in 'dIPTC.pas',
  msData in 'msData.pas',
  TagPickU in 'TagPickU.pas' {TagPickDlg};

{.$R *.RES}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TIPTCform, IPTCform);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TTagPickDlg, TagPickDlg);
  Application.Run;
end.
