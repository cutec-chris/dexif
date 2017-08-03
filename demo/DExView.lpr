program DExView;

{$MODE Delphi}



{%File 'InkNames'}

uses
  Forms, Interfaces,
  MainView in 'MainView.pas' {Form1},
  dEXIF in 'dEXIF.pas',
  About in 'About.pas' {AboutBox},
  dIPTC in 'dIPTC.pas',
  msData in 'msData.pas';

{.$R *.RES}
begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
