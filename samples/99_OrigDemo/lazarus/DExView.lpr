program DExView;

{$MODE Delphi}


{%File 'InkNames'}

uses
  Forms, Interfaces,
  MainView in '..\common\MainView.pas' {Form1},
  About in '..\common\About.pas' {AboutBox},
  dEXIF in 'dEXIF.pas',
  dIPTC in 'dIPTC.pas',
  msData in 'msData.pas';

{.$R *.RES}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
