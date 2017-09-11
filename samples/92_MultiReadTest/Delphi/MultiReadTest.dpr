program MultiReadTest;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

uses
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF }
  Forms,
  mrtmain in '..\common\mrtmain.pas',
  dEXIF in '..\..\..\dEXIF.pas',
  dexifwrite in '..\..\..\dexifwrite.pas',
  dglobal in '..\..\..\dglobal.pas',
  dIPTC in '..\..\..\dIPTC.pas',
  dtags in '..\..\..\dtags.pas',
  dUtils in '..\..\..\dUtils.pas',
  msData in '..\..\..\msData.pas',
  diptcwrite in '..\..\..\diptcwrite.pas',
  dmetadata in '..\..\..\dmetadata.pas';

{$R *.res}

begin
{$IFDEF FPC}
  RequireDerivedFormResource := True;
{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

