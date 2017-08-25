program TestGUIdEXIF;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, dEXIF, dIPTC, msData, dUtils,
  tstBasic, tstreadexif, tstwritereadexif, tstthumbnail, tstselfimage;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

