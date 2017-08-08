program TestGUIdEXIF;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, tstBasic, tstreadexif, tstselfimage;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

