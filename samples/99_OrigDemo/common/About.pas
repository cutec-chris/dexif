//--------------------------------------------------------------------------
// dEXIF mult-app about box.  Call FormSetup prior to
// display to setup app name and version.
//
// Release history:
//   Gerry McGuire, September 3, 2001 - Initial Beta Release - 0.9
//                  October 23, 2001 - Add Writing Routines
//
//--------------------------------------------------------------------------
unit About;

{$IFDEF FPC}
 {$MODE Delphi}
{$ENDIF}

interface

uses
 {$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
 {$ELSE}
  Windows, ShellAPI,
 {$ENDIF}
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    OKButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure Label3Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure FormSetup(pName, pVersion: string);
  end;

var
  AboutBox: TAboutBox;

implementation

uses dEXIF;

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

  ////////////////////
  //
  //   A couple of utility functions that are handy
  //   in an About box module...
  //
  Procedure SendEMail(instr: string);
  var loc:string;
  begin
    if instr <> '' then begin
      loc := 'mailto:' + trim(instr);
      {$IFDEF FPC}
      OpenDocument(loc)
      {$ELSE}
      ShellExecute(0, 'open', pChar(loc), '', '', sw_ShowNormal);
      {$ENDIF}
    end;
  end;

  Procedure BrowseURL(instr: string);
  begin
    if trim(instr) <> '' then
      {$IFDEF FPC}
      OpenDocument(instr)
      {$ELSE}
      ShellExecute(0, 'open', pChar(instr), '', '', sw_ShowNormal)
      {$ENDIF}
  end;

procedure TAboutBox.FormSetup(pName,pVersion:string);
begin
  Version.Caption := 'Version:  '+pVersion;
  ProductName.Caption := 'Product:  '+pName;
end;

procedure TAboutBox.Label3Click(Sender: TObject);
begin
  BrowseUrl('http://'+tlabel(sender).caption);
end;

procedure TAboutBox.Label4Click(Sender: TObject);
begin
  SendEMail(tlabel(sender).caption);
end;

end.

