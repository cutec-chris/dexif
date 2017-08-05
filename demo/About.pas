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

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes, Graphics, Forms, Controls,
  StdCtrls, Buttons, ExtCtrls;

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

{$IFDEF DELPHI}
 {$R *.dfm}
{$ENDIF}
{$IFDEF LCL}
 {$R *.lfm}
{$ENDIF}


  ////////////////////
  //
  //   A couple of utility functions that are handy
  //   in an About box module...
  //
  Procedure SendEMail(instr: string );
  var loc:string;
  begin
    loc := 'mailto:'+trim(instr);
    if loc <> 'mailto:' then
       OpenDocument(pchar(loc))
  end;

  Procedure BrowseURL( instr:string );
  begin
    if trim(instr) <> '' then
       OpenDocument(pchar(instr))
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

