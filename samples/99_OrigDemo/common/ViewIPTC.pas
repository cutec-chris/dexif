//--------------------------------------------------------------------------
// Test program: Demonstration of dEXIF used with an IPTC section
//
// Release history:
//   Gerry McGuire, September 3, 2001 - Initial Beta Release - 0.9
//
//--------------------------------------------------------------------------
// This program not only demonstrates the use of the dIPTC module but
// also why Delphi is a powerful windows programming tool.  It reads the
// available IPTC data and dynamically builds a custom entry panel based
// on this data.  In only a little more than 1 page of code!  This could
// easily be transformed into other formats: XML or HTML tables for example.
// This example includes using ScrollBox, size constraints, element
// resizing and dynamic control management.  Have Fun!
//--------------------------------------------------------------------------

unit ViewIPTC;

{$IFDEF FPC}
 {$MODE Delphi}
 {$DEFINE dExifNoJpeg}
{$ENDIF}

interface

uses
 {$IFDEF FPC}
  LCLIntf, LCLType, LCLProc, LMessages,
 {$ELSE}
  Windows, Messages,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ExtDlgs, ComCtrls,
  dMetadata, dEXIF, dIPTC, About;

const
  ProgName = 'IPTCView';
  SC_TransMenuItem = WM_USER + 1;

 type

  { TIPTCform }

  TIPTCform = class(TForm)
    StatusBar1: TStatusBar;
    pdlg: TOpenPictureDialog;
    Panel1: TPanel;
    Memo1: TMemo;
    btnAbout: TButton;
    btnLoad: TButton;
    Splitter1: TSplitter;
    ScrollBox1: TScrollBox;
    btnClose: TButton;
    btnTags: TButton;
    btnWrite: TButton;
    WriteDlg: TSavePictureDialog;
    btnXML: TButton;
    btnSetDT: TButton;
    lblDateTime: TLabel;
    procedure btnAboutClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSetDTClick(Sender: TObject);
    procedure btnTagsClick(Sender: TObject);
    procedure btnWriteClick(Sender: TObject);
    procedure btnXMLClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure Memo(AMsg: string);
    procedure AddControlSet(idx:integer; vName, vValue:string; MaxChars:integer);
    procedure CleanScrollBox;
    procedure LoadDisplayFromArray;
    procedure CopyDisplayToArray;
   {$IFDEF DELPHI}
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
   {$ENDIF}
  public
    Verbose:boolean;
  end;

var
  IPTCform: TIPTCform;
  ImgData: TImgData;

implementation

uses
 TagPickU;

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

// Shortcut to add memo data
procedure TIPTCForm.Memo(AMsg: string);
begin
  Memo1.Lines.Add(AMsg);
end;

// setup universal about box
procedure TIPTCform.btnAboutClick(Sender: TObject);
begin
  AboutBox.FormSetup(ProgName, dIPTCVersion);
  AboutBox.ShowModal;
end;

procedure TIPTCform.FormCreate(Sender: TObject);
var
  SysMenu: HMenu;
begin
  ImgData := TimgData.Create;

 {$IFDEF DELPHI}
  // Fun with Translations...
 {$IFDEF MSWINDOWS}
  SysMenu := GetSystemMenu(Handle, FALSE);
  AppendMenu(SysMenu, MF_SEPARATOR, 0, '');
  AppendMenu(SysMenu, MF_STRING, SC_TransMenuItem, 'Write Translation file');
  IPTCReadTransFile('transIn.Txt');
 {$ENDIF}
 {$ENDIF}
  Verbose := false;
  Constraints.MinWidth := Width;               // no smaller than
  Constraints.MinHeight := Height;             // initial size
  Panel1.Constraints.MinWidth := Panel1.Width; // set sizing limits
  Panel1.Constraints.MinHeight := Panel1.Height;

  //  wp --- removed for testing...
  //Panel1.DoubleBuffered := true;             // block that flicker
  //Memo1.DoubleBuffered := true;
  //ScrollBox1.DoubleBuffered := true;
end;

procedure TIPTCform.FormDestroy(Sender: TObject);
begin
  ImgData.Free;
end;

// Wipe all controls from scroll panel
procedure TIPTCform.CleanScrollBox;
var
  i: Integer;
  tc: TControl;
begin
  Memo1.Clear;
  for i := ScrollBox1.ControlCount-1 downto 0 do
  begin
    tc := ScrollBox1.Controls[i];
    tc.Parent := nil;
    tc.Free;
  end;
end;

procedure TIPTCform.AddControlSet(idx: integer; vName,vValue: String;
  MaxChars: integer);
var
  x: Integer;

begin
  { Create new label }
  with {$IFDEF FPC}TStaticText{$ELSE}TLabel{$ENDIF}.Create(Scrollbox1) do
  begin
    Parent := ScrollBox1;
    Alignment := taRightJustify;
    AutoSize := false;
    Top := 8 + 30*idx;
    Left := 8;
    Width := 200;
    Font.Style := [fsBold];
    Caption := vName;
    x := Left + Width + 8;
  end;

  { Create new edit box }
  with TEdit.Create(Scrollbox1) do
  begin
    Parent := ScrollBox1;
    Anchors := [akTop,akLeft, akRight]; // expand when window resized
    Top := 7 + 30*idx;
    Left := x;
    Tag := idx;
    MaxLength := MaxChars;
    Width := Scrollbox1.ClientWidth - Left - 16;
    Text := vValue;
  //  DoubleBuffered := true;             // Flicker-free dynamic resizing
  end;
end;

// Populate form
procedure TIPTCform.LoadDisplayFromArray;
var
  maxChars: Integer;
  lName, lValue: String;
  i: integer;
  dt: TDateTime;
begin
 {$IFDEF DELPHI}
  Scrollbox1.DisableAutoRange;    // Suspend repainting
 {$ENDIF}
  for i := 0 to ImgData.IptcObj.Count-1 do
  begin
    lName := ImgData.IptcObj[i].Desc;
    lValue := ImgData.IptcObj[i].Data;
    maxChars := ImgData.IptcObj[i].Size;
    if lName = 'Image caption'  then
    begin
      Memo(lValue);
      // scroll back to top - memo1.Lines.
      Memo1.CaretPos := Point(0, 0);
//      memo1.Perform(EM_LINESCROLL, 0, -memo1.Lines.Count);
    end;
    AddControlSet(i, lName, lValue, maxChars); // Add a label/edit box pair
  end;
  dt := ImgData.IptcObj.GetDateTime();
  if dt > 0 then
    if frac(dt) = 0 then
      lblDateTime.Caption := FormatDateTime('mmm-dd-yyyy', dt)
    else
      lblDateTime.Caption := FormatDateTime('mmm-dd-yyyy hh:nn:ss', dt);
 {$IFDEF DELPHI}
  Scrollbox1.EnableAutoRange;             // Enable control painting
 {$ENDIF}
end;

procedure TIPTCform.CopyDisplayToArray;
var
  tc: TEdit;
  i,insrt: Integer;
begin
  for i := 0 to Scrollbox1.ControlCount-1 do
    if Scrollbox1.Controls[i] is TEdit then
    begin
      tc := TEdit(Scrollbox1.Controls[i]);   // To save a search,
      insrt := tc.Tag;                       // control has array position
      ImgData.IptcObj.SetTagByIdx(insrt, tc.Text);   // stored in .tag field
     end;
end;

procedure TIPTCform.btnLoadClick(Sender: TObject);
begin
  if pdlg.Execute then
  begin
    StatusBar1.SimpleText := 'Info for '+pdlg.FileName;
    Memo1.Clear;
    CleanScrollBox();               // get rid of previous controls
    if imgData.ProcessFile(pdlg.FileName) and (ImgData.IptcObj <> nil) then
    begin
//    Added in 1.02a to demonstrate use of custom fields:
//      if these tags are found, then descriptions are altered,
//      otherwise these statements have no effect
      ImgData.IptcObj.UpdateTagDesc('Custom_230','Image Notes');
      ImgData.IptcObj.UpdateTagDesc('Custom_231','History');
      ImgData.IptcObj.UpdateTagDesc('Custom_232','Camera Information');
      LoadDisplayFromArray()        // create controls based on found tags
    end
    else
      Memo('Sorry: No IPTC info detected.');
    btnWrite.Enabled := true;
  end;
end;

procedure TIPTCform.btnCloseClick(Sender: TObject);
begin
  close;
end;

procedure TIPTCform.btnTagsClick(Sender: TObject);
begin
  if TagPickDlg.Showmodal = mrOK then
  begin
    CleanScrollBox();         // get rid of previous controls
    LoadDisplayFromArray();
  end;
end;

procedure TIPTCform.btnWriteClick(Sender: TObject);
begin
  if not WriteDlg.Execute then
    exit;
  CopyDisplayToArray();
  ImgData.WriteEXIFJpegTo(WriteDlg.FileName);

  (*
 {$IFNDEF dExifNoJpeg}
  if not WriteDlg.execute then
    exit;
  CopyDisplayToArray();              // Save form values
  ImgData.IptcObj.WriteFile(WriteDlg.FileName,pdlg.FileName);   // Write to jpeg file
 {$ELSE}
  ShowMessage('This function is not implemented.');
 {$ENDIF}
 *)
end;

procedure TIPTCform.btnXMLClick(Sender: TObject);
var
  xml: TStringList;
begin
  Memo1.Clear;
  xml := TStringList.Create;
  try
    ImgData.MetaDataToXML(xml);
    Memo1.Lines.AddStrings(xml);
  finally
    xml.Free;
  end;
end;

{$IFDEF DELPHI}
procedure TIPTCform.WMSysCommand(var Msg:TWMSysCommand);
begin
 if Msg.CmdType = SC_TransMenuItem then
 begin
   IPTCWriteTransFile('TransOut.txt');
   MessageBeep(0);
 end
 else
   inherited;
end;
{$ENDIF}

procedure TIPTCform.btnSetDTClick(Sender: TObject);
begin
  // current valid date/time tag prefixes are:
  //  Release, Expire, and Digitize
  ImgData.IptcObj.SetDateTimeExt(now, 'Release');
  CleanScrollBox;
  LoadDisplayFromArray;
end;

end.
