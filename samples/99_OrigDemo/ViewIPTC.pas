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
  {$IFDEF MSWINDOWS} Windows, Messages,
  {$ELSE}
   LCLIntf, LCLType, LCLProc, LMessages,
  {$ENDIF}
 {$ELSE}
  Windows, Messages,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ExtDlgs, ComCtrls, dEXIF, dIPTC, About;

const
  ProgName = 'IPTCView';
  SC_TransMenuItem = WM_USER + 1;

 type
  TIPTCform = class(TForm)
    StatusBar1: TStatusBar;
    pdlg: TOpenPictureDialog;
    Panel1: TPanel;
    Memo1: TMemo;
    btnAbout: TButton;
    btnLoad: TButton;
    Splitter1: TSplitter;
    ScrollBox1: TScrollBox;
    Button1: TButton;
    btnTags: TButton;
    btnWrite: TButton;
    WriteDlg: TSavePictureDialog;
    Button2: TButton;
    btnSetDT: TButton;
    lblDateTime: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnTagsClick(Sender: TObject);
    procedure btnWriteClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnSetDTClick(Sender: TObject);
  private
    procedure Memo(s: string);
    procedure AddControlSet(idx:integer; vName,vValue:string;
        MaxChars:integer);
    procedure CleanScrollBox;
    procedure LoadDisplayFromArray;
    procedure CopyDisplayToArray;
    procedure WMSysCommand(var Msg: TWMSysCommand); message wm_syscommand;
    { Private declarations }
  public
    { Public declarations }
    verbose:boolean;
  end;

var
  IPTCform: TIPTCform;
  ImgData:TImgData;

implementation

uses TagPickU;

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

procedure TIPTCform.Memo(s:string);
begin
  Memo1.Lines.Add(s);                           // Shortcut to add memo data
end;

procedure TIPTCform.btnAboutClick(Sender: TObject);
begin
  AboutBox.FormSetup(ProgName,dIPTCVersion);    // setup universal about box
  AboutBox.ShowModal;
end;

procedure TIPTCform.FormCreate(Sender: TObject);
var SysMenu : HMenu;
begin
   ImgData := TimgData.Create;

   // Fun with Translations...
   {$IFDEF MSWINDOWS}
   SysMenu := GetSystemMenu(Handle, FALSE);
   AppendMenu(SysMenu, MF_SEPARATOR, 0, '');
   AppendMenu(SysMenu, MF_STRING, SC_TransMenuItem, 'Write Translation file');
   IPTCReadTransFile('transIn.Txt');
   {$ENDIF}

   verbose := false;
   Constraints.MinWidth := width;               // no smaller than
   Constraints.MinHeight := height;             // initial size
   Panel1.Constraints.MinWidth := Panel1.width; // set sizing limits
   Panel1.Constraints.MinHeight := Panel1.height;
   Panel1.DoubleBuffered := true;               // block that flicker
   memo1.DoubleBuffered := true;
   ScrollBox1.DoubleBuffered := true;
end;

procedure TIPTCform.CleanScrollBox;
var i:integer;
    tc:tControl;
begin
  Memo1.Clear;
  for i := ScrollBox1.ControlCount-1 downto 0 do  // Wipe all controls from
  begin                                           // scroll panel.
    tc := ScrollBox1.Controls[i];
    tc.Parent := nil;
    tc.Free;
  end;
end;

procedure TIPTCform.AddControlSet(idx:integer; vName,vValue:string;
  MaxChars:integer);
var newLabel:TLabel;
    newEdit:TEdit;
begin
  newLabel := TLabel.Create(ScrollBox1);  // create new label
  newEdit := TEdit.Create(ScrollBox1);    // create new edit box
  with newLabel do
  begin
    Alignment := taRightJustify;
    AutoSize := false;
    top := 8+30*idx;
    left := 8;
    width := 190;
    parent := ScrollBox1;
    font.Style := [fsBold];
    Caption := vName;
  end;
  with newEdit do
  begin
    Anchors := [akTop,akLeft, akRight]; // expand when window resized
    parent := ScrollBox1;
    top := 7+30*idx;
    left := 200;
    tag := idx;
    MaxLength := MaxChars;
    width :=  IPTCform.width-226;      // hardcoded but otherwise changes when
    Text := vValue;                     // vertical scroll bar is displayed.
    DoubleBuffered := true;             // Flicker-free dynamic resizing
  end;
end;

procedure TIPTCform.LoadDisplayFromArray;  // populate form
var maxChars:integer;
    name,value:string;
    i:integer;
    tdTmp:TDateTime;
begin
 {$IFDEF DELPHI}
  scrollbox1.DisableAutoRange;    // Suspend repainting
 {$ENDIF}
  for i := 0 to ImgData.IptcObj.Count-1 do
  begin
    name  := ImgData.IptcObj[i].Desc;
    value := ImgData.IptcObj[i].Data;
    maxChars := ImgData.IptcObj[i].Size;
    if name = 'Image caption'  then
    begin
      memo(value);
      // scroll back to top - memo1.Lines.
      memo1.Perform(EM_LINESCROLL,0,-memo1.Lines.Count);
    end;
    AddControlSet(i,Name,Value,maxChars); // Add a label/edit box pair
  end;
  tdTmp := ImgData.IptcObj.GetDateTime();
  if tdTmp > 0 then
    if frac(tdtmp) = 0 then
      lblDateTime.Caption := FormatDateTime('mmm-dd-yyyy',tdTmp)
    else
      lblDateTime.Caption := FormatDateTime('mmm-dd-yyyy hh:nn:ss',tdTmp);
 {$IFDEF DELPHI}
  scrollbox1.enableAutoRange;             // Enable control painting
 {$ENDIF}
end;

procedure TIPTCform.CopyDisplayToArray;
var tc:tEdit;
    i,insrt:integer;
begin
  for i := 0 to scrollbox1.ControlCount-1 do
     if scrollbox1.Controls[i] is TEdit then
     begin
       tc := tEdit(scrollbox1.Controls[i]);   // To save a search,
       insrt := tc.Tag;                       // control has array position
       ImgData.IptcObj.SetTagByIdx(insrt,tc.text);   // stored in .tag field
     end;
end;

procedure TIPTCform.btnLoadClick(Sender: TObject);
begin
  if pdlg.Execute then
  begin
    StatusBar1.SimpleText  := 'Info for '+pdlg.FileName;
    Memo1.Clear;
    CleanScrollBox();               // get rid of previous controls
    if imgData.ProcessFile(pdlg.FileName) and (ImgData.IptcObj <> nil) then
    begin
      ImgData.IptcObj.ParseIPTCArray;
//    Added in 1.02a to demonstrate use of custom fields:
//      if these tags are found, then descriptions are altered,
//      otherwise these statements have no effect
      ImgData.IptcObj.UpdateTag('Custom_230','Image Notes');
      ImgData.IptcObj.UpdateTag('Custom_231','History');
      ImgData.IptcObj.UpdateTag('Custom_232','Camera Information');
      LoadDisplayFromArray()        // create controls based on found tags
    end
    else
      memo('Sorry: No IPTC info detected.');
    btnWrite.Enabled := true;
  end;
end;

procedure TIPTCform.Button1Click(Sender: TObject);
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
 {$IFNDEF dExifNoJpeg}
  if not WriteDlg.execute then
    exit;
  CopyDisplayToArray();              // Save form values
  ImgData.IptcObj.WriteFile(WriteDlg.FileName,pdlg.FileName);   // Write to jpeg file
 {$ELSE}
  ShowMessage('This function is not implemented.');
 {$ENDIF}
end;

procedure TIPTCform.Button2Click(Sender: TObject);
var xml:tstringlist;
begin
  xml := ImgData.MetaDataToXML;
  if xml = nil then
    exit;
  Memo1.Clear;
  Memo1.Lines.AddStrings(xml);
  xml.Free;
end;

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

procedure TIPTCform.btnSetDTClick(Sender: TObject);
begin
  // current valid date/time tag prefixes are:
  //  Release, Expire, and Digitize
  ImgData.IptcObj.SetDateTimeExt(now,'Release');
  CleanScrollBox;
  LoadDisplayFromArray;
end;

end.
