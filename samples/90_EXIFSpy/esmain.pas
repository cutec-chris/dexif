unit esMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterXML, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, EditBtn, Grids, ComCtrls, ValEdit,
  Buttons, ActnList, Menus, khexeditor, kfunctions, dEXIF, dIPTC, Types,
  mrumanager;

type

  { TMainForm }

  TMainForm = class(TForm)
    AcImgFit: TAction;
    AcGotoIFD0: TAction;
    AcGotoIFD1: TAction;
    AcGotoExifSubIFD: TAction;
    AcGotoTIFFHeader: TAction;
    AcGotoGPSSubIFD: TAction;
    AcGotoInteropSubIFD: TAction;
    AcFileOpen: TAction;
    AcFileQuit: TAction;
    ActionList: TActionList;
    CbHexAddressMode: TCheckBox;
    CbHexSingleBytes: TCheckBox;
    dExif_ThumbTagsGrid: TStringGrid;
    HexEditor: TKHexEditor;
    Image: TImage;
    ImageList: TImageList;
    AnalysisInfo: TLabel;
    MainMenu: TMainMenu;
    MainPageControl: TPageControl;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MnuFileReOpen: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog: TOpenDialog;
    HexPageControl: TPageControl;
    PageControl1: TPageControl;
    Panel2: TPanel;
    HexPanel: TPanel;
    PgHex: TTabSheet;
    APP1Popup: TPopupMenu;
    RecentFilesPopup: TPopupMenu;
    ScrollBox: TScrollBox;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    AnalysisGrid: TStringGrid;
    PgImage: TTabSheet;
    ImageToolBar: TToolBar;
    PgDExifTags: TTabSheet;
    PgDExifThumbTags: TTabSheet;
    PgDExifXML: TTabSheet;
    TbFit: TToolButton;
    TbGotoSOF1: TToolButton;
    TbGotoSOF2: TToolButton;
    TbGotoSOF3: TToolButton;
    TbGotoSOF5: TToolButton;
    TbGotoDHT: TToolButton;
    TbGotoSOF6: TToolButton;
    TbGotoSOF7: TToolButton;
    TbGotoJPG: TToolButton;
    TbGotoSOF9: TToolButton;
    TbGotoSOF10: TToolButton;
    TbGotoSOF11: TToolButton;
    TbGotoSOF13: TToolButton;
    TbGotoSOF14: TToolButton;
    TbGotoSOF15: TToolButton;
    TbGotoDAC: TToolButton;
    TbGotoDQT: TToolButton;
    MainToolBar: TToolBar;
    ToolButton3: TToolButton;
    TbNextSegment: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    XML_SynEdit: TSynEdit;
    SynXMLSyn: TSynXMLSyn;
    PgAnalysis: TTabSheet;
    PgConverter: TTabSheet;
    HexToolBar: TToolBar;
    TbGotoSOI: TToolButton;
    TbGotoSOF0: TToolButton;
    TbGotoAPP0: TToolButton;
    TbGotoAPP1: TToolButton;
    TbGotoAPP2: TToolButton;
    TbGotoEOI: TToolButton;
    TbGotoCOM: TToolButton;
    TbGotoSOS: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ValueGrid: TStringGrid;
    dExif_TagsGrid: TStringGrid;
    PgDExif: TTabSheet;
    procedure AcFileOpenExecute(Sender: TObject);
    procedure AcFileQuitExecute(Sender: TObject);
    procedure AcImgFitExecute(Sender: TObject);
    procedure AnalysisGridClick(Sender: TObject);
    procedure AnalysisGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure CbHexAddressModeChange(Sender: TObject);
    procedure CbHexSingleBytesChange(Sender: TObject);
    procedure dExifGridCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HexEditorClick(Sender: TObject);
    procedure HexEditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MRUMenuManagerRecentFile(Sender:TObject; const AFileName:string);
    procedure TbGotoAPP1ArrowClick(Sender: TObject);
    procedure TbGotoMarker(Sender: TObject);
    procedure TbGotoTIFFHeaderClick(Sender: TObject);
    procedure TbGotoIFD(Sender: TObject);
    procedure TbNextSegmentClick(Sender: TObject);
    procedure ValueGridClick(Sender: TObject);

  private
    FImgData: TImgData;
    FFileName: String;
    FCurrOffset: Int64;
    FBuffer: PBytes;
    FBufferSize: Int64;
    FMotorolaOrder: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    IFDList: array[0..4] of Int64;
    FMRUMenuManager : TMRUMenuManager;
    function DisplayGenericMarker(AOffset: Int64): Boolean;
    function DisplayMarker(AOffset: Int64): Boolean;
    function DisplayMarkerAPP0(AOffset: Int64): Boolean;
    function DisplayMarkerAPP1(AOffset: Int64): Boolean;
    function DisplayMarkerCOM(AOffset: Int64): Boolean;
    function DisplayMarkerEOI(AOffset: Int64): Boolean;
    function DisplayIFD(AOffset: Int64; ATIFFHeaderOffset: Int64;
      AInfo: String): Boolean;
    function DisplayMarkerSOF0(AOffset: Int64): Boolean;
    function DisplayMarkerSOI(AOffset: Int64): Boolean;
    function DisplayMarkerSOS(AOffset: Int64): Boolean;
    function DisplayTIFFHeader(AOffset: Int64): Boolean;
    function FindMarker(AMarker: Byte): Int64;
    function FindNextMarker(AMarker: byte; AFromPos: Int64): Int64;
    function FindNextMarker: Int64;
    function FindTIFFHeader: Int64;
    function GetBEIntValue(AOffset: Int64; AByteCount: Byte; out AValue: Int64): Boolean;
    function GetExifIntValue(AOffset: Int64; AByteCount: Byte; out AValue: Int64): Boolean;
    function GetExifValue(AOffset: Int64; ADataType, ADataSize: Integer): String;
    function GetMarkerName(AMarker: Byte; Long: Boolean): String;
    function GetValueGridDataSize: Integer;
    function GotoNextIFD(var AOffset: Int64): Boolean;
    procedure GotoOffset(AOffset: Int64);
    function GotoSubIFD(ATag: Word; var AOffset: Int64; ATiffHeaderOffset: Int64): Boolean;
    procedure OpenFile(const AFileName: String);
    procedure Populate_dExifGrid(Thumbs: Boolean);
    procedure Populate_ValueGrid;
    procedure ScanIFDs;
    procedure StatusMsg(const AMsg: String);
    procedure UpdateIFDs;
    procedure UpdateMarkers;
    procedure UpdateStatusbar;

    procedure ReadFromIni;
    procedure WriteToIni;

  public
    procedure BeforeRun;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLType, StrUtils, Math, IniFiles,
  KEditCommon,
  dGlobal, dTags;

const
  VALUE_ROW_INDEX         =  1;
  VALUE_ROW_BITS          =  2;
  VALUE_ROW_BYTE          =  3;
  VALUE_ROW_SHORTINT      =  4;
  VALUE_ROW_WORD          =  5;
  VALUE_ROW_WORD_BE       =  6;
  VALUE_ROW_SMALLINT      =  7;
  VALUE_ROW_SMALLINT_BE   =  8;
  VALUE_ROW_DWORD         =  9;
  VALUE_ROW_DWORD_BE      = 10;
  VALUE_ROW_LONGINT       = 11;
  VALUE_ROW_LONGINT_BE    = 12;
  VALUE_ROW_QWORD         = 13;
  VALUE_ROW_QWORD_BE      = 14;
  VALUE_ROW_INT64         = 15;
  VALUE_ROW_INT64_BE      = 16;
  VALUE_ROW_SINGLE        = 17;
  VALUE_ROW_DOUBLE        = 18;
  VALUE_ROW_ANSISTRING    = 19;
  VALUE_ROW_PANSICHAR     = 20;
  VALUE_ROW_WIDESTRING    = 21;
  VALUE_ROW_PWIDECHAR     = 22;

const
  MARKER_SOI   = $D8;
  MARKER_EOI   = $D9;
  MARKER_APP0  = $E0;
  MARKER_APP1  = $E1;
  MARKER_APP2  = $E2;
  MARKER_APP14 = $EE;
  MARKER_COM   = $FE;
  MARKER_DAC   = $CC;
  MARKER_DHT   = $C4;
  MARKER_DQT   = $DB;
  MARKER_JPG   = $C8;
  MARKER_SOF0  = $C0;
  MARKER_SOF1  = $C1;
  MARKER_SOF2  = $C2;
  MARKER_SOF3  = $C3;
  MARKER_SOF5  = $C5;
  MARKER_SOF6  = $C6;
  MARKER_SOF7  = $C7;
  MARKER_SOF9  = $C9;
  MARKER_SOF10  = $CA;
  MARKER_SOF11  = $CB;
  MARKER_SOF12  = $CC;
  MARKER_SOF13  = $CD;
  MARKER_SOF14  = $CE;
  MARKER_SOF15  = $CF;
  MARKER_SOS   = $DA;

  TAG_EXIF_OFFSET        = $8769;
  TAG_GPS_OFFSET         = $8825;
  TAG_INTEROP_OFFSET     = $A005;
  TAG_SUBIFD_OFFSET      = $014A;

  INDEX_IFD0     = 0;
  INDEX_EXIF     = 1;
  INDEX_INTEROP  = 2;
  INDEX_GPS      = 3;
  INDEX_IFD1     = 4;

  OFFSET_MASK = '%d ($%0:.8x)';

var
  MaxHistory: Integer = 10;

type
  THexEditorOpener = class(TKHexEditor);

function GetFixedFontName: String;
var
  idx: Integer;
begin
  Result := Screen.SystemFont.Name;
  idx := Screen.Fonts.IndexOf('Courier New');
  if idx = -1 then
    idx := Screen.Fonts.IndexOf('Courier 10 Pitch');
  if idx <> -1 then
    Result := Screen.Fonts[idx]
  else
    for idx := 0 to Screen.Fonts.Count-1 do
      if pos('courier', Lowercase(Screen.Fonts[idx])) = 1 then
      begin
        Result := Screen.Fonts[idx];
        exit;
      end;
end;

function CalcIniName: String;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;


{ TMainForm }

procedure TMainForm.AcFileOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
    OpenFile(OpenDialog.Filename);
end;

procedure TMainForm.AcFileQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AcImgFitExecute(Sender: TObject);
begin
  if AcImgFit.Checked then begin
    Image.Parent := PgImage;
    Image.Center := true;
  end else
  begin
    Image.Parent := Scrollbox;
    Image.Center := false;
  end;
  Image.Stretch := AcImgFit.Checked;
  Image.Proportional := AcImgFit.Checked;
  Scrollbox.Visible := not AcImgFit.Checked;
end;

procedure TMainForm.AnalysisGridClick(Sender: TObject);
var
  sel: TKHexEditorSelection;
  n: Integer;
  s: String;
begin
  sel := HexEditor.SelStart;

  s := AnalysisGrid.Cells[0, AnalysisGrid.Row];
  n := pos(' ', s);
  if n > 0 then
    s := Copy(s, 1, n-1);
  if not TryStrToInt(s, n) then
    exit;
  if n > 0 then begin
    sel.Index := n;
    sel.Digit := 0;
    HexEditor.SelStart := sel;
    n := n + StrToInt(AnalysisGrid.Cells[1, AnalysisGrid.Row]) - 1;
    sel.Index := n;
    sel.Digit := 1;
    HexEditor.SelEnd := sel;
  end else
    HexEditor.SelEnd := sel;

  if not HexEditor.CaretVisible then
    HexEditor.ExecuteCommand(ecScrollCenter);;
end;

procedure TMainForm.AnalysisGridPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  s: String;
begin
  s := AnalysisGrid.Cells[ACol, ARow];
  if (s <> '') and ((s[1] = '#') or (s = 'Offset to next IFD')) and (ACol = 3) then
    AnalysisGrid.Canvas.Font.Style := [fsBold];
end;

procedure TMainForm.BeforeRun;
begin
  ReadFromIni;
end;

procedure TMainForm.CbHexAddressModeChange(Sender: TObject);
begin
  HexEditor.AddressMode := TKHexEditorAddressMode(ord(CbHexAddressMode.Checked));
  case HexEditor.AddressMode of
    eamHex: HexEditor.AddressPrefix := '$';
    eamDec: HexEditor.AddressPrefix := ' ';
  end;
end;

procedure TMainForm.CbHexSingleBytesChange(Sender: TObject);
begin
  HexEditor.DigitGrouping := IfThen(CbHexSingleBytes.Checked, 1, 2);
end;

procedure TMainForm.dExifGridCompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
var
  sA, sB: String;
begin
  sA := dExif_TagsGrid.Cells[ACol, ARow];
  sB := dExif_TagsGrid.Cells[BCol, BRow];
  Result := CompareText(sA, sB);
  if dExif_TagsGrid.SortOrder = soDescending then Result := -Result;
end;

function TMainForm.DisplayGenericMarker(AOffset: Int64): Boolean;
var
  m: byte;
  j: Integer;
  numbytes: byte;
  val: Int64;
begin
  Result := false;

  m := FBuffer^[AOffset + 1];

  AnalysisInfo.Caption := Format('%s (%s)', [GetMarkerName(m, false), GetMarkerName(m, true)]);
  AnalysisGrid.RowCount := 2 + AnalysisGrid.FixedRows;
  j := AnalysisGrid.FixedRows;

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := Format('%d ($%.4x)', [val, val]);
  AnalysisGrid.Cells[3, j] := GetMarkerName(m, true);
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'Size';
  inc(j);
  inc(AOffset, numbytes);
end;

function TMainForm.DisplayMarker(AOffset: Int64): Boolean;
var
  m: Byte;
begin
  Result := false;
  GotoOffset(AOffset);
  m := FBuffer^[AOffset + 1];
  case m of
    MARKER_SOI  : Result := DisplayMarkerSOI(AOffset);
    MARKER_APP0 : Result := DisplayMarkerAPP0(AOffset);
    MARKER_APP1 : Result := DisplayMarkerAPP1(AOffset);
    MARKER_EOI  : Result := DisplayMarkerEOI(AOffset);
    MARKER_SOF0 : Result := DisplayMarkerSOF0(AOffset);
    MARKER_SOS  : Result := DisplayMarkerSOS(AOffset);
    MARKER_COM  : Result := DisplayMarkerCOM(AOffset);
    else          Result := DisplayGenericMarker(AOffset);
  end;
end;

function TMainForm.DisplayMarkerAPP0(AOffset: Int64): Boolean;
var
  s: String;
  val: Int64;
  j: Integer;
  numbytes: Byte;
begin
  Result := false;

  AnalysisInfo.Caption := 'APP0 (Application marker 0)';
  AnalysisGrid.RowCount := 9 + AnalysisGrid.FixedRows;
  j := AnalysisGrid.FixedRows;

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := Format('%d ($%.4x)', [val, val]);
  AnalysisGrid.Cells[3, j] := 'APP0 marker';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'Size';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 5;
  SetLength(s, 5);
  Move(FBuffer^[AOffset], s[1], 5);
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := s;
  AnalysisGrid.Cells[3, j] := 'Identifier (must be JFIF)';
  inc(j);
  inc(AOffset, 5);

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'JFIF format revision';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 1;
  if not GetBEIntvalue(AOffset, numbytes, val) then exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  case val of
    0: s := 'aspect ratio';
    1: s := 'inches';
    2: s := 'cm';
    else s := 'unknown';
  end;
  AnalysisGrid.Cells[3, j] := 'Units: ' + s;
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'X density';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'Y density';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 1;
  if not GetBEIntValue(AOffset, numbytes, val) then exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'Thumbnail width';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 1;
  if not GetBEIntValue(AOffset, numbytes, val) then exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'Thumbnail height';
  inc(j);
  inc(AOffset, numbytes);

  Result := True;
end;

function TMainForm.DisplayMarkerAPP1(AOffset: Int64): Boolean;
var
  s: String;
  val: Int64;
  j: Integer;
  numbytes: Byte;
begin
  Result := false;

  AnalysisInfo.Caption := 'APP1 (Application marker 1)';
  AnalysisGrid.RowCount := 3 + AnalysisGrid.FixedRows;
  j := AnalysisGrid.FixedRows;

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := Format('%d ($%.4x)', [val, val]);
  AnalysisGrid.Cells[3, j] := 'APP1 marker';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'Size';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 6;
  SetLength(s, numbytes);
  Move(FBuffer^[AOffset], s[1], numbytes);
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := s;
  AnalysisGrid.Cells[3, j] := 'EXIF identifer';
  inc(j);
  inc(AOffset, numbytes);

  Result := true;
end;

function TMainForm.DisplayIFD(AOffset: Int64; ATIFFHeaderOffset: Int64;
  AInfo: String): Boolean;
var
  n: Int64;
  val: Int64;
  i, j: Integer;
  numBytes: byte;
  s: String;
  pTag: PTagEntry;
  dt: byte;
  ds: Integer;
begin
  Result := false;

  numBytes := 2;
  if not GetExifIntValue(AOffset, numBytes, n) then
    exit;

  AnalysisInfo.Caption := AInfo;
  AnalysisGrid.RowCount := AnalysisGrid.FixedRows + n*4 + 2;

  j := AnalysisGrid.FixedRows;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(n);
  AnalysisGrid.Cells[3, j] := 'Number of directory entries';
  inc(AOffset, numbytes);
  inc(j);

  for i:=0 to n-1 do begin
    numbytes := 2;
    if not GetExifIntValue(AOffset, numbytes, val) then
      exit;

    pTag := FindExifTag(val);
    if pTag = nil then
      s := 'unknown'
    else
      s := pTag^.Desc;
    AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
    AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
    AnalysisGrid.Cells[2, j] := Format('%d ($%.4x)', [val, val]);
    AnalysisGrid.Cells[3, j] := Format('#%d: Tag type (%s)', [i, s]);
    inc(AOffset, numbytes);
    inc(j);

    numbytes := 2;
    if not GetExifIntValue(AOffset, numbytes, val) then
      exit;
    case val of
       1: s := 'UInt8';
       2: s := 'Zero-term. byte-string';
       3: s := 'UInt16';
       4: s := 'UInt32';
       5: s := 'Fraction';
       6: s := 'Int8';
       7: s := 'binary';
       8: s := 'Int16';
       9: s := 'Int32';
      10: s := 'Signed fraction';
      11: s := 'Single';
      12: s := 'Double';
      else s := '';
    end;
    dt := val;
    AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
    AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
    AnalysisGrid.Cells[2, j] := Format('%d', [val]);
    AnalysisGrid.Cells[3, j] := Format('   Data type (%s)', [s]);;
    inc(AOffset, numbytes);
    inc(j);

    numbytes := 4;
    if not GetExifIntValue(AOffset, numbytes, val) then
      exit;
    ds := val;
    AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
    AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
    AnalysisGrid.Cells[2, j] := IntToStr(ds);
    AnalysisGrid.Cells[3, j] := '   Data size (L)';
    inc(AOffset, numbytes);
    inc(j);

    numbytes := 4;
    if not GetExifIntValue(AOffset, numbytes, val) then
      exit;
    AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
    AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
    s := IntToStr(val);
    if val > 4 then
      s := s + ' --> ' + GetExifValue(ATiffHeaderOffset + val, dt, ds);
    AnalysisGrid.Cells[2, j] := s;
    AnalysisGrid.Cells[3, j] := '   if L <= 4: Data value, else: Offset to data from TIFF header';
    inc(AOffset, numbytes);
    inc(j);
  end;

  numbytes := 4;
  if not GetExifIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'Offset to next IFD';
  inc(AOffset, numbytes);

  Result := true;
end;

function TMainForm.DisplayMarkerCOM(AOffset: Int64): Boolean;
var
  j: Integer;
  numbytes: Integer;
  val: Int64;
  s: String;
begin
  Result := false;

  AnalysisInfo.Caption := 'COM (Comment)';
  AnalysisGrid.RowCount := 3 + AnalysisGrid.FixedRows;
  j := AnalysisGrid.FixedRows;

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := Format('%d ($%.4x)', [val, val]);
  AnalysisGrid.Cells[3, j] := 'COM marker';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'Size';
  inc(j);
  inc(AOffset, numbytes);

  SetLength(s, val - 2);
  Move(FBuffer^[AOffset], s[1], Length(s));
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := s;
  AnalysisGrid.Cells[3, j] := 'Comment text';
  inc(j);

  Result := true;
end;

function TMainForm.DisplayMarkerEOI(AOffset: Int64): Boolean;
var
  s: String;
  val: Int64;
  j: Integer;
  numbytes: Byte;
begin
  Result := false;

  AnalysisInfo.Caption := 'EOI (End of image)';
  AnalysisGrid.RowCount := 1 + AnalysisGrid.FixedRows;
  j := AnalysisGrid.FixedRows;

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := Format('%d ($%.4x)', [val, val]);
  AnalysisGrid.Cells[3, j] := 'EOI marker';
  inc(j);
  inc(AOffset, numbytes);

  Result := true;
end;

function TMainForm.DisplayMarkerSOF0(AOffset: Int64): Boolean;
var
  s: String;
  val: Int64;
  j: Integer;
  numbytes: Byte;
begin
  Result := false;

  AnalysisInfo.Caption := 'SOF0 (Start of frame 0)';
  AnalysisGrid.RowCount := 6 + AnalysisGrid.FixedRows;
  j := AnalysisGrid.FixedRows;

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := Format('%d ($%.4x)', [val, val]);
  AnalysisGrid.Cells[3, j] := 'SOF0 marker';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'Size';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 1;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'Data precision (bits/sample)';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'Image height';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'Image width';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 1;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'Number of components';
  inc(j);
  inc(AOffset, numbytes);

  Result := true;
end;

function TMainForm.DisplayTIFFHeader(AOffset: Int64): Boolean;
var
  s: String;
  numbytes: Byte;
  j, val: Int64;
  tiffHdr: Int64;
begin
  Result := false;

  tiffHdr := AOffset;
  AnalysisInfo.Caption := 'TIFF header';
  AnalysisGrid.RowCount := AnalysisGrid.FixedRows + 3;
  j := AnalysisGrid.FixedRows;

  s := char(FBuffer^[AOffset]) + char(FBuffer^[AOffset+1]);
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := '2';
  AnalysisGrid.Cells[2, j] := s;
  if s = 'II' then s := 'Intel (little endian)' else
  if s = 'MM' then s := 'Motorola (big endian)' else s := '';
  AnalysisGrid.Cells[3, j] := 'Byte order (' + s + ')';
  inc(j);
  inc(AOffset, 2);

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := Format('%d ($%.4x)', [val, val]);
  AnalysisGrid.Cells[3, j] := 'TIFF version number';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 4;
  if not GetBEIntValue(AOffset, numbytes, val) then exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := Format('%d --> %d', [val, tiffHdr + val]);
  AnalysisGrid.Cells[3, j] := 'Offset to first IFD (from begin of TIFF header)';
  inc(j);
  inc(AOffset, numbytes);
end;


function TMainForm.DisplayMarkerSOI(AOffset: Int64): Boolean;
var
  s: String;
  val: Int64;
  j: Integer;
  numbytes: Byte;
begin
  Result := false;

  AnalysisInfo.Caption := 'SOI (Start of image)';
  AnalysisGrid.RowCount := 1 + AnalysisGrid.FixedRows;
  j := AnalysisGrid.FixedRows;

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := Format('%d ($%.4x)', [val, val]);
  AnalysisGrid.Cells[3, j] := 'SOI marker';
  inc(j);
  inc(AOffset, numbytes);

  Result := true;
end;


function TMainForm.DisplayMarkerSOS(AOffset: Int64): Boolean;
var
  s: String;
  val: Int64;
  j: Integer;
  numbytes: Byte;
begin
  Result := false;

  AnalysisInfo.Caption := 'SOS (Start of scan)';
  AnalysisGrid.RowCount := 2 + AnalysisGrid.FixedRows;
  j := AnalysisGrid.FixedRows;

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := Format('%d ($%.4x)', [val, val]);
  AnalysisGrid.Cells[3, j] := 'SOS marker';
  inc(j);
  inc(AOffset, numbytes);

  numbytes := 2;
  if not GetBEIntValue(AOffset, numbytes, val) then
    exit;
  AnalysisGrid.Cells[0, j] := Format(OFFSET_MASK, [AOffset]);
  AnalysisGrid.Cells[1, j] := IntToStr(numbytes);
  AnalysisGrid.Cells[2, j] := IntToStr(val);
  AnalysisGrid.Cells[3, j] := 'Size';
  inc(j);
  inc(AOffset, numbytes);

  Result := true;
end;


function TMainForm.FindMarker(AMarker: byte): Int64;
var
  p: PByte;
  pw: PWord;
  len: Integer;
begin
  Result := -1;
  if FBuffer = nil then
    exit;

  // The EOI marker is the last marker of the file, following the compressed data
  // which have unknown length. --> We find the marker by scanning
  // NOTE: Markers are not allowed in compressed data
  // (https://stackoverflow.com/questions/26715684/parsing-jpeg-sos-marker)
  if AMarker = MARKER_EOI then begin
    Result := 0;
    p := @FBuffer^[0];
    while Result < FBufferSize do begin
      if p^ = $FF then begin
        inc(p);
        if p^ = AMarker then
          exit;
        inc(Result);
      end;
      inc(p);
      inc(Result);
    end;
    Result := -1;
    exit;
  end;

  p := @FBuffer^[0];
  if p^ <> $FF then exit;
  p := @FBuffer^[1];
  if p^ <> $D8 then exit;

  if (AMarker = $D8) then begin
    Result := 0;
    exit;
  end;

  Result := 2;
  p := @FBuffer^[2];
  while (Result < FBufferSize) do begin
    if p^ <> $FF then begin
      Result := -1;
      exit;
    end;
    inc(p);
    if p^ = AMarker then begin
//      dec(Result, 2);
      exit;
    end;
    inc(p);
    pw := PWord(p);
    len := BEToN(pw^);
    inc(p, len);
    Result := Result + 2 + len;
  end;
  Result := -1;
end;

function TMainForm.FindNextMarker(AMarker: Byte; AFromPos: Int64): Int64;
var
  p: PByte;
begin
  Result := AFromPos;
  p := @FBuffer^[AFromPos];
  while Result < FBufferSize do begin
    if p^ = $FF then begin
      inc(p);
      if p^ = AMarker then
        exit;
      inc(Result);
    end;
    inc(p);
    inc(Result);
  end;
  Result := -1;
end;

function TMainForm.FindNextMarker: Int64;
var
  p: PByte;
  w: word;
  offs0: Int64;
begin
  Result := -1;
  if FBuffer^[FCurrOffset] <> $FF then begin
    ShowMessage('Navigate to the begin of a segment first.');
    exit;
  end;
  if FCurrOffset = 0 then
    Result := 2
  else begin
    offs0 := FCurrOffset;
    p := @FBuffer^[FCurrOffset + 2];
    w := BEToN(PWord(p)^);
    inc(p, w + 2);
    Result := FCurrOffset + w + 2;
    if FBuffer^[Result] <> $FF then
      FCurroffset := offs0;
  end;
end;

function TMainForm.FindTIFFHeader: Int64;
var
  p: PByte;
begin
  Result := FindMarker(MARKER_APP1);
  if Result = -1 then
    exit;
  while Result < FBufferSize do begin
    p := @FBuffer^[Result];
    inc(p, 4);
    if PChar(p) = 'Exif' then begin
      inc(Result, 2 + 2 + Length('Exif'#0#0));
      exit;
    end else begin
      Result := FindNextMarker(Marker_APP1, Result+1);
      if Result > 0 then
        exit;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FCurrOffset := -1;

  AcGotoIFD0.Tag := AcGotoTIFFHeader.Tag + 1 + INDEX_IFD0;
  AcGotoEXIFSubIFD.Tag := AcGotoTIFFHeader.Tag + 1 + INDEX_EXIF;
  AcGotoINTEROPSubIFD.Tag := AcGotoTIFFHeader.Tag + 1+ INDEX_INTEROP;
  AcGotoGPSSubIFD.Tag := AcGotoTIFFHeader.Tag + 1 + INDEX_GPS;
  AcGotoIFD1.Tag := AcGotoTIFFHeader.Tag + 1 + INDEX_IFD1;

  FMRUMenuManager := TMRUMenuManager.Create(self);
  with FMRUMenuManager do begin
    Name := 'MRUMenuManager';
    IniFileName := CalcIniName;
    IniSection := 'RecentFiles';
    MaxRecent := 16;
    MenuCaptionMask := '&%x - %s';    // & --> create hotkey
    MenuItem := MnuFileReopen;
    PopupMenu := RecentFilesPopup;
    OnRecentFile := @MRUMenuManagerRecentFile;
  end;

  with HexEditor do begin
    Font.Name := GetFixedFontName;  // The hard-coded Courier New does not exist in Linux
    Font.Style := [];
    Font.Pitch := fpDefault;
    Font.Quality := fqClearType;
  end;

  with AnalysisGrid do begin
    ColWidths[0] := 120;
    ColWidths[1] := 50;
    ColWidths[2] := 150;
  end;

  with ValueGrid do begin
    ColCount := 3;
    RowCount := VALUE_ROW_PWIDECHAR + 1;
    Cells[0, 0] := 'Data type';
    Cells[1, 0] := 'Value';
    Cells[2, 0] := 'Offset range';
    Cells[0, VALUE_ROW_INDEX] := 'Offset';
    Cells[0, VALUE_ROW_BITS] := 'Bits';
    Cells[0, VALUE_ROW_BYTE] := 'Byte';
    Cells[0, VALUE_ROW_SHORTINT] := 'ShortInt';
    Cells[0, VALUE_ROW_WORD] := 'Word';
    Cells[0, VALUE_ROW_WORD_BE] := 'Word (BE)';
    Cells[0, VALUE_ROW_SMALLINT] := 'SmallInt';
    Cells[0, VALUE_ROW_SMALLINT_BE] := 'SmallInt (BE)';
    Cells[0, VALUE_ROW_DWORD] := 'DWord';
    Cells[0, VALUE_ROW_DWORD_BE] := 'DWord (BE)';
    Cells[0, VALUE_ROW_LONGINT] := 'LongInt';
    Cells[0, VALUE_ROW_LONGINT_BE] := 'LongInt (BE)';
    Cells[0, VALUE_ROW_QWORD] := 'QWord';
    Cells[0, VALUE_ROW_QWORD_BE] := 'QWord (BE)';
    Cells[0, VALUE_ROW_INT64] := 'Int64';
    Cells[0, VALUE_ROW_INT64_BE] := 'Int64 (BE)';
    Cells[0, VALUE_ROW_SINGLE] := 'Single';
    Cells[0, VALUE_ROW_DOUBLE] := 'Double';
    Cells[0, VALUE_ROW_ANSISTRING] := 'AnsiString';
    Cells[0, VALUE_ROW_PANSICHAR] := 'PAnsiChar';
    Cells[0, VALUE_ROW_WIDESTRING] := 'WideString';
    Cells[0, VALUE_ROW_PWIDECHAR] := 'PWideChar';
    ColWidths[0] := Canvas.TextWidth(' SmallInt (BE) ');
  end;
  CbHexAddressModeChange(nil);

  for i:=0 to HexToolbar.ButtonCount-1 do
    HexToolbar.Buttons[i].Enabled := false;

  Populate_dExifGrid(false);
  Populate_dExifGrid(true);
  {
  for i:=0 to APP1Popup.Items.Count-1 do
    APP1Popup.Items[i].Enabled := false;
  }
end;

procedure TMainForm.FormDeactivate(Sender: TObject);
begin
  WriteToIni;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteToIni;
end;

function TMainForm.GetBEIntValue(AOffset: Int64; AByteCount: Byte;
  out AValue: Int64): Boolean;
var
  b: Byte;
  w: Word;
  dw: DWord;
  qw: Int64;
begin
  Result := false;
  if AOffset - AByteCount >= FBufferSize then
    exit;
  case AByteCount of
    1: begin
         b := FBuffer^[AOffset];
         AValue := b;
       end;
    2: begin
         Move(FBuffer^[AOffset], w, 2);
         AValue := BEToN(w);
       end;
    4: begin
         Move(FBuffer^[AOffset], dw, 4);
         AValue := BEToN(dw);
       end;
    8: begin
         Move(FBuffer^[AOffset], qw, 8);
         AValue := BEToN(qw);
       end;
    else
      exit;
  end;
  Result := true;
end;

function TMainForm.GetExifIntValue(AOffset: Int64; AByteCount: Byte;
  out AValue: Int64): Boolean;
var
  b: Byte;
  w: Word;
  dw: DWord;
  qw: Int64;
begin
  Result := false;
  if AOffset - AByteCount >= FBufferSize then
    exit;
  case AByteCount of
    1: begin
         b := FBuffer^[AOffset];
         AValue := b;
       end;
    2: begin
         Move(FBuffer^[AOffset], w, 2);
         if FMotorolaOrder then w := BEToN(w);
         AValue := w;
       end;
    4: begin
         Move(FBuffer^[AOffset], dw, 4);
         if FMotorolaOrder then dw := BEToN(dw);
         AValue := dw;
       end;
    8: begin
         Move(FBuffer^[AOffset], qw, 8);
         if FMotorolaOrder then qw := BEToN(qw);
         AValue := qw;
       end;
    else
      exit;
  end;
  Result := true;
end;

function TMainForm.GetExifValue(AOffset: Int64; ADataType,ADataSize: Integer): String;
var
  n: Int64;
  sng: Single;
  dbl: Double;
  i: Integer;
begin
  if (AOffset + ADataSize > FBufferSize) or
     (AOffset + ADataSize < 0) then
  begin
    Result := '<???>';
    exit;
  end;

  case ADataType of
    1: Result := IntToStr(FBuffer^[AOffset]);  // unsigned byte
    2: begin   // ASCII string
         SetLength(Result, ADataSize);
         for i:=1 to ADataSize do
           Result[i] := char(FBuffer^[AOffset + i - 1]);  // ASCII string
         exit;
       end;
    3: begin  // unsigned short
         n := PWord(@FBuffer^[AOffset])^;
         if FMotorolaOrder then n := BEToN(n);
         Result := IntToStr(n);
       end;
    4: begin  // unsigned long
         n := PDWord(@FBuffer^[AOffset])^;
         if FMotorolaOrder then n := BEToN(n);
         Result := IntToStr(n);
       end;
    5: begin  // unsigned rational
         n := PDWord(@FBuffer^[AOffset])^;
         if FMotorolaOrder then n := BEToN(n);
         Result := IntToStr(n);
         n := PDWord(@FBuffer^[AOffset + 4])^;
         if FMotorolaOrder then n := BEToN(n);
         Result := Result + '/' + IntToStr(n);
       end;
    6: begin   // singed byte
         Result := IntToStr(ShortInt(@FBuffer^[AOffset]));
       end;
    7: begin   // undefined
         Result := '';
       end;
    8: begin   // signed short
         n := SmallInt(PWord(@FBuffer^[AOffset]));
         if FMotorolaOrder then n := BEToN(n);
         Result := IntToStr(n);
       end;
    9: begin   // signed long
         n := LongInt(PDWord(@FBuffer^[AOffset])^);
         if FMotorolaOrder then n := BEToN(n);
         Result := IntToStr(n);
       end;
   10: begin   // signed rational
         n := LongInt(PDWord(@FBuffer^[AOffset])^);
         if FMotorolaOrder then n := BEToN(n);
         Result := IntToStr(n);
         n := LongInt(PDWord(@FBuffer^[AOffset + 4])^);
         if FMotorolaOrder then n := BEToN(n);
         Result := Result + '/' + IntToStr(n);
       end;
   11: begin  // single
         sng := PSingle(@FBuffer^[AOffset])^;
         // Motorola?
         Result := Format('%g', [sng]);
       end;
   12: begin  // double
         dbl := PDouble(@FBuffer^[AOffset])^;
         // Motorola ??
         Result := Format('%g', [dbl]);
       end;
   else
       Result := '';
  end;
end;

function TMainForm.GetMarkerName(AMarker: Byte; Long: Boolean): String;
begin
  case AMarker of
    MARKER_SOI   : Result := IfThen(Long, 'Start of image', 'SOI');
    MARKER_EOI   : Result := IfThen(Long, 'End of image', 'EOI');
    MARKER_APP0  : Result := IfThen(Long, 'Application marker 0', 'APP0 (JFIF)');
    MARKER_APP1  : Result := IfThen(Long, 'Application marker 1', 'APP1 (EXIF)');
    MARKER_APP2  : Result := IfThen(Long, 'Application marker 2', 'APP2');
    MARKER_APP14 : Result := IfThen(Long, 'Application marker 14 - Copyright', 'APP14)');
    MARKER_COM   : Result := IfThen(Long, 'Comment marker', 'COM');
    MARKER_DAC   : Result := IfThen(Long, 'Definition of arithmetic coding', 'DAC');
    MARKER_DHT   : Result := IfThen(Long, 'Definition of Huffman tables', 'DHT');
    MARKER_DQT   : Result := IfThen(Long, 'Definition of quantization tables', 'DQT');
    MARKER_JPG   : Result := IfThen(Long, 'JPG extensions', 'JPG');
    MARKER_SOF0  : Result := IfThen(Long, 'Start of frame 0 - Baseline DCT', 'SOF0');
    MARKER_SOF1  : Result := IfThen(Long, 'Start of frame 1 - Extended sequential DCT', 'SOF1');
    MARKER_SOF2  : Result := IfThen(Long, 'Start of frame 2 - Progressive DCT', 'SOF2');
    MARKER_SOF3  : Result := IfThen(Long, 'Start of frame 3 - Lossless (sequential)', 'SOF3');
    MARKER_SOF5  : Result := IfThen(Long, 'Start of frame 5 - Differential sequential DCT', 'SOF5');
    MARKER_SOF6  : Result := IfThen(Long, 'Start of frame 6 - Differential progressive DCT', 'SOF6');
    MARKER_SOF7  : Result := IfThen(Long, 'Start of frame 7 - Differential lossless (sequential)', 'SOF7');
    MARKER_SOF9  : Result := IfThen(Long, 'Start of frame 0 - Extended sequential DCT', 'SOF9');
    MARKER_SOF10 : Result := IfThen(Long, 'Start of frame 10 - Proogressive DCT', 'SOF10');
    MARKER_SOF11 : Result := IfThen(Long, 'Start of frame 11 - Lossless (sequential)', 'SOF11');
    MARKER_SOF13 : Result := IfThen(Long, 'Start of frame 13 - Differential sequential DCT', 'SOF13');
    MARKER_SOF14 : Result := IfThen(Long, 'Start of frame 14 - Differential progressive DCT', 'SOF14');
    MARKER_SOF15 : Result := IfThen(Long, 'Start of frame 15 - Differential lossless (sequential)', 'SOF15');
    MARKER_SOS   : Result := IfThen(Long, 'Start of scan', 'SOS');
    else           Result := IfThen(Long, 'Unknown', Format('$%.2x', [AMarker]));
  end;
end;

function TMainForm.GetValueGridDataSize: Integer;

  function ExtractLength(s: String): Integer;
  var
    i: Integer;
    n1, n2: Integer;
    isFirst: Boolean;
  begin
    isFirst := true;
    n1 := 0;
    n2 := 0;
    for i:=1 to Length(s) do
      case s[i] of
        '0'..'9':
          if isFirst then
            n1 := n1*10 + ord(s[i]) - ord('0') else
            n2 := n2*10 + ord(s[i]) - ord('0');
        ' ': if isFirst then isFirst := false;
      end;
    Result := n2 - n1 + 1;
  end;

begin
  Result := -1;
  case ValueGrid.Row of
    VALUE_ROW_BITS       : Result := SizeOf(Byte);
    VALUE_ROW_BYTE       : Result := SizeOf(Byte);
    VALUE_ROW_SHORTINT   : Result := SizeOf(ShortInt);
    VALUE_ROW_WORD,
    VALUE_ROW_WORD_BE    : Result := SizeOf(Word);
    VALUE_ROW_SMALLINT,
    VALUE_ROW_SMALLINT_BE: Result := SizeOf(SmallInt);
    VALUE_ROW_DWORD,
    VALUE_ROW_DWORD_BE   : Result := SizeOf(DWord);
    VALUE_ROW_LONGINT,
    VALUE_ROW_LONGINT_BE : Result := SizeOf(LongInt);
    VALUE_ROW_QWORD,
    VALUE_ROW_QWORD_BE   : Result := SizeOf(QWord);
    VALUE_ROW_INT64,
    VALUE_ROW_INT64_BE   : Result := SizeOf(Int64);
    VALUE_ROW_SINGLE     : Result := SizeOf(Single);
    VALUE_ROW_DOUBLE     : Result := SizeOf(Double);
    VALUE_ROW_ANSISTRING,
    VALUE_ROW_WIDESTRING,
    VALUE_ROW_PANSICHAR,
    VALUE_ROW_PWIDECHAR  : Result := ExtractLength(ValueGrid.Cells[2, ValueGrid.Row]);
  end;
end;

function TMainForm.GotoNextIFD(var AOffset: Int64): Boolean;
var
  n: Int64;
begin
  Result := false;

  if not GetExifIntValue(AOffset, 2, n) then
    exit;

  inc(AOffset, 2 + n*(2+2+4+4));
  if not GetExifIntValue(AOffset, 4, n) then
    exit;

  inc(AOffset, 4 + n);
  Result := true;
end;

procedure TMainForm.GotoOffset(AOffset: Int64);
var
  sel: TKHexEditorSelection;
begin
  if AOffset > High(FBuffer^) then begin
    StatusMsg('Out of buffer limits.');
    exit;
  end;

  if AOffset < 0 then begin
    StatusMsg('Out of buffer limits.');
    exit;
  end;

  FCurrOffset := AOffset;
  sel := HexEditor.SelStart;
  sel.Index := AOffset;
  sel.Digit := 0;
  HexEditor.SelStart := sel;
  HexEditor.SelEnd := sel;
  if not HexEditor.CaretVisible then
    HexEditor.ExecuteCommand(ecScrollCenter);
  Populate_ValueGrid;
  UpdateStatusbar;
  TbNextSegment.Enabled := FBuffer^[AOffset] = $FF;
end;

function TMainForm.GotoSubIFD(ATag: Word;
  var AOffset: Int64; ATIFFHeaderOffset: Int64): boolean;
var
  n,L: Int64;
  val: Int64;
  i: Integer;
begin
  Result := false;

  if not GetExifIntValue(AOffset, 2, n) then
    exit;

  inc(AOffset, 2);

  for i:=0 to n-1 do begin
    if not GetExifIntValue(AOffset, 2, val) then
      exit;
    if val = ATag then begin        // See TAG_XXXX_OFFSET constants
      inc(AOffset, 2 + 2);
      if not GetExifIntValue(AOffset, 4, L) then exit;
      if L > 4 then
        AOffset := ATiffHeaderOffset + L
      else
        inc(AOffset, 4);
      if not GetExifIntValue(AOffset, 4, val) then exit;
      AOffset := ATiffHeaderOffset + val;
      Result := true;
      exit;
    end;
    inc(AOffset, 2 + 2 + 4 + 4);
  end;
end;

procedure TMainForm.HexEditorClick(Sender: TObject);
begin
  GoToOffset(HexEditor.SelStart.Index);
end;

procedure TMainForm.HexEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  sel: TKHexEditorSelection;
begin
  case Key of
    VK_LEFT  : dec(FCurrOffset);
    VK_RIGHT : inc(FCurrOffset);
    VK_UP    : dec(FCurrOffset, HexEditor.LineSize);
    VK_DOWN  : inc(FCurrOffset, HexEditor.LineSize);
    VK_HOME  : if (Shift = [ssCtrl]) then
                 FCurrOffset := 0 else
                 FCurrOffset := (FCurrOffset div HexEditor.LineSize) * HexEditor.LineSize;
    VK_END   : if (Shift = [ssCtrl]) then
                 FCurrOffset := FBufferSize-1 else
                 FCurrOffset := succ(FCurrOffset div HexEditor.LineSize) * HexEditor.lineSize - 1;
    VK_NEXT  : begin
                 if (Shift = [ssCtrl]) then
                   inc(FCurrOffset, HexEditor.LineSize * HexEditor.LineCount)
                 else
                   inc(FCurrOffset, HexEditor.LineSize * HexEditor.GetClientHeightChars);
                 while (FCurrOffset >= FBufferSize) do
                   dec(FCurrOffset, HexEditor.LineSize);
               end;
    VK_PRIOR : if (Shift = [ssCtrl]) then
                 FCurrOffset := FCurrOffset mod HexEditor.LineSize
               else
               begin
                 dec(FCurrOffset, HexEditor.LineSize * HexEditor.GetClientHeightChars);
                 while (FCurrOffset < 0) do
                   inc(FCurrOffset, HexEditor.LineSize);
               end;
    else
               exit;
  end;
  if FCurrOffset < 0 then FCurrOffset := 0;
  if FCurrOffset >= FBufferSize then FCurrOffset := FBufferSize - 1;
  sel.Index := FCurrOffset;
  sel.Digit := 0;
  HexEditor.SelStart := sel;
  HexEditorClick(nil);
  if not HexEditor.CaretInView then
    HexEditor.ExecuteCommand(ecScrollCenter);
   // THexEditorOpener(HexEditor).ScrollTo(HexEditor.SelToPoint(HexEditor.SelStart, HexEditor.EditArea), false, true);

  // Don't process these keys any more!
  Key := 0;
end;

procedure TMainForm.MRUMenuManagerRecentFile(Sender: TObject;
  const AFileName: string);
begin
  OpenFile(AFileName);
end;

procedure TMainForm.OpenFile(const AFileName: String);
var
  i, j: Integer;
  t: TTagEntry;
  L: TStringList;
begin
  if AFileName = '' then begin
    ShowMessage('No file selected.');
    exit;
  end;
  if not FileExists(AFileName) then begin
    ShowMessage('File "' + AFilename + '" does not exist.');
    exit;
  end;

  FFilename := AFilename;

  FreeAndNil(FImgData);
  FImgData := TImgData.Create;
  FImgData.ProcessFile(AFileName);
  FMotorolaOrder := FImgData.MotorolaOrder;
  FWidth := FImgData.Width;
  FHeight := FImgData.Height;

  Populate_dExifGrid(false);
  Populate_dExifGrid(true);

  HexEditor.LoadFromFile(AFileName);
  FBuffer := THexEditorOpener(HexEditor).Buffer;
  FBufferSize := THexEditorOpener(HexEditor).Size;
  FCurrOffset := 0;
  HexEditorClick(nil);

  ScanIFDs;
  UpdateIFDs;
  UpdateMarkers;

  L := FImgData.MetadataToXML;
  try
    XML_SynEdit.Lines.Assign(L);
  finally
    L.Free;
  end;

  Image.Picture.LoadFromFile(AFileName);
  Image.Width := FWidth;
  Image.Height := FHeight;
  AcImgFitExecute(nil);

  FMRUMenuManager.AddToRecent(AFileName);

  Caption := Format('Exif Spy - "%s"', [FFilename]);
end;

procedure TMainForm.Populate_dExifGrid(Thumbs: Boolean);
var
  tagArray: Array of TTagEntry;
  tagCount: Integer;
  t: TTagEntry;
  i, j: Integer;
  grid: TStringGrid;
begin
  if thumbs then
    grid := dExif_ThumbTagsGrid
  else
    grid := dExif_TagsGrid;
  grid.ColCount := 16;

  if (FImgData <> nil) and FImgData.HasExif then begin
    if Thumbs then begin
      tagArray := FImgData.ExifObj.FIThumbArray;
      tagCount := FImgData.ExifObj.FIThumbCount;
    end else begin
      tagArray := FImgData.ExifObj.FITagArray;
      tagCount := FImgData.ExifObj.FITagCount;
    end;
    grid.RowCount := tagCount + grid.FixedRows;

    for i:=0 to tagCount-1 do begin
      t := tagArray[i];
      j := grid.FixedRows + i;
      with grid do begin
        Cells[ 0, j] := IntToStr(i);
        Cells[ 1, j] := IntToStr(t.TID);
        Cells[ 2, j] := IntToStr(t.TType);
        cells[ 3, j] := IntToStr(t.ICode);
        Cells[ 4, j] := Format('%d ($%.4x)', [t.Tag, t.Tag]);
        Cells[ 5, j] := t.Name;
        Cells[ 6, j] := t.Desc;
        Cells[ 7, j] := t.Code;
        Cells[ 8, j] := t.Data;
        Cells[ 9, j] := t.Raw;
        Cells[10, j] := IntToStr(t.PRaw);
        Cells[11, j] := t.FormatS;
        Cells[12, j] := IntToStr(t.Size);
        Cells[13, j] := Format('$%.8x', [PtrInt(@t.CallBack)]);
        Cells[14, j] := IntToStr(t.id);
        Cells[15, j] := IntToStr(t.parentid);
      end;
    end;
  end else
  begin
    grid.RowCount := grid.FixedRows + 1;
    grid.Rows[grid.FixedRows].Clear;
    StatusMsg('No EXIF data found.');
  end;
  with grid do begin
    Cells[ 1, 0] := 'TID';
    Cells[ 2, 0] := 'TType';
    Cells[ 3, 0] := 'TCode';
    Cells[ 4, 0] := 'Tag';
    Cells[ 5, 0] := 'Name';
    Cells[ 6, 0] := 'Desc';
    Cells[ 7, 0] := 'Code';
    Cells[ 8, 0] := 'Data';
    Cells[ 9, 0] := 'Raw';
    Cells[10, 0] := 'PRaw';
    Cells[11, 0] := 'FormatS';
    Cells[12, 0] := 'Size';
    Cells[13, 0] := 'CallBack';
    Cells[14, 0] := 'id';
    Cells[15, 0] := 'parentid';
  end;
end;

procedure TMainForm.Populate_ValueGrid;
const
  MAX_LEN = 32;
var
  buf: array[0..1023] of Byte;
  w: word absolute buf;
  dw: DWord absolute buf;
  qw: QWord absolute buf;
  dbl: double absolute buf;
  sng: single absolute buf;
  idx: Integer;
  i, j: Integer;
  s: String;
  sw: WideString;
  ls: SizeInt;
  pw: PWideChar;
  pa: PAnsiChar;
begin
  idx := FCurrOffset;

  i := ValueGrid.RowCount;
  j := ValueGrid.ColCount;

  ValueGrid.Cells[1, VALUE_ROW_INDEX] := IntToStr(idx);

  // Byte, ShortInt
  if idx <= FBufferSize - SizeOf(byte) then begin
    ValueGrid.Cells[1, VALUE_ROW_BITS] := IntToBin(FBuffer^[idx], 8);
    ValueGrid.Cells[2, VALUE_ROW_BITS] := Format('%d ... %d', [idx, idx]);
    ValueGrid.Cells[1, VALUE_ROW_BYTE] := IntToStr(FBuffer^[idx]);
    ValueGrid.Cells[2, VALUE_ROW_BYTE] := ValueGrid.Cells[2, VALUE_ROW_BITS];
    ValueGrid.Cells[1, VALUE_ROW_SHORTINT] := IntToStr(ShortInt(FBuffer^[idx]));
    ValueGrid.Cells[2, VALUE_ROW_SHORTINT] := ValueGrid.Cells[2, VALUE_ROW_BITS];
  end
  else begin
    ValueGrid.Cells[1, VALUE_ROW_BYTE] := '';
    ValueGrid.Cells[2, VALUE_ROW_BYTE] := '';
    ValueGrid.Cells[1, VALUE_ROW_SHORTINT] := '';
    ValueGrid.Cells[2, VALUE_ROW_SHORTINT] := '';
  end;

  // Word, SmallInt
  if idx <= FBufferSize - SizeOf(word) then begin
    buf[0] := FBuffer^[idx];
    buf[1] := FBuffer^[idx+1];
    ValueGrid.Cells[1, VALUE_ROW_WORD] := IntToStr(LEToN(w));
    ValueGrid.Cells[2, VALUE_ROW_WORD] := Format('%d ... %d', [idx, idx+SizeOf(Word)-1]);
    ValueGrid.Cells[1, VALUE_ROW_SMALLINT] := IntToStr(SmallInt(LEToN(w)));
    ValueGrid.Cells[2, VALUE_ROW_SMALLINT] := ValueGrid.Cells[2, VALUE_ROW_WORD];
    ValueGrid.Cells[1, VALUE_ROW_WORD_BE] := IntToStr(BEToN(w));
    ValueGrid.Cells[2, VALUE_ROW_WORD_BE] := Format('%d ... %d', [idx, idx+SizeOf(Word)-1]);
    ValueGrid.Cells[1, VALUE_ROW_SMALLINT_BE] := IntToStr(SmallInt(BEToN(w)));
    ValueGrid.Cells[2, VALUE_ROW_SMALLINT_BE] := ValueGrid.Cells[2, VALUE_ROW_WORD];
  end else begin
    ValueGrid.Cells[1, VALUE_ROW_WORD] := '';
    ValueGrid.Cells[2, VALUE_ROW_WORD] := '';
    ValueGrid.Cells[1, VALUE_ROW_SMALLINT] := '';
    ValueGrid.Cells[2, VALUE_ROW_SMALLINT] := '';
    ValueGrid.Cells[1, VALUE_ROW_WORD_BE] := '';
    ValueGrid.Cells[2, VALUE_ROW_WORD_BE] := '';
    ValueGrid.Cells[1, VALUE_ROW_SMALLINT_BE] := '';
    ValueGrid.Cells[2, VALUE_ROW_SMALLINT_BE] := '';
  end;

  // DWord, LongInt
  if idx <= FBufferSize - SizeOf(DWord) then begin
    for i:=0 to SizeOf(DWord)-1 do buf[i] :=
      FBuffer^[idx+i];
    ValueGrid.Cells[1, VALUE_ROW_DWORD] := IntToStr(LEToN(dw));
    ValueGrid.Cells[2, VALUE_ROW_DWORD] := Format('%d ... %d', [idx, idx+SizeOf(DWord)-1]);
    ValueGrid.Cells[1, VALUE_ROW_LONGINT] := IntToStr(LongInt(LEToN(dw)));
    ValueGrid.Cells[2, VALUE_ROW_LONGINT] := ValueGrid.Cells[2, VALUE_ROW_DWORD];
    ValueGrid.Cells[1, VALUE_ROW_DWORD_BE] := IntToStr(BEToN(dw));
    ValueGrid.Cells[2, VALUE_ROW_DWORD_BE] := Format('%d ... %d', [idx, idx+SizeOf(DWord)-1]);
    ValueGrid.Cells[1, VALUE_ROW_LONGINT_BE] := IntToStr(LongInt(BEToN(dw)));
    ValueGrid.Cells[2, VALUE_ROW_LONGINT_BE] := ValueGrid.Cells[2, VALUE_ROW_DWORD];
  end else begin
    ValueGrid.Cells[1, VALUE_ROW_DWORD] := '';
    ValueGrid.Cells[2, VALUE_ROW_DWORD] := '';
    ValueGrid.Cells[1, VALUE_ROW_LONGINT] := '';
    ValueGrid.Cells[2, VALUE_ROW_LONGINT] := '';
    ValueGrid.Cells[1, VALUE_ROW_DWORD_BE] := '';
    ValueGrid.Cells[2, VALUE_ROW_DWORD_BE] := '';
    ValueGrid.Cells[1, VALUE_ROW_LONGINT_BE] := '';
    ValueGrid.Cells[2, VALUE_ROW_LONGINT_BE] := '';
  end;

  // QWord, Int64
  if idx <= FBufferSize - SizeOf(QWord) then begin
    for i:=0 to SizeOf(QWord)-1 do
      buf[i] := FBuffer^[idx+i];
    ValueGrid.Cells[1, VALUE_ROW_QWORD] := Format('%d', [qw]);
    ValueGrid.Cells[2, VALUE_ROW_QWORD] := Format('%d ... %d', [idx, idx+SizeOf(QWord)-1]);
    ValueGrid.Cells[1, VALUE_ROW_INT64] := Format('%d', [Int64(qw)]);
    ValueGrid.Cells[2, VALUE_ROW_INT64] := ValueGrid.Cells[2, VALUE_ROW_QWORD];
    ValueGrid.Cells[1, VALUE_ROW_QWORD_BE] := Format('%d', [BEToN(qw)]);
    ValueGrid.Cells[2, VALUE_ROW_QWORD_BE] := Format('%d ... %d', [idx, idx+SizeOf(QWord)-1]);
    ValueGrid.Cells[1, VALUE_ROW_INT64_BE] := Format('%d', [Int64(BEToN(qw))]);
    ValueGrid.Cells[2, VALUE_ROW_INT64_BE] := ValueGrid.Cells[2, VALUE_ROW_QWORD];
  end else begin
    ValueGrid.Cells[1, VALUE_ROW_QWORD] := '';
    ValueGrid.Cells[2, VALUE_ROW_QWORD] := '';
    ValueGrid.Cells[1, VALUE_ROW_INT64] := '';
    ValueGrid.Cells[2, VALUE_ROW_INT64] := '';
    ValueGrid.Cells[1, VALUE_ROW_QWORD_BE] := '';
    ValueGrid.Cells[2, VALUE_ROW_QWORD_BE] := '';
    ValueGrid.Cells[1, VALUE_ROW_INT64_BE] := '';
    ValueGrid.Cells[2, VALUE_ROW_INT64_BE] := '';
  end;

  // Single
  if idx <= FBufferSize - SizeOf(single) then begin
    for i:=0 to SizeOf(single)-1 do buf[i] := FBuffer^[idx+i];
    ValueGrid.Cells[1, VALUE_ROW_SINGLE] := Format('%f', [sng]);
    ValueGrid.Cells[2, VALUE_ROW_SINGLE] := Format('%d ... %d', [idx, idx+SizeOf(Single)-1]);
  end else begin
    ValueGrid.Cells[1, VALUE_ROW_SINGLE] := '';
    ValueGrid.Cells[2, VALUE_ROW_SINGLE] := '';
  end;

  // Double
  if idx <= FBufferSize - SizeOf(double) then begin
    for i:=0 to SizeOf(double)-1 do buf[i] := FBuffer^[idx+i];
    ValueGrid.Cells[1, VALUE_ROW_DOUBLE] := Format('%f', [dbl]);
    ValueGrid.Cells[2, VALUE_ROW_DOUBLE] := Format('%d ... %d', [idx, idx+SizeOf(Double)-1]);
  end else begin
    ValueGrid.Cells[1, VALUE_ROW_DOUBLE] := '';
    ValueGrid.Cells[2, VALUE_ROW_DOUBLE] := '';
  end;

  // AnsiString
  if idx < FBufferSize then begin
    ls := Min(FBuffer^[idx], FBufferSize - idx - 1);
    SetLength(s, ls);
    i := idx + 1;
    j := 0;
    while (i < FBufferSize) and (j < Length(s)) do begin
      inc(j);
      s[j] := char(FBuffer^[i]);
      inc(i);
    end;
    SetLength(s, j);
    ValueGrid.Cells[1, VALUE_ROW_ANSISTRING] := s;
    ValueGrid.Cells[2, VALUE_ROW_ANSISTRING] := Format('%d ... %d', [idx, ls * SizeOf(char) + 1]);
  end else begin
    ValueGrid.Cells[1, VALUE_ROW_ANSISTRING] := '';
    ValueGrid.Cells[2, VALUE_ROW_ANSISTRING] := '';
  end;

  // PAnsiChar
  // Avoid buffer overrun
  if idx < FBufferSize then begin
    pa := PAnsiChar(@FBuffer^[idx]);
    ls := 0;
    while (pa^ <> #0) and (idx < FBufferSize) and (ls < MAX_LEN) do //pa - @FBuffer[0] < FBufferSize) do
    begin
      inc(pa);
      inc(ls);
    end;
    SetLength(s, ls);
    if ls = MAX_LEN then s := s + '...';
    Move(FBuffer^[idx], s[1], ls);
    ValueGrid.Cells[1, VALUE_ROW_PANSICHAR] := s;
    ValueGrid.Cells[2, VALUE_ROW_PANSICHAR] := Format('%d ... %d', [idx, idx + ls]);
  end else
  begin
    ValueGrid.Cells[1, VALUE_ROW_PANSICHAR] := '';
    ValueGrid.Cells[2, VALUE_ROW_PANSICHAR] := '';
  end;

  // WideString
  if idx < FBufferSize then begin
    ls := Min(FBuffer^[idx], (FBufferSize - idx - 1) div SizeOf(WideChar));
    if ls > MAX_LEN then ls := MAX_LEN;
    SetLength(sw, ls);
    j := 0;
    i := idx + 2;
    while (i < FBufferSize-1) and (j < Length(sw)) do begin
      buf[0] := FBuffer^[i];
      buf[1] := FBuffer^[i+1];
      inc(i, SizeOf(WideChar));
      inc(j);
      sw[j] := WideChar(w);
    end;
    SetLength(sw, j);
    ValueGrid.Cells[1, VALUE_ROW_WIDESTRING] := UTF8Encode(sw);
    ValueGrid.Cells[2, VALUE_ROW_WIDESTRING] := Format('%d ... %d', [idx, idx + (ls+1)*SizeOf(wideChar)]);
  end else begin
    ValueGrid.Cells[1, VALUE_ROW_WIDESTRING] := '';
    ValueGrid.Cells[2, VALUE_ROW_WIDESTRING] := '';
  end;

  // PWideChar
  // Avoid buffer overrun
  if idx < FBufferSize then begin
    pw := PWideChar(@FBuffer^[idx]);
    ls := 0;
    while (pw^ <> #0) and (pw - @FBuffer^[0] < FBufferSize-1) and (ls < MAX_LEN) do
    begin
      inc(pw);
      inc(ls);
    end;
    s := {%H-}WideCharLenToString(PWideChar(@FBuffer^[idx]), ls);
    if ls = MAX_LEN then s := s + '...';
    ValueGrid.Cells[1, VALUE_ROW_PWIDECHAR] := s;
    ValueGrid.Cells[2, VALUE_ROW_PWIDECHAR] := Format('%d ... %d', [idx, idx + ls * SizeOf(widechar)]);
  end else
  begin
    ValueGrid.Cells[1, VALUE_ROW_PWIDECHAR] := '';
    ValueGrid.Cells[2, VALUE_ROW_PWIDECHAR] := '';
  end;
end;

procedure TMainForm.ReadFromIni;
var
  ini: TCustomIniFile;
  W, H, L, T: Integer;
  Rct: TRect;
  i: Integer;
  s: String;
begin
  ini := TMemIniFile.Create(CalcIniName);
  try
    Rct := Screen.DesktopRect;
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    L := ini.ReadInteger('MainForm', 'Left', Left);
    T := ini.ReadInteger('MainForm', 'Top', Top);
    if W > Rct.Right - Rct.Left then W := Rct.Right - Rct.Left;
    if L + W > Rct.Right then L := Rct.Right - W;
    if L < Rct.Left then L := Rct.Left;
    if H > Rct.Bottom - Rct.Top then H := Rct.Bottom - Rct.Top;
    if T + H > Rct.Bottom then T := Rct.Bottom - H;
    if T < Rct.Top then T := Rct.Top;
    SetBounds(L, T, W, H);
    HexPanel.Width := ini.ReadInteger('MainForm', 'HexPanelWidth', HexPanel.Width);
    MainPageControl.PageIndex := ini.ReadInteger('MainForm', 'MainPageControl', 0);
    HexPageControl.PageIndex := ini.ReadInteger('MainForm', 'HexPageControl', 0);
  finally
    ini.Free;
  end;
end;

procedure TMainForm.ScanIFDs;
var
  tiffHeaderStart: Int64;

  // p is at the start of a IFD or SubIFD.
  procedure ScanIFD(p: Int64);
  var
    n: word;
    i: Integer;
    TagID: Word;
    val: DWord;
  begin
    if p > High(FBuffer^) then
      exit;

    n := PWord(@FBuffer^[p])^;
    if FMotorolaOrder then n := BEToN(n);
    inc(p, 2);
    for i:=0 to n-1 do begin
      TagID := PWord(@FBuffer^[p])^;
      if FMotorolaOrder then TagID := BEToN(TagID);
      inc(p, 2);  // --> type
      inc(p, 2);  // --> size
      inc(p, 4);  // --> value
      val := PDWord(@FBuffer^[p])^;
      if FMotorolaOrder then val := BEToN(val);
      if TagID = TAG_EXIF_OFFSET then begin
        IFDList[INDEX_EXIF] := val + tiffHeaderStart;
        ScanIFD(IFDList[INDEX_EXIF]);
      end else
      if val = TAG_GPS_OFFSET then begin
        IFDList[INDEX_GPS] := val + tiffHeaderStart;
        ScanIFD(IFDList[INDEX_GPS]);
      end else
      if val = TAG_INTEROP_OFFSET then begin
        IFDList[INDEX_INTEROP] := val + tiffHeaderStart;
        ScanIFD(IFDList[INDEX_INTEROP]);
      end;
      inc(p, 4)  // --> next tag
    end;
  end;

var
  p: Int64;
  n: word;
  offs: DWord;
begin
  FillChar(IFDList[0], SizeOf(IFDList), -1);

  tiffHeaderStart := FindTiffHeader;
  IFDList[0] := tiffHeaderStart + 8;
  ScanIFD(IFDList[0]);

  // Find IFD1
  p := IFDList[0];
  n := PWord(@FBuffer^[p])^;
  if FMotorolaOrder then n := BEToN(n);
  inc(p, 2 + n*12);
  offs := PDWord(@FBuffer^[p])^;
  if FMotorolaOrder then
    offs := BEToN(offs);
  if offs = 0 then
    IFDList[Index_IFD1] := -1  // there is no IFD1
  else
    IFDList[INDEX_IFD1] := offs + tiffHeaderStart;
end;

procedure TMainForm.StatusMsg(const AMsg: String);
begin
  Statusbar.SimpleText := AMsg;
  Statusbar.Refresh;
end;

procedure TMainForm.TbGotoAPP1ArrowClick(Sender: TObject);
begin
  UpdateIFDs;
end;

procedure TMainForm.TbGotoIFD(Sender: TObject);
var
  p: Int64;
  TiffHeaderOffs: Int64;
  ok: Boolean;

  function GetIFDIndex(AComponent: TComponent): PtrInt;
  begin
    Result := AComponent.Tag - (AcGotoTIFFHeader.Tag + 1);
  end;

begin
  p := FindMarker(MARKER_APP1);
  if p = -1 then begin
    StatusMsg('APP1 marker not found.');
    exit;
  end;

  // APP1 marker
  inc(p, 2 + 2 + Length('EXIF'#0#0));

  // TIFF header
  TiffHeaderOffs := p;
  inc(p, 8);  // Size of TIFF header

  p := IFDList[GetIFDIndex(TComponent(Sender))];
  {
  case TComponent(Sender).Tag of
    1001: p := FIFDList[0];   // IFD0
    1002: p := FIFDList[4];   // IFD1
    1003: p := FIFDList[1];   // EXIF
    1004: p := FIFDList[3];   // GPS
    1005: p := FIFDList[2];   // Interop;
  end;
  }

  if p = -1 then
  begin
    StatusMsg(TAction(Sender).Caption + ' does not exit.');
    exit;
  end;

  GotoOffset(p);

  case GetIFDIndex(TComponent(Sender)) of
    INDEX_IFD0    : ok := DisplayIFD(p, TiffHeaderOffs, 'IFD0 (Image file directory 0)');
    INDEX_EXIF    : ok := DisplayIFD(p, TIFFHeaderOffs, 'EXIF SubIFD (Image file subdirectory)');
    INDEX_INTEROP : ok := DisplayIFD(p, TiffHeaderOffs, 'Interoperability SubIFD (Image file subdirectory)');
    INDEX_GPS     : ok := DisplayIFD(p, TiffHeaderOffs, 'GPS SubIFD (Image file subdirectory)');
    INDEX_IFD1    : ok := DisplayIFD(p, TiffHeaderOffs, 'IFD1 (Image file directory 1 - thumbnail)');
    else            ok := true;
  end;
  if not ok then
    StatusMsg('ERROR');

  (*
  // IFD0
  if TComponent(Sender).Tag = 1001 then begin
    GotoOffset(p);
    if not DisplayIFD(p, TiffHeaderOffs, 'IFD0 (Image file directory 0)') then
      Statusbar.SimpleText := 'ERROR';
    exit;
  end else
  // IFD1
  if TComponent(Sender).Tag = 1002 then begin
    GotoOffset(p);
    if not GotoNextIFD(p) then begin
      Statusbar.SimpleText := 'No IFD1';
      exit;
    end;
    GotoOffset(p);
    if not DisplayIFD(p, TiffHeaderOffs, 'IFD1 (Image file directory 1)') then
      Statusbar.SimpleText := 'ERROR';
    exit;
  end else
  // EXIF SubIFD
  if TComponent(Sender).Tag = 1003 then begin
    GotoOffset(p);
    if not GotoSubIFD(TAG_EXIF_OFFSET, p, TiffHeaderOffs) then begin
      Statusbar.SimpleText := 'No EXIF SubIFD';
      exit;
    end;
    GotoOffset(p);
    if not DisplayIFD(p, TIFFHeaderOffs, 'EXIF SubIFD (Image file subdirectory)') then
      Statusbar.SimpleText := 'ERROR';
    exit;
  end else
  // GPS SubIFD
  if TComponent(Sender).Tag = 1004 then begin
    GotoOffset(p);
    if not GotoSubIFD(TAG_GPS_OFFSET, p, TiffHeaderOffs) then begin
      Statusbar.SimpleText := 'No GPS SubIFD';
      exit;
    end;
    GotoOffset(p);
    if not DisplayIFD(p, TIFFHeaderOffs, 'GPS SubIFD (Image file subdirectory)') then
      Statusbar.SimpleText := 'ERROR';
    exit;
  end else
  // Interoperability SubIFD
  if TComponent(Sender).Tag = 1005 then begin
    GotoOffset(p);
    if not GotoSubIFD(TAG_INTEROP_OFFSET, p, TiffHeaderOffs) then begin
      Statusbar.SimpleText := 'No Interoperability SubIFD';
      exit;
    end;
    GotoOffset(p);
    if not DisplayIFD(p, TIFFHeaderOffs, 'Interoperability SubIFD (Image file subdirectory)') then
      Statusbar.SimpleText := 'ERROR';
    exit;
  end;
  *)
end;

procedure TMainForm.TbGotoMarker(Sender: TObject);
var
  m: Byte;
  p: Int64;
  ok: Boolean;
begin
  if      Sender = TbGotoSOI  then
    m := MARKER_SOI
  else if Sender = TbGotoEOI  then
    m := MARKER_EOI
  else if Sender = TbGotoAPP0 then
    m := MARKER_APP0
  else if Sender = TbGotoAPP1 then
    m := MARKER_APP1
  else if Sender = TbGotoAPP2 then
    m := MARKER_APP2
  else if Sender = TbGotoDAC then
    m := MARKER_DAC
  else if Sender = TbGotoDHT then
    m := MARKER_DHT
  else if Sender = TbGotoDQT then
    m := MARKER_DQT
  else if Sender = TbGotoJPG then
    m := MARKER_JPG
  else if Sender = TbGotoSOF0 then
    m := MARKER_SOF0
  else if Sender = TbGotoSOF1 then
    m := MARKER_SOF1
  else if Sender = TbGotoSOF2 then
    m := MARKER_SOF2
  else if Sender = TbGotoSOF3 then
    m := MARKER_SOF3
  else if Sender = TbGotoSOF5 then
    m := MARKER_SOF5
  else if Sender = TbGotoSOF6 then
    m := MARKER_SOF6
  else if Sender = TbGotoSOF7 then
    m := MARKER_SOF7
  else if Sender = TbGotoSOF9 then
    m := MARKER_SOF9
  else if Sender = TbGotoSOF10 then
    m := MARKER_SOF10
  else if Sender = TbGotoSOF11 then
    m := MARKER_SOF11
  else if Sender = TbGotoSOF13 then
    m := MARKER_SOF12
  else if Sender = TbGotoSOS then
    m := MARKER_SOS
  else if Sender = TbGotoCOM then
    m := MARKER_COM
  else
    raise Exception.Create('Marker unknown');

  p := FindMarker(m);
  if p >= 0 then begin
    ok := DisplayMarker(p);
    if not ok then
      StatusMsg('ERROR');
  end else
    StatusMsg('Marker not found.');
end;

procedure TMainForm.TbGotoTIFFHeaderClick(Sender: TObject);
var
  p: Int64;
begin
  p := FindTIFFHeader;
//  p := FindMarker(MARKER_APP1);
  if p = -1 then
    StatusMsg('TIFF header not found.')
  else begin
//    inc(p, 2 + 2 + Length('EXIF'#0#0));
    GotoOffset(p);
    DisplayTiffHeader(p);
  end;
end;

procedure TMainForm.TbNextSegmentClick(Sender: TObject);
var
  p: Int64;
begin
  p := FindNextMarker;

  if p >= 0 then begin
    GotoOffset(p);
    if not DisplayMarker(p) then
      StatusMsg('ERROR');
  end else
    StatusMsg('Marker not found.');
end;

procedure TMainForm.UpdateIFDs;
var
  i: Integer;
  ac: TAction;
  startTag: PtrInt;
  endTag: PtrInt;
begin
  startTag := AcGotoTiffHeader.Tag + 1;
  endtag := startTag + High(IFDList);
  for i:=0 to ActionList.ActionCount-1 do begin
    ac := TAction(ActionList[i]);
    if (ac.Tag >= startTag) and (ac.Tag <= endTag) then
      ac.Enabled := IFDList[ac.Tag - startTag] > -1;
  end;
end;

procedure TMainForm.UpdateMarkers;
var
  i: Integer;
  tb: TToolbutton;
  p: Int64;
begin
  for i:=0 to HexToolbar.ButtonCount-1 do begin
    tb := HexToolbar.Buttons[i];
    case tb.Caption of
      'SOI'   : tb.Enabled := FindMarker(MARKER_SOI)   <> -1;
      'APP0'  : tb.Enabled := FindMarker(MARKER_APP0)  <> -1;
      'APP1'  : tb.Enabled := FindMarker(MARKER_APP1)  <> -1;
      'APP2'  : tb.Enabled := FindMarker(MARKER_APP2)  <> -1;
      'COM'   : tb.Enabled := FindMarker(MARKER_COM)   <> -1;
      'SOS'   : tb.Enabled := FindMarker(Marker_SOS)   <> -1;
      'EOI'   : tb.Enabled := FindMarker(Marker_EOI)   <> -1;
      'SOF0'  : tb.Enabled := FindMarker(MARKER_SOF0)  <> -1;
      'SOF1'  : tb.Enabled := FindMarker(MARKER_SOF1)  <> -1;
      'SOF2'  : tb.Enabled := FindMarker(Marker_SOF2)  <> -1;
      'SOF3'  : tb.Enabled := FindMarker(Marker_SOF3)  <> -1;
      'DHT'   : tb.Enabled := FindMarker(MARKER_DHT)   <> -1;
      'SOF5'  : tb.Enabled := FindMarker(MARKER_SOF5)  <> -1;
      'SOF6'  : tb.Enabled := FindMarker(MARKER_SOF6)  <> -1;
      'SOF7'  : tb.Enabled := FindMarker(MARKER_SOF7)  <> -1;
      'JPG'   : tb.Enabled := FindMarker(MARKER_JPG)   <> -1;
      'SOF9'  : tb.Enabled := FindMarker(MARKER_SOF9)  <> -1;
      'SOF10' : tb.Enabled := FindMarker(MARKER_SOF10) <> -1;
      'SOF11' : tb.Enabled := FindMarker(MARKER_SOF11) <> -1;
      'SOF13' : tb.Enabled := FindMarker(MARKER_SOF13) <> -1;
      'SOF14' : tb.Enabled := FindMarker(MARKER_SOF14) <> -1;
      'SOF15' : tb.Enabled := FindMarker(MARKER_SOF15) <> -1;
      'DAC'   : tb.Enabled := FindMarker(MARKER_DAC)   <> -1;
      'DQT'   : tb.Enabled := FindMarker(MARKER_DQT)   <> -1;
    end;
  end;
  TbNextSegment.Enabled := FBuffer^[FCurrOffset] = $FF;
end;

procedure TMainForm.UpdateStatusbar;
begin
  if FCurrOffset > -1 then
    StatusMsg(Format('HexViewer offset: %d ($%x)', [FCurrOffset, FCurrOffset]))
  else
    StatusMsg('');
end;

procedure TMainForm.ValueGridClick(Sender: TObject);
var
  sel: TKHexEditorSelection;
  n: Integer;
begin
  sel := HexEditor.SelStart;

  n := GetValueGridDataSize;
  if n > 0 then begin
    sel.Digit := 0;
    HexEditor.SelStart := sel;
    inc(sel.Index, n-1);
    sel.Digit := 1;
    HexEditor.SelEnd := sel;
  end else
    HexEditor.SelEnd := sel;
end;

procedure TMainForm.WriteToIni;
var
  ini: TCustomIniFile;
  i: Integer;
begin
  ini := TMemIniFile.Create(CalcIniName);
  try
    ini.WriteInteger('MainForm', 'Left', Left);
    ini.Writeinteger('MainForm', 'Top', Top);
    ini.WriteInteger('MainForm', 'Width', Width);
    ini.WriteInteger('MainForm', 'Height', Height);
    ini.WriteInteger('MainForm', 'HexPanelWidth', HexPanel.Width);
    ini.WriteInteger('MainForm', 'HexPageControl', HexPageControl.PageIndex);
    ini.WriteInteger('MainForm', 'MainPageControl', MainPageControl.PageIndex);
  finally
    ini.Free;
  end;
end;


end.

