unit TagPickU;

interface

uses
 {$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
 {$ELSE}
  Windows, Messages,
 {$ENDIF}
  SysUtils, Classes, Graphics, Forms, Dialogs, Controls, StdCtrls, Buttons,
  dEXIF;

type
  TTagPickDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    SrcList: TListBox;
    DstList: TListBox;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    IncludeBtn: TSpeedButton;
    IncAllBtn: TSpeedButton;
    ExcludeBtn: TSpeedButton;
    ExAllBtn: TSpeedButton;
    procedure IncludeBtnClick(Sender: TObject);
    procedure ExcludeBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExcAllBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SrcListDblClick(Sender: TObject);
    procedure DstListDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure MoveSelected(List: TCustomListBox; Items: TStrings);
    procedure SetItem(List: TListBox; Index: Integer);
    function GetFirstSelection(List: TCustomListBox): Integer;
    procedure SetButtons;
  end;

var
  TagPickDlg: TTagPickDlg;

implementation

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.DFM}
{$ENDIF}

uses
  dIPTC, ViewIPTC;

const
  LB_ERR = -1;

procedure TTagPickDlg.IncludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(SrcList);
  MoveSelected(SrcList, DstList.Items);
  SetItem(SrcList, Index);
end;

procedure TTagPickDlg.ExcludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(DstList);
  MoveSelected(DstList, SrcList.Items);
  SetItem(DstList, Index);
end;

procedure TTagPickDlg.IncAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to SrcList.Items.Count - 1 do
    DstList.Items.AddObject(SrcList.Items[I],
      SrcList.Items.Objects[I]);
  SrcList.Items.Clear;
  SetItem(SrcList, 0);
end;

procedure TTagPickDlg.ExcAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DstList.Items.Count - 1 do
    SrcList.Items.AddObject(DstList.Items[I], DstList.Items.Objects[I]);
  DstList.Items.Clear;
  SetItem(DstList, 0);
end;

procedure TTagPickDlg.MoveSelected(List: TCustomListBox; Items: TStrings);
var
  I: Integer;
begin
  for I := List.Items.Count - 1 downto 0 do
    if List.Selected[I] then
    begin
      Items.AddObject(List.Items[I], List.Items.Objects[I]);
      List.Items.Delete(I);
    end;
end;

procedure TTagPickDlg.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := SrcList.Items.Count = 0;
  DstEmpty := DstList.Items.Count = 0;
  IncludeBtn.Enabled := not SrcEmpty;
  IncAllBtn.Enabled := not SrcEmpty;
  ExcludeBtn.Enabled := not DstEmpty;
  ExAllBtn.Enabled := not DstEmpty;
end;

function TTagPickDlg.GetFirstSelection(List: TCustomListBox): Integer;
begin
  for Result := 0 to List.Items.Count - 1 do
    if List.Selected[Result] then Exit;
  Result := LB_ERR;
end;

procedure TTagPickDlg.SetItem(List: TListBox; Index: Integer);
var
  MaxIndex: Integer;
begin
  with List do
  begin
    SetFocus;
    MaxIndex := List.Items.Count - 1;
    if Index = LB_ERR then Index := 0
    else if Index > MaxIndex then Index := MaxIndex;
    Selected[Index] := True;
  end;
  SetButtons;
end;

procedure TTagPickDlg.FormShow(Sender: TObject);
var idx:integer;
begin
  idx := 0;
  srcList.Clear;
  dstList.Clear;
// for some bizzarre reason, the for loop executes
// backwards! [only in the debugger - BUG still in D6  :-( ]
//  for idx := 0 to (TAGCNT-1) do
  while (idx < IPTCTAGCNT) do
  begin
    if IPTCTable[idx].name <> 'SKIP' then
    begin
      if ((ImgData <> nil) and (ImgData.HasIPTC) and
          (ImgData.IptcObj.LookupTag(IPTCTable[idx].name) >= 0))
        then dstList.Items.add(IPTCTable[idx].name)
        else srcList.Items.add(IPTCTable[idx].name);
    end;
    inc(idx);
  end;
  SetButtons;
end;

procedure TTagPickDlg.OKBtnClick(Sender: TObject);
var i:integer;
begin
  if ImgData.IptcObj = nil then
    ImgData.CreateIPTCObj;
  for i := 0 to dstList.Items.Count-1 do
    ImgData.IptcObj.AddTag(dstList.Items.Strings[i]);
  for i := 0 to srcList.Items.Count-1 do
    ImgData.IptcObj.RemoveTag(srcList.Items.Strings[i]);
end;

procedure TTagPickDlg.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := width;
  Constraints.MaxWidth := width;
  Constraints.MinHeight := Height;
end;

procedure TTagPickDlg.SrcListDblClick(Sender: TObject);
begin
  IncludeBtn.Click;
end;

procedure TTagPickDlg.DstListDblClick(Sender: TObject);
begin
  ExcludeBtn.Click;
end;

end.
