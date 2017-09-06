unit mrtmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, EditBtn,
  dExif;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    BtnReadFiles: TButton;
    BtnCreateTxtFiles: TButton;
    BtnRunTest: TButton;
    EdImageDir: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Memo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    FileTreeView: TTreeView;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    procedure BtnReadFilesClick(Sender: TObject);
    procedure BtnRunTestClick(Sender: TObject);
    procedure BtnCreateTxtFilesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTotalCount: Integer;
    FMismatchCount: Integer;
    function CreateRefTags(ANode: TTreeNode; AFileName: String): Boolean;
    function ExtractRefTags(ANode: TTreeNode; AList: TStringList): Boolean;
    function GetImageDir: String;
    procedure Log(AMsg: String);
    procedure RunTest(ANode: TTreeNode);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Variants, Process,
  dGlobal;

{ TMainForm }

const
  EXIFTOOL_CMD = '..\..\..\tools\Exiftool.exe';

procedure TMainForm.BtnReadFilesClick(Sender: TObject);
var
  info: TSearchRec;
  imgDir: String;
  node: TTreeNode;
  tagFile: String;
  L: TStringList;
begin
  FileTreeView.Items.Clear;
  imgDir := GetImageDir;
  if FindFirst(imgDir + '*.jpg', faAnyFile and faDirectory, info) = 0 then
  begin
    repeat
      if (info.Name <> '.') and (info.Name <> '..') and (info.Attr and faDirectory = 0) then
      begin
        node := FileTreeview.Items.AddChild(nil, ExtractFileName(info.Name));
        node.ImageIndex := 1;
        tagFile := ChangeFileExt(imgDir + info.Name, '.txt');
        if FileExists(tagFile) then
        begin
          L := TStringList.Create;
          try
            L.LoadFromFile(tagFile);
            if ExtractRefTags(node, L) then
              node.ImageIndex := 2;
          finally
            L.Free;
          end;
        end;
        node.SelectedIndex := node.ImageIndex;
      end;
    until FindNext(info) <> 0;
  end;
  FindClose(info);
end;

procedure TMainForm.BtnRunTestClick(Sender: TObject);
var
  node: TTreeNode;
begin
  FMismatchCount := 0;
  FTotalCount := 0;
  node := FileTreeView.Items.GetFirstNode;
  while node <> nil do begin
    RunTest(node);
    node := node.GetNextSibling;
  end;
  Label1.Caption := Format('%d mismatches out of %d tests (%.0f%%)', [
    FMismatchCount, FTotalCount, FMismatchCount/FTotalCount*100]);
end;

procedure TMainForm.BtnCreateTxtFilesClick(Sender: TObject);
var
  imgDir: String;
  node: TTreeNode;
begin
  if not FileExists(EXIFTOOL_CMD) then
  begin
    MessageDlg(Format('Program "ExifTool" not found in folder "%s".', [
      ExtractFileDir(ExpandFilename(EXIFTOOL_CMD))
      ]), mtError, [mbOK], 0
    );
    exit;
  end;

  imgDir := GetImageDir;

  node := FileTreeView.Items.GetFirstNode;
  while (node <> nil) do begin
    node.ImageIndex := -1;
    node := node.GetNextSibling;
  end;

  node := FileTreeView.Items.GetFirstNode;
  while (node <> nil) do begin
    node.ImageIndex := 0;
    Application.ProcessMessages;
    if not CreateRefTags(node, imgDir + node.Text) then begin
      node.ImageIndex := 1;
    end else
      node.ImageIndex := 2;
    node.SelectedIndex := node.ImageIndex;
    node := node.GetNextSibling;
  end;
end;

function TMainForm.CreateRefTags(ANode: TTreeNode; AFileName: String): Boolean;
var
  prc: TProcess;
  destFile: String;
  output: String;
  fs: TFileStream;
  L: TStringList;
begin
  Result := false;
  if RunCommand(EXIFTOOL_CMD, ['-a', '-H', '-s', '-G', AFileName], output) then
    // -a ... extract all tags, also duplicates.
    // -H ... extract hex tag id if possible
    // -s ... short tag name (hopefully this is the dExif tag name)
    // -G ... print group name for each tag
  begin
    if (output = '') then
      exit;

    destFile := ChangeFileExt(AFileName, '.txt');
    L := TStringList.Create;
    try
      L.Text := output;
      if ExtractReftags(ANode, L) then
        ANode.ImageIndex := 2 else
        ANode.ImageIndex := 1;
      ANode.SelectedIndex := ANode.ImageIndex;
      L.SaveToFile(destFile);
      Result := true;
    finally
      L.Free;
    end;
  end;
end;

function TMainForm.ExtractRefTags(ANode: TTreeNode; AList: TStringList): Boolean;
const
  GROUP_START = 1;
  GROUP_LEN   = 15;
  TAGID_START = 19;
  TAGID_LEN   =  4;
  NAME_START  = 24;
  NAME_LEN    = 32;
  VALUE_START = 58;
var
  i: Integer;
  p: Integer;
  s: String;
  sGroup: String;
  sTagID: String;
  sTagName: String;
  sTagValue: String;
  tagID: Word;
  node: TTreeNode;
begin
  Result := false;
  for i:=0 to AList.Count-1 do begin
    s := AList[i];
    sGroup := trim(Copy(s, GROUP_START, GROUP_LEN));
    sTagID := trim(Copy(s, TAGID_START, TAGID_LEN));
    sTagName := trim(Copy(s, NAME_START, NAME_LEN));
    sTagValue := trim(Copy(s, VALUE_START, MaxInt));

    if sTagID = '-' then
      Continue;

    // So far, consider only EXIF-Tag
    if sGroup <> '[EXIF]' then
      Continue;

    tagID := StrToInt('$' + sTagID);
    node := ANode.Owner.AddChild(ANode, sTagName + ': ' + sTagValue);
    node.Data := Pointer(PtrInt(tagID));
  end;
  Result := ANode.Count > 0;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  if EdImageDir.Text <> '' then
    BtnReadFilesClick(nil);
end;

function TMainForm.GetImageDir: String;
begin
  Result := IncludeTrailingPathDelimiter(ExpandFilename(EdImageDir.Text));
  caption := Result;
end;

procedure TMainForm.Log(AMsg: String);
begin
  Memo.Lines.Add(AMsg);
  Memo.SelStart := Length(Memo.Lines.Text);
end;

procedure TMainForm.RunTest(ANode: TTreeNode);
var
  imgName: String;
  imgData: TImgData;
  tagName: String;
  expectedTagValue: String;
  currTagValue: String;
  s: String;
  p: Integer;
  node: TTreeNode;
  tagID: Word;
  lTag: TTagEntry;
begin
  if ANode.Count = 0 then begin
    Log('Skipping image "' + ANode.Text + '": No EXIF data found by ExifTool.');
    Log('');
    exit;
  end;

  Log('Testing image "' + ANode.Text + '":');
  imgData := TImgData.Create;
  try
    imgData.ProcessFile(GetImageDir + ANode.Text);
    if not imgData.HasExif then begin
      Log('Skipping "' + ANode.Text + '": no EXIF data found.');
      Log('');
      exit;
    end;

    node := ANode.GetFirstChild;
    while node <> nil do begin
      s := node.Text;
      p := pos(':', s);
      if p = 0 then begin
        node := node.GetNextSibling;
        Log('Skipping tag "' + s + '": Has no value');
        continue;
      end;

      tagID := PtrInt(node.Data);
      tagName := trim(Copy(s, 1, p-1));
      lTag := imgData.ExifObj.TagByName[tagName];
      if (lTag.Name = '') or SameText(lTag.Name, 'Unknown') then begin
        // tag not found by name
        tagID := PtrInt(node.Data);
        tagName := Format('[$%.4x] %s', [tagID, tagName]);
        currTagValue := imgData.ExifObj.TagValueAsStringByID[tagID];
      end else
        currTagValue := imgData.ExifObj.TagValueAsString[tagName];
      expectedTagvalue := trim(Copy(s, p+1, MaxInt));

      if SameText(expectedTagValue, currTagValue) then
        node.ImageIndex := 3
      else begin
        Log('    Mismatching tag "' + tagName + '"');
        Log('        expected: ' + expectedTagValue);
        Log('        found: ' + currTagValue);
        node.ImageIndex := 1;
        inc(FMismatchCount);
      end;
      node.SelectedIndex := node.ImageIndex;

      node := node.GetNextSibling;
      inc(FTotalCount);
    end;
  finally
    Log('');
    imgData.Free;
  end;

end;

end.

