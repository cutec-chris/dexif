unit mrtmain;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
 {$IFDEF FPC}
  FileUtil,
 {$ELSE}
  Windows, {$IFDEF UNICODE}ImageList,{$ENDIF}
 {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, ImgList,
  dMetaData;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    BtnReadFiles: TButton;
    BtnCreateTxtFiles: TButton;
    BtnRunTest: TButton;
    EdImageDir: TEdit;
    Label1: TLabel;
    Memo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    FileTreeView: TTreeView;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    ImageList1: TImageList;
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

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

uses
  Variants,
 {$IFDEF FPC}
  Process,
 {$ELSE}
  ShellApi,
 {$ENDIF}
  dGlobal;

{ TMainForm }

const
  EXIFTOOL_CMD = '..\..\..\tools\Exiftool.exe';

  IMG_INDEX_WORKING = 0;
  IMG_INDEX_FAIL = 1;
  IMG_INDEX_IGNORE = 1;
  IMG_INDEX_EXIF = 2;
  IMG_INDEX_SUCCESS = 3;

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
        node.ImageIndex := IMG_INDEX_IGNORE;
        tagFile := ChangeFileExt(imgDir + info.Name, '.txt');
        if FileExists(tagFile) then
        begin
          L := TStringList.Create;
          try
            L.LoadFromFile(tagFile);
            if ExtractRefTags(node, L) then
              node.ImageIndex := IMG_INDEX_EXIF;
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
  Label1.Show;
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
    node.DeleteChildren;
    node.ImageIndex := -1;
    node := node.GetNextSibling;
  end;

  node := FileTreeView.Items.GetFirstNode;
  while (node <> nil) do begin
    node.ImageIndex := IMG_INDEX_WORKING;
    Application.ProcessMessages;
    if not CreateRefTags(node, imgDir + node.Text) then begin
      node.ImageIndex := IMG_INDEX_IGNORE;
    end else
      node.ImageIndex := IMG_INDEX_EXIF;
    node.SelectedIndex := node.ImageIndex;
    node := node.GetNextSibling;
  end;
end;

function TMainForm.CreateRefTags(ANode: TTreeNode; AFileName: String): Boolean;
var
  destFile: String;
  output: String;
  L: TStringList;
{$IFNDEF FPC}
  params: String;
  res: Integer;
const
  DEG_SYMBOL: ansistring = #176;
{$ENDIF}
begin
  Result := false;
  destFile := ChangeFileExt(AFileName, '.txt');

  {$IFDEF FPC}
  if RunCommand(EXIFTOOL_CMD, ['-a', '-H', '-s', '-G', '-c', '"%d° %d'' %.2f"\"', AFileName], output) then
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
        ANode.ImageIndex := IMG_INDEX_EXIF else
        ANode.ImageIndex := IMG_INDEX_IGNORE;
      ANode.SelectedIndex := ANode.ImageIndex;
      L.SaveToFile(destFile);
      Result := true;
    finally
      L.Free;
    end;
  end;
  {$ELSE}
//  params := '/c ' + EXIFTOOL_CMD + ' -a -H -s -G -c "%d' + DEG_SYMBOL + ' %d'' %.2f"\"' + AFileName + ' > ' + destFile;
  params := '/c ' + EXIFTOOL_CMD + ' -a -H -s -G -c "%d° %d'' %.2f"\"' + AFileName + ' > ' + destFile;
  res := ShellExecute(Application.Handle, 'open', PChar('cmd'), PChar(params), '', SW_HIDE);
  if res <= 32 then
    exit;
  L := TStringList.Create;
  try
    L.LoadFromFile(destFile);
    if ExtractRefTags(ANode, L) then
      ANode.ImageIndex := IMG_INDEX_EXIF else
      ANode.ImageIndex := IMG_INDEX_IGNORE;
    ANode.SelectedIndex := ANode.ImageIndex;
    Result := true;
  finally
    L.Free;
  end;
  {$ENDIF}
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
  v: Variant;
begin
  if ANode.Count = 0 then begin
    Log('Skipping image "' + ANode.Text + '":');
    Log('    No EXIF data found by ExifTool.');
    Log('');
    exit;
  end;

  Log('Testing image "' + ANode.Text + '":');
  imgData := TImgData.Create;
  try
    imgData.ProcessFile(GetImageDir + ANode.Text);
    if not imgData.HasExif then begin
      Log('Skipping "' + ANode.Text + '":');
      Log('    No EXIF data found by dExif.');
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
        if pos('Thumbnail', tagName) > 0 then begin
          lTag := imgData.ExifObj.ThumbTagByID[tagID];
          if (lTag.Name = '') or SameText(lTag.Name, 'Unknown') then
            currTagValue := ''
          else
            currTagValue := imgData.ExifObj.ThumbTagValueAsString[lTag.Name];
        end else begin
          lTag := imgData.ExifObj.TagById[tagID];
          if (lTag.Name = '') or SameText(lTag.Name, 'Unknown') then
            currTagValue := ''
          else
            currTagValue := imgData.ExifObj.TagValueAsString[ltag.Name];
        end;
      end else
        currTagValue := imgData.ExifObj.TagValueAsString[tagName];
      expectedTagvalue := trim(Copy(s, p+1, MaxInt));

      if SameText(expectedTagValue, currTagValue) then
        node.ImageIndex := IMG_INDEX_SUCCESS
      else begin
        Log('    Mismatching tag "' + Format('[$%.4x] %s', [tagID, tagName]) + '"');
        Log('        expected: ' + expectedTagValue);
        Log('        found: ' + currTagValue);
        node.ImageIndex := IMG_INDEX_FAIL;
        node.Text := tagname + ': ' + expectedTagValue + ' --> found: ' + currTagValue;
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

  FileTreeView.Invalidate;

end;

end.

