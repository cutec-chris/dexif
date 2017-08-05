//--------------------------------------------------------------------------
// Test program: Demonstration of dEXIF module use
//    Has two modes: 1)  Dump all detail regarding a specific file
//                   2)  Show summary of all jpgs in directory tree
//
// Release history:
//   Gerry McGuire, March - April 7, 2001 - Initial Beta Release - 0.8
//   Gerry McGuire, September 3, 2001      - Second Beta Release - 0.9
//
//--------------------------------------------------------------------------
unit MainView;

{$IFDEF LCL}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF DELPHI}
  Windows, Messages, SHLObj,
  Jpeg,
  {$ELSE}
  LCLIntf, LCLType,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtDlgs,
  StdCtrls, ComCtrls, ExtCtrls,
  msData, dExif, dIPTC;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnLoad: TButton;
    btnWriteChDate: TButton;
    NoThumbInfo: TLabel;
    pdlg: TOpenPictureDialog;
    Memo: TMemo;
    StatusBar: TStatusBar;
    cbClearOnLoad: TCheckBox;
    btnAbout: TButton;
    btnTree: TButton;
    PBar: TProgressBar;
    cbVerbose: TCheckBox;
    btnWriteSmall: TButton;
    JpegOut: TSavePictureDialog;
    cbDecode: TCheckBox;
    btnComment: TButton;
    ThumbnailImage: TImage;
    procedure btnAboutClick(Sender: TObject);
    procedure btnCommentClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnTreeClick(Sender: TObject);
    procedure btnWriteChDateClick(Sender: TObject);
    procedure btnWriteSmallClick(Sender: TObject);
    procedure cbDecodeClick(Sender: TObject);
    procedure cbVerboseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure CleanupPreview;
    procedure DumpSections;
    procedure DumpEXIF;
    procedure DumpMSpecific;
    procedure DumpThumb;
    procedure ReadExifDir(start:string;justcnt:boolean);
    procedure WriteToMemo(s: string);
  public
    exifBuffer: string;
    lastDir: string;
    jpgcnt: integer;
    scrar: real;
    flist: TStringList;
    Verbose:boolean;
  end;

var
  Form1: TForm1;
  ImgData:TImgData;

implementation

{$IFDEF DELPHI}
 {$R *.dfm}
{$ELSE}
 {$R *.lfm}
{$ENDIF}

uses
  About; //, ShellApi;

const
  {$IFDEF DELPHI}
  crlf = #13#10;
  {$ELSE}
  crlf = LineEnding;
  {$ENDIF}
  progName = 'DExView';

  var DirBrowseName : string;

{$IFDEF DELPHI}
  // Direct call to undocumented Windows function
  PROCEDURE FreePIDL;  EXTERNAL 'Shell32.DLL'  INDEX 155;

  FUNCTION BrowseCallback(Wnd :  hWND;
                        MessageID:  UINT;
                        Param:  LPARAM;
                        Data:    LPARAM):  INTEGER STDCALL;
    VAR
      Name:  ARRAY[0..MAX_PATH] OF CHAR;
      pIDL:  pItemIDList;
      s   :  STRING;
  BEGIN
    CASE  MessageID OF
      BFFM_INITIALIZED: BEGIN
        IF (LENGTH(DirBrowseName) > 0) AND
          DirectoryExists(DirBrowseName) THEN
                SendMessage(Wnd, BFFM_SETSELECTION, Integer(TRUE),
                Integer( pChar(DirBrowseName) ) );
        END;
    ELSE      // ignore
    END;
    pIDL := Pointer(Param);
    s := '';
    IF   Assigned(PIDL) THEN
      SHGetPathFromIDList(pIDL, Name);
    RESULT := 0
  END; // BrowseCallback

  function BrowseForDir(handle:hwnd;dirname:string):string;
  VAR
    BrowseInfo :  TBrowseInfo;
    ItemIDList :  pItemIDList;   // some would use PIDL here
    DisplayName:  ARRAY[0..MAX_PATH] OF CHAR;
  begin
    result := '';
    DirBrowseName := ExcludeTrailingBackslash(DirName);
    BrowseInfo.hwndOwner := Handle;
    BrowseInfo.pidlRoot := NIL;
    BrowseInfo.pszDisplayName := @DisplayName[0];
    BrowseInfo.lpszTitle := 'Select Directory';
    BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS;
    BrowseInfo.lpfn := BrowseCallback;
    BrowseInfo.lParam := 0;
    BrowseInfo.iImage := 0;
    // Display browse folder set as the return value to itemlist
   {$IFDEF DELPHI}
    ItemIDList := SHBrowseForFolder(BrowseInfo);
   {$ENDIF}
   {$IFDEF LCL}
    ItemIDList := SHBrowseForFolder(@BrowseInfo);
   {$ENDIF}
    TRY // Get directory from the ItemIDList
      IF   Assigned(ItemIDList) THEN
        IF  SHGetPathFromIDList(ItemIDList, DisplayName) THEN
        BEGIN
          result := DisplayName;
        END;
    FINALLY
//      FreePIDL;  //  Causes crash if left in
    END
  end;
{$ENDIF}

function Clock: TDateTime;
begin
  Result := Now();
end;


{ TForm1 }

procedure TForm1.btnAboutClick(Sender: TObject);
begin
  AboutBox.FormSetup(ProgName, dEXIFVersion);
  AboutBox.ShowModal;
end;

procedure TForm1.btnCommentClick(Sender: TObject);
var
  cmt:string;
begin
  if ImgData.ExifObj.CommentPosn = 0 then
    ShowMessage('No EXIF comment field detected')
  else
  begin
    cmt := InputBox('Enter EXIF comment:',
       'Enter a new comment (limited to '+
       inttostr(ImgData.ExifObj.CommentSize)+' characters )',
       ImgData.ExifObj.Comments
    );
    if ImgData.ExifObj.Comments <> cmt then
    begin
      ImgData.ExifObj.SetExifComment(cmt);
      Memo.Lines.Add('Comment set to: '+cmt);
      ImgData.ExifObj.Comments := cmt;
    end;
  end;
end;

procedure TForm1.btnLoadClick(Sender: TObject);
var
  i: integer;
  ts: TStringList;
  tmp: string;
  jpegThumb: TJpegImage;
  {$IFNDEF DELPHI}
  stream: TStream;
  {$ENDIF}
begin
  btnWriteSmall.Enabled := false;
  btnWriteChDate.Enabled := false;
  btnComment.enabled := false;
  if pdlg.Execute then
  begin
    CleanupPreview;
    StatusBar.SimpleText := 'Info for ' + pdlg.FileName;
    if verbose
      then ExifTrace := 1
      else ExifTrace := 0;
    if cbClearOnLoad.Checked then
      Memo.Clear;

    ImgData.BuildList := GenAll;  // on by default anyway

    ImgData.ProcessFile(pdlg.FileName);

    if Verbose then
      DumpSections;

    DumpExif;

    if not ImgData.HasMetaData() then
      exit;

    if ImgData.HasEXIF and ImgData.ExifObj.msAvailable then
      DumpMSpecific;

    if ImgData.HasThumbnail then
    begin
      ImgData.ExifObj.ProcessThumbnail;
      DumpThumb;
    end
    else
      WriteToMemo('No Thumbnail');

    if ImgData.commentSegment <> nil then
    begin
      WriteToMemo(' ');
      WriteToMemo('--- Comment Segment Available ---');
      WriteToMemo(ImgData.GetCommentStr());
    end;

    if ImgData.IPTCSegment <> nil then
    begin
      ts := ImgData.IptcObj.ParseIPTCStrings(ImgData.IPTCSegment^.Data);
      if ts.Count > 0 then
      begin
        WriteToMemo(crlf + '--- IPTC Segment Available! ---' + crlf);
        for i := 0 to ts.Count-1 do
          WriteToMemo(ts.strings[i]);
      end;
      ts.Free;
    end;

    if not ImgData.HasEXIF then
      exit;

    if ImgData.HasThumbnail then
    begin
      {$IFDEF DELPHI}
      jpegThumb := imgData.ExtractThumbnailJpeg();
      ThumbnailImage.Picture.Assign(jpegThumb);
      jpegThumb.Free;
      {$ELSE}
      stream := TMemoryStream.Create;
      if imgData.ExtractThumbnailJpeg(stream) then begin
        jpegThumb := TJpegImage.Create;
        jpegThumb.LoadfromStream(stream);
        ThumbnailImage.Picture.Assign(jpegThumb);
        jpegThumb.Free;
      end;
      stream.Free;
      NoThumbInfo.Hide;
      {$ENDIF}
    end else
      NoThumbInfo.Show;

    try
    // ProcessHWSpecific(ImageInfo.MakerNote,Nikon1Table,8,MakerOffset);
      WriteToMemo(' ');
      WriteToMemo('--- EXIF Summary (short) --- ');
      WriteToMemo(ImgData.ExifObj.toShortString());
      WriteToMemo(' ');
      WriteToMemo('--- EXIF Summary (long) --- ');
      WriteToMemo(ImgData.ExifObj.toLongString());
    // only allow image to be written if no errors
      if ImgData.ErrStr = '<none>' then begin
        btnWriteSmall.Enabled := true;
        btnWriteChDate.Enabled := true;
      end;
      if ImgData.ExifObj.CommentPosn > 0 then
        btnComment.Enabled := true;
      WriteToMemo('');
    // An example of pulling some specific tags out of
    // the found items list.  I'll change the names
    // around a little just because...
      tmp := ImgData.ExifObj.LookupTagVal('MaxApertureValue');
      if tmp <> '' then
        WriteToMemo(' ** Widest Aperture is ' + tmp);
      tmp := ImgData.ExifObj.LookupTagVal('ShutterSpeedValue');
      if tmp <> '' then
        WriteToMemo(' ** Response Time is ' + tmp);
      tmp := ImgData.ExifObj.LookupTagVal('MeteringMode');
      if tmp <> '' then
        WriteToMemo(' ** Light Meter mode is ' + tmp);
    finally
      {$IFDEF DELPHI}
      if cbClearOnLoad.Checked then
        Memo.Perform(EM_LINESCROLL,0,-Memo.Lines.Count);
      {$ENDIF}
    end;
  end;
end;

procedure TForm1.btnTreeClick(Sender: TObject);
var
  etime: TDateTime;
begin
  NoThumbInfo.Hide;
  btnWriteSmall.Enabled := false;
  btnWriteChDate.Enabled := false;
  if cbClearOnLoad.Checked then
    Memo.Clear;
  Flist.Clear;
  CleanupPreview;
  JpgCnt := 0;
  PBar.Position := 0;

  {$IFDEF DELPHI}
  Lastdir := BrowseForDir(Handle,lastDir);
  {$ELSE}
  if not SelectDirectory('Directory', LastDir, LastDir) then
    exit;
  {$ENDIF}

  cursor := crHourglass;
  StatusBar.SimpleText := 'Scanning Directory Structure';
  PBar.BorderSpacing.Left := StatusBar.Canvas.TextWidth(StatusBar.SimpleText) + 8;
  StatusBar.Refresh;

  ReadExifDir(lastDir, true);        //  run through it just to count jpegs

  PBar.Show;
  etime := Clock();
  ReadExifDir(lastDir, false);
  etime := Clock() - etime;
  PBar.Hide;
  StatusBar.SimpleText := Format('Elapsed time (%d jpegs): %s sec', [
    JpgCnt, FormatDateTime('s.zz', etime)
  ]);
  Memo.Lines.AddStrings(flist);
  cursor := crDefault;
  PBar.Hide;
end;

procedure TForm1.btnWriteSmallClick(Sender: TObject);
var
  Orig,Smaller: TJpegImage;
  buffer: TBitmap;
  smallFname: string;
  jpegStream: TMemoryStream;
begin
  smallFName := ChangeFileExt(ImgData.FileName, '') + '_smaller.jpg';
  JpegOut.FileName := smallFName;
  if not JpegOut.Execute then
    exit;

  SmallFName := JPegOut.FileName;
  Buffer := TBitmap.Create;
  Orig := TJpegImage.Create;
  Smaller := TJpegImage.create;
  try
    Orig.LoadFromFile(ImgData.Filename);
   {$IFDEF DELPHI}
    Orig.DIBNeeded;
   {$ENDIF}
    Buffer.PixelFormat := pf24bit;
    Buffer.Width := orig.Width div 2;
    Buffer.Height := orig.Height div 2;
    // Simple resize
    Buffer.Canvas.StretchDraw(rect(0,0,Buffer.width,buffer.height),Orig);
    Smaller.Assign(Buffer);
    //Smaller.CompressionQuality := 75;
   {$IFDEF DELPHI}
    Smaller.Compress;
   {$ENDIF}
    //  the following line removes the embedded thumbnail
    //  but breaks with some cameras (e.g. Nikon)
    //  ImgData.ExifObj.removeThumbnail;
    //
    //  Use the following to remove all metadata from an image
    // ImgData.ClearSections;  // <--- wp: no - we want to transfer exit data to the new image
    //
    //  The following allows a program to apply a correction
    //  to the DateTime fields in the EXIF.  This can compensate,
    //  for example, for an inaccurate clock in a camera.
    //  ImgData.ExifObj.AdjDateTime(-1,1,10,10);
    //
    // If dEXIF is built into a control then
    //   Smaller.SaveToFile(SmallFName);
    // Since it's not we use:
    {$IFDEF DELPHI}
    ImgData.WriteEXIFjpeg(Smaller,SmallFName);
    {$ELSE}
    // Lazarus cannot access the jpeg image directly because dEXIF is ported
    // without LCL. --> Use an intermediate stream
    jpegStream := TMemoryStream.Create;
    try
      Smaller.SaveToStream(jpegStream);
      ImgData.WriteEXIFJpeg(jpegStream, SmallFName);
    finally
      jpegStream.Free;
    end;
    {$ENDIF}

  finally // Cleanup
    Buffer.free;
    Orig.Free;
    SMaller.Free;
  end;
end;

procedure TForm1.btnWriteChDateClick(Sender: TObject);
var
  newFName: string;
  jpegStream: TMemoryStream;
begin
  newFName := ChangeFileExt(ImgData.FileName, '') + '_today.jpg';
  JpegOut.FileName := newFName;
  if not JpegOut.Execute then
    exit;

  ImgData.ExifObj.OverwritedateTime(now);

  jpegStream := TMemoryStream.Create;
  try
    jpegStream.LoadFromFile(ImgData.Filename);
    ImgData.WriteEXIFJpeg(jpegStream, newFName);
  finally
    jpegStream.Free;
  end;
end;

procedure TForm1.CleanupPreview;
begin
  if ThumbnailImage.Picture.Bitmap <> nil then
  begin
    ThumbnailImage.Picture.Bitmap.FreeImage;
    ThumbnailImage.Picture.Bitmap := nil;
  end;
end;

procedure TForm1.cbDecodeClick(Sender: TObject);
begin
  // This variable will determine if the tags are decoded into human-based terms
  DexifDecode := cbDecode.Checked;
end;

procedure TForm1.cbVerboseClick(Sender: TObject);
begin
  Verbose := cbVerbose.Checked;
end;

procedure TForm1.DumpEXIF;
var
  item:TTagEntry;
begin
  if Memo.Lines.Count > 0 then
    WriteToMemo(' ');
  WriteToMemo('-- EXIF-Data -------------- ');
  WriteToMemo('ErrStr = '+ImgData.ErrStr);

  if not ImgData.HasEXIF() then
    exit;

  if ImgData.MotorolaOrder
    then WriteToMemo('Motorola Byte Order')
    else WriteToMemo('Intel Byte Order');

  // verbose data is only available in the trace strings
  if cbVerbose.Checked then
    Memo.Lines.Add(ImgData.ExifObj.TraceStr)
  else
  begin
    ImgData.ExifObj.ResetIterator;
    while ImgData.ExifObj.IterateFoundTags(GenericEXIF ,item) do
      WriteToMemo(item.Desc+DexifDelim+item.Data);
  end;
end;

procedure TForm1.DumpMSpecific;
var
  item:TTagEntry;
begin
  if Memo.Lines.Count > 0 then
    WriteToMemo(' ');
  WriteToMemo(' -- Maker Specific Data ---- ');
  // verbose data is only available in the trace strings
  if cbVerbose.Checked then
    Memo.Lines.Add(ImgData.ExifObj.msTraceStr)
  else
  begin
    ImgData.ExifObj.ResetIterator;
    while ImgData.ExifObj.IterateFoundTags(CustomEXIF,item) do
      WriteToMemo(item.Desc+DexifDelim+item.Data);
  end;
end;

procedure TForm1.DumpSections;
var
  i: integer;
  sh: string;
begin
  WriteToMemo(' --------------------------- ');
  WriteToMemo('File = '+ImgData.Filename);
  WriteToMemo('Section count = '+inttostr(ImgData.SectionCnt));
  for i := 1 to ImgData.SectionCnt do
  begin
    sh := '    Section['+inttostr(i)+']';
    WriteToMemo(
      sh + '.type = $' + IntToHex(ImgData.Sections[i].dtype,2) +
      ' - ' + LookupType(ImgData.Sections[i].dtype) + ' (' +
      IntToStr(ImgData.Sections[i].Size) + ')'
    );
//    WriteToMemo(' Printable -> '+MakePrintable(copy(ImgData.Sections[i].data,1,100)));
  end;
end;

procedure TForm1.DumpThumb;
var
  item: TTagEntry;
begin
  if Memo.Lines.Count > 0 then
    WriteToMemo(' ');
  WriteToMemo(' -- Thumbnail Data ---- ');
  WriteToMemo('Thumbnail Start = ' +inttostr(ImgData.ExifObj.ThumbStart));
  WriteToMemo('Thumbnail Length = '+inttostr(ImgData.ExifObj.ThumbLength));
  // verbose data is only available in the trace strings
  if cbVerbose.Checked then
    Memo.Lines.Add(ImgData.ExifObj.ThumbTrace)
  else
  begin
    ImgData.ExifObj.ResetThumbIterator;
    while ImgData.ExifObj.IterateFoundThumbTags(GenericEXIF,item) do
      WriteToMemo(item.Desc+DexifDelim+item.Data);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ImgData := TImgData.Create;
  Verbose := false;
  constraints.MinHeight := height;
  constraints.MinWidth := width;
  fList := tStringList.Create;
  lastDir := GetCurrentDir;
  DoubleBuffered := true;
  Memo.DoubleBuffered := true;
  PBar.Parent := StatusBar;
  PBar.Align := alClient;
  {$IFDEF MSWINDOWS}
  Memo.Font.Name := 'Courier New';
  Memo.Font.Size := 9;
  {$ENDIF}
  {$IFDEF DELPHI}
  Caption := 'Delphi EXIF jpeg viewer';
  {$ELSE}
  Caption := 'Lazarus EXIF jpeg viewer';
  {$ENDIF}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ImgData.Free;
  fList.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if not Memo.DoubleBuffered then
    Memo.DoubleBuffered := true;
end;

procedure TForm1.ReadExifDir(start:string;justcnt:boolean);
var
  SR: TSearchRec;
  status: Word;
  finfo: string;
  ext: String;
begin
  refresh;                   // repaint the window now
  if start = '' then exit;   // in case user pressed <cancel>
  Memo.Lines.BeginUpdate;   // reduce repainting overhead
  status := FindFirst(start+'\*.*', faAnyFile, SR);
  // ImgData.BuildList := GenNone;  // remove overhead but loose size
  while status = 0 do
  begin
    if not ((SR.Name = '.') or (SR.name = '..')) then
    begin
      if (SR.Attr and faDirectory) <> 0 then
        ReadExifDir(start+ '\' + SR.Name, JustCnt)  // recurse into subdirs
      else
      begin
        ext := uppercase(ExtractFileExt(SR.Name));
        if (ext = '.JPG') or (ext = '.JPEG') or (ext = '.NEF') or (ext = '.TIF')
        then
        begin
          if justCnt then
            inc(JpgCnt)
          else
          if ImgData.ProcessFile(start + '\' + SR.Name) then
          begin
            if ImgData.HasMetaData then
            begin
              if  ImgData.HasEXIF then
                finfo := ImgData.ExifObj.ToShortString()    // Just so you know:
              else
                finfo := SR.Name;
              if ImgData.IPTCSegment <> nil then
                finfo := finfo + ' + IPTC';
            end
            else
              finfo := SR.Name + ' - No metadata';
            Memo.lines.Add(finfo);            //   this will blow up if there
            PBar.StepIt;
            StatusBar.SimpleText :=
              Format('%0.1f%% of %d files.',[Pbar.Position/JpgCnt*100, JpgCnt]);
            if pbar.Position mod 100 = 0 then   // too many refreshes will show
              Application.ProcessMessages       // down the whole process
          end;
        end;
      end;
    end;
    status := FindNext(SR);
  end;
  FindClose(SR);
  Memo.Lines.EndUpdate;
end;

procedure TForm1.WriteToMemo(s:string);
begin
  Memo.Lines.Add(s);
end;


end.
