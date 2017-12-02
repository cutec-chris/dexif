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
 {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFNDEF FPC}
   Windows, Messages, Jpeg, SHLObj, FileCtrl, mmsystem,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtDlgs, StdCtrls, ComCtrls, ExtCtrls,
  dGlobal, dMetadata, dExif, dIPTC, msData;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnComment: TButton;
    pdlg: TOpenPictureDialog;
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    cbClearOnLoad: TCheckBox;
    btnAbout: TButton;
    btnTree: TButton;
    PBar: TProgressBar;
    cbVerbose: TCheckBox;
    JpegOut: TSavePictureDialog;
    cbDecode: TCheckBox;
    Image1: TImage;
    btnLoad: TButton;
    btnWriteSame: TButton;
    btnWriteSmall: TButton;
    btnExifComment: TButton;
    btnSaveThumb: TButton;
    btnLoadThumb: TButton;
    btnRemoveThumb: TButton;
    btnCreateThumb: TButton;
    procedure btnAboutClick(Sender: TObject);
    procedure btnCommentClick(Sender: TObject);
    procedure btnCreateThumbClick(Sender: TObject);
    procedure btnExifCommentClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnLoadThumbClick(Sender: TObject);
    procedure btnRemoveThumbClick(Sender: TObject);
    procedure btnSaveThumbClick(Sender: TObject);
    procedure btnTreeClick(Sender: TObject);
    procedure btnWriteSameClick(Sender: TObject);
    procedure btnWriteSmallClick(Sender: TObject);
    procedure cbDecodeClick(Sender: TObject);
    procedure cbVerboseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure CleanupPreview;
    procedure dumpSections;
    procedure dumpEXIF;
    procedure dumpMSpecific;
    procedure dumpThumb;
    procedure Memo(s: string);
    procedure ReadExifDir(start:string; justcnt:boolean);
  public
    lastDir: string;
    jpgcnt: integer;
    scrar:real;
    etime:longint;
    Verbose:boolean;
  end;

var
  Form1: TForm1;
  ImgData: TImgData;

implementation

{$IFDEF LCL}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
  {$IFNDEF FPC}
  ShellApi,
  {$ENDIF}
  About;

const
  ProgName = 'DExView';

{$IFDEF FPC}
  EM_LINESCROLL = $00B6;
{$ENDIF}

{$IFNDEF FPC}
var
  DirBrowseName : string;

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
    ItemIDList := SHBrowseForFolder(BrowseInfo);
    trY // Get directory from the ItemIDList
      if Assigned(ItemIDList) then
        if SHGetPathFromIDList(ItemIDList, DisplayName) then
          result := DisplayName;
    finally
//      FreePIDL;  //  Causes crash if left in
    end;
  end;
{$ENDIF}

function clock:longint;
begin
  {$IFDEF FPC}
  Result := GetTickCount64;
  {$ELSE}
  Clock := TimeGetTime;
  {$ENDIF}
end;


{ TForm1 }

procedure TForm1.btnAboutClick(Sender: TObject);
begin
  AboutBox.FormSetup(ProgName,dEXIFVersion);
  AboutBox.ShowModal;
end;

procedure TForm1.btnCommentClick(Sender: TObject);
var
  cmt:string;
  fn: String;
  jpeg: TJpegImage;
  ms: TMemoryStream;
begin
  if ImgData.Comment = '' then
    ShowMessage('No comment segment detected')
  else
  begin
    // Input (new) comment
    cmt := InputBox('Enter comment:', 'Enter a new comment', ImgData.Comment);
    if ImgData.Comment <> cmt then
    begin
      // Change comment in EXIF
      ImgData.Comment := cmt;
      Memo1.Lines.Add('');
      Memo1.Lines.Add('Comment set to: '+cmt);

      // Save to file ("_comment" appended to filename)
      fn := ChangeFileExt(ImgData.FileName, '') + '_comment.jpg';
      ImgData.WriteEXIFJpegTo(fn);
      Memo1.Lines.Add('');
      Memo1.Lines.Add('Image saved in: ' + fn );
    end;
  end;
end;

procedure TForm1.btnCreateThumbClick(Sender: TObject);
var
  ms: TMemoryStream;
  fn: String;
  jpeg: TJpegImage;
begin
  if not ImgData.HasExif then
    exit;

  ImgData.ExifObj.CreateThumbnail;

  fn := ChangeFileExt(ImgData.FileName, '') + '_new_thumbnail.jpg';
  ms := TMemoryStream.Create;
  try
    // Load current image data
    ms.LoadfromFile(ImgData.FileName);
    // and merge with current exif data (i.e. new thumbnail image)
    ImgData.WriteEXIFJpeg(ms, fn);
  finally
    ms.Free;
  end;

  ms := TMemoryStream.Create;
  try
    ImgData.ExifObj.SaveThumbnailToStream(ms);
    ms.Position := 0;
    jpeg := TJPegImage.Create;
    try
      jpeg.LoadFromStream(ms);
      Image1.Picture.Bitmap.Assign(jpeg);
    finally
      jpeg.Free;
    end;
//    Image1.Picture.LoadFromStream(ms);
  finally
    ms.Free;
  end;

  btnRemoveThumb.Enabled := true;
  btnSaveThumb.Enabled := true;
  btnCreateThumb.Enabled := true;

  Memo('Thumbnail image created. Image saved as "' + fn + '"');
end;

procedure TForm1.btnExifCommentClick(Sender: TObject);
var
  cmt:string;
  fn: String;
  jpeg: TJpegImage;
  ms: TMemoryStream;
begin
  if ImgData.ExifObj.ExifComment = '' then
    ShowMessage('No EXIF comment field detected')
  else
  begin
    // Input (new) comment
    cmt := InputBox('Enter EXIF comment:', 'Enter a new comment',
      ImgData.ExifObj.ExifComment);
    if ImgData.ExifObj.ExifComment <> cmt then
    begin
      // Change comment in EXIF
      ImgData.ExifObj.ExifComment := cmt;
      Memo1.Lines.Add('');
      Memo1.Lines.Add('Exif comment set to: '+cmt);

      // Save to file ("_exifcomment" appended to filename)
      fn := ChangeFileExt(ImgData.FileName, '') + '_exifcomment.jpg';
      ImgData.WriteEXIFJpegTo(fn);
      Memo1.Lines.Add('');
      Memo1.Lines.Add('Image saved in: ' + fn );
    end;
  end;
end;

procedure TForm1.btnLoadClick(Sender: TObject);
var
  i: integer;
  ts: tstringlist;
  tmp:string;
  {$IFDEF FPC}
  jpegThumb: TMemoryStream;
  {$ELSE}
  jpegThumb:tjpegimage;
  {$ENDIF}
begin
  btnWriteSame.Enabled := false;
  btnWriteSmall.Enabled := false;
  btnExifComment.Enabled := false;
  btnComment.Enabled := false;
  if pdlg.Execute then
  begin
    CleanupPreview;
    StatusBar1.SimpleText  := 'Info for '+pdlg.FileName;
    if verbose
      then ExifTrace := 1
      else ExifTrace := 0;
    if cbClearOnLoad.Checked then
      memo1.Clear;

    ImgData.BuildList := GenAll;  // on by default anyway

    ImgData.ProcessFile(pdlg.FileName);

    if Verbose then
      dumpSections;

    dumpExif;

    if not ImgData.HasMetaData() then
      exit;

    if ImgData.HasEXIF and ImgData.ExifObj.msAvailable then
      dumpMSpecific;

    if ImgData.HasThumbnail then
    begin
//      ImgData.ExifObj.ProcessThumbnail;
      dumpThumb;

      {$IFDEF FPC}
      jpegThumb := TMemoryStream.Create;
      try
        imgData.ExtractThumbnailJpeg(jpegthumb);
        if jpegthumb.Size > 0 then begin
          jpegThumb.Position := 0;
          Image1.Picture.LoadFromStream(jpegThumb);
        end else
        begin
          Memo(' ');
          Memo('Thumbnail expected, but not found');
        end;
      finally
        jpegThumb.Free;
      end;
      {$ELSE}
      jpegThumb := imgData.ExtractThumbnailJpeg();
      try
        if jpegThumb <> nil then
          Image1.Picture.Assign(jpegThumb)
        else begin
          Memo(' ');
          Memo('Thumbnail expected, but not found');
        end;
      finally
        jpegThumb.Free;
      end;
      {$ENDIF}
    end
    else
      Memo('No Thumbnail');


//    Image1.Visible := ImgData.HasThumbnail;
    btnSaveThumb.Enabled := ImgData.HasThumbnail;
    btnRemoveThumb.Enabled := ImgData.HasThumbnail;
    btnLoadThumb.Enabled := ImgData.HasExif;
    btnCreateThumb.Enabled := ImgData.HasExif;
    btnExifComment.Enabled := ImgData.HasExif;
    btnComment.Enabled := true;

    if ImgData.HasComment then
    begin
      Memo(' ');
      Memo('Comment segment available');
      Memo(ImgData.Comment);
    end;

    if ImgData.HasIPTC then
    begin
      ts := TStringList.Create;
      try
        ImgData.IptcObj.IPTCArrayToList(ts);
        if ts.Count > 0 then
        begin
          Memo(crlf + 'IPTC segment available' + crlf);
          for i := 0 to ts.Count-1 do
          begin
            Memo(ts.strings[i]);
          end;
        end;
      finally
        ts.Free;
      end;
    end;

    if not ImgData.HasEXIF then
      exit;

    try
      // ProcessHWSpecific(ImageInfo.MakerNote,Nikon1Table,8,MakerOffset);
      Memo(' ');
      Memo('-- EXIF Summary (short) -----');
      Memo(ImgData.ExifObj.toShortString());
      Memo(' ');
      Memo('-- EXIF Summary (long) ------');
      Memo(ImgData.ExifObj.toLongString());
      // only allow image to be written if no errors
      if ImgData.ErrStr = '<none>' then begin
        btnWriteSmall.Enabled := true;
        btnWriteSame.Enabled := true;
      end;

      Memo('');

      // An example of pulling some specific tags out of
      // the found items list.  I'll change the names
      // around a little just because...
      tmp := ImgData.ExifObj.TagValueAsString['MaxApertureValue'];
      if tmp <> '' then
        Memo(' ** Widest Aperture is '+tmp);
      tmp := ImgData.ExifObj.TagValueAsString['ShutterSpeedValue'];
      if tmp <> '' then
        Memo(' ** Response Time is '+tmp);
      tmp := ImgData.ExifObj.TagValueAsString['MeteringMode'];
      if tmp <> '' then
        Memo(' ** Light Meter mode is '+tmp);
    finally
      if cbClearOnLoad.Checked then
        Memo1.Perform(EM_LINESCROLL,0,-memo1.Lines.Count);
    end;
  end;
end;

procedure TForm1.btnLoadThumbClick(Sender: TObject);
var
  ms: TMemoryStream;
  fn: String;
  jpeg: TJpegImage;
begin
  if not ImgData.HasExif then
    exit;

  pdlg.Filename := '';
  if pdlg.Execute then begin
    ms := TMemoryStream.Create;
    try
      // load new thumbnail image from file
      ms.LoadFromFile(pdlg.Filename);
      // attach new thumbnail to ExifObj
      ms.Position := 0;
      ImgData.ExifObj.LoadThumbnailFromStream(ms);
      // Show new thumbnail
      ms.Position := 0;
      jpeg := TJpegImage.Create;
      try
        jpeg.LoadFromStream(ms);
        Image1.Picture.Assign(jpeg);
//        Image1.Show;
      finally
        jpeg.Free;
      end;
      btnRemoveThumb.Enabled := ImgData.HasThumbnail;
      btnSaveThumb.Enabled := ImgData.HasThumbnail;
    finally
      ms.Free;
    end;

    // Save ExifObj with new thumbnail to a new file
    fn := ChangeFileExt(ImgData.FileName, '') + '_changed_thumbnail.jpg';
    ms := TMemoryStream.Create;
    try
      // Load current image data
      ms.LoadfromFile(ImgData.FileName);
      // and merge with current exif data (i.e. new thumbnail image)
      ImgData.WriteEXIFJpeg(ms, fn);
    finally
      ms.Free;
    end;

    Memo('Image with replaced thumbnail saved to "' + fn + '"');
  end;

end;

procedure TForm1.btnRemoveThumbClick(Sender: TObject);
var
  ms: TMemoryStream;
  fn: String;
begin
  ImgData.ExifObj.RemoveThumbnail;

  // Save ExifObj without thumbnail to a new file
  fn := ChangeFileExt(ImgData.FileName, '') + '_no_thumbnail.jpg';
  ms := TMemoryStream.Create;
  try
    // Load current image data
    ms.LoadfromFile(ImgData.FileName);
    // and merge with current exif data (i.e. new thumbnail image)
    ImgData.WriteEXIFJpeg(ms, fn);
    CleanupPreview;
//    Image1.Hide;
    btnRemoveThumb.Enabled := false;
    btnSaveThumb.Enabled := false;
  finally
    ms.Free;
  end;
  Memo('Image with removed thumbnail saved to "' + fn + '"');
end;

procedure TForm1.btnSaveThumbClick(Sender: TObject);
var
  fn: String;
  stream: TFileStream;
begin
  if not (ImgData.HasExif and ImgData.HasThumbnail) then
    exit;

  fn := ChangeFileExt(ImgData.FileName, '') + '_thumbnail.jpg';
  stream := TFileStream.Create(fn, fmCreate);
  try
    ImgData.ExifObj.SaveThumbnailToStream(stream);
  finally
    stream.Free;
  end;
  Memo('Thumbnail image saved as "' + fn + '"');
end;

procedure TForm1.btnTreeClick(Sender: TObject);
begin
  btnWriteSame.Enabled := false;
  btnWriteSmall.Enabled := false;
  if cbClearOnLoad.Checked then
    Memo1.Clear;
  CleanupPreview;
  JpgCnt := 0;
  PBar.Position := 0;
  PBar.Parent := Statusbar1;
  PBar.Left := 0;
  {$IFDEF FPC}
  if not SelectDirectory('Directory', LastDir, LastDir) then
    exit;
  {$ELSE}
  Lastdir := BrowseForDir(Handle,lastDir);
  {$ENDIF}

  Cursor := crHourglass;
  try
    StatusBar1.SimpleText := 'Scanning directory structure';
    StatusBar1.Refresh;
    ReadExifDir(lastDir,true);        //  run through it just to count jpegs
    PBar.Max := JpgCnt;
    etime := clock();
    ReadExifDir(lastDir,false);
    StatusBar1.SimpleText := Format('Elapsed time (%d jpegs): %0.2f sec', [
      JpgCnt, (clock-etime)/1000
    ]);
  finally
    Cursor := crDefault;
  end;
end;

procedure TForm1.btnWriteSameClick(Sender: TObject);
var
  fn: String;
begin
  fn := ChangeFileExt(ImgData.Filename, '') + '_same.jpg';
  ImgData.WriteExifJpegTo(fn);
  Memo('');
  Memo('Currently loaded image written to "' + fn + '"');
end;

procedure TForm1.btnWriteSmallClick(Sender: TObject);
var
  Orig, Smaller: TJpegImage;
  buffer: tbitmap;
  smallFname:string;
  {$IFDEF FPC}
  stream: TMemoryStream;
  {$ENDIF}
begin
  smallFname := copy(ImgData.Filename,1,length(ImgData.Filename)-4)
    +'_smaller.jpg';
  JpegOut.FileName := smallFName;
  if not JpegOut.Execute then
    exit;
  SmallFName := JPegOut.FileName;
  Buffer := tbitmap.Create;
  Orig := tjpegImage.Create;
  Smaller := tjpegimage.create;
  try
    Orig.LoadFromFile(ImgData.Filename);
   {$IFNDEF FPC}
    Orig.DIBNeeded;
   {$ENDIF}
    Buffer.PixelFormat := pf24bit;
    Buffer.Width := orig.Width div 2;
    Buffer.Height := orig.Height div 2;
    // Simple resize
    Buffer.Canvas.StretchDraw(rect(0,0,Buffer.width,buffer.height),Orig);
    Smaller.Assign(Buffer);
    Smaller.CompressionQuality := 75;
   {$IFNDEF FPC}
    Smaller.Compress;
   {$ENDIF}
    //  the following line removes the embedded thumbnail
    //  but breaks with some cameras (e.g. Nikon)
    //  ImgData.ExifObj.removeThumbnail;
    //
    //  Use the following to remove all metadata from an image
    ImgData.ClearSections;
    //
    //  The following allows a program to apply a correction
    //  to the DateTime fields in the EXIF.  This can compensate,
    //  for example, for an inaccurate clock in a camera.
    //  ImgData.ExifObj.AdjDateTime(-1,1,10,10);
    //
    // If dEXIF is built into a control then
    //   Smaller.SaveToFile(SmallFName);
    // Since it's not we use:
    {$IFDEF FPC}
    stream := TMemoryStream.Create;
    try
      Smaller.SaveToStream(stream);
      ImgData.WriteEXIFJpeg(stream, SmallFName);
    finally
      stream.Free;
    end;
    {$ELSE}
    ImgData.WriteEXIFjpeg(Smaller, SmallFName);
    {$ENDIF}
  finally // Cleanup
    Buffer.free;
    Orig.Free;
    SMaller.Free;
  end;
end;

procedure TForm1.cbDecodeClick(Sender: TObject);
begin
  // This variable will determine if the
  // tags are decoded into human-based terms
  ImgData.Decode := cbDecode.Checked;
end;

procedure TForm1.cbVerboseClick(Sender: TObject);
begin
  Verbose := cbVerbose.Checked;
end;

procedure TForm1.CleanupPreview;
begin
  if image1.Picture.Bitmap <> nil then
  begin
    image1.Picture.Bitmap.FreeImage;
    image1.Picture.Bitmap := nil;
  end;
end;

procedure TForm1.DumpEXIF;
var
  item: TTagEntry;
begin
  Memo(' ');
  Memo('-- EXIF-Data --------------');
  Memo('ErrStr = '+ImgData.ErrStr);
  if not ImgData.HasEXIF() then
    exit;
  If ImgData.MotorolaOrder
    then Memo('Motorola Byte Order')
    else Memo('Intel Byte Order');
  // verbose data is only available in the trace strings
  if cbVerbose.Checked then
    Memo1.Lines.Add(ImgData.ExifObj.TraceStr)
  else
  begin
    ImgData.ExifObj.ResetIterator;
    while ImgData.ExifObj.IterateFoundTags(GenericEXIF ,item) do
      Memo(item.Desc+DexifDelim+item.Data);
  end;
end;

procedure TForm1.DumpMSpecific;
var
  item: TTagEntry;
begin
  Memo(' ');
  Memo('-- Maker Specific Data ----');
  // verbose data is only available in the trace strings
  if cbVerbose.Checked then
    Memo1.Lines.Add(ImgData.ExifObj.msTraceStr)
  else
  begin
    ImgData.ExifObj.ResetIterator;
    while ImgData.ExifObj.IterateFoundTags(CustomEXIF,item) do
      Memo(item.Desc+DexifDelim+item.Data);
  end;
end;

procedure TForm1.DumpSections;
var
  i: integer;
  sh: string;
begin
  exit;  // Sections no longer supported
  {
  Memo('---------------------------');
  Memo('File = '+ImgData.Filename);
  Memo('Section count = '+inttostr(ImgData.SectionCnt));
  for i := 1 to ImgData.SectionCnt do
  begin
    sh := '    Section['+inttostr(i)+']';
    Memo(sh+'.type = $'+IntToHex(ImgData.Sections[i].dtype,2)
           +' - '+LookupType(ImgData.Sections[i].dtype) +' ('
           +IntToStr(ImgData.Sections[i].size)+')');
//    Memo(' Printable -> '+MakePrintable(
//        copy(ImgData.Sections[i].data,1,100)));
  end;
  }
end;

procedure TForm1.DumpThumb;
var item:TTagEntry;
begin
  Memo(' ');
  Memo('-- Thumbnail Data ---');
  {
  Memo('Thumbnail Start = ' +inttostr(ImgData.ExifObj.FThumbStart));
  Memo('Thumbnail Length = '+inttostr(ImgData.ExifObj.FThumbLength));
  }
  // verbose data is only available in the trace strings
  if cbVerbose.Checked then
    Memo1.Lines.Add(ImgData.ExifObj.ThumbTrace)
  else
  begin
    ImgData.ExifObj.ResetThumbIterator;
    while ImgData.ExifObj.IterateFoundThumbTags(GenericEXIF,item) do
      Memo(item.Desc+DexifDelim+item.Data);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ImgData := TimgData.Create;
  Verbose := false;
  constraints.MinHeight := height;
  constraints.MinWidth := width;
  lastDir := GetCurrentDir;
//  DoubleBuffered := true;
//  memo1.DoubleBuffered := true;
  PBar.Hide;
  btnRemoveThumb.Enabled := false;
  btnSaveThumb.Enabled := false;
  btnLoadThumb.Enabled := false;
  btnCreateThumb.Enabled := false;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ImgData.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if not memo1.DoubleBuffered then
    memo1.DoubleBuffered := true;
end;

procedure TForm1.Memo(s:string);
begin
  Memo1.Lines.Add(s);
end;

procedure TForm1.ReadExifDir(start:string; justcnt:boolean);
var
  s: tsearchrec;
  status: word;
  finfo: string;
  ext: String;
begin
  refresh;                   // repaint the window now
  if start = '' then exit;   // in case user pressed <cancel>
  memo1.Lines.BeginUpdate;   // reduce repainting overhead
  status := FindFirst(start+'\*.*',faAnyFile,s);
  // ImgData.BuildList := GenNone;  // remove overhead but loose size
  PBar.Show;
  while status = 0 do
  begin
    if not ((s.Name = '.') or (s.name = '..')) then
    if (s.Attr and faDirectory) <> 0 then
      ReadExifDir(start+'\'+s.Name, JustCnt)  // recurse into subdirs
    else
    begin
      ext := Uppercase(ExtractFileExt(s.Name));
      if (ext = '.JPG') or (ext = '.NEF') or (ext = '.TIF') then
      begin
        if justCnt then
          inc(JpgCnt)
        else
        if ImgData.ProcessFile(start+'\'+s.name) then
        begin
          if ImgData.HasMetaData then
          begin
            if ImgData.HasEXIF then
              finfo := ImgData.ExifObj.toShortString()    // Just so you know:
            else
              finfo := s.name;
            if ImgData.HasIPTC then
              finfo := finfo + ' + IPTC';
          end
          else
              finfo := s.name+' - No metadata';
          Memo1.lines.Add(finfo);            //   this will blow up if there
          PBar.StepIt;
          StatusBar1.SimpleText :=
            Format('%0.1f%% of %d files.',[Pbar.Position/JpgCnt*100,JpgCnt]);
          if pbar.Position mod 100 = 0 then   // too many refreshes will show
            application.ProcessMessages       // down the whole process
        end;
      end;
    end;
    status := FindNext(s);
  end;
  FindClose(s);
  PBar.Hide;
  memo1.Lines.EndUpdate;
end;


end.
