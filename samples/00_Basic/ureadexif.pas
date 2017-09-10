unit ureadexif;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ExtCtrls, ComCtrls, ValEdit, StdActns;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionList: TActionList;
    FileExit: TFileExit;
    FileOpen: TFileOpen;
    Image1: TImage;
    ImageList: TImageList;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ValueListEditor1: TValueListEditor;
    procedure FileOpenAccept(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses
  dGlobal, dMetadata;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FileOpenAccept(Sender: TObject);
var
  MyFileName : String;
  ImgData: TImgData;
begin
  MyFileName:= FileOpen.Dialog.FileName;
  if FileExists(MyFileName) then begin
    ValueListEditor1.Clear;
    Image1.Proportional:= True;
    Image1.Stretch:= True;
    Image1.Picture.LoadFromFile(MyFileName);
    ImgData:= TImgData.Create();
    try
      if ImgData.ProcessFile(MyFileName) then begin
        if ImgData.HasEXIF then begin
          ValueListEditor1.InsertRow('Camera Make',
            ImgData.ExifObj.CameraMake,True);
          ValueListEditor1.InsertRow('Camera Modell',
            ImgData.ExifObj.CameraModel,True);
          ValueListEditor1.InsertRow('Picture DateTime',
            FormatDateTime(ISO_DATETIME_FORMAT, ImgData.ExifObj.GetImgDateTime),True);
        end
        else
          ValueListEditor1.InsertRow('No EXIF','No Data',True);
      end
      else
        ValueListEditor1.InsertRow('No EXIF','Processdata',True);
    finally
      ImgData.Free;
    end;
  end;
end;

end.

