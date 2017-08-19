object Form1: TForm1
  Left = 135
  Top = 179
  Caption = 'Delphi EXIF Jpeg Viewer'
  ClientHeight = 278
  ClientWidth = 576
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    576
    278)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 393
    Top = 16
    Width = 169
    Height = 121
    Anchors = [akTop, akRight]
  end
  object btnLoad: TButton
    Left = 24
    Top = 8
    Width = 75
    Height = 25
    Caption = 'One File'
    TabOrder = 0
    OnClick = btnLoadClick
  end
  object Memo1: TMemo
    Left = 120
    Top = 8
    Width = 247
    Height = 249
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 259
    Width = 576
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object cbClearOnLoad: TCheckBox
    Left = 16
    Top = 72
    Width = 97
    Height = 17
    Caption = 'Clear on Load'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object btnAbout: TButton
    Left = 24
    Top = 230
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'About...'
    TabOrder = 4
    OnClick = btnAboutClick
  end
  object btnTree: TButton
    Left = 24
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Directory...'
    TabOrder = 5
    OnClick = btnTreeClick
  end
  object PBar: TProgressBar
    Left = 120
    Top = 236
    Width = 450
    Height = 16
    Anchors = [akLeft, akRight, akBottom]
    Step = 1
    TabOrder = 6
  end
  object cbVerbose: TCheckBox
    Left = 16
    Top = 96
    Width = 97
    Height = 17
    Caption = 'Verbose Trace'
    TabOrder = 7
    OnClick = cbVerboseClick
  end
  object btnWrite: TButton
    Left = 24
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Write Small'
    Enabled = False
    TabOrder = 8
    OnClick = btnWriteClick
  end
  object cbDecode: TCheckBox
    Left = 16
    Top = 120
    Width = 89
    Height = 17
    Caption = 'Decode'
    Checked = True
    State = cbChecked
    TabOrder = 9
    OnClick = cbDecodeClick
  end
  object btnCmt: TButton
    Left = 24
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Comment...'
    Enabled = False
    TabOrder = 10
    OnClick = btnCmtClick
  end
  object pdlg: TOpenPictureDialog
    Filter = 
      'All (*.gif;*.ani;*.pcx;*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf;*.ti' +
      'f;*.nef)|*.gif;*.ani;*.pcx;*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf;' +
      '*.tif;*.nef|CompuServe GIF Image (*.gif)|*.gif|ANI Image (*.ani)' +
      '|*.ani|PCX Image (*.pcx)|*.pcx|JPEG Image File (*.jpg)|*.jpg|JPE' +
      'G Image File (*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)' +
      '|*.ico|Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf|' +
      'TIFF Image File (*.tif)|*.tif|Nikon Image File (*.nef)|*.nef'
    InitialDir = '.'
    Left = 144
    Top = 16
  end
  object JpegOut: TSavePictureDialog
    Left = 144
    Top = 48
  end
end
