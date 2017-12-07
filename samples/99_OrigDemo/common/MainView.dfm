object Form1: TForm1
  Left = 287
  Top = 191
  Width = 712
  Height = 393
  Caption = 'Delphi EXIF Jpeg Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    696
    354)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 531
    Top = 8
    Width = 151
    Height = 121
    Anchors = [akTop, akRight]
  end
  object btnLoad: TButton
    Left = 16
    Top = 8
    Width = 96
    Height = 25
    Caption = 'Open a file'
    TabOrder = 0
    OnClick = btnLoadClick
  end
  object Memo1: TMemo
    Left = 120
    Top = 8
    Width = 403
    Height = 315
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 331
    Width = 696
    Height = 23
    Panels = <>
  end
  object cbClearOnLoad: TCheckBox
    Left = 16
    Top = 72
    Width = 93
    Height = 19
    Caption = 'Clear on Load'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object btnAbout: TButton
    Left = 16
    Top = 296
    Width = 94
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'About...'
    TabOrder = 4
    OnClick = btnAboutClick
  end
  object btnTree: TButton
    Left = 16
    Top = 40
    Width = 96
    Height = 25
    Caption = 'Directory...'
    TabOrder = 5
    OnClick = btnTreeClick
  end
  object PBar: TProgressBar
    Left = 128
    Top = 307
    Width = 554
    Height = 16
    Anchors = [akLeft, akRight, akBottom]
    Step = 1
    TabOrder = 6
  end
  object cbVerbose: TCheckBox
    Left = 16
    Top = 96
    Width = 94
    Height = 19
    Caption = 'Verbose Trace'
    TabOrder = 7
    OnClick = cbVerboseClick
  end
  object btnWriteSmall: TButton
    Left = 16
    Top = 208
    Width = 96
    Height = 25
    Caption = 'Write Small'
    Enabled = False
    TabOrder = 8
    OnClick = btnWriteSmallClick
  end
  object cbDecode: TCheckBox
    Left = 16
    Top = 120
    Width = 60
    Height = 19
    Caption = 'Decode'
    Checked = True
    State = cbChecked
    TabOrder = 9
    OnClick = cbDecodeClick
  end
  object btnExifComment: TButton
    Left = 16
    Top = 144
    Width = 96
    Height = 25
    Caption = 'Exif Comment...'
    Enabled = False
    TabOrder = 10
    OnClick = btnExifCommentClick
  end
  object btnSaveThumb: TButton
    Left = 547
    Top = 144
    Width = 124
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save thumbnail'
    TabOrder = 11
    OnClick = btnSaveThumbClick
  end
  object btnLoadThumb: TButton
    Left = 547
    Top = 176
    Width = 124
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Replace thumbnail'
    TabOrder = 12
    OnClick = btnLoadThumbClick
  end
  object btnRemoveThumb: TButton
    Left = 547
    Top = 208
    Width = 124
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Remove thumbnail'
    TabOrder = 13
    OnClick = btnRemoveThumbClick
  end
  object btnCreateThumb: TButton
    Left = 547
    Top = 240
    Width = 124
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Create thumbnail'
    TabOrder = 14
    OnClick = btnCreateThumbClick
  end
  object btnWriteSame: TButton
    Left = 16
    Top = 239
    Width = 96
    Height = 25
    Caption = 'Write same'
    Enabled = False
    TabOrder = 15
    OnClick = btnWriteSameClick
  end
  object btnComment: TButton
    Left = 16
    Top = 176
    Width = 96
    Height = 25
    Caption = 'Std Comment...'
    Enabled = False
    TabOrder = 16
    OnClick = btnCommentClick
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
    Top = 88
  end
end
