object IPTCform: TIPTCform
  Left = 121
  Top = 191
  Width = 668
  Height = 503
  Caption = 'IPTC Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 135
    Width = 652
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 138
    Width = 652
    Height = 307
    VertScrollBar.Style = ssHotTrack
    VertScrollBar.Tracking = True
    Align = alClient
    TabOrder = 2
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 445
    Width = 652
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 652
    Height = 135
    Align = alTop
    TabOrder = 0
    DesignSize = (
      652
      135)
    object lblDateTime: TLabel
      Left = 112
      Top = 12
      Width = 10
      Height = 16
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsItalic]
      ParentFont = False
    end
    object Memo1: TMemo
      Left = 112
      Top = 40
      Width = 532
      Height = 87
      Anchors = [akLeft, akTop, akRight, akBottom]
      Color = clBtnFace
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      WantReturns = False
    end
    object btnAbout: TButton
      Left = 58
      Top = 99
      Width = 41
      Height = 25
      Caption = 'About'
      TabOrder = 1
      OnClick = btnAboutClick
    end
    object btnLoad: TButton
      Left = 10
      Top = 6
      Width = 89
      Height = 25
      Caption = 'Read IPTC File'
      Default = True
      TabOrder = 2
      OnClick = btnLoadClick
    end
    object btnClose: TButton
      Left = 10
      Top = 99
      Width = 41
      Height = 25
      Cancel = True
      Caption = 'Close'
      ModalResult = 2
      TabOrder = 3
      OnClick = btnCloseClick
    end
    object btnTags: TButton
      Left = 10
      Top = 67
      Width = 41
      Height = 25
      Caption = 'Tags'
      TabOrder = 4
      OnClick = btnTagsClick
    end
    object btnWrite: TButton
      Left = 10
      Top = 36
      Width = 89
      Height = 25
      Caption = 'Write...'
      Enabled = False
      TabOrder = 5
      OnClick = btnWriteClick
    end
    object btnXML: TButton
      Left = 58
      Top = 67
      Width = 41
      Height = 25
      Caption = 'XML'
      TabOrder = 6
      OnClick = btnXMLClick
    end
    object btnSetDT: TButton
      Left = 568
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Release'
      TabOrder = 7
      OnClick = btnSetDTClick
    end
  end
  object pdlg: TOpenPictureDialog
    InitialDir = '.'
    Left = 200
    Top = 8
  end
  object WriteDlg: TSavePictureDialog
    Left = 256
    Top = 8
  end
end
