object frmEditText: TfrmEditText
  Left = 0
  Top = 0
  Caption = 'Edit Text Slide'
  ClientHeight = 497
  ClientWidth = 691
  Color = clBtnFace
  Constraints.MinHeight = 442
  Constraints.MinWidth = 338
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    691
    497)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 14
    Width = 79
    Height = 13
    Caption = 'Overview name:'
  end
  object Label3: TLabel
    Left = 8
    Top = 85
    Width = 27
    Height = 13
    Caption = 'Picto:'
  end
  object Shape1: TShape
    Left = 126
    Top = 83
    Width = 96
    Height = 96
  end
  object edtOverviewName: TEdit
    Left = 128
    Top = 11
    Width = 185
    Height = 21
    TabOrder = 0
  end
  object ImgViewPicto: TImgView32
    Left = 128
    Top = 85
    Width = 92
    Height = 92
    Cursor = crHandPoint
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smOptimalScaled
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 17
    ScrollBars.Visibility = svAuto
    OverSize = 0
    TabOrder = 1
    OnClick = ImgViewPictoClick
  end
  object btnOK: TButton
    Left = 527
    Top = 465
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object btnCancel: TButton
    Left = 608
    Top = 465
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object mmoText: TMemo
    Left = 8
    Top = 208
    Width = 674
    Height = 251
    Anchors = [akLeft, akTop, akRight, akBottom]
    PopupMenu = ppmText
    ScrollBars = ssVertical
    TabOrder = 3
    WantTabs = True
    OnKeyPress = mmoTextKeyPress
  end
  object btnInsertSlide: TButton
    Left = 8
    Top = 465
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '<--->'
    TabOrder = 4
    OnClick = btnInsertSlideClick
  end
  object btnPaste: TButton
    Left = 89
    Top = 465
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Paste'
    TabOrder = 5
    OnClick = btnPasteClick
  end
  object cbxAutomaticSmallNumbers: TCheckBox
    Left = 127
    Top = 185
    Width = 185
    Height = 17
    Caption = 'Automatic small numbers'
    TabOrder = 2
  end
  object btnInternet: TButton
    Left = 608
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Internet'
    TabOrder = 8
    OnClick = btnInternetClick
  end
  object btnFill: TButton
    Left = 335
    Top = 177
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Fill'
    TabOrder = 9
    OnClick = btnFillClick
  end
  object cbxPartOfForm: TCheckBox
    Left = 128
    Top = 60
    Width = 185
    Height = 17
    Caption = 'Part Of Form'
    TabOrder = 10
  end
  object cbxShowInOverview: TCheckBox
    Left = 128
    Top = 38
    Width = 184
    Height = 17
    Caption = 'Show In Overview'
    TabOrder = 11
  end
  object btnSelectPictoNone: TButton
    Left = 228
    Top = 83
    Width = 75
    Height = 25
    Caption = 'None'
    TabOrder = 12
    OnClick = btnSelectPictoNoneClick
  end
  object btnConvertToAGB: TButton
    Left = 527
    Top = 70
    Width = 156
    Height = 25
    Caption = 'Apostolische Geloofsbelijdenis'
    TabOrder = 13
    OnClick = btnConvertToAGBClick
  end
  object btnConvertToDL: TButton
    Left = 527
    Top = 101
    Width = 156
    Height = 25
    Caption = 'Dortse Leerregels'
    TabOrder = 14
    OnClick = btnConvertToDLClick
  end
  object btnConvertToGBA: TButton
    Left = 527
    Top = 132
    Width = 156
    Height = 25
    Caption = 'Geloofsbel. van Athanasius'
    TabOrder = 15
    OnClick = btnConvertToGBAClick
  end
  object btnConvertToHC: TButton
    Left = 527
    Top = 163
    Width = 156
    Height = 25
    Caption = 'Heidelbergse Catechismus'
    TabOrder = 16
    OnClick = btnConvertToHCClick
  end
  object btnBibleNBV: TButton
    Left = 608
    Top = 39
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'NBV'
    TabOrder = 17
    OnClick = btnBibleNBVClick
  end
  object ppmText: TPopupMenu
    OnPopup = ppmTextPopup
    Left = 304
    Top = 336
    object mniPaste: TMenuItem
      Caption = 'Paste'
      ShortCut = 16470
      OnClick = mniPasteClick
    end
  end
end
