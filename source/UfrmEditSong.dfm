object frmEditSong: TfrmEditSong
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Edit Song Slide'
  ClientHeight = 516
  ClientWidth = 369
  Color = clBtnFace
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
    Top = 84
    Width = 27
    Height = 13
    Caption = 'Picto:'
  end
  object Label4: TLabel
    Left = 8
    Top = 187
    Width = 36
    Height = 13
    Caption = 'Verses:'
  end
  object Shape1: TShape
    Left = 126
    Top = 82
    Width = 96
    Height = 96
  end
  object btnOK: TButton
    Left = 205
    Top = 483
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 15
  end
  object btnCancel: TButton
    Left = 286
    Top = 483
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 16
  end
  object lbVerses: TListBox
    Left = 8
    Top = 182
    Width = 223
    Height = 283
    Style = lbOwnerDrawFixed
    DragMode = dmAutomatic
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnDblClick = lbVersesDblClick
    OnDragDrop = lbVersesDragDrop
    OnDragOver = lbVersesDragOver
    OnDrawItem = lbVersesDrawItem
    OnMouseDown = lbVersesMouseDown
  end
  object btnVerseAdd: TButton
    Left = 237
    Top = 182
    Width = 125
    Height = 25
    Caption = 'Add'
    TabOrder = 6
    OnClick = btnVerseAddClick
  end
  object btnVerseEdit: TButton
    Left = 238
    Top = 404
    Width = 125
    Height = 25
    Caption = 'Edit'
    TabOrder = 12
    OnClick = btnVerseEditClick
  end
  object btnVerseDelete: TButton
    Left = 238
    Top = 435
    Width = 125
    Height = 25
    Caption = 'Delete'
    TabOrder = 13
    OnClick = btnVerseDeleteClick
  end
  object ImgViewPicto: TImgView32
    Left = 128
    Top = 84
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
    TabOrder = 3
    OnClick = ImgViewPictoClick
  end
  object btnVerseCopy: TButton
    Left = 238
    Top = 373
    Width = 125
    Height = 25
    Caption = 'Copy'
    TabOrder = 11
    OnClick = btnVerseCopyClick
  end
  object btnInternet: TButton
    Left = 8
    Top = 483
    Width = 75
    Height = 25
    Caption = 'Internet'
    TabOrder = 14
    OnClick = btnInternetClick
  end
  object cbxShowInOverview: TCheckBox
    Left = 128
    Top = 38
    Width = 184
    Height = 17
    Caption = 'Show In Overview'
    TabOrder = 1
  end
  object cbxPartOfForm: TCheckBox
    Left = 128
    Top = 61
    Width = 184
    Height = 17
    Caption = 'Part Of Form'
    TabOrder = 2
  end
  object btnSelectPictoNone: TButton
    Left = 238
    Top = 84
    Width = 75
    Height = 25
    Caption = 'None'
    TabOrder = 4
    OnClick = btnSelectPictoNoneClick
  end
  object cmbOverviewName: TComboBox
    Left = 128
    Top = 11
    Width = 185
    Height = 21
    TabOrder = 0
    OnEnter = cmbOverviewNameEnter
    OnSelect = cmbOverviewNameEnter
    Items.Strings = (
      'Ps '
      'Gez '
      'LB '
      'Opw ')
  end
  object btnVerseAddPsalm: TButton
    Left = 237
    Top = 213
    Width = 125
    Height = 25
    Caption = 'Add Psalm'
    TabOrder = 7
    OnClick = btnVerseAddPsalmClick
  end
  object btnVerseAddGezang: TButton
    Left = 237
    Top = 244
    Width = 125
    Height = 25
    Caption = 'Add Gezang'
    TabOrder = 8
    OnClick = btnVerseAddGezangClick
  end
  object btnVerseAddLiedboek: TButton
    Left = 238
    Top = 275
    Width = 125
    Height = 25
    Caption = 'Add Liedboek'
    TabOrder = 9
    OnClick = btnVerseAddLiedboekClick
  end
  object btnVerseAddOpwekking: TButton
    Left = 237
    Top = 306
    Width = 125
    Height = 25
    Caption = 'Add Opwekking'
    TabOrder = 10
    OnClick = btnVerseAddOpwekkingClick
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 152
    Top = 190
  end
end
