object frmEditExtSlide: TfrmEditExtSlide
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Edit External Slide'
  ClientHeight = 288
  ClientWidth = 323
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
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 14
    Width = 59
    Height = 13
    Caption = 'Footer text:'
  end
  object Label3: TLabel
    Left = 8
    Top = 38
    Width = 27
    Height = 13
    Caption = 'Picto:'
  end
  object Label4: TLabel
    Left = 8
    Top = 141
    Width = 36
    Height = 13
    Caption = 'Verses:'
  end
  object Shape1: TShape
    Left = 126
    Top = 36
    Width = 96
    Height = 96
  end
  object btnOK: TButton
    Left = 157
    Top = 252
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object btnCancel: TButton
    Left = 238
    Top = 252
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object edtOverviewName: TEdit
    Left = 128
    Top = 11
    Width = 185
    Height = 21
    TabOrder = 0
  end
  object lbVerses: TListBox
    Left = 8
    Top = 136
    Width = 223
    Height = 110
    Style = lbOwnerDrawFixed
    DragMode = dmAutomatic
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnDblClick = lbVersesDblClick
    OnDragDrop = lbVersesDragDrop
    OnDragOver = lbVersesDragOver
    OnDrawItem = lbVersesDrawItem
    OnMouseDown = lbVersesMouseDown
  end
  object btnVerseAdd: TButton
    Left = 238
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 4
    OnClick = btnVerseAddClick
  end
  object btnEdit: TButton
    Left = 238
    Top = 167
    Width = 75
    Height = 25
    Caption = 'Edit'
    TabOrder = 5
    OnClick = btnEditClick
  end
  object btnDelete: TButton
    Left = 238
    Top = 198
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 6
    OnClick = btnDeleteClick
  end
  object ImgViewPicto: TImgView32
    Left = 128
    Top = 38
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
  object btnSelectPictoNone: TButton
    Left = 238
    Top = 38
    Width = 75
    Height = 25
    Caption = 'None'
    TabOrder = 2
    OnClick = btnSelectPictoNoneClick
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 152
    Top = 144
  end
end
