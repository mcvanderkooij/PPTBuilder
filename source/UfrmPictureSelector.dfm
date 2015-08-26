object frmPictureSelector: TfrmPictureSelector
  Left = 0
  Top = 0
  Caption = 'Picture Selector'
  ClientHeight = 536
  ClientWidth = 635
  Color = clBtnFace
  DragMode = dmAutomatic
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
    635
    536)
  PixelsPerInch = 96
  TextHeight = 13
  object lblFooterText: TLabel
    Left = 8
    Top = 508
    Width = 59
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Footer text:'
  end
  object ImgViewSelection: TImgView32
    Left = 247
    Top = 8
    Width = 380
    Height = 489
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smOptimalScaled
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 17
    OverSize = 0
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 552
    Top = 503
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnSelect: TButton
    Left = 471
    Top = 503
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Select'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object lbPictures: TListBox
    Left = 8
    Top = 8
    Width = 233
    Height = 489
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object edtFooterText: TEdit
    Left = 96
    Top = 505
    Width = 369
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 4
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 416
    Top = 504
  end
end
