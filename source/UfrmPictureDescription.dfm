object frmPictureDescription: TfrmPictureDescription
  Left = 0
  Top = 0
  Caption = 'Picture Description'
  ClientHeight = 491
  ClientWidth = 620
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
  DesignSize = (
    620
    491)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 404
    Width = 53
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Description'
  end
  object Label2: TLabel
    Left = 8
    Top = 431
    Width = 89
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Remark / Antiphon'
  end
  object ImgView321: TImgView32
    Left = 0
    Top = 0
    Width = 620
    Height = 385
    Anchors = [akLeft, akTop, akRight, akBottom]
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smOptimalScaled
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 17
    OverSize = 0
    TabOrder = 0
  end
  object edtDescription: TEdit
    Left = 128
    Top = 401
    Width = 484
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 456
    Top = 455
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    ExplicitTop = 432
  end
  object btnCancel: TButton
    Left = 537
    Top = 455
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    ExplicitTop = 432
  end
  object edtRemark: TEdit
    Left = 128
    Top = 428
    Width = 484
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 2
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 80
    Top = 288
  end
end
