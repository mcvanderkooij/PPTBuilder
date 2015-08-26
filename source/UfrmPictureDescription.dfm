object frmPictureDescription: TfrmPictureDescription
  Left = 0
  Top = 0
  Caption = 'Picture Description'
  ClientHeight = 468
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
    468)
  PixelsPerInch = 96
  TextHeight = 13
  object ImgView321: TImgView32
    Left = 0
    Top = 0
    Width = 620
    Height = 399
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
    Left = 8
    Top = 405
    Width = 604
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 456
    Top = 432
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 537
    Top = 432
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 48
    Top = 432
  end
end
