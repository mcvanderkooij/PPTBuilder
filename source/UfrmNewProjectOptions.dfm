object frmNewProjectOptions: TfrmNewProjectOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'New Project Options'
  ClientHeight = 267
  ClientWidth = 549
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    549
    267)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 112
    Width = 37
    Height = 13
    Caption = 'Options'
  end
  object btnOK: TButton
    Left = 385
    Top = 234
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 466
    Top = 234
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  inline FrameProjectProperties1: TFrameProjectProperties
    Left = 8
    Top = 8
    Width = 542
    Height = 87
    TabOrder = 2
  end
  object lbSlideOptions: TCheckListBox
    Left = 120
    Top = 101
    Width = 360
    Height = 124
    ItemHeight = 13
    TabOrder = 3
  end
end
