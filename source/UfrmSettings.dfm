object frmSettings: TfrmSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 114
  ClientWidth = 276
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 90
    Height = 13
    Caption = 'Website Username'
  end
  object Label2: TLabel
    Left = 8
    Top = 38
    Width = 88
    Height = 13
    Caption = 'Website Password'
  end
  object btnOK: TButton
    Left = 109
    Top = 80
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 190
    Top = 80
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edtUsername: TEdit
    Left = 128
    Top = 8
    Width = 137
    Height = 21
    TabOrder = 2
  end
  object edtPassword: TEdit
    Left = 128
    Top = 35
    Width = 137
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
end
