object frmQuickStart: TfrmQuickStart
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Quick Start'
  ClientHeight = 488
  ClientWidth = 609
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 24
    Width = 98
    Height = 13
    Caption = 'Create New Project:'
  end
  object Label2: TLabel
    Left = 16
    Top = 400
    Width = 107
    Height = 13
    Caption = 'Open Existing Project:'
  end
  object Label3: TLabel
    Left = 16
    Top = 201
    Width = 107
    Height = 13
    Caption = 'Recently Opened Files'
  end
  object btnOpenExisting: TButton
    Left = 519
    Top = 395
    Width = 75
    Height = 25
    Caption = 'Select...'
    TabOrder = 0
    OnClick = btnOpenExistingClick
  end
  object btnClose: TButton
    Left = 519
    Top = 448
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 1
  end
  object lbLiturgies: TListBox
    Left = 16
    Top = 43
    Width = 497
    Height = 150
    ItemHeight = 13
    TabOrder = 2
    OnDblClick = lbLiturgiesDblClick
  end
  object lbRecentFiles: TListBox
    Left = 15
    Top = 220
    Width = 498
    Height = 150
    ItemHeight = 13
    TabOrder = 3
    OnDblClick = lbRecentFilesDblClick
  end
  object btnOpenRecent: TButton
    Left = 519
    Top = 220
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 4
    OnClick = lbRecentFilesDblClick
  end
  object btnNew: TButton
    Left = 519
    Top = 43
    Width = 75
    Height = 25
    Caption = 'New'
    TabOrder = 5
    OnClick = lbLiturgiesDblClick
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 336
    Top = 408
  end
end
