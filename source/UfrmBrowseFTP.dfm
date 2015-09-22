object frmBrowseFTP: TfrmBrowseFTP
  Left = 0
  Top = 0
  Caption = 'Browse Server'
  ClientHeight = 513
  ClientWidth = 828
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 472
    Width = 828
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      828
      41)
    object btnOK: TButton
      Left = 664
      Top = 5
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 745
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 828
    Height = 472
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 338
      Top = 1
      Width = 5
      Height = 470
    end
    object tvDirs: TTreeView
      Left = 1
      Top = 1
      Width = 337
      Height = 470
      Align = alLeft
      Indent = 19
      ReadOnly = True
      RightClickSelect = True
      TabOrder = 0
      OnChange = tvDirsChange
    end
    object Panel3: TPanel
      Left = 343
      Top = 1
      Width = 484
      Height = 470
      Align = alClient
      Caption = 'Panel3'
      TabOrder = 1
      object lvFiles: TListView
        Left = 1
        Top = 42
        Width = 482
        Height = 427
        Align = alClient
        Columns = <
          item
            AutoSize = True
          end>
        ReadOnly = True
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvFilesDblClick
      end
      object pnlFolder: TPanel
        Left = 1
        Top = 1
        Width = 482
        Height = 41
        Align = alTop
        TabOrder = 1
        object lblFolder: TLabel
          Left = 1
          Top = 1
          Width = 480
          Height = 39
          Align = alClient
          Alignment = taCenter
          AutoSize = False
          ShowAccelChar = False
          Layout = tlCenter
          ExplicitLeft = 136
          ExplicitTop = 8
          ExplicitWidth = 31
          ExplicitHeight = 13
        end
      end
    end
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 488
    Top = 8
  end
end
