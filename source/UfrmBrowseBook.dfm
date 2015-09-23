object frmBrowseBook: TfrmBrowseBook
  Left = 0
  Top = 0
  Caption = 'Browse Book'
  ClientHeight = 308
  ClientWidth = 721
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 225
    Top = 0
    Width = 5
    Height = 267
    ExplicitLeft = 338
    ExplicitTop = -111
    ExplicitHeight = 470
  end
  object tvBooks: TTreeView
    Left = 0
    Top = 0
    Width = 225
    Height = 267
    Align = alLeft
    Indent = 19
    MultiSelectStyle = [msControlSelect, msShiftSelect, msSiblingOnly]
    ReadOnly = True
    RightClickSelect = True
    TabOrder = 0
    OnClick = tvBooksClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 267
    Width = 721
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      721
      41)
    object btnOK: TButton
      Left = 553
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
      Left = 634
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
  object mmoVerse: TMemo
    Left = 230
    Top = 0
    Width = 491
    Height = 267
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 112
    Top = 112
  end
end
