object frmPPTViewer: TfrmPPTViewer
  Left = 0
  Top = 0
  Caption = 'frmPPTViewer'
  ClientHeight = 545
  ClientWidth = 907
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    907
    545)
  PixelsPerInch = 96
  TextHeight = 13
  object lblPPTFile: TLabel
    Left = 175
    Top = 13
    Width = 44
    Height = 13
    Caption = 'lblPPTFile'
  end
  object lblDescription: TLabel
    Left = 8
    Top = 489
    Width = 53
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Description'
  end
  object lblRemark: TLabel
    Left = 8
    Top = 517
    Width = 89
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Remark / Antiphon'
  end
  object btnOpenComputer: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Computer'
    TabOrder = 1
    OnClick = btnOpenComputerClick
  end
  object tvPPT: TTreeView
    Left = 8
    Top = 39
    Width = 161
    Height = 441
    Anchors = [akLeft, akTop, akBottom]
    HideSelection = False
    Indent = 19
    MultiSelect = True
    MultiSelectStyle = [msControlSelect, msShiftSelect, msSiblingOnly]
    ReadOnly = True
    TabOrder = 2
    OnClick = TreeView1DblClick
  end
  object ImgView321: TImgView32
    Left = 175
    Top = 39
    Width = 724
    Height = 441
    Anchors = [akLeft, akTop, akRight, akBottom]
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
  end
  object btnCancel: TButton
    Left = 824
    Top = 512
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object btnSelect: TButton
    Left = 743
    Top = 512
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Select'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object edtDescription: TEdit
    Left = 113
    Top = 486
    Width = 456
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 4
  end
  object btnOpenInternet: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Internet'
    TabOrder = 0
    OnClick = btnOpenInternetClick
  end
  object edtRemark: TEdit
    Left = 113
    Top = 514
    Width = 456
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 5
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Powerpoint|*.ppt;*.pptx|All|*.*'
    Left = 176
    Top = 8
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 392
    Top = 424
  end
end
