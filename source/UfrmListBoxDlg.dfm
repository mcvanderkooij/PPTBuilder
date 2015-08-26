object frmListBoxDlg: TfrmListBoxDlg
  Left = 0
  Top = 0
  Caption = 'frmListBoxDlg'
  ClientHeight = 435
  ClientWidth = 284
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 292
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 7
  Padding.Right = 7
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object pnlHeaderMessage: TPanel
    Left = 7
    Top = 0
    Width = 270
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 4
      Height = 14
      Align = alClient
      Alignment = taCenter
      Layout = tlCenter
      WordWrap = True
    end
  end
  object pnlButtons: TPanel
    Left = 7
    Top = 394
    Width = 270
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    object btnYes: TButton
      Left = 15
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Yes'
      ModalResult = 6
      TabOrder = 0
    end
    object btnNo: TButton
      Left = 96
      Top = 6
      Width = 75
      Height = 25
      Caption = 'No'
      ModalResult = 7
      TabOrder = 1
    end
    object btnOk: TButton
      Left = 153
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 2
    end
    object btnCancel: TButton
      Left = 209
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
  end
  object pnlFooterMessage: TPanel
    Left = 7
    Top = 353
    Width = 270
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object lblFooter: TLabel
      Left = 0
      Top = 0
      Width = 4
      Height = 14
      Align = alClient
      Alignment = taCenter
      Layout = tlCenter
      WordWrap = True
    end
  end
  object lbItems: TListBox
    Left = 7
    Top = 41
    Width = 270
    Height = 281
    Align = alClient
    ItemHeight = 14
    TabOrder = 1
    OnDblClick = lbItemsDblClick
  end
  object pnlEdit: TPanel
    Left = 7
    Top = 322
    Width = 270
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      270
      31)
    object edtItem: TEdit
      Left = 0
      Top = 3
      Width = 270
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = edtItemChange
    end
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 168
    Top = 72
  end
end
