object frmSelectString: TfrmSelectString
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Select String'
  ClientHeight = 354
  ClientWidth = 435
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
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 298
    Width = 28
    Height = 13
    Caption = 'Filter:'
  end
  object lbStrings: TListBox
    Left = 8
    Top = 8
    Width = 337
    Height = 281
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbStringsClick
    OnDblClick = lbStringsDblClick
  end
  object btnAdd: TButton
    Left = 351
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 1
    OnClick = btnAddClick
  end
  object btnEdit: TButton
    Left = 351
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Edit'
    TabOrder = 2
    OnClick = btnEditClick
  end
  object btnDelete: TButton
    Left = 351
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 3
    OnClick = btnDeleteClick
  end
  object btnOK: TButton
    Left = 270
    Top = 322
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 351
    Top = 322
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object edtFilter: TEdit
    Left = 72
    Top = 295
    Width = 273
    Height = 21
    TabOrder = 6
    OnChange = edtFilterChange
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 376
    Top = 200
  end
end
