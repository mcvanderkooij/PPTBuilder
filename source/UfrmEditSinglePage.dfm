object frmEditSinglePage: TfrmEditSinglePage
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Edit Single'
  ClientHeight = 386
  ClientWidth = 323
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
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 14
    Width = 59
    Height = 13
    Caption = 'Footer text:'
  end
  object Label3: TLabel
    Left = 8
    Top = 60
    Width = 27
    Height = 13
    Caption = 'Picto:'
  end
  object Shape1: TShape
    Left = 6
    Top = 76
    Width = 96
    Height = 96
  end
  object edtOverviewName: TEdit
    Left = 8
    Top = 33
    Width = 307
    Height = 21
    TabOrder = 0
  end
  object ImgViewPicto: TImgView32
    Left = 8
    Top = 78
    Width = 92
    Height = 92
    Cursor = crHandPoint
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smOptimalScaled
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 17
    ScrollBars.Visibility = svAuto
    OverSize = 0
    TabOrder = 1
    OnClick = ImgViewPictoClick
  end
  object btnSelectPictoNone: TButton
    Left = 106
    Top = 78
    Width = 75
    Height = 25
    Caption = 'None'
    TabOrder = 2
    OnClick = btnSelectPictoNoneClick
  end
  object btnOK: TButton
    Left = 159
    Top = 353
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 240
    Top = 353
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object PageControlContent: TPageControl
    Left = 8
    Top = 176
    Width = 307
    Height = 171
    ActivePage = tsPicture
    TabOrder = 5
    object tsText: TTabSheet
      Caption = 'tsText'
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object edtContent: TEdit
        Left = 3
        Top = 3
        Width = 293
        Height = 21
        TabOrder = 0
      end
    end
    object tsMemo: TTabSheet
      Caption = 'tsMemo'
      ImageIndex = 1
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object mmoContent: TMemo
        Left = 0
        Top = 0
        Width = 299
        Height = 161
        Align = alClient
        TabOrder = 0
      end
    end
    object tsPicture: TTabSheet
      Caption = 'tsPicture'
      ImageIndex = 2
      TabVisible = False
      object ImgViewPicture: TImgView32
        Left = 0
        Top = 0
        Width = 299
        Height = 161
        Cursor = crHandPoint
        Align = alClient
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baCustom
        Scale = 1.000000000000000000
        ScaleMode = smOptimalScaled
        ScrollBars.ShowHandleGrip = True
        ScrollBars.Style = rbsDefault
        ScrollBars.Size = 17
        ScrollBars.Visibility = svAuto
        OverSize = 0
        TabOrder = 0
        OnClick = ImgViewPictureClick
      end
    end
  end
end
