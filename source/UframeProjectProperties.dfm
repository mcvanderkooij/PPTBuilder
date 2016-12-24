object FrameProjectProperties: TFrameProjectProperties
  Left = 0
  Top = 0
  Width = 542
  Height = 87
  TabOrder = 0
  object Label2: TLabel
    Left = 8
    Top = 6
    Width = 43
    Height = 13
    Caption = 'Speaker:'
  end
  object Label3: TLabel
    Left = 8
    Top = 33
    Width = 51
    Height = 13
    Caption = 'Collecte 1:'
  end
  object Label4: TLabel
    Left = 8
    Top = 60
    Width = 51
    Height = 13
    Caption = 'Collecte 2:'
  end
  object edtSpeaker: TEdit
    Left = 112
    Top = 3
    Width = 360
    Height = 21
    TabOrder = 0
    OnExit = edtSpeakerExit
  end
  object edtCollecte1: TEdit
    Left = 112
    Top = 30
    Width = 360
    Height = 21
    TabOrder = 1
    OnExit = edtSpeakerExit
  end
  object edtCollecte2: TEdit
    Left = 112
    Top = 57
    Width = 360
    Height = 21
    TabOrder = 2
    OnExit = edtSpeakerExit
  end
  object btnSpeakerAdd: TButton
    Left = 478
    Top = 2
    Width = 23
    Height = 23
    Caption = '+'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = btnSpeakerAddClick
  end
  object btnSpeakerSelect: TButton
    Left = 507
    Top = 2
    Width = 23
    Height = 23
    Caption = '...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    OnClick = btnSpeakerSelectClick
  end
  object btnCollecte1Add: TButton
    Left = 478
    Top = 29
    Width = 23
    Height = 23
    Caption = '+'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = btnCollecte1AddClick
  end
  object btnCollecte1Select: TButton
    Left = 507
    Top = 29
    Width = 23
    Height = 23
    Caption = '...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = btnCollecte1SelectClick
  end
  object btnCollecte2Add: TButton
    Left = 478
    Top = 56
    Width = 23
    Height = 23
    Caption = '+'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
    OnClick = btnCollecte2AddClick
  end
  object btnCollecte2Select: TButton
    Left = 507
    Top = 56
    Width = 23
    Height = 23
    Caption = '...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 8
    OnClick = btnCollecte2SelectClick
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 80
    Top = 32
  end
end
