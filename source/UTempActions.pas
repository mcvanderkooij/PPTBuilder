unit UTempActions;

interface

uses
  Controls;

function SetWaitCursor(cursor: TCursor = crHourGlass): IUnknown;
function SetDisableActions: IUnknown;
function SetDecimalSeparator(separator: char = '.'): IUnknown;

implementation

uses
  SysUtils, Forms;

type
  TWaitCursor = class(TInterfacedObject, IUnknown)
  private
    FOldCursor: TCursor;
  public
    constructor Create(cursor: TCursor); virtual;
    destructor Destroy; override;
  end;

function SetWaitCursor(cursor: TCursor = crHourGlass): IUnknown;
begin
  Result := TWaitCursor.Create(cursor);
end;

{ TWaitCursor }

constructor TWaitCursor.Create(cursor: TCursor);
begin
  inherited Create;
  FOldCursor := Screen.Cursor;
  Screen.Cursor := cursor;
end;

destructor TWaitCursor.Destroy;
begin
  Screen.Cursor := FOldCursor;
  inherited;
end;


{ TDisableActions }

var
  gl_DisableActions: boolean = false;

type
  TDisableActions = class(TInterfacedObject, IUnknown)
  private
    FHasSet: boolean;  // remember if i have changed the gl_DisableActions
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

constructor TDisableActions.Create;
begin
  FHasSet := False;
  if gl_DisableActions then
    Abort;
  gl_DisableActions := True;
  FHasSet := True;
end;

destructor TDisableActions.Destroy;
begin
  if FHasSet then
    gl_DisableActions := False;
  inherited;
end;

function SetDisableActions: IUnknown;
begin
  Result := TDisableActions.Create();
end;

type
  TDecimalSeparator = class(TInterfacedObject, IUnknown)
  private
    FOldSeparator: char;
  public
    constructor Create(separator: char); virtual;
    destructor Destroy; override;
  end;

function SetDecimalSeparator(separator: char = '.'): IUnknown;
begin
  Result := TDecimalSeparator.Create(separator);
end;

{ TDecimalSeparator }

constructor TDecimalSeparator.Create(separator: char);
begin
  inherited Create;
  FOldSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := separator;
end;

destructor TDecimalSeparator.Destroy;
begin
  FormatSettings.DecimalSeparator := FOldSeparator;
  inherited;
end;


end.
