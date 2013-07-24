unit SquaredPointUnit;

interface

uses
  SysUtils,
  SmartObjectUnit,
  gvector;

type

  { TSquaredPoint }

  TSquaredPoint = class(TSmartObject)
  public
    x, y: Single;
    constructor Create; override;
    procedure Clear; inline;
    procedure SetValue(const aX, aY: Single); inline;
    function ToDebugString: string; override;
  end;

  TSquaredPointVector = specialize TVector<TSquaredPoint>;

implementation

{ TSquaredPoint }

constructor TSquaredPoint.Create;
begin
  inherited Create;
  Clear;
end;

procedure TSquaredPoint.Clear;
begin
  SetValue(0, 0);
end;

procedure TSquaredPoint.SetValue(const aX, aY: Single);
begin
  x := aX;
  y := aY;
end;

function TSquaredPoint.ToDebugString: string;
begin
  Result := '[' + FloatToStr(x) + ' ' + FloatToStr(y) + ']';
end;

end.

