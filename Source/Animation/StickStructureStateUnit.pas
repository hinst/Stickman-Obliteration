unit StickStructureStateUnit;

interface

uses
  SmartObjectUnit,
  StringBuilderUnit,
  SquaredPointUnit,
  StickStructureUnit;

type

  { TStickStructureState }

  TStickStructureState = class(TSmartObject)
  protected
    FCirclePositions: TSquaredPointVector;
  public
    property CirclePositions: TSquaredPointVector read FCirclePositions;
    constructor Create; override;
    procedure Setup(const aStructure: TStickStructure);
    procedure ThrowAround;
    function ToDebugText: string;
    destructor Destroy; override;
  end;

implementation

{ TStickStructureState }

constructor TStickStructureState.Create;
begin
  inherited Create;
  FCirclePositions := TSquaredPointVector.Create;
end;

procedure TStickStructureState.Setup(const aStructure: TStickStructure);
var
  circles: TStickCircleVector;
  i, n: SizeUInt;
begin
  circles := aStructure.Circles;
  n := circles.Size;
  CirclePositions.Reserve(n);
  for i := 0 to n - 1 do
  begin
    CirclePositions[i] := TSquaredPoint.Create;
  end;
end;

procedure TStickStructureState.ThrowAround;
var
  squareLength, n, i, x, y: SizeUInt;
  delta: Single;
begin
  squareLength := 0;
  n := CirclePositions.Size;
  if
    n <> 0
  then
  begin
    while
      Sqr(squareLength) < n
    do
      Inc(squareLength);

    x := 0;
    y := 0;
    delta := 1 / squareLength;
    for i := 0 to n - 1 do
    begin
      CirclePositions[i].SetValue((x + 0.5) * delta - 0.5, (y + 0.5) * delta - 0.5);
      Inc(x);
      if
        x >= squareLength
      then
      begin
        x := 0;
        Inc(y);
      end;
    end;
  end;
end;

function TStickStructureState.ToDebugText: string;
var
  i: SizeUInt;
  circlePosition: TSquaredPoint;
  s: TStringBuilder;
begin
  s := TStringBuilder.Create;
  for i := 0 to CirclePositions.Size - 1 do
  begin
    circlePosition := CirclePositions[i];
    s.Add([circlePosition.ToDebugString, LineEnding]);
  end;
  result := s.ToString;
  s.Free;
end;

destructor TStickStructureState.Destroy;
begin
  CirclePositions.Free;
  inherited Destroy;
end;

end.







