unit StickStructureUnit;

interface

uses
  Types,
  SysUtils,
  Classes,
  SmartObjectUnit,
  StringBuilderUnit,
  gvector,
  LazUtils,
  CommonLog;

type

  TStickCircle = record
    Name: string;
    defaultRadius: byte;
  end;

  PStickCircle = ^TStickCircle;

  TStickCircleVector = specialize TVector<PStickCircle>;

  TStickPipe = record
    Name: string;
    A, B: PStickCircle; //< connection points
  end;

  TStickPipeVector = specialize TVector<TStickPipe>;

  { TStickStructure }

  TStickStructure = class(TSmartObject)
  protected
    FCircles: TStickCircleVector;
    FPipes: TStickPipeVector;
    procedure CirclesToDebugText(const s: TStringBuilder);
    procedure PipesToDebugText(const s: TStringBuilder);
    procedure ReleaseCircles;
  public
    property Circles: TStickCircleVector read FCircles;
    property Pipes: TStickPipeVector read FPipes;
    function FindCircle(const aName: string): PStickCircle;
    constructor Create; override;
    function ToDebugText: string;
    destructor Destroy; override;
  end;

implementation

{ TStickStructure }

procedure TStickStructure.CirclesToDebugText(const s: TStringBuilder);
var
  i: SizeUInt;
  currentCircle: PStickCircle;
begin
  s.Add(['Circles (', IntToStr(Circles.Size), ' units)', LineEnding]);
  for i := 0 to Circles.Size - 1 do
  begin
    currentCircle := Circles[i];
    s.Add(['  ', currentCircle^.Name, ' ', IntToStr(currentCircle^.DefaultRadius), LineEnding]);
  end;
end;

procedure TStickStructure.PipesToDebugText(const s: TStringBuilder);
  function ConnectionPointToString(const aPoint: PStickCircle): string;
  begin
    if
      aPoint <> nil
    then
      result := aPoint^.Name
    else
      result := 'nil_point';
  end;

var
  i: SizeUInt;
  currentPipe: TStickPipe;
begin
  s.Add(['Pipes (', IntToStr(Pipes.Size), ' units)', LineEnding]);
  for i := 0 to Pipes.Size - 1 do
  begin
    currentPipe := Pipes[i];
    s.Add(
      ['  ',
        currentPipe.Name, ': ',
        ConnectionPointToString(currentPipe.A), ' ',
        ConnectionPointToString(currentPipe.B),
        LineEnding]
    )
  end;
end;

procedure TStickStructure.ReleaseCircles;
var
  i: Cardinal;
begin
  for i := 0 to Circles.Size - 1 do
    Dispose(Circles[i]);
  Circles.Clear;
end;

function TStickStructure.FindCircle(const aName: string): PStickCircle;
var
  i: Cardinal;
  c: PStickCircle;
begin
  result := nil;
  for i := 0 to Circles.Size - 1 do
  begin
    c := Circles[i];
    if
      c^.Name = aName
    then
    begin
      result := c;
      break;
    end;
  end;
end;

constructor TStickStructure.Create;
begin
  inherited Create;
  FCircles := TStickCircleVector.Create;
  FPipes := TStickPipeVector.Create;
end;

function TStickStructure.ToDebugText: string;
var
  s: TStringBuilder;
begin
  s := TStringBuilder.Create;
  s.Add('Stickman Structure:');
  s.Add(LineEnding);
  CirclesToDebugText(s);
  PipesToDebugText(s);
  result := s.ToString;
  s.Free;
end;

destructor TStickStructure.Destroy;
begin
  Pipes.Free;
  ReleaseCircles;
  Circles.Free;
  inherited Destroy;
end;

end.

