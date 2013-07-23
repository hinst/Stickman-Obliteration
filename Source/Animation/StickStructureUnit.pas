unit StickStructureUnit;

interface

uses
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

  TStickCircleVector = specialize TVector<TStickCircle>;

  TStickPipe = record
    Name: string;
    A, B: string; // connection point identifiers
  end;

  TStickPipeVector = specialize TVector<TStickPipe>;

  { TStickStructure }

  TStickStructure = class(TSmartObject)
  protected
    FCircles: TStickCircleVector;
    FPipes: TStickPipeVector;
  public
    property Circles: TStickCircleVector read FCircles;
    property Pipes: TStickPipeVector read FPipes;
    constructor Create; override;
    function ToDebugText: string;
    destructor Destroy; override;
  end;

implementation

{ TStickStructure }

constructor TStickStructure.Create;
begin
  inherited Create;
  FCircles := TStickCircleVector.Create;
  FPipes := TStickPipeVector.Create;
end;

function TStickStructure.ToDebugText: string;
var
  s: TStringBuilder;
  i: Cardinal;
begin
  s := TStringBuilder.Create;
  s.Add('Circles');
  s.Add(LineEnding);
  for i := 0 to Circles.Size - 1 do
  begin
    s.Add('  ');
    s.Add(Circles[i].Name);
    s.Add(' ');
    s.Add(IntToStr(Circles[i].defaultRadius));
  end;
  s.Free;
end;

destructor TStickStructure.Destroy;
begin
  Pipes.Free;
  Circles.Free;
  inherited Destroy;
end;

end.

