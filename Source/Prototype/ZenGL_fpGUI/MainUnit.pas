unit MainUnit;

interface

uses
  Classes, SysUtils,
  fpg_main, fpg_form, fpg_panel, fpg_button,
  zglHeader;

type

  { TMainWindow }

  TMainWindow = class(TfpgForm)
  protected
    FViewPanel: TfpgPanel;
    FButtonsPanel: TfpgPanel;
    FFirstActivation: Boolean;
    FFetchEngineButton: TfpgButton;
    function GetViewPanel: TfpgPanel;
    procedure SetupButtons;
    procedure FetchZenGLEngine;
    procedure ActivationHandler(aSender: TObject);
    procedure CloseHandler(aSender: TObject; var aCloseAction: TCloseAction);
    procedure FetchEngineButtonClickHandler(aSender: TObject);
  public
    property ViewPanel: TfpgPanel read GetViewPanel;
    property ButtonsPanel: TfpgPanel read FButtonsPanel;
    property FirstActivation: Boolean read FFirstActivation;
    property FetchEngineButton: TfpgButton read FFetchEngineButton;
    constructor Create(aOwner: TComponent); override;
    procedure AfterCreate; override;
  end;

var
  MainWindow: TMainWindow;
  EngineActivated: Boolean;

procedure MainRoutine;

implementation

procedure EngineInitializationHandler;
begin
  WriteLN('Now adjusting...');
  scr_SetVSync(True);
  wnd_SetPos(MainWindow.ViewPanel.Left, MainWindow.ViewPanel.Top);
  wnd_SetSize(MainWindow.ViewPanel.Width, MainWindow.ViewPanel.Height);
  WriteLN('Adjusted...');
  EngineActivated := True;
end;

procedure EngineDrawHandler;
begin
  WriteLN('Redraw...');
  pr2d_Rect( 10, 10, 30, 30, $FF0000, 255 );
  fpgApplication.ProcessMessages;
end;

procedure EngineUpdateHandler(const aTime: Double);
begin

end;

{ TMainWindow }

function TMainWindow.GetViewPanel: TfpgPanel;
begin
  result := FViewPanel;
end;

procedure TMainWindow.SetupButtons;
begin
  FButtonsPanel := TfpgPanel.Create(self);
  ButtonsPanel.Align := alBottom;
  ButtonsPanel.Height := 24;

  FFetchEngineButton := TfpgButton.Create(ButtonsPanel);
  FetchEngineButton.Text := 'Fetch engine';
  FetchEngineButton.Align := alLeft;
  FetchEngineButton.OnClick := @FetchEngineButtonClickHandler;
end;

procedure TMainWindow.FetchZenGLEngine;
begin
  if
    not zglLoad(libZenGL)
  then
    exit; // everything went worse than expected
  zgl_Reg(SYS_LOAD, @EngineInitializationHandler);
  zgl_Reg(SYS_DRAW, @EngineDrawHandler);
  zgl_Reg(SYS_UPDATE, @EngineUpdateHandler);
  wnd_ShowCursor(True);
  WriteLN('Now initializing...');
  zgl_InitToHandle(WinHandle);
  WriteLN('fffuuu');
end;

procedure TMainWindow.ActivationHandler(aSender: TObject);
begin
  if FirstActivation then
  begin
    FFirstActivation := False;
  end;
end;

procedure TMainWindow.CloseHandler(aSender: TObject; var aCloseAction: TCloseAction);
begin
  if
    EngineActivated
  then
    zgl_Exit;
end;

procedure TMainWindow.FetchEngineButtonClickHandler(aSender: TObject);
begin
  FetchZenGLEngine;
end;

constructor TMainWindow.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFirstActivation := True;
  OnActivate := @ActivationHandler;
  OnClose := @CloseHandler;
end;

procedure TMainWindow.AfterCreate;
begin
  FViewPanel := TfpgPanel.Create(self);
  ViewPanel.Name := 'ViewPanel';
  ViewPanel.Text := ViewPanel.Name;
  ViewPanel.Align := alClient;
  SetupButtons;
end;

procedure MainRoutine;
begin
  EngineActivated := false;
  fpgApplication.Initialize;
  MainWindow := TMainWindow.Create(nil);
  MainWindow.Width := 800;
  MainWindow.Height := 600;
  MainWindow.Show;
  fpgApplication.Run;
  MainWindow.Free;
end;

end.

